# Module 3: Cluster Sampling Economics - Supporting Functions
# Harry's Workshop - Day 2, Tuesday
# SADC Advanced Sampling Workshop

library(tidyverse)
library(survey)
library(sampling)

# ============================================
# 1. COST CALCULATION FUNCTIONS
# ============================================

#' Calculate Total Survey Cost
#' @param c0 Fixed costs
#' @param c1 Cost per PSU (cluster)
#' @param c2 Cost per unit within cluster
#' @param m Number of PSUs
#' @param n_bar Average cluster size
#' @return Total cost
calculate_cost <- function(c0, c1, c2, m, n_bar) {
  total_cost <- c0 + c1 * m + c2 * m * n_bar
  return(total_cost)
}

#' Compare SRS vs Cluster Costs
#' @param n Total sample size
#' @param c0 Fixed costs
#' @param c_srs Cost per unit in SRS
#' @param c1 Cost per cluster
#' @param c2 Cost per unit in cluster
#' @param b Cluster size
#' @return Comparison data frame
compare_costs <- function(n, c0, c_srs, c1, c2, b) {
  
  # SRS cost
  srs_cost <- c0 + n * c_srs
  
  # Cluster cost
  m <- ceiling(n / b)
  cluster_cost <- calculate_cost(c0, c1, c2, m, b)
  
  # Savings
  savings <- srs_cost - cluster_cost
  savings_pct <- (savings / srs_cost) * 100
  
  result <- tibble(
    Design = c("SRS", "Cluster"),
    Total_Cost = c(srs_cost, cluster_cost),
    Fixed = c(c0, c0),
    Variable = c(srs_cost - c0, cluster_cost - c0),
    Cost_per_Unit = c(srs_cost/n, cluster_cost/n)
  )
  
  attr(result, "savings") <- savings
  attr(result, "savings_pct") <- savings_pct
  
  return(result)
}

# ============================================
# 2. ICC AND DESIGN EFFECT FUNCTIONS
# ============================================

#' Calculate ICC from Data
#' @param data Data frame with cluster and outcome variable
#' @param cluster_var Name of cluster variable
#' @param outcome_var Name of outcome variable
#' @return ICC value
calculate_icc <- function(data, cluster_var, outcome_var) {
  
  # ANOVA approach
  formula <- as.formula(paste(outcome_var, "~", cluster_var))
  aov_model <- aov(formula, data = data)
  
  # Extract variance components
  ms_table <- anova(aov_model)
  MSB <- ms_table$`Mean Sq`[1]  # Between clusters
  MSW <- ms_table$`Mean Sq`[2]  # Within clusters
  
  # Average cluster size
  cluster_sizes <- data %>%
    group_by(!!sym(cluster_var)) %>%
    summarise(n = n(), .groups = 'drop') %>%
    pull(n)
  
  k0 <- mean(cluster_sizes)  # Could use harmonic mean for unequal sizes
  
  # Calculate ICC
  icc <- (MSB - MSW) / (MSB + (k0 - 1) * MSW)
  
  # Ensure ICC is in valid range
  icc <- max(0, min(1, icc))
  
  return(icc)
}

#' Calculate Design Effect
#' @param b Cluster size
#' @param rho ICC
#' @return Design effect
calculate_deff <- function(b, rho) {
  deff <- 1 + (b - 1) * rho
  return(deff)
}

#' Calculate Effective Sample Size
#' @param n Actual sample size
#' @param deff Design effect
#' @return Effective sample size
effective_sample_size <- function(n, deff) {
  n_eff <- n / deff
  return(n_eff)
}

# ============================================
# 3. OPTIMAL CLUSTER SIZE
# ============================================

#' Calculate Optimal Cluster Size
#' @param c1 Cost per cluster
#' @param c2 Cost per unit within cluster
#' @param rho ICC
#' @return Optimal cluster size
optimal_cluster_size <- function(c1, c2, rho) {
  if (rho == 0) {
    warning("ICC = 0, no clustering effect")
    return(Inf)
  }
  
  b_opt <- sqrt((c1 * (1 - rho)) / (c2 * rho))
  return(b_opt)
}

#' Find Practical Cluster Size
#' @param b_opt Theoretical optimal size
#' @param min_size Minimum practical size
#' @param max_size Maximum practical size
#' @param round_to Round to nearest multiple
#' @return Practical cluster size
practical_cluster_size <- function(b_opt, min_size = 10, max_size = 30, round_to = 5) {
  
  # Constrain to practical range
  b_practical <- max(min_size, min(max_size, b_opt))
  
  # Round to nearest multiple
  b_practical <- round(b_practical / round_to) * round_to
  
  return(b_practical)
}

# ============================================
# 4. PPS SELECTION
# ============================================

#' Systematic PPS Selection
#' @param frame PSU frame with size measure
#' @param n_psu Number of PSUs to select
#' @param size_var Name of size variable
#' @param id_var Name of ID variable
#' @return Selected PSUs
systematic_pps <- function(frame, n_psu, size_var = "size", id_var = "psu_id") {
  
  # Calculate cumulative sizes
  frame <- frame %>%
    arrange(!!sym(id_var)) %>%
    mutate(
      cumsize = cumsum(!!sym(size_var)),
      cumsize_prev = lag(cumsize, default = 0)
    )
  
  # Total size and interval
  total_size <- sum(frame[[size_var]])
  interval <- total_size / n_psu
  
  # Random start
  start <- runif(1, 0, interval)
  
  # Selection points
  selection_points <- start + (0:(n_psu-1)) * interval
  
  # Select PSUs
  selected <- map_df(selection_points, function(point) {
    frame %>%
      filter(cumsize >= point) %>%
      slice(1) %>%
      mutate(selection_point = point)
  })
  
  # Add selection probability
  selected <- selected %>%
    mutate(
      prob_selection = n_psu * !!sym(size_var) / total_size,
      weight_psu = 1 / prob_selection
    )
  
  return(selected)
}

# ============================================
# 5. TWO-STAGE DESIGN
# ============================================

#' Design Two-Stage Sample
#' @param frame PSU frame
#' @param n_psu Number of PSUs
#' @param n_ssu Number of SSUs per PSU
#' @param stratify_by Stratification variable (optional)
#' @param size_var Size measure for PPS
#' @return Design specifications
design_two_stage <- function(frame, n_psu, n_ssu, 
                             stratify_by = NULL, 
                             size_var = "households") {
  
  if (!is.null(stratify_by)) {
    # Stratified design
    strata <- unique(frame[[stratify_by]])
    n_psu_per_stratum <- ceiling(n_psu / length(strata))
    
    design <- map_df(strata, function(s) {
      frame_s <- frame %>% filter(!!sym(stratify_by) == s)
      selected <- systematic_pps(frame_s, n_psu_per_stratum, size_var)
      selected[[stratify_by]] <- s
      return(selected)
    })
  } else {
    # Unstratified design
    design <- systematic_pps(frame, n_psu, size_var)
  }
  
  # Add second stage specifications
  design <- design %>%
    mutate(
      n_ssu_planned = n_ssu,
      total_sample = n() * n_ssu
    )
  
  # Calculate design effect estimate
  avg_cluster_size <- n_ssu
  assumed_icc <- 0.08  # Default assumption
  est_deff <- calculate_deff(avg_cluster_size, assumed_icc)
  
  # Summary
  summary <- list(
    n_psu_selected = nrow(design),
    n_ssu_per_psu = n_ssu,
    total_sample = nrow(design) * n_ssu,
    estimated_deff = est_deff,
    effective_n = (nrow(design) * n_ssu) / est_deff
  )
  
  return(list(
    design = design,
    summary = summary
  ))
}

# ============================================
# 6. BUDGET OPTIMIZATION
# ============================================

#' Optimize Design Within Budget
#' @param budget Total budget available
#' @param c0 Fixed costs
#' @param c1 Cost per PSU
#' @param c2 Cost per unit
#' @param target_n Target sample size
#' @param rho Expected ICC
#' @return Optimal design parameters
optimize_within_budget <- function(budget, c0, c1, c2, target_n, rho = 0.08) {
  
  # Available for variable costs
  available <- budget - c0
  
  # Optimal cluster size
  b_opt <- optimal_cluster_size(c1, c2, rho)
  b_practical <- practical_cluster_size(b_opt)
  
  # Number of PSUs possible
  cost_per_psu <- c1 + b_practical * c2
  max_psu <- floor(available / cost_per_psu)
  
  # Achievable sample
  achievable_n <- max_psu * b_practical
  
  # If can't reach target, find best compromise
  if (achievable_n < target_n) {
    # Try different cluster sizes
    cluster_sizes <- 10:40
    
    results <- map_df(cluster_sizes, function(b) {
      m <- floor(available / (c1 + b * c2))
      n <- m * b
      cost <- c0 + m * c1 + n * c2
      deff <- calculate_deff(b, rho)
      n_eff <- n / deff
      
      tibble(
        cluster_size = b,
        n_psu = m,
        total_n = n,
        total_cost = cost,
        deff = deff,
        effective_n = n_eff,
        efficiency = n_eff / cost
      )
    })
    
    # Find best efficiency
    best <- results %>%
      filter(total_cost <= budget) %>%
      filter(efficiency == max(efficiency)) %>%
      slice(1)
    
    b_practical <- best$cluster_size
    max_psu <- best$n_psu
    achievable_n <- best$total_n
  }
  
  result <- list(
    budget = budget,
    fixed_cost = c0,
    optimal_cluster_size = round(b_opt, 1),
    practical_cluster_size = b_practical,
    n_psu = max_psu,
    total_sample = achievable_n,
    total_cost = c0 + max_psu * c1 + achievable_n * c2,
    target_achieved = achievable_n >= target_n,
    deff = calculate_deff(b_practical, rho),
    effective_sample = achievable_n / calculate_deff(b_practical, rho)
  )
  
  return(result)
}

# ============================================
# 7. VARIANCE ESTIMATION
# ============================================

#' Calculate Cluster Sample Variance
#' @param data Survey data
#' @param cluster_var Cluster identifier
#' @param outcome_var Outcome variable
#' @param weight_var Weight variable (optional)
#' @param strata_var Strata variable (optional)
#' @return Variance estimate
cluster_variance <- function(data, cluster_var, outcome_var, 
                             weight_var = NULL, strata_var = NULL) {
  
  # Create survey design object
  if (is.null(weight_var)) {
    data$weight <- 1
    weight_var <- "weight"
  }
  
  if (!is.null(strata_var)) {
    design <- svydesign(
      ids = as.formula(paste("~", cluster_var)),
      strata = as.formula(paste("~", strata_var)),
      weights = as.formula(paste("~", weight_var)),
      data = data,
      nest = TRUE
    )
  } else {
    design <- svydesign(
      ids = as.formula(paste("~", cluster_var)),
      weights = as.formula(paste("~", weight_var)),
      data = data
    )
  }
  
  # Calculate mean and variance
  result <- svymean(as.formula(paste("~", outcome_var)), design, deff = TRUE)
  
  return(result)
}

# ============================================
# 8. SIMULATION FUNCTIONS
# ============================================

#' Simulate Cluster Sample Performance
#' @param pop_size Population size
#' @param n_clusters Number of clusters in population
#' @param rho True ICC
#' @param n_psu Sample PSUs
#' @param n_ssu Units per PSU
#' @param n_sims Number of simulations
#' @return Simulation results
simulate_cluster_sampling <- function(pop_size, n_clusters, rho, 
                                      n_psu, n_ssu, n_sims = 100) {
  
  # Generate population with clustering
  cluster_size <- pop_size / n_clusters
  
  results <- map_df(1:n_sims, function(sim) {
    
    # Generate clustered population
    population <- map_df(1:n_clusters, function(c) {
      cluster_mean <- rnorm(1, 50, 10)
      within_sd <- sqrt(100 * (1 - rho))
      
      tibble(
        cluster_id = c,
        unit_id = 1:cluster_size,
        value = rnorm(cluster_size, cluster_mean, within_sd)
      )
    })
    
    # Sample clusters
    sampled_clusters <- sample(1:n_clusters, n_psu)
    
    # Sample units within clusters
    sample_data <- population %>%
      filter(cluster_id %in% sampled_clusters) %>%
      group_by(cluster_id) %>%
      sample_n(min(n_ssu, n())) %>%
      ungroup()
    
    # Calculate estimate
    estimate <- mean(sample_data$value)
    
    tibble(
      sim = sim,
      estimate = estimate,
      n = nrow(sample_data)
    )
  })
  
  # True value
  true_mean <- 50
  
  # Calculate performance metrics
  performance <- results %>%
    summarise(
      mean_estimate = mean(estimate),
      bias = mean(estimate) - true_mean,
      se = sd(estimate),
      rmse = sqrt(mean((estimate - true_mean)^2)),
      cv = sd(estimate) / mean(estimate) * 100
    )
  
  # Calculate actual design effect
  srs_variance <- 100 / (n_psu * n_ssu)  # Theoretical SRS variance
  cluster_variance <- var(results$estimate)
  actual_deff <- cluster_variance / srs_variance
  
  performance$actual_deff <- actual_deff
  performance$theoretical_deff <- calculate_deff(n_ssu, rho)
  
  return(list(
    simulations = results,
    performance = performance
  ))
}

# ============================================
# 9. PRACTICAL TOOLS
# ============================================

#' Quick Cluster Design
#' @param budget Total budget
#' @param target_n Target sample size
#' @param urban_pct Percentage urban
#' @return Quick design recommendations
quick_cluster_design <- function(budget, target_n, urban_pct = 0.4) {
  
  # Standard cost assumptions
  c0 <- budget * 0.1  # 10% fixed costs
  
  # Cost ratios
  if (urban_pct > 0.5) {
    c1 <- 300  # Lower travel costs
    c2 <- 40   # Higher interview costs
    rho <- 0.10
  } else {
    c1 <- 500  # Higher travel costs
    c2 <- 30   # Lower interview costs
    rho <- 0.06
  }
  
  # Calculate optimal design
  result <- optimize_within_budget(budget, c0, c1, c2, target_n, rho)
  
  # Add recommendations
  result$recommendations <- list(
    cluster_size = paste(result$practical_cluster_size, "households"),
    n_psu = paste(result$n_psu, "PSUs"),
    total_sample = paste(result$total_sample, "households"),
    reserve_budget = paste("Keep 10% reserve:", scales::dollar(budget * 0.1))
  )
  
  return(result)
}

# ============================================
# 10. REPORTING FUNCTIONS
# ============================================

#' Generate Cluster Design Report
#' @param design Design object from design_two_stage
#' @param costs Cost parameters
#' @return Formatted report
generate_design_report <- function(design, costs) {
  
  report <- list()
  
  # Design summary
  report$summary <- design$summary
  
  # Cost breakdown
  n_psu <- design$summary$n_psu_selected
  n_ssu <- design$summary$n_ssu_per_psu
  
  report$costs <- tibble(
    Component = c("Fixed", "PSU Travel", "Interviews", "Total"),
    Amount = c(
      costs$c0,
      costs$c1 * n_psu,
      costs$c2 * n_psu * n_ssu,
      costs$c0 + costs$c1 * n_psu + costs$c2 * n_psu * n_ssu
    )
  )
  
  # Efficiency metrics
  report$efficiency <- tibble(
    Metric = c("Cost per interview", "Cost per PSU", 
               "Interviews per day", "Design effect"),
    Value = c(
      sum(report$costs$Amount) / (n_psu * n_ssu),
      costs$c1 + costs$c2 * n_ssu,
      n_ssu / 3,  # Assume 3 days per PSU
      design$summary$estimated_deff
    )
  )
  
  class(report) <- "cluster_design_report"
  return(report)
}

# Print method for report
print.cluster_design_report <- function(x, ...) {
  cat("CLUSTER SAMPLE DESIGN REPORT\n")
  cat("============================\n\n")
  
  cat("DESIGN SUMMARY\n")
  cat("--------------\n")
  print(x$summary)
  
  cat("\nCOST BREAKDOWN\n")
  cat("--------------\n")
  print(x$costs)
  
  cat("\nEFFICIENCY METRICS\n")
  cat("------------------\n")
  print(x$efficiency)
}

# ============================================
# EXAMPLE USAGE
# ============================================

# Example: Complete cluster design workflow
run_cluster_example <- function() {
  
  # Set parameters
  budget <- 500000
  target_n <- 3000
  
  # Find optimal design
  cat("Finding optimal design...\n")
  optimal <- optimize_within_budget(
    budget = budget,
    c0 = 50000,
    c1 = 400,
    c2 = 35,
    target_n = target_n,
    rho = 0.08
  )
  
  print(optimal)
  
  # Create PSU frame
  cat("\nCreating PSU frame...\n")
  psu_frame <- tibble(
    psu_id = 1:500,
    stratum = rep(c("Urban", "Rural"), c(200, 300)),
    households = sample(50:500, 500, replace = TRUE)
  )
  
  # Design sample
  cat("\nDesigning two-stage sample...\n")
  design <- design_two_stage(
    frame = psu_frame,
    n_psu = optimal$n_psu,
    n_ssu = optimal$practical_cluster_size,
    stratify_by = "stratum",
    size_var = "households"
  )
  
  # Generate report
  cat("\nGenerating report...\n")
  costs <- list(c0 = 50000, c1 = 400, c2 = 35)
  report <- generate_design_report(design, costs)
  print(report)
  
  # Run simulation
  cat("\nRunning simulation (100 iterations)...\n")
  sim_results <- simulate_cluster_sampling(
    pop_size = 100000,
    n_clusters = 500,
    rho = 0.08,
    n_psu = optimal$n_psu,
    n_ssu = optimal$practical_cluster_size,
    n_sims = 100
  )
  
  cat("Simulation Results:\n")
  print(sim_results$performance)
  
  return(list(
    optimal = optimal,
    design = design,
    report = report,
    simulation = sim_results
  ))
}

# Print success message
cat("Module 3 Functions Loaded Successfully!\n")
cat("Key functions available:\n")
cat("- calculate_cost()\n")
cat("- optimal_cluster_size()\n")
cat("- systematic_pps()\n")
cat("- design_two_stage()\n")
cat("- optimize_within_budget()\n")
cat("- quick_cluster_design()\n")
cat("\nRun run_cluster_example() for complete demonstration\n")