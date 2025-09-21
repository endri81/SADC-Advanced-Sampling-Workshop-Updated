# Module 4: Multi-Stage Integration - Supporting Functions
# Harry's Workshop - Day 2, Tuesday
# SADC Advanced Sampling Workshop
# Following World Bank, Eurostat, OECD, UN Standards

library(tidyverse)
library(survey)
library(sampling)

# ============================================
# 1. MULTI-STAGE DESIGN FUNCTIONS
# ============================================

#' Design Multi-Stage Sample
#' @param population Total population
#' @param stages Number of stages (2-5)
#' @param units_per_stage List of units at each stage
#' @param method_per_stage Selection method for each stage
#' @return Design specification
design_multistage <- function(population, 
                              stages = 3,
                              units_per_stage = NULL,
                              method_per_stage = NULL) {
  
  # Default World Bank standard design
  if (is.null(units_per_stage)) {
    if (stages == 2) {
      units_per_stage <- list(
        stage1 = "PSUs",
        stage2 = "Households"
      )
    } else if (stages == 3) {
      units_per_stage <- list(
        stage1 = "Districts",
        stage2 = "PSUs", 
        stage3 = "Households"
      )
    } else if (stages == 4) {
      units_per_stage <- list(
        stage1 = "Provinces",
        stage2 = "Districts",
        stage3 = "PSUs",
        stage4 = "Households"
      )
    }
  }
  
  # Default selection methods (Eurostat standard)
  if (is.null(method_per_stage)) {
    if (stages == 2) {
      method_per_stage <- c("PPS", "Systematic")
    } else if (stages == 3) {
      method_per_stage <- c("Certainty", "PPS", "Systematic")
    } else if (stages == 4) {
      method_per_stage <- c("Certainty", "PPS", "PPS", "Systematic")
    }
  }
  
  design <- list(
    population = population,
    stages = stages,
    units = units_per_stage,
    methods = method_per_stage,
    timestamp = Sys.time()
  )
  
  class(design) <- "multistage_design"
  return(design)
}

# ============================================
# 2. PROBABILITY CALCULATIONS
# ============================================

#' Calculate Selection Probabilities
#' @param stage_specs List of stage specifications
#' @return Overall selection probability
calculate_multistage_prob <- function(stage_specs) {
  
  # Extract probabilities for each stage
  probs <- map_dbl(stage_specs, ~ .x$probability)
  
  # Overall probability is product
  overall_prob <- prod(probs)
  
  # Also return by stage
  result <- list(
    stage_probs = probs,
    overall_prob = overall_prob,
    weight = 1 / overall_prob
  )
  
  return(result)
}

#' Calculate Weights for Multi-Stage Design
#' @param data Survey data with stage identifiers
#' @param stage_vars Vector of stage variable names
#' @param size_vars Vector of size variables for PPS stages
#' @return Data with calculated weights
calculate_multistage_weights <- function(data, stage_vars, size_vars = NULL) {
  
  # Initialize weight components
  data$weight_component <- 1
  
  # For each stage, calculate selection probability
  for (i in seq_along(stage_vars)) {
    
    stage_var <- stage_vars[i]
    
    if (!is.null(size_vars) && i <= length(size_vars)) {
      # PPS selection
      size_var <- size_vars[i]
      
      # Calculate probability based on size
      stage_totals <- data %>%
        group_by(!!sym(stage_var)) %>%
        summarise(
          n_selected = n_distinct(!!sym(stage_var)),
          total_size = sum(!!sym(size_var)),
          .groups = 'drop'
        )
      
      data <- data %>%
        left_join(stage_totals, by = stage_var) %>%
        mutate(
          stage_prob = n_selected * !!sym(size_var) / total_size,
          weight_component = weight_component / stage_prob
        )
      
    } else {
      # Equal probability selection
      n_units <- n_distinct(data[[stage_var]])
      n_total <- 1000  # Would need frame size
      
      stage_prob <- n_units / n_total
      data$weight_component <- data$weight_component / stage_prob
    }
  }
  
  data$final_weight <- data$weight_component
  
  return(data)
}

# ============================================
# 3. PPS SELECTION FUNCTIONS
# ============================================

#' Systematic PPS Selection for Multi-Stage
#' @param frame Frame with units and sizes
#' @param n Number to select
#' @param size_var Size variable name
#' @param id_var ID variable name
#' @return Selected units with probabilities
systematic_pps_multistage <- function(frame, n, size_var, id_var) {
  
  # Sort by implicit stratification variables if available
  if ("region" %in% names(frame)) {
    frame <- frame %>% arrange(region, !!sym(size_var))
  }
  
  # Calculate cumulative sizes
  frame <- frame %>%
    mutate(
      cumsize = cumsum(!!sym(size_var)),
      cumsize_prev = lag(cumsize, default = 0)
    )
  
  # Total and interval
  total_size <- sum(frame[[size_var]])
  interval <- total_size / n
  
  # Random start (UN standard)
  start <- runif(1, min = 0.0001, max = interval)
  
  # Selection points
  selection_points <- start + (0:(n-1)) * interval
  
  # Select units
  selected <- map_df(selection_points, function(point) {
    frame %>%
      filter(cumsize >= point) %>%
      slice(1) %>%
      mutate(
        selection_point = point,
        selection_order = which(selection_points == point)
      )
  })
  
  # Add probabilities
  selected <- selected %>%
    mutate(
      inclusion_prob = n * !!sym(size_var) / total_size,
      base_weight = 1 / inclusion_prob
    )
  
  return(selected)
}

# ============================================
# 4. VARIANCE ESTIMATION
# ============================================

#' Setup Survey Design for Multi-Stage
#' @param data Survey data
#' @param stage_ids Vector of stage ID variables
#' @param strata Stratification variable
#' @param weights Weight variable
#' @param fpc Finite population corrections (optional)
#' @return Survey design object
setup_multistage_design <- function(data, stage_ids, strata = NULL, 
                                    weights = "weight", fpc = NULL) {
  
  # Create ID formula
  if (length(stage_ids) == 1) {
    id_formula <- as.formula(paste("~", stage_ids[1]))
  } else {
    id_formula <- as.formula(paste("~", paste(stage_ids, collapse = " + ")))
  }
  
  # Create design
  if (!is.null(strata)) {
    design <- svydesign(
      ids = id_formula,
      strata = as.formula(paste("~", strata)),
      weights = as.formula(paste("~", weights)),
      data = data,
      nest = TRUE
    )
  } else {
    design <- svydesign(
      ids = id_formula,
      weights = as.formula(paste("~", weights)),
      data = data
    )
  }
  
  return(design)
}

#' Calculate Design Effect for Multi-Stage
#' @param design Survey design object
#' @param variable Variable to analyze
#' @return DEFF and related statistics
calculate_multistage_deff <- function(design, variable) {
  
  # Calculate with design effect
  result <- svymean(as.formula(paste("~", variable)), 
                    design, 
                    deff = TRUE, 
                    na.rm = TRUE)
  
  # Extract components
  deff_value <- deff(result)
  se_value <- SE(result)
  mean_value <- coef(result)
  
  # Calculate effective sample size
  n_actual <- sum(weights(design) > 0)
  n_effective <- n_actual / deff_value
  
  output <- list(
    mean = mean_value,
    se = se_value,
    deff = deff_value,
    n_actual = n_actual,
    n_effective = n_effective,
    cv = (se_value / mean_value) * 100
  )
  
  return(output)
}

# ============================================
# 5. SAMPLE SIZE CALCULATIONS
# ============================================

#' Calculate Sample Size for Multi-Stage Design
#' @param cv_target Target coefficient of variation
#' @param deff_expected Expected design effect
#' @param response_rate Expected response rate
#' @param stages Number of stages
#' @return Required sample sizes
calculate_multistage_sample_size <- function(cv_target = 0.05,
                                             deff_expected = 1.5,
                                             response_rate = 0.85,
                                             stages = 3) {
  
  # Base sample size for SRS
  # Using approximation CV = 1/sqrt(n) for proportion = 0.5
  n_srs <- (1 / cv_target)^2
  
  # Adjust for design effect
  n_complex <- n_srs * deff_expected
  
  # Adjust for non-response
  n_adjusted <- n_complex / response_rate
  
  # Stage-specific allocations (World Bank approach)
  if (stages == 2) {
    n_psu <- ceiling(n_adjusted / 20)  # 20 HH per PSU typical
    n_ssu <- 20
    
    result <- list(
      total_sample = n_adjusted,
      n_psu = n_psu,
      n_per_psu = n_ssu,
      total_planned = n_psu * n_ssu
    )
    
  } else if (stages == 3) {
    n_psu <- ceiling(sqrt(n_adjusted * 2))  # Rule of thumb
    n_ssu <- ceiling(n_adjusted / n_psu)
    
    result <- list(
      total_sample = n_adjusted,
      n_districts = "All or subset",
      n_psu = n_psu,
      n_per_psu = n_ssu,
      total_planned = n_psu * n_ssu
    )
  }
  
  return(result)
}

# ============================================
# 6. COST OPTIMIZATION
# ============================================

#' Optimize Multi-Stage Design for Budget
#' @param budget Total budget
#' @param cost_structure List of costs by stage
#' @param target_cv Target CV
#' @param population Population size
#' @return Optimal design
optimize_multistage_budget <- function(budget, 
                                       cost_structure,
                                       target_cv = 0.05,
                                       population = 1000000) {
  
  # Default cost structure (World Bank estimates)
  if (is.null(cost_structure)) {
    cost_structure <- list(
      fixed = budget * 0.15,
      per_psu = 500,
      per_hh = 35,
      listing = 100  # Per PSU
    )
  }
  
  # Available for sampling
  available <- budget - cost_structure$fixed
  
  # Grid search for optimal combination
  designs <- expand.grid(
    n_psu = seq(50, 300, by = 10),
    n_per_psu = seq(10, 30, by = 2)
  )
  
  designs <- designs %>%
    mutate(
      total_sample = n_psu * n_per_psu,
      total_cost = cost_structure$per_psu * n_psu + 
        cost_structure$per_hh * total_sample +
        cost_structure$listing * n_psu,
      deff_est = 1 + (n_per_psu - 1) * 0.08,  # Assume ICC = 0.08
      cv_est = sqrt(deff_est / total_sample)
    ) %>%
    filter(total_cost <= available)
  
  # Find optimal
  optimal <- designs %>%
    filter(cv_est <= target_cv) %>%
    arrange(total_cost) %>%
    slice(1)
  
  if (nrow(optimal) == 0) {
    # Can't achieve target
    optimal <- designs %>%
      arrange(cv_est) %>%
      slice(1)
    
    warning("Cannot achieve target CV within budget")
  }
  
  result <- list(
    design = optimal,
    total_cost = cost_structure$fixed + optimal$total_cost,
    cv_achieved = optimal$cv_est,
    recommendations = paste(
      "Select", optimal$n_psu, "PSUs with",
      optimal$n_per_psu, "households each"
    )
  )
  
  return(result)
}

# ============================================
# 7. SIMULATION FUNCTIONS
# ============================================

#' Simulate Multi-Stage Sample Performance
#' @param design_params Design parameters
#' @param n_sims Number of simulations
#' @param true_mean True population mean
#' @param true_var True population variance
#' @return Simulation results
simulate_multistage <- function(design_params, 
                                n_sims = 100,
                                true_mean = 50,
                                true_var = 100) {
  
  results <- vector("list", n_sims)
  
  for (sim in 1:n_sims) {
    # Generate hierarchical population
    # Districts -> PSUs -> Households
    
    # District effects
    n_districts <- 20
    district_effects <- rnorm(n_districts, 0, sqrt(true_var * 0.2))
    
    # PSU effects within districts
    n_psu_per_district <- 25
    psu_data <- expand.grid(
      district = 1:n_districts,
      psu = 1:n_psu_per_district
    ) %>%
      mutate(
        district_effect = district_effects[district],
        psu_effect = rnorm(n(), 0, sqrt(true_var * 0.3))
      )
    
    # Household data
    n_hh_per_psu <- 50
    pop_data <- expand.grid(
      district = 1:n_districts,
      psu = 1:n_psu_per_district,
      hh = 1:n_hh_per_psu
    ) %>%
      left_join(psu_data, by = c("district", "psu")) %>%
      mutate(
        hh_value = true_mean + district_effect + psu_effect + 
          rnorm(n(), 0, sqrt(true_var * 0.5))
      )
    
    # Sample selection
    # Stage 1: Select districts (certainty or sample)
    if (design_params$n_districts == "all") {
      sampled_districts <- unique(pop_data$district)
    } else {
      sampled_districts <- sample(unique(pop_data$district), 
                                  design_params$n_districts)
    }
    
    # Stage 2: Select PSUs within districts
    sampled_psus <- pop_data %>%
      filter(district %in% sampled_districts) %>%
      group_by(district) %>%
      summarise(psus = list(unique(psu)), .groups = 'drop') %>%
      mutate(
        selected_psus = map(psus, ~ sample(.x, 
                                           min(design_params$psu_per_district, length(.x))))
      ) %>%
      unnest(selected_psus) %>%
      rename(psu = selected_psus)
    
    # Stage 3: Select households
    sample_data <- pop_data %>%
      inner_join(sampled_psus, by = c("district", "psu")) %>%
      group_by(district, psu) %>%
      sample_n(min(design_params$hh_per_psu, n())) %>%
      ungroup()
    
    # Calculate estimate
    estimate <- mean(sample_data$hh_value)
    
    results[[sim]] <- tibble(
      sim = sim,
      estimate = estimate,
      n = nrow(sample_data)
    )
  }
  
  # Combine results
  all_results <- bind_rows(results)
  
  # Calculate performance metrics
  performance <- all_results %>%
    summarise(
      mean_estimate = mean(estimate),
      bias = mean(estimate) - true_mean,
      se = sd(estimate),
      mse = mean((estimate - true_mean)^2),
      cv = sd(estimate) / mean(estimate) * 100,
      coverage_95 = mean(abs(estimate - true_mean) <= 1.96 * sd(estimate))
    )
  
  return(list(
    simulations = all_results,
    performance = performance
  ))
}

# ============================================
# 8. DOCUMENTATION FUNCTIONS
# ============================================

#' Generate Multi-Stage Design Documentation
#' @param design Design object
#' @param output_format Format for output (text, html, latex)
#' @return Documentation text
document_multistage_design <- function(design, output_format = "text") {
  
  doc <- list()
  
  doc$title <- "MULTI-STAGE SAMPLE DESIGN DOCUMENTATION"
  doc$date <- Sys.Date()
  doc$standard <- "Following World Bank LSMS, Eurostat EU-SILC, OECD Standards"
  
  # Design summary
  doc$summary <- paste(
    "Number of stages:", design$stages,
    "\nTotal sample size:", design$total_sample,
    "\nExpected design effect:", round(design$deff_expected, 2)
  )
  
  # Stage details
  doc$stages <- map_chr(1:design$stages, function(i) {
    paste0(
      "Stage ", i, ": ",
      "Units = ", design$units[[i]],
      ", Method = ", design$methods[i],
      ", Sample = ", design$sample_sizes[i]
    )
  })
  
  # Weight calculation
  doc$weights <- paste(
    "Base weights = 1 / (product of selection probabilities)",
    "\nNon-response adjustment applied",
    "\nPost-stratification to population totals"
  )
  
  # Quality measures
  doc$quality <- paste(
    "Target CV:", design$target_cv,
    "\nExpected response rate:", design$response_rate,
    "\nMinimum domain sample:", design$min_domain
  )
  
  # Format output
  if (output_format == "text") {
    output <- paste(
      doc$title, "\n",
      paste(rep("=", 50), collapse = ""), "\n",
      "Date:", doc$date, "\n",
      doc$standard, "\n\n",
      "DESIGN SUMMARY\n",
      doc$summary, "\n\n",
      "STAGE DETAILS\n",
      paste(doc$stages, collapse = "\n"), "\n\n",
      "WEIGHT CALCULATION\n",
      doc$weights, "\n\n",
      "QUALITY MEASURES\n",
      doc$quality
    )
  }
  
  return(output)
}

# ============================================
# 9. PRACTICAL TOOLS
# ============================================

#' Quick Multi-Stage Designer
#' @param country_name Country identifier
#' @param population Population size
#' @param budget Budget in USD
#' @param districts Number of districts
#' @return Quick design recommendation
quick_multistage_design <- function(country_name = "Country X",
                                    population = 10000000,
                                    budget = 1000000,
                                    districts = 50) {
  
  # Determine number of stages based on size
  if (population < 1000000) {
    stages <- 2
  } else if (population < 10000000) {
    stages <- 3
  } else {
    stages <- 4
  }
  
  # Quick calculations
  cost_per_interview <- 40
  affordable_n <- (budget * 0.85) / cost_per_interview
  
  # Stage allocation
  if (stages == 2) {
    n_psu <- ceiling(affordable_n / 20)
    design_text <- paste(n_psu, "PSUs x 20 HH")
  } else if (stages == 3) {
    n_psu <- ceiling(sqrt(affordable_n * 1.5))
    n_hh <- ceiling(affordable_n / n_psu)
    design_text <- paste("All districts,", n_psu, "PSUs,", n_hh, "HH each")
  } else {
    design_text <- "4-stage: Provinces > Districts > PSUs > HH"
  }
  
  result <- list(
    country = country_name,
    recommendation = paste(stages, "stage design"),
    design = design_text,
    total_sample = floor(affordable_n),
    estimated_cv = round(sqrt(1.5 / affordable_n) * 100, 2),
    note = "Based on World Bank standard assumptions"
  )
  
  return(result)
}

# ============================================
# 10. INTERACTIVE EXERCISES
# ============================================

#' DataCamp-Style Exercise: Weight Calculation
#' @param show_solution Show the solution
#' @return Exercise prompt and solution
exercise_weight_calculation <- function(show_solution = FALSE) {
  
  cat("=== DATACAMP EXERCISE ===\n")
  cat("Calculate the weight for a household selected through:\n")
  cat("- Stage 1: Province (certainty)\n")
  cat("- Stage 2: 8 PSUs from 120 (PPS)\n")
  cat("- Stage 3: 2 segments from 5\n")
  cat("- Stage 4: 15 HH from 200\n\n")
  
  if (!show_solution) {
    cat("# Your code here:\n")
    cat("stage1_prob <- 1.0\n")
    cat("stage2_prob <- 8/120\n")
    cat("stage3_prob <- ___  # Fill this\n")
    cat("stage4_prob <- 15/200\n")
    cat("weight <- ___  # Calculate this\n")
    
  } else {
    cat("# SOLUTION:\n")
    stage1_prob <- 1.0
    stage2_prob <- 8/120
    stage3_prob <- 2/5
    stage4_prob <- 15/200
    
    overall_prob <- stage1_prob * stage2_prob * stage3_prob * stage4_prob
    weight <- 1 / overall_prob
    
    cat("stage1_prob <- 1.0\n")
    cat("stage2_prob <- 8/120\n")
    cat("stage3_prob <- 2/5\n")
    cat("stage4_prob <- 15/200\n")
    cat("overall_prob <-", overall_prob, "\n")
    cat("weight <-", round(weight, 2), "\n")
    cat("\nThis household represents", round(weight), "households\n")
  }
}

# ============================================
# EXAMPLE USAGE
# ============================================

# Complete workflow example
run_multistage_example <- function() {
  
  cat("MULTI-STAGE DESIGN EXAMPLE\n")
  cat("==========================\n\n")
  
  # Set parameters
  cat("Country parameters:\n")
  params <- list(
    population = 15000000,
    districts = 45,
    urban_pct = 38,
    budget = 1200000
  )
  print(params)
  
  # Quick design
  cat("\n1. Quick design recommendation:\n")
  quick <- quick_multistage_design(
    country_name = "Example Country",
    population = params$population,
    budget = params$budget,
    districts = params$districts
  )
  print(quick)
  
  # Optimize design
  cat("\n2. Optimizing within budget:\n")
  optimal <- optimize_multistage_budget(
    budget = params$budget,
    cost_structure = NULL,
    target_cv = 0.03
  )
  print(optimal$design)
  
  # Simulate performance
  cat("\n3. Simulating performance (100 iterations):\n")
  sim_params <- list(
    n_districts = "all",
    psu_per_district = 3,
    hh_per_psu = 20
  )
  
  sim_results <- simulate_multistage(
    design_params = sim_params,
    n_sims = 100
  )
  print(sim_results$performance)
  
  # Generate documentation
  cat("\n4. Documentation template:\n")
  design_doc <- list(
    stages = 3,
    units = list("Districts", "PSUs", "Households"),
    methods = c("Certainty", "PPS", "Systematic"),
    sample_sizes = c(45, 135, 20),
    total_sample = 2700,
    deff_expected = 1.8,
    target_cv = 0.03,
    response_rate = 0.85,
    min_domain = 500
  )
  
  doc_text <- document_multistage_design(design_doc)
  cat(substr(doc_text, 1, 500), "...\n")
  
  cat("\n=== EXAMPLE COMPLETE ===\n")
}

# Print success message
cat("Module 4 Functions Loaded Successfully!\n")
cat("Key functions available:\n")
cat("- design_multistage()\n")
cat("- calculate_multistage_weights()\n")
cat("- systematic_pps_multistage()\n")
cat("- optimize_multistage_budget()\n")
cat("- simulate_multistage()\n")
cat("- quick_multistage_design()\n")
cat("- exercise_weight_calculation()\n")
cat("\nRun run_multistage_example() for complete demonstration\n")
cat("Run exercise_weight_calculation() for DataCamp exercise\n")