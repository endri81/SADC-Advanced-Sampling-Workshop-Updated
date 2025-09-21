# Module 2: Stratification Mastery - Supporting Functions
# Harry's Workshop - Day 2, Tuesday
# SADC Advanced Sampling Workshop

# Load required libraries
library(tidyverse)
library(sampling)
library(survey)
library(PracTools)

# ============================================
# 1. STRATIFICATION TOOLS
# ============================================

#' Calculate Neyman Allocation
#' @param N_h Vector of stratum populations
#' @param S_h Vector of stratum standard deviations
#' @param n Total sample size
#' @return Vector of sample sizes by stratum
neyman_allocation <- function(N_h, S_h, n) {
  NS <- N_h * S_h
  proportions <- NS / sum(NS)
  n_h <- round(proportions * n)
  return(n_h)
}

#' Calculate Cost-Optimal Neyman Allocation
#' @param N_h Vector of stratum populations
#' @param S_h Vector of stratum standard deviations
#' @param C_h Vector of costs per unit by stratum
#' @param n Total sample size
#' @return Vector of sample sizes by stratum
cost_optimal_allocation <- function(N_h, S_h, C_h, n) {
  NS_C <- N_h * S_h / sqrt(C_h)
  proportions <- NS_C / sum(NS_C)
  n_h <- round(proportions * n)
  return(n_h)
}

#' Apply Minimum Sample Constraints
#' @param n_h Vector of calculated sample sizes
#' @param min_n Minimum sample per stratum
#' @param total_n Total sample size to maintain
#' @return Adjusted sample sizes
apply_minimums <- function(n_h, min_n = 50, total_n = NULL) {
  # Apply minimums
  n_adjusted <- pmax(n_h, min_n)
  
  # If total specified, scale to match
  if (!is.null(total_n)) {
    if (sum(n_adjusted) != total_n) {
      # Proportionally adjust strata above minimum
      above_min <- n_adjusted > min_n
      excess <- total_n - sum(n_adjusted[!above_min])
      n_adjusted[above_min] <- round(n_adjusted[above_min] * 
                                       excess / sum(n_adjusted[above_min]))
    }
  }
  return(n_adjusted)
}

# ============================================
# 2. VARIANCE CALCULATIONS
# ============================================

#' Calculate Stratified Sampling Variance
#' @param N_h Stratum populations
#' @param S_h Stratum standard deviations
#' @param n_h Stratum sample sizes
#' @return Variance of stratified mean
stratified_variance <- function(N_h, S_h, n_h) {
  N <- sum(N_h)
  W_h <- N_h / N
  f_h <- n_h / N_h
  
  variance <- sum(W_h^2 * S_h^2 / n_h * (1 - f_h))
  return(variance)
}

#' Compare Allocation Methods
#' @param data Data frame with N_h, S_h, C_h columns
#' @param n Total sample size
#' @return Comparison table
compare_allocations <- function(data, n) {
  
  results <- data %>%
    mutate(
      # Equal allocation
      n_equal = round(n / nrow(data)),
      
      # Proportional allocation
      n_prop = round(n * N_h / sum(N_h)),
      
      # Neyman allocation
      NS = N_h * S_h,
      n_neyman = round(n * NS / sum(NS)),
      
      # Cost-optimal allocation
      NS_C = N_h * S_h / sqrt(C_h),
      n_cost = round(n * NS_C / sum(NS_C))
    ) %>%
    select(-NS, -NS_C)
  
  # Calculate costs
  costs <- tibble(
    Method = c("Equal", "Proportional", "Neyman", "Cost-Optimal"),
    Total_Cost = c(
      sum(results$n_equal * results$C_h),
      sum(results$n_prop * results$C_h),
      sum(results$n_neyman * results$C_h),
      sum(results$n_cost * results$C_h)
    )
  )
  
  # Calculate variances (simplified)
  variances <- tibble(
    Method = c("Equal", "Proportional", "Neyman", "Cost-Optimal"),
    Variance = c(
      stratified_variance(results$N_h, results$S_h, results$n_equal),
      stratified_variance(results$N_h, results$S_h, results$n_prop),
      stratified_variance(results$N_h, results$S_h, results$n_neyman),
      stratified_variance(results$N_h, results$S_h, results$n_cost)
    )
  )
  
  comparison <- costs %>%
    left_join(variances, by = "Method") %>%
    mutate(
      CV = sqrt(Variance) / 100,  # Simplified CV calculation
      Efficiency = min(Total_Cost) / Total_Cost * min(Variance) / Variance
    )
  
  return(list(
    allocations = results,
    comparison = comparison
  ))
}

# ============================================
# 3. PRACTICAL DESIGN FUNCTIONS
# ============================================

#' Create Stratification Scheme
#' @param frame Survey frame data
#' @param geo_var Geographic variable name
#' @param urban_var Urban/rural variable name
#' @param other_vars Other stratification variables
#' @return Frame with stratum identifiers
create_strata <- function(frame, geo_var, urban_var = NULL, other_vars = NULL) {
  
  # Start with geographic
  frame$stratum <- frame[[geo_var]]
  
  # Add urban/rural if provided
  if (!is.null(urban_var)) {
    frame$stratum <- paste(frame$stratum, frame[[urban_var]], sep = "_")
  }
  
  # Add other variables if provided
  if (!is.null(other_vars)) {
    for (var in other_vars) {
      frame$stratum <- paste(frame$stratum, frame[[var]], sep = "_")
    }
  }
  
  # Create stratum ID
  frame$stratum_id <- as.numeric(as.factor(frame$stratum))
  
  return(frame)
}

#' Calculate Stratum Statistics
#' @param frame Survey frame with stratum variable
#' @param target_var Variable of interest
#' @param stratum_var Name of stratum variable
#' @return Summary statistics by stratum
stratum_statistics <- function(frame, target_var, stratum_var = "stratum") {
  
  stats <- frame %>%
    group_by(!!sym(stratum_var)) %>%
    summarise(
      N_h = n(),
      Mean_h = mean(!!sym(target_var), na.rm = TRUE),
      S_h = sd(!!sym(target_var), na.rm = TRUE),
      CV_h = S_h / Mean_h,
      Min = min(!!sym(target_var), na.rm = TRUE),
      Max = max(!!sym(target_var), na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(
      W_h = N_h / sum(N_h)
    )
  
  return(stats)
}

# ============================================
# 4. SIMULATION FUNCTIONS
# ============================================

#' Simulate Stratified Sample Performance
#' @param population Population data frame
#' @param strata_var Stratification variable
#' @param target_var Variable to estimate
#' @param n_sims Number of simulations
#' @param sample_sizes Named vector of sample sizes by stratum
#' @return Simulation results
simulate_stratified <- function(population, strata_var, target_var, 
                                n_sims = 100, sample_sizes) {
  
  results <- vector("list", n_sims)
  
  for (i in 1:n_sims) {
    # Sample from each stratum
    sample_data <- population %>%
      group_by(!!sym(strata_var)) %>%
      sample_n(size = sample_sizes[cur_group()[[strata_var]]], 
               replace = FALSE) %>%
      ungroup()
    
    # Calculate estimate
    estimate <- sample_data %>%
      group_by(!!sym(strata_var)) %>%
      summarise(
        mean_h = mean(!!sym(target_var)),
        n_h = n(),
        .groups = 'drop'
      ) %>%
      left_join(
        population %>%
          group_by(!!sym(strata_var)) %>%
          summarise(N_h = n(), .groups = 'drop'),
        by = strata_var
      ) %>%
      mutate(W_h = N_h / sum(N_h)) %>%
      summarise(estimate = sum(W_h * mean_h)) %>%
      pull(estimate)
    
    results[[i]] <- estimate
  }
  
  # Summarize results
  true_mean <- mean(population[[target_var]])
  estimates <- unlist(results)
  
  summary_stats <- tibble(
    True_Value = true_mean,
    Mean_Estimate = mean(estimates),
    Bias = mean(estimates) - true_mean,
    SE = sd(estimates),
    CV = sd(estimates) / mean(estimates) * 100,
    Coverage_95 = mean(abs(estimates - true_mean) < 1.96 * sd(estimates))
  )
  
  return(list(
    estimates = estimates,
    summary = summary_stats
  ))
}

# ============================================
# 5. POST-STRATIFICATION
# ============================================

#' Apply Post-Stratification Weights
#' @param sample_data Sample data with strata
#' @param population_counts True population by stratum
#' @param stratum_var Name of stratum variable
#' @param weight_var Name of existing weight variable
#' @return Data with post-stratification weights
post_stratify <- function(sample_data, population_counts, 
                          stratum_var = "stratum", 
                          weight_var = "weight") {
  
  # Calculate sample counts
  sample_counts <- sample_data %>%
    group_by(!!sym(stratum_var)) %>%
    summarise(n_sample = n(), .groups = 'drop')
  
  # Join with population counts
  adjustment <- population_counts %>%
    left_join(sample_counts, by = stratum_var) %>%
    mutate(
      ps_factor = N_pop / n_sample
    )
  
  # Apply adjustment to weights
  sample_data <- sample_data %>%
    left_join(adjustment %>% select(!!sym(stratum_var), ps_factor),
              by = stratum_var) %>%
    mutate(
      ps_weight = !!sym(weight_var) * ps_factor
    )
  
  return(sample_data)
}

# ============================================
# 6. QUALITY CHECKS
# ============================================

#' Check Stratification Quality
#' @param frame Frame with strata
#' @param stratum_var Stratum variable name
#' @param min_size Minimum acceptable stratum size
#' @return Quality assessment
check_strata_quality <- function(frame, stratum_var = "stratum", min_size = 50) {
  
  quality <- frame %>%
    group_by(!!sym(stratum_var)) %>%
    summarise(
      n = n(),
      .groups = 'drop'
    ) %>%
    mutate(
      adequate_size = n >= min_size,
      percent_of_total = n / sum(n) * 100
    ) %>%
    arrange(n)
  
  summary <- list(
    total_strata = nrow(quality),
    small_strata = sum(!quality$adequate_size),
    smallest_size = min(quality$n),
    largest_size = max(quality$n),
    size_ratio = max(quality$n) / min(quality$n)
  )
  
  return(list(
    details = quality,
    summary = summary,
    warnings = quality %>% filter(!adequate_size)
  ))
}

# ============================================
# 7. EXAMPLE USAGE
# ============================================

# Example: Complete stratification workflow
run_stratification_example <- function() {
  
  # Generate example data
  set.seed(2024)
  population <- tibble(
    id = 1:10000,
    region = sample(c("North", "South", "East", "West"), 10000, 
                    replace = TRUE, prob = c(0.3, 0.3, 0.2, 0.2)),
    urban = sample(c("Urban", "Rural"), 10000, 
                   replace = TRUE, prob = c(0.4, 0.6)),
    income = rlnorm(10000, log(30000), 0.6)
  )
  
  # Create strata
  population <- create_strata(population, "region", "urban")
  
  # Calculate statistics
  strata_stats <- stratum_statistics(population, "income", "stratum")
  
  # Add costs (urban costs more)
  strata_stats <- strata_stats %>%
    mutate(
      C_h = ifelse(grepl("Urban", stratum), 60, 30)
    )
  
  # Compare allocations
  comparison <- compare_allocations(strata_stats, n = 1000)
  
  # Run simulation with optimal allocation
  sample_sizes <- setNames(comparison$allocations$n_cost, 
                           comparison$allocations$stratum)
  
  sim_results <- simulate_stratified(
    population, "stratum", "income", 
    n_sims = 100, sample_sizes
  )
  
  # Check quality
  quality <- check_strata_quality(population, "stratum", min_size = 200)
  
  return(list(
    population = population,
    strata_stats = strata_stats,
    comparison = comparison,
    simulation = sim_results,
    quality = quality
  ))
}

# ============================================
# 8. VISUALIZATION FUNCTIONS
# ============================================

#' Plot Allocation Comparison
#' @param comparison_result Result from compare_allocations
#' @return ggplot object
plot_allocation_comparison <- function(comparison_result) {
  
  p1 <- comparison_result$comparison %>%
    ggplot(aes(x = Method, y = Total_Cost)) +
    geom_col(fill = "#002244", alpha = 0.8) +
    scale_y_continuous(labels = scales::dollar) +
    labs(title = "Total Cost by Allocation Method",
         y = "Total Cost (USD)") +
    theme_minimal()
  
  p2 <- comparison_result$comparison %>%
    ggplot(aes(x = Method, y = CV)) +
    geom_col(fill = "#004494", alpha = 0.8) +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Coefficient of Variation by Method",
         y = "CV (%)") +
    theme_minimal()
  
  return(list(cost_plot = p1, cv_plot = p2))
}

# Print message for successful load
cat("Module 2 Functions Loaded Successfully!\n")
cat("Available functions:\n")
cat("- neyman_allocation()\n")
cat("- cost_optimal_allocation()\n")
cat("- compare_allocations()\n")
cat("- simulate_stratified()\n")
cat("- create_strata()\n")
cat("- post_stratify()\n")
cat("\nRun run_stratification_example() for a complete demo\n")