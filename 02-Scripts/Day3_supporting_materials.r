# ============================================================================
# ADVANCED SAMPLING METHODS - LECTURE 3
# Supporting Materials: Scripts, Exercises, and Solutions
# Complex Household Survey Sampling
# ============================================================================

# ============================================================================
# SECTION 1: DATA GENERATION SCRIPT
# Generate realistic SADC household survey dataset
# ============================================================================

generate_sadc_survey_data <- function(n_households = 5000, 
                                     n_provinces = 8,
                                     n_ea_per_province = 31,
                                     seed = 2024) {
  
  set.seed(seed)
  library(tidyverse)
  
  # Generate Enumeration Areas (EAs)
  ea_data <- expand.grid(
    province = paste0("P", 1:n_provinces),
    ea_number = 1:n_ea_per_province
  ) %>%
    mutate(
      ea_id = paste0("EA", sprintf("%03d", row_number())),
      urban_rural = sample(c("Urban", "Rural"), 
                          n(), 
                          replace = TRUE, 
                          prob = c(0.45, 0.55)),
      stratum = paste(province, urban_rural, sep = "_")
    )
  
  # Generate households
  households <- data.frame(
    household_id = paste0("HH", sprintf("%05d", 1:n_households))
  )
  
  # Assign households to EAs (20 per EA on average)
  households$ea_id <- sample(ea_data$ea_id, n_households, replace = TRUE)
  
  # Merge EA characteristics
  households <- households %>%
    left_join(ea_data, by = "ea_id")
  
  # Generate household characteristics
  households <- households %>%
    mutate(
      # Demographics
      household_size = sample(1:15, n(), replace = TRUE, 
                            prob = c(0.15, 0.20, 0.25, 0.20, 0.10, 
                                   0.05, 0.02, 0.01, 0.01, 0.005,
                                   0.003, 0.002, 0.001, 0.0005, 0.0005)),
      
      respondent_age = sample(18:85, n(), replace = TRUE),
      respondent_gender = sample(c("Male", "Female"), n(), 
                                replace = TRUE, prob = c(0.48, 0.52)),
      
      # Dwelling characteristics
      dwelling_type = case_when(
        urban_rural == "Urban" ~ sample(c("House", "Flat", "Shack", "Other"), 
                                       n(), replace = TRUE,
                                       prob = c(0.50, 0.30, 0.15, 0.05)),
        TRUE ~ sample(c("House", "Traditional", "Shack", "Other"),
                     n(), replace = TRUE,
                     prob = c(0.40, 0.35, 0.20, 0.05))
      ),
      
      tenure_status = sample(c("Owned", "Rented", "Free", "Other"),
                            n(), replace = TRUE,
                            prob = c(0.55, 0.30, 0.12, 0.03)),
      
      wall_material = sample(c("Brick", "Mud", "Wood", "Iron", "Other"),
                            n(), replace = TRUE,
                            prob = c(0.45, 0.25, 0.15, 0.10, 0.05)),
      
      roof_material = sample(c("Tile", "Iron", "Thatch", "Concrete", "Other"),
                            n(), replace = TRUE,
                            prob = c(0.30, 0.40, 0.15, 0.10, 0.05)),
      
      # Utilities
      water_source = case_when(
        urban_rural == "Urban" ~ sample(c("Piped", "Borehole", "Well", "Other"),
                                       n(), replace = TRUE,
                                       prob = c(0.70, 0.20, 0.08, 0.02)),
        TRUE ~ sample(c("Borehole", "Well", "River", "Piped", "Other"),
                     n(), replace = TRUE,
                     prob = c(0.40, 0.30, 0.20, 0.08, 0.02))
      ),
      
      electricity = case_when(
        urban_rural == "Urban" ~ sample(c("Grid", "Solar", "Generator", "None"),
                                       n(), replace = TRUE,
                                       prob = c(0.85, 0.08, 0.05, 0.02)),
        TRUE ~ sample(c("None", "Grid", "Solar", "Generator"),
                     n(), replace = TRUE,
                     prob = c(0.45, 0.30, 0.20, 0.05))
      ),
      
      toilet_type = sample(c("Flush", "Pit", "VIP", "None", "Other"),
                          n(), replace = TRUE,
                          prob = c(0.35, 0.40, 0.15, 0.08, 0.02)),
      
      cooking_fuel = sample(c("Electricity", "Gas", "Paraffin", "Wood", 
                             "Charcoal", "Other"),
                           n(), replace = TRUE,
                           prob = c(0.20, 0.15, 0.10, 0.35, 0.15, 0.05)),
      
      # Rooms
      n_rooms = sample(1:10, n(), replace = TRUE,
                      prob = c(0.08, 0.20, 0.25, 0.20, 0.12, 
                              0.08, 0.04, 0.02, 0.008, 0.002)),
      n_bedrooms = pmin(n_rooms, sample(1:6, n(), replace = TRUE)),
      
      # Assets (correlated with income)
      has_radio = rbinom(n(), 1, 0.65),
      has_tv = rbinom(n(), 1, 0.55),
      has_fridge = rbinom(n(), 1, 0.45),
      has_bicycle = rbinom(n(), 1, 0.40),
      has_motorcycle = rbinom(n(), 1, 0.20),
      has_car = rbinom(n(), 1, ifelse(urban_rural == "Urban", 0.35, 0.15)),
      has_computer = rbinom(n(), 1, 0.25),
      has_internet = rbinom(n(), 1, ifelse(urban_rural == "Urban", 0.40, 0.15))
    )
  
  # Generate income (correlated with assets and location)
  households <- households %>%
    mutate(
      asset_index = (has_radio + has_tv + has_fridge + has_bicycle + 
                    has_motorcycle + 2*has_car + has_computer + has_internet) / 10,
      
      base_income = case_when(
        urban_rural == "Urban" ~ rnorm(n(), 6500, 2500),
        TRUE ~ rnorm(n(), 3500, 1800)
      ),
      
      monthly_income = pmax(500, base_income + 
                           asset_index * 3000 + 
                           household_size * (-200) +
                           rnorm(n(), 0, 800)),
      
      monthly_expenditure = pmax(400, monthly_income * runif(n(), 0.6, 1.2) +
                                rnorm(n(), 0, 500)),
      
      receives_grants = rbinom(n(), 1, 
                              ifelse(monthly_income < 3000, 0.35, 0.10)),
      agricultural_hh = rbinom(n(), 1, 
                              ifelse(urban_rural == "Rural", 0.75, 0.15)),
      livestock_owned = rbinom(n(), 1, 
                              ifelse(agricultural_hh == 1, 0.60, 0.10))
    )
  
  # Survey administration variables
  households <- households %>%
    mutate(
      interview_date = sample(seq.Date(as.Date("2024-01-01"), 
                                      as.Date("2024-07-31"), 
                                      by = "day"),
                             n(), replace = TRUE),
      
      interview_mode = sample(c("Face-to-face", "Telephone", "Web"),
                             n(), replace = TRUE,
                             prob = c(0.85, 0.12, 0.03)),
      
      interview_duration_min = pmax(20, rnorm(n(), 55, 15)),
      
      interview_language = sample(c("English", "Portuguese", "French", "Local"),
                                  n(), replace = TRUE,
                                  prob = c(0.45, 0.25, 0.15, 0.15)),
      
      interview_result = sample(c("Complete", "Partial", "Refused", "Not Home"),
                               n(), replace = TRUE,
                               prob = c(0.86, 0.05, 0.06, 0.03))
    )
  
  # Generate sampling weights
  # Stage 1: PSU weights (EA selection probability)
  ea_sizes <- households %>%
    group_by(ea_id) %>%
    summarise(n_hh = n()) %>%
    mutate(psu_weight = 1 / (n_hh / sum(n_hh)))
  
  households <- households %>%
    left_join(ea_sizes, by = "ea_id")
  
  # Stage 2: Household weights within EA
  households <- households %>%
    group_by(ea_id) %>%
    mutate(hh_weight = 1 / (1/n())) %>%
    ungroup()
  
  # Combined design weight
  households <- households %>%
    mutate(design_weight = psu_weight * hh_weight)
  
  # Nonresponse adjustment
  households <- households %>%
    mutate(
      response_prob = case_when(
        urban_rural == "Urban" ~ 0.84,
        TRUE ~ 0.88
      ),
      nr_weight = design_weight / response_prob
    )
  
  # Post-stratification to census totals
  province_totals <- c(712500, 800000, 475000, 537500,
                      412500, 455000, 525000, 585000)
  names(province_totals) <- paste0("P", 1:8)
  
  households <- households %>%
    mutate(
      province_total = province_totals[province],
      province_sample = as.numeric(table(province)[province]),
      ps_factor = province_total / province_sample / sum(nr_weight[province == province[1]]),
      final_weight = nr_weight * ps_factor
    )
  
  # Data quality flags
  households <- households %>%
    mutate(
      quality_check = ifelse(
        monthly_income > monthly_expenditure * 0.5 & 
        monthly_income < monthly_expenditure * 2 &
        respondent_age >= 18 &
        interview_duration_min >= 20,
        1, 0
      ),
      
      data_entry_date = interview_date + sample(1:14, n(), replace = TRUE)
    )
  
  # Introduce some realistic missingness
  households <- households %>%
    mutate(
      monthly_income = ifelse(runif(n()) < 0.125, NA, monthly_income),
      monthly_expenditure = ifelse(runif(n()) < 0.082, NA, monthly_expenditure),
      has_car = ifelse(runif(n()) < 0.037, NA, has_car)
    )
  
  return(households)
}

# ============================================================================
# SECTION 2: COMPLETE ANALYSIS SCRIPT
# Comprehensive analysis pipeline for SADC survey
# ============================================================================

# Load required packages
required_packages <- c(
  "tidyverse", "survey", "srvyr", "convey", "sae", 
  "sampling", "samplingbook", "PracTools",
  "VIM", "mice", "simputation",
  "knitr", "kableExtra", "ggplot2", "plotly",
  "data.table", "dtplyr"
)

# Install if needed
install_if_needed <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new_packages)) install.packages(new_packages)
  invisible(sapply(packages, library, character.only = TRUE))
}

install_if_needed(required_packages)

# ============================================================================
# MODULE 1: Complex Survey Design Analysis
# ============================================================================

analyze_survey_design <- function(data) {
  
  cat("=== MODULE 1: SURVEY DESIGN ANALYSIS ===\n\n")
  
  # 1.1 Basic Structure
  cat("1.1 Survey Structure:\n")
  structure_summary <- data %>%
    summarise(
      Total_Households = n(),
      Provinces = n_distinct(province),
      EAs = n_distinct(ea_id),
      Urban_HH = sum(urban_rural == "Urban"),
      Rural_HH = sum(urban_rural == "Rural"),
      Avg_HH_per_EA = n() / n_distinct(ea_id)
    )
  print(structure_summary)
  
  # 1.2 Calculate ICC
  cat("\n1.2 Intraclass Correlation (ICC):\n")
  calculate_icc <- function(variable, cluster, data) {
    cluster_means <- tapply(data[[variable]], data[[cluster]], 
                           mean, na.rm = TRUE)
    between_var <- var(cluster_means, na.rm = TRUE)
    total_var <- var(data[[variable]], na.rm = TRUE)
    icc <- between_var / total_var
    return(icc)
  }
  
  icc_results <- data.frame(
    Variable = c("monthly_income", "monthly_expenditure", "has_car"),
    ICC = c(
      calculate_icc("monthly_income", "ea_id", data),
      calculate_icc("monthly_expenditure", "ea_id", data),
      calculate_icc("has_car", "ea_id", data)
    )
  ) %>%
    mutate(
      DEFF = 1 + (20 - 1) * ICC,
      Effective_n = 5000 / DEFF
    )
  
  print(icc_results)
  
  # 1.3 Design Effect by Stratum
  cat("\n1.3 Design Effects by Stratum:\n")
  deff_by_stratum <- data %>%
    group_by(stratum) %>%
    summarise(
      n = n(),
      n_ea = n_distinct(ea_id),
      mean_income = mean(monthly_income, na.rm = TRUE),
      sd_income = sd(monthly_income, na.rm = TRUE),
      icc_income = calculate_icc("monthly_income", "ea_id", 
                                 cur_data()),
      deff = 1 + (n()/n_distinct(ea_id) - 1) * icc_income
    )
  
  print(deff_by_stratum)
  
  return(list(
    structure = structure_summary,
    icc = icc_results,
    deff_stratum = deff_by_stratum
  ))
}

# ============================================================================
# MODULE 2: Calibration and Small Area Estimation
# ============================================================================

perform_calibration_sae <- function(data) {
  
  cat("\n=== MODULE 2: CALIBRATION & SAE ===\n\n")
  
  # 2.1 Create Survey Design
  library(survey)
  
  data_complete <- data %>%
    filter(interview_result == "Complete")
  
  design <- svydesign(
    ids = ~ea_id,
    strata = ~stratum,
    weights = ~final_weight,
    data = data_complete,
    nest = TRUE
  )
  
  cat("2.1 Survey Design Created\n")
  print(summary(design))
  
  # 2.2 Calibration
  # Population totals by province (from census)
  pop_totals <- data.frame(
    province = paste0("P", 1:8),
    Freq = c(712500, 800000, 475000, 537500,
             412500, 455000, 525000, 585000)
  )
  
  design_cal <- postStratify(
    design = design,
    strata = ~province,
    population = pop_totals
  )
  
  cat("\n2.2 Post-Stratification Applied\n")
  
  # Compare estimates
  est_before <- svymean(~monthly_income, design, na.rm = TRUE)
  est_after <- svymean(~monthly_income, design_cal, na.rm = TRUE)
  
  comparison <- data.frame(
    Method = c("Before Calibration", "After Calibration"),
    Estimate = c(coef(est_before), coef(est_after)),
    SE = c(SE(est_before), SE(est_after))
  ) %>%
    mutate(CV = SE / Estimate)
  
  print(comparison)
  
  # 2.3 Fay-Herriot SAE
  cat("\n2.3 Fay-Herriot Small Area Estimation\n")
  
  # Direct estimates by province
  direct_est <- svyby(
    ~monthly_income,
    ~province,
    design_cal,
    svymean,
    na.rm = TRUE
  )
  
  # Get variances
  variance_mat <- vcov(direct_est)
  
  # Prepare data for Fay-Herriot
  library(sae)
  
  fh_data <- data.frame(
    province = direct_est$province,
    income_direct = coef(direct_est),
    variance = diag(variance_mat)
  ) %>%
    mutate(
      province_num = as.numeric(substr(province, 2, 2)),
      urban_pct = c(0.65, 0.60, 0.70, 0.58, 0.72, 0.55, 0.68, 0.62),
      pop_density = c(85, 92, 68, 75, 88, 62, 95, 78)
    )
  
  # Fit Fay-Herriot model
  fh_model <- eblupFH(
    formula = income_direct ~ urban_pct + pop_density,
    vardir = fh_data$variance,
    data = fh_data
  )
  
  fh_results <- data.frame(
    Province = fh_data$province,
    Direct = fh_data$income_direct,
    EBLUP = fh_model$eblup$eblup,
    MSE = fh_model$mse
  ) %>%
    mutate(
      EBLUP_SE = sqrt(MSE),
      Direct_SE = sqrt(fh_data$variance),
      MSE_Reduction = (1 - MSE / fh_data$variance) * 100
    )
  
  print(fh_results)
  
  return(list(
    design = design_cal,
    direct_estimates = direct_est,
    fh_results = fh_results
  ))
}

# ============================================================================
# MODULE 3: Optimal Sample Allocation
# ============================================================================

optimize_sample_allocation <- function(data) {
  
  cat("\n=== MODULE 3: OPTIMAL ALLOCATION ===\n\n")
  
  # 3.1 Variance analysis by stratum
  variance_data <- data %>%
    group_by(stratum) %>%
    summarise(
      N_h = n(),
      mean_income = mean(monthly_income, na.rm = TRUE),
      S_h = sd(monthly_income, na.rm = TRUE),
      cost_per_hh = 180  # Assume constant for simplicity
    ) %>%
    ungroup() %>%
    mutate(
      W_h = N_h / sum(N_h),
      
      # Proportional allocation
      n_proportional = round(5000 * W_h),
      
      # Neyman allocation
      neyman_factor = W_h * S_h,
      n_neyman = round(5000 * neyman_factor / sum(neyman_factor)),
      
      # Optimal allocation (minimize cost for fixed variance)
      optimal_factor = W_h * S_h / sqrt(cost_per_hh),
      n_optimal = round(5000 * optimal_factor / sum(optimal_factor)),
      
      # Current allocation
      n_current = N_h
    )
  
  cat("3.1 Allocation Comparison:\n")
  print(variance_data %>% 
          select(stratum, n_current, n_proportional, n_neyman, n_optimal))
  
  # 3.2 Variance under different allocations
  calculate_variance <- function(allocation, variance_data) {
    sum(variance_data$W_h^2 * variance_data$S_h^2 / allocation)
  }
  
  variance_comparison <- data.frame(
    Method = c("Current", "Proportional", "Neyman", "Optimal"),
    Variance = c(
      calculate_variance(variance_data$n_current, variance_data),
      calculate_variance(variance_data$n_proportional, variance_data),
      calculate_variance(variance_data$n_neyman, variance_data),
      calculate_variance(variance_data$n_optimal, variance_data)
    )
  ) %>%
    mutate(
      SE = sqrt(Variance),
      Relative_Efficiency = Variance[Method == "Optimal"] / Variance
    )
  
  cat("\n3.2 Variance Comparison:\n")
  print(variance_comparison)
  
  # 3.3 Sample size calculation
  cat("\n3.3 Required Sample Sizes:\n")
  
  calculate_sample_size <- function(cv_target, deff, prevalence = 0.35) {
    z <- qnorm(0.975)
    se_target <- cv_target * prevalence
    n_srs <- (z^2 * prevalence * (1 - prevalence)) / se_target^2
    n_complex <- n_srs * deff
    return(ceiling(n_complex))
  }
  
  sample_size_results <- data.frame(
    Target_CV = c(0.03, 0.05, 0.10),
    DEFF = 3.85
  ) %>%
    mutate(
      Required_n = map2_dbl(Target_CV, DEFF, calculate_sample_size),
      Cost_Estimate = Required_n * 180
    )
  
  print(sample_size_results)
  
  return(list(
    allocation_comparison = variance_data,
    variance_comparison = variance_comparison,
    sample_sizes = sample_size_results
  ))
}

# ============================================================================
# EXERCISES FOR STUDENTS
# ============================================================================

# EXERCISE 1: Basic Survey Design Analysis
exercise_1 <- function() {
  cat("
  ╔══════════════════════════════════════════════════════════════════╗
  ║ EXERCISE 1: Survey Design Diagnostics                           ║
  ╠══════════════════════════════════════════════════════════════════╣
  ║                                                                  ║
  ║ Given the SADC household survey data:                           ║
  ║                                                                  ║
  ║ 1. Calculate the design effect (DEFF) for poverty rate         ║
  ║ 2. Compute the effective sample size                           ║
  ║ 3. Determine if precision targets are met (CV < 0.10)          ║
  ║ 4. Identify which strata need larger samples                   ║
  ║                                                                  ║
  ║ Expected Output:                                                 ║
  ║ - Table with DEFF by outcome variable                          ║
  ║ - Effective sample sizes                                        ║
  ║ - Precision assessment                                          ║
  ║ - Recommendations for sample reallocation                       ║
  ║                                                                  ║
  ╚══════════════════════════════════════════════════════════════════╝
  ")
  
  # Starter code
  cat("\n# Starter Code:\n")
  cat("
  # Load data
  data <- generate_sadc_survey_data()
  
  # Task 1: Calculate DEFF for poverty rate
  # poverty_rate <- mean(data$monthly_income < 3000, na.rm = TRUE)
  # icc_poverty <- calculate_icc('poverty', 'ea_id', data)
  # deff_poverty <- 1 + (m - 1) * icc_poverty
  
  # Task 2: Calculate effective sample size
  # eff_n <- nrow(data) / deff_poverty
  
  # Task 3: Check precision
  # se <- sqrt(poverty_rate * (1 - poverty_rate) / eff_n)
  # cv <- se / poverty_rate
  
  # Task 4: Stratum analysis
  # ... your code here
  ")
}

# EXERCISE 2: Calibration Implementation
exercise_2 <- function() {
  cat("
  ╔══════════════════════════════════════════════════════════════════╗
  ║ EXERCISE 2: Multi-Level Calibration                             ║
  ╠══════════════════════════════════════════════════════════════════╣
  ║                                                                  ║
  ║ Implement raking calibration to match:                          ║
  ║                                                                  ║
  ║ 1. Provincial population totals                                 ║
  ║ 2. Urban/rural distribution                                     ║
  ║ 3. Age-gender distribution                                      ║
  ║                                                                  ║
  ║ Tasks:                                                           ║
  ║ a) Create survey design object                                  ║
  ║ b) Apply iterative proportional fitting                         ║
  ║ c) Assess weight variability (CV of weights)                   ║
  ║ d) Compare estimates before and after                           ║
  ║                                                                  ║
  ╚══════════════════════════════════════════════════════════════════╝
  ")
  
  cat("\n# Starter Code:\n")
  cat("
  library(survey)
  
  # Create design
  design <- svydesign(...)
  
  # Define population margins
  # pop_province <- data.frame(province = ..., Freq = ...)
  # pop_urban <- data.frame(urban_rural = ..., Freq = ...)
  
  # Apply raking
  # design_cal <- rake(design, ...)
  
  # Assess quality
  # weight_cv <- sd(weights(design_cal)) / mean(weights(design_cal))
  ")
}

# EXERCISE 3: Poverty Mapping
exercise_3 <- function() {
  cat("
  ╔══════════════════════════════════════════════════════════════════╗
  ║ EXERCISE 3: District-Level Poverty Mapping using SAE            ║
  ╠══════════════════════════════════════════════════════════════════╣
  ║                                                                  ║
  ║ Create poverty maps for all districts using Fay-Herriot model   ║
  ║                                                                  ║
  ║ Steps:                                                           ║
  ║ 1. Calculate direct estimates for each province                 ║
  ║ 2. Collect auxiliary variables (census data)                    ║
  ║ 3. Fit Fay-Herriot EBLUP model                                 ║
  ║ 4. Calculate MSE for each area                                  ║
  ║ 5. Create visualization of results                              ║
  ║                                                                  ║
  ╚══════════════════════════════════════════════════════════════════╝
  ")
  
  cat("\n# Starter Code:\n")
  cat("
  library(sae)
  
  # Direct estimates
  # direct_est <- svyby(~I(monthly_income < 3000), ~province, 
  #                     design, svymean)
  
  # Prepare Fay-Herriot data
  # fh_data <- data.frame(
  #   poverty_direct = coef(direct_est),
  #   variance = diag(vcov(direct_est)),
  #   urban_pct = ...,
  #   population = ...
  # )
  
  # Fit model
  # fh_model <- eblupFH(poverty_direct ~ urban_pct + log(population),
  #                     vardir = variance, data = fh_data)
  ")
}

# ============================================================================
# SOLUTIONS TO EXERCISES
# ============================================================================

# SOLUTION 1
solution_1 <- function(data) {
  
  cat("\n╔══════════════════════════════════════════════════════════════════╗\n")
  cat("║ SOLUTION 1: Survey Design Diagnostics                           ║\n")
  cat("╚══════════════════════════════════════════════════════════════════╝\n\n")
  
  # Task 1: DEFF for poverty rate
  data <- data %>%
    mutate(poverty = as.numeric(monthly_income < 3000))
  
  calculate_icc_solution <- function(variable, cluster, data) {
    data_clean <- data[!is.na(data[[variable]]), ]
    cluster_means <- tapply(data_clean[[variable]], 
                           data_clean[[cluster]], 
                           mean)
    between_var <- var(cluster_means, na.rm = TRUE)
    total_var <- var(data_clean[[variable]], na.rm = TRUE)
    icc <- between_var / total_var
    return(icc)
  }
  
  icc_poverty <- calculate_icc_solution("poverty", "ea_id", data)
  m <- mean(table(data$ea_id))  # Average cluster size
  deff_poverty <- 1 + (m - 1) * icc_poverty
  
  cat("Task 1: Design Effect for Poverty Rate\n")
  cat(sprintf("ICC: %.4f\n", icc_poverty))
  cat(sprintf("Average cluster size: %.1f\n", m))
  cat(sprintf("DEFF: %.2f\n", deff_poverty))
  
  # Task 2: Effective sample size
  n_actual <- nrow(data[!is.na(data$poverty), ])
  eff_n <- n_actual / deff_poverty
  
  cat(sprintf("\nTask 2: Effective Sample Size\n"))
  cat(sprintf("Actual n: %d\n", n_actual))
  cat(sprintf("Effective n: %.0f\n", eff_n))
  cat(sprintf("Efficiency loss: %.1f%%\n", (1 - eff_n/n_actual) * 100))
  
  # Task 3: Precision check
  poverty_rate <- mean(data$poverty, na.rm = TRUE)
  se <- sqrt(poverty_rate * (1 - poverty_rate) / eff_n)
  cv <- se / poverty_rate
  
  cat(sprintf("\nTask 3: Precision Assessment\n"))
  cat(sprintf("Poverty rate: %.3f\n", poverty_rate))
  cat(sprintf("Standard error: %.4f\n", se))
  cat(sprintf("CV: %.4f\n", cv))
  cat(sprintf("Target met (CV < 0.10)? %s\n", 
              ifelse(cv < 0.10, "YES ✓", "NO ✗")))
  
  # Task 4: Stratum analysis
  stratum_analysis <- data %>%
    group_by(stratum) %>%
    summarise(
      n = n(),
      poverty_rate = mean(poverty, na.rm = TRUE),
      sd = sd(poverty, na.rm = TRUE),
      icc = calculate_icc_solution("poverty", "ea_id", cur_data()),
      deff = 1 + (mean(table(ea_id)) - 1) * icc,
      eff_n = n / deff,
      se = sqrt(poverty_rate * (1 - poverty_rate) / eff_n),
      cv = se / poverty_rate
    ) %>%
    mutate(
      needs_more = ifelse(cv > 0.10, "YES", "NO")
    )
  
  cat("\nTask 4: Stratum-Level Analysis\n")
  print(stratum_analysis %>% 
          select(stratum, n, poverty_rate, cv, needs_more))
  
  cat("\n✓ Solution complete!\n")
  
  invisible(list(
    deff = deff_poverty,
    eff_n = eff_n,
    cv = cv,
    stratum_analysis = stratum_analysis
  ))
}

# SOLUTION 2
solution_2 <- function(data) {
  
  cat("\n╔══════════════════════════════════════════════════════════════════╗\n")
  cat("║ SOLUTION 2: Multi-Level Calibration                             ║\n")
  cat("╚══════════════════════════════════════════════════════════════════╝\n\n")
  
  library(survey)
  
  # Filter complete cases
  data_complete <- data %>%
    filter(interview_result == "Complete")
  
  # Task a: Create survey design
  design <- svydesign(
    ids = ~ea_id,
    strata = ~stratum,
    weights = ~final_weight,
    data = data_complete,
    nest = TRUE
  )
  
  cat("Task a: Survey Design Created\n")
  cat(sprintf("Sample size: %d\n", nrow(data_complete)))
  cat(sprintf("Number of PSUs: %d\n", length(unique(data_complete$ea_id))))
  cat(sprintf("Number of strata: %d\n", length(unique(data_complete$stratum))))
  
  # Task b: Define population margins
  # Provincial totals (from census)
  pop_province <- data.frame(
    province = paste0("P", 1:8),
    Freq = c(712500, 800000, 475000, 537500,
             412500, 455000, 525000, 585000)
  )
  
  # Urban/rural totals
  pop_urban <- data.frame(
    urban_rural = c("Urban", "Rural"),
    Freq = c(2500000, 2600000)  # Hypothetical census totals
  )
  
  # Apply raking
  design_raked <- rake(
    design = design,
    sample.margins = list(~province, ~urban_rural),
    population.margins = list(pop_province, pop_urban),
    control = list(maxit = 50, epsilon = 1e-7)
  )
  
  cat("\nTask b: Raking Applied\n")
  
  # Task c: Assess weight variability
  weights_original <- weights(design)
  weights_raked <- weights(design_raked)
  
  weight_stats <- data.frame(
    Method = c("Original", "Raked"),
    Mean = c(mean(weights_original), mean(weights_raked)),
    SD = c(sd(weights_original), sd(weights_raked)),
    CV = c(sd(weights_original)/mean(weights_original),
           sd(weights_raked)/mean(weights_raked)),
    Min = c(min(weights_original), min(weights_raked)),
    Max = c(max(weights_original), max(weights_raked))
  )
  
  cat("\nTask c: Weight Variability Assessment\n")
  print(weight_stats)
  
  # Task d: Compare estimates
  est_before <- svymean(~monthly_income, design, na.rm = TRUE)
  est_after <- svymean(~monthly_income, design_raked, na.rm = TRUE)
  
  comparison <- data.frame(
    Method = c("Before Calibration", "After Calibration"),
    Estimate = c(coef(est_before), coef(est_after)),
    SE = c(SE(est_before), SE(est_after))
  ) %>%
    mutate(
      CV = SE / Estimate,
      Change = Estimate - Estimate[1],
      SE_Reduction = (SE[1] - SE) / SE[1] * 100
    )
  
  cat("\nTask d: Estimate Comparison\n")
  print(comparison)
  
  cat("\n✓ Solution complete!\n")
  
  invisible(list(
    design_raked = design_raked,
    weight_stats = weight_stats,
    comparison = comparison
  ))
}

# ============================================================================
# MAIN EXECUTION FUNCTION
# ============================================================================

run_lecture3_analysis <- function(save_outputs = TRUE) {
  
  cat("
  ╔══════════════════════════════════════════════════════════════════╗
  ║          LECTURE 3: COMPLETE ANALYSIS PIPELINE                   ║
  ║     Complex Household Survey Sampling - SADC Example             ║
  ╚══════════════════════════════════════════════════════════════════╝
  \n")
  
  # Generate data
  cat("Generating SADC survey data...\n")
  data <- generate_sadc_survey_data(seed = 2024)
  
  if(save_outputs) {
    write.csv(data, "sadc_household_survey_2024.csv", row.names = FALSE)
    cat("✓ Data saved to sadc_household_survey_2024.csv\n\n")
  }
  
  # Run all modules
  results <- list()
  
  results$module1 <- analyze_survey_design(data)
  results$module2 <- perform_calibration_sae(data)
  results$module3 <- optimize_sample_allocation(data)
  
  # Run exercises
  cat("\n\n=== EXERCISES ===\n")
  exercise_1()
  cat("\n")
  exercise_2()
  cat("\n")
  exercise_3()
  
  # Run solutions
  cat("\n\n=== SOLUTIONS ===\n")
  results$solution1 <- solution_1(data)
  results$solution2 <- solution_2(data)
  
  # Save results
  if(save_outputs) {
    saveRDS(results, "lecture3_analysis_results.rds")
    cat("\n✓ Results saved to lecture3_analysis_results.rds\n")
  }
  
  cat("\n
  ╔══════════════════════════════════════════════════════════════════╗
  ║                    ANALYSIS COMPLETE                             ║
  ╚══════════════════════════════════════════════════════════════════╝
  ")
  
  invisible(results)
}

# ============================================================================
# ADDITIONAL UTILITY FUNCTIONS
# ============================================================================

# Visualization functions
create_visualizations <- function(data, results) {
  
  library(ggplot2)
  
  # 1. Income distribution by urban/rural
  p1 <- ggplot(data, aes(x = monthly_income, fill = urban_rural)) +
    geom_density(alpha = 0.6) +
    scale_x_continuous(labels = scales::dollar) +
    labs(title = "Income Distribution by Location",
         x = "Monthly Income", y = "Density",
         fill = "Location") +
    theme_minimal()
  
  # 2. Design effect by stratum
  p2 <- ggplot(results$module1$deff_stratum, 
               aes(x = reorder(stratum, deff), y = deff)) +
    geom_col(fill = "steelblue") +
    geom_hline(yintercept = 2, linetype = "dashed", color = "red") +
    coord_flip() +
    labs(title = "Design Effects by Stratum",
         x = "Stratum", y = "DEFF") +
    theme_minimal()
  
  # 3. Fay-Herriot comparison
  if(!is.null(results$module2$fh_results)) {
    p3 <- ggplot(results$module2$fh_results, 
                 aes(x = Direct, y = EBLUP)) +
      geom_point(size = 3) +
      geom_abline(slope = 1, intercept = 0, 
                 linetype = "dashed", color = "red") +
      geom_errorbar(aes(ymin = EBLUP - 1.96*EBLUP_SE,
                       ymax = EBLUP + 1.96*EBLUP_SE),
                   width = 100) +
      labs(title = "Direct vs Fay-Herriot Estimates",
           x = "Direct Estimate", y = "EBLUP Estimate") +
      theme_minimal()
  }
  
  return(list(p1 = p1, p2 = p2, p3 = p3))
}

# Export function for reporting
export_results <- function(results, format = "excel") {
  
  if(format == "excel") {
    library(openxlsx)
    
    wb <- createWorkbook()
    
    # Add worksheets
    addWorksheet(wb, "Survey Structure")
    addWorksheet(wb, "ICC and DEFF")
    addWorksheet(wb, "Calibration")
    addWorksheet(wb, "SAE Results")
    
    # Write data
    writeData(wb, "Survey Structure", results$module1$structure)
    writeData(wb, "ICC and DEFF", results$module1$icc)
    writeData(wb, "Calibration", results$module2$direct_estimates)
    writeData(wb, "SAE Results", results$module2$fh_results)
    
    saveWorkbook(wb, "lecture3_results.xlsx", overwrite = TRUE)
    cat("✓ Results exported to lecture3_results.xlsx\n")
  }
}

# ============================================================================
# EXECUTION INSTRUCTIONS
# ============================================================================

cat("
╔══════════════════════════════════════════════════════════════════════╗
║                    LECTURE 3 SUPPORTING MATERIALS                    ║
║                           READY TO USE                               ║
╠══════════════════════════════════════════════════════════════════════╣
║                                                                      ║
║  To run the complete analysis:                                       ║
║  > results <- run_lecture3_analysis()                               ║
║                                                                      ║
║  To generate data only:                                              ║
║  > data <- generate_sadc_survey_data()                              ║
║                                                                      ║
║  To view exercises:                                                  ║
║  > exercise_1()                                                      ║
║  > exercise_2()                                                      ║
║  > exercise_3()                                                      ║
║                                                                      ║
║  To view solutions:                                                  ║
║  > solution_1(data)                                                  ║
║  > solution_2(data)                                                  ║
║                                                                      ║
║  To create visualizations:                                           ║
║  > plots <- create_visualizations(data, results)                    ║
║                                                                      ║
╚══════════════════════════════════════════════════════════════════════╝
")

# Save this script
# saveRDS(list(
#   generate_data = generate_sadc_survey_data,
#   analyze = run_lecture3_analysis,
#   exercises = list(exercise_1, exercise_2, exercise_3),
#   solutions = list(solution_1, solution_2)
# ), "lecture3_complete_materials.rds")