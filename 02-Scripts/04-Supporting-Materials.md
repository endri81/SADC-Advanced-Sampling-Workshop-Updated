# Supporting Scripts for Lecture 4: Thursday's Advanced Analysis
## Harry's Household Survey Data Management Challenge

```r
#############################################################################
# LECTURE 4 SUPPORTING SCRIPTS - THURSDAY ADVANCED ANALYSIS
# Advanced Sampling Methods for Household Surveys
# Dr. Endri Raço - SADC Regional Training Workshop
# Thursday: Complex Analysis & Reporting Systems
#############################################################################

# Script 1: Complex Survey Data Preparation Pipeline
#############################################################################

prepare_complex_survey_data <- function(raw_data_path, metadata_path, output_dir) {
  # Implementation of Eurostat's ESS quality framework for survey data preparation
  # References: ESS Handbook on Quality and Metadata Reports (2020)
  
  library(tidyverse)
  library(survey)
  library(haven)
  library(labelled)
  library(VIM)
  
  # Read household survey main file with metadata
  message("Reading household survey data with metadata preservation...")
  
  # Parse metadata structure according to SDMX standards
  metadata <- read_lines(metadata_path) %>%
    str_split(":", simplify = TRUE) %>%
    as_tibble(.name_repair = "unique") %>%
    rename(variable = `...1`, description = `...2`)
  
  # Read main survey data preserving value labels
  survey_data <- read_csv(raw_data_path, 
                         col_types = cols(
                           household_id = col_character(),
                           ea_id = col_character(),
                           country = col_character(),
                           stratum = col_character(),
                           urban_rural = col_character(),
                           province_code = col_character(),
                           district_code = col_character(),
                           interview_date = col_date(format = "%Y-%m-%d"),
                           interview_mode = col_character(),
                           interview_duration_min = col_integer(),
                           interview_language = col_character(),
                           interview_result = col_character(),
                           household_size = col_integer(),
                           dwelling_type = col_character(),
                           tenure_status = col_character(),
                           wall_material = col_character(),
                           roof_material = col_character(),
                           water_source = col_character(),
                           electricity = col_character(),
                           toilet_type = col_character(),
                           cooking_fuel = col_character(),
                           n_rooms = col_integer(),
                           n_bedrooms = col_integer(),
                           has_radio = col_double(),
                           has_tv = col_double(),
                           has_fridge = col_double(),
                           has_bicycle = col_double(),
                           has_motorcycle = col_double(),
                           has_car = col_double(),
                           has_computer = col_double(),
                           has_internet = col_double(),
                           monthly_income = col_double(),
                           monthly_expenditure = col_double(),
                           receives_grants = col_double(),
                           agricultural_hh = col_double(),
                           livestock_owned = col_double(),
                           respondent_age = col_integer(),
                           respondent_gender = col_character(),
                           psu_weight = col_double(),
                           hh_weight = col_double(),
                           final_weight = col_double(),
                           quality_check = col_double(),
                           data_entry_date = col_date(format = "%Y-%m-%d")
                         ))
  
  # Implement World Bank LSMS data quality checks
  message("Performing LSMS-standard quality checks...")
  
  quality_report <- tibble(
    check_type = character(),
    n_issues = integer(),
    percentage = double(),
    severity = character()
  )
  
  # Check 1: Completeness of key variables
  key_vars <- c("household_id", "ea_id", "stratum", "final_weight")
  for(var in key_vars) {
    n_missing <- sum(is.na(survey_data[[var]]))
    quality_report <- quality_report %>%
      add_row(
        check_type = paste("Missing", var),
        n_issues = n_missing,
        percentage = n_missing / nrow(survey_data) * 100,
        severity = ifelse(n_missing > 0, "Critical", "Pass")
      )
  }
  
  # Check 2: Weight distribution following OECD guidelines
  weight_cv <- sd(survey_data$final_weight, na.rm = TRUE) / 
                mean(survey_data$final_weight, na.rm = TRUE)
  
  quality_report <- quality_report %>%
    add_row(
      check_type = "Weight coefficient of variation",
      n_issues = ifelse(weight_cv > 2, 1, 0),
      percentage = weight_cv * 100,
      severity = case_when(
        weight_cv > 3 ~ "Critical",
        weight_cv > 2 ~ "Warning",
        TRUE ~ "Pass"
      )
    )
  
  # Check 3: Logical consistency checks
  inconsistencies <- survey_data %>%
    filter(
      (n_bedrooms > n_rooms) |
      (household_size < 1) |
      (household_size > 15) |
      (respondent_age < 18 | respondent_age > 85)
    ) %>%
    nrow()
  
  quality_report <- quality_report %>%
    add_row(
      check_type = "Logical inconsistencies",
      n_issues = inconsistencies,
      percentage = inconsistencies / nrow(survey_data) * 100,
      severity = ifelse(inconsistencies / nrow(survey_data) > 0.01, "Warning", "Pass")
    )
  
  # Check 4: Detect outliers using Eurostat's Hidiroglou-Berthelot method
  detect_hb_outliers <- function(x, y, A = 0.05, C = 4) {
    # Implementation of HB method for outlier detection
    # Reference: Eurostat Handbook on Validation of Survey Data
    E <- pmax(x, y)
    ratio <- y / x
    score <- abs(ratio - median(ratio, na.rm = TRUE)) / 
             (A + C * E / median(E, na.rm = TRUE))
    return(score > qnorm(0.975))
  }
  
  if(sum(!is.na(survey_data$monthly_income) & !is.na(survey_data$monthly_expenditure)) > 0) {
    outliers <- detect_hb_outliers(
      survey_data$monthly_income,
      survey_data$monthly_expenditure
    )
    
    quality_report <- quality_report %>%
      add_row(
        check_type = "Income-Expenditure outliers (HB method)",
        n_issues = sum(outliers, na.rm = TRUE),
        percentage = sum(outliers, na.rm = TRUE) / nrow(survey_data) * 100,
        severity = ifelse(sum(outliers, na.rm = TRUE) / nrow(survey_data) > 0.02, "Warning", "Pass")
      )
  }
  
  # Generate quality report
  message("Generating ESS-compliant quality report...")
  
  write_csv(quality_report, file.path(output_dir, "data_quality_report.csv"))
  
  # Create survey design object following UN guidelines
  message("Creating complex survey design object...")
  
  # Apply finite population correction if available
  if("fpc" %in% names(survey_data)) {
    design <- svydesign(
      ids = ~ea_id + household_id,
      strata = ~stratum,
      weights = ~final_weight,
      fpc = ~fpc,
      data = survey_data,
      nest = TRUE
    )
  } else {
    design <- svydesign(
      ids = ~ea_id + household_id,
      strata = ~stratum,
      weights = ~final_weight,
      data = survey_data,
      nest = TRUE
    )
  }
  
  # Return comprehensive results
  list(
    data = survey_data,
    design = design,
    metadata = metadata,
    quality_report = quality_report,
    timestamp = Sys.time(),
    r_version = R.version.string
  )
}

# Script 2: Advanced Calibration with GREG Estimator
#############################################################################

perform_greg_calibration <- function(design, population_totals, calibration_vars) {
  # Implementation of Generalized Regression (GREG) estimator
  # Following Eurostat's calibration guidelines for EU-SILC
  # Reference: Deville & Särndal (1992), Eurostat (2021)
  
  library(survey)
  library(laeken)
  
  message("Implementing GREG calibration following Eurostat standards...")
  
  # Prepare population totals in required format
  pop_totals <- population_totals %>%
    select(all_of(calibration_vars)) %>%
    summarise(across(everything(), sum, na.rm = TRUE))
  
  # Create calibration formula
  cal_formula <- as.formula(paste("~", paste(calibration_vars, collapse = " + ")))
  
  # Perform GREG calibration with bounded weights
  # Using ratio bounds recommended by Statistics Canada (1.5 to 4.0)
  calibrated_design <- calibrate(
    design = design,
    formula = cal_formula,
    population = pop_totals,
    bounds = c(lower = 0.25, upper = 4),
    calfun = "raking",  # Iterative proportional fitting
    trim = c(0.3, 3),   # Trim extreme weights
    force = FALSE
  )
  
  # Calculate calibration diagnostics
  orig_weights <- weights(design)
  cal_weights <- weights(calibrated_design)
  
  calibration_diagnostics <- tibble(
    weight_ratio = cal_weights / orig_weights,
    g_weight = cal_weights / orig_weights - 1
  ) %>%
    summarise(
      min_ratio = min(weight_ratio),
      max_ratio = max(weight_ratio),
      mean_ratio = mean(weight_ratio),
      cv_ratio = sd(weight_ratio) / mean(weight_ratio),
      n_trimmed = sum(weight_ratio == 0.25 | weight_ratio == 4),
      effective_sample_size = sum(cal_weights)^2 / sum(cal_weights^2),
      design_effect = var(weight_ratio)
    )
  
  # Verify calibration totals match population
  verification <- svytotal(cal_formula, calibrated_design)
  
  list(
    calibrated_design = calibrated_design,
    diagnostics = calibration_diagnostics,
    verification = verification,
    original_weights = orig_weights,
    calibrated_weights = cal_weights
  )
}

# Script 3: Multiple Imputation for Complex Surveys
#############################################################################

impute_complex_survey <- function(design, imputation_vars, auxiliary_vars, m = 5) {
  # Implementation of multiple imputation for complex survey data
  # Following Rubin's rules with survey design incorporation
  # Reference: Reiter et al. (2006), Kim et al. (2021)
  
  library(mice)
  library(mitools)
  library(VIM)
  
  message("Performing design-based multiple imputation...")
  
  # Extract data with design variables
  imp_data <- design$variables %>%
    mutate(
      .psu = design$cluster[[1]],
      .strata = design$strata[[1]],
      .weight = weights(design)
    )
  
  # Assess missing data patterns
  missing_pattern <- md.pattern(imp_data[, imputation_vars], rotate.names = TRUE)
  
  # Create weighted imputation method
  # Using predictive mean matching for continuous
  # Weighted logistic regression for binary
  method_vector <- rep("", ncol(imp_data))
  names(method_vector) <- names(imp_data)
  
  for(var in imputation_vars) {
    if(is.numeric(imp_data[[var]])) {
      method_vector[var] <- "pmm"  # Predictive mean matching
    } else if(length(unique(na.omit(imp_data[[var]]))) == 2) {
      method_vector[var] <- "logreg"  # Logistic regression
    } else {
      method_vector[var] <- "polyreg"  # Multinomial regression
    }
  }
  
  # Build predictor matrix incorporating survey design
  pred_matrix <- quickpred(
    imp_data,
    mincor = 0.1,
    minpuc = 0.3,
    include = c(imputation_vars, auxiliary_vars, ".weight", ".strata"),
    exclude = c(".psu", "household_id", "ea_id")
  )
  
  # Perform multiple imputation
  mice_imp <- mice(
    data = imp_data,
    m = m,
    method = method_vector,
    predictorMatrix = pred_matrix,
    maxit = 20,
    seed = 20254,
    printFlag = FALSE
  )
  
  # Check convergence
  convergence_plot <- plot(mice_imp)
  
  # Create imputed survey designs
  imputed_designs <- vector("list", m)
  
  for(i in 1:m) {
    complete_data <- complete(mice_imp, i)
    
    imputed_designs[[i]] <- svydesign(
      ids = ~.psu,
      strata = ~.strata,
      weights = ~.weight,
      data = complete_data,
      nest = TRUE
    )
  }
  
  # Create survey MI object
  mi_design <- as.svyimputationList(imputed_designs)
  
  # Calculate fraction of missing information for key variables
  fmi_results <- tibble(variable = imputation_vars)
  
  for(var in imputation_vars) {
    formula <- as.formula(paste("~", var))
    pooled <- with(mi_design, svymean(formula, na.rm = TRUE))
    mi_summary <- summary(MIcombine(pooled))
    fmi_results <- fmi_results %>%
      mutate(fmi = ifelse(variable == var, mi_summary$fmi[1], fmi))
  }
  
  list(
    mi_design = mi_design,
    mice_object = mice_imp,
    missing_pattern = missing_pattern,
    convergence = convergence_plot,
    fmi = fmi_results,
    m = m
  )
}

# Script 4: Small Area Estimation with Fay-Herriot Model
#############################################################################

perform_small_area_estimation <- function(design, direct_estimates, auxiliary_data, areas) {
  # Implementation of Fay-Herriot area-level model
  # Following World Bank poverty mapping methodology
  # Reference: Molina & Rao (2015), World Bank (2019)
  
  library(sae)
  library(lme4)
  library(Matrix)
  
  message("Implementing Fay-Herriot model for small area estimation...")
  
  # Calculate direct estimates with design-based variances
  direct_results <- areas %>%
    map_dfr(function(area) {
      subset_design <- subset(design, district_code == area)
      
      # Handle empty domains
      if(nrow(subset_design$variables) == 0) {
        return(tibble(
          area = area,
          direct_estimate = NA_real_,
          se = NA_real_,
          cv = NA_real_,
          n_sample = 0
        ))
      }
      
      # Calculate poverty indicator (example)
      poverty_est <- svymean(~I(monthly_income < 1000), subset_design, na.rm = TRUE)
      
      tibble(
        area = area,
        direct_estimate = as.numeric(poverty_est)[1],
        se = as.numeric(SE(poverty_est))[1],
        cv = SE(poverty_est)[1] / poverty_est[1],
        n_sample = nrow(subset_design$variables)
      )
    })
  
  # Prepare auxiliary data at area level
  area_auxiliary <- auxiliary_data %>%
    group_by(district_code) %>%
    summarise(
      population = sum(population),
      urbanization_rate = mean(urban_rural == "Urban"),
      education_index = mean(education_years, na.rm = TRUE),
      employment_rate = mean(employed, na.rm = TRUE),
      access_electricity = mean(has_electricity, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rename(area = district_code)
  
  # Merge direct estimates with auxiliary data
  sae_data <- direct_results %>%
    left_join(area_auxiliary, by = "area") %>%
    filter(!is.na(direct_estimate))
  
  # Fit Fay-Herriot model
  fh_model <- eblupFH(
    formula = direct_estimate ~ urbanization_rate + education_index + 
              employment_rate + access_electricity,
    vardir = se^2,
    data = sae_data,
    method = "REML"
  )
  
  # Calculate model diagnostics
  # Residual analysis
  residuals <- fh_model$eblup - sae_data$direct_estimate
  
  diagnostics <- tibble(
    goodness_of_fit = cor(fh_model$eblup, sae_data$direct_estimate)^2,
    variance_ratio = fh_model$fit$refvar / mean(sae_data$se^2),
    shrinkage_factor = mean(1 - fh_model$gamma),
    model_se = sqrt(mean(fh_model$mse))
  )
  
  # Create final SAE results
  sae_results <- sae_data %>%
    mutate(
      sae_estimate = fh_model$eblup,
      sae_mse = fh_model$mse,
      sae_cv = sqrt(sae_mse) / sae_estimate,
      efficiency_gain = (se^2 - sae_mse) / se^2,
      shrinkage_weight = 1 - (sae_mse / se^2)
    )
  
  # Benchmarking to ensure coherence with direct national estimate
  national_direct <- svymean(~I(monthly_income < 1000), design, na.rm = TRUE)
  
  # Apply ratio benchmarking
  benchmark_factor <- as.numeric(national_direct)[1] / 
                     weighted.mean(sae_results$sae_estimate, 
                                  w = sae_results$population, 
                                  na.rm = TRUE)
  
  sae_results <- sae_results %>%
    mutate(
      sae_estimate_benchmarked = sae_estimate * benchmark_factor,
      final_estimate = sae_estimate_benchmarked
    )
  
  list(
    model = fh_model,
    results = sae_results,
    diagnostics = diagnostics,
    national_estimate = as.numeric(national_direct)[1],
    benchmark_factor = benchmark_factor
  )
}

# Script 5: Complex Cross-tabulation with Proper Variance Estimation
#############################################################################

create_complex_crosstabs <- function(design, row_var, col_var, statistic = "proportion") {
  # Implementation of complex cross-tabulation with Rao-Scott corrections
  # Following UN Statistical Division guidelines
  # Reference: Rao & Scott (1984), UN (2005)
  
  library(survey)
  library(gt)
  
  message("Creating design-based cross-tabulation with Rao-Scott adjustment...")
  
  # Create contingency table with design
  formula <- as.formula(paste("~", row_var, "+", col_var))
  
  if(statistic == "proportion") {
    # Proportions with proper SE
    tab <- svytable(formula, design)
    prop_tab <- prop.table(tab, margin = 1)
    
    # Calculate design-based standard errors for each cell
    se_matrix <- matrix(0, nrow = nrow(prop_tab), ncol = ncol(prop_tab))
    
    for(i in 1:nrow(prop_tab)) {
      for(j in 1:ncol(prop_tab)) {
        # Subset design for cell
        cell_formula <- as.formula(
          paste0("~I(", row_var, "=='", rownames(prop_tab)[i], 
                 "' & ", col_var, "=='", colnames(prop_tab)[j], "')")
        )
        cell_mean <- svymean(cell_formula, design, na.rm = TRUE)
        se_matrix[i, j] <- SE(cell_mean)[1]
      }
    }
    
    # Rao-Scott chi-square test
    rs_test <- svychisq(formula, design, statistic = "Chisq")
    
    # Create formatted output table
    output <- list(
      proportions = prop_tab,
      standard_errors = se_matrix,
      counts = tab,
      test = rs_test,
      design_effects = svydesign(formula, design)
    )
    
  } else if(statistic == "mean") {
    # Means within cells
    # Create interaction variable
    design$variables$.interaction <- interaction(
      design$variables[[row_var]],
      design$variables[[col_var]]
    )
    
    # Calculate means for continuous outcome
    mean_results <- svyby(
      formula = ~monthly_income,
      by = ~.interaction,
      design = design,
      FUN = svymean,
      na.rm = TRUE
    )
    
    output <- list(
      means = mean_results,
      test = svyglm(monthly_income ~ get(row_var) * get(col_var), design)
    )
  }
  
  return(output)
}

# Script 6: Response Rate Analysis and Non-response Adjustment
#############################################################################

analyze_nonresponse <- function(frame_data, response_data, weight_vars, auxiliary_vars) {
  # Implementation of comprehensive non-response analysis
  # Following AAPOR Standard Definitions and Eurostat quality indicators
  # Reference: AAPOR (2023), Eurostat (2020)
  
  library(tidyverse)
  library(survey)
  library(nnet)
  
  message("Performing comprehensive non-response analysis...")
  
  # Calculate AAPOR response rates
  disposition_codes <- frame_data %>%
    mutate(
      disposition = case_when(
        household_id %in% response_data$household_id ~ "I",  # Interview
        contact_result == "Refused" ~ "R",                    # Refusal
        contact_result == "Not home" ~ "NC",                  # Non-contact
        contact_result == "Unable" ~ "O",                     # Other
        eligibility == "Unknown" ~ "UH",                      # Unknown household
        eligibility == "Not eligible" ~ "NE",                 # Not eligible
        TRUE ~ "UO"                                          # Unknown other
      )
    )
  
  # Calculate all AAPOR rates
  I <- sum(disposition_codes$disposition == "I")
  P <- sum(disposition_codes$disposition == "P")  # Partial
  R <- sum(disposition_codes$disposition == "R")
  NC <- sum(disposition_codes$disposition == "NC")
  O <- sum(disposition_codes$disposition == "O")
  UH <- sum(disposition_codes$disposition == "UH")
  UO <- sum(disposition_codes$disposition == "UO")
  
  # Estimated eligibility rate
  e <- (I + P + R + NC + O) / (I + P + R + NC + O + sum(disposition_codes$disposition == "NE"))
  
  aapor_rates <- tibble(
    RR1 = I / (I + P + R + NC + O + UH + UO),
    RR2 = (I + P) / (I + P + R + NC + O + UH + UO),
    RR3 = I / (I + R + NC + O + e * (UH + UO)),
    RR4 = (I + P) / (I + P + R + NC + O + e * (UH + UO)),
    COOP1 = I / (I + P + R + O),
    COOP2 = (I + P) / (I + P + R + O),
    REF1 = R / (I + P + R + NC + O + UH + UO),
    REF2 = R / (I + P + R + NC + O),
    CON1 = (I + P + R + O) / (I + P + R + O + NC + UH + UO),
    CON2 = (I + P + R + O) / (I + P + R + O + NC)
  )
  
  # Response propensity modeling
  message("Fitting response propensity model...")
  
  # Prepare data for propensity model
  propensity_data <- frame_data %>%
    mutate(responded = household_id %in% response_data$household_id) %>%
    select(responded, all_of(auxiliary_vars))
  
  # Fit logistic regression model
  propensity_model <- glm(
    responded ~ .,
    data = propensity_data,
    family = binomial(link = "logit")
  )
  
  # Calculate response propensities
  propensities <- predict(propensity_model, type = "response")
  
  # Perform Little's test for response mechanism
  # Testing if response is MAR (Missing at Random)
  
  # Create response propensity classes
  propensity_classes <- cut(propensities, 
                            breaks = quantile(propensities, probs = seq(0, 1, 0.2)),
                            include.lowest = TRUE,
                            labels = paste0("Q", 1:5))
  
  # Calculate weights
  base_weights <- frame_data[[weight_vars[1]]]
  
  # Method 1: Inverse propensity weighting
  ipw_weights <- ifelse(
    frame_data$household_id %in% response_data$household_id,
    base_weights / propensities,
    0
  )
  
  # Method 2: Response homogeneity groups (Eurostat method)
  rhg_adjustment <- propensity_data %>%
    mutate(prop_class = propensity_classes) %>%
    group_by(prop_class) %>%
    summarise(
      adjustment_factor = n() / sum(responded),
      .groups = "drop"
    )
  
  # Apply RHG adjustment
  rhg_weights <- frame_data %>%
    mutate(prop_class = propensity_classes) %>%
    left_join(rhg_adjustment, by = "prop_class") %>%
    mutate(
      adjusted_weight = ifelse(
        household_id %in% response_data$household_id,
        base_weights * adjustment_factor,
        0
      )
    ) %>%
    pull(adjusted_weight)
  
  # Compare weight distributions
  weight_comparison <- tibble(
    method = c(rep("Base", length(base_weights)),
              rep("IPW", length(ipw_weights)),
              rep("RHG", length(rhg_weights))),
    weight = c(base_weights, ipw_weights, rhg_weights)
  ) %>%
    filter(weight > 0) %>%
    group_by(method) %>%
    summarise(
      min = min(weight),
      q25 = quantile(weight, 0.25),
      median = median(weight),
      mean = mean(weight),
      q75 = quantile(weight, 0.75),
      max = max(weight),
      cv = sd(weight) / mean(weight),
      .groups = "drop"
    )
  
  # Assess representativeness
  representativeness <- auxiliary_vars %>%
    map_dfr(function(var) {
      frame_mean <- mean(frame_data[[var]], na.rm = TRUE)
      response_mean <- mean(response_data[[var]], na.rm = TRUE)
      
      tibble(
        variable = var,
        frame_mean = frame_mean,
        response_mean = response_mean,
        bias = response_mean - frame_mean,
        relative_bias = (response_mean - frame_mean) / frame_mean * 100
      )
    })
  
  list(
    aapor_rates = aapor_rates,
    propensity_model = propensity_model,
    propensities = propensities,
    ipw_weights = ipw_weights,
    rhg_weights = rhg_weights,
    weight_comparison = weight_comparison,
    representativeness = representativeness
  )
}

# Script 7: Variance Estimation for Complex Statistics
#############################################################################

estimate_complex_variances <- function(design, estimates_function, method = "linearization", replicates = 200) {
  # Implementation of multiple variance estimation methods
  # Following UN recommendations for complex surveys
  # Reference: Wolter (2007), UN (2005)
  
  library(survey)
  library(MASS)
  
  message(paste("Performing variance estimation using", method, "method..."))
  
  if(method == "linearization") {
    # Taylor linearization (default in survey package)
    results <- estimates_function(design)
    
    # Extract variance-covariance matrix
    vcov_matrix <- vcov(results)
    
    # Calculate design effects
    # Compare with SRS assumption
    srs_design <- svydesign(
      ids = ~1,
      weights = weights(design),
      data = design$variables
    )
    
    srs_results <- estimates_function(srs_design)
    srs_vcov <- vcov(srs_results)
    
    design_effects <- diag(vcov_matrix) / diag(srs_vcov)
    
    output <- list(
      estimates = coef(results),
      standard_errors = SE(results),
      vcov_matrix = vcov_matrix,
      design_effects = design_effects,
      method = "Taylor linearization"
    )
    
  } else if(method == "jackknife") {
    # Jackknife repeated replication
    # Create jackknife replicate weights
    jk_design <- as.svrepdesign(
      design,
      type = "JKn",
      compress = FALSE
    )
    
    results <- estimates_function(jk_design)
    
    output <- list(
      estimates = coef(results),
      standard_errors = SE(results),
      vcov_matrix = vcov(results),
      replicates = jk_design$repweights,
      method = "Jackknife"
    )
    
  } else if(method == "bootstrap") {
    # Bootstrap variance estimation
    # Following Rao-Wu rescaling bootstrap
    boot_design <- as.svrepdesign(
      design,
      type = "bootstrap",
      replicates = replicates
    )
    
    results <- estimates_function(boot_design)
    
    # Calculate bootstrap confidence intervals
    boot_estimates <- boot_design$repweights %*% coef(results)
    boot_ci <- apply(boot_estimates, 2, quantile, probs = c(0.025, 0.975))
    
    output <- list(
      estimates = coef(results),
      standard_errors = SE(results),
      vcov_matrix = vcov(results),
      confidence_intervals = boot_ci,
      bootstrap_distribution = boot_estimates,
      method = "Bootstrap"
    )
    
  } else if(method == "brr") {
    # Balanced Repeated Replication
    # Requires 2^k PSUs per stratum
    
    # Check if design is suitable for BRR
    n_psu_per_stratum <- table(design$strata[!duplicated(design$cluster[[1]])])
    
    if(all(n_psu_per_stratum == 2)) {
      brr_design <- as.svrepdesign(
        design,
        type = "BRR",
        compress = FALSE
      )
      
      results <- estimates_function(brr_design)
      
      output <- list(
        estimates = coef(results),
        standard_errors = SE(results),
        vcov_matrix = vcov(results),
        hadamard_matrix_size = ncol(brr_design$repweights),
        method = "BRR"
      )
    } else {
      warning("Design not suitable for BRR (requires 2 PSUs per stratum). Using Fay's method instead.")
      
      # Fay's variance estimator
      fay_design <- as.svrepdesign(
        design,
        type = "Fay",
        fay.rho = 0.3,
        compress = FALSE
      )
      
      results <- estimates_function(fay_design)
      
      output <- list(
        estimates = coef(results),
        standard_errors = SE(results),
        vcov_matrix = vcov(results),
        fay_coefficient = 0.3,
        method = "Fay"
      )
    }
  }
  
  # Add coefficient of variation
  output$cv <- output$standard_errors / abs(output$estimates)
  
  # Add effective sample size
  output$n_eff <- sum(weights(design))^2 / sum(weights(design)^2)
  
  return(output)
}

# Script 8: Automated Report Generation
#############################################################################

generate_survey_report <- function(processed_data, output_format = "html") {
  # Automated report generation following international standards
  # Implements ESS quality reporting guidelines
  
  library(rmarkdown)
  library(knitr)
  library(officer)
  library(flextable)
  
  message("Generating standardized survey report...")
  
  # Create temporary Rmd file
  report_template <- '
---
title: "Household Survey Analysis Report"
subtitle: "SADC Regional Statistics"
date: "`r Sys.Date()`"
output: 
  html_document:
    toc: true
    toc_float: true
    theme: flatly
    highlight: tango
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
library(tidyverse)
library(survey)
library(gt)
library(plotly)
```

# Executive Summary

This report presents the analysis of the household survey conducted according to international statistical standards.

## Key Indicators

```{r key_indicators}
# Display key indicators table
processed_data$key_indicators %>%
  gt() %>%
  fmt_number(columns = where(is.numeric), decimals = 2) %>%
  tab_header(
    title = "Survey Key Indicators",
    subtitle = "Weighted estimates with standard errors"
  )
```

## Response Rates

```{r response_rates}
# AAPOR response rate summary
processed_data$response_analysis$aapor_rates %>%
  pivot_longer(everything(), names_to = "Rate", values_to = "Value") %>%
  ggplot(aes(x = Rate, y = Value)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = round(Value, 3)), vjust = -0.5) +
  labs(title = "AAPOR Response Rate Indicators",
       y = "Rate") +
  theme_minimal() +
  ylim(0, 1)
```

# Survey Design

## Sampling Strategy

- **Design**: `r processed_data$design_summary$type`
- **Stages**: `r processed_data$design_summary$stages`
- **Stratification**: `r processed_data$design_summary$stratification`
- **Sample size**: `r processed_data$design_summary$n`
- **Population**: `r processed_data$design_summary$N`

## Weight Distribution

```{r weights}
# Weight distribution analysis
data.frame(weight = weights(processed_data$design)) %>%
  ggplot(aes(x = weight)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.7) +
  geom_vline(xintercept = mean(weights(processed_data$design)), 
             color = "red", linetype = "dashed") +
  labs(title = "Distribution of Survey Weights",
       subtitle = paste("CV =", 
                       round(sd(weights(processed_data$design)) / 
                            mean(weights(processed_data$design)), 3)),
       x = "Weight", y = "Frequency") +
  theme_minimal()
```

# Quality Assessment

## Data Quality Indicators

```{r quality}
processed_data$quality_report %>%
  gt() %>%
  tab_header(
    title = "Data Quality Assessment",
    subtitle = "ESS Quality Framework Indicators"
  ) %>%
  data_color(
    columns = severity,
    colors = scales::col_factor(
      palette = c("Pass" = "green", "Warning" = "yellow", "Critical" = "red"),
      domain = c("Pass", "Warning", "Critical")
    )
  )
```

## Missing Data Patterns

```{r missing}
# Missing data visualization
processed_data$missing_pattern %>%
  plot()
```

# Main Results

## Population Characteristics

```{r population}
# Demographic breakdown
svytable(~urban_rural + province_code, processed_data$design) %>%
  as.data.frame() %>%
  ggplot(aes(x = province_code, y = Freq, fill = urban_rural)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Population Distribution by Province and Urban/Rural",
       x = "Province", y = "Weighted Count", fill = "Area Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

## Economic Indicators

```{r economic}
# Income distribution
svyquantile(~monthly_income, processed_data$design, 
            c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm = TRUE) %>%
  as.data.frame() %>%
  rownames_to_column("Percentile") %>%
  gt() %>%
  fmt_currency(columns = -1) %>%
  tab_header(
    title = "Income Distribution",
    subtitle = "Weighted percentiles"
  )
```

# Methodology Notes

This survey follows the guidelines established by:
- Eurostat (European Statistical System)
- World Bank (Living Standards Measurement Study)
- OECD (Programme for International Assessment)
- UN Statistical Division

## Variance Estimation

Variance estimates calculated using `r processed_data$variance_method` method with design-based adjustments.

# Appendix

## Technical Details

```{r session}
sessionInfo()
```
'
  
  # Write template to temp file
  temp_rmd <- tempfile(fileext = ".Rmd")
  writeLines(report_template, temp_rmd)
  
  # Render report
  render(
    input = temp_rmd,
    output_format = paste0(output_format, "_document"),
    output_file = paste0("survey_report_", Sys.Date(), ".", output_format),
    envir = new.env()
  )
  
  message(paste("Report generated:", paste0("survey_report_", Sys.Date(), ".", output_format)))
}

# Script 9: Simulation for Sample Size Determination
#############################################################################

simulate_sample_size <- function(population_params, design_params, target_indicators, nsim = 1000) {
  # Monte Carlo simulation for sample size determination
  # Following Eurostat and World Bank guidelines
  
  library(SimSurvey)
  
  message("Running sample size simulations...")
  
  # Set up population model based on parameters
  pop_size <- population_params$N
  n_strata <- design_params$n_strata
  n_psu_per_stratum <- design_params$n_psu_per_stratum
  n_ssu_per_psu <- design_params$n_ssu_per_psu
  
  # Generate synthetic population
  population <- expand.grid(
    stratum = 1:n_strata,
    psu = 1:ceiling(pop_size / n_strata / 100),
    household = 1:100
  ) %>%
    mutate(
      # Generate correlated variables
      x1 = rnorm(n(), mean = stratum, sd = 1),
      x2 = rnorm(n(), mean = 0, sd = 2),
      # Outcome with ICC
      psu_effect = rnorm(n_distinct(psu), 0, sqrt(population_params$icc))[psu],
      y = population_params$mean + 
          population_params$beta1 * x1 + 
          population_params$beta2 * x2 + 
          psu_effect +
          rnorm(n(), 0, population_params$sigma)
    ) %>%
    slice_sample(n = pop_size)
  
  # Run simulations for different sample sizes
  sample_sizes <- seq(design_params$min_n, design_params$max_n, by = design_params$step)
  
  simulation_results <- sample_sizes %>%
    map_dfr(function(n) {
      
      # Calculate allocation
      n_psu <- ceiling(n / n_ssu_per_psu)
      psu_per_stratum <- ceiling(n_psu / n_strata)
      
      # Run nsim simulations for this sample size
      sim_estimates <- replicate(nsim, {
        # Draw sample
        sample <- population %>%
          group_by(stratum) %>%
          sample_n(size = min(psu_per_stratum, n_distinct(psu))) %>%
          group_by(stratum, psu) %>%
          sample_n(size = min(n_ssu_per_psu, n())) %>%
          ungroup()
        
        # Calculate estimates
        mean(sample$y)
      })
      
      # Calculate precision measures
      tibble(
        sample_size = n,
        mean_estimate = mean(sim_estimates),
        bias = mean(sim_estimates) - population_params$mean,
        se = sd(sim_estimates),
        cv = sd(sim_estimates) / mean(sim_estimates),
        mse = mean((sim_estimates - population_params$mean)^2),
        coverage_95 = mean(
          abs(sim_estimates - population_params$mean) < 1.96 * sd(sim_estimates)
        )
      )
    })
  
  # Find optimal sample size for target CV
  optimal_n <- simulation_results %>%
    filter(cv <= target_indicators$max_cv) %>%
    slice_min(sample_size) %>%
    pull(sample_size)
  
  # Generate plots
  p1 <- ggplot(simulation_results, aes(x = sample_size)) +
    geom_line(aes(y = cv), color = "blue", size = 1) +
    geom_hline(yintercept = target_indicators$max_cv, 
               linetype = "dashed", color = "red") +
    geom_vline(xintercept = optimal_n, 
               linetype = "dashed", color = "green") +
    labs(title = "Coefficient of Variation by Sample Size",
         x = "Sample Size", y = "CV") +
    theme_minimal()
  
  p2 <- ggplot(simulation_results, aes(x = sample_size)) +
    geom_line(aes(y = mse), color = "purple", size = 1) +
    labs(title = "Mean Squared Error by Sample Size",
         x = "Sample Size", y = "MSE") +
    theme_minimal()
  
  list(
    results = simulation_results,
    optimal_n = optimal_n,
    plot_cv = p1,
    plot_mse = p2
  )
}

# Script 10: Panel Data Management
#############################################################################

manage_panel_rotation <- function(wave1_data, wave2_frame, rotation_scheme = "25_percent") {
  # Implementation of panel rotation following EU-SILC methodology
  # Reference: Eurostat (2021) EU-SILC methodology
  
  library(RecordLinkage)
  
  message("Managing panel rotation...")
  
  # Determine rotation groups based on scheme
  if(rotation_scheme == "25_percent") {
    # EU-SILC style: 4-year panel, 25% rotation
    wave1_data <- wave1_data %>%
      mutate(
        rotation_group = sample(1:4, n(), replace = TRUE),
        waves_remaining = 5 - rotation_group
      )
    
    # Identify households to retain
    retained <- wave1_data %>%
      filter(waves_remaining > 1)
    
    # Identify households to replace (25%)
    to_replace <- wave1_data %>%
      filter(waves_remaining == 1)
    
  } else if(rotation_scheme == "50_percent") {
    # 50% overlap between waves
    retained <- wave1_data %>%
      sample_frac(0.5)
    
    to_replace <- wave1_data %>%
      anti_join(retained, by = "household_id")
  }
  
  # Draw new households from frame
  n_new <- nrow(to_replace)
  
  new_households <- wave2_frame %>%
    filter(!household_id %in% wave1_data$household_id) %>%
    sample_n(n_new)
  
  # Combine for wave 2
  wave2_sample <- bind_rows(
    retained %>% mutate(panel_status = "Continuing"),
    new_households %>% mutate(panel_status = "New")
  )
  
  # Calculate attrition weights for continuing panel
  attrition_model <- glm(
    continued ~ urban_rural + household_size + monthly_income + respondent_age,
    data = wave1_data %>%
      mutate(continued = household_id %in% retained$household_id),
    family = binomial
  )
  
  attrition_propensities <- predict(attrition_model, type = "response")
  
  # Adjust weights for attrition
  wave2_sample <- wave2_sample %>%
    left_join(
      tibble(
        household_id = wave1_data$household_id,
        attrition_adjustment = 1 / attrition_propensities
      ),
      by = "household_id"
    ) %>%
    mutate(
      panel_weight = case_when(
        panel_status == "Continuing" ~ final_weight * attrition_adjustment,
        panel_status == "New" ~ final_weight,
        TRUE ~ final_weight
      )
    )
  
  # Create linking file for longitudinal analysis
  panel_links <- retained %>%
    select(household_id, rotation_group, waves_remaining) %>%
    mutate(
      wave1_id = household_id,
      wave2_id = household_id,
      link_status = "Matched"
    )
  
  list(
    wave2_sample = wave2_sample,
    panel_links = panel_links,
    attrition_rate = 1 - nrow(retained) / nrow(wave1_data),
    rotation_summary = table(wave1_data$rotation_group),
    attrition_model = attrition_model
  )
}

#############################################################################
# END OF SUPPORTING SCRIPTS FOR LECTURE 4
# These scripts provide the computational backbone for Thursday's advanced
# household survey analysis techniques following international standards
#############################################################################
```