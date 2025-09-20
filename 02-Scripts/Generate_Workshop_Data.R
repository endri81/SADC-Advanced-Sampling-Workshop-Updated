# ═══════════════════════════════════════════════════════════════════════════
# SADC Advanced Sampling Methods Workshop
# Synthetic Data Generation Script
# Purpose: Create realistic household survey datasets for workshop exercises
# ═══════════════════════════════════════════════════════════════════════════

# Load required packages
library(tidyverse)
library(data.table)
library(sampling)
library(truncnorm)

# Set seed for reproducibility
set.seed(2024)

# Define SADC countries for stratification
sadc_countries <- c("Angola", "Botswana", "Comoros", "DRC", "Eswatini", 
                    "Lesotho", "Madagascar", "Malawi", "Mauritius", "Mozambique",
                    "Namibia", "Seychelles", "South Africa", "Tanzania", 
                    "Zambia", "Zimbabwe")

# ═══════════════════════════════════════════════════════════════════════════
# 1. ENUMERATION AREAS MASTER FILE
# ═══════════════════════════════════════════════════════════════════════════

generate_ea_master <- function() {
  n_eas <- 5000  # Total enumeration areas
  
  ea_master <- data.frame(
    ea_id = sprintf("EA%05d", 1:n_eas),
    country = sample(sadc_countries[1:6], n_eas, replace = TRUE, 
                     prob = c(0.25, 0.20, 0.15, 0.20, 0.10, 0.10)),
    province_code = paste0("P", sample(1:8, n_eas, replace = TRUE)),
    district_code = paste0("D", sample(1:25, n_eas, replace = TRUE)),
    stratum = NA,
    urban_rural = sample(c("Urban", "Rural"), n_eas, replace = TRUE, 
                         prob = c(0.35, 0.65)),
    ea_type = NA,
    total_households = NA,
    total_population = NA,
    households_listed = NA,
    longitude = runif(n_eas, 15, 35),  # Approximate SADC region
    latitude = runif(n_eas, -35, -5),
    elevation = round(rnorm(n_eas, 800, 400)),
    road_distance_km = rexp(n_eas, rate = 1/25),
    health_facility_km = rexp(n_eas, rate = 1/10),
    school_km = rexp(n_eas, rate = 1/5),
    market_km = rexp(n_eas, rate = 1/15),
    mobile_coverage = sample(c("None", "2G", "3G", "4G"), n_eas,
                             replace = TRUE, prob = c(0.15, 0.25, 0.40, 0.20)),
    accessibility = sample(c("Easy", "Moderate", "Difficult", "Very Difficult"),
                           n_eas, replace = TRUE, prob = c(0.40, 0.35, 0.20, 0.05)),
    last_census_year = 2022,
    listing_date = as.Date("2023-01-01") + sample(0:365, n_eas, replace = TRUE),
    selection_probability = NA,
    psu_weight = NA,
    quality_flag = sample(c(0, 1), n_eas, replace = TRUE, prob = c(0.95, 0.05))
  )
  
  # Create stratification variable
  ea_master$stratum <- paste(ea_master$country, ea_master$urban_rural, sep = "_")
  
  # Assign EA types based on urban/rural
  ea_master$ea_type <- ifelse(ea_master$urban_rural == "Urban",
                              sample(c("Formal", "Informal", "Mixed"), n_eas, replace = TRUE,
                                     prob = c(0.60, 0.25, 0.15)),
                              sample(c("Village", "Settlement", "Farm"), n_eas, replace = TRUE,
                                     prob = c(0.50, 0.35, 0.15)))
  
  # Generate household and population counts with urban/rural differences
  ea_master$total_households <- ifelse(ea_master$urban_rural == "Urban",
                                       round(rtruncnorm(n_eas, a = 50, b = 500, mean = 150, sd = 50)),
                                       round(rtruncnorm(n_eas, a = 20, b = 200, mean = 80, sd = 30)))
  
  ea_master$total_population <- round(ea_master$total_households * 
                                        rtruncnorm(n_eas, a = 2.5, b = 6, mean = 4.2, sd = 0.8))
  
  # Listed households (with some undercoverage/overcoverage)
  coverage_factor <- rnorm(n_eas, mean = 0.98, sd = 0.05)
  ea_master$households_listed <- round(ea_master$total_households * coverage_factor)
  ea_master$households_listed[ea_master$households_listed < 20] <- 20
  
  # Calculate selection probabilities (PPS to households)
  ea_master$selection_probability <- ea_master$total_households / sum(ea_master$total_households)
  
  # Calculate base weights
  ea_master$psu_weight <- 1 / ea_master$selection_probability
  
  # Add some missing values for realism
  missing_coords <- sample(1:n_eas, size = n_eas * 0.02)
  ea_master$longitude[missing_coords] <- NA
  ea_master$latitude[missing_coords] <- NA
  
  return(ea_master)
}

# ═══════════════════════════════════════════════════════════════════════════
# 2. HOUSEHOLD SURVEY MAIN FILE
# ═══════════════════════════════════════════════════════════════════════════

generate_household_main <- function(ea_master) {
  # Select sample of EAs (two-stage design)
  n_sample_eas <- 250
  sampled_eas <- ea_master %>%
    group_by(stratum) %>%
    sample_n(size = ceiling(n_sample_eas / n_distinct(ea_master$stratum)), 
             weight = total_households) %>%
    ungroup() %>%
    slice_sample(n = n_sample_eas)
  
  # Generate households within selected EAs
  household_list <- list()
  
  for (i in 1:nrow(sampled_eas)) {
    ea <- sampled_eas[i, ]
    n_hh <- min(20, round(ea$households_listed * 0.15))  # Sample 15% or max 20
    
    hh_data <- data.frame(
      household_id = sprintf("HH%05d_%03d", i, 1:n_hh),
      ea_id = ea$ea_id,
      country = ea$country,
      stratum = ea$stratum,
      urban_rural = ea$urban_rural,
      province_code = ea$province_code,
      district_code = ea$district_code,
      interview_date = as.Date("2024-01-15") + sample(-30:180, n_hh, replace = TRUE),
      household_size = round(rtruncnorm(n_hh, a = 1, b = 15, mean = 4.5, sd = 2)),
      dwelling_type = sample(c("House", "Flat", "Shack", "Traditional", "Other"),
                             n_hh, replace = TRUE, prob = c(0.45, 0.15, 0.20, 0.15, 0.05)),
      tenure_status = sample(c("Owned", "Rented", "Free", "Other"),
                             n_hh, replace = TRUE, prob = c(0.55, 0.25, 0.15, 0.05)),
      wall_material = sample(c("Brick", "Mud", "Wood", "Iron", "Other"),
                             n_hh, replace = TRUE, prob = c(0.40, 0.25, 0.15, 0.15, 0.05)),
      roof_material = sample(c("Tile", "Iron", "Thatch", "Concrete", "Other"),
                             n_hh, replace = TRUE, prob = c(0.25, 0.45, 0.15, 0.10, 0.05)),
      water_source = sample(c("Piped", "Borehole", "Well", "River", "Other"),
                            n_hh, replace = TRUE, prob = c(0.35, 0.25, 0.20, 0.15, 0.05)),
      electricity = sample(c("Grid", "Solar", "Generator", "None"),
                           n_hh, replace = TRUE, prob = c(0.50, 0.10, 0.05, 0.35)),
      toilet_type = sample(c("Flush", "Pit", "VIP", "None", "Other"),
                           n_hh, replace = TRUE, prob = c(0.30, 0.35, 0.15, 0.15, 0.05)),
      cooking_fuel = sample(c("Electricity", "Gas", "Paraffin", "Wood", "Charcoal", "Other"),
                            n_hh, replace = TRUE, prob = c(0.25, 0.15, 0.10, 0.30, 0.15, 0.05)),
      n_rooms = round(rtruncnorm(n_hh, a = 1, b = 10, mean = 3, sd = 1.5)),
      n_bedrooms = round(rtruncnorm(n_hh, a = 0, b = 6, mean = 2, sd = 1)),
      has_radio = sample(0:1, n_hh, replace = TRUE, prob = c(0.3, 0.7)),
      has_tv = sample(0:1, n_hh, replace = TRUE, prob = c(0.5, 0.5)),
      has_fridge = sample(0:1, n_hh, replace = TRUE, prob = c(0.6, 0.4)),
      has_bicycle = sample(0:1, n_hh, replace = TRUE, prob = c(0.7, 0.3)),
      has_motorcycle = sample(0:1, n_hh, replace = TRUE, prob = c(0.85, 0.15)),
      has_car = sample(0:1, n_hh, replace = TRUE, prob = c(0.8, 0.2)),
      has_computer = sample(0:1, n_hh, replace = TRUE, prob = c(0.75, 0.25)),
      has_internet = sample(0:1, n_hh, replace = TRUE, prob = c(0.65, 0.35)),
      monthly_income = round(rlnorm(n_hh, meanlog = 7.5, sdlog = 1.2)),
      monthly_expenditure = NA,
      receives_grants = sample(0:1, n_hh, replace = TRUE, prob = c(0.7, 0.3)),
      agricultural_hh = sample(0:1, n_hh, replace = TRUE, prob = c(0.5, 0.5)),
      livestock_owned = sample(0:1, n_hh, replace = TRUE, prob = c(0.6, 0.4)),
      interview_mode = sample(c("Face-to-face", "Telephone", "Web"),
                              n_hh, replace = TRUE, prob = c(0.85, 0.12, 0.03)),
      interview_duration_min = round(rnorm(n_hh, mean = 45, sd = 15)),
      respondent_age = round(rtruncnorm(n_hh, a = 18, b = 85, mean = 42, sd = 15)),
      respondent_gender = sample(c("Male", "Female"), n_hh, replace = TRUE),
      interview_language = sample(c("English", "Portuguese", "French", "Local"),
                                  n_hh, replace = TRUE, prob = c(0.4, 0.2, 0.1, 0.3)),
      interview_result = sample(c("Complete", "Partial", "Refused", "Not Home"),
                                n_hh, replace = TRUE, prob = c(0.85, 0.08, 0.04, 0.03)),
      psu_weight = ea$psu_weight,
      hh_weight = NA,
      final_weight = NA,
      quality_check = sample(0:1, n_hh, replace = TRUE, prob = c(0.9, 0.1)),
      data_entry_date = as.Date("2024-01-15") + sample(1:200, n_hh, replace = TRUE)
    )
    
    household_list[[i]] <- hh_data
  }
  
  households <- bind_rows(household_list)
  
  # Calculate expenditure as function of income with noise
  households$monthly_expenditure <- round(households$monthly_income * 
                                            runif(nrow(households), 0.6, 0.95))
  
  # Calculate household weights
  households$hh_weight <- households$psu_weight * 
    runif(nrow(households), 0.8, 1.2)  # Second stage adjustment
  
  # Apply non-response adjustment
  nr_adjustment <- ifelse(households$interview_result == "Complete", 1.0,
                          ifelse(households$interview_result == "Partial", 0.5, 0))
  households$final_weight <- households$hh_weight / mean(nr_adjustment[nr_adjustment > 0])
  households$final_weight[nr_adjustment == 0] <- 0
  
  # Add some item non-response
  vars_with_missing <- c("monthly_income", "monthly_expenditure", "n_bedrooms")
  for (var in vars_with_missing) {
    missing_idx <- sample(1:nrow(households), size = nrow(households) * 0.05)
    households[[var]][missing_idx] <- NA
  }
  
  return(households)
}

# ═══════════════════════════════════════════════════════════════════════════
# 3. HOUSEHOLD ROSTER FILE
# ═══════════════════════════════════════════════════════════════════════════

generate_household_roster <- function(households) {
  roster_list <- list()
  
  for (i in 1:nrow(households)) {
    hh <- households[i, ]
    n_members <- hh$household_size
    
    if (is.na(n_members)) n_members <- 4  # Default if missing
    
    roster <- data.frame(
      person_id = sprintf("%s_%02d", hh$household_id, 1:n_members),
      household_id = hh$household_id,
      ea_id = hh$ea_id,
      member_number = 1:n_members,
      relationship = NA,
      age = NA,
      gender = sample(c("Male", "Female"), n_members, replace = TRUE),
      marital_status = NA,
      education_level = NA,
      employment_status = NA,
      occupation_code = NA,
      industry_code = NA,
      hours_worked_week = NA,
      income_monthly = NA,
      chronic_illness = sample(0:1, n_members, replace = TRUE, prob = c(0.85, 0.15)),
      disability = sample(0:1, n_members, replace = TRUE, prob = c(0.92, 0.08)),
      school_attendance = NA,
      literacy = NA,
      birth_certificate = sample(0:1, n_members, replace = TRUE, prob = c(0.2, 0.8)),
      id_document = NA,
      mobile_phone = NA,
      migrant_status = sample(c("Non-migrant", "Internal", "International"),
                              n_members, replace = TRUE, prob = c(0.85, 0.12, 0.03))
    )
    
    # Set household head
    roster$relationship[1] <- "Head"
    roster$age[1] <- hh$respondent_age
    roster$gender[1] <- hh$respondent_gender
    
    # Generate ages for other members
    if (n_members > 1) {
      # Spouse (if exists)
      if (n_members >= 2 && runif(1) > 0.3) {
        roster$relationship[2] <- "Spouse"
        roster$age[2] <- roster$age[1] + round(rnorm(1, 0, 5))
        roster$gender[2] <- ifelse(roster$gender[1] == "Male", "Female", "Male")
      }
      
      # Children and others
      for (j in 2:n_members) {
        if (is.na(roster$relationship[j])) {
          roster$relationship[j] <- sample(c("Child", "Grandchild", "Parent", 
                                             "Sibling", "Other relative", "Non-relative"),
                                           1, prob = c(0.45, 0.15, 0.1, 0.15, 0.1, 0.05))
        }
        if (is.na(roster$age[j])) {
          if (roster$relationship[j] == "Child") {
            roster$age[j] <- round(runif(1, 0, min(30, roster$age[1] - 15)))
          } else if (roster$relationship[j] == "Parent") {
            roster$age[j] <- roster$age[1] + round(runif(1, 20, 40))
          } else {
            roster$age[j] <- round(rtruncnorm(1, a = 0, b = 95, mean = 25, sd = 20))
          }
        }
      }
    }
    
    # Set marital status based on age - vectorized approach
    roster$marital_status <- ifelse(
      roster$age < 15, 
      "Never married",
      sample(c("Never married", "Married", "Divorced", "Widowed", "Separated"),
             n_members, replace = TRUE, prob = c(0.25, 0.50, 0.10, 0.10, 0.05))
    )
    
    # Set education based on age - vectorized approach with proper length matching
    education_vec <- character(n_members)
    for (k in 1:n_members) {
      if (roster$age[k] < 5) {
        education_vec[k] <- "None"
      } else if (roster$age[k] < 15) {
        education_vec[k] <- sample(c("None", "Primary incomplete", "Primary complete"),
                                   1, prob = c(0.1, 0.6, 0.3))
      } else {
        education_vec[k] <- sample(c("None", "Primary incomplete", "Primary complete",
                                     "Secondary incomplete", "Secondary complete", "Tertiary"),
                                   1, prob = c(0.15, 0.15, 0.20, 0.20, 0.20, 0.10))
      }
    }
    roster$education_level <- education_vec
    
    # Set employment based on age - vectorized approach with proper length matching
    employment_vec <- character(n_members)
    for (k in 1:n_members) {
      if (roster$age[k] < 15) {
        employment_vec[k] <- "Not applicable"
      } else if (roster$age[k] > 65) {
        employment_vec[k] <- sample(c("Retired", "Employed", "Unemployed"),
                                    1, prob = c(0.6, 0.2, 0.2))
      } else {
        employment_vec[k] <- sample(c("Employed", "Unemployed", "Student", "Homemaker"),
                                    1, prob = c(0.45, 0.25, 0.20, 0.10))
      }
    }
    roster$employment_status <- employment_vec
    
    # Set work hours for employed - count employed individuals first
    employed_indices <- which(roster$employment_status == "Employed")
    n_employed <- length(employed_indices)
    
    if (n_employed > 0) {
      roster$hours_worked_week[employed_indices] <- 
        round(rtruncnorm(n_employed, a = 1, b = 80, mean = 40, sd = 10))
      
      # Set income for workers
      roster$income_monthly[employed_indices] <- 
        round(rlnorm(n_employed, meanlog = 6.5, sdlog = 1))
    }
    
    # School attendance for young people - vectorized with proper length
    attendance_vec <- character(n_members)
    for (k in 1:n_members) {
      if (roster$age[k] < 5) {
        attendance_vec[k] <- "Not applicable"
      } else if (roster$age[k] > 25) {
        attendance_vec[k] <- "Not attending"
      } else {
        attendance_vec[k] <- sample(c("Attending", "Not attending"),
                                    1, prob = c(0.75, 0.25))
      }
    }
    roster$school_attendance <- attendance_vec
    
    # Literacy based on education - vectorized approach
    roster$literacy <- ifelse(
      roster$age < 5,
      NA,
      ifelse(roster$education_level %in% c("None", "Primary incomplete"),
             sample(0:1, n_members, replace = TRUE, prob = c(0.4, 0.6)),
             1)
    )
    
    # ID document for adults
    roster$id_document <- ifelse(
      roster$age < 16,
      0,
      sample(0:1, n_members, replace = TRUE, prob = c(0.15, 0.85))
    )
    
    # Mobile phone for teens and adults
    roster$mobile_phone <- ifelse(
      roster$age < 12,
      0,
      sample(0:1, n_members, replace = TRUE, prob = c(0.25, 0.75))
    )
    
    roster_list[[i]] <- roster
  }
  
  household_roster <- bind_rows(roster_list)
  
  # Add some missing values
  vars_with_missing <- c("income_monthly", "hours_worked_week", "education_level")
  for (var in vars_with_missing) {
    missing_idx <- sample(1:nrow(household_roster), size = nrow(household_roster) * 0.03)
    household_roster[[var]][missing_idx] <- NA
  }
  
  return(household_roster)
}

# ═══════════════════════════════════════════════════════════════════════════
# 4. AUXILIARY CENSUS DATA
# ═══════════════════════════════════════════════════════════════════════════

generate_auxiliary_census <- function() {
  # District level census data for small area estimation
  n_districts <- 150
  
  census_data <- data.frame(
    district_code = sprintf("D%03d", 1:n_districts),
    country = sample(sadc_countries[1:6], n_districts, replace = TRUE),
    district_name = paste("District", 1:n_districts),
    total_population = round(rlnorm(n_districts, meanlog = 11, sdlog = 0.8)),
    total_households = NA,
    urban_population = NA,
    rural_population = NA,
    population_density = round(rlnorm(n_districts, meanlog = 3.5, sdlog = 1.2)),
    sex_ratio = rnorm(n_districts, mean = 95, sd = 5),
    median_age = rnorm(n_districts, mean = 22, sd = 4),
    dependency_ratio = rnorm(n_districts, mean = 85, sd = 15),
    literacy_rate = rbeta(n_districts, 6, 4) * 100,
    primary_completion_rate = rbeta(n_districts, 7, 3) * 100,
    secondary_completion_rate = rbeta(n_districts, 5, 5) * 100,
    unemployment_rate = rbeta(n_districts, 2, 8) * 100,
    formal_employment_rate = rbeta(n_districts, 4, 6) * 100,
    poverty_headcount = rbeta(n_districts, 3, 7) * 100,
    electricity_access = rbeta(n_districts, 6, 4) * 100,
    piped_water_access = rbeta(n_districts, 5, 5) * 100,
    improved_sanitation = rbeta(n_districts, 6, 4) * 100,
    mobile_phone_coverage = rbeta(n_districts, 7, 3) * 100,
    internet_access = rbeta(n_districts, 3, 7) * 100,
    health_facilities_per_10000 = rgamma(n_districts, shape = 2, rate = 1),
    schools_per_10000 = rgamma(n_districts, shape = 3, rate = 1),
    avg_household_size = rnorm(n_districts, mean = 4.2, sd = 0.8),
    stunting_rate = rbeta(n_districts, 3, 7) * 100,
    infant_mortality_rate = rgamma(n_districts, shape = 2, rate = 0.05),
    gdp_per_capita = round(rlnorm(n_districts, meanlog = 7, sdlog = 0.6)),
    gini_coefficient = rbeta(n_districts, 5, 5) * 0.3 + 0.35,
    census_year = 2022,
    projection_year = 2024
  )
  
  # Derive related variables
  census_data$total_households <- round(census_data$total_population / 
                                          census_data$avg_household_size)
  
  urban_prop <- rbeta(n_districts, 4, 6)
  census_data$urban_population <- round(census_data$total_population * urban_prop)
  census_data$rural_population <- census_data$total_population - census_data$urban_population
  
  # Add data quality indicators
  census_data$data_quality_score <- rbeta(n_districts, 8, 2) * 100
  census_data$coverage_rate <- rbeta(n_districts, 9, 1) * 100
  
  return(census_data)
}

# ═══════════════════════════════════════════════════════════════════════════
# 5. PANEL ROTATION COHORT DATA
# ═══════════════════════════════════════════════════════════════════════════

generate_panel_rotation <- function() {
  n_panel_hh <- 1000
  n_waves = 4
  
  panel_data_list <- list()
  
  for (wave in 1:n_waves) {
    # Determine which households are in this wave
    if (wave == 1) {
      active_hh <- 1:n_panel_hh
    } else {
      # 25% rotation: drop oldest quartile, add new quartile
      continuing <- sample(active_hh, size = length(active_hh) * 0.75)
      new_hh <- max(active_hh) + 1:(length(active_hh) * 0.25)
      active_hh <- c(continuing, new_hh)
    }
    
    panel_wave <- data.frame(
      panel_household_id = sprintf("PH%05d", active_hh),
      wave = wave,
      wave_date = as.Date("2023-01-01") + (wave - 1) * 90,
      rotation_group = (active_hh - 1) %% 4 + 1,
      panel_status = sample(c("Continuing", "New", "Attrited"),
                            length(active_hh), replace = TRUE,
                            prob = c(0.70, 0.25, 0.05)),
      interview_result = sample(c("Complete", "Partial", "Refused", "Not found"),
                                length(active_hh), replace = TRUE,
                                prob = c(0.85, 0.08, 0.04, 0.03)),
      household_size = round(rtruncnorm(length(active_hh), a = 1, b = 12, mean = 4.3, sd = 1.8)),
      monthly_income = round(rlnorm(length(active_hh), meanlog = 7.5, sdlog = 1.1)),
      employment_adults = round(rtruncnorm(length(active_hh), a = 0, b = 6, mean = 1.5, sd = 1)),
      school_children = round(rtruncnorm(length(active_hh), a = 0, b = 5, mean = 1.2, sd = 1)),
      chronic_illness_any = sample(0:1, length(active_hh), replace = TRUE, prob = c(0.7, 0.3)),
      food_security_score = rnorm(length(active_hh), mean = 65, sd = 20),
      dwelling_quality_index = rnorm(length(active_hh), mean = 50, sd = 15),
      asset_index = rnorm(length(active_hh), mean = 0, sd = 1),
      panel_weight_base = runif(length(active_hh), 80, 150),
      attrition_weight = NA,
      panel_weight_final = NA
    )
    
    # Calculate attrition weights
    if (wave > 1) {
      panel_wave$attrition_weight <- ifelse(panel_wave$panel_status == "Continuing",
                                            runif(length(active_hh), 1.0, 1.3), 1.0)
    } else {
      panel_wave$attrition_weight <- 1.0
    }
    
    panel_wave$panel_weight_final <- panel_wave$panel_weight_base * panel_wave$attrition_weight
    
    panel_data_list[[wave]] <- panel_wave
  }
  
  panel_rotation <- bind_rows(panel_data_list)
  
  # Add transitions between waves
  panel_rotation <- panel_rotation %>%
    arrange(panel_household_id, wave) %>%
    group_by(panel_household_id) %>%
    mutate(
      income_change = monthly_income - lag(monthly_income),
      employment_change = employment_adults - lag(employment_adults),
      household_size_change = household_size - lag(household_size)
    ) %>%
    ungroup()
  
  return(panel_rotation)
}

# ═══════════════════════════════════════════════════════════════════════════
# 6. MOBILE POPULATIONS SAMPLE
# ═══════════════════════════════════════════════════════════════════════════

generate_mobile_populations <- function() {
  n_mobile <- 500
  
  mobile_pop <- data.frame(
    respondent_id = sprintf("MP%05d", 1:n_mobile),
    sample_method = sample(c("TLS", "RDS", "Venue"), n_mobile, replace = TRUE,
                           prob = c(0.50, 0.35, 0.15)),
    location_type = sample(c("Border post", "Transport hub", "Market", "Construction site",
                             "Mining area", "Agricultural zone"),
                           n_mobile, replace = TRUE),
    interview_date = as.Date("2024-01-01") + sample(0:180, n_mobile, replace = TRUE),
    interview_time = sprintf("%02d:%02d", sample(6:20, n_mobile, replace = TRUE),
                             sample(0:59, n_mobile, replace = TRUE)),
    age = round(rtruncnorm(n_mobile, a = 15, b = 65, mean = 28, sd = 10)),
    gender = sample(c("Male", "Female"), n_mobile, replace = TRUE, prob = c(0.65, 0.35)),
    nationality = sample(c("National", "SADC country", "Other African", "Other"),
                         n_mobile, replace = TRUE, prob = c(0.40, 0.35, 0.20, 0.05)),
    migration_reason = sample(c("Work", "Trade", "Family", "Education", "Other"),
                              n_mobile, replace = TRUE, prob = c(0.45, 0.25, 0.15, 0.10, 0.05)),
    duration_current_location_days = round(rexp(n_mobile, rate = 1/30)),
    frequency_movement = sample(c("Daily", "Weekly", "Monthly", "Seasonal", "Irregular"),
                                n_mobile, replace = TRUE),
    employment_type = sample(c("Formal", "Informal", "Self-employed", "Unemployed"),
                             n_mobile, replace = TRUE, prob = c(0.15, 0.50, 0.25, 0.10)),
    income_last_month = round(rlnorm(n_mobile, meanlog = 6.5, sdlog = 1.3)),
    accommodation_type = sample(c("Rented room", "Hostel", "Employer provided", 
                                  "Street", "Other"),
                                n_mobile, replace = TRUE),
    health_service_access = sample(0:1, n_mobile, replace = TRUE, prob = c(0.6, 0.4)),
    documentation_status = sample(c("Full", "Partial", "None"),
                                  n_mobile, replace = TRUE, prob = c(0.30, 0.45, 0.25)),
    network_size = round(rtruncnorm(n_mobile, a = 0, b = 100, mean = 15, sd = 10)),
    recruitment_coupon = NA,
    recruiter_id = NA,
    venue_size_estimate = round(rlnorm(n_mobile, meanlog = 4, sdlog = 1)),
    sampling_weight = runif(n_mobile, 50, 200),
    rds_weight = NA,
    tls_weight = NA
  )
  
  # Add RDS specific fields
  rds_idx <- which(mobile_pop$sample_method == "RDS")
  if (length(rds_idx) > 0) {
    # Simulate recruitment chains
    seeds <- sample(rds_idx, size = min(10, length(rds_idx)))
    mobile_pop$recruitment_coupon[rds_idx] <- sprintf("RDS%04d", 1:length(rds_idx))
    
    for (i in rds_idx) {
      if (i %in% seeds) {
        mobile_pop$recruiter_id[i] <- "SEED"
      } else {
        potential_recruiters <- rds_idx[rds_idx < i]
        if (length(potential_recruiters) > 0) {
          mobile_pop$recruiter_id[i] <- mobile_pop$respondent_id[sample(potential_recruiters, 1)]
        }
      }
    }
    
    # Calculate RDS weights (simplified)
    mobile_pop$rds_weight[rds_idx] <- mobile_pop$network_size[rds_idx] / 
      mean(mobile_pop$network_size[rds_idx])
  }
  
  # Add TLS weights
  tls_idx <- which(mobile_pop$sample_method == "TLS")
  if (length(tls_idx) > 0) {
    mobile_pop$tls_weight[tls_idx] <- mobile_pop$venue_size_estimate[tls_idx] / 
      mean(mobile_pop$venue_size_estimate[tls_idx])
  }
  
  return(mobile_pop)
}

# ═══════════════════════════════════════════════════════════════════════════
# 7. MIXED MODE RESPONSES
# ═══════════════════════════════════════════════════════════════════════════

generate_mixed_mode <- function() {
  n_mixed <- 2000
  
  mixed_mode <- data.frame(
    response_id = sprintf("MM%05d", 1:n_mixed),
    household_id = sprintf("HH%05d", sample(1:1500, n_mixed, replace = TRUE)),
    mode_sequence = sample(c("Web only", "Web-Phone", "Web-F2F", "Phone-F2F", "F2F only"),
                           n_mixed, replace = TRUE, prob = c(0.15, 0.20, 0.25, 0.15, 0.25)),
    initial_mode = NA,
    final_mode = NA,
    n_mode_switches = sample(0:3, n_mixed, replace = TRUE, prob = c(0.5, 0.3, 0.15, 0.05)),
    web_attempt_date = as.Date("2024-01-01") + sample(0:30, n_mixed, replace = TRUE),
    web_response = sample(c("Complete", "Partial", "No response"),
                          n_mixed, replace = TRUE, prob = c(0.25, 0.15, 0.60)),
    phone_attempt_date = NA,
    phone_response = NA,
    f2f_attempt_date = NA,
    f2f_response = NA,
    final_response_status = NA,
    response_propensity_score = rbeta(n_mixed, 2, 3),
    mode_preference_stated = sample(c("Web", "Phone", "Face-to-face", "No preference"),
                                    n_mixed, replace = TRUE),
    device_type = sample(c("Desktop", "Tablet", "Mobile", "NA"),
                         n_mixed, replace = TRUE, prob = c(0.25, 0.15, 0.40, 0.20)),
    completion_time_minutes = NA,
    data_quality_score = rnorm(n_mixed, mean = 75, sd = 15),
    item_nonresponse_rate = rbeta(n_mixed, 2, 18),
    straightlining_detected = sample(0:1, n_mixed, replace = TRUE, prob = c(0.92, 0.08)),
    mode_effect_adjustment = rnorm(n_mixed, mean = 1, sd = 0.1),
    cost_total = NA,
    interviewer_id = NA,
    paradata_available = sample(0:1, n_mixed, replace = TRUE, prob = c(0.3, 0.7))
  )
  
  # Set initial and final modes based on sequence
  mixed_mode$initial_mode <- case_when(
    grepl("Web", mixed_mode$mode_sequence) ~ "Web",
    grepl("Phone", mixed_mode$mode_sequence) ~ "Phone",
    TRUE ~ "Face-to-face"
  )
  
  mixed_mode$final_mode <- case_when(
    grepl("only", mixed_mode$mode_sequence) ~ mixed_mode$initial_mode,
    grepl("F2F", mixed_mode$mode_sequence) ~ "Face-to-face",
    grepl("Phone", mixed_mode$mode_sequence) ~ "Phone",
    TRUE ~ "Web"
  )
  
  # Set attempt dates and responses for different modes
  phone_idx <- grepl("Phone", mixed_mode$mode_sequence)
  mixed_mode$phone_attempt_date[phone_idx] <- mixed_mode$web_attempt_date[phone_idx] + 
    sample(7:21, sum(phone_idx), replace = TRUE)
  mixed_mode$phone_response[phone_idx] <- sample(c("Complete", "Partial", "No response"),
                                                 sum(phone_idx), replace = TRUE,
                                                 prob = c(0.35, 0.20, 0.45))
  
  f2f_idx <- grepl("F2F", mixed_mode$mode_sequence)
  mixed_mode$f2f_attempt_date[f2f_idx] <- mixed_mode$web_attempt_date[f2f_idx] + 
    sample(14:35, sum(f2f_idx), replace = TRUE)
  mixed_mode$f2f_response[f2f_idx] <- sample(c("Complete", "Partial", "Refused"),
                                             sum(f2f_idx), replace = TRUE,
                                             prob = c(0.75, 0.15, 0.10))
  
  # Set final response status
  mixed_mode$final_response_status <- case_when(
    mixed_mode$final_mode == "Web" ~ mixed_mode$web_response,
    mixed_mode$final_mode == "Phone" ~ mixed_mode$phone_response,
    mixed_mode$final_mode == "Face-to-face" ~ mixed_mode$f2f_response,
    TRUE ~ "No response"
  )
  
  # Set completion times by mode
  mixed_mode$completion_time_minutes <- case_when(
    mixed_mode$final_mode == "Web" ~ round(rnorm(n_mixed, mean = 25, sd = 8)),
    mixed_mode$final_mode == "Phone" ~ round(rnorm(n_mixed, mean = 35, sd = 10)),
    mixed_mode$final_mode == "Face-to-face" ~ round(rnorm(n_mixed, mean = 45, sd = 12)),
    TRUE ~ NA_real_
  )
  
  # Calculate costs by mode
  mixed_mode$cost_total <- case_when(
    mixed_mode$final_mode == "Web" ~ round(runif(n_mixed, 2, 5), 2),
    mixed_mode$final_mode == "Phone" ~ round(runif(n_mixed, 8, 15), 2),
    mixed_mode$final_mode == "Face-to-face" ~ round(runif(n_mixed, 25, 50), 2),
    TRUE ~ 0
  )
  
  # Assign interviewer IDs for phone and F2F
  needs_interviewer <- mixed_mode$final_mode %in% c("Phone", "Face-to-face")
  mixed_mode$interviewer_id[needs_interviewer] <- 
    sample(sprintf("INT%03d", 1:50), sum(needs_interviewer), replace = TRUE)
  
  return(mixed_mode)
}

# ═══════════════════════════════════════════════════════════════════════════
# 8. SMALL AREA INDICATORS
# ═══════════════════════════════════════════════════════════════════════════

generate_small_area_indicators <- function() {
  n_areas <- 500  # Small areas (sub-district level)
  
  small_area <- data.frame(
    area_id = sprintf("SA%04d", 1:n_areas),
    district_code = sprintf("D%03d", sample(1:150, n_areas, replace = TRUE)),
    area_name = paste("Area", 1:n_areas),
    area_type = sample(c("Urban formal", "Urban informal", "Rural village", 
                         "Rural scattered", "Commercial", "Industrial"),
                       n_areas, replace = TRUE),
    total_population = round(rlnorm(n_areas, meanlog = 8, sdlog = 0.9)),
    sample_size = round(runif(n_areas, 10, 100)),
    direct_estimate_poverty = rbeta(n_areas, 3, 7),
    direct_estimate_se = rbeta(n_areas, 1, 20) * 0.1,
    auxiliary_mean_income = rlnorm(n_areas, meanlog = 7, sdlog = 0.8),
    auxiliary_unemployment = rbeta(n_areas, 2, 8),
    auxiliary_education_years = rnorm(n_areas, mean = 8, sd = 2.5),
    auxiliary_asset_index = rnorm(n_areas, mean = 0, sd = 1),
    auxiliary_dependency_ratio = rnorm(n_areas, mean = 0.8, sd = 0.3),
    auxiliary_urbanization = rbeta(n_areas, 4, 6),
    satellite_nightlights = rlnorm(n_areas, meanlog = 2, sdlog = 1.5),
    satellite_vegetation = rbeta(n_areas, 6, 4),
    distance_to_road_km = rexp(n_areas, rate = 0.2),
    distance_to_market_km = rexp(n_areas, rate = 0.1),
    climate_rainfall_mm = rnorm(n_areas, mean = 800, sd = 300),
    climate_temperature_c = rnorm(n_areas, mean = 22, sd = 3),
    model_based_estimate = NA,
    model_based_mse = NA,
    fh_estimate = NA,
    fh_mse = NA,
    synthetic_estimate = NA,
    composite_estimate = NA,
    reliability_flag = sample(c("Reliable", "Use with caution", "Unreliable"),
                              n_areas, replace = TRUE, prob = c(0.60, 0.30, 0.10))
  )
  
  # Generate model-based estimates (simplified Fay-Herriot)
  # True relationship with auxiliary variables
  true_poverty <- 0.8 - 0.00003 * small_area$auxiliary_mean_income + 
    0.5 * small_area$auxiliary_unemployment - 
    0.03 * small_area$auxiliary_education_years +
    rnorm(n_areas, 0, 0.05)
  
  true_poverty[true_poverty < 0] <- 0
  true_poverty[true_poverty > 1] <- 1
  
  # Fay-Herriot estimates (simplified)
  gamma <- small_area$direct_estimate_se^2 / 
    (small_area$direct_estimate_se^2 + 0.01)  # Assuming model variance = 0.01
  
  small_area$fh_estimate <- gamma * small_area$direct_estimate_poverty + 
    (1 - gamma) * true_poverty
  small_area$fh_mse <- gamma * small_area$direct_estimate_se^2
  
  # Synthetic estimate
  small_area$synthetic_estimate <- true_poverty
  
  # Composite estimate
  small_area$composite_estimate <- 0.7 * small_area$fh_estimate + 
    0.3 * small_area$direct_estimate_poverty
  
  # Model-based estimate with some noise
  small_area$model_based_estimate <- small_area$fh_estimate + rnorm(n_areas, 0, 0.02)
  small_area$model_based_mse <- small_area$fh_mse * runif(n_areas, 0.8, 1.2)
  
  return(small_area)
}

# ═══════════════════════════════════════════════════════════════════════════
# MAIN EXECUTION
# ═══════════════════════════════════════════════════════════════════════════

cat("Generating SADC Workshop Datasets\n")
cat("═══════════════════════════════════════════════════\n")

# Generate all datasets
cat("1. Generating Enumeration Areas Master File...")
ea_master <- generate_ea_master()
cat(" Done! (", nrow(ea_master), "records)\n")

cat("2. Generating Household Survey Main File...")
household_main <- generate_household_main(ea_master)
cat(" Done! (", nrow(household_main), "records)\n")

cat("3. Generating Household Roster File...")
household_roster <- generate_household_roster(household_main)
cat(" Done! (", nrow(household_roster), "records)\n")

cat("4. Generating Auxiliary Census Data...")
auxiliary_census <- generate_auxiliary_census()
cat(" Done! (", nrow(auxiliary_census), "records)\n")

cat("5. Generating Panel Rotation Cohort Data...")
panel_rotation <- generate_panel_rotation()
cat(" Done! (", nrow(panel_rotation), "records)\n")

cat("6. Generating Mobile Populations Sample...")
mobile_populations <- generate_mobile_populations()
cat(" Done! (", nrow(mobile_populations), "records)\n")

cat("7. Generating Mixed Mode Responses...")
mixed_mode <- generate_mixed_mode()
cat(" Done! (", nrow(mixed_mode), "records)\n")

cat("8. Generating Small Area Indicators...")
small_area_indicators <- generate_small_area_indicators()
cat(" Done! (", nrow(small_area_indicators), "records)\n")

# Save all datasets
cat("\nSaving datasets to 01-Data folder...\n")

write.csv(ea_master, "01-Data/enumeration_areas_master.csv", row.names = FALSE)
write.csv(household_main, "01-Data/household_survey_main_2024.csv", row.names = FALSE)
write.csv(household_roster, "01-Data/household_roster_2024.csv", row.names = FALSE)
write.csv(auxiliary_census, "01-Data/auxiliary_census_2022.csv", row.names = FALSE)
write.csv(panel_rotation, "01-Data/panel_rotation_cohort_2023.csv", row.names = FALSE)
write.csv(mobile_populations, "01-Data/mobile_populations_sample.csv", row.names = FALSE)
write.csv(mixed_mode, "01-Data/mixed_mode_responses.csv", row.names = FALSE)
write.csv(small_area_indicators, "01-Data/small_area_indicators.csv", row.names = FALSE)

cat("\n═══════════════════════════════════════════════════\n")
cat("All datasets generated successfully!\n")
cat("Total files created: 8\n")
cat("Total records generated: ", 
    nrow(ea_master) + nrow(household_main) + nrow(household_roster) + 
      nrow(auxiliary_census) + nrow(panel_rotation) + nrow(mobile_populations) +
      nrow(mixed_mode) + nrow(small_area_indicators), "\n")
cat("═══════════════════════════════════════════════════\n")