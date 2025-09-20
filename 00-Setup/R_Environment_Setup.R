# ═══════════════════════════════════════════════════════════════════════════
# SADC Advanced Sampling Methods Workshop
# R Environment Setup and Configuration Script
# ═══════════════════════════════════════════════════════════════════════════

# Script Purpose:
# - Configure R environment for workshop requirements
# - Install and verify all necessary packages
# - Set up project structure and paths
# - Configure options for reproducible research

# ═══════════════════════════════════════════════════════════════════════════
# PART 1: INITIAL CONFIGURATION
# ═══════════════════════════════════════════════════════════════════════════

# Clear workspace and set options
rm(list = ls())
gc()

# Set global options for the session
options(
  repos = c(CRAN = "https://cran.rstudio.com/"),
  stringsAsFactors = FALSE,
  digits = 4,
  scipen = 10,
  width = 80,
  max.print = 100,
  warning.length = 1000,
  tidyverse.quiet = TRUE,
  dplyr.summarise.inform = FALSE,
  readr.show_col_types = FALSE
)

# Set random seed for reproducibility across all workshop materials
set.seed(2024)

# ═══════════════════════════════════════════════════════════════════════════
# PART 2: PACKAGE INSTALLATION
# ═══════════════════════════════════════════════════════════════════════════

cat("═══════════════════════════════════════════════════════════════\n")
cat("Starting SADC Workshop Environment Setup\n")
cat("This process may take 10-15 minutes\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

# Function to install packages if not already installed
install_if_missing <- function(packages) {
  installed <- rownames(installed.packages())
  to_install <- packages[!packages %in% installed]
  
  if (length(to_install) > 0) {
    cat("Installing missing packages:", paste(to_install, collapse = ", "), "\n")
    install.packages(to_install, dependencies = TRUE, quiet = TRUE)
  } else {
    cat("All specified packages already installed\n")
  }
  
  # Return installation status
  invisible(all(packages %in% rownames(installed.packages())))
}

# Define package groups for organized installation
packages_survey <- c(
  "survey",       # Complex survey analysis
  "sampling",     # Sampling algorithms and selection
  "sae",          # Small area estimation
  "pps",          # PPS sampling procedures
  "samplingVarEst", # Variance estimation for complex samples
  "PracTools"     # Practical tools for survey sampling
)

packages_data <- c(
  "tidyverse",    # Complete data science ecosystem
  "data.table",   # High-performance data manipulation
  "janitor",      # Data cleaning utilities
  "haven",        # Import SPSS/Stata/SAS files
  "readxl",       # Excel file import
  "writexl",      # Excel file export
  "validate",     # Data validation rules
  "naniar"        # Missing data visualization
)

packages_stats <- c(
  "lme4",         # Linear mixed-effects models
  "Matrix",       # Sparse and dense matrix operations
  "MASS",         # Modern applied statistics functions
  "boot",         # Bootstrap resampling
  "car",          # Companion to applied regression
  "emmeans",      # Estimated marginal means
  "broom",        # Tidy statistical outputs
  "performance"   # Model performance assessment
)

packages_viz <- c(
  "ggplot2",      # Grammar of graphics (in tidyverse)
  "plotly",       # Interactive visualizations
  "sf",           # Simple features for spatial data
  "tmap",         # Thematic maps
  "viridis",      # Color-blind friendly palettes
  "scales",       # Scale functions for graphics
  "patchwork",    # Combine multiple plots
  "corrplot"      # Correlation matrices visualization
)

packages_report <- c(
  "rmarkdown",    # Dynamic document generation
  "knitr",        # Dynamic report generation
  "xaringan",     # Presentation slides
  "kableExtra",   # Enhanced table formatting
  "gt",           # Grammar of tables
  "flextable",    # Flexible table formatting
  "officedown",   # MS Office document generation
  "pagedown"      # Paginated HTML documents
)

packages_utility <- c(
  "here",         # Project-relative file paths
  "conflicted",   # Function conflict resolution
  "devtools",     # Development tools
  "roxygen2",     # Documentation generation
  "testthat",     # Unit testing framework
  "bench",        # Performance benchmarking
  "tictoc",       # Simple timing functions
  "progressr"     # Progress notifications
)

# Install packages by group with progress reporting
cat("\n[1/6] Installing survey statistics packages...\n")
install_if_missing(packages_survey)

cat("\n[2/6] Installing data manipulation packages...\n")
install_if_missing(packages_data)

cat("\n[3/6] Installing statistical modeling packages...\n")
install_if_missing(packages_stats)

cat("\n[4/6] Installing visualization packages...\n")
install_if_missing(packages_viz)

cat("\n[5/6] Installing reporting packages...\n")
install_if_missing(packages_report)

cat("\n[6/6] Installing utility packages...\n")
install_if_missing(packages_utility)

# ═══════════════════════════════════════════════════════════════════════════
# PART 3: PACKAGE VERIFICATION
# ═══════════════════════════════════════════════════════════════════════════

cat("\n═══════════════════════════════════════════════════════════════\n")
cat("Verifying package installations...\n")
cat("═══════════════════════════════════════════════════════════════\n")

# Combine all packages for verification
all_packages <- c(packages_survey, packages_data, packages_stats, 
                  packages_viz, packages_report, packages_utility)

# Remove duplicates
all_packages <- unique(all_packages)

# Check each package
verification_results <- data.frame(
  Package = character(),
  Installed = logical(),
  Version = character(),
  stringsAsFactors = FALSE
)

for (pkg in all_packages) {
  installed <- requireNamespace(pkg, quietly = TRUE)
  version <- if (installed) as.character(packageVersion(pkg)) else "Not installed"
  
  verification_results <- rbind(
    verification_results,
    data.frame(
      Package = pkg,
      Installed = installed,
      Version = version,
      stringsAsFactors = FALSE
    )
  )
}

# Display results
n_installed <- sum(verification_results$Installed)
n_total <- nrow(verification_results)

cat("\nInstallation Summary:\n")
cat(sprintf("Successfully installed: %d/%d packages\n", n_installed, n_total))

if (n_installed < n_total) {
  cat("\nFailed installations:\n")
  failed <- verification_results[!verification_results$Installed, "Package"]
  cat(paste("  -", failed), sep = "\n")
  cat("\nPlease install these packages manually or check error messages above.\n")
}

# ═══════════════════════════════════════════════════════════════════════════
# PART 4: PROJECT STRUCTURE SETUP
# ═══════════════════════════════════════════════════════════════════════════

cat("\n═══════════════════════════════════════════════════════════════\n")
cat("Setting up project directory structure...\n")
cat("═══════════════════════════════════════════════════════════════\n")

# Define required directories
project_dirs <- c(
  "00-Setup",
  "01-Data",
  "02-Scripts",
  "03-Outputs",
  "03-Outputs/design_diagnostics",
  "03-Outputs/variance_estimates",
  "03-Outputs/small_area_maps",
  "03-Outputs/quality_reports",
  "03-Outputs/final_weights",
  "04-Presentations",
  "05-Exercises",
  "06-Solutions",
  "07-Resources",
  "08-Harry-Journey"
)

# Create directories if they don't exist
for (dir in project_dirs) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    cat("Created:", dir, "\n")
  } else {
    cat("Exists: ", dir, "\n")
  }
}

# ═══════════════════════════════════════════════════════════════════════════
# PART 5: CUSTOM FUNCTIONS LIBRARY
# ═══════════════════════════════════════════════════════════════════════════

cat("\n═══════════════════════════════════════════════════════════════\n")
cat("Loading custom workshop functions...\n")
cat("═══════════════════════════════════════════════════════════════\n")

# Function to calculate design effect
calculate_deff <- function(design, variable) {
  # Calculate design effect for a variable in a survey design
  # design: survey design object
  # variable: character string of variable name
  
  formula_obj <- as.formula(paste0("~", variable))
  
  # Get design-based variance
  design_var <- SE(svymean(formula_obj, design, na.rm = TRUE))^2
  
  # Get SRS variance (assuming same sample size)
  n <- sum(weights(design) > 0)
  pop_var <- svyvar(formula_obj, design, na.rm = TRUE)[1]
  srs_var <- pop_var / n
  
  # Calculate DEFF
  deff <- design_var / srs_var
  
  return(as.numeric(deff))
}

# Function to check coefficient of variation
check_cv <- function(estimate, se, threshold = 0.20) {
  # Calculate and evaluate coefficient of variation
  # estimate: point estimate
  # se: standard error
  # threshold: maximum acceptable CV (default 20%)
  
  cv <- se / abs(estimate)
  quality <- case_when(
    cv < 0.05 ~ "Excellent (CV < 5%)",
    cv < 0.10 ~ "Good (CV < 10%)",
    cv < 0.15 ~ "Acceptable (CV < 15%)",
    cv < 0.20 ~ "Marginal (CV < 20%)",
    TRUE ~ "Poor (CV >= 20%)"
  )
  
  return(list(cv = cv, quality = quality))
}

# Function for PPS selection
select_pps <- function(frame, size_measure, n_select, method = "systematic") {
  # Probability proportional to size selection
  # frame: sampling frame data
  # size_measure: variable name for size measure
  # n_select: number of units to select
  # method: "systematic" or "random"
  
  require(sampling)
  
  if (method == "systematic") {
    selected <- sampling::UPsystematic(frame[[size_measure]])
  } else {
    selected <- sampling::UPrandomsystematic(frame[[size_measure]])
  }
  
  frame$selected <- selected
  frame$inclusion_prob <- n_select * frame[[size_measure]] / sum(frame[[size_measure]])
  
  return(frame[frame$selected == 1, ])
}

# Save custom functions for use in scripts
cat("\n✓ Custom functions loaded into environment\n")

# ═══════════════════════════════════════════════════════════════════════════
# PART 6: SYSTEM INFORMATION
# ═══════════════════════════════════════════════════════════════════════════

cat("\n═══════════════════════════════════════════════════════════════\n")
cat("System Information\n")
cat("═══════════════════════════════════════════════════════════════\n")

# Display R version
cat("R Version:", R.version.string, "\n")

# Display platform
cat("Platform:", R.version$platform, "\n")

# Display locale
cat("Locale:", Sys.getlocale("LC_COLLATE"), "\n")

# Memory information
if (.Platform$OS.type == "windows") {
  memory_limit <- memory.limit()
  cat("Memory Limit:", memory_limit, "MB\n")
} else {
  cat("Memory: System-managed\n")
}

# CPU information
n_cores <- parallel::detectCores()
cat("CPU Cores:", n_cores, "\n")



# ═══════════════════════════════════════════════════════════════════════════
# PART 7: FINAL CONFIGURATION
# ═══════════════════════════════════════════════════════════════════════════

cat("\n═══════════════════════════════════════════════════════════════\n")
cat("Finalizing environment configuration...\n")
cat("═══════════════════════════════════════════════════════════════\n")

# Set ggplot2 theme for consistency across all visualizations
if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)
  
  # Define custom theme for workshop
  theme_sadc <- theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(color = "gray40"),
      plot.caption = element_text(color = "gray60", size = 9),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 10),
      axis.title = element_text(face = "bold", size = 11)
    )
  
  # Set as default
  theme_set(theme_sadc)
  
  cat("✓ Custom ggplot2 theme configured\n")
}

# Configure conflicted package for function conflicts
if (requireNamespace("conflicted", quietly = TRUE)) {
  library(conflicted)
  
  # Set preferences for common conflicts
  conflict_prefer("filter", "dplyr")
  conflict_prefer("select", "dplyr")
  conflict_prefer("lag", "dplyr")
  conflict_prefer("first", "dplyr")
  conflict_prefer("last", "dplyr")
  
  cat("✓ Package conflict preferences set\n")
}

# Load core packages silently for immediate use
suppressPackageStartupMessages({
  library(tidyverse)
  library(survey)
  library(here)
  library(data.table)
})

cat("✓ Core packages loaded\n")

# ═══════════════════════════════════════════════════════════════════════════
# PART 8: COMPLETION MESSAGE
# ═══════════════════════════════════════════════════════════════════════════

cat("\n═══════════════════════════════════════════════════════════════\n")
cat("    SADC WORKSHOP ENVIRONMENT SETUP COMPLETE! ✓               \n")
cat("═══════════════════════════════════════════════════════════════\n")
cat("\n")
cat("Environment Summary:\n")
cat("  • R Version:", R.version.string, "\n")
cat("  • Packages Installed:", n_installed, "of", n_total, "\n")
cat("  • Project Directories: Created\n")
cat("  • Custom Functions: Loaded\n")
cat("  • Default Theme: Configured\n")
cat("\n")
cat("You are now ready to begin the workshop!\n")
cat("\n")
cat("Next steps:\n")
cat("  1. Run Script_1.1_Environment_Setup.R to verify data import\n")
cat("  2. Open Day1_Foundations_International_Standards.Rmd\n")
cat("  3. Review 05-Exercises/Exercise_1.1_Sampling_Frame_Assessment.md\n")
cat("\n")
cat("For support: workshop.tech@sadc-stats.org\n")
cat("═══════════════════════════════════════════════════════════════\n")

# Save session info for reference
session_info <- sessionInfo()
saveRDS(session_info, file = "00-Setup/session_info.rds")
cat("\nSession information saved to: 00-Setup/session_info.rds\n")

# End of setup script