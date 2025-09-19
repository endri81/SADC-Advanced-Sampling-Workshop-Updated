#===============================================================================
# ADVANCED SAMPLING METHODS FOR HOUSEHOLD SURVEYS
# Setup Script: Installing and Loading Essential Packages
# Dr. Endri Raço
# 
# Purpose: This script ensures all workshop participants have the same
#          R environment with all necessary packages installed and loaded.
#          Run this script once at the beginning of Day 1.
#===============================================================================

# Clear the workspace to start fresh
rm(list = ls())

# Set CRAN mirror for consistent package downloads
options(repos = c(CRAN = "https://cloud.r-project.org/"))

#-------------------------------------------------------------------------------
# STEP 1: Define Required Packages
#-------------------------------------------------------------------------------

# Core packages we'll use throughout the workshop
required_packages <- c(
  # Data manipulation and visualization
  "tidyverse",      # Modern R toolkit for data science (includes ggplot2, dplyr, tidyr)
  "here",           # Simplifies file paths - no more setwd() headaches!
  "janitor",        # Clean variable names and create tables
  "scales",         # Format numbers and axes in plots
  
  # Survey statistics packages
  "survey",         # The crown jewel - handles complex survey designs
  "srvyr",          # Makes survey package work seamlessly with tidyverse
  "sampling",       # Various sampling algorithms and tools
  
  # Presentation and reporting
  "xaringan",       # Creates beautiful HTML presentations
  "knitr",          # Dynamic report generation
  "kableExtra",     # Beautiful tables for reports
  "DT",             # Interactive data tables
  
  # Color palettes and themes
  "viridis",        # Colorblind-friendly palettes
  "RColorBrewer",   # Additional color schemes
  
  # Data import/export
  "readxl",         # Read Excel files
  "writexl",        # Write Excel files
  "haven"           # Read SPSS, Stata, and SAS files
)

#-------------------------------------------------------------------------------
# STEP 2: Install Missing Packages
#-------------------------------------------------------------------------------

# Check which packages are not yet installed
packages_to_install <- required_packages[!required_packages %in% 
                                           installed.packages()[, "Package"]]

# Install any missing packages
if(length(packages_to_install) > 0) {
  cat("\n========================================\n")
  cat("Installing missing packages...\n")
  cat("========================================\n\n")
  
  for(pkg in packages_to_install) {
    cat(paste("Installing:", pkg, "\n"))
    install.packages(pkg, quiet = TRUE)
  }
  
  cat("\n✓ All packages successfully installed!\n")
} else {
  cat("\n✓ Great! All required packages are already installed.\n")
}

#-------------------------------------------------------------------------------
# STEP 3: Load All Packages
#-------------------------------------------------------------------------------

cat("\n========================================\n")
cat("Loading packages...\n")
cat("========================================\n\n")

# Load each package and report success
for(pkg in required_packages) {
  library(pkg, character.only = TRUE, quietly = TRUE)
  cat(paste("✓", pkg, "loaded successfully\n"))
}

#-------------------------------------------------------------------------------
# STEP 4: Set Global Options for Better Experience
#-------------------------------------------------------------------------------

cat("\n========================================\n")
cat("Configuring R environment...\n")
cat("========================================\n\n")

# Prevent scientific notation for better readability
options(scipen = 999)

# Set default theme for ggplot2 (clean and professional)
theme_set(theme_minimal(base_size = 12))

# Set default color palette
options(ggplot2.continuous.colour = "viridis")
options(ggplot2.continuous.fill = "viridis")

# Increase console width for better output display
options(width = 80)

# Set random seed for reproducibility
# We use 2025 to remember the workshop year
set.seed(2025)

cat("✓ Scientific notation disabled\n")
cat("✓ Default plot theme set\n")
cat("✓ Color palette configured\n")
cat("✓ Random seed set to 2025\n")

#-------------------------------------------------------------------------------
# STEP 5: Verify Everything Works
#-------------------------------------------------------------------------------

cat("\n========================================\n")
cat("Running verification tests...\n")
cat("========================================\n\n")

# Test 1: Can we create a simple tibble?
test_data <- tibble(
  id = 1:5,
  value = rnorm(5)
)

if(exists("test_data")) {
  cat("✓ Data creation works\n")
} else {
  cat("✗ Problem with data creation\n")
}

# Test 2: Can we use the here package?
project_root <- here::here()
cat(paste("✓ Project root identified as:", project_root, "\n"))

# Test 3: Can we create a simple plot?
test_plot <- ggplot(test_data, aes(x = id, y = value)) + 
  geom_point() +
  labs(title = "Test Plot")

if(exists("test_plot")) {
  cat("✓ Plotting system works\n")
} else {
  cat("✗ Problem with plotting\n")
}

# Clean up test objects
rm(test_data, test_plot)

#-------------------------------------------------------------------------------
# STEP 6: Create Project Folders (if they don't exist)
#-------------------------------------------------------------------------------

cat("\n========================================\n")
cat("Setting up project folders...\n")
cat("========================================\n\n")

# Define folder structure
folders <- c(
  "00-Setup",
  "01-Data",
  "02-Scripts", 
  "03-Outputs",
  "04-Presentations",
  "05-Exercises"
)

# Create each folder if it doesn't exist
for(folder in folders) {
  folder_path <- here::here(folder)
  if(!dir.exists(folder_path)) {
    dir.create(folder_path, recursive = TRUE)
    cat(paste("✓ Created folder:", folder, "\n"))
  } else {
    cat(paste("✓ Folder exists:", folder, "\n"))
  }
}

#-------------------------------------------------------------------------------
# FINAL MESSAGE
#-------------------------------------------------------------------------------

cat("\n")
cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║                                                                ║\n")
cat("║   🎉  SETUP COMPLETE! YOU'RE READY FOR THE WORKSHOP!  🎉     ║\n")
cat("║                                                                ║\n")
cat("║   All packages are installed and loaded.                      ║\n")
cat("║   Your R environment is configured.                           ║\n")
cat("║   Project folders are ready.                                  ║\n")
cat("║                                                                ║\n")
cat("║   Let's begin our journey with Harry!                         ║\n")
cat("║                                                                ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n")

# Display session information for troubleshooting
cat("\n========================================\n")
cat("Session Information (for reference):\n")
cat("========================================\n")
cat(paste("R version:", R.version.string, "\n"))
cat(paste("Platform:", Sys.info()["sysname"], Sys.info()["release"], "\n"))
cat(paste("Date:", Sys.Date(), "\n"))