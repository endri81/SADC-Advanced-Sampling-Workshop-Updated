#===============================================================================
# WORKSHOP FOLDER STRUCTURE AND INITIAL FILES GENERATOR
# Creates complete directory structure and placeholder files
# Dr. Endri Raço
# 
# Purpose: This script creates the entire workshop folder structure and
#          generates initial placeholder files for the 5-day workshop.
#          Run this once before the workshop begins.
#===============================================================================

# Load necessary packages
if(!require(here)) install.packages("here")
library(here)

#-------------------------------------------------------------------------------
# STEP 1: Create Main Folder Structure
#-------------------------------------------------------------------------------

cat("\n════════════════════════════════════════════════════════════════\n")
cat("Creating Workshop Folder Structure...\n")
cat("════════════════════════════════════════════════════════════════\n\n")

# Define main folders
main_folders <- c(
  "00-Setup",
  "01-Data",
  "02-Scripts",
  "03-Outputs",
  "04-Presentations",
  "05-Exercises",
  "06-Solutions",
  "07-Resources",
  "08-Harry-Journey"
)

# Create each main folder
for(folder in main_folders) {
  folder_path <- here::here(folder)
  if(!dir.exists(folder_path)) {
    dir.create(folder_path, recursive = TRUE)
    cat(paste("✓ Created:", folder, "\n"))
  } else {
    cat(paste("• Exists:", folder, "\n"))
  }
}

#-------------------------------------------------------------------------------
# STEP 2: Create Day-Specific Subfolders
#-------------------------------------------------------------------------------

cat("\n────────────────────────────────────────────────────────────────\n")
cat("Creating Day-Specific Subfolders...\n")
cat("────────────────────────────────────────────────────────────────\n\n")

# Create subfolders for Scripts (one for each day)
for(day in 1:5) {
  day_folder <- here::here("02-Scripts", paste0("Day", day))
  if(!dir.exists(day_folder)) {
    dir.create(day_folder, recursive = TRUE)
    cat(paste("✓ Created: Scripts/Day", day, "\n"))
  }
}

# Create subfolders for Exercises (one for each day)
for(day in 1:5) {
  day_folder <- here::here("05-Exercises", paste0("Day", day))
  if(!dir.exists(day_folder)) {
    dir.create(day_folder, recursive = TRUE)
    cat(paste("✓ Created: Exercises/Day", day, "\n"))
  }
}

# Create subfolders for Solutions (one for each day)
for(day in 1:5) {
  day_folder <- here::here("06-Solutions", paste0("Day", day))
  if(!dir.exists(day_folder)) {
    dir.create(day_folder, recursive = TRUE)
    cat(paste("✓ Created: Solutions/Day", day, "\n"))
  }
}

# Create subfolders for Data
data_subfolders <- c("Raw", "Processed", "Simulated", "External")
for(subfolder in data_subfolders) {
  folder_path <- here::here("01-Data", subfolder)
  if(!dir.exists(folder_path)) {
    dir.create(folder_path, recursive = TRUE)
    cat(paste("✓ Created: Data/", subfolder, "\n"))
  }
}

#-------------------------------------------------------------------------------
# STEP 3: Create Initial Data Files
#-------------------------------------------------------------------------------

cat("\n────────────────────────────────────────────────────────────────\n")
cat("Generating Initial Datasets...\n")
cat("────────────────────────────────────────────────────────────────\n\n")

# Set seed for reproducibility
set.seed(2025)

# Create a simple population frame for Day 1
population_frame <- data.frame(
  household_id = 1:10000,
  province = sample(c("Gauteng", "Western Cape", "KwaZulu-Natal", 
                      "Eastern Cape", "Limpopo", "Mpumalanga",
                      "North West", "Free State", "Northern Cape"), 
                    10000, replace = TRUE, 
                    prob = c(0.25, 0.15, 0.20, 0.10, 0.10, 0.08, 0.06, 0.04, 0.02)),
  urban_rural = sample(c("Urban", "Rural"), 10000, replace = TRUE, prob = c(0.65, 0.35)),
  household_size = rpois(10000, lambda = 3.5) + 1,  # Poisson distribution + 1 to avoid 0
  income_category = sample(c("Low", "Medium", "High"), 10000, replace = TRUE,
                           prob = c(0.45, 0.40, 0.15))
)

# Add some derived variables
population_frame$enumeration_area <- paste0("EA_", 
                                            sprintf("%04d", sample(1:500, 10000, replace = TRUE)))
population_frame$dwelling_type <- ifelse(population_frame$urban_rural == "Urban",
                                         sample(c("House", "Flat", "Townhouse", "Informal"), 10000, 
                                                replace = TRUE, prob = c(0.4, 0.3, 0.2, 0.1)),
                                         sample(c("House", "Traditional", "Informal"), 10000,
                                                replace = TRUE, prob = c(0.6, 0.25, 0.15)))

# Save the population frame
write.csv(population_frame, 
          file = here::here("01-Data", "Raw", "population_frame.csv"),
          row.names = FALSE)
cat("✓ Created: population_frame.csv (10,000 households)\n")

# Create a smaller sample dataset for initial practice
sample_data <- population_frame[sample(1:10000, 500), ]
write.csv(sample_data,
          file = here::here("01-Data", "Raw", "sample_data.csv"),
          row.names = FALSE)
cat("✓ Created: sample_data.csv (500 households)\n")

# Create Harry's first challenge dataset
harrys_pilot <- data.frame(
  respondent_id = 1:100,
  age = round(rnorm(100, mean = 38, sd = 15)),
  gender = sample(c("Male", "Female"), 100, replace = TRUE),
  education = sample(c("No schooling", "Primary", "Secondary", "Matric", "Tertiary"),
                     100, replace = TRUE, prob = c(0.05, 0.15, 0.25, 0.35, 0.20)),
  employed = sample(c("Yes", "No"), 100, replace = TRUE, prob = c(0.45, 0.55)),
  satisfaction = sample(1:10, 100, replace = TRUE)
)
harrys_pilot$age[harrys_pilot$age < 18] <- 18  # Ensure all adults
harrys_pilot$age[harrys_pilot$age > 80] <- 80  # Cap at 80

write.csv(harrys_pilot,
          file = here::here("01-Data", "Raw", "harrys_pilot_data.csv"),
          row.names = FALSE)
cat("✓ Created: harrys_pilot_data.csv (100 respondents)\n")

#-------------------------------------------------------------------------------
# STEP 4: Create README Files
#-------------------------------------------------------------------------------

cat("\n────────────────────────────────────────────────────────────────\n")
cat("Creating README Files...\n")
cat("────────────────────────────────────────────────────────────────\n\n")

# Main README
main_readme <- "# Advanced Sampling Methods for Household Surveys
## 5-Day Workshop Materials

Welcome to your workshop repository! This folder contains everything you need for our 5-day journey.

### Folder Structure:
- **00-Setup**: Installation and setup scripts
- **01-Data**: All datasets (raw, processed, simulated)
- **02-Scripts**: R scripts organized by day
- **03-Outputs**: Your generated results will go here
- **04-Presentations**: Daily slide presentations
- **05-Exercises**: Practice problems for each day
- **06-Solutions**: Solutions to exercises (released daily)
- **07-Resources**: Additional reading materials
- **08-Harry-Journey**: Harry's story and challenges

### Getting Started:
1. Run `00-Setup/setup.R` to install all packages
2. Run `00-Setup/create_workshop_structure.R` (you just did this!)
3. Open the Day 1 presentation in `04-Presentations`
4. Follow along with scripts in `02-Scripts/Day1`

### Your Instructor:
Dr. Endri Raço
Statistical Training Specialist

Remember: We're in this together! Every error is a learning opportunity.
"

writeLines(main_readme, here::here("README.md"))
cat("✓ Created: Main README.md\n")

# Data folder README
data_readme <- "# Data Folder Structure

## Subfolders:
- **Raw**: Original, unmodified datasets
- **Processed**: Cleaned and prepared datasets
- **Simulated**: Synthetic data for exercises
- **External**: Any external data sources

## Main Datasets:
- `population_frame.csv`: Complete sampling frame (10,000 households)
- `sample_data.csv`: Practice sample (500 households)
- `harrys_pilot_data.csv`: Harry's first pilot survey (100 respondents)

## Variables in Population Frame:
- household_id: Unique identifier
- province: South African province
- urban_rural: Urban/Rural classification
- household_size: Number of household members
- income_category: Low/Medium/High
- enumeration_area: EA code
- dwelling_type: Type of dwelling

All datasets are created with seed 2025 for reproducibility.
"

writeLines(data_readme, here::here("01-Data", "README.md"))
cat("✓ Created: Data README.md\n")

#-------------------------------------------------------------------------------
# STEP 5: Create Placeholder Scripts for Each Day
#-------------------------------------------------------------------------------

cat("\n────────────────────────────────────────────────────────────────\n")
cat("Creating Placeholder Scripts...\n")
cat("────────────────────────────────────────────────────────────────\n\n")

# Day 1 Scripts
day1_scripts <- c(
  "day1_01_introduction.R",
  "day1_02_data_import.R",
  "day1_03_simple_random_sampling.R",
  "day1_04_sample_size_calculation.R",
  "day1_05_harrys_first_sample.R"
)

for(script in day1_scripts) {
  script_content <- paste0(
    "#===============================================================================\n",
    "# DAY 1: ", toupper(gsub("day1_\\d+_|\\.R", "", script)), "\n",
    "# ", gsub("_", " ", gsub("day1_\\d+_|\\.R", "", script)), "\n",
    "#===============================================================================\n\n",
    "# Load packages\n",
    "library(tidyverse)\n",
    "library(here)\n\n",
    "# Script content will be added during the workshop\n",
    "# This is a placeholder file\n\n",
    "print('Ready for Day 1!')\n"
  )
  writeLines(script_content, here::here("02-Scripts", "Day1", script))
}
cat("✓ Created: Day 1 placeholder scripts\n")

# Repeat for Days 2-5 with appropriate topics
day_topics <- list(
  Day2 = c("review", "stratification_theory", "proportional_allocation", 
           "optimal_allocation", "harrys_stratified_sample"),
  Day3 = c("review", "cluster_theory", "single_stage_cluster", 
           "two_stage_cluster", "harrys_cluster_challenge"),
  Day4 = c("review", "pps_sampling", "survey_weights", 
           "nonresponse_adjustment", "harrys_complex_design"),
  Day5 = c("review", "survey_package", "design_declaration", 
           "estimation_inference", "harrys_final_analysis")
)

for(day in 2:5) {
  day_name <- paste0("Day", day)
  topics <- day_topics[[day_name]]
  
  for(i in seq_along(topics)) {
    script_name <- sprintf("day%d_%02d_%s.R", day, i, topics[i])
    script_content <- paste0(
      "#===============================================================================\n",
      "# DAY ", day, ": ", toupper(gsub("_", " ", topics[i])), "\n",
      "#===============================================================================\n\n",
      "# Load packages\n",
      "library(tidyverse)\n",
      "library(here)\n",
      "library(survey)\n\n",
      "# Script content will be added during the workshop\n",
      "# This is a placeholder file\n\n",
      "print('Ready for Day ", day, "!')\n"
    )
    writeLines(script_content, here::here("02-Scripts", day_name, script_name))
  }
  cat(paste("✓ Created: Day", day, "placeholder scripts\n"))
}

#-------------------------------------------------------------------------------
# STEP 6: Create Harry's Journey Narrative Files
#-------------------------------------------------------------------------------

cat("\n────────────────────────────────────────────────────────────────\n")
cat("Creating Harry's Journey Files...\n")
cat("────────────────────────────────────────────────────────────────\n\n")

# Day 1: Harry's Introduction
harry_day1 <- "# Harry's Journey - Day 1: The Beginning

## Meet Harry

Harry is a statistician at the National Statistical Office. He's been working 
with surveys for 5 years, but always using point-and-click software.

Today, his director called him in:

> 'Harry, we need to modernize our survey operations. I'm sending you to learn R.
> By Friday, I expect you to handle our entire household survey pipeline in R.'

Harry feels nervous but excited. He's heard R can do amazing things, but he's
never written a line of code in his life.

## Today's Challenge

The director hands Harry a USB drive:

> 'Here's data from 10,000 households. I need you to:
> 1. Take a simple random sample of 500 households
> 2. Calculate the sampling error
> 3. Document everything so others can replicate it'

Harry takes a deep breath. Let's help him succeed!

## What Harry Will Learn Today

- How to use R and RStudio
- How to import data
- How to take a simple random sample
- How to calculate sample statistics
- How to save and document his work

## Harry's Mindset

*'If thousands of statisticians worldwide can do this, so can I. One step at a time.'*
"

writeLines(harry_day1, here::here("08-Harry-Journey", "Day1_Harry.md"))
cat("✓ Created: Harry's Day 1 journey\n")

# Create brief narratives for other days
for(day in 2:5) {
  harry_content <- paste0(
    "# Harry's Journey - Day ", day, "\n\n",
    "## Today's Challenge\n\n",
    "Details will be revealed during the workshop...\n\n",
    "## Harry's Growth\n\n",
    "Harry is becoming more confident with each day!\n"
  )
  writeLines(harry_content, here::here("08-Harry-Journey", paste0("Day", day, "_Harry.md")))
}
cat("✓ Created: Harry's journey files for Days 2-5\n")

#-------------------------------------------------------------------------------
# STEP 7: Create Exercise Templates
#-------------------------------------------------------------------------------

cat("\n────────────────────────────────────────────────────────────────\n")
cat("Creating Exercise Templates...\n")
cat("────────────────────────────────────────────────────────────────\n\n")

# Day 1 Exercise Template
exercise_template <- "
#===============================================================================
# DAY 1 EXERCISES: Putting It All Together
#===============================================================================

# Exercise 1: Basic Operations
# ----------------------------
# Calculate the sample size needed for a 95% confidence level,
# 5% margin of error, and 50% proportion.
# Hint: n = (1.96^2 * 0.5 * 0.5) / 0.05^2

# Your code here:


# Exercise 2: Import and Explore
# -------------------------------
# Import the population_frame.csv and calculate:
# a) Total number of households
# b) Average household size
# c) Distribution by province

# Your code here:


# Exercise 3: Take a Sample
# --------------------------
# Take a simple random sample of 300 households from the population frame
# Compare the sample statistics to the population statistics

# Your code here:


# Exercise 4: Harry's Bonus Challenge
# ------------------------------------
# Harry's director just asked for samples from each province
# Take a sample of 50 households from each province
# Hint: Use group_by() and sample_n() from tidyverse

# Your code here:


#===============================================================================
# END OF EXERCISES
# When complete, save this file and compare with solutions
#===============================================================================
"

writeLines(exercise_template, here::here("05-Exercises", "Day1", "day1_exercises.R"))
cat("✓ Created: Day 1 exercise template\n")

#-------------------------------------------------------------------------------
# STEP 8: Create Resource Links File
#-------------------------------------------------------------------------------

cat("\n────────────────────────────────────────────────────────────────\n")
cat("Creating Resource Files...\n")
cat("────────────────────────────────────────────────────────────────\n\n")

resources_content <- "# Workshop Resources and References

## Essential R Resources

### Books (Free Online)
- [R for Data Science](https://r4ds.had.co.nz/) by Wickham & Grolemund
- [Complex Surveys: A Guide to Analysis Using R](https://r-survey.r-forge.r-project.org/svybook/) by Thomas Lumley
- [Sampling: Design and Analysis](https://www.sharonlohr.com/) by Sharon Lohr

### Online Courses
- [DataCamp: Introduction to R](https://www.datacamp.com/courses/free-introduction-to-r)
- [Coursera: R Programming](https://www.coursera.org/learn/r-programming)

### Quick References
- [RStudio Cheatsheets](https://www.rstudio.com/resources/cheatsheets/)
- [R Graph Gallery](https://www.r-graph-gallery.com/)

## Survey-Specific Resources

### R Survey Package
- [Survey Package Documentation](http://r-survey.r-forge.r-project.org/survey/)
- [Survey Package Vignettes](https://cran.r-project.org/web/packages/survey/vignettes/)

### International Examples
- [Statistics Canada R Resources](https://www.statcan.gc.ca/)
- [UK Office for National Statistics](https://www.ons.gov.uk/)
- [US Census Bureau](https://www.census.gov/)

## Getting Help

### Online Communities
- [RStudio Community](https://community.rstudio.com/)
- [Stack Overflow R Tag](https://stackoverflow.com/questions/tagged/r)
- [R-help Mailing List](https://stat.ethz.ch/mailman/listinfo/r-help)

### Local Support
- Create a WhatsApp/Teams group for workshop participants
- Schedule monthly follow-up sessions
- Buddy system: Pair up for peer support

## After the Workshop

### Week 1-2: Practice Basics
- Recreate one of your regular reports in R
- Join an online R course

### Week 3-4: Apply to Real Work
- Convert one routine task to R
- Share success with colleagues

### Month 2: Build Confidence
- Help a colleague learn R
- Create your first automated report

### Month 3: Advanced Topics
- Explore new packages
- Contribute to your NSO's R codebase

Remember: Learning R is a journey, not a destination!
"

writeLines(resources_content, here::here("07-Resources", "README.md"))
cat("✓ Created: Resources README\n")

#-------------------------------------------------------------------------------
# STEP 9: Create a Workshop Checklist
#-------------------------------------------------------------------------------

checklist_content <- "# Workshop Preparation Checklist

## Before the Workshop

### Technical Setup
- [ ] R installed (version 4.0 or higher)
- [ ] RStudio installed (latest version)
- [ ] Run setup.R successfully
- [ ] Run create_workshop_structure.R
- [ ] Test opening a script
- [ ] Test creating a plot

### Materials
- [ ] Workshop folder on desktop
- [ ] Slides accessible
- [ ] Scripts ready
- [ ] Data files loaded

### Personal Preparation
- [ ] Notebook for notes
- [ ] Water bottle
- [ ] Positive attitude!
- [ ] Ready to make mistakes and learn

## Daily Checklist

### Each Morning
- [ ] Open RStudio
- [ ] Set working directory with here::here()
- [ ] Load required packages
- [ ] Open day's presentation
- [ ] Open day's first script

### During Sessions
- [ ] Run code line by line
- [ ] Ask questions when confused
- [ ] Help neighbors if they're stuck
- [ ] Save work frequently
- [ ] Take breaks when needed

### End of Day
- [ ] Complete exercises
- [ ] Save all outputs
- [ ] Review what was learned
- [ ] Prepare questions for tomorrow
- [ ] Celebrate progress!

## After the Workshop

### Week 1
- [ ] Review all materials
- [ ] Redo exercises independently
- [ ] Apply one technique to real data

### Month 1
- [ ] Complete an online R course
- [ ] Convert one work task to R
- [ ] Share learning with colleague

### Ongoing
- [ ] Join R community
- [ ] Practice regularly
- [ ] Build portfolio of R projects
- [ ] Become the R champion in your office!
"

writeLines(checklist_content, here::here("Workshop_Checklist.md"))
cat("✓ Created: Workshop Checklist\n")

#-------------------------------------------------------------------------------
# STEP 10: Create Quick Start Guide
#-------------------------------------------------------------------------------

quickstart_content <- "# QUICK START GUIDE

## Day 1 - Getting Started (5 minutes)

1. **Open RStudio**
   - Double-click the RStudio icon

2. **Run Setup** (only once)
   ```r
   source('00-Setup/setup.R')
   ```

3. **Open Day 1 Slides**
   - Navigate to `04-Presentations/day1_slides.html`

4. **Open Day 1 Script**
   - File → Open → `02-Scripts/Day1/day1_01_introduction.R`

5. **You're Ready!**
   - Follow along with the instructor
   - Run code when prompted
   - Ask questions anytime

## Keyboard Shortcuts

- **Run current line**: Ctrl + Enter (Windows) / Cmd + Enter (Mac)
- **Run entire script**: Ctrl + Shift + Enter / Cmd + Shift + Enter
- **Clear console**: Ctrl + L / Cmd + L
- **Help for function**: F1 (with cursor on function name)

## Getting Help

If you see an error:
1. Don't panic!
2. Read the error message
3. Check for typos
4. Ask your neighbor
5. Raise your hand

## Remember

> Every R expert started exactly where you are now.
> You're not behind, you're on your journey!

---
*Dr. Endri Raço - Your guide to R mastery*
"

writeLines(quickstart_content, here::here("QUICK_START.md"))
cat("✓ Created: Quick Start Guide\n")

#-------------------------------------------------------------------------------
# FINAL SUMMARY
#-------------------------------------------------------------------------------

cat("\n")
cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║                                                                ║\n")
cat("║   🎉  WORKSHOP STRUCTURE COMPLETE!  🎉                       ║\n")
cat("║                                                                ║\n")
cat("║   Created:                                                     ║\n")
cat("║   • 8 main folders                                            ║\n")
cat("║   • 15 day-specific subfolders                                ║\n")
cat("║   • 4 data subfolders                                         ║\n")
cat("║   • 3 initial datasets                                        ║\n")
cat("║   • 25 placeholder scripts                                    ║\n")
cat("║   • 5 Harry's journey narratives                              ║\n")
cat("║   • Multiple README and guide files                           ║\n")
cat("║                                                                ║\n")
cat("║   Your workshop environment is ready!                         ║\n")
cat("║                                                                ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n")

# Display final folder structure
cat("\n Final Folder Structure:\n")
cat("────────────────────────────────────────────────────────────────\n")
fs::dir_tree(here::here(), recurse = 1)