#===============================================================================
# DAY 1: INTRODUCTION TO R
# Your First R Script - Learning the Basics
# 
# This script introduces fundamental R concepts through hands-on practice.
# Run each section when instructed during the presentation.
# 
# Remember: Every expert was once a beginner. You're doing great!
#===============================================================================

#-------------------------------------------------------------------------------
# SECTION 1: R as Your Calculator (Run lines 15-30)
#-------------------------------------------------------------------------------

# R can do basic math. Try these calculations:

# Addition
5 + 3

# Subtraction  
100 - 27

# Multiplication
12 * 8

# Division
144 / 12

# R follows mathematical order of operations (PEMDAS)
2 + 3 * 4  # Multiplication happens first, then addition

# Using parentheses to control order
(2 + 3) * 4  # Now addition happens first

#-------------------------------------------------------------------------------
# SECTION 2: Creating Variables - Storing Values (Run lines 35-55)
#-------------------------------------------------------------------------------

# In survey work, we often need to remember important numbers.
# We store values in variables using the arrow: <-

# Store the total population
total_population <- 58000000  # South Africa's approximate population

# Store the sample size we want
target_sample <- 5000

# Store the response rate we expect
expected_response_rate <- 0.75  # This means 75%

# Now R remembers these values. Type each variable name to see:
total_population
target_sample
expected_response_rate

# We can use these variables in calculations
actual_responses <- target_sample * expected_response_rate
actual_responses

# What percentage of the population is our sample?
sample_percentage <- (target_sample / total_population) * 100
sample_percentage

#-------------------------------------------------------------------------------
# SECTION 3: Working with Collections of Data (Run lines 60-90)
#-------------------------------------------------------------------------------

# In surveys, we rarely work with single numbers.
# We work with collections of data.
# The c() function combines values into a collection (vector).

# Ages of five respondents
respondent_ages <- c(23, 45, 67, 34, 52)
respondent_ages

# Household sizes
household_sizes <- c(1, 4, 3, 5, 2, 6, 3, 4)
household_sizes

# Province names (text data needs quotes)
provinces <- c("Gauteng", "Western Cape", "KwaZulu-Natal", 
               "Eastern Cape", "Limpopo")
provinces

# You can even combine variables
sample_sizes <- c(target_sample, 3000, 4500, 2000)
sample_sizes

# R can do math on entire collections at once!
# Add 5 years to all ages
adjusted_ages <- respondent_ages + 5
adjusted_ages

# Double all household sizes (hypothetical scenario)
doubled_households <- household_sizes * 2
doubled_households

#-------------------------------------------------------------------------------
# SECTION 4: Your First Functions (Run lines 95-125)
#-------------------------------------------------------------------------------

# Functions are R's tools for doing specific tasks.
# They take input (arguments) and give output (results).

# The mean() function calculates averages
mean(respondent_ages)

# The median() function finds the middle value
median(respondent_ages)

# The sum() function adds everything up
sum(household_sizes)

# The length() function counts how many items
length(provinces)

# The min() and max() functions find extremes
min(respondent_ages)  # Youngest respondent
max(respondent_ages)  # Oldest respondent

# The range() function shows both min and max
range(respondent_ages)

# The sd() function calculates standard deviation
sd(household_sizes)

# The summary() function gives us everything at once!
summary(respondent_ages)

#-------------------------------------------------------------------------------
# SECTION 5: Creating Simple Data Frames (Run lines 130-160)
#-------------------------------------------------------------------------------

# In real surveys, our data has multiple variables.
# A data frame is like an Excel spreadsheet in R.

# Let's create a small survey dataset
survey_data <- data.frame(
  respondent_id = c(1, 2, 3, 4, 5),
  age = c(23, 45, 67, 34, 52),
  province = c("Gauteng", "Western Cape", "KwaZulu-Natal", 
               "Eastern Cape", "Limpopo"),
  household_size = c(2, 4, 1, 5, 3),
  employed = c(TRUE, TRUE, FALSE, TRUE, FALSE)
)

# View our data frame
survey_data

# See the structure of our data
str(survey_data)

# Get a summary of all variables
summary(survey_data)

# Access specific columns using $
survey_data$age
survey_data$province

# Calculate statistics on specific columns
mean(survey_data$age)
mean(survey_data$household_size)

#-------------------------------------------------------------------------------
# SECTION 6: Logical Operations - Asking Questions (Run lines 165-190)
#-------------------------------------------------------------------------------

# We often need to identify specific cases in our data.
# R uses logical operations to answer yes/no questions.

# Which respondents are over 40?
respondent_ages > 40

# Which households have exactly 3 members?
household_sizes == 3  # Note: double equals for checking equality

# Which respondents are between 30 and 60?
respondent_ages >= 30 & respondent_ages <= 60

# Count how many respondents are over 40
sum(respondent_ages > 40)  # TRUE counts as 1, FALSE as 0

# Find the positions of households with more than 4 members
which(household_sizes > 4)

# Select only the ages of respondents over 40
respondent_ages[respondent_ages > 40]

# What proportion of respondents are over 40?
mean(respondent_ages > 40)  # Clever trick: mean of TRUE/FALSE gives proportion

#-------------------------------------------------------------------------------
# SECTION 7: Basic Sampling Concept (Run lines 195-220)
#-------------------------------------------------------------------------------

# Let's simulate a simple random sample!
# Imagine we have a population of 100 households.

# Create household IDs from 1 to 100
population <- 1:100  # The colon creates a sequence
population

# Take a random sample of 10 households
set.seed(123)  # This ensures we all get the same "random" sample
sample_ids <- sample(population, size = 10)
sample_ids

# What if we want a 20% sample?
sample_size_needed <- length(population) * 0.20
sample_20percent <- sample(population, size = sample_size_needed)
sample_20percent

# The sample() function is the foundation of all sampling in R!
# Options we'll learn later:
# - replace = TRUE (sampling with replacement)
# - prob = weights (unequal probability sampling)

#-------------------------------------------------------------------------------
# SECTION 8: Your First Plot (Run lines 225-250)
#-------------------------------------------------------------------------------

# Visualization helps us understand our data.
# R has powerful plotting capabilities.

# A simple histogram of ages
hist(respondent_ages,
     main = "Distribution of Respondent Ages",
     xlab = "Age",
     ylab = "Frequency",
     col = "lightblue",
     border = "darkblue")

# A bar plot of household sizes
barplot(table(household_sizes),
        main = "Household Size Distribution",
        xlab = "Household Size",
        ylab = "Count",
        col = "lightgreen")

# A basic scatter plot
plot(survey_data$age, survey_data$household_size,
     main = "Age vs Household Size",
     xlab = "Age",
     ylab = "Household Size",
     pch = 19,  # Point character (19 = solid circle)
     col = "darkred")

#-------------------------------------------------------------------------------
# SECTION 9: Saving Your Work (Run lines 255-270)
#-------------------------------------------------------------------------------

# It's important to save outputs for reports and documentation.

# Save a plot to a file
pdf(here::here("03-Outputs", "my_first_plot.pdf"))
hist(respondent_ages,
     main = "My First R Plot!",
     col = "skyblue")
dev.off()  # This closes the file

# Save data to a CSV file
write.csv(survey_data, 
          file = here::here("03-Outputs", "my_first_data.csv"),
          row.names = FALSE)

# Success! You've created your first outputs!
print("Congratulations! You've completed your first R script!")

#-------------------------------------------------------------------------------
# SECTION 10: Challenge Yourself! (Optional)
#-------------------------------------------------------------------------------

# Try these exercises if you finish early:

# Challenge 1: Create a variable with your country's population
# your_country_pop <- ???

# Challenge 2: Create a vector of 5 random numbers between 1 and 100
# Hint: use sample()
# random_numbers <- sample(???, ???)

# Challenge 3: Calculate the mean and median of these numbers
# mean(???)
# median(???)

# Challenge 4: Which numbers are greater than 50?
# ??? > 50

# Challenge 5: Create a simple data frame with 3 variables
# my_data <- data.frame(
#   name = c(???, ???, ???),
#   age = c(???, ???, ???),
#   city = c(???, ???, ???)
# )

#===============================================================================
# END OF SCRIPT
# 
# Fantastic work! You've just learned:
# ✅ Basic R calculations
# ✅ Creating and using variables
# ✅ Working with vectors (collections)
# ✅ Using functions
# ✅ Creating data frames
# ✅ Logical operations
# ✅ Basic sampling
# ✅ Simple plotting
# ✅ Saving outputs
# 
# Next: We'll help Harry with his first real sampling challenge!
#===============================================================================