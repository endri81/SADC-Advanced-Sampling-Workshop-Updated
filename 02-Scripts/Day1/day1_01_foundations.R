#===============================================================================
# DAY 1: HARRY'S FIRST MISSION
# Solving Anya's Request with Simple Random Sampling
# Dr. Endri Raço
# 
# Context: It's Monday morning. Harry just received an urgent email from
# Director Anya Sharma requesting an estimate of average household size
# from a frame of 10,000 households. This script documents Harry's solution.
#===============================================================================

# Load the tools Harry needs
library(tidyverse)  # For data manipulation
library(here)       # For file paths

#-------------------------------------------------------------------------------
# SECTION 1: Harry's Initial Exploration (Run lines 20-45)
# "Let me make sure I understand how this sampling thing works in R..."
#-------------------------------------------------------------------------------

# Harry's thought: "First, let me try with a tiny example - just 10 households"

# Create a mini sampling frame for practice
mini_frame <- 1:10
print("Harry's practice frame (10 households):")
print(mini_frame)

# Harry's lottery method in R
set.seed(123)  # Harry picks a random number to document his work
practice_sample <- sample(mini_frame, size = 3)

print("Harry randomly selected these 3 households:")
print(practice_sample)

# Harry verifies: "Did each household have equal probability?"
# Probability = n/N = 3/10 = 0.3
print("Probability each household had of being selected: 3/10 = 0.3")

# Harry's confidence boost:
print("✓ Success! If this works for 10, it will work for 10,000!")

# Harry's note to self:
# "The sample() function is literally doing a lottery draw.
#  It's not programming magic - it's implementing the statistical 
#  method I learned in university!"

#-------------------------------------------------------------------------------
# SECTION 2: The Real Challenge - 10,000 Households (Run lines 50-80)
# "OK, time to tackle Anya's actual request..."
#-------------------------------------------------------------------------------

print("=" * 70)
print("HARRY'S ACTUAL SAMPLING TASK")
print("=" * 70)

# The email said: 10,000 households in the frame
N <- 10000  # Population size

# Harry decided: Sample 500 households (5% sampling fraction)
n <- 500    # Sample size

# Document the sampling plan for Anya
cat("\nHARRY'S SAMPLING PLAN FOR ANYA:\n")
cat("Population size (N):", N, "households\n")
cat("Sample size (n):", n, "households\n")
cat("Sampling fraction:", n/N * 100, "%\n")
cat("Method: Simple Random Sampling without replacement\n")
cat("Random seed: 2025 (for reproducibility)\n\n")

# Take the actual sample!
set.seed(2025)  # Harry uses the year as his seed - easy to remember
selected_household_ids <- sample(1:N, size = n)

# Show Anya the first few selected households as proof
cat("First 10 selected household IDs:\n")
cat(selected_household_ids[1:10], "\n\n")

# Verification check (Harry being thorough)
cat("VERIFICATION CHECKS:\n")
cat("✓ Sample size correct?", length(selected_household_ids) == n, "\n")
cat("✓ No duplicates?", length(unique(selected_household_ids)) == n, "\n")

#-------------------------------------------------------------------------------
# SECTION 3: Loading the Actual Data (Run lines 85-110)
# "Now I need the real household data for these selected IDs..."
#-------------------------------------------------------------------------------

print("=" * 70)
print("LOADING HARRY'S FIELD DATA")
print("=" * 70)

# Load the complete sampling frame with household characteristics
# (In reality, this would come from the NSO database)
full_frame <- read.csv(here("01-Data", "Raw", "population_frame.csv"))

cat("\nSampling frame loaded successfully!\n")
cat("Variables in the frame:\n")
names(full_frame) %>% print()

# Extract Harry's sample based on selected IDs
harrys_sample <- full_frame[selected_household_ids, ]

cat("\nSample extracted! First 5 households:\n")
head(harrys_sample, 5) %>% print()

# Harry's check: "Do I have the right number of households?"
cat("\nSample size check:", nrow(harrys_sample), "households (should be", n, ")\n")

# Quick look at what Harry sampled
cat("\nDistribution of Harry's sample by province:\n")
table(harrys_sample$province) %>% print()

#-------------------------------------------------------------------------------
# SECTION 4: Calculating Anya's Estimate (Run lines 115-145)
# "Time to calculate the average household size - this is what Anya needs!"
#-------------------------------------------------------------------------------

print("=" * 70)
print("CALCULATING THE ESTIMATE FOR ANYA")
print("=" * 70)

# THE KEY CALCULATION - Average household size
# Remember the formula: ȳ = Σyᵢ/n
# In words: Add up all household sizes, divide by number of households

sample_mean <- mean(harrys_sample$household_size)

cat("\n🎯 HARRY'S ESTIMATE OF AVERAGE HOUSEHOLD SIZE:", 
    round(sample_mean, 2), "persons\n\n")

# Harry's internal monologue:
# "But Anya will ask: How reliable is this estimate?
#  I need to calculate the standard error!"

# Calculate the sample variance (how different households are from each other)
sample_variance <- var(harrys_sample$household_size)
cat("Sample variance (s²):", round(sample_variance, 3), "\n")

# The formula Harry learned: SE = sqrt(s²/n × (1 - n/N))
# Let's break it down:
variance_of_mean <- sample_variance / n  # This is s²/n
fpc <- 1 - (n/N)  # Finite population correction
standard_error <- sqrt(variance_of_mean * fpc)

cat("Standard error calculation:\n")
cat("  - s²/n =", round(variance_of_mean, 4), "\n")
cat("  - FPC (1 - n/N) =", round(fpc, 4), "\n")
cat("  - Standard Error =", round(standard_error, 4), "\n")

#-------------------------------------------------------------------------------
# SECTION 5: Building a Confidence Interval (Run lines 150-175)
# "I should give Anya a range, not just a point estimate..."
#-------------------------------------------------------------------------------

print("=" * 70)
print("CONSTRUCTING THE CONFIDENCE INTERVAL")
print("=" * 70)

# 95% Confidence Interval: estimate ± 1.96 × SE
# Harry remembers: "1.96 is the magic number for 95% confidence"

margin_of_error <- 1.96 * standard_error
lower_bound <- sample_mean - margin_of_error
upper_bound <- sample_mean + margin_of_error

cat("\n95% CONFIDENCE INTERVAL:\n")
cat("Average household size is between", 
    round(lower_bound, 2), "and", round(upper_bound, 2), "persons\n\n")

cat("Interpretation for Anya:\n")
cat("'I am 95% confident that the true average household size\n")
cat(" in the Eastern Province is between", round(lower_bound, 2), 
    "and", round(upper_bound, 2), "persons.'\n\n")

# Harry's thought: "This is solid! The interval is quite narrow,
# which means our estimate is precise. Anya will be happy!"

#-------------------------------------------------------------------------------
# SECTION 6: Checking Against Truth (Run lines 180-200)
# "In practice, we never know the true population value,
#  but let's check how well we did..."
#-------------------------------------------------------------------------------

print("=" * 70)
print("VALIDATION (FOR LEARNING ONLY)")
print("=" * 70)

# Calculate the true population parameter
# (In real life, we'd never know this - that's why we sample!)
true_population_mean <- mean(full_frame$household_size)

cat("\nTrue population mean:", round(true_population_mean, 3), "\n")
cat("Harry's estimate:", round(sample_mean, 3), "\n")
cat("Difference:", round(abs(sample_mean - true_population_mean), 3), "\n")

# Check if confidence interval contains the true value
contains_truth <- (true_population_mean >= lower_bound) & 
  (true_population_mean <= upper_bound)

cat("\nDoes our 95% CI contain the true value?", contains_truth, "\n")

if(contains_truth) {
  cat("✓ Success! Our interval captured the true parameter!\n")
} else {
  cat("⚠ We're in the unlucky 5% - but that's expected to happen sometimes!\n")
}

#-------------------------------------------------------------------------------
# SECTION 7: Preparing Harry's Report (Run lines 205-245)
# "Let me create a professional report for Anya..."
#-------------------------------------------------------------------------------

print("=" * 70)
print("GENERATING HARRY'S REPORT")
print("=" * 70)

# Create a summary table for the report
summary_stats <- data.frame(
  Statistic = c("Sample Size", "Population Size", "Sampling Fraction",
                "Sample Mean", "Standard Error", "95% CI Lower", 
                "95% CI Upper", "Margin of Error"),
  Value = c(n, N, paste0(round(n/N * 100, 1), "%"),
            round(sample_mean, 3), round(standard_error, 4),
            round(lower_bound, 3), round(upper_bound, 3),
            round(margin_of_error, 3))
)

print(summary_stats)

# Save the sample for documentation
write.csv(harrys_sample, 
          file = here("03-Outputs", "harrys_sample_monday.csv"),
          row.names = FALSE)

# Create Anya's report
report_text <- paste(
  "SAMPLING REPORT FOR DIRECTOR ANYA SHARMA",
  "==========================================",
  paste("Date:", Sys.Date()),
  paste("Analyst: Harry Statistician"),
  "",
  "EXECUTIVE SUMMARY",
  "-----------------",
  paste("Estimated average household size:", round(sample_mean, 2), "persons"),
  paste("95% Confidence Interval: [", round(lower_bound, 2), ",", 
        round(upper_bound, 2), "]"),
  "",
  "METHODOLOGY",
  "-----------",
  "Method: Simple Random Sampling without replacement",
  paste("Population size:", N, "households"),
  paste("Sample size:", n, "households"),
  paste("Random seed:", 2025, "(for reproducibility)"),
  "",
  "RECOMMENDATION",
  "--------------",
  "The estimate is highly reliable with a margin of error of only",
  paste(round(margin_of_error, 2), "persons."),
  "This precision is sufficient for resource allocation decisions.",
  "",
  "Report generated using R for complete reproducibility.",
  sep = "\n"
)

writeLines(report_text, here("03-Outputs", "harrys_report_monday.txt"))
cat("\n✓ Report saved to: 03-Outputs/harrys_report_monday.txt\n")

#-------------------------------------------------------------------------------
# SECTION 8: Harry's Email Response (Run lines 250-270)
# "Time to respond to Anya with confidence!"
#-------------------------------------------------------------------------------

cat("\n")
cat("=" * 70, "\n")
cat("HARRY'S EMAIL RESPONSE (DRAFT)\n")
cat("=" * 70, "\n\n")

email_response <- "
To: Anya Sharma
From: Harry Statistician  
Date: Monday, September 22, 2025, 4:30 PM
Subject: Re: Urgent: Pilot Data for the Household Income Survey

Hi Anya,

Great news! I've completed the analysis you requested.

RESULTS:
• Estimated average household size: 3.54 persons
• 95% Confidence Interval: [3.42, 3.66] persons
• Margin of error: ±0.12 persons

METHODOLOGY:
I used Simple Random Sampling to select 500 households from the 
frame of 10,000 (5% sampling fraction). The estimate is unbiased 
and highly precise.

The full report with documentation is attached. All analysis was 
done in R with seed 2025, so you can reproduce these exact results.

The estimate is ready for the Ministry's resource allocation decisions.
Please let me know if you need any additional analysis.

Best regards,
Harry

P.S. - R made this incredibly efficient. What would have taken days 
       was completed in hours!
"

cat(email_response)

#-------------------------------------------------------------------------------
# SECTION 9: Harry's Reflection (Run lines 275-290)
# "What did I learn today?"
#-------------------------------------------------------------------------------

cat("\n")
cat("=" * 70, "\n")
cat("HARRY'S LEARNING JOURNAL - MONDAY EVENING\n")
cat("=" * 70, "\n\n")

cat("Today I learned:\n\n")

cat("1. Simple Random Sampling isn't just theory - it's practical and powerful\n\n")

cat("2. R doesn't do magic - it implements the exact statistical methods\n")
cat("   I learned in university, just MUCH faster\n\n")

cat("3. The formulas that seemed abstract (like SE = sqrt(s²/n × (1-n/N)))\n")
cat("   actually produce real numbers that build confidence in our estimates\n\n")

cat("4. Reproducibility (via set.seed) means Anya can verify everything\n\n")

cat("5. I solved a real problem in hours that would have taken days by hand\n\n")

cat("Tomorrow: Anya mentioned something about 'stratification'...\n")
cat("I wonder what that means? 🤔\n")

#===============================================================================
# MISSION COMPLETE!
# 
# Harry successfully:
# ✅ Understood the business problem (Anya's request)
# ✅ Applied appropriate statistical methodology (SRS)
# ✅ Implemented the solution efficiently (using R)
# ✅ Calculated reliable estimates with uncertainty quantification
# ✅ Documented everything for reproducibility
# ✅ Delivered results on time with confidence
# 
# Key Insight: R is not about programming - it's about implementing
#              statistical methodology at professional scale!
# 
# Next: Day 2 - Harry discovers that not all samples are created equal...
#===============================================================================