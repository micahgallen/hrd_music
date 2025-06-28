#-----------------------------------------------------------------------
# MUSIC PIPELINE - BATCH SCRIPT FOR SINGLE-SUBJECT BAYESIAN ANALYSIS
#
# This script runs the single-subject Bayesian psychometric analysis for
# all participants with data located in the `/data` directory.
#
# It uses the functions defined in your project's
# `bayesian_psychometric_analysis.R` script.
#
# For each participant, it will:
#  - Fit a Bayesian psychometric function using the
#    `Standard_Cumulative_Normal.stan` model.
#  - Generate and save diagnostic and posterior predictive plots.
#  - Save results to a dedicated folder: `results/bayesian_psychophysics/`
#
# At the end, it will create a single CSV file with the combined
# Bayesian parameter estimates from all participants.
#-----------------------------------------------------------------------

## 1. SETUP: Load Libraries
#-----------------------------------------------------------------------
# This script requires the 'tidyverse' and 'cmdstanr' packages.
# 'cmdstanr' is used to run the Stan models.
# 'bayesplot' and 'posterior' (loaded by the sourced script) are used for plotting.
pacman::p_load(tidyverse, cmdstanr)


## 2. SOURCE THE ANALYSIS FUNCTIONS
#-----------------------------------------------------------------------
# This line loads the functions from your project's existing Bayesian
# analysis script, making `run_bayesian_psychophysics()` available.
source("bayesian_psychometric_analysis.R")


## 3. FIND DATA FILES
#-----------------------------------------------------------------------
cat("🔎 Finding all participant data...\n")

# Recursively find all 'HRD_final.txt' files within the 'data' directory.
data_files <- list.files(
  path = "data",
  pattern = "HRD_final\\.txt$",
  recursive = TRUE,
  full.names = TRUE
)

if (length(data_files) == 0) {
  stop("No 'HRD_final.txt' files found in the 'data' directory. Please check the path.")
}

cat(sprintf("✅ Found data for %d participants.\n", length(data_files)))


## 4. RUN THE BATCH ANALYSIS
#-----------------------------------------------------------------------
cat("\n🚀 Starting single-subject Bayesian analysis for all participants...\n")
cat("   This may take several minutes to compile the model and fit each participant.\n\n")

# This is the main function call. It will loop through all the data files,
# run the Bayesian analysis on each one, and save all results.
run_bayesian_psychophysics(data_files)


## 5. ANALYSIS COMPLETE
#-----------------------------------------------------------------------
cat("\n\n🎉 All analyses complete!\n")
cat("A summary table 'bayesian_summary.csv' has been saved in: results/bayesian_psychophysics/\n")
cat("Individual diagnostic and results plots for each subject have also been saved in that directory.\n")

