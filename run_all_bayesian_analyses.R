#-----------------------------------------------------------------------
# MUSIC PIPELINE - MAIN EXECUTION SCRIPT
#
# This script runs the single-subject Bayesian analysis pipeline.
#
# It sources the required analysis functions and then calls the main
# wrapper function to process all data files found in the /data folder.
#-----------------------------------------------------------------------

## 1. SETUP
#-----------------------------------------------------------------------
# Load the 'here' package to help with file paths
pacman::p_load(here)

# Source the updated analysis script you saved
# Make sure the filename matches what you saved in Step 1.
source(here("bayesian_analysis_by_modality.R"))


## 2. FIND DATA FILES
#-----------------------------------------------------------------------
cat("🔎 Finding all participant data...\n")

DATA_PATH <- here("data")

# Recursively find all 'HRD_final.txt' files.
data_files <- list.files(
  path = DATA_PATH,
  pattern = "HRD_final\\.txt$",
  recursive = TRUE,
  full.names = TRUE
)

if (length(data_files) == 0) {
  stop("No 'HRD_final.txt' files found in the 'data' directory. Please check the path.")
}

cat(sprintf("✅ Found data for %d participants.\n", length(data_files)))


## 3. RUN THE BATCH ANALYSIS
#-----------------------------------------------------------------------
cat("\n🚀 Starting single-subject Bayesian analysis for all participants...\n")

# Call the main function from the sourced script.
# It will loop through all files, analyze each modality, and save results.
run_bayesian_psychophysics(data_files)


## 4. ANALYSIS COMPLETE
#-----------------------------------------------------------------------
cat("\n\n🎉 Pipeline finished!\n")

