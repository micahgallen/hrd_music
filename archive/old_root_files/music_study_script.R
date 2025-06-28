#-----------------------------------------------------------------------
# MASTER ANALYSIS SCRIPT: Cardiac Interoception in Musicians
#
# This script has been refactored to be more modular. It now:
#  1. Loads all necessary libraries and custom functions.
#  2. Sources a primary analysis function, `analyze_hrd_data`, which
#     contains the core logic for a single participant's analysis.
#  3. Recursively finds all 'HRD_final.txt' files in the './data' directory.
#  4. Loops through each participant, calling `analyze_hrd_data` for each.
#  5. Collates the results from all participants into a single summary dataframe.
#  6. Saves the final summary table to a CSV file.
#-----------------------------------------------------------------------

# 1. SETUP: LOAD LIBRARIES AND FUNCTIONS
#-----------------------------------------------------------------------
# Load standard libraries
library(tidyverse) # Includes readr, dplyr, ggplot2, and more
library(patchwork) # For combining plots
library(magrittr)  # For the %<>% pipe
library(rjags)     # For running Bayesian models
library(coda)      # For analyzing MCMC output
library(ggmcmc)    # For plotting MCMC output

# --- IMPORTANT ---
# Source the custom functions from the hrd_metacog submodule.
# This assumes the script 'analyze_hrd_data.R' exists and contains
# the main analysis logic you provided.
source("code/hrd_metacog/code/helper_functions.R")
source("code/hrd_metacog/code/trials2counts.R")
source("code/hrd_metacog/code/fit_metad_indiv.R")
source("code/hrd_metacog/code/calc_auroc2.R")
source("code/hrd_metacog/code/analyze_hrd_data.R") # Sourcing the main analysis function

# Create directories to save results and plots if they don't exist
# The analyze_hrd_data function will use this path.
dir.create("results", showWarnings = FALSE)
dir.create("results/plots", showWarnings = FALSE)


# 2. DATA PROCESSING: FIND FILES AND LOOP THROUGH THEM
#-----------------------------------------------------------------------

# Find all subject data files recursively within the 'data' directory
data_files <- list.files(path = "data",      # The directory to search in
                         pattern = "HRD_final\\.txt$", # The file pattern to match
                         recursive = TRUE,  # Look inside subdirectories
                         full.names = TRUE) # Return the full path to the file

# Initialize an empty list to store results from each participant
all_results <- list()

# Loop through each data file found
for (file_path in data_files) {
  
  # Extract a clean participant ID from the file path for naming things
  participant_id <- basename(dirname(file_path))
  cat(paste("--- Processing Participant:", participant_id, "---\n"))
  
  # Read the data for the current participant
  hrd_data_raw <- read_delim(file_path, delim = ",", col_types = cols())
  
  # Use a tryCatch block to gracefully handle errors for any single participant
  results_df <- tryCatch({
    # Call the main analysis function. It performs the full analysis,
    # saves plots, and returns a dataframe with the results.
    analyze_hrd_data(hrd_data = hrd_data_raw,
                     nRatings = 4,
                     plot_results = TRUE, # Set to FALSE to speed up, TRUE to save plots
                     show_traceplot = TRUE,
                     participant_id = participant_id)
    
  }, error = function(e) {
    # If an error occurs, print a message and return NULL
    cat(paste("  ERROR analyzing", participant_id, ":", e$message, "\n\n"))
    return(NULL)
  })
  
  # If analysis was successful, add the participant ID and store the results
  if (!is.null(results_df)) {
    results_df$id <- participant_id
    all_results[[participant_id]] <- results_df
  }
}

# 3. FINALIZE: COMBINE AND SAVE RESULTS
#-----------------------------------------------------------------------

# Check if any results were successfully processed
if (length(all_results) > 0) {
  # Combine the list of individual data frames into one master data frame
  final_summary_table <- do.call(rbind, all_results)
  # Reorder columns to have 'id' first
  final_summary_table <- final_summary_table %>% select(id, everything())
  rownames(final_summary_table) <- NULL # Clean up row names
  
  # Print the final summary table to the console
  print("--- Analysis Complete: Final Summary Table ---")
  print(final_summary_table)
  
  # Save the final summary table to a CSV file
  write.csv(final_summary_table, file.path("results", "summary_hrd_music_study.csv"), row.names = FALSE)
  
  cat("\nSummary results saved to: results/summary_hrd_music_study.csv\n")
  cat("Individual plots saved in: results/plots/\n")
} else {
  cat("\n--- Analysis finished, but no participants were successfully processed. ---\n")
}
