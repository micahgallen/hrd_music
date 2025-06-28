#-----------------------------------------------------------------------
# MASTER ANALYSIS SCRIPT: Cardiac Interoception in Musicians
#
# This script uses the project-specific versions of the HRD analysis
# functions (copied from the submodule and adapted for this project)
#-----------------------------------------------------------------------

# Load required libraries
suppressPackageStartupMessages({
  library(tidyverse)
  library(patchwork)
  library(magrittr)
  library(rjags)
  library(coda)
  library(ggmcmc)
})

# Source project-specific functions (not from submodule)
cat("Loading analysis functions...\n")
source("code/hrd_analysis/helper_functions.R")
source("code/hrd_analysis/bin_confidence_quantiles.R")
source("code/hrd_analysis/trials2counts.R")
source("code/hrd_analysis/fit_metad_indiv.R")
source("code/hrd_analysis/calc_auroc2.R")
source("code/hrd_analysis/analyze_hrd_data.R")

# Find all HRD data files
cat("\n--- FINDING DATA FILES ---\n")
data_files <- list.files(
  path = "data",
  pattern = "HRD_final\\.txt$",
  recursive = TRUE,
  full.names = TRUE
)

if (length(data_files) == 0) {
  stop("No HRD_final.txt files found in the data directory.")
}

cat(sprintf("Found %d HRD data files to process.\n", length(data_files)))

# Process each participant
cat("\n--- PROCESSING PARTICIPANTS ---\n")
all_results <- list()
failed_participants <- character()

for (i in seq_along(data_files)) {
  file_path <- data_files[i]
  
  # Extract participant ID
  participant_id <- basename(dirname(file_path))
  if (participant_id %in% c(".", "data", "")) {
    participant_id <- gsub("HRD_final\\.txt$", "", basename(file_path))
  }
  
  cat(sprintf("\n[%d/%d] Processing %s...", i, length(data_files), participant_id))
  
  # Read data
  hrd_data_raw <- tryCatch({
    read_delim(file_path, delim = ",", col_types = cols(), show_col_types = FALSE)
  }, error = function(e) {
    cat(sprintf("\n  ERROR reading file: %s", e$message))
    NULL
  })
  
  if (is.null(hrd_data_raw)) {
    failed_participants <- c(failed_participants, participant_id)
    next
  }
  
  # Analyze data
  results_df <- tryCatch({
    analyze_hrd_data(
      hrd_data = hrd_data_raw,
      nRatings = 4,
      plot_results = TRUE,
      show_traceplot = TRUE,
      participant_id = participant_id
    )
  }, error = function(e) {
    cat(sprintf("\n  ERROR analyzing: %s", e$message))
    NULL
  })
  
  if (!is.null(results_df)) {
    results_df$participant_id <- participant_id
    all_results[[participant_id]] <- results_df
    cat(" ✓")
  } else {
    failed_participants <- c(failed_participants, participant_id)
    cat(" ✗")
  }
}

# Save results
cat("\n\n--- SAVING RESULTS ---\n")

if (length(all_results) > 0) {
  final_summary_table <- do.call(rbind, all_results)
  
  # Reorder columns
  col_order <- c("participant_id", "mean_accuracy", "mean_confidence", 
                 "estimated_threshold", "estimated_slope", 
                 "auroc", "d", "metad", "mratio")
  existing_cols <- intersect(col_order, names(final_summary_table))
  final_summary_table <- final_summary_table[, existing_cols]
  rownames(final_summary_table) <- NULL
  
  # Save CSV
  write.csv(final_summary_table, "results/summary_hrd_music_study.csv", row.names = FALSE)
  
  # Print summary
  cat(sprintf("Successfully processed: %d participants\n", nrow(final_summary_table)))
  if (length(failed_participants) > 0) {
    cat(sprintf("Failed participants: %s\n", paste(failed_participants, collapse = ", ")))
  }
  
  cat("\nResults saved to: results/summary_hrd_music_study.csv\n")
  cat("Individual plots saved in: results/plots/\n")
  
} else {
  cat("No participants were successfully processed.\n")
}

cat("\n--- ANALYSIS COMPLETE ---\n")
