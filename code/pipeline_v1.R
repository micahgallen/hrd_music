#-----------------------------------------------------------------------
# MASTER ANALYSIS SCRIPT: Cardiac Interoception in Musicians
#
# This script processes HRD (Heart Rate Discrimination) data for multiple
# participants, analyzing cardiac interoception using metacognitive measures.
#
# The pipeline:
#  1. Loads all necessary libraries and custom functions from the submodule
#  2. Recursively finds all 'HRD_final.txt' files in the './data' directory
#  3. Processes each participant's data using the analyze_hrd_data function
#  4. Compiles results into a summary table
#  5. Saves individual plots and the final summary CSV
#-----------------------------------------------------------------------

# 1. SETUP: LOAD LIBRARIES AND FUNCTIONS
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

# Source custom functions from the hrd_metacog submodule
# The correct path is: code/hrd_metacog/code/
cat("Loading custom functions from hrd_metacog submodule...\n")
source("code/hrd_metacog/code/helper_functions.R")
source("code/hrd_metacog/code/bin_confidence_quantiles.R")
source("code/hrd_metacog/code/trials2counts.R")
source("code/hrd_metacog/code/fit_metad_indiv.R")
source("code/hrd_metacog/code/calc_auroc2.R")
source("code/hrd_metacog/code/analyze_hrd_data.R")

# Create directories for results if they don't exist
dir.create("results", showWarnings = FALSE)
dir.create("results/plots", showWarnings = FALSE)
dir.create("results/individual_plots", showWarnings = FALSE)

# 2. FIND DATA FILES
#-----------------------------------------------------------------------
cat("\n--- FINDING DATA FILES ---\n")

# Find all HRD data files in the data directory
data_files <- list.files(
  path = "data",
  pattern = "HRD_final\\.txt$",
  recursive = TRUE,
  full.names = TRUE
)

# Check if any files were found
if (length(data_files) == 0) {
  stop("No HRD_final.txt files found in the 'data' directory. Please check your data folder.")
}

cat(sprintf("Found %d HRD data files to process.\n", length(data_files)))

# 3. PROCESS EACH PARTICIPANT
#-----------------------------------------------------------------------
cat("\n--- PROCESSING PARTICIPANTS ---\n")

# Initialize list to store results
all_results <- list()
failed_participants <- character()

# Progress counter
total_files <- length(data_files)
processed_count <- 0

# Process each data file
for (i in seq_along(data_files)) {
  file_path <- data_files[i]
  
  # Extract participant ID from file path
  # This handles various folder structures
  participant_id <- basename(dirname(file_path))
  
  # If participant_id is empty or generic, use filename
  if (participant_id %in% c(".", "data", "")) {
    participant_id <- gsub("HRD_final\\.txt$", "", basename(file_path))
  }
  
  cat(sprintf("\n[%d/%d] Processing %s...", i, total_files, participant_id))
  
  # Try to read the data file
  hrd_data_raw <- tryCatch({
    read_delim(file_path, delim = ",", col_types = cols(), show_col_types = FALSE)
  }, error = function(e) {
    cat(sprintf("\n  ERROR reading file: %s", e$message))
    NULL
  })
  
  # Skip if file couldn't be read
  if (is.null(hrd_data_raw)) {
    failed_participants <- c(failed_participants, participant_id)
    next
  }
  
  # Check if data has minimum required columns
  required_cols <- c("nTrials", "Condition", "Decision", "Confidence", 
                     "EstimatedThreshold", "EstimatedSlope", "Alpha", 
                     "ResponseCorrect")
  missing_cols <- setdiff(required_cols, names(hrd_data_raw))
  
  if (length(missing_cols) > 0) {
    cat(sprintf("\n  WARNING: Missing columns: %s", paste(missing_cols, collapse = ", ")))
  }
  
  # Try to analyze the data
  results_df <- tryCatch({
    # Note: The analyze_hrd_data function expects the JAGS model file to be at
    # 'code/hrd_metacog/code/Bayes_metad_indiv_R.txt'
    # We'll modify the path in the function or copy the file
    
    # Create a temporary directory for individual plots
    participant_plot_dir <- file.path("results/individual_plots", participant_id)
    dir.create(participant_plot_dir, showWarnings = FALSE, recursive = TRUE)
    
    # Run the analysis
    analyze_hrd_data(
      hrd_data = hrd_data_raw,
      nRatings = 4,
      plot_results = TRUE,
      show_traceplot = TRUE,
      participant_id = participant_id
    )
    
  }, error = function(e) {
    cat(sprintf("\n  ERROR analyzing data: %s", e$message))
    NULL
  })
  
  # If analysis was successful, store results
  if (!is.null(results_df)) {
    results_df$participant_id <- participant_id
    all_results[[participant_id]] <- results_df
    processed_count <- processed_count + 1
    cat(" ✓ Success")
  } else {
    failed_participants <- c(failed_participants, participant_id)
    cat(" ✗ Failed")
  }
}

# 4. COMPILE AND SAVE RESULTS
#-----------------------------------------------------------------------
cat("\n\n--- ANALYSIS SUMMARY ---\n")
cat(sprintf("Successfully processed: %d/%d participants\n", processed_count, total_files))

if (length(failed_participants) > 0) {
  cat(sprintf("Failed participants: %s\n", paste(failed_participants, collapse = ", ")))
}

# Combine results if any were successful
if (length(all_results) > 0) {
  # Combine all individual results into one data frame
  final_summary_table <- do.call(rbind, all_results)
  
  # Reorder columns for clarity
  col_order <- c("participant_id", "mean_accuracy", "mean_confidence", 
                 "estimated_threshold", "estimated_slope", 
                 "auroc", "d", "metad", "mratio")
  
  # Reorder only columns that exist
  existing_cols <- intersect(col_order, names(final_summary_table))
  final_summary_table <- final_summary_table[, existing_cols]
  
  # Reset row names
  rownames(final_summary_table) <- NULL
  
  # Save results to CSV
  output_file <- file.path("results", "summary_hrd_music_study.csv")
  write.csv(final_summary_table, output_file, row.names = FALSE)
  
  # Print summary statistics
  cat("\n--- RESULTS SUMMARY ---\n")
  cat(sprintf("Number of participants: %d\n", nrow(final_summary_table)))
  
  # Calculate and display group means
  numeric_cols <- c("mean_accuracy", "mean_confidence", "auroc", "d", "metad", "mratio")
  existing_numeric <- intersect(numeric_cols, names(final_summary_table))
  
  cat("\nGroup means:\n")
  for (col in existing_numeric) {
    col_mean <- mean(final_summary_table[[col]], na.rm = TRUE)
    col_sd <- sd(final_summary_table[[col]], na.rm = TRUE)
    cat(sprintf("  %s: %.3f (SD = %.3f)\n", col, col_mean, col_sd))
  }
  
  cat(sprintf("\nResults saved to: %s\n", output_file))
  cat("Individual plots saved in: results/plots/\n")
  
  # Display the first few rows of results
  cat("\nFirst 5 participants:\n")
  print(head(final_summary_table, 5))
  
} else {
  cat("\n*** No participants were successfully processed. ***\n")
  cat("Please check:\n")
  cat("1. Data file format and column names\n")
  cat("2. JAGS model file location\n")
  cat("3. R package installations\n")
}

# 5. OPTIONAL: CREATE GROUP-LEVEL PLOTS
#-----------------------------------------------------------------------
if (length(all_results) > 0 && nrow(final_summary_table) > 1) {
  cat("\nCreating group-level visualizations...\n")
  
  # Create a long-format data frame for plotting
  plot_data <- final_summary_table %>%
    pivot_longer(
      cols = all_of(existing_numeric),
      names_to = "measure",
      values_to = "value"
    ) %>%
    filter(!is.na(value) & !is.infinite(value))
  
  # Create violin plots for each measure
  group_plot <- ggplot(plot_data, aes(x = measure, y = value)) +
    geom_violin(fill = "lightblue", alpha = 0.7) +
    geom_boxplot(width = 0.2, fill = "white", alpha = 0.8) +
    geom_jitter(width = 0.1, alpha = 0.5, size = 2) +
    facet_wrap(~measure, scales = "free", ncol = 3) +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      strip.text = element_text(size = 12, face = "bold")
    ) +
    labs(
      title = "HRD Task Performance Across Participants",
      y = "Value"
    )
  
  # Save group plot
  ggsave(
    filename = file.path("results", "group_summary_plot.png"),
    plot = group_plot,
    width = 12,
    height = 8,
    dpi = 300
  )
  
  cat("Group visualization saved to: results/group_summary_plot.png\n")
}

cat("\n--- PIPELINE COMPLETE ---\n")