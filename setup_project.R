#-----------------------------------------------------------------------
# SETUP SCRIPT: Initialize HRD Music Study Environment
#
# This script:
# 1. Checks for required R packages
# 2. Verifies the git submodule is initialized
# 3. Copies necessary files from submodule to project directory
# 4. Updates paths in copied files for project-specific use
# 5. Creates necessary directories
#-----------------------------------------------------------------------

cat("=== HRD Music Study Setup ===\n\n")

# 1. CHECK AND INSTALL REQUIRED PACKAGES
#-----------------------------------------------------------------------
cat("Checking R packages...\n")

required_packages <- c(
  "tidyverse", "patchwork", "magrittr", "rjags", 
  "coda", "ggmcmc", "readr", "dplyr", "ggplot2"
)

# Check which packages are not installed
missing_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]

if (length(missing_packages) > 0) {
  cat(sprintf("Missing packages: %s\n", paste(missing_packages, collapse = ", ")))
  
  # Ask user if they want to install
  response <- readline("Do you want to install missing packages? (y/n): ")
  
  if (tolower(response) == "y") {
    install.packages(missing_packages)
    cat("Packages installed successfully.\n")
  } else {
    cat("Please install the required packages manually before running the analysis.\n")
  }
} else {
  cat("✓ All required packages are installed.\n")
}

# 2. CHECK GIT SUBMODULE
#-----------------------------------------------------------------------
cat("\nChecking git submodule...\n")

# Check if the submodule directory exists
submodule_path <- "code/hrd_metacog"
if (!dir.exists(submodule_path)) {
  cat("✗ Submodule directory not found.\n")
  cat("Please run the following commands in your terminal:\n")
  cat("  git submodule init\n")
  cat("  git submodule update\n")
  stop("Cannot proceed without submodule. Please initialize it first.")
}

# Check if key files exist
key_files <- c(
  "code/hrd_metacog/code/analyze_hrd_data.R",
  "code/hrd_metacog/code/fit_metad_indiv.R",
  "code/hrd_metacog/code/helper_functions.R",
  "code/hrd_metacog/code/bin_confidence_quantiles.R",
  "code/hrd_metacog/code/trials2counts.R",
  "code/hrd_metacog/code/calc_auroc2.R",
  "code/hrd_metacog/code/Bayes_metad_indiv_R.txt"
)

missing_files <- key_files[!file.exists(key_files)]

if (length(missing_files) > 0) {
  cat("✗ Some submodule files are missing:\n")
  cat(paste("  -", missing_files, collapse = "\n"), "\n")
  cat("Please run: git submodule update --init --recursive\n")
  stop("Cannot proceed with missing submodule files.")
} else {
  cat("✓ Submodule is properly initialized.\n")
}

# 3. CREATE PROJECT-SPECIFIC CODE DIRECTORY
#-----------------------------------------------------------------------
cat("\nSetting up project-specific code directory...\n")

# Create a local code directory for project-specific versions
local_code_dir <- "code/hrd_analysis"
dir.create(local_code_dir, showWarnings = FALSE, recursive = TRUE)

# 4. COPY AND MODIFY FILES
#-----------------------------------------------------------------------
cat("Copying and adapting submodule files...\n")

# Copy the JAGS model file (no modifications needed)
file.copy(
  from = "code/hrd_metacog/code/Bayes_metad_indiv_R.txt",
  to = file.path(local_code_dir, "Bayes_metad_indiv_R.txt"),
  overwrite = TRUE
)
cat("  ✓ Copied JAGS model file\n")

# Copy and modify fit_metad_indiv.R
fit_metad_content <- readLines("code/hrd_metacog/code/fit_metad_indiv.R")

# Find and replace the model path - handle different possible formats
fit_metad_content <- gsub(
  "jags.model\\(file = 'code/Bayes_metad_indiv_R.txt'",
  "jags.model(file = 'code/hrd_analysis/Bayes_metad_indiv_R.txt'",
  fit_metad_content,
  fixed = TRUE
)

# Also handle if it's written without quotes or with double quotes
fit_metad_content <- gsub(
  'jags.model\\(file = "code/Bayes_metad_indiv_R.txt"',
  'jags.model(file = "code/hrd_analysis/Bayes_metad_indiv_R.txt"',
  fit_metad_content,
  fixed = TRUE
)

# Write the modified version
writeLines(fit_metad_content, file.path(local_code_dir, "fit_metad_indiv.R"))
cat("  ✓ Created project-specific fit_metad_indiv.R\n")

# Copy and modify analyze_hrd_data.R
analyze_content <- readLines("code/hrd_metacog/code/analyze_hrd_data.R")

# Update the source statements to use local versions
analyze_content <- gsub(
  'source\\("code/helper_functions.R"\\)',
  'source("code/hrd_analysis/helper_functions.R")',
  analyze_content
)
analyze_content <- gsub(
  'source\\("code/trials2counts.R"\\)',
  'source("code/hrd_analysis/trials2counts.R")',
  analyze_content
)
analyze_content <- gsub(
  'source\\("code/fit_metad_indiv.R"\\)',
  'source("code/hrd_analysis/fit_metad_indiv.R")',
  analyze_content
)
analyze_content <- gsub(
  'source\\("code/calc_auroc2.R"\\)',
  'source("code/hrd_analysis/calc_auroc2.R")',
  analyze_content
)

# Update the figure directory to be project-specific
analyze_content <- gsub(
  'if \\(!dir.exists\\("figs"\\)\\) \\{',
  'if (!dir.exists("results/plots")) {',
  analyze_content
)
analyze_content <- gsub(
  'dir.create\\("figs"\\)',
  'dir.create("results/plots", recursive = TRUE)',
  analyze_content
)
analyze_content <- gsub(
  'ggsave\\(filename = sprintf\\("figs/%s',
  'ggsave(filename = sprintf("results/plots/%s',
  analyze_content
)
analyze_content <- gsub(
  'png\\(file = sprintf\\("figs/%s',
  'png(file = sprintf("results/plots/%s',
  analyze_content
)

writeLines(analyze_content, file.path(local_code_dir, "analyze_hrd_data.R"))
cat("  ✓ Created project-specific analyze_hrd_data.R\n")

# Copy other files without modification
files_to_copy <- c(
  "helper_functions.R",
  "bin_confidence_quantiles.R", 
  "trials2counts.R",
  "calc_auroc2.R"
)

for (file in files_to_copy) {
  file.copy(
    from = file.path("code/hrd_metacog/code", file),
    to = file.path(local_code_dir, file),
    overwrite = TRUE
  )
}
cat("  ✓ Copied helper functions\n")

# 5. CREATE DIRECTORY STRUCTURE
#-----------------------------------------------------------------------
cat("\nCreating project directories...\n")

dirs_to_create <- c(
  "data",
  "results",
  "results/plots",
  "results/individual_plots"
)

for (dir in dirs_to_create) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
}
cat("  ✓ Created directory structure\n")

# 6. CREATE THE UPDATED MAIN ANALYSIS SCRIPT
#-----------------------------------------------------------------------
cat("\nCreating main analysis script...\n")

main_script <- '
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
cat("Loading analysis functions...\\n")
source("code/hrd_analysis/helper_functions.R")
source("code/hrd_analysis/bin_confidence_quantiles.R")
source("code/hrd_analysis/trials2counts.R")
source("code/hrd_analysis/fit_metad_indiv.R")
source("code/hrd_analysis/calc_auroc2.R")
source("code/hrd_analysis/analyze_hrd_data.R")

# Find all HRD data files
cat("\\n--- FINDING DATA FILES ---\\n")
data_files <- list.files(
  path = "data",
  pattern = "HRD_final\\\\.txt$",
  recursive = TRUE,
  full.names = TRUE
)

if (length(data_files) == 0) {
  stop("No HRD_final.txt files found in the data directory.")
}

cat(sprintf("Found %d HRD data files to process.\\n", length(data_files)))

# Process each participant
cat("\\n--- PROCESSING PARTICIPANTS ---\\n")
all_results <- list()
failed_participants <- character()

for (i in seq_along(data_files)) {
  file_path <- data_files[i]
  
  # Extract participant ID
  participant_id <- basename(dirname(file_path))
  if (participant_id %in% c(".", "data", "")) {
    participant_id <- gsub("HRD_final\\\\.txt$", "", basename(file_path))
  }
  
  cat(sprintf("\\n[%d/%d] Processing %s...", i, length(data_files), participant_id))
  
  # Read data
  hrd_data_raw <- tryCatch({
    read_delim(file_path, delim = ",", col_types = cols(), show_col_types = FALSE)
  }, error = function(e) {
    cat(sprintf("\\n  ERROR reading file: %s", e$message))
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
    cat(sprintf("\\n  ERROR analyzing: %s", e$message))
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
cat("\\n\\n--- SAVING RESULTS ---\\n")

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
  cat(sprintf("Successfully processed: %d participants\\n", nrow(final_summary_table)))
  if (length(failed_participants) > 0) {
    cat(sprintf("Failed participants: %s\\n", paste(failed_participants, collapse = ", ")))
  }
  
  cat("\\nResults saved to: results/summary_hrd_music_study.csv\\n")
  cat("Individual plots saved in: results/plots/\\n")
  
} else {
  cat("No participants were successfully processed.\\n")
}

cat("\\n--- ANALYSIS COMPLETE ---\\n")
'

writeLines(trimws(main_script), "music_study_analysis.R")
cat("  ✓ Created music_study_analysis.R\n")

# 7. SUMMARY
#-----------------------------------------------------------------------
cat("\n=== SETUP COMPLETE ===\n")
cat("\nYour project is now ready! The following has been set up:\n")
cat("  • Project-specific analysis code in: code/hrd_analysis/\n")
cat("  • Main analysis script: music_study_analysis.R\n")
cat("  • Results will be saved to: results/\n")
cat("\nTo run your analysis:\n")
cat("  source('music_study_analysis.R')\n")
cat("\nNote: The original submodule files remain unchanged.\n")