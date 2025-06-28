#-----------------------------------------------------------------------
# MASTER ANALYSIS SCRIPT: Cardiac Interoception in Musicians
#
# This script uses the project-specific versions of the HRD analysis
# functions (copied from the submodule and adapted for this project)
#
# Prerequisites:
#   1. Run setup_hrd_analysis.R first to initialize the environment
#   2. Place HRD_final.txt files in the data/ directory
#
# Output:
#   - Individual participant plots in results/plots/
#   - Summary CSV file: results/summary_hrd_music_study.csv
#   - Group visualization: results/group_summary_plot.png
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
cat("=== HRD MUSIC STUDY ANALYSIS ===\n")
cat("\nLoading analysis functions...\n")
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

# Display file list
cat("\nFiles to process:\n")
for (i in seq_len(min(10, length(data_files)))) {
  cat(sprintf("  %d. %s\n", i, data_files[i]))
}
if (length(data_files) > 10) {
  cat(sprintf("  ... and %d more files\n", length(data_files) - 10))
}

# Process each participant
cat("\n--- PROCESSING PARTICIPANTS ---\n")
cat("This may take several minutes per participant...\n")

all_results <- list()
failed_participants <- character()
start_time <- Sys.time()

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
  
  # Check data integrity
  required_cols <- c("Condition", "Decision", "Confidence")
  missing_cols <- setdiff(required_cols, names(hrd_data_raw))
  if (length(missing_cols) > 0) {
    cat(sprintf("\n  WARNING: Missing required columns: %s", 
                paste(missing_cols, collapse = ", ")))
  }
  
  # Analyze data
  results_df <- tryCatch({
    analyze_hrd_data(
      hrd_data = hrd_data_raw,
      nRatings = 4,
      plot_results = TRUE,
      show_traceplot = FALSE,  # Set to TRUE if you want MCMC traceplots
      participant_id = participant_id
    )
  }, error = function(e) {
    cat(sprintf("\n  ERROR analyzing: %s", e$message))
    NULL
  })
  
  if (!is.null(results_df)) {
    results_df$participant_id <- participant_id
    all_results[[participant_id]] <- results_df
    cat(" ✓ Success")
  } else {
    failed_participants <- c(failed_participants, participant_id)
    cat(" ✗ Failed")
  }
}

# Calculate processing time
end_time <- Sys.time()
processing_time <- difftime(end_time, start_time, units = "mins")

# Save results
cat("\n\n--- SAVING RESULTS ---\n")

if (length(all_results) > 0) {
  # Combine all results
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
  
  # Print summary statistics
  cat(sprintf("\nSuccessfully processed: %d/%d participants\n", 
              nrow(final_summary_table), length(data_files)))
  
  if (length(failed_participants) > 0) {
    cat(sprintf("Failed participants: %s\n", paste(failed_participants, collapse = ", ")))
  }
  
  cat(sprintf("Total processing time: %.1f minutes\n", processing_time))
  
  # Display summary statistics
  cat("\n--- GROUP SUMMARY STATISTICS ---\n")
  numeric_cols <- c("mean_accuracy", "mean_confidence", "auroc", "d", "metad", "mratio")
  existing_numeric <- intersect(numeric_cols, names(final_summary_table))
  
  summary_stats <- data.frame(
    Measure = existing_numeric,
    Mean = numeric(length(existing_numeric)),
    SD = numeric(length(existing_numeric)),
    Min = numeric(length(existing_numeric)),
    Max = numeric(length(existing_numeric)),
    N = numeric(length(existing_numeric))
  )
  
  for (i in seq_along(existing_numeric)) {
    col <- existing_numeric[i]
    values <- final_summary_table[[col]]
    values <- values[!is.na(values) & !is.infinite(values)]
    
    summary_stats$Mean[i] <- mean(values, na.rm = TRUE)
    summary_stats$SD[i] <- sd(values, na.rm = TRUE)
    summary_stats$Min[i] <- min(values, na.rm = TRUE)
    summary_stats$Max[i] <- max(values, na.rm = TRUE)
    summary_stats$N[i] <- length(values)
  }
  
  print(summary_stats, digits = 3)
  
  # Create group visualization
  if (nrow(final_summary_table) > 1) {
    cat("\n--- CREATING GROUP VISUALIZATIONS ---\n")
    
    # Prepare data for plotting
    plot_data <- final_summary_table %>%
      pivot_longer(
        cols = all_of(existing_numeric),
        names_to = "measure",
        values_to = "value"
      ) %>%
      filter(!is.na(value) & !is.infinite(value)) %>%
      mutate(
        measure = factor(measure, 
                         levels = existing_numeric,
                         labels = c(
                           "mean_accuracy" = "Accuracy",
                           "mean_confidence" = "Confidence", 
                           "auroc" = "AUROC",
                           "d" = "d'",
                           "metad" = "meta-d'",
                           "mratio" = "M-ratio"
                         )[existing_numeric])
      )
    
    # Create violin plots
    group_plot <- ggplot(plot_data, aes(x = measure, y = value)) +
      geom_violin(fill = "lightblue", alpha = 0.7) +
      geom_boxplot(width = 0.2, fill = "white", alpha = 0.8) +
      geom_jitter(width = 0.1, alpha = 0.5, size = 2) +
      facet_wrap(~measure, scales = "free", ncol = 3) +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        strip.text = element_text(size = 12, face = "bold"),
        panel.grid.minor = element_blank()
      ) +
      labs(
        title = "HRD Task Performance Across All Participants",
        subtitle = sprintf("N = %d participants", nrow(final_summary_table)),
        y = "Value"
      )
    
    # Save group plot
    ggsave(
      filename = "results/group_summary_plot.png",
      plot = group_plot,
      width = 12,
      height = 8,
      dpi = 300
    )
    
    cat("  ✓ Group visualization saved\n")
    
    # Create correlation matrix if enough participants
    if (nrow(final_summary_table) >= 10) {
      cor_data <- final_summary_table %>%
        select(all_of(existing_numeric)) %>%
        filter(complete.cases(.))
      
      if (nrow(cor_data) >= 10) {
        cor_matrix <- cor(cor_data, use = "complete.obs")
        
        # Create correlation plot
        cor_plot_data <- as.data.frame(as.table(cor_matrix)) %>%
          rename(Var1 = Var1, Var2 = Var2, value = Freq)
        
        cor_plot <- ggplot(cor_plot_data, aes(x = Var1, y = Var2, fill = value)) +
          geom_tile() +
          geom_text(aes(label = sprintf("%.2f", value)), size = 3) +
          scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                               midpoint = 0, limits = c(-1, 1)) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          labs(title = "Correlation Matrix of HRD Measures",
               x = "", y = "", fill = "Correlation")
        
        ggsave(
          filename = "results/correlation_matrix.png",
          plot = cor_plot,
          width = 8,
          height = 8,
          dpi = 300
        )
        
        cat("  ✓ Correlation matrix saved\n")
      }
    }
  }
  
  # Save detailed results summary
  cat("\n--- FILE LOCATIONS ---\n")
  cat("Results saved to:\n")
  cat("  • Summary table: results/summary_hrd_music_study.csv\n")
  cat("  • Individual plots: results/plots/\n")
  if (nrow(final_summary_table) > 1) {
    cat("  • Group visualization: results/group_summary_plot.png\n")
    if (file.exists("results/correlation_matrix.png")) {
      cat("  • Correlation matrix: results/correlation_matrix.png\n")
    }
  }
  
  # Display first few results
  cat("\nFirst 5 participants:\n")
  print(head(final_summary_table, 5), digits = 3)
  
} else {
  cat("No participants were successfully processed.\n")
  cat("Please check:\n")
  cat("  1. Data file format (should be comma-delimited)\n")
  cat("  2. Required columns: Condition, Decision, Confidence\n")
  cat("  3. Error messages above for specific issues\n")
}

cat("\n=== ANALYSIS COMPLETE ===\n")