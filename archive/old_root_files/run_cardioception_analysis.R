#-----------------------------------------------------------------------
# CARDIOCEPTION-STYLE ANALYSIS WRAPPER
# Main script to run beautiful HRD analysis
#-----------------------------------------------------------------------

# Load required libraries
suppressPackageStartupMessages({
  library(tidyverse)
  library(patchwork)
  library(magrittr)
  library(rjags)
  library(coda)
  library(ggmcmc)
  library(ggdist)
  library(pROC)
})

# Source all necessary functions
source("code/hrd_analysis/helper_functions.R")
source("code/hrd_analysis/bin_confidence_quantiles.R")
source("code/hrd_analysis/trials2counts.R")
source("code/hrd_analysis/fit_metad_indiv.R")
source("code/hrd_analysis/calc_auroc2.R")
source("code/hrd_analysis/analyze_hrd_data.R")

# Source Cardioception plotting functions
source("cardioception_plotting_functions.R")

#-----------------------------------------------------------------------
# MAIN ANALYSIS FUNCTION
#-----------------------------------------------------------------------

analyze_participant_cardio <- function(hrd_data, participant_id = "Unknown",
                                       save_plots = TRUE, 
                                       output_dir = "results/cardioception_style") {
  
  # Check modalities in the data
  modalities <- unique(hrd_data$Modality)
  n_modalities <- length(modalities)
  
  cat(sprintf("       Found %d modalities: %s\n", n_modalities, paste(modalities, collapse = ", ")))
  
  # Create output directory
  if (save_plots) {
    participant_dir <- file.path(output_dir, participant_id)
    dir.create(participant_dir, showWarnings = FALSE, recursive = TRUE)
  }
  
  # Initialize results list
  results <- list()
  results$modalities <- modalities
  
  # 1. Run metacognition analysis for each modality separately
  cat("       • Metacognition analysis:\n")
  meta_results_all <- list()
  
  for (mod in modalities) {
    cat(sprintf("         - %s: ", mod))
    mod_data <- hrd_data %>% filter(Modality == mod)
    
    meta_results <- tryCatch({
      analyze_hrd_data(mod_data, nRatings = 4, plot_results = FALSE,
                       show_traceplot = FALSE, participant_id = paste(participant_id, mod, sep = "_"))
    }, error = function(e) {
      cat("✗\n")
      warning(sprintf("Meta-d analysis failed for %s %s: %s", participant_id, mod, e$message))
      NULL
    })
    
    if (!is.null(meta_results)) {
      cat("✓\n")
      meta_results$modality <- mod
      meta_results_all[[mod]] <- meta_results
    }
  }
  
  # Combine meta results
  if (length(meta_results_all) > 0) {
    results$meta <- meta_results_all
    
    # Create combined summary for compatibility
    if (n_modalities == 2) {
      # Average the metrics across modalities for overall summary
      results$meta_combined <- data.frame(
        mean_accuracy = mean(sapply(meta_results_all, function(x) x$mean_accuracy)),
        mean_confidence = mean(sapply(meta_results_all, function(x) x$mean_confidence)),
        d = mean(sapply(meta_results_all, function(x) x$d)),
        metad = mean(sapply(meta_results_all, function(x) x$metad)),
        mratio = mean(sapply(meta_results_all, function(x) x$mratio)),
        auroc = mean(sapply(meta_results_all, function(x) x$auroc))
      )
    } else {
      results$meta_combined <- meta_results_all[[1]]
    }
  }
  
  # 2. Create psychometric curve (shows both modalities on same plot)
  cat("       • Psychometric curve...")
  tryCatch({
    p_psychometric <- plot_psychometric_cardio(hrd_data)
    if (save_plots) {
      ggsave(file.path(participant_dir, "psychometric_curve.png"),
             p_psychometric, width = 10, height = 8, dpi = 300)
    }
    results$plots$psychometric <- p_psychometric
    cat(" ✓\n")
  }, error = function(e) {
    cat(" ✗\n")
    warning(sprintf("Psychometric plot failed: %s", e$message))
  })
  
  # 3. Create reaction time plot
  cat("       • Reaction times...")
  tryCatch({
    p_rt <- plot_reaction_time_cardio(hrd_data)
    if (save_plots) {
      ggsave(file.path(participant_dir, "reaction_times.png"),
             p_rt, width = 10, height = 6, dpi = 300)
    }
    results$plots$rt <- p_rt
    cat(" ✓\n")
  }, error = function(e) {
    cat(" ✗\n")
    warning(sprintf("RT plot failed: %s", e$message))
  })
  
  # 4. Create confidence histogram
  cat("       • Confidence distribution...")
  tryCatch({
    p_conf <- plot_confidence_histogram_cardio(hrd_data)
    if (save_plots) {
      ggsave(file.path(participant_dir, "confidence_histogram.png"),
             p_conf, width = ifelse(n_modalities == 2, 12, 8), height = 6, dpi = 300)
    }
    results$plots$confidence <- p_conf
    cat(" ✓\n")
  }, error = function(e) {
    cat(" ✗\n")
    warning(sprintf("Confidence plot failed: %s", e$message))
  })
  
  # 5. Calculate AUROC for each modality
  cat("       • AUROC analysis:\n")
  auroc_results <- list()
  
  for (mod in modalities) {
    cat(sprintf("         - %s: ", mod))
    mod_data <- hrd_data %>% filter(Modality == mod)
    
    tryCatch({
      auc_result <- calculate_auroc_cardio(mod_data)
      auroc_results[[mod]] <- auc_result$auc
      cat(sprintf("%.3f ✓\n", auc_result$auc))
      
      if (save_plots) {
        ggsave(file.path(participant_dir, sprintf("auroc_analysis_%s.png", mod)),
               auc_result$plot, width = 12, height = 5, dpi = 300)
      }
      
      # Store the first modality's plot for composite
      if (is.null(results$plots$auroc)) {
        results$plots$auroc <- auc_result$plot
      }
    }, error = function(e) {
      cat("✗\n")
      warning(sprintf("AUROC analysis failed for %s: %s", mod, e$message))
      auroc_results[[mod]] <- NA
    })
  }
  
  results$auroc_values <- auroc_results
  
  # 6. Create composite plot
  if (!is.null(results$meta_combined) && length(results$plots) > 0) {
    cat("       • Composite plot...")
    tryCatch({
      # Create enhanced summary plot for multiple modalities
      if (n_modalities == 2 && length(meta_results_all) == 2) {
        summary_text <- sprintf(
          "Summary Statistics\n\nEXTERO:\nAccuracy: %.1f%%  Confidence: %.1f%%\nd' = %.2f  meta-d' = %.2f\nM-ratio = %.2f  AUROC = %.2f\n\nINTERO:\nAccuracy: %.1f%%  Confidence: %.1f%%\nd' = %.2f  meta-d' = %.2f\nM-ratio = %.2f  AUROC = %.2f",
          meta_results_all[["Extero"]]$mean_accuracy * 100,
          meta_results_all[["Extero"]]$mean_confidence,
          meta_results_all[["Extero"]]$d,
          meta_results_all[["Extero"]]$metad,
          meta_results_all[["Extero"]]$mratio,
          ifelse(is.na(auroc_results[["Extero"]]), meta_results_all[["Extero"]]$auroc, auroc_results[["Extero"]]),
          meta_results_all[["Intero"]]$mean_accuracy * 100,
          meta_results_all[["Intero"]]$mean_confidence,
          meta_results_all[["Intero"]]$d,
          meta_results_all[["Intero"]]$metad,
          meta_results_all[["Intero"]]$mratio,
          ifelse(is.na(auroc_results[["Intero"]]), meta_results_all[["Intero"]]$auroc, auroc_results[["Intero"]])
        )
      } else {
        # Single modality summary
        mod <- modalities[1]
        summary_text <- sprintf(
          "Summary Statistics (%s)\n\nAccuracy: %.1f%%\nConfidence: %.1f%%\n\nMetacognition:\nd' = %.2f\nmeta-d' = %.2f\nM-ratio = %.2f\nAUROC = %.2f",
          mod,
          results$meta_combined$mean_accuracy * 100,
          results$meta_combined$mean_confidence,
          results$meta_combined$d,
          results$meta_combined$metad,
          results$meta_combined$mratio,
          ifelse(is.na(auroc_results[[mod]]), results$meta_combined$auroc, auroc_results[[mod]])
        )
      }
      
      p_summary <- ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = summary_text,
                 size = 4, hjust = 0.5, vjust = 0.5, family = "mono") +
        theme_void() +
        theme(
          panel.border = element_rect(fill = NA, color = "gray80"),
          plot.margin = margin(20, 20, 20, 20)
        )
      
      # Create composite
      if (!is.null(results$plots$psychometric)) {
        top_plots <- list()
        if (!is.null(results$plots$rt)) top_plots$rt <- results$plots$rt
        if (!is.null(results$plots$confidence)) top_plots$conf <- results$plots$confidence
        
        if (length(top_plots) > 0) {
          top_row <- wrap_plots(top_plots, ncol = 2)
          composite <- top_row / (results$plots$psychometric + p_summary) +
            plot_annotation(
              title = sprintf("HRD Analysis - %s", participant_id),
              theme = theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 18))
            ) +
            plot_layout(heights = c(1, 1.5))
          
          if (save_plots) {
            ggsave(file.path(participant_dir, "composite_analysis.png"),
                   composite, width = 16, height = 12, dpi = 300)
          }
          results$plots$composite <- composite
          cat(" ✓\n")
        }
      }
    }, error = function(e) {
      cat(" ✗\n")
      warning(sprintf("Composite plot failed: %s", e$message))
    })
  }
  
  cat("       ✓ Complete\n")
  return(results)
}

#-----------------------------------------------------------------------
# BATCH PROCESSING FUNCTION
#-----------------------------------------------------------------------

analyze_all_participants <- function(data_dir = "data", 
                                     output_dir = "results/cardioception_style") {
  
  cat("\n🎨 CARDIOCEPTION-STYLE HRD ANALYSIS\n")
  cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")
  
  # Find all HRD files
  data_files <- list.files(data_dir, 
                           pattern = "HRD_final\\.txt$",
                           recursive = TRUE, 
                           full.names = TRUE)
  
  if (length(data_files) == 0) {
    stop("No HRD_final.txt files found!")
  }
  
  cat(sprintf("Found %d participants\n", length(data_files)))
  
  # Process each participant
  all_results <- list()
  
  for (i in seq_along(data_files)) {
    file_path <- data_files[i]
    
    # Extract participant ID
    participant_id <- basename(dirname(file_path))
    if (participant_id %in% c(".", "data", "")) {
      participant_id <- gsub("HRD_final\\.txt$", "", basename(file_path))
    }
    
    # Clear progress reporting with file info
    cat(sprintf("\n[%d/%d] %s\n", i, length(data_files), participant_id))
    cat(sprintf("       File: %s\n", file_path))
    
    # Read data
    hrd_data <- tryCatch({
      read_delim(file_path, delim = ",", show_col_types = FALSE)
    }, error = function(e) {
      cat(sprintf("       ✗ Error reading file: %s\n", e$message))
      NULL
    })
    
    if (is.null(hrd_data)) next
    
    # Analyze
    results <- analyze_participant_cardio(hrd_data, participant_id, 
                                          save_plots = TRUE, 
                                          output_dir = output_dir)
    
    # Store results - handle both single and multiple modalities
    if (!is.null(results$meta)) {
      # Check if we have multiple modalities
      if (length(results$modalities) > 1) {
        # Save each modality separately
        for (mod in names(results$meta)) {
          tryCatch({
            summary_row <- results$meta[[mod]]
            # Ensure all required columns exist
            summary_row$participant_id <- participant_id
            summary_row$modality <- mod
            summary_row$auroc_logistic <- ifelse(is.null(results$auroc_values[[mod]]), 
                                                 NA, results$auroc_values[[mod]])
            # Standardize column names
            if ("estimated_threshold" %in% names(summary_row)) {
              summary_row$threshold <- summary_row$estimated_threshold
            }
            if ("estimated_slope" %in% names(summary_row)) {
              summary_row$slope <- summary_row$estimated_slope
            }
            
            all_results[[paste(participant_id, mod, sep = "_")]] <- summary_row
          }, error = function(e) {
            cat(sprintf("       ⚠️  Warning: Could not save results for %s %s: %s\n", 
                        participant_id, mod, e$message))
          })
        }
      } else {
        # Single modality
        tryCatch({
          summary_row <- results$meta_combined
          summary_row$participant_id <- participant_id
          summary_row$modality <- results$modalities[1]
          summary_row$auroc_logistic <- ifelse(is.null(results$auroc_values[[results$modalities[1]]]), 
                                               NA, results$auroc_values[[results$modalities[1]]])
          # Standardize column names
          if ("estimated_threshold" %in% names(summary_row)) {
            summary_row$threshold <- summary_row$estimated_threshold
          }
          if ("estimated_slope" %in% names(summary_row)) {
            summary_row$slope <- summary_row$estimated_slope
          }
          
          all_results[[participant_id]] <- summary_row
        }, error = function(e) {
          cat(sprintf("       ⚠️  Warning: Could not save results for %s: %s\n", 
                      participant_id, e$message))
        })
      }
    }
  }
  
  # Save summary table - handle potential column mismatches
  if (length(all_results) > 0) {
    # Get all unique column names
    all_cols <- unique(unlist(lapply(all_results, names)))
    
    # Standardize all data frames to have the same columns
    standardized_results <- lapply(all_results, function(df) {
      missing_cols <- setdiff(all_cols, names(df))
      if (length(missing_cols) > 0) {
        for (col in missing_cols) {
          df[[col]] <- NA
        }
      }
      # Reorder columns to match
      df[, all_cols]
    })
    
    # Now rbind should work
    summary_table <- do.call(rbind, standardized_results)
    rownames(summary_table) <- NULL
    
    # Save CSV
    write.csv(summary_table, 
              file.path(output_dir, "summary_cardioception_analysis.csv"), 
              row.names = FALSE)
    
    cat("\n✅ ANALYSIS COMPLETE\n")
    cat(sprintf("   Processed: %d participant-modality combinations\n", nrow(summary_table)))
    cat(sprintf("   Results saved to: %s\n", output_dir))
    
    # Calculate summary statistics by modality
    cat("\nSummary by modality:\n")
    summary_by_mod <- summary_table %>%
      group_by(modality) %>%
      summarise(
        n = n(),
        mean_accuracy = mean(mean_accuracy, na.rm = TRUE),
        sd_accuracy = sd(mean_accuracy, na.rm = TRUE),
        mean_mratio = mean(mratio, na.rm = TRUE),
        sd_mratio = sd(mratio, na.rm = TRUE),
        mean_auroc = mean(auroc_logistic, na.rm = TRUE),
        .groups = 'drop'
      )
    
    for (i in 1:nrow(summary_by_mod)) {
      cat(sprintf("\n%s (n=%d):\n", summary_by_mod$modality[i], summary_by_mod$n[i]))
      cat(sprintf("  Accuracy: %.1f%% (SD = %.1f%%)\n", 
                  summary_by_mod$mean_accuracy[i] * 100, 
                  summary_by_mod$sd_accuracy[i] * 100))
      cat(sprintf("  M-ratio: %.2f (SD = %.2f)\n", 
                  summary_by_mod$mean_mratio[i], 
                  summary_by_mod$sd_mratio[i]))
      cat(sprintf("  AUROC: %.3f\n", summary_by_mod$mean_auroc[i]))
    }
    
    return(summary_table)
  } else {
    warning("No participants were successfully analyzed")
    return(NULL)
  }
}

#-----------------------------------------------------------------------
# RUN ANALYSIS
#-----------------------------------------------------------------------

if (interactive()) {
  # Run the analysis
  results <- analyze_all_participants()
  
  # Display summary
  if (!is.null(results)) {
    cat("\nSummary statistics:\n")
    cat(sprintf("  Mean accuracy: %.1f%% (SD = %.1f%%)\n", 
                mean(results$mean_accuracy) * 100, 
                sd(results$mean_accuracy) * 100))
    cat(sprintf("  Mean M-ratio: %.2f (SD = %.2f)\n", 
                mean(results$mratio), 
                sd(results$mratio)))
  }
}