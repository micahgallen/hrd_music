#-----------------------------------------------------------------------
# HRD MUSIC STUDY - INTEGRATED PSYCHOPHYSICS ANALYSIS
# 
# This script combines HRD metacognition analysis with psychophysical
# modeling and visualization from the Cardioception package
#
# Features:
# - Metacognitive efficiency (meta-d') analysis
# - Psychophysical curve fitting
# - Comprehensive visualization suite
# - Optional Bayesian psychometric function fitting
#-----------------------------------------------------------------------

# Load required packages
suppressPackageStartupMessages({
  library(tidyverse)
  library(patchwork)
  library(magrittr)
  library(rjags)
  library(coda)
  library(ggmcmc)
  library(ggdist)
  library(psycho)
  library(pracma)
  library(grid)
  library(cowplot)
})

# Source our HRD analysis functions
source("code/hrd_analysis/helper_functions.R")
source("code/hrd_analysis/bin_confidence_quantiles.R")
source("code/hrd_analysis/trials2counts.R")
source("code/hrd_analysis/fit_metad_indiv.R")
source("code/hrd_analysis/calc_auroc2.R")
source("code/hrd_analysis/analyze_hrd_data.R")

#-----------------------------------------------------------------------
# PSYCHOPHYSICAL ANALYSIS FUNCTIONS
#-----------------------------------------------------------------------

# Function to create psychometric curve plot
plot_psychometric_curve <- function(df, participant_id = "Unknown") {
  # Calculate proportion of "More" responses for each alpha level
  psychometric_data <- df %>%
    group_by(Alpha) %>%
    summarise(
      n_trials = n(),
      n_more = sum(Decision == "More", na.rm = TRUE),
      prop_more = n_more / n_trials,
      se = sqrt(prop_more * (1 - prop_more) / n_trials)
    ) %>%
    filter(!is.na(Alpha))
  
  # Get final threshold and slope
  final_threshold <- tail(df$EstimatedThreshold, 1)
  final_slope <- tail(df$EstimatedSlope, 1)
  
  # Create psychometric function
  alpha_range <- seq(min(psychometric_data$Alpha, na.rm = TRUE), 
                     max(psychometric_data$Alpha, na.rm = TRUE), 
                     length.out = 100)
  
  # Cumulative normal psychometric function
  psychometric_function <- pnorm((alpha_range - final_threshold) * final_slope)
  
  # Create plot
  p <- ggplot() +
    # Data points with error bars
    geom_point(data = psychometric_data, 
               aes(x = Alpha, y = prop_more), 
               size = 3, alpha = 0.8) +
    geom_errorbar(data = psychometric_data,
                  aes(x = Alpha, ymin = prop_more - se, ymax = prop_more + se),
                  width = 0.02, alpha = 0.5) +
    # Fitted psychometric function
    geom_line(aes(x = alpha_range, y = psychometric_function), 
              color = "blue", size = 1.2) +
    # Threshold line
    geom_vline(xintercept = final_threshold, 
               linetype = "dashed", color = "red", alpha = 0.7) +
    # 50% line
    geom_hline(yintercept = 0.5, 
               linetype = "dotted", alpha = 0.5) +
    # Labels
    labs(
      title = sprintf("Psychometric Function - %s", participant_id),
      subtitle = sprintf("Threshold = %.3f, Slope = %.2f", final_threshold, final_slope),
      x = "Stimulus Intensity (Alpha)",
      y = "Proportion 'More' Responses"
    ) +
    scale_y_continuous(limits = c(0, 1)) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(color = "gray50")
    )
  
  return(p)
}

# Function to create reaction time analysis plot
plot_reaction_times <- function(df, participant_id = "Unknown") {
  # Prepare RT data
  rt_data <- df %>%
    filter(!is.na(DecisionRT) & !is.na(ConfidenceRT)) %>%
    mutate(
      ResponseCorrect = factor(ResponseCorrect, 
                               levels = c(0, 1), 
                               labels = c("Incorrect", "Correct")),
      TotalRT = DecisionRT + ConfidenceRT
    )
  
  # Decision RT by accuracy
  p1 <- ggplot(rt_data, aes(x = ResponseCorrect, y = DecisionRT, fill = ResponseCorrect)) +
    geom_violin(alpha = 0.7) +
    geom_boxplot(width = 0.2, fill = "white", alpha = 0.8) +
    scale_fill_manual(values = c("Incorrect" = "#E74C3C", "Correct" = "#27AE60")) +
    labs(
      title = "Decision RT by Accuracy",
      x = "Response",
      y = "Decision RT (s)"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
  
  # Confidence RT by accuracy
  p2 <- ggplot(rt_data, aes(x = ResponseCorrect, y = ConfidenceRT, fill = ResponseCorrect)) +
    geom_violin(alpha = 0.7) +
    geom_boxplot(width = 0.2, fill = "white", alpha = 0.8) +
    scale_fill_manual(values = c("Incorrect" = "#E74C3C", "Correct" = "#27AE60")) +
    labs(
      title = "Confidence RT by Accuracy",
      x = "Response",
      y = "Confidence RT (s)"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
  
  # RT over trials
  p3 <- ggplot(rt_data, aes(x = nTrials)) +
    geom_smooth(aes(y = DecisionRT), color = "blue", method = "loess", se = TRUE, alpha = 0.3) +
    geom_smooth(aes(y = ConfidenceRT), color = "red", method = "loess", se = TRUE, alpha = 0.3) +
    labs(
      title = "RT Evolution Over Trials",
      x = "Trial Number",
      y = "Reaction Time (s)"
    ) +
    theme_minimal() +
    annotate("text", x = max(rt_data$nTrials) * 0.8, y = max(rt_data$DecisionRT) * 0.9,
             label = "Decision RT", color = "blue", size = 3) +
    annotate("text", x = max(rt_data$nTrials) * 0.8, y = max(rt_data$DecisionRT) * 0.8,
             label = "Confidence RT", color = "red", size = 3)
  
  # Combine plots
  combined_rt <- p1 + p2 + p3 + 
    plot_annotation(
      title = sprintf("Reaction Time Analysis - %s", participant_id),
      theme = theme(plot.title = element_text(face = "bold", hjust = 0.5))
    )
  
  return(combined_rt)
}

# Function to create comprehensive confidence analysis
plot_confidence_analysis <- function(df, participant_id = "Unknown") {
  # Prepare confidence data
  conf_data <- df %>%
    filter(!is.na(Confidence)) %>%
    mutate(
      ResponseCorrect = factor(ResponseCorrect, 
                               levels = c(0, 1), 
                               labels = c("Incorrect", "Correct")),
      ConfidenceBin = cut(Confidence, 
                          breaks = c(-1, 25, 50, 75, 100),
                          labels = c("Low", "Medium-Low", "Medium-High", "High"))
    )
  
  # Confidence distribution by accuracy
  p1 <- ggplot(conf_data, aes(x = Confidence, fill = ResponseCorrect)) +
    geom_histogram(alpha = 0.6, position = "identity", bins = 20) +
    scale_fill_manual(values = c("Incorrect" = "#E74C3C", "Correct" = "#27AE60")) +
    labs(
      title = "Confidence Distribution",
      x = "Confidence Rating",
      y = "Count",
      fill = "Response"
    ) +
    theme_minimal()
  
  # Mean confidence by accuracy
  conf_summary <- conf_data %>%
    group_by(ResponseCorrect) %>%
    summarise(
      mean_conf = mean(Confidence),
      se_conf = sd(Confidence) / sqrt(n()),
      n = n()
    )
  
  p2 <- ggplot(conf_summary, aes(x = ResponseCorrect, y = mean_conf, fill = ResponseCorrect)) +
    geom_col(alpha = 0.7) +
    geom_errorbar(aes(ymin = mean_conf - se_conf, ymax = mean_conf + se_conf),
                  width = 0.2) +
    scale_fill_manual(values = c("Incorrect" = "#E74C3C", "Correct" = "#27AE60")) +
    labs(
      title = "Mean Confidence by Accuracy",
      x = "Response",
      y = "Mean Confidence",
      fill = "Response"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
  
  # Confidence calibration
  calibration_data <- conf_data %>%
    group_by(ConfidenceBin) %>%
    summarise(
      accuracy = mean(ResponseCorrect == "Correct"),
      n = n(),
      se = sqrt(accuracy * (1 - accuracy) / n)
    )
  
  p3 <- ggplot(calibration_data, aes(x = ConfidenceBin, y = accuracy)) +
    geom_point(size = 4) +
    geom_errorbar(aes(ymin = accuracy - se, ymax = accuracy + se), width = 0.2) +
    geom_line(group = 1, alpha = 0.5) +
    geom_hline(yintercept = 0.5, linetype = "dashed", alpha = 0.5) +
    labs(
      title = "Confidence Calibration",
      x = "Confidence Level",
      y = "Actual Accuracy"
    ) +
    scale_y_continuous(limits = c(0, 1)) +
    theme_minimal()
  
  # Combine plots
  combined_conf <- p1 / (p2 + p3) + 
    plot_annotation(
      title = sprintf("Confidence Analysis - %s", participant_id),
      theme = theme(plot.title = element_text(face = "bold", hjust = 0.5))
    )
  
  return(combined_conf)
}

# Function to create trial-by-trial dynamics plot
plot_trial_dynamics <- function(df, participant_id = "Unknown") {
  # Prepare data
  trial_data <- df %>%
    mutate(
      ResponseCorrect = as.numeric(ResponseCorrect),
      ConfidenceZ = scale(Confidence)[,1],
      AlphaZ = scale(Alpha)[,1]
    ) %>%
    filter(!is.na(Confidence))
  
  # Running accuracy
  trial_data$RunningAccuracy <- cumsum(trial_data$ResponseCorrect) / seq_along(trial_data$ResponseCorrect)
  
  # Create multi-panel plot
  p1 <- ggplot(trial_data, aes(x = nTrials)) +
    geom_line(aes(y = EstimatedThreshold), color = "blue", size = 1) +
    geom_point(aes(y = Alpha), alpha = 0.3) +
    labs(title = "Threshold Estimation", y = "Alpha / Threshold") +
    theme_minimal()
  
  p2 <- ggplot(trial_data, aes(x = nTrials)) +
    geom_line(aes(y = RunningAccuracy), color = "darkgreen", size = 1) +
    geom_hline(yintercept = 0.5, linetype = "dashed", alpha = 0.5) +
    labs(title = "Cumulative Accuracy", y = "Proportion Correct") +
    theme_minimal()
  
  p3 <- ggplot(trial_data, aes(x = nTrials, y = Confidence)) +
    geom_smooth(method = "loess", se = TRUE, color = "purple") +
    geom_point(aes(color = factor(ResponseCorrect)), alpha = 0.5) +
    scale_color_manual(values = c("0" = "#E74C3C", "1" = "#27AE60"),
                       labels = c("Incorrect", "Correct")) +
    labs(title = "Confidence Over Time", y = "Confidence", color = "Response") +
    theme_minimal()
  
  # Combine
  combined_dynamics <- p1 / p2 / p3 +
    plot_annotation(
      title = sprintf("Trial Dynamics - %s", participant_id),
      theme = theme(plot.title = element_text(face = "bold", hjust = 0.5))
    )
  
  return(combined_dynamics)
}

#-----------------------------------------------------------------------
# INTEGRATED ANALYSIS FUNCTION
#-----------------------------------------------------------------------

analyze_hrd_psychophysics <- function(hrd_data, participant_id = "Unknown", 
                                      save_plots = TRUE, output_dir = "results/psychophysics") {
  
  cat(sprintf("\nAnalyzing psychophysics for %s...\n", participant_id))
  
  # Create output directory
  if (save_plots) {
    participant_dir <- file.path(output_dir, participant_id)
    dir.create(participant_dir, showWarnings = FALSE, recursive = TRUE)
  }
  
  # Run standard HRD metacognition analysis
  cat("  Running metacognition analysis...")
  meta_results <- tryCatch({
    analyze_hrd_data(hrd_data, nRatings = 4, plot_results = FALSE, 
                     show_traceplot = FALSE, participant_id = participant_id)
  }, error = function(e) {
    cat(" [Failed]\n")
    NULL
  })
  
  if (!is.null(meta_results)) {
    cat(" [Done]\n")
  }
  
  # Create psychophysical plots
  cat("  Creating psychophysical visualizations...\n")
  
  # 1. Psychometric curve
  psychometric_plot <- plot_psychometric_curve(hrd_data, participant_id)
  if (save_plots) {
    ggsave(file.path(participant_dir, "psychometric_curve.png"), 
           psychometric_plot, width = 8, height = 6, dpi = 300)
  }
  
  # 2. Reaction time analysis
  rt_plot <- plot_reaction_times(hrd_data, participant_id)
  if (save_plots) {
    ggsave(file.path(participant_dir, "reaction_times.png"), 
           rt_plot, width = 12, height = 4, dpi = 300)
  }
  
  # 3. Confidence analysis
  conf_plot <- plot_confidence_analysis(hrd_data, participant_id)
  if (save_plots) {
    ggsave(file.path(participant_dir, "confidence_analysis.png"), 
           conf_plot, width = 10, height = 8, dpi = 300)
  }
  
  # 4. Trial dynamics
  dynamics_plot <- plot_trial_dynamics(hrd_data, participant_id)
  if (save_plots) {
    ggsave(file.path(participant_dir, "trial_dynamics.png"), 
           dynamics_plot, width = 8, height = 10, dpi = 300)
  }
  
  # 5. Create integrated summary plot
  if (!is.null(meta_results)) {
    # Top row: metacognition results
    meta_text <- sprintf(
      "Metacognitive Efficiency\n\nd' = %.2f\nmeta-d' = %.2f\nM-ratio = %.2f\nAUROC = %.2f",
      meta_results$d, meta_results$metad, meta_results$mratio, meta_results$auroc
    )
    
    meta_summary <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = meta_text, 
               size = 6, hjust = 0.5, vjust = 0.5) +
      theme_void() +
      theme(
        panel.border = element_rect(fill = NA, color = "gray80"),
        plot.margin = margin(10, 10, 10, 10)
      )
    
    # Bottom row: key metrics
    metrics_text <- sprintf(
      "Performance Summary\n\nAccuracy: %.1f%%\nMean Confidence: %.1f%%\nThreshold: %.3f\nSlope: %.2f",
      meta_results$mean_accuracy * 100, meta_results$mean_confidence,
      meta_results$estimated_threshold, meta_results$estimated_slope
    )
    
    metrics_summary <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = metrics_text, 
               size = 6, hjust = 0.5, vjust = 0.5) +
      theme_void() +
      theme(
        panel.border = element_rect(fill = NA, color = "gray80"),
        plot.margin = margin(10, 10, 10, 10)
      )
    
    # Combine with main visualizations
    summary_plot <- (meta_summary + metrics_summary) / psychometric_plot +
      plot_annotation(
        title = sprintf("HRD Psychophysics Summary - %s", participant_id),
        theme = theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 16))
      )
    
    if (save_plots) {
      ggsave(file.path(participant_dir, "summary_psychophysics.png"), 
             summary_plot, width = 12, height = 10, dpi = 300)
    }
  }
  
  cat("  Psychophysics analysis complete!\n")
  
  # Return results
  return(list(
    meta_results = meta_results,
    plots = list(
      psychometric = psychometric_plot,
      reaction_times = rt_plot,
      confidence = conf_plot,
      dynamics = dynamics_plot
    )
  ))
}

#-----------------------------------------------------------------------
# MAIN ANALYSIS SCRIPT
#-----------------------------------------------------------------------

if (interactive()) {
  cat("\n🎵 HRD MUSIC STUDY - PSYCHOPHYSICS ANALYSIS\n")
  cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")
  
  # Find all HRD data files
  data_files <- list.files("data", pattern = "HRD_final\\.txt$", 
                           recursive = TRUE, full.names = TRUE)
  
  if (length(data_files) == 0) {
    stop("No HRD data files found in the data directory!")
  }
  
  cat(sprintf("Found %d participant(s) to analyze\n\n", length(data_files)))
  
  # Process each participant
  all_results <- list()
  
  for (i in seq_along(data_files)) {
    file_path <- data_files[i]
    
    # Extract participant ID
    participant_id <- basename(dirname(file_path))
    if (participant_id %in% c(".", "data", "")) {
      participant_id <- gsub("HRD_final\\.txt$", "", basename(file_path))
    }
    
    cat(sprintf("[%d/%d] Processing %s\n", i, length(data_files), participant_id))
    
    # Read data
    hrd_data <- read_delim(file_path, delim = ",", show_col_types = FALSE)
    
    # Run integrated analysis
    results <- analyze_hrd_psychophysics(hrd_data, participant_id)
    
    if (!is.null(results$meta_results)) {
      results$meta_results$participant_id <- participant_id
      all_results[[participant_id]] <- results$meta_results
    }
  }
  
  # Create summary table
  if (length(all_results) > 0) {
    summary_table <- do.call(rbind, all_results)
    write.csv(summary_table, "results/psychophysics_summary.csv", row.names = FALSE)
    
    cat("\n✅ ANALYSIS COMPLETE!\n")
    cat(sprintf("   Analyzed %d participants\n", nrow(summary_table)))
    cat("   Results saved to: results/psychophysics/\n")
    cat("   Summary table: results/psychophysics_summary.csv\n")
  }
}