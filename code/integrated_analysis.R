#-----------------------------------------------------------------------
# INTEGRATED HRD ANALYSIS WITH CARDIOCEPTION-STYLE PLOTS
# 
# This script combines:
# 1. HRD metacognition analysis (meta-d', AUROC)
# 2. Beautiful Cardioception-style psychophysics plots
# 3. Optional Bayesian psychometric fitting
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
  library(caret)
  library(gt)
  library(pROC)
})

# Source our HRD analysis functions
source("code/hrd_analysis/helper_functions.R")
source("code/hrd_analysis/bin_confidence_quantiles.R")
source("code/hrd_analysis/trials2counts.R")
source("code/hrd_analysis/fit_metad_indiv.R")
source("code/hrd_analysis/calc_auroc2.R")
source("code/hrd_analysis/analyze_hrd_data.R")

#-----------------------------------------------------------------------
# CARDIOCEPTION HELPER FUNCTIONS (adapted for single modality)
#-----------------------------------------------------------------------

# Function to create bins (equivalent of python metadpy version)
discretebins <- function(df, nbins) {
  temp <- list()
  out <- list()
  
  quan <- quantile(df$Confidence, probs = seq(0, 1, length.out = nbins + 1), na.rm = TRUE)
  
  if ((quan[1] == quan[2]) & (quan[nbins] == quan[nbins + 1])) {
    warning("The resulting rating scale contains too many identical values")
    # Return equal bins as fallback
    ratings <- cut(df$Confidence, breaks = nbins, labels = FALSE)
    return(list(ratings, out))
  }
  
  if (quan[nbins] == quan[nbins + 1]) {
    print("Correcting for bias in high confidence ratings")
    hiConf <- tail(quan, n = 1)
    quan <- quantile(df$Confidence[df$Confidence != hiConf], probs = seq(0, 1, length.out = nbins + 1))
    for (b in 1:(length(quan) - 1)) {
      temp[[b]] <- (df$Confidence >= quan[b]) & (df$Confidence <= quan[b + 1])
    }
    out[["quan"]] <- quan
    out[["hiconf"]] <- hiConf
    out[["rebin"]] <- 1
  } else if (quan[1] == quan[2]) {
    print("Correction for bias in low confidence ratings")
    lowConf <- quan[1]
    quan <- quantile(df$Confidence[df$Confidence != lowConf], probs = seq(0, 1, length.out = nbins + 1))
    for (b in 2:(length(quan))) {
      temp[[b]] <- (df$Confidence >= quan[b - 1]) & (df$Confidence <= quan[b])
    }
    out[["quan"]] <- quan
    out[["lowConf"]] <- lowConf
    out[["rebin"]] <- 1
  } else {
    for (b in 1:(length(quan) - 1)) {
      temp[[b]] <- (df$Confidence >= quan[b]) & (df$Confidence <= quan[b + 1])
    }
    out[["quan"]] <- quan
    out[["rebin"]] <- 0
  }
  
  ratings <- array(0, dim = length(df$Confidence))
  for (b in 1:nbins) {
    if (!is.null(temp[[b]])) {
      ratings[temp[[b]]] <- b
    }
  }
  
  return(list(ratings, out))
}

# Beautiful psychometric curve plot (Cardioception style)
analysis_plot_cardio <- function(df) {
  # Add a Modality column if it doesn't exist (for compatibility)
  if (!"Modality" %in% names(df)) {
    df$Modality <- "Intero"
  }
  
  # Get the estimated threshold and slope
  df_summary <- df %>%
    filter(!is.na(Decision)) %>%
    group_by(Modality) %>%
    summarize(
      means = last(na.omit(EstimatedThreshold)),
      sds = last(na.omit(EstimatedSlope)),
      .groups = 'drop'
    )
  
  # Create psychometric function data
  x_range <- seq(-40, 40, length.out = 200)
  
  # For each modality, create the curve
  curve_data <- df_summary %>%
    rowwise() %>%
    do({
      mod_data <- .
      data.frame(
        Modality = mod_data$Modality,
        x = x_range,
        y = pnorm((x_range - mod_data$means) * mod_data$sds)
      )
    })
  
  # Calculate proportion of "More" responses for each Alpha level
  response_data <- df %>%
    filter(!is.na(Decision)) %>%
    group_by(Alpha, Modality) %>%
    summarize(
      n_total = n(),
      n_more = sum(Decision == "More"),
      prop_more = n_more / n_total,
      .groups = 'drop'
    )
  
  # Set colors based on modality
  if (length(unique(df$Modality)) == 1 && unique(df$Modality) == "Intero") {
    color_values <- c("#c44e52")
  } else if (length(unique(df$Modality)) == 1 && unique(df$Modality) == "Extero") {
    color_values <- c("#4c72b0")
  } else {
    color_values <- c("#4c72b0", "#c44e52")
  }
  
  # Create the plot
  p <- ggplot() +
    # Psychometric curves
    geom_line(data = curve_data, 
              aes(x = x, y = y, color = Modality),
              linetype = "dashed", size = 1.2) +
    # Threshold lines
    geom_segment(data = df_summary,
                 aes(x = means, xend = means, y = 0, yend = 0.5, color = Modality),
                 show.legend = FALSE) +
    # Points at threshold
    geom_point(data = df_summary,
               aes(x = means, y = 0.5, color = Modality),
               size = 8, show.legend = FALSE) +
    # Data points sized by number of trials
    geom_point(data = response_data,
               aes(x = Alpha, y = prop_more, color = Modality, size = n_total),
               alpha = 0.6) +
    # Text annotations
    geom_text(data = df_summary,
              aes(x = -20, y = 0.75,
                  label = paste("Threshold:", round(means, 2)),
                  color = Modality),
              hjust = 0, show.legend = FALSE) +
    geom_text(data = df_summary,
              aes(x = -20, y = 0.70,
                  label = paste("Slope:", round(sds, 2)),
                  color = Modality),
              hjust = 0, show.legend = FALSE) +
    # Styling
    scale_color_manual(values = color_values) +
    scale_size_continuous(range = c(2, 10), guide = "none") +
    coord_cartesian(xlim = c(-40, 40), ylim = c(0, 1)) +
    labs(
      x = expression(paste("Intensity  (", Delta, "BPM)")),
      y = "P(Response = More | Intensity)",
      color = NULL
    ) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(colour = "black"),
      legend.position = c(0.85, 0.15),
      legend.key.size = unit(1, "cm"),
      legend.background = element_rect(fill = "white", color = NA),
      text = element_text(size = 12),
      axis.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 12)
    )
  
  return(p)
}

# Reaction time plot (Cardioception style)
reaction_time_plot_cardio <- function(df) {
  # Set up parameters
  s <- 3     # spacing between conditions
  sc <- 0.4  # spacing between correct/incorrect
  col <- c("#5f9e6e", "#b55d60")  # green for correct, red for incorrect
  
  # Add Modality if missing
  if (!"Modality" %in% names(df)) {
    df$Modality <- "Intero"
  }
  
  # Prepare data
  df_long <- df %>%
    filter(!is.na(DecisionRT) & !is.na(ConfidenceRT)) %>%
    pivot_longer(cols = c(DecisionRT, ConfidenceRT),
                 names_to = "RTType",
                 values_to = "RT") %>%
    mutate(
      ResponseCorrect = factor(ifelse(ResponseCorrect == 1, "Correct", "Incorrect"),
                               levels = c("Correct", "Incorrect")),
      RTType = factor(RTType, 
                      levels = c("DecisionRT", "ConfidenceRT"),
                      labels = c("Decision", "Confidence"))
    )
  
  # Create plot
  p <- ggplot(df_long, aes(y = RT, fill = ResponseCorrect)) +
    # Rain cloud plots
    ggdist::stat_halfeye(
      aes(x = as.numeric(ResponseCorrect) - 1.5),
      adjust = 1.5,
      width = 0.6,
      .width = 0,
      justification = -0.2,
      alpha = 0.7
    ) +
    geom_boxplot(
      aes(x = as.numeric(ResponseCorrect) - 1.5),
      width = 0.15,
      outlier.shape = NA,
      alpha = 0.5
    ) +
    geom_point(
      aes(x = as.numeric(ResponseCorrect) - 1.5),
      position = position_jitter(width = 0.05),
      size = 1,
      alpha = 0.3
    ) +
    facet_wrap(~RTType, scales = "free_x") +
    scale_fill_manual(values = col) +
    coord_flip() +
    labs(
      y = "Response Time (s)",
      x = "",
      fill = NULL
    ) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(colour = "black"),
      strip.text = element_text(size = 14, face = "bold"),
      legend.position = "bottom",
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )
  
  return(p)
}

# Confidence histogram (Cardioception style)
plot_confidence_cardio <- function(df) {
  if (!"Modality" %in% names(df)) {
    df$Modality <- "Intero"
  }
  
  # Bin confidence ratings
  df$Confidence_bin <- discretebins(df, 4)[[1]]
  
  # Calculate proportions for correct/incorrect
  conf_summary <- df %>%
    filter(!is.na(Confidence_bin)) %>%
    mutate(ResponseCorrect = ifelse(ResponseCorrect == 1, "Correct", "Incorrect")) %>%
    group_by(Modality, Confidence_bin, ResponseCorrect) %>%
    summarise(n = n(), .groups = 'drop') %>%
    group_by(Modality, ResponseCorrect) %>%
    mutate(prop = n / sum(n)) %>%
    ungroup()
  
  # Create plot
  p <- ggplot(conf_summary, aes(x = Confidence_bin, y = prop, fill = ResponseCorrect)) +
    geom_col(position = "dodge", color = "black", width = 0.7) +
    scale_fill_manual(values = c("Correct" = "#5f9e6e", "Incorrect" = "#b55d60")) +
    facet_wrap(~Modality) +
    labs(
      x = "Confidence Rating Bin",
      y = "P(Confidence = y | Outcome)",
      fill = NULL
    ) +
    scale_x_continuous(breaks = 1:4) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(colour = "black"),
      strip.text = element_text(size = 14, face = "bold"),
      legend.position = "bottom"
    )
  
  return(p)
}

# AUROC plot with confidence scatter
get_AUC_cardio <- function(df) {
  if (!"Modality" %in% names(df)) {
    df$Modality <- "Intero"
  }
  
  # Calculate AUROC for each modality
  auroc_data <- df %>%
    group_by(Modality) %>%
    do({
      mod_data <- .
      # Fit logistic regression
      m1 <- glm(ResponseCorrect ~ Confidence, 
                data = mod_data, 
                family = binomial(link = "logit"))
      # Calculate ROC
      roc_obj <- pROC::roc(mod_data$ResponseCorrect, m1$fitted.values, quiet = TRUE)
      auc_val <- pROC::auc(roc_obj)
      
      # Get ROC curve data
      roc_df <- data.frame(
        sensitivity = roc_obj$sensitivities,
        specificity = roc_obj$specificities,
        Modality = unique(mod_data$Modality),
        AUC = as.numeric(auc_val)
      )
      roc_df
    })
  
  # Set colors
  if (unique(df$Modality) == "Intero") {
    color_values <- c("#c44e52")
  } else if (unique(df$Modality) == "Extero") {
    color_values <- c("#4c72b0")
  } else {
    color_values <- c("#4c72b0", "#c44e52")
  }
  
  # Create AUROC plot
  auc_labels <- auroc_data %>%
    group_by(Modality) %>%
    slice(1) %>%
    mutate(AUC_label = paste0("AUC = ", round(AUC, 3)))
  
  p1 <- ggplot(auroc_data, aes(x = 1 - specificity, y = sensitivity, color = Modality)) +
    geom_line(size = 1.2) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.5) +
    geom_text(data = auc_labels,
              aes(x = 0.7, y = 0.3, label = AUC_label),
              size = 5) +
    scale_color_manual(values = color_values) +
    labs(
      x = "1 - Specificity",
      y = "Sensitivity",
      color = NULL
    ) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.line = element_line(color = "black"),
      legend.position = "none"
    )
  
  # Create confidence scatter plot
  p2 <- ggplot(df, aes(x = factor(ResponseCorrect), y = Confidence, color = Modality)) +
    geom_jitter(width = 0.2, alpha = 0.5) +
    scale_color_manual(values = color_values) +
    scale_x_discrete(labels = c("Incorrect", "Correct")) +
    labs(
      x = "Response",
      y = "Confidence",
      color = NULL
    ) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.line = element_line(color = "black"),
      legend.position = "none"
    )
  
  # Combine plots
  combined_plot <- p2 + p1
  
  return(list(plot = combined_plot, auc = unique(auc_labels$AUC)))
}

#-----------------------------------------------------------------------
# INTEGRATED ANALYSIS FUNCTION
#-----------------------------------------------------------------------

analyze_hrd_complete <- function(hrd_data, participant_id = "Unknown",
                                 save_plots = TRUE, output_dir = "results/cardioception_style") {
  
  cat(sprintf("\n🎨 Creating Cardioception-style analysis for %s...\n", participant_id))
  
  # Create output directory
  if (save_plots) {
    participant_dir <- file.path(output_dir, participant_id)
    dir.create(participant_dir, showWarnings = FALSE, recursive = TRUE)
  }
  
  # Ensure data has required columns
  if (!"Modality" %in% names(hrd_data)) {
    hrd_data$Modality <- "Intero"
  }
  
  # Run standard HRD metacognition analysis
  cat("  • Running metacognition analysis...")
  meta_results <- tryCatch({
    analyze_hrd_data(hrd_data, nRatings = 4, plot_results = FALSE,
                     show_traceplot = FALSE, participant_id = participant_id)
  }, error = function(e) {
    cat(" [Failed]\n")
    NULL
  })
  
  if (!is.null(meta_results)) {
    cat(" ✓\n")
  }
  
  # Create Cardioception-style plots
  cat("  • Creating psychophysics visualizations...\n")
  
  # 1. Main psychometric curve
  cat("    - Psychometric curve...")
  psychometric_plot <- analysis_plot_cardio(hrd_data)
  if (save_plots) {
    ggsave(file.path(participant_dir, "psychometric_curve_cardio.png"),
           psychometric_plot, width = 10, height = 8, dpi = 300)
  }
  cat(" ✓\n")
  
  # 2. Reaction times
  cat("    - Reaction time analysis...")
  rt_plot <- reaction_time_plot_cardio(hrd_data)
  if (save_plots) {
    ggsave(file.path(participant_dir, "reaction_times_cardio.png"),
           rt_plot, width = 10, height = 6, dpi = 300)
  }
  cat(" ✓\n")
  
  # 3. Confidence histogram
  cat("    - Confidence distribution...")
  conf_plot <- plot_confidence_cardio(hrd_data)
  if (save_plots) {
    ggsave(file.path(participant_dir, "confidence_histogram_cardio.png"),
           conf_plot, width = 8, height = 6, dpi = 300)
  }
  cat(" ✓\n")
  
  # 4. AUROC analysis
  cat("    - AUROC analysis...")
  auc_result <- get_AUC_cardio(hrd_data)
  if (save_plots) {
    ggsave(file.path(participant_dir, "auroc_analysis_cardio.png"),
           auc_result$plot, width = 12, height = 5, dpi = 300)
  }
  cat(" ✓\n")
  
  # 5. Create composite plot (similar to Cardioception)
  if (!is.null(meta_results)) {
    cat("    - Creating composite plot...")
    
    # Create a summary text plot
    summary_text <- sprintf(
      "Summary Statistics\n\nAccuracy: %.1f%%\nConfidence: %.1f%%\n\nMetacognition:\nd' = %.2f\nmeta-d' = %.2f\nM-ratio = %.2f\nAUROC = %.2f",
      meta_results$mean_accuracy * 100,
      meta_results$mean_confidence,
      meta_results$d,
      meta_results$metad,
      meta_results$mratio,
      auc_result$auc
    )
    
    summary_plot <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = summary_text,
               size = 5, hjust = 0.5, vjust = 0.5) +
      theme_void() +
      theme(
        panel.border = element_rect(fill = NA, color = "gray80"),
        plot.margin = margin(20, 20, 20, 20)
      )
    
    # Combine all plots
    composite_plot <- (rt_plot + conf_plot + auc_result$plot) / 
      (psychometric_plot + summary_plot) +
      plot_annotation(
        title = sprintf("HRD Analysis - %s", participant_id),
        theme = theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 18))
      ) +
      plot_layout(heights = c(1, 1.5))
    
    if (save_plots) {
      ggsave(file.path(participant_dir, "composite_analysis_cardio.png"),
             composite_plot, width = 16, height = 12, dpi = 300)
    }
    cat(" ✓\n")
  }
  
  cat("  ✓ Analysis complete!\n")
  
  # Return results
  return(list(
    meta_results = meta_results,
    auroc = auc_result$auc,
    plots = list(
      psychometric = psychometric_plot,
      reaction_times = rt_plot,
      confidence = conf_plot,
      auroc = auc_result$plot
    )
  ))
}

#-----------------------------------------------------------------------
# MAIN EXECUTION
#-----------------------------------------------------------------------

if (interactive()) {
  cat("\n🎨 HRD ANALYSIS WITH CARDIOCEPTION-STYLE PLOTS\n")
  cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")
  
  # Find all HRD data files
  data_files <- list.files("data", pattern = "HRD_final\\.txt$",
                           recursive = TRUE, full.names = TRUE)
  
  if (length(data_files) == 0) {
    stop("No HRD data files found in the data directory!")
  }
  
  cat(sprintf("Found %d participant(s) to analyze\n", length(data_files)))
  
  # Process each participant
  all_results <- list()
  
  for (i in seq_along(data_files)) {
    file_path <- data_files[i]
    
    # Extract participant ID
    participant_id <- basename(dirname(file_path))
    if (participant_id %in% c(".", "data", "")) {
      participant_id <- gsub("HRD_final\\.txt$", "", basename(file_path))
    }
    
    cat(sprintf("\n[%d/%d] Processing %s", i, length(data_files), participant_id))
    
    # Read data
    hrd_data <- read_delim(file_path, delim = ",", show_col_types = FALSE)
    
    # Run integrated analysis
    results <- analyze_hrd_complete(hrd_data, participant_id)
    
    if (!is.null(results$meta_results)) {
      results$meta_results$participant_id <- participant_id
      results$meta_results$auroc_logistic <- results$auroc
      all_results[[participant_id]] <- results$meta_results
    }
  }
  
  # Create summary table
  if (length(all_results) > 0) {
    summary_table <- do.call(rbind, all_results)
    write.csv(summary_table, "results/cardioception_style/summary_complete.csv", row.names = FALSE)
    
    cat("\n\n✅ ALL ANALYSES COMPLETE!\n")
    cat(sprintf("   Analyzed %d participants\n", nrow(summary_table)))
    cat("   Beautiful plots saved to: results/cardioception_style/\n")
    cat("   Summary table: results/cardioception_style/summary_complete.csv\n")
  }
}