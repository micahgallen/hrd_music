#-----------------------------------------------------------------------
# NON-BAYESIAN ANALYSIS & PLOTTING FUNCTIONS (WITH TRIAL DYNAMICS)
#
# This script contains all core functions for the non-Bayesian analysis,
# now with a corrected trial dynamics plot.
#-----------------------------------------------------------------------

# Load required libraries
pacman::p_load(tidyverse, patchwork, magrittr, rjags, coda, ggmcmc, ggdist, pROC)

# Source the foundational HRD analysis functions from the project
source("code/hrd_analysis/helper_functions.R")
source("code/hrd_analysis/bin_confidence_quantiles.R")
source("code/hrd_analysis/trials2counts.R")
source("code/hrd_analysis/fit_metad_indiv.R")
source("code/hrd_analysis/calc_auroc2.R") # Corrected path
source("code/hrd_analysis/analyze_hrd_data.R")

# --- Plotting Functions ---

# --- FIXED Function to plot trial-by-trial dynamics by modality ---
plot_trial_dynamics <- function(df, participant_id = "Unknown") {
  
  # --- FIX for Cumulative Accuracy ---
  # This now correctly handles NA values in ResponseCorrect by treating them
  # as 0 for the cumulative sum calculation, while respecting trial order.
  accuracy_data <- df %>%
    arrange(Modality, nTrials) %>%
    group_by(Modality) %>%
    mutate(
      ResponseCorrect_numeric = as.numeric(ResponseCorrect),
      RunningAccuracy = cumsum(if_else(is.na(ResponseCorrect_numeric), 0, ResponseCorrect_numeric)) / row_number()
    ) %>%
    ungroup()
  
  # Filter data specifically for the other plots to handle potential NAs there
  threshold_data <- df %>% filter(!is.na(EstimatedThreshold))
  confidence_data <- df %>% filter(!is.na(Confidence))
  
  # Plot 1: Threshold Estimation (faceted)
  p1 <- ggplot(threshold_data, aes(x = nTrials)) +
    geom_line(aes(y = EstimatedThreshold), color = "blue", linewidth = 1) +
    geom_point(aes(y = Alpha), alpha = 0.3) +
    facet_wrap(~Modality, scales = "free_x") + 
    labs(title = "Threshold Estimation", y = "Alpha / Threshold") +
    theme_minimal()
  
  # Plot 2: Cumulative Accuracy (faceted)
  p2 <- ggplot(accuracy_data, aes(x = nTrials)) +
    geom_line(aes(y = RunningAccuracy), color = "darkgreen", linewidth = 1) +
    geom_hline(yintercept = 0.5, linetype = "dashed", alpha = 0.5) +
    facet_wrap(~Modality, scales = "free_x") + 
    labs(title = "Cumulative Accuracy", y = "Proportion Correct") +
    theme_minimal() +
    coord_cartesian(ylim = c(0, 1))
  
  # Plot 3: Confidence Over Time (faceted)
  p3 <- ggplot(confidence_data, aes(x = nTrials, y = Confidence)) +
    geom_smooth(method = "loess", se = TRUE, color = "purple") +
    geom_point(aes(color = factor(ResponseCorrect)), alpha = 0.5) +
    scale_color_manual(values = c("0" = "#E74C3C", "1" = "#27AE60"), labels = c("Incorrect", "Correct"), na.value="grey") +
    facet_wrap(~Modality, scales = "free_x") + 
    labs(title = "Confidence Over Time", y = "Confidence", color = "Response") +
    theme_minimal()
  
  # Combine and return
  combined_dynamics <- p1 / p2 / p3 +
    plot_annotation(
      title = sprintf("Trial Dynamics - %s", participant_id),
      theme = theme(plot.title = element_text(face = "bold", hjust = 0.5))
    )
  return(combined_dynamics)
}


plot_psychometric_cardio <- function(df) {
  df_summary <- df %>%
    filter(!is.na(Decision)) %>%
    group_by(Modality) %>%
    summarize(
      means = last(na.omit(EstimatedThreshold)),
      sds = last(na.omit(EstimatedSlope)),
      .groups = 'drop'
    )
  
  x_range <- seq(-40, 40, length.out = 200)
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
  
  response_data <- df %>%
    filter(!is.na(Decision)) %>%
    group_by(Alpha, Modality) %>%
    summarize(
      n_total = n(),
      n_more = sum(Decision == "More"),
      prop_more = n_more / n_total,
      .groups = 'drop'
    )
  
  color_values <- c("Extero" = "#4c72b0", "Intero" = "#c44e52")
  if (length(unique(df$Modality)) == 1) {
    color_values <- if (unique(df$Modality) == "Intero") c("Intero" = "#c44e52") else c("Extero" = "#4c72b0")
  }
  
  p <- ggplot() +
    geom_line(data = curve_data, aes(x = x, y = y, color = Modality), linetype = "dashed", linewidth = 1.2) +
    geom_segment(data = df_summary, aes(x = means, xend = means, y = 0, yend = 0.5, color = Modality), show.legend = FALSE) +
    geom_point(data = df_summary, aes(x = means, y = 0.5, color = Modality), size = 8, show.legend = FALSE) +
    geom_point(data = response_data, aes(x = Alpha, y = prop_more, color = Modality, size = n_total), alpha = 0.6) +
    geom_text(data = df_summary, aes(x = -38, y = 0.85, label = paste("Threshold:", round(means, 2)), color = Modality), hjust = 0, show.legend = FALSE) +
    geom_text(data = df_summary, aes(x = -38, y = 0.78, label = paste("Slope:", round(sds, 2)), color = Modality), hjust = 0, show.legend = FALSE) +
    scale_color_manual(values = color_values) +
    scale_size_continuous(range = c(2, 10), guide = "none") +
    coord_cartesian(xlim = c(-40, 40), ylim = c(0, 1)) +
    labs(x = expression(paste("Intensity (", Delta, "BPM)")), y = "P(Response = More | Intensity)", color = NULL) +
    theme_minimal() +
    theme(panel.grid = element_blank(), axis.line = element_line(colour = "black"), legend.position = c(0.85, 0.15))
  return(p)
}

plot_reaction_time_cardio <- function(df) {
  col <- c("Correct" = "#5f9e6e", "Incorrect" = "#b55d60")
  df_long <- df %>%
    filter(!is.na(DecisionRT) & !is.na(ConfidenceRT)) %>%
    pivot_longer(cols = c(DecisionRT, ConfidenceRT), names_to = "RTType", values_to = "RT") %>%
    mutate(
      ResponseCorrect = factor(ifelse(ResponseCorrect == 1, "Correct", "Incorrect"), levels = c("Correct", "Incorrect")),
      RTType = factor(RTType, levels = c("DecisionRT", "ConfidenceRT"), labels = c("Decision", "Confidence"))
    )
  
  p <- ggplot(df_long, aes(y = RT, fill = ResponseCorrect)) +
    ggdist::stat_halfeye(aes(x = as.numeric(ResponseCorrect) - 1.5), adjust = 1.5, width = 0.6, .width = 0, justification = -0.2, alpha = 0.7) +
    geom_boxplot(aes(x = as.numeric(ResponseCorrect) - 1.5), width = 0.15, outlier.shape = NA, alpha = 0.5) +
    geom_point(aes(x = as.numeric(ResponseCorrect) - 1.5), position = position_jitter(width = 0.05), size = 1, alpha = 0.3) +
    facet_wrap(~RTType, scales = "free_x") +
    scale_fill_manual(values = col) +
    coord_flip() +
    labs(y = "Response Time (s)", x = "", fill = NULL) +
    theme_minimal() +
    theme(panel.grid = element_blank(), axis.line = element_line(colour = "black"), strip.text = element_text(face = "bold"), legend.position = "bottom", axis.text.y = element_blank(), axis.ticks.y = element_blank())
  return(p)
}

plot_confidence_histogram_cardio <- function(df) {
  df_clean <- df %>% filter(!is.na(Confidence) & !is.na(ResponseCorrect))
  if(nrow(df_clean) == 0) return(ggplot() + theme_void() + ggtitle("No confidence data"))
  
  df_clean$Confidence_bin <- cut(df_clean$Confidence, breaks = 4, labels = 1:4)
  conf_summary <- df_clean %>%
    mutate(ResponseCorrect = ifelse(ResponseCorrect == 1, "Correct", "Incorrect")) %>%
    group_by(Modality, Confidence_bin, ResponseCorrect) %>%
    summarise(n = n(), .groups = 'drop') %>%
    group_by(Modality, ResponseCorrect) %>%
    mutate(prop = n / sum(n)) %>%
    ungroup()
  
  p <- ggplot(conf_summary, aes(x = Confidence_bin, y = prop, fill = ResponseCorrect)) +
    geom_col(position = "dodge", color = "black", width = 0.7) +
    scale_fill_manual(values = c("Correct" = "#5f9e6e", "Incorrect" = "#b55d60")) +
    facet_wrap(~Modality) +
    labs(x = "Confidence Rating Bin", y = "P(Confidence | Outcome)", fill = NULL) +
    theme_minimal() +
    theme(panel.grid = element_blank(), axis.line = element_line(colour = "black"), strip.text = element_text(face = "bold"), legend.position = "bottom")
  return(p)
}

calculate_auroc_cardio <- function(df) {
  df_clean <- df %>% filter(!is.na(Confidence) & !is.na(ResponseCorrect))
  if(nrow(df_clean) < 2 || length(unique(df_clean$ResponseCorrect)) < 2) {
    return(list(plot = ggplot() + theme_void() + ggtitle("Not enough data for AUROC"), auc = NA))
  }
  
  m1 <- glm(ResponseCorrect ~ Confidence, data = df_clean, family = binomial(link = "logit"))
  roc_obj <- pROC::roc(df_clean$ResponseCorrect, fitted(m1), quiet = TRUE)
  auc_val <- as.numeric(pROC::auc(roc_obj))
  roc_df <- data.frame(sensitivity = roc_obj$sensitivities, specificity = roc_obj$specificities)
  
  plot_color <- if (unique(df$Modality) == "Intero") "#c44e52" else "#4c72b0"
  p1 <- ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
    geom_line(color = plot_color, linewidth = 1.2) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.5) +
    annotate("text", x = 0.7, y = 0.3, label = paste0("AUC = ", round(auc_val, 3)), size = 5) +
    labs(x = "1 - Specificity", y = "Sensitivity") +
    theme_minimal() +
    theme(panel.grid = element_blank(), axis.line = element_line(color = "black"))
  
  p2 <- ggplot(df_clean, aes(x = factor(ResponseCorrect), y = Confidence)) +
    geom_jitter(width = 0.2, alpha = 0.5, color = plot_color) +
    scale_x_discrete(labels = c("Incorrect", "Correct")) +
    labs(x = "Response", y = "Confidence") +
    theme_minimal() +
    theme(panel.grid = element_blank(), axis.line = element_line(color = "black"))
  
  return(list(plot = p2 + p1, auc = auc_val))
}

# --- Main Analysis Function ---

analyze_participant_cardio <- function(hrd_data, participant_id = "Unknown", save_plots = TRUE, output_dir) {
  modalities <- unique(hrd_data$Modality)
  cat(sprintf("       Found %d modalities: %s\n", length(modalities), paste(modalities, collapse = ", ")))
  participant_dir <- file.path(output_dir, participant_id)
  if (save_plots) dir.create(participant_dir, showWarnings = FALSE, recursive = TRUE)
  
  meta_results_all <- list()
  for (mod in modalities) {
    cat(sprintf("         - Analyzing %s...\n", mod))
    mod_data <- hrd_data %>% filter(Modality == mod)
    meta_results <- tryCatch(analyze_hrd_data(mod_data, plot_results = FALSE), error = function(e) NULL)
    if (!is.null(meta_results)) meta_results_all[[mod]] <- meta_results
  }
  
  p_psychometric <- plot_psychometric_cardio(hrd_data)
  p_rt <- plot_reaction_time_cardio(hrd_data)
  p_conf <- plot_confidence_histogram_cardio(hrd_data)
  p_dynamics <- plot_trial_dynamics(hrd_data, participant_id)
  
  auroc_plots <- list()
  auroc_values <- list()
  for (mod in modalities) {
    mod_data <- hrd_data %>% filter(Modality == mod)
    auroc_result <- calculate_auroc_cardio(mod_data)
    auroc_plots[[mod]] <- auroc_result$plot
    auroc_values[[mod]] <- auroc_result$auc
  }
  
  summary_text <- ""
  for (mod in modalities) {
    if (!is.null(meta_results_all[[mod]])) {
      res <- meta_results_all[[mod]]
      auc <- ifelse(is.na(auroc_values[[mod]]), res$auroc, auroc_values[[mod]])
      summary_text <- paste0(summary_text, sprintf(
        "\n%s:\nAccuracy: %.1f%%\nd' = %.2f, meta-d' = %.2f\nM-ratio = %.2f, AUROC = %.2f\n",
        mod, res$mean_accuracy * 100, res$d, res$metad, res$mratio, auc))
    }
  }
  p_summary <- ggplot() + annotate("text", x = 0, y = 0, label = trimws(summary_text), size = 4, hjust = 0, vjust = 1) + theme_void()
  
  composite_plot <- (p_rt + p_conf) / (p_psychometric + p_summary) +
    plot_annotation(title = sprintf("HRD Analysis - %s", participant_id), theme = theme(plot.title = element_text(face = "bold", size = 18)))
  
  if (save_plots) {
    ggsave(file.path(participant_dir, "composite_analysis.png"), composite_plot, width = 16, height = 12, dpi = 300)
    ggsave(file.path(participant_dir, "trial_dynamics.png"), p_dynamics, width = 8, height = 10)
    ggsave(file.path(participant_dir, "psychometric_curve.png"), p_psychometric, width = 8, height = 6)
    ggsave(file.path(participant_dir, "reaction_times.png"), p_rt, width = 8, height = 6)
    ggsave(file.path(participant_dir, "confidence_histogram.png"), p_conf, width = 8, height = 6)
    for (mod in modalities) {
      if(!is.null(auroc_plots[[mod]])) {
        ggsave(file.path(participant_dir, sprintf("auroc_analysis_%s.png", mod)), auroc_plots[[mod]], width = 10, height = 5)
      }
    }
  }
  
  final_summary <- NULL
  if(length(meta_results_all) > 0) {
    final_summary <- imap_dfr(meta_results_all, ~data.frame(
      participant_id = participant_id,
      modality = .y,
      auroc_logistic = auroc_values[[.y]],
      .x
    ))
  }
  return(final_summary)
}

# Main Batch Processing Function
analyze_all_participants_non_bayesian <- function(data_files, output_dir = "results/non_bayesian_analysis") {
  cat("\n📊 NON-BAYESIAN METACOGNITION & PSYCHOPHYSICS ANALYSIS\n")
  cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  all_results_list <- list()
  for (i in seq_along(data_files)) {
    file_path <- data_files[i]
    participant_id <- basename(dirname(file_path))
    if (participant_id %in% c(".", "data", "")) {
      participant_id <- gsub("HRD_final\\.txt$", "", basename(file_path))
    }
    cat(sprintf("[%d/%d] Processing %s\n", i, length(data_files), participant_id))
    
    hrd_data <- read_delim(file_path, delim = ",", show_col_types = FALSE)
    if (!"Modality" %in% names(hrd_data)) {
      cat("    ⚠️ No 'Modality' column found, skipping.\n")
      next
    }
    
    participant_summary <- analyze_participant_cardio(hrd_data, participant_id, output_dir = output_dir)
    if (!is.null(participant_summary)) {
      all_results_list[[participant_id]] <- participant_summary
    }
  }
  
  if (length(all_results_list) > 0) {
    summary_table <- do.call(rbind, all_results_list)
    write.csv(summary_table, file.path(output_dir, "summary_non_bayesian_analysis.csv"), row.names = FALSE)
    cat("\n✅ ANALYSIS COMPLETE!\n")
    cat(sprintf("   Results saved to: %s\n", output_dir))
  } else {
    cat("\n⚠️ No participants were successfully analyzed.\n")
  }
}
