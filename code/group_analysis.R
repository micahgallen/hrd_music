#-----------------------------------------------------------------------
# GROUP-LEVEL ANALYSIS & PLOTTING SCRIPT
#
# This script loads the summary data from both the non-Bayesian and
# single-subject Bayesian analysis pipelines, combines them, and
# creates and saves separate, publication-quality plots for key metrics.
#-----------------------------------------------------------------------

## 1. SETUP: Load Libraries
#-----------------------------------------------------------------------
pacman::p_load(tidyverse, patchwork, here)


## 2. LOAD SUMMARY DATA
#-----------------------------------------------------------------------
cat("🔎 Loading summary data from previous analyses...\n")

# Define paths to the summary files
non_bayesian_file <- here("results", "non_bayesian_analysis", "summary_non_bayesian_analysis.csv")
bayesian_file <- here("results", "bayesian_psychophysics", "bayesian_summary.csv")

# Check if files exist
if (!file.exists(non_bayesian_file) || !file.exists(bayesian_file)) {
  stop("One or both summary CSV files not found. Please run both analysis pipelines first.")
}

# Read the CSV files
non_bayesian_data <- read_csv(non_bayesian_file, show_col_types = FALSE)
bayesian_data <- read_csv(bayesian_file, show_col_types = FALSE)

cat("✅ Data loaded successfully.\n")


## 3. COMBINE AND PREPARE DATA FOR PLOTTING
#-----------------------------------------------------------------------
cat("⚙️  Preparing data for plotting...\n")

# --- Prepare Non-Bayesian Data ---
# Select and rename key columns for clarity, now including accuracy and confidence
summary_non_bayesian <- non_bayesian_data %>%
  select(participant_id, modality, mratio, d, metad, auroc_logistic, mean_accuracy, mean_confidence, estimated_threshold, estimated_slope) %>%
  rename(
    threshold = estimated_threshold,
    slope = estimated_slope,
    auroc = auroc_logistic,
    accuracy = mean_accuracy # Add accuracy for plotting
  ) %>%
  mutate(analysis_type = "Non-Bayesian")

# --- Prepare Bayesian Data ---
# Select and rename key columns
summary_bayesian <- bayesian_data %>%
  select(participant_id, modality, alpha_mean, beta_mean) %>%
  rename(
    threshold = alpha_mean,
    slope = beta_mean
  ) %>%
  mutate(analysis_type = "Bayesian")

# --- Combine Data for Summary Plots ---
# Combine the two dataframes, creating a long-format dataset suitable for ggplot
combined_summary_long <- bind_rows(
  summary_non_bayesian,
  summary_bayesian
) %>%
  pivot_longer(
    cols = c(mratio, d, metad, auroc, accuracy, mean_confidence, threshold, slope),
    names_to = "metric",
    values_to = "value",
    values_drop_na = TRUE # Exclude rows where the metric doesn't apply
  )

# --- Prepare Data for Scatter Plots ---
# Create a wide-format dataframe to compare Bayesian and Non-Bayesian estimates directly
comparison_data <- full_join(
  summary_non_bayesian %>% select(participant_id, modality, threshold, slope),
  summary_bayesian %>% select(participant_id, modality, threshold, slope),
  by = c("participant_id", "modality"),
  suffix = c("_non_bayesian", "_bayesian")
)

cat("✅ Data prepared.\n")


## 4. CREATE AND SAVE PLOTS
#-----------------------------------------------------------------------
cat("📊 Generating and saving individual group-level plots...\n")

# --- Function for Bar Plots with SEM and Jitter ---
create_summary_barplot <- function(data, metrics_to_plot, plot_title, plot_subtitle, show_analysis_type = TRUE) {
  
  plot_data <- data %>% 
    filter(metric %in% metrics_to_plot) %>%
    group_by(analysis_type, modality, metric) %>%
    summarise(
      mean_val = mean(value, na.rm = TRUE),
      sem = sd(value, na.rm = TRUE) / sqrt(n()),
      .groups = 'drop'
    )
  
  raw_points <- data %>% filter(metric %in% metrics_to_plot)
  
  # Custom labels for facets
  facet_labels <- c(
    d = "d-prime", 
    metad = "meta-d'", 
    mratio = "M-Ratio", 
    auroc = "AUROC", 
    accuracy = "Accuracy", 
    mean_confidence = "Mean Confidence",
    threshold = "Threshold (Alpha)",
    slope = "Slope (Beta)"
  )
  
  p <- ggplot() +
    # Bar plot for mean
    geom_bar(data = plot_data, aes(x = modality, y = mean_val, fill = if(show_analysis_type) analysis_type else modality), 
             stat = "identity", position = position_dodge(width = 0.9), alpha = 0.7) +
    # SEM error bars
    geom_errorbar(data = plot_data, aes(x = modality, ymin = mean_val - sem, ymax = mean_val + sem, group = if(show_analysis_type) analysis_type else modality),
                  position = position_dodge(width = 0.9), width = 0.2, linewidth = 0.7) +
    # Raw data points
    geom_jitter(data = raw_points, aes(x = modality, y = value, color = if(show_analysis_type) analysis_type else modality),
                position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.9, seed=1), alpha = 0.6) +
    # Facet by metric to create separate panels
    facet_wrap(~metric, scales = "free_y", labeller = as_labeller(facet_labels)) +
    labs(
      title = plot_title,
      subtitle = plot_subtitle,
      x = "Modality",
      y = "Value"
    ) +
    theme_classic(base_size = 14) +
    theme(
      legend.position = "bottom",
      strip.text = element_text(face = "bold", size = 12),
      plot.title = element_text(face="bold", hjust=0.5),
      plot.subtitle = element_text(hjust=0.5, color="gray40"),
      strip.background = element_rect(fill="gray90", color="white"),
      strip.text.x = element_text(color="white")
    )
  
  if(show_analysis_type) {
    p <- p + scale_fill_manual(values = c("Non-Bayesian" = "#7570b3", "Bayesian" = "#1b9e77"), name = "Analysis Type") +
      scale_color_manual(values = c("Non-Bayesian" = "#7570b3", "Bayesian" = "#1b9e77"), name = "Analysis Type")
  } else {
    p <- p + scale_fill_manual(values = c("Extero" = "#4c72b0", "Intero" = "#c44e52"), name = "Modality") +
      scale_color_manual(values = c("Extero" = "#4c72b0", "Intero" = "#c44e52"), name = "Modality")
  }
  
  return(p)
}

# --- Function for Comparison Scatter Plots ---
create_scatter_plot <- function(data, metric_to_plot) {
  x_var <- paste0(metric_to_plot, "_non_bayesian")
  y_var <- paste0(metric_to_plot, "_bayesian")
  
  ggplot(data, aes_string(x = x_var, y = y_var)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
    geom_smooth(method = "lm", color = "black", linewidth = 0.7, se = FALSE) +
    geom_point(aes(color = modality, shape = modality), size = 4, alpha = 0.8) +
    scale_color_manual(values = c("Extero" = "#4c72b0", "Intero" = "#c44e52")) +
    scale_shape_manual(values = c("Extero" = 16, "Intero" = 17)) +
    labs(
      title = paste("Comparison of", str_to_title(metric_to_plot), "Estimates"),
      subtitle = "Non-Bayesian vs. Bayesian Analysis",
      x = paste("Non-Bayesian", str_to_title(metric_to_plot)),
      y = paste("Bayesian", str_to_title(metric_to_plot)),
      color = "Modality",
      shape = "Modality"
    ) +
    theme_minimal(base_size = 14) +
    theme(aspect.ratio = 1,
          plot.title = element_text(face="bold", hjust=0.5),
          plot.subtitle = element_text(hjust=0.5, color="gray40"),
          legend.position = "bottom")
}


# --- Create and save all plots ---
# Create a dedicated folder for group plots
group_plot_dir <- here("results", "group_plots")
dir.create(group_plot_dir, showWarnings = FALSE)

# Plot 1: Metacognitive & Performance Metrics
plot_metacognition <- create_summary_barplot(
  combined_summary_long, 
  c("mratio", "d", "metad", "auroc", "accuracy", "mean_confidence"),
  "Group-Level Metacognition & Performance",
  "Metacognitive metrics, AUROC, accuracy, and mean confidence by modality",
  show_analysis_type = FALSE
)

# Plot 2: Psychometric Model Parameters
plot_parameters <- create_summary_barplot(
  combined_summary_long %>% filter(metric %in% c("threshold", "slope")),
  c("threshold", "slope"),
  "Group-Level Psychometric Model Parameters",
  "Comparing Non-Bayesian and Bayesian estimates across modalities"
)

# Plot 3: Comparison Scatter Plots
scatter_threshold <- create_scatter_plot(comparison_data, "threshold")
scatter_slope <- create_scatter_plot(comparison_data, "slope")
plot_comparisons <- (scatter_threshold + scatter_slope) + 
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

# Save the plots
ggsave(file.path(group_plot_dir, "group_metacognition_and_performance.png"), plot_metacognition, width = 15, height = 10, dpi = 150, bg = "white")
ggsave(file.path(group_plot_dir, "group_psychometric_parameters.png"), plot_parameters, width = 12, height = 7, dpi = 150, bg = "white")
ggsave(file.path(group_plot_dir, "comparison_scatter_plots.png"), plot_comparisons, width = 12, height = 7, dpi = 150, bg = "white")


## 5. COMPLETE
#-----------------------------------------------------------------------
cat(sprintf("\n✅ All group-level plots have been saved to: %s\n", group_plot_dir))
cat("\n🎉 Pipeline finished!\n")
