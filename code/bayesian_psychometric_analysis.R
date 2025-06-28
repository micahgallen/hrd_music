#-----------------------------------------------------------------------
# BAYESIAN PSYCHOMETRIC FUNCTION ANALYSIS (MODIFIED FOR MODALITIES)
#
# This script has been updated to handle multiple modalities (e.g.,
# Intero and Extero) within a single participant's data file,
# replicating the behavior of the project tutorials.
#-----------------------------------------------------------------------

# Load required packages
pacman::p_load(cmdstanr, posterior, bayesplot, tidyverse, patchwork)

# Function to prepare data for Stan
prepare_stan_data <- function(df) {
  df_grouped <- df %>%
    filter(!is.na(Alpha) & !is.na(Decision)) %>%
    group_by(Alpha) %>%
    summarize(
      x = mean(Alpha),
      resp = sum(Decision == "More"),
      n = n(),
      .groups = 'drop'
    ) %>%
    arrange(Alpha)
  
  stan_data <- list(
    N = nrow(df_grouped),
    n = df_grouped$n,
    y = df_grouped$resp,
    x = df_grouped$x # Use aggregated x values
  )
  return(stan_data)
}

# Function to fit Bayesian psychometric function
fit_bayesian_psychometric <- function(df, participant_id = "Unknown", modality = "Unknown",
                                      iter_sampling = 2000, chains = 4) {
  
  cat(sprintf("    - Fitting model for modality: %s\n", modality))
  
  model_file <- "code/stan_models/Standard_Cumulative_Normal.stan"
  
  if (!file.exists(model_file)) {
    stop("Stan model file not found. Make sure it is at: ", model_file)
  }
  
  model <- cmdstan_model(model_file)
  stan_data <- prepare_stan_data(df)
  
  fit <- tryCatch({
    model$sample(
      data = stan_data,
      iter_sampling = iter_sampling,
      iter_warmup = 1000,
      chains = chains,
      parallel_chains = chains,
      refresh = 0,
      show_messages = FALSE
    )
  }, error = function(e) {
    cat(sprintf("    ⚠️ Error during sampling for %s: %s\n", modality, e$message))
    return(NULL)
  })
  
  if (is.null(fit)) return(NULL)
  
  # --- PLOTTING ---
  draws <- fit$draws(variables = c("alpha", "beta"))
  summary <- fit$summary(variables = c("alpha", "beta"))
  
  # Set plot colors based on modality
  plot_color <- if (modality == "Intero") "#c44e52" else "#4c72b0"
  color_scheme_set(plot_color)
  
  # Diagnostic plots
  trace_plot <- mcmc_trace(draws) + labs(title = "Trace Plots")
  posterior_plot <- mcmc_areas(draws, prob = 0.95) + labs(title = "Posterior Distributions")
  
  # Posterior predictive plot (Tutorial Style)
  ppc_data <- data.frame(Alpha = stan_data$x, prop_more = stan_data$y / stan_data$n, n = stan_data$n)
  x_pred <- seq(min(stan_data$x), max(stan_data$x), length.out = 100)
  alpha_samples <- as.vector(draws[, , "alpha"])
  beta_samples <- as.vector(draws[, , "beta"])
  
  # Create the base plot
  ppc_plot <- ggplot(ppc_data, aes(x = Alpha, y = prop_more)) +
    labs(
      title = "Data and Model Fit",
      subtitle = sprintf("Modality: %s", modality),
      x = "Stimulus Intensity (Alpha)",
      y = "P(More)"
    ) +
    theme_minimal() +
    coord_cartesian(xlim = c(-35, 35))
  
  # Add multiple posterior samples as faint lines
  n_lines <- min(100, length(alpha_samples))
  for (i in sample(length(alpha_samples), n_lines)) {
    y_pred_sample <- pnorm((x_pred - alpha_samples[i]) * beta_samples[i])
    pred_df_sample <- data.frame(x = x_pred, y = y_pred_sample)
    ppc_plot <- ppc_plot +
      geom_line(data = pred_df_sample, aes(x = x, y = y), alpha = 0.1, color = plot_color, inherit.aes = FALSE)
  }
  
  # Add mean posterior curve as a bold line
  mean_alpha <- mean(alpha_samples)
  mean_beta <- mean(beta_samples)
  y_pred_mean <- pnorm((x_pred - mean_alpha) * mean_beta)
  mean_pred_df <- data.frame(x = x_pred, y = y_pred_mean)
  ppc_plot <- ppc_plot +
    geom_line(data = mean_pred_df, aes(x = x, y = y), color = plot_color, linewidth = 1.5, inherit.aes = FALSE) +
    geom_point(aes(size = n), alpha = 0.8, color = plot_color) +
    scale_size_continuous(range = c(2, 8))
  
  # Combine into a final plot with diagnostics
  combined_plot <- (posterior_plot + trace_plot) / ppc_plot +
    plot_annotation(
      title = sprintf("Bayesian Analysis: %s - %s", participant_id, modality),
      theme = theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 16))
    )
  
  return(list(fit = fit, summary = summary, plot = combined_plot))
}


# Main function to loop through files and modalities
run_bayesian_psychophysics <- function(data_files, output_dir = "results/bayesian_psychophysics") {
  cat("\n🧮 BAYESIAN PSYCHOMETRIC FUNCTION ANALYSIS\n")
  cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")
  
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  all_results <- list()
  
  for (i in seq_along(data_files)) {
    file_path <- data_files[i]
    participant_id <- basename(dirname(file_path))
    if (participant_id %in% c(".", "data", "")) {
      participant_id <- gsub("HRD_final\\.txt$", "", basename(file_path))
    }
    
    cat(sprintf("[%d/%d] Processing %s\n", i, length(data_files), participant_id))
    hrd_data <- read_delim(file_path, delim = ",", show_col_types = FALSE)
    
    # Find the unique modalities in this participant's file
    modalities <- unique(hrd_data$Modality)
    if (length(modalities) == 0) {
      cat("    ⚠️ No 'Modality' column found, skipping.\n")
      next
    }
    
    participant_dir <- file.path(output_dir, participant_id)
    dir.create(participant_dir, showWarnings = FALSE)
    
    # --- NEW: Loop through each modality ---
    for (mod in modalities) {
      mod_data <- hrd_data %>% filter(Modality == mod)
      
      bayes_result <- fit_bayesian_psychometric(mod_data, participant_id, modality = mod)
      
      if (!is.null(bayes_result) && !is.null(bayes_result$fit)) {
        # Save the combined plot for this modality
        ggsave(file.path(participant_dir, sprintf("bayesian_analysis_%s.png", mod)),
               bayes_result$plot,
               width = 12, height = 10, dpi = 300)
        
        # Store results for final summary table
        result_row <- data.frame(
          participant_id = participant_id,
          modality = mod,
          alpha_mean = bayes_result$summary$mean[bayes_result$summary$variable == "alpha"],
          beta_mean = bayes_result$summary$mean[bayes_result$summary$variable == "beta"],
          alpha_sd = bayes_result$summary$sd[bayes_result$summary$variable == "alpha"],
          beta_sd = bayes_result$summary$sd[bayes_result$summary$variable == "beta"],
          rhat_alpha = bayes_result$summary$rhat[bayes_result$summary$variable == "alpha"],
          rhat_beta = bayes_result$summary$rhat[bayes_result$summary$variable == "beta"]
        )
        all_results[[paste(participant_id, mod)]] <- result_row
      }
    }
  }
  
  # Save combined summary results
  if (length(all_results) > 0) {
    summary_table <- do.call(rbind, all_results)
    write.csv(summary_table, file.path(output_dir, "bayesian_summary.csv"), row.names = FALSE)
    cat("\n✅ BAYESIAN ANALYSIS COMPLETE!\n")
    cat(sprintf("   Successfully analyzed %d participant-modality combinations.\n", nrow(summary_table)))
    cat(sprintf("   Results saved to: %s\n", output_dir))
  } else {
    cat("\n⚠️ No participants were successfully analyzed.\n")
  }
  
  return(invisible(all_results))
}
