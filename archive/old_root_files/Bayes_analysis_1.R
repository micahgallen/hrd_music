#-----------------------------------------------------------------------
# BAYESIAN PSYCHOMETRIC FUNCTION ANALYSIS
#
# Optional extension for fitting Bayesian psychometric functions
# Requires cmdstanr and Stan to be installed
#-----------------------------------------------------------------------

# Check if cmdstanr is available
if (!requireNamespace("cmdstanr", quietly = TRUE)) {
  stop("cmdstanr is required for Bayesian analysis. See setup_stan_models.R for installation instructions.")
}

library(cmdstanr)
library(posterior)
library(bayesplot)

# Function to prepare data for Stan
prepare_stan_data <- function(df) {
  # Remove NA values
  df_clean <- df %>%
    filter(!is.na(Alpha) & !is.na(Decision)) %>%
    mutate(
      response = ifelse(Decision == "More", 1, 0)
    )
  
  # Create Stan data list
  stan_data <- list(
    N = nrow(df_clean),
    x = df_clean$Alpha,
    y = df_clean$response,
    run_estimation = 1
  )
  
  return(stan_data)
}

# Function to fit Bayesian psychometric function
fit_bayesian_psychometric <- function(df, participant_id = "Unknown", 
                                      iter_sampling = 2000, chains = 4) {
  
  cat(sprintf("  Fitting Bayesian psychometric function for %s...\n", participant_id))
  
  # Compile model (only done once)
  model_file <- "code/stan_models/Standard_Cumulative_Normal.stan"
  
  if (!file.exists(model_file)) {
    stop("Stan model file not found. Run setup_stan_models.R first.")
  }
  
  # Compile model
  model <- cmdstan_model(model_file)
  
  # Prepare data
  stan_data <- prepare_stan_data(df)
  
  # Fit model
  fit <- model$sample(
    data = stan_data,
    iter_sampling = iter_sampling,
    iter_warmup = iter_sampling,
    chains = chains,
    parallel_chains = chains,
    refresh = 0,
    show_messages = FALSE
  )
  
  # Extract parameters
  draws <- fit$draws(variables = c("alpha", "beta", "lapse"))
  summary <- fit$summary(variables = c("alpha", "beta", "lapse"))
  
  # Create diagnostic plots
  # 1. Trace plots
  trace_plot <- mcmc_trace(draws, pars = c("alpha", "beta", "lapse")) +
    labs(title = "MCMC Trace Plots")
  
  # 2. Posterior distributions
  posterior_plot <- mcmc_areas(draws, pars = c("alpha", "beta", "lapse"),
                               prob = 0.95) +
    labs(title = "Posterior Distributions")
  
  # 3. Posterior predictive check
  alpha_samples <- as.vector(draws[,,"alpha"])
  beta_samples <- as.vector(draws[,,"beta"])
  lapse_samples <- as.vector(draws[,,"lapse"])
  
  # Sample from posterior
  n_samples <- min(100, length(alpha_samples))
  sample_idx <- sample(length(alpha_samples), n_samples)
  
  # Create predicted curves
  x_pred <- seq(min(stan_data$x), max(stan_data$x), length.out = 100)
  y_pred <- matrix(NA, nrow = n_samples, ncol = length(x_pred))
  
  for (i in 1:n_samples) {
    idx <- sample_idx[i]
    y_pred[i,] <- lapse_samples[idx]/2 + (1 - lapse_samples[idx]) * 
      pnorm((x_pred - alpha_samples[idx]) * beta_samples[idx])
  }
  
  # Plot posterior predictive
  ppc_data <- df %>%
    filter(!is.na(Alpha) & !is.na(Decision)) %>%
    group_by(Alpha) %>%
    summarise(
      prop_more = mean(Decision == "More"),
      n = n()
    )
  
  ppc_plot <- ggplot() +
    # Posterior predictive curves
    apply(y_pred, 1, function(y) {
      geom_line(aes(x = x_pred, y = y), alpha = 0.1, color = "blue")
    }) +
    # Data points
    geom_point(data = ppc_data, aes(x = Alpha, y = prop_more, size = n),
               alpha = 0.8) +
    # Mean posterior curve
    geom_line(aes(x = x_pred, y = colMeans(y_pred)), 
              color = "red", size = 1.2) +
    labs(
      title = "Posterior Predictive Check",
      x = "Stimulus Intensity (Alpha)",
      y = "P(More)"
    ) +
    theme_minimal() +
    scale_size_continuous(range = c(2, 6))
  
  # Combine all Bayesian plots
  bayes_plot <- (trace_plot / posterior_plot / ppc_plot) +
    plot_annotation(
      title = sprintf("Bayesian Psychometric Analysis - %s", participant_id),
      theme = theme(plot.title = element_text(face = "bold", hjust = 0.5))
    )
  
  # Return results
  return(list(
    fit = fit,
    summary = summary,
    plots = list(
      trace = trace_plot,
      posterior = posterior_plot,
      ppc = ppc_plot,
      combined = bayes_plot
    ),
    estimates = list(
      alpha = summary$mean[summary$variable == "alpha"],
      beta = summary$mean[summary$variable == "beta"],
      lapse = summary$mean[summary$variable == "lapse"]
    )
  ))
}

# Function to run full Bayesian psychophysics analysis
run_bayesian_psychophysics <- function(data_files, output_dir = "results/bayesian_psychophysics") {
  
  cat("\n🧮 BAYESIAN PSYCHOMETRIC FUNCTION ANALYSIS\n")
  cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")
  
  # Create output directory
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Store results
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
    
    # Try Bayesian fitting
    bayes_result <- tryCatch({
      fit_bayesian_psychometric(hrd_data, participant_id)
    }, error = function(e) {
      cat(sprintf("  ⚠️  Bayesian fitting failed: %s\n", e$message))
      NULL
    })
    
    if (!is.null(bayes_result)) {
      # Save plots
      participant_dir <- file.path(output_dir, participant_id)
      dir.create(participant_dir, showWarnings = FALSE)
      
      ggsave(file.path(participant_dir, "bayesian_analysis.png"),
             bayes_result$plots$combined,
             width = 10, height = 12, dpi = 300)
      
      # Store results
      all_results[[participant_id]] <- data.frame(
        participant_id = participant_id,
        alpha_mean = bayes_result$estimates$alpha,
        beta_mean = bayes_result$estimates$beta,
        lapse_mean = bayes_result$estimates$lapse,
        alpha_sd = bayes_result$summary$sd[bayes_result$summary$variable == "alpha"],
        beta_sd = bayes_result$summary$sd[bayes_result$summary$variable == "beta"],
        lapse_sd = bayes_result$summary$sd[bayes_result$summary$variable == "lapse"],
        rhat_alpha = bayes_result$summary$rhat[bayes_result$summary$variable == "alpha"],
        rhat_beta = bayes_result$summary$rhat[bayes_result$summary$variable == "beta"],
        rhat_lapse = bayes_result$summary$rhat[bayes_result$summary$variable == "lapse"]
      )
      
      cat("  ✓ Bayesian analysis complete\n")
    }
  }
  
  # Save summary results
  if (length(all_results) > 0) {
    summary_table <- do.call(rbind, all_results)
    write.csv(summary_table, file.path(output_dir, "bayesian_summary.csv"), row.names = FALSE)
    
    cat("\n✅ BAYESIAN ANALYSIS COMPLETE!\n")
    cat(sprintf("   Successfully analyzed %d participants\n", nrow(summary_table)))
    cat(sprintf("   Results saved to: %s\n", output_dir))
  }
  
  return(all_results)
}

# Example usage:
# data_files <- list.files("data", pattern = "HRD_final\\.txt$", recursive = TRUE, full.names = TRUE)
# bayes_results <- run_bayesian_psychophysics(data_files)