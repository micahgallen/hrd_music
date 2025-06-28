# Load required libraries
library(readr)
library(patchwork)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(magrittr)
library(rjags)
library(coda)
library(ggmcmc)

# Source helper functions
source("code/hrd_analysis/helper_functions.R")
source("code/hrd_analysis/trials2counts.R")
source("code/hrd_analysis/fit_metad_indiv.R")
source("code/hrd_analysis/calc_auroc2.R")

analyze_hrd_data <- function(hrd_data, nRatings = 4, plot_results = TRUE, show_traceplot = TRUE, participant_id = "unknown") {
  
  # Ensure factors are correctly formatted
  hrd_data <- make_factors(hrd_data)
  
  # Process HRD data with the specified number of rating bins
  processed_hrd_data <- process_hrd_data(hrd_data, nRatings)
  
  # Ensure figs/ directory exists
  if (!dir.exists("results/plots")) {
    dir.create("results/plots", recursive = TRUE)
  }
  
  # Generate and display plots if requested
  if (plot_results) {
    confidence_hist <- plot_confidence_histogram(hrd_data)
    trial_alpha_plot <- plot_trial_alpha(hrd_data)
    combined_plot <- confidence_hist + trial_alpha_plot
    
    print(combined_plot)
    
    # Save figure
    ggsave(filename = sprintf("results/plots/%s_confidence_trial_alpha.png", participant_id), plot = combined_plot, width = 8, height = 4, dpi = 150)
  }
  
  # Extract inputs for trials2counts
  stimID <- processed_hrd_data$Signal
  response <- processed_hrd_data$Response
  rating <- processed_hrd_data$ConfidenceBinned
  
  # Remove NA values before trials2counts
  valid_trials <- complete.cases(stimID, response, rating)
  stimID <- stimID[valid_trials]
  response <- response[valid_trials]
  rating <- rating[valid_trials]
  
  # Convert trials to counts
  counts <- trials2counts(stimID, response, rating, nRatings)
  nR_S1 <- counts[[1]]
  nR_S2 <- counts[[2]]
  
  # Fit the metacognitive model
  fit <- fit_metad_indiv(nR_S1, nR_S2)
  auroc <- calc_auroc2(nR_S1, nR_S2, 3)
  
  # Extract model output
  output <- fit[[1]]
  d1 <- fit[[2]]$d1
  
  # Compute mean statistics
  Value <- summary(output)
  stat <- data.frame(mean = Value[["statistics"]][, "Mean"]) %>%
    rownames_to_column(var = "name")
  
  # Compute convergence statistics (Rhat)
  Rhat <- data.frame(conv = gelman.diag(output, confidence = 0.95)$psrf)
  
  # Compute HDI
  HDI <- data.frame(HPDinterval(output, prob = 0.95)) %>%
    rownames_to_column(var = "name")
  
  # Merge all values into a single dataframe
  Fit <- stat %>%
    cbind(lower = HDI$lower, upper = HDI$upper)
  
  # Compute metacognitive ratio
  metad <- stat$mean[stat$name == "meta_d"]
  mratio <- metad / d1
  
  # Compute mean confidence
  mean_confidence <- mean(hrd_data$Confidence, na.rm = TRUE)
  
  # Ensure ResponseCorrect is numeric
  hrd_data$ResponseCorrect <- as.numeric(hrd_data$ResponseCorrect) - 1
  
  # Compute mean accuracy
  mean_accuracy <- mean(hrd_data$ResponseCorrect, na.rm = TRUE)
  
  # Extract final EstimatedThreshold and EstimatedSlope values
  estimated_threshold <- tail(hrd_data$EstimatedThreshold, 1)
  estimated_slope <- tail(hrd_data$EstimatedSlope, 1)
  
  # Print results
  message(sprintf("Metacognition scores: AUROC = %.2f, MRatio = %.2f, Mean Confidence = %.2f", auroc, mratio, mean_confidence))
  
  # Show traceplot if requested and save it
  if (show_traceplot) {
    png(file = sprintf("results/plots/%s_traceplot.png", participant_id), width = 6, height = 4, units = "in", res = 300, bg = "white")
    traceplot(output)
    dev.off()
  }
  
  # Generate posterior distribution plot
  mcmc.sample <- ggs(output)
  post_plot <- mcmc.sample %>%
    filter(Parameter == "meta_d") %>% 
    ggplot(aes(value)) +
    geom_histogram(binwidth = 0.03, fill = "blue", colour = "grey", alpha = 0.5) +
    geom_vline(xintercept = stat$mean[stat$name == "meta_d"], linetype = "dashed", linewidth = 1.5) +
    annotate("segment", x = HDI$lower[HDI$name == "meta_d"], y = 50, 
             xend = HDI$upper[HDI$name == "meta_d"], yend = 50, colour = "white", linewidth = 2.5) +
    ylab("Sample count") +
    xlab(expression(paste("Meta d'")))
  
  if (plot_results) {
    print(post_plot)
    ggsave(filename = sprintf("results/plots/%s_posterior_meta_d.png", participant_id), plot = post_plot, width = 6, height = 4, dpi = 150)
  }
  
  # Return results as a dataframe
  results_df <- data.frame(
    mean_confidence = mean_confidence,
    estimated_threshold = estimated_threshold,
    estimated_slope = estimated_slope,
    mean_accuracy = mean_accuracy,
    auroc = auroc,
    d = d1,
    metad = metad,
    mratio = mratio
  )
  
  return(results_df)
}


# Example usage:
# hrd_data <- read_delim("path/to/data.txt", delim = ",")
# results <- analyze_hrd_data(hrd_data, nRatings = 4, plot_results = TRUE, show_traceplot = TRUE)
# print(results)
