library(ggplot2)
library(tidyverse)

#' Custom ggplot Theme
#'
#' A minimal, clean theme for consistent styling of all ggplots.
#'
#' @return A ggplot2 theme object.
theme_custom <- function() {
  theme_minimal(base_size = 10) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.title = element_text(face = "bold"),
      panel.grid.major = element_line(color = "gray80"),
      panel.grid.minor = element_line(color = "gray90"),
      legend.position = "top"
    )
}

#' Convert Character and Logical Columns to Factors
#'
#' This function converts all character and logical columns in a data frame
#' to factors.
#'
#' @param df A data frame or tibble.
#' @return The modified data frame with character and logical columns converted to factors.
make_factors <- function(df) {
  df[] <- lapply(df, function(col) {
    if (is.character(col) || is.logical(col)) {
      factor(col)
    } else {
      col
    }
  })
  df
}

#' Plot Alpha by Trial Number with Final Estimate Threshold
#'
#' Creates a scatter plot of Alpha values versus trial numbers and adds a horizontal
#' dashed line at the final estimate threshold (from the last trial).
#'
#' @param data A data frame containing 'nTrials', 'Alpha', and 'EstimatedThreshold' columns.
#' @return A ggplot object.
plot_trial_alpha <- function(data) {
  final_threshold <- tail(data$EstimatedThreshold, 1)
  ggplot(data, aes(x = nTrials, y = Alpha)) +
    geom_point(color = "blue") +
    geom_hline(yintercept = final_threshold, color = "red", linetype = "dashed") +
    labs(title = "Alpha by Trial Number", x = "Trial Number", y = "Alpha") +
    theme_custom()
}

#' Plot Confidence Histogram for Correct vs Incorrect Responses
#'
#' Creates a histogram of Confidence values, separated by whether the response was correct.
#'
#' @param data A data frame containing 'Confidence' and 'ResponseCorrect' columns.
#' @return A ggplot object.
plot_confidence_histogram <- function(data) {
  data$ResponseCorrectF <- factor(data$ResponseCorrect, levels = c(TRUE, FALSE),
                                  labels = c("Correct", "Incorrect"))
  ggplot(data, aes(x = Confidence, fill = ResponseCorrectF)) +
    geom_histogram(alpha = 0.5, position = "identity", bins = 20) +
    labs(title = "Confidence Histogram: Correct vs Incorrect",
         x = "Confidence", fill = "Response") +
    scale_fill_manual(values = c("blue", "red")) +
    scale_x_continuous(limits = c(0, 100)) +
    theme_custom()
}


process_hrd_data <- function(hrd_data, confbins) {
  # Store the original number of trials
  total_trials <- nrow(hrd_data)
  
  # Process the raw HRD data and bin confidence ratings
  processed_hrd_data <- hrd_data %>%
    select(nTrials, Condition, Decision, Confidence) %>% 
    mutate(
      Signal = if_else(Condition == "More", 1, 0),
      Response = if_else(Decision == "More", 1, 0),
      Confidence = as.numeric(Confidence)
    ) %>%
    filter(!is.na(Confidence)) %>%  # Remove trials with missing confidence
    mutate(ConfidenceBinned = bin_confidence_quantiles(Confidence, confbins)) %>% 
    ungroup()
  
  # Calculate and print number of dropped trials
  dropped_trials <- total_trials - nrow(processed_hrd_data)
  if (dropped_trials > 0) {
    message(sprintf("Warning: Dropped %d trials due to missing confidence values.", dropped_trials))
  } else {
    message("No trials dropped due to missing confidence values.")
  }
  
  return(processed_hrd_data)
}


