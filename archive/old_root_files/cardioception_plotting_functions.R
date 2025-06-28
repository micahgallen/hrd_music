#-----------------------------------------------------------------------
# CARDIOCEPTION-STYLE PLOTTING FUNCTIONS
# Beautiful psychophysics plots adapted from Cardioception package
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
plot_psychometric_cardio <- function(df) {
  # Get the estimated threshold and slope for each modality
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
  
  # Set colors - always use both colors if we have both modalities
  n_modalities <- length(unique(df_summary$Modality))
  if (n_modalities == 2) {
    color_values <- c("Extero" = "#4c72b0", "Intero" = "#c44e52")
  } else if (unique(df_summary$Modality) == "Intero") {
    color_values <- c("Intero" = "#c44e52")
  } else {
    color_values <- c("Extero" = "#4c72b0")
  }
  
  # Set y positions for text annotations
  y_positions <- list(
    "Extero" = c(0.85, 0.80),
    "Intero" = c(0.75, 0.70)
  )
  
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
    # Text annotations for each modality
    {if("Extero" %in% df_summary$Modality) 
      geom_text(data = df_summary %>% filter(Modality == "Extero"),
                aes(x = -20, y = y_positions[["Extero"]][1],
                    label = paste("Extero Threshold:", round(means, 2)),
                    color = Modality),
                hjust = 0, show.legend = FALSE)} +
    {if("Extero" %in% df_summary$Modality)
      geom_text(data = df_summary %>% filter(Modality == "Extero"),
                aes(x = -20, y = y_positions[["Extero"]][2],
                    label = paste("Extero Slope:", round(sds, 2)),
                    color = Modality),
                hjust = 0, show.legend = FALSE)} +
    {if("Intero" %in% df_summary$Modality)
      geom_text(data = df_summary %>% filter(Modality == "Intero"),
                aes(x = -20, y = y_positions[["Intero"]][1],
                    label = paste("Intero Threshold:", round(means, 2)),
                    color = Modality),
                hjust = 0, show.legend = FALSE)} +
    {if("Intero" %in% df_summary$Modality)
      geom_text(data = df_summary %>% filter(Modality == "Intero"),
                aes(x = -20, y = y_positions[["Intero"]][2],
                    label = paste("Intero Slope:", round(sds, 2)),
                    color = Modality),
                hjust = 0, show.legend = FALSE)} +
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
      legend.position.inside = c(0.85, 0.15),
      legend.key.size = unit(1, "cm"),
      legend.background = element_rect(fill = "white", color = NA),
      text = element_text(size = 12),
      axis.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 12)
    )
  
  return(p)
}

# Reaction time plot (Cardioception style)
plot_reaction_time_cardio <- function(df) {
  # Set up parameters
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

# Confidence histogram (Cardioception style) - fixed version
plot_confidence_histogram_cardio <- function(df) {
  if (!"Modality" %in% names(df)) {
    df$Modality <- "Intero"
  }
  
  # Remove NA confidence values first
  df_clean <- df %>%
    filter(!is.na(Confidence) & !is.na(ResponseCorrect))
  
  if (nrow(df_clean) < 10) {
    warning("Too few complete cases for confidence histogram")
    return(ggplot() + theme_void())
  }
  
  # Bin confidence ratings with better error handling
  tryCatch({
    df_clean$Confidence_bin <- discretebins(df_clean, 4)[[1]]
  }, error = function(e) {
    # Fallback to simple quantile binning
    df_clean$Confidence_bin <- cut(df_clean$Confidence,
                                   breaks = quantile(df_clean$Confidence, 
                                                     probs = seq(0, 1, length.out = 5)),
                                   include.lowest = TRUE,
                                   labels = 1:4)
    df_clean$Confidence_bin <- as.numeric(df_clean$Confidence_bin)
  })
  
  # Remove any remaining NAs from binning
  df_clean <- df_clean %>%
    filter(!is.na(Confidence_bin))
  
  # Calculate proportions for correct/incorrect
  conf_summary <- df_clean %>%
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

# AUROC analysis (fixed version)
calculate_auroc_cardio <- function(df) {
  if (!"Modality" %in% names(df)) {
    df$Modality <- "Intero"
  }
  
  # Remove NA values and ensure we have the right data types
  df_clean <- df %>%
    filter(!is.na(Confidence) & !is.na(ResponseCorrect)) %>%
    mutate(
      Confidence = as.numeric(Confidence),
      ResponseCorrect = as.numeric(ResponseCorrect)
    )
  
  # Check if we have enough data and variability
  if (nrow(df_clean) < 10) {
    warning("Too few complete cases for AUROC calculation")
    return(list(plot = ggplot() + theme_void(), auc = NA))
  }
  
  if (length(unique(df_clean$ResponseCorrect)) < 2) {
    warning("No variability in responses for AUROC calculation")
    return(list(plot = ggplot() + theme_void(), auc = NA))
  }
  
  # Calculate AUROC using confidence binning for better stability
  df_clean$Confidence_bin <- cut(df_clean$Confidence, 
                                 breaks = quantile(df_clean$Confidence, 
                                                   probs = seq(0, 1, length.out = 5),
                                                   na.rm = TRUE),
                                 include.lowest = TRUE,
                                 labels = 1:4)
  df_clean$Confidence_bin <- as.numeric(df_clean$Confidence_bin)
  
  # Use binned confidence for model if continuous fails
  tryCatch({
    # Try continuous first
    m1 <- glm(ResponseCorrect ~ Confidence, 
              data = df_clean, 
              family = binomial(link = "logit"))
    
    # Check if model converged
    if (!m1$converged) {
      # Use binned confidence instead
      m1 <- glm(ResponseCorrect ~ Confidence_bin, 
                data = df_clean, 
                family = binomial(link = "logit"))
    }
    
    # Calculate ROC
    roc_obj <- pROC::roc(df_clean$ResponseCorrect, 
                         fitted(m1), 
                         quiet = TRUE)
    auc_val <- as.numeric(pROC::auc(roc_obj))
    
    # Get ROC curve data
    roc_df <- data.frame(
      sensitivity = roc_obj$sensitivities,
      specificity = roc_obj$specificities
    )
    
  }, error = function(e) {
    warning(sprintf("GLM failed, using simple AUROC: %s", e$message))
    # Fallback to simple AUROC calculation
    roc_obj <- pROC::roc(df_clean$ResponseCorrect, 
                         df_clean$Confidence, 
                         quiet = TRUE)
    auc_val <- as.numeric(pROC::auc(roc_obj))
    roc_df <- data.frame(
      sensitivity = roc_obj$sensitivities,
      specificity = roc_obj$specificities
    )
  })
  
  # Set colors
  modality_val <- unique(df$Modality)[1]
  if (modality_val == "Intero") {
    color_value <- "#c44e52"
  } else {
    color_value <- "#4c72b0"
  }
  
  # Create AUROC plot
  p1 <- ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
    geom_line(color = color_value, size = 1.2) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.5) +
    annotate("text", x = 0.7, y = 0.3, 
             label = paste0("AUC = ", round(auc_val, 3)),
             size = 5) +
    labs(
      x = "1 - Specificity",
      y = "Sensitivity"
    ) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.line = element_line(color = "black")
    )
  
  # Create confidence scatter plot
  p2 <- ggplot(df_clean, aes(x = factor(ResponseCorrect), y = Confidence)) +
    geom_jitter(width = 0.2, alpha = 0.5, color = color_value) +
    scale_x_discrete(labels = c("Incorrect", "Correct")) +
    labs(
      x = "Response",
      y = "Confidence"
    ) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.line = element_line(color = "black")
    )
  
  # Combine plots
  combined_plot <- p2 + p1
  
  return(list(plot = combined_plot, auc = auc_val))
}

# Create summary statistics plot
create_summary_plot <- function(meta_results, auc_val) {
  summary_text <- sprintf(
    "Summary Statistics\n\nAccuracy: %.1f%%\nConfidence: %.1f%%\n\nMetacognition:\nd' = %.2f\nmeta-d' = %.2f\nM-ratio = %.2f\nAUROC = %.2f",
    meta_results$mean_accuracy * 100,
    meta_results$mean_confidence,
    meta_results$d,
    meta_results$metad,
    meta_results$mratio,
    ifelse(is.na(auc_val), meta_results$auroc, auc_val)
  )
  
  p <- ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = summary_text,
             size = 5, hjust = 0.5, vjust = 0.5) +
    theme_void() +
    theme(
      panel.border = element_rect(fill = NA, color = "gray80"),
      plot.margin = margin(20, 20, 20, 20)
    )
  
  return(p)
}