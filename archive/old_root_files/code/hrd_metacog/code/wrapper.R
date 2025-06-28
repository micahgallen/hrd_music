library(readr)
library(patchwork)
# List all .txt files in the current working directory
log_files <- list.files(pattern = "\\.txt$", full.names = TRUE)

# For each file, read it as a separate data frame in your workspace
for (file in log_files) {
  # Create a data frame name from the file name (without extension)
  df_name <- tools::file_path_sans_ext(basename(file))
  # Read the file as a tab-delimited data frame
  assign(df_name, read_delim(file, delim = ","))
}

hrd_data <- make_factors(SS9909HRD_final)

nRatings = 4 # to bin ratings
processed_hrd_data <- process_hrd_data(hrd_data, 4)

a <- plot_confidence_histogram(hrd_data)
b <- plot_trial_alpha(hrd_data)
a+b

# Extract inputs for trials2counts
stimID <- processed_hrd_data$Signal
response <- processed_hrd_data$Response
rating <- processed_hrd_data$ConfidenceBinned


# Call trials2counts
counts <- trials2counts(stimID, response, rating, nRatings)
nR_S1 <- counts[[1]]
nR_S2 <- counts[[2]]

fit <- fit_metad_indiv(nR_S1, nR_S2)

auroc <- calc_auroc2(nR_S1, nR_S2, 3)


## Model output ------------------------------------------------------------
output = fit[[1]]
d1 = fit[[2]]$d1

# Mean values 
Value <- summary(output)
stat <- data.frame(mean = Value[["statistics"]][, "Mean"])
stat %<>%
  rownames_to_column(var = "name")

# Rhat 
Value <- gelman.diag(output, confidence = 0.95)
Rhat <- data.frame(conv = Value$psrf)

# HDI 
HDI <- data.frame(HPDinterval(output, prob = 0.95))
HDI %<>%
  rownames_to_column(var = "name")

# All values in the same dataframe
Fit <- stat %>%
  cbind(lower = HDI$lower,
        upper = HDI$upper)

metad <- stat$mean[stat$name == "meta_d"]
mratio <- metad/d1

meanConfidence = mean(hrd_data$Confidence)

print(sprintf("Metacognition scores: auroc = %.2f, mratio = %.2f, mean confidence = %.2f", auroc, mratio, meanConfidence))

## Plots ---------------------------------------------------------

# Plot trace mcmc
traceplot(output)

# mcmc values in df for plot posterior distributions
mcmc.sample <- ggs(output)

# Plot posterior distribution for meta-d value
Plot <- mcmc.sample %>%
  filter(Parameter == "meta_d") %>% 
  ggplot(aes(value)) +
  geom_histogram(binwidth = 0.03, fill = "blue", colour = "grey", alpha = 0.5) +
  geom_vline(xintercept = stat$mean[stat$name == "meta_d"],linetype="dashed", linewidth= 1.5) +
  annotate("segment", x = HDI$lower[HDI$name == "meta_d"], y = 50, 
           xend = HDI$upper[HDI$name == "meta_d"],
           yend = 50, colour = "white", linewidth = 2.5) +
  ylab("Sample count") +
  xlab(expression(paste("Meta d'")))

Plot