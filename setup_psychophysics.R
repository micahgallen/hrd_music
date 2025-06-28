#-----------------------------------------------------------------------
# SETUP SCRIPT: Download Stan Models from Cardioception
#
# This script downloads the necessary Stan model files for optional
# Bayesian psychometric function fitting
#-----------------------------------------------------------------------

cat("Setting up Stan models for Bayesian analysis...\n\n")

# Create directory for Stan models
stan_dir <- "code/stan_models"
dir.create(stan_dir, showWarnings = FALSE, recursive = TRUE)

# URLs for Stan model files
stan_files <- list(
  "Standard_Cumulative_Normal.stan" = "https://raw.githubusercontent.com/embodied-computation-group/Cardioception/master/docs/source/examples/R/src/Standard%20Cummulative%20normal.stan",
  "Hierarchical_Cumulative_Normal.stan" = "https://raw.githubusercontent.com/embodied-computation-group/Cardioception/master/docs/source/examples/R/src/Hierarchical%20Cummulative%20Normal.stan"
)

# Download each file
for (filename in names(stan_files)) {
  url <- stan_files[[filename]]
  dest_file <- file.path(stan_dir, filename)
  
  cat(sprintf("Downloading %s...", filename))
  
  tryCatch({
    download.file(url, dest_file, quiet = TRUE)
    cat(" ✓\n")
  }, error = function(e) {
    cat(" ✗\n")
    cat(sprintf("  Error: %s\n", e$message))
  })
}

# Check if cmdstanr is installed for Bayesian analysis
cat("\nChecking for Bayesian analysis dependencies...\n")

if (!requireNamespace("cmdstanr", quietly = TRUE)) {
  cat("  ℹ cmdstanr not installed. Bayesian analysis will not be available.\n")
  cat("  To enable Bayesian analysis, install cmdstanr:\n")
  cat("    install.packages('cmdstanr', repos = c('https://mc-stan.org/r-packages/', getOption('repos')))\n")
  cat("    cmdstanr::install_cmdstan()\n")
} else {
  cat("  ✓ cmdstanr is installed\n")
  
  # Check if cmdstan is installed
  tryCatch({
    cmdstanr::cmdstan_version()
    cat("  ✓ CmdStan is installed\n")
  }, error = function(e) {
    cat("  ℹ CmdStan not found. Run: cmdstanr::install_cmdstan()\n")
  })
}

cat("\nStan model setup complete!\n")