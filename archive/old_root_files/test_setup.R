#-----------------------------------------------------------------------
# TEST SCRIPT: Verify HRD Analysis Setup
#
# This script tests that all components are properly set up
#-----------------------------------------------------------------------

cat("=== Testing HRD Analysis Setup ===\n\n")

# 1. Check if setup has been run
cat("1. Checking setup...\n")
if (!dir.exists("code/hrd_analysis")) {
  cat("   ✗ Setup has not been run. Please run:\n")
  cat("     source('setup_hrd_analysis.R')\n")
  stop("Setup required before testing.")
} else {
  cat("   ✓ Project-specific code directory exists\n")
}

# 2. Check required files
cat("\n2. Checking required files...\n")
required_files <- c(
  "code/hrd_analysis/analyze_hrd_data.R",
  "code/hrd_analysis/fit_metad_indiv.R",
  "code/hrd_analysis/helper_functions.R",
  "code/hrd_analysis/bin_confidence_quantiles.R",
  "code/hrd_analysis/trials2counts.R",
  "code/hrd_analysis/calc_auroc2.R",
  "code/hrd_analysis/Bayes_metad_indiv_R.txt"
)

all_exist <- TRUE
for (file in required_files) {
  if (file.exists(file)) {
    cat(sprintf("   ✓ %s\n", file))
  } else {
    cat(sprintf("   ✗ Missing: %s\n", file))
    all_exist <- FALSE
  }
}

if (!all_exist) {
  stop("Some required files are missing. Please run setup again.")
}

# 3. Test loading functions
cat("\n3. Testing function loading...\n")
tryCatch({
  source("code/hrd_analysis/helper_functions.R")
  source("code/hrd_analysis/bin_confidence_quantiles.R")
  source("code/hrd_analysis/trials2counts.R")
  source("code/hrd_analysis/fit_metad_indiv.R")
  source("code/hrd_analysis/calc_auroc2.R")
  source("code/hrd_analysis/analyze_hrd_data.R")
  cat("   ✓ All functions loaded successfully\n")
}, error = function(e) {
  cat(sprintf("   ✗ Error loading functions: %s\n", e$message))
  stop("Function loading failed.")
})

# 4. Check for data files
cat("\n4. Checking for data files...\n")
data_files <- list.files(
  path = "data",
  pattern = "HRD_final\\.txt$",
  recursive = TRUE,
  full.names = TRUE
)

if (length(data_files) == 0) {
  cat("   ! No HRD data files found in data/\n")
  cat("   ! Please add your HRD_final.txt files to the data directory\n")
} else {
  cat(sprintf("   ✓ Found %d HRD data files\n", length(data_files)))
  cat("   Files:\n")
  for (i in seq_len(min(5, length(data_files)))) {
    cat(sprintf("     - %s\n", data_files[i]))
  }
  if (length(data_files) > 5) {
    cat(sprintf("     ... and %d more\n", length(data_files) - 5))
  }
}

# 5. Test JAGS
cat("\n5. Testing JAGS installation...\n")
tryCatch({
  library(rjags)
  # Create a simple test model
  model_string <- "model {
    # Priors
    mu ~ dnorm(0, 0.001)
    tau ~ dgamma(0.001, 0.001)
    
    # Likelihood
    for (i in 1:N) {
      x[i] ~ dnorm(mu, tau)
    }
  }"
  
  # Test data
  test_data <- list(x = rnorm(10), N = 10)
  
  # Try to compile
  test_model <- jags.model(
    textConnection(model_string),
    data = test_data,
    n.chains = 1,
    quiet = TRUE
  )
  
  cat("   ✓ JAGS is properly installed and working\n")
}, error = function(e) {
  cat("   ✗ JAGS test failed. Make sure JAGS is installed:\n")
  cat("     https://mcmc-jags.sourceforge.io/\n")
})

# 6. Create a minimal test
cat("\n6. Running minimal function test...\n")
tryCatch({
  # Test bin_confidence_quantiles
  test_conf <- c(0, 25, 50, 75, 100)
  binned <- bin_confidence_quantiles(test_conf, 4)
  cat("   ✓ bin_confidence_quantiles works\n")
  
  # Test trials2counts with minimal data
  test_stim <- c(0, 1, 0, 1)
  test_resp <- c(0, 1, 1, 0)
  test_rating <- c(1, 2, 3, 4)
  counts <- trials2counts(test_stim, test_resp, test_rating, 4)
  cat("   ✓ trials2counts works\n")
  
  # Test calc_auroc2
  test_nR_S1 <- c(5, 4, 3, 2, 2, 3, 4, 5)
  test_nR_S2 <- c(2, 3, 4, 5, 5, 4, 3, 2)
  auroc <- calc_auroc2(test_nR_S1, test_nR_S2, 4)
  cat(sprintf("   ✓ calc_auroc2 works (AUROC = %.3f)\n", auroc))
  
}, error = function(e) {
  cat(sprintf("   ✗ Function test failed: %s\n", e$message))
})

# Summary
cat("\n=== TEST COMPLETE ===\n")
cat("\nIf all tests passed, you're ready to run:\n")
cat("  source('music_study_analysis.R')\n")
cat("\nIf you have no data files yet, add them to the data/ directory first.\n")