#-----------------------------------------------------------------------
# MASTER SCRIPT: Run All HRD Analyses
#
# This script runs all available analyses on your HRD data:
# 1. Basic metacognition analysis (meta-d', AUROC, etc.)
# 2. Comprehensive psychophysics visualization
# 3. Optional Bayesian psychometric function fitting
#-----------------------------------------------------------------------

cat("\n")
cat("╔═══════════════════════════════════════════════════════╗\n")
cat("║         HRD MUSIC STUDY - COMPLETE ANALYSIS           ║\n")
cat("╚═══════════════════════════════════════════════════════╝\n")
cat("\n")

# Check what analyses are available
analyses_available <- list(
  metacognition = TRUE,
  psychophysics = TRUE,
  bayesian = requireNamespace("cmdstanr", quietly = TRUE)
)

cat("Available analyses:\n")
cat(sprintf("  ✓ Metacognition (meta-d', AUROC)\n"))
cat(sprintf("  ✓ Psychophysics visualizations\n"))
if (analyses_available$bayesian) {
  cat(sprintf("  ✓ Bayesian psychometric fitting\n"))
} else {
  cat(sprintf("  ✗ Bayesian fitting (install cmdstanr to enable)\n"))
}
cat("\n")

# Ask user what to run
if (interactive()) {
  cat("Which analyses would you like to run?\n")
  cat("1. Basic metacognition only\n")
  cat("2. Metacognition + Psychophysics\n")
  if (analyses_available$bayesian) {
    cat("3. All analyses (including Bayesian)\n")
  }
  
  choice <- readline("Enter your choice (1-3): ")
  choice <- as.numeric(choice)
  
  if (is.na(choice) || choice < 1 || choice > 3) {
    choice <- 2  # Default to option 2
    cat("Invalid choice. Running option 2 (Metacognition + Psychophysics)\n")
  }
} else {
  # Non-interactive mode: run metacognition + psychophysics
  choice <- 2
}

# Execute chosen analyses
cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

if (choice >= 1) {
  cat("\n📊 RUNNING BASIC METACOGNITION ANALYSIS\n")
  cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
  source("music_study_analysis.R")
}

if (choice >= 2) {
  cat("\n🔬 RUNNING PSYCHOPHYSICS ANALYSIS\n")
  cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
  source("music_psychophysics_analysis.R")
}

if (choice == 3 && analyses_available$bayesian) {
  cat("\n🧮 RUNNING BAYESIAN ANALYSIS\n")
  cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
  
  # Source Bayesian functions
  source("bayesian_psychometric_analysis.R")
  
  # Find data files
  data_files <- list.files("data", pattern = "HRD_final\\.txt$", 
                           recursive = TRUE, full.names = TRUE)
  
  # Run Bayesian analysis
  bayes_results <- run_bayesian_psychophysics(data_files)
}

# Summary
cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("🎉 ALL ANALYSES COMPLETE!\n\n")

cat("📁 Output locations:\n")
if (choice >= 1) {
  cat("   • Metacognition results: results/summary_hrd_music_study.csv\n")
  cat("   • Individual meta-d plots: results/plots/\n")
}
if (choice >= 2) {
  cat("   • Psychophysics plots: results/psychophysics/\n")
  cat("   • Psychophysics summary: results/psychophysics_summary.csv\n")
}
if (choice == 3 && analyses_available$bayesian) {
  cat("   • Bayesian results: results/bayesian_psychophysics/\n")
  cat("   • Bayesian summary: results/bayesian_psychophysics/bayesian_summary.csv\n")
}

cat("\n💡 Tip: Open the results folders to view all generated plots and data!\n")