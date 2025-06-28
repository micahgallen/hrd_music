#-----------------------------------------------------------------------
# PROJECT CLEANUP SCRIPT
# This script will reorganize your HRD Music project into a clean structure
# IMPORTANT: This will create a backup first, then reorganize files
#-----------------------------------------------------------------------

# Create backup first
backup_project <- function() {
  backup_dir <- paste0("../hrd_music_backup_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  cat(sprintf("Creating backup at: %s\n", backup_dir))
  
  if (!dir.exists(backup_dir)) {
    dir.create(backup_dir, recursive = TRUE)
  }
  
  # Copy everything except .git
  files_to_backup <- list.files(".", all.files = TRUE, recursive = TRUE, full.names = TRUE)
  files_to_backup <- files_to_backup[!grepl("^\\.git", files_to_backup)]
  
  for (file in files_to_backup) {
    dest_file <- file.path(backup_dir, file)
    dest_dir <- dirname(dest_file)
    if (!dir.exists(dest_dir)) {
      dir.create(dest_dir, recursive = TRUE)
    }
    file.copy(file, dest_file, overwrite = TRUE)
  }
  
  cat("✓ Backup complete\n\n")
  return(backup_dir)
}

# Main cleanup function
cleanup_project <- function() {
  cat("🧹 HRD MUSIC PROJECT CLEANUP\n")
  cat("============================\n\n")
  
  # 1. Create backup
  cat("Step 1: Creating backup...\n")
  backup_dir <- backup_project()
  
  # 2. Create new organized structure
  cat("\nStep 2: Creating organized structure...\n")
  
  # Create main directories
  new_dirs <- c(
    "01_data",
    "02_scripts",
    "02_scripts/core_functions",
    "02_scripts/analysis",
    "02_scripts/plotting",
    "03_results",
    "03_results/figures",
    "03_results/tables",
    "04_documentation",
    "archive"
  )
  
  for (dir in new_dirs) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
      cat(sprintf("  Created: %s/\n", dir))
    }
  }
  
  # 3. Move and organize data files
  cat("\nStep 3: Organizing data files...\n")
  
  # Move HRD data files
  hrd_files <- list.files("data", pattern = "HRD.*\\.txt$", recursive = TRUE, full.names = TRUE)
  for (file in hrd_files) {
    participant_id <- basename(dirname(file))
    new_path <- file.path("01_data", participant_id)
    if (!dir.exists(new_path)) dir.create(new_path, recursive = TRUE)
    
    # Only copy the essential HRD files
    if (grepl("HRD_final\\.txt$|HRD\\.txt$", file)) {
      file.copy(file, file.path(new_path, basename(file)), overwrite = TRUE)
      cat(sprintf("  Moved: %s\n", basename(file)))
    }
  }
  
  # Archive large data files
  cat("\n  Archiving large data files...\n")
  large_files <- list.files("data", pattern = "\\.pickle$|\\.npy$|_signal\\.txt$|_ppg.*\\.txt$", 
                            recursive = TRUE, full.names = TRUE)
  for (file in large_files) {
    participant_id <- basename(dirname(file))
    archive_path <- file.path("archive/large_data_files", participant_id)
    if (!dir.exists(archive_path)) dir.create(archive_path, recursive = TRUE)
    file.copy(file, file.path(archive_path, basename(file)), overwrite = TRUE)
    cat(sprintf("  Archived: %s\n", basename(file)))
  }
  
  # 4. Consolidate scripts
  cat("\nStep 4: Consolidating scripts...\n")
  
  # Core analysis functions (from hrd_analysis, not the submodule)
  core_functions <- c(
    "code/hrd_analysis/analyze_hrd_data.R",
    "code/hrd_analysis/fit_metad_indiv.R",
    "code/hrd_analysis/calc_auroc2.R",
    "code/hrd_analysis/trials2counts.R",
    "code/hrd_analysis/bin_confidence_quantiles.R",
    "code/hrd_analysis/helper_functions.R",
    "code/hrd_analysis/Bayes_metad_indiv_R.txt"
  )
  
  for (file in core_functions) {
    if (file.exists(file)) {
      file.copy(file, file.path("02_scripts/core_functions", basename(file)), overwrite = TRUE)
      cat(sprintf("  Copied: %s\n", basename(file)))
    }
  }
  
  # Main analysis scripts (keep the best ones)
  main_scripts <- list(
    "02_scripts/analysis/01_run_metacognition_analysis.R" = "music_study_analysis.R",
    "02_scripts/analysis/02_run_psychophysics_analysis.R" = "music_psychophysics_analysis.R",
    "02_scripts/analysis/03_run_bayesian_analysis.R" = "bayesian_psychometric_analysis.R",
    "02_scripts/analysis/00_run_all_analyses.R" = "run_cardioception_analysis.R"
  )
  
  for (new_name in names(main_scripts)) {
    old_file <- main_scripts[[new_name]]
    if (file.exists(old_file)) {
      file.copy(old_file, new_name, overwrite = TRUE)
      cat(sprintf("  Organized: %s\n", basename(new_name)))
    }
  }
  
  # Plotting functions
  if (file.exists("cardioception_plotting_functions.R")) {
    file.copy("cardioception_plotting_functions.R", 
              "02_scripts/plotting/cardioception_plots.R", overwrite = TRUE)
  }
  
  # Setup and utility scripts
  utility_scripts <- c("setup_project.R", "test_setup.R")
  for (script in utility_scripts) {
    if (file.exists(script)) {
      file.copy(script, file.path("02_scripts", script), overwrite = TRUE)
    }
  }
  
  # 5. Organize results
  cat("\nStep 5: Organizing results...\n")
  
  # Copy best results
  if (dir.exists("results/cardioception_style")) {
    # Copy figures
    figs <- list.files("results/cardioception_style", pattern = "\\.png$", 
                       recursive = TRUE, full.names = TRUE)
    for (fig in figs) {
      participant <- basename(dirname(fig))
      new_path <- file.path("03_results/figures", participant)
      if (!dir.exists(new_path)) dir.create(new_path, recursive = TRUE)
      file.copy(fig, file.path(new_path, basename(fig)), overwrite = TRUE)
    }
    cat("  ✓ Moved figure files\n")
  }
  
  # Copy CSV results
  csv_files <- list.files("results", pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
  for (csv in csv_files) {
    file.copy(csv, file.path("03_results/tables", basename(csv)), overwrite = TRUE)
  }
  cat("  ✓ Moved result tables\n")
  
  # 6. Archive old files
  cat("\nStep 6: Archiving old files...\n")
  
  # Archive redundant analysis scripts
  redundant_scripts <- c(
    "master_script.R", "pipeline_v1.R", "integrated_analysis.R",
    "hrd_music_psychophysics.R", "msuic_analysis_script.R",
    "music_analysis_script.R", "music_study_script.R"
  )
  
  archive_scripts_dir <- "archive/old_scripts"
  if (!dir.exists(archive_scripts_dir)) dir.create(archive_scripts_dir, recursive = TRUE)
  
  for (script in redundant_scripts) {
    if (file.exists(script)) {
      file.copy(script, file.path(archive_scripts_dir, script), overwrite = TRUE)
      cat(sprintf("  Archived: %s\n", script))
    }
  }
  
  # 7. Create documentation
  cat("\nStep 7: Creating documentation...\n")
  
  # Create new README
  readme_content <- '# HRD Music Study

## Overview
This project analyzes Heart Rate Discrimination (HRD) data to investigate cardiac interoception and metacognitive efficiency in musicians.

## Project Structure

```
hrd_music/
├── 01_data/                    # Raw HRD data files
│   ├── 49987HRD/
│   ├── 55330HRD/
│   └── 55363HRD/
├── 02_scripts/                  # All analysis scripts
│   ├── core_functions/          # Core HRD analysis functions
│   ├── analysis/                # Main analysis scripts
│   │   ├── 00_run_all_analyses.R        # Master script - run this!
│   │   ├── 01_run_metacognition_analysis.R
│   │   ├── 02_run_psychophysics_analysis.R
│   │   └── 03_run_bayesian_analysis.R
│   └── plotting/                # Plotting functions
├── 03_results/                  # Analysis outputs
│   ├── figures/                 # All plots
│   └── tables/                  # Summary statistics
├── 04_documentation/            # Project documentation
└── archive/                     # Old/unused files

## Quick Start

1. **Run all analyses:**
   ```r
   source("02_scripts/analysis/00_run_all_analyses.R")
   ```

2. **Run specific analysis:**
   ```r
   # Metacognition only
   source("02_scripts/analysis/01_run_metacognition_analysis.R")
   
   # Psychophysics plots
   source("02_scripts/analysis/02_run_psychophysics_analysis.R")
   ```

## Key Metrics

- **Meta-d\'**: Metacognitive sensitivity
- **M-ratio**: Metacognitive efficiency (meta-d\'/d\')
- **AUROC**: Area under ROC curve for confidence-accuracy relationship
- **Psychometric curves**: Separate for Intero and Extero conditions

## Dependencies

Required R packages:
- tidyverse, patchwork, rjags, coda, ggmcmc
- ggdist, pROC, psycho, pracma
- cmdstanr (optional, for Bayesian analysis)
'
  
  writeLines(readme_content, "04_documentation/README.md")
  writeLines(readme_content, "README.md")  # Also at root
  cat("  ✓ Created README.md\n")
  
  # 8. Clean up old directories
  cat("\nStep 8: Cleaning up...\n")
  
  # Move old directories to archive
  old_dirs <- c("results", "code", "data")
  for (dir in old_dirs) {
    if (dir.exists(dir)) {
      new_archive_path <- file.path("archive", paste0("old_", dir))
      if (!dir.exists(dirname(new_archive_path))) {
        dir.create(dirname(new_archive_path), recursive = TRUE)
      }
      file.rename(dir, new_archive_path)
      cat(sprintf("  Moved to archive: %s/\n", dir))
    }
  }
  
  # 9. Create .gitignore
  cat("\nStep 9: Creating .gitignore...\n")
  gitignore_content <- '# Large data files
archive/large_data_files/
*.pickle
*.npy
*_signal.txt
*_ppg*.txt

# R files
.Rhistory
.RData
.Rproj.user/
*.Rproj

# OS files
.DS_Store
Thumbs.db

# Temporary files
*~
*.tmp
*.bak
'
  writeLines(gitignore_content, ".gitignore")
  cat("  ✓ Created .gitignore\n")
  
  cat("\n✅ CLEANUP COMPLETE!\n")
  cat("\nYour project is now organized as follows:\n")
  cat("- 01_data/: Only essential HRD data files\n")
  cat("- 02_scripts/: Consolidated, well-named scripts\n")
  cat("- 03_results/: Organized figures and tables\n")
  cat("- 04_documentation/: Project documentation\n")
  cat("- archive/: All old/redundant files (can be deleted later)\n")
  cat(sprintf("\nBackup saved at: %s\n", backup_dir))
  cat("\nNext steps:\n")
  cat("1. Review the archive/ folder and delete if not needed\n")
  cat("2. Run: source('02_scripts/analysis/00_run_all_analyses.R')\n")
  cat("3. Check results in 03_results/\n")
}

# Run cleanup
if (interactive()) {
  response <- readline("This will reorganize your entire project. Continue? (yes/no): ")
  if (tolower(response) == "yes") {
    cleanup_project()
  } else {
    cat("Cleanup cancelled.\n")
  }
} else {
  cat("Run this script interactively to proceed with cleanup.\n