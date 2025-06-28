# HRD Music Study: Analysis Pipeline

## 1. Project Overview

This project contains a complete analysis pipeline for a study on Heart Rate Discrimination (HRD), metacognition, and psychophysics, likely comparing musicians and non-musicians. The pipeline is designed to process raw participant data from the `/data` directory, perform several layers of analysis, and generate publication-quality visualizations and summary tables.

The key analyses include:
* **Non-Bayesian Metacognition**: Calculation of standard metrics like d-prime, meta-d', and M-Ratio.
* **Psychophysics**: Analysis of psychometric function parameters (threshold, slope), reaction times, and confidence.
* **Single-Subject Bayesian Analysis**: Individual fitting of psychometric functions using Stan for more robust parameter estimation.
* **Group-Level Analysis**: Visualization and comparison of results across all participants.

---

## 2. Initial Project Setup

Before running any analysis, you must set up the project environment.

### Step 2.1: Run the Project Setup Script
This is a **mandatory first step**. The `setup_project.R` script copies essential functions from a git submodule into a local `code/hrd_analysis` directory and ensures all required R packages are installed.

In your R console, run:
```R
source("setup_project.R")
```
The script will check for packages like `tidyverse`, `patchwork`, `rjags`, and `ggmcmc` and will prompt you to install any that are missing.

### Step 2.2: Setup for Bayesian Analysis (Optional)
If you plan to run the Bayesian analysis pipeline, you also need to download the necessary Stan model files.

In your R console, run:
```R
source("setup_psychophysics.R")
```
This script will create the `code/stan_models` directory and download the required `.stan` files. It will also check if `cmdstanr` is installed.

---

## 3. Running the Analysis Pipelines

After setup, you can run any of the analysis pipelines. The results will be saved in the `/results` directory.

### Pipeline 1: Non-Bayesian Analysis (Recommended First)
This pipeline calculates all standard metacognitive and psychophysical metrics and generates a comprehensive set of plots for each participant, including the composite "Cardioception-style" dashboard and the trial dynamics plot.

To run this analysis, execute the following script in your R console:
```R
source("run_all_non_bayesian_analyses.R")
```
* **Output**: This will create a `non_bayesian_analysis` folder inside `/results`. For each participant, a subfolder will be created containing all generated plots. A summary file, `summary_non_bayesian_analysis.csv`, will also be created.

### Pipeline 2: Single-Subject Bayesian Analysis
This pipeline fits a Bayesian psychometric model to each participant's data for each modality (Intero/Extero), providing robust estimates of the threshold and slope parameters.

To run this analysis, execute the following script:
```R
source("run_all_bayesian_analyses.R")
```
* **Output**: This will create a `bayesian_psychophysics` folder inside `/results`. For each participant, a subfolder will be created containing diagnostic plots for the model fit. A summary file, `bayesian_summary.csv`, will also be created.

### Pipeline 3: Group-Level Visualization
After running both the non-Bayesian and Bayesian pipelines, you can run this script to generate plots that compare the results across all participants.

To run this analysis, execute the following script:
```R
source("group_analysis.R")
```
* **Output**: This will create a `group_plots` folder inside `/results`, containing three summary plots that visualize the group-level distributions and comparisons.
