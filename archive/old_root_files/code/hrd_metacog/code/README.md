

# **Heart Rate Discrimination (HRD) & Metacognitive Sensitivity Estimation (meta-d')**

This document describes an **R pipeline** for analyzing **Heart Rate Discrimination (HRD) task data** and estimating **metacognitive sensitivity (meta-d')**. This approach allows researchers to assess **how accurately individuals judge their own performance** on an interoceptive discrimination task.

## **Heart Rate Discrimination Task (HRD)**
The HRD task is used to assess **interoceptive accuracy**—the ability to perceive one’s own heartbeat. Participants judge whether presented auditory tones match their own heartbeats. This method provides a **psychophysical estimate** of interoceptive precision.

For more details, see:
> **Legrand, N., Nikolova, N., Correa, C., Brændholt, M., Stuckert, A., Kildahl, N., ... & Allen, M. (2022).**  The heart rate discrimination task: A psychophysical method to estimate the accuracy and precision of interoceptive beliefs. *Biological Psychology, 168*, 108239.  
> **[DOI: 10.1016/j.biopsycho.2022.108239](https://www.sciencedirect.com/science/article/pii/S0301051121002325)**

## **Metacognitive Efficiency Estimation (meta-d')**
To evaluate metacognitive ability in HRD, we estimate **meta-d’**, a measure of how well confidence ratings reflect actual task performance. This is adapted in **R** from **Steve Fleming’s MATLAB function `fit_meta_d_mcmc.m`**.

For more details on meta-d’ estimation, see:
> **Fleming, S. M. (2017).** HMeta-d: Hierarchical Bayesian Estimation of Metacognitive Efficiency from Confidence Ratings. *Neuroscience of Consciousness.*  
> **[DOI: 10.1093/nc/nix007](https://doi.org/10.1093/nc/nix007)**.

---

# **1. `analyze_hrd_data()`**
## **Function Overview**
This function:
- **Processes HRD task data**
- **Fits the hierarchical metacognition model** using Bayesian estimation
- **Computes key metrics**: meta-d', AUROC2, decision threshold, slope, confidence, and accuracy
- **Saves plots** for visualization

### **Function Signature**
```r
analyze_hrd_data(hrd_data, nRatings = 4, plot_results = TRUE, show_traceplot = TRUE, participant_id = "unknown")
```

### **Input Arguments**
| Argument          | Type    | Description |
|------------------|--------|-------------|
| `hrd_data`       | DataFrame | HRD dataset for one subject |
| `nRatings`       | Integer | Number of confidence rating bins |
| `plot_results`   | Boolean | Whether to display and save plots |
| `show_traceplot` | Boolean | Whether to generate a traceplot |
| `participant_id` | String  | Subject identifier for saving figures |

### **Returns**
A **data frame** containing:
- **`estimated_threshold`**: Decision threshold from last trial
- **`estimated_slope`**: Estimated decision slope
- **`mean_accuracy`**: Average accuracy across trials
- **`mean_confidence`**: Average confidence rating
- **`auroc`**: Area Under the ROC Curve (discrimination measure)
- **`d`**: Discrimination index (d')
- **`metad`**: Meta-d' (metacognitive sensitivity)
- **`mratio`**: Meta-d' / d' ratio (metacognitive efficiency)

---

## **Example Usage**
```r
# Load HRD data
hrd_data <- read_delim("data/SS9909HRD_final.txt", delim = ",")

# Analyze subject data
results <- analyze_hrd_data(hrd_data, nRatings = 4, plot_results = TRUE, show_traceplot = TRUE, participant_id = "SS9909")

# View results
print(results)
```

---

## **Example Figures**
### **Confidence Histogram + Trial Alpha**
![Confidence Histogram](figs/SS9909_confidence_trial_alpha.png)

### **Posterior Meta-d’ Distribution**
![Posterior Meta-d](figs/SS9909_posterior_meta_d.png)

---

# **2. `process_hrd_data()`**
## **Function Overview**
Prepares HRD data by:
- Converting categorical variables into **factors**
- Binning confidence ratings into **quantiles**
- Removing trials with **missing (`NA`) confidence ratings**

### **Function Signature**
```r
process_hrd_data(hrd_data, confbins)
```

### **Input Arguments**
| Argument  | Type    | Description |
|-----------|--------|-------------|
| `hrd_data` | DataFrame | Raw HRD data |
| `confbins` | Integer | Number of confidence bins |

### **Returns**
A processed **data frame** with:
- `Signal`: Binary-coded stimulus type (0/1)
- `Response`: Binary-coded subject response (0/1)
- `ConfidenceBinned`: Binned confidence levels
- Other cleaned HRD variables

---

## **Example Usage**
```r
# Process HRD data before modeling
processed_data <- process_hrd_data(hrd_data, confbins = 4)

# Preview processed data
head(processed_data)
```

---

# **Metacognitive Model Fitting Functions**
The metacognitive model uses **Bayesian estimation** to infer **meta-d’**. This method is adapted from **Steve Fleming’s hierarchical Bayesian framework**.

### **Key Functions in `code/`**
- `fit_metad_indiv.R` – Fits **individual-level** meta-d’ models.
- `trials2counts.R` – Converts trial-by-trial responses into **summary counts** for Bayesian modeling.
- `calc_auroc2.R` – Computes **AUROC** as a measure of discrimination performance.

---

# **References**
- **Legrand, N., et al. (2022).** The heart rate discrimination task: A psychophysical method to estimate the accuracy and precision of interoceptive beliefs. *Biological Psychology, 168*, 108239.  
  **[DOI: 10.1016/j.biopsycho.2022.108239](https://www.sciencedirect.com/science/article/pii/S0301051121002325)**
- **Fleming, S. M. (2017).** HMeta-d: Hierarchical Bayesian Estimation of Metacognitive Efficiency from Confidence Ratings. *Neuroscience of Consciousness.*  
  **[DOI: 10.1093/nc/nix007](https://doi.org/10.1093/nc/nix007)**
