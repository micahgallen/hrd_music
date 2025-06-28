# Load required libraries
library(tidyr)
library(dplyr)

# Pivot data to wide format
# Load required libraries
library(tidyr)
library(dplyr)

# Pivot data to wide format

# Remove rows where `mratio` is Inf (either in PRE or POST)
group_results_filtered <- group_results %>%
  filter(!is.infinite(mratio))  # Removes rows where `mratio` is Inf

# Pivot data to wide format
group_results_wide <- group_results_filtered %>%
  pivot_wider(
    names_from = session_type, 
    values_from = c(estimated_threshold, estimated_slope, mean_accuracy, 
                    mean_confidence, auroc, d, metad, mratio),
    names_glue = "{.value}_{session_type}"  # Appends "_PRE" and "_POST"
  ) %>%
  # Replace NA with "" in character columns
  mutate(across(where(is.character), ~replace_na(.x, ""))) %>%
  # Replace NA with "" in numeric columns (JASP-safe formatting)
  mutate(across(where(is.numeric), ~ifelse(is.na(.x), "", .x)))

# Print structure to confirm
str(group_results_wide)



# Print structure to confirm
write.csv(group_results_wide, "group_results_wide.csv", row.names = FALSE)