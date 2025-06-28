library(readr)
library(dplyr)
library(stringr)

# Source the analysis function
source("code/analyze_hrd_data.R")

# Define the directory where HRD log files are located
data_dir <- "studydata/"

# Get a list of all HRD log files (*.txt) in the directory
log_files <- list.files(path = data_dir, pattern = ".*HRD.*\\.txt$", full.names = TRUE)

# Initialize an empty data frame to store results
group_results <- data.frame()

# Process each file
for (file in log_files) {
  
  print(sprintf("Processing file: %s", file))  # Debugging output
  
  # Extract participant ID (assumes ID is at the start before "HRD")
  participant_id <- str_extract(basename(file), "^[A-Z0-9]+(?=HRD)")
  
  
  # Determine session type (PRE or POST)
  session_type <- ifelse(grepl("POST", file, ignore.case = TRUE), "POST", "PRE")
  
  # Load the data
  hrd_data <- read_delim(file, delim = ",")
  
  # Apply the HRD analysis function
  result <- analyze_hrd_data(hrd_data, nRatings = 4, plot_results = FALSE, show_traceplot = FALSE, participant_id = participant_id)
  
  # Add participant ID and session type
  result <- result %>%
    mutate(participant_id = participant_id, session_type = session_type)
  
  # Reorder columns: subject ID, session, threshold, slope, accuracy, confidence, auroc, d, metad, mratio
  result <- result %>%
    select(
      participant_id, session_type,
      estimated_threshold, estimated_slope, mean_accuracy, mean_confidence,
      auroc, d, metad, mratio
    )
  
  # Append to group results data frame
  group_results <- bind_rows(group_results, result)
}

# Save the results to a CSV file
write.csv(group_results, "group_results.csv", row.names = FALSE)

# Print the final results
print(group_results)
