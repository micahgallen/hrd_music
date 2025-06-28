# Load required library
library(fs)

# Define source directory (where participant folders are located)
source_dir <- "/Users/au288926/Downloads/Cardioception data 2"

# Define destination directory
destination_dir <- "studydata/"

# Ensure destination directory exists
if (!dir.exists(destination_dir)) {
  dir.create(destination_dir, recursive = TRUE)
}

# Define the HRD file types to search for
file_patterns <- c("HRD_final.txt$", "HRD_POST_final.txt$")

# Initialize an empty vector to store found files
log_files <- c()

# Loop over each file type and collect matching files
for (pattern in file_patterns) {
  files <- dir_ls(source_dir, recurse = TRUE, regexp = pattern)
  log_files <- c(log_files, files)  # Append found files
}

# Check if any files were found
if (length(log_files) == 0) {
  message("No HRD log files found. Check the folder structure and filenames.")
} else {
  # Copy each HRD log file to the new destination
  for (file in log_files) {
    # Extract the filename
    file_name <- basename(file)
    
    # Define new path
    new_path <- file.path(destination_dir, file_name)
    
    # Copy the file (instead of moving)
    file_copy(file, new_path, overwrite = TRUE)
    
    # Print confirmation
    message(sprintf("Copied: %s -> %s", file, new_path))
  }
  
  message("All HRD log files copied successfully!")
}
