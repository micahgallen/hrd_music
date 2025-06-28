#-----------------------------------------------------------------------
# PROJECT STRUCTURE VISUALIZATION
# One-time script to show the complete directory and file tree
#-----------------------------------------------------------------------

# Function to create tree structure
create_tree <- function(path = ".", prefix = "", max_depth = 10, current_depth = 0,
                        ignore_patterns = c("\\.Rproj\\.user", "\\.git", "\\.DS_Store", 
                                            "__pycache__", "\\.Rhistory", "\\.RData")) {
  
  if (current_depth >= max_depth) return()
  
  # Get all items in the directory
  items <- list.files(path, all.files = TRUE, full.names = FALSE)
  
  # Filter out items matching ignore patterns
  for (pattern in ignore_patterns) {
    items <- items[!grepl(pattern, items)]
  }
  
  # Remove . and ..
  items <- items[!items %in% c(".", "..")]
  
  # Sort items (directories first)
  item_paths <- file.path(path, items)
  is_dir <- file.info(item_paths)$isdir
  items <- c(items[is_dir & !is.na(is_dir)], items[!is_dir & !is.na(is_dir)])
  
  for (i in seq_along(items)) {
    item <- items[i]
    item_path <- file.path(path, item)
    
    # Determine if it's the last item
    is_last <- i == length(items)
    
    # Create the tree symbols
    if (is_last) {
      cat(prefix, "└── ", sep = "")
      new_prefix <- paste0(prefix, "    ")
    } else {
      cat(prefix, "├── ", sep = "")
      new_prefix <- paste0(prefix, "│   ")
    }
    
    # Print the item name
    if (file.info(item_path)$isdir && !is.na(file.info(item_path)$isdir)) {
      cat(crayon::blue(item), "/\n", sep = "")
      # Recursively print subdirectory
      create_tree(item_path, new_prefix, max_depth, current_depth + 1, ignore_patterns)
    } else {
      # Add file size for context
      size <- file.info(item_path)$size
      if (!is.na(size)) {
        if (size < 1024) {
          size_str <- paste0(" (", size, " B)")
        } else if (size < 1024^2) {
          size_str <- paste0(" (", round(size/1024, 1), " KB)")
        } else {
          size_str <- paste0(" (", round(size/1024^2, 1), " MB)")
        }
      } else {
        size_str <- ""
      }
      
      # Color code by file type
      if (grepl("\\.R$|\\.r$", item)) {
        cat(crayon::green(item), size_str, "\n", sep = "")
      } else if (grepl("\\.csv$", item)) {
        cat(crayon::yellow(item), size_str, "\n", sep = "")
      } else if (grepl("\\.png$|\\.jpg$|\\.jpeg$|\\.pdf$", item)) {
        cat(crayon::magenta(item), size_str, "\n", sep = "")
      } else if (grepl("\\.txt$", item)) {
        cat(crayon::cyan(item), size_str, "\n", sep = "")
      } else {
        cat(item, size_str, "\n", sep = "")
      }
    }
  }
}

# Count files by type
count_files <- function(path = ".", pattern, recursive = TRUE) {
  files <- list.files(path, pattern = pattern, recursive = recursive, full.names = TRUE)
  return(length(files))
}

# Main execution
cat("\n🌳 HRD MUSIC PROJECT STRUCTURE\n")
cat("================================\n\n")

# Check if crayon is installed for colors
if (!requireNamespace("crayon", quietly = TRUE)) {
  cat("Installing crayon for colored output...\n")
  install.packages("crayon")
}
library(crayon)

# Get project info
cat("📊 PROJECT SUMMARY:\n")
cat("------------------\n")

# Count different file types
n_r_files <- count_files(pattern = "\\.R$|\\.r$")
n_csv_files <- count_files(pattern = "\\.csv$")
n_txt_files <- count_files(pattern = "\\.txt$")
n_png_files <- count_files(pattern = "\\.png$")
n_pdf_files <- count_files(pattern = "\\.pdf$")

cat(sprintf("   R scripts: %d\n", n_r_files))
cat(sprintf("   CSV files: %d\n", n_csv_files))
cat(sprintf("   Text files: %d\n", n_txt_files))
cat(sprintf("   PNG images: %d\n", n_png_files))
cat(sprintf("   PDF files: %d\n", n_pdf_files))

# Calculate total project size
get_dir_size <- function(path) {
  files <- list.files(path, recursive = TRUE, full.names = TRUE)
  sum(file.info(files)$size, na.rm = TRUE)
}

total_size <- get_dir_size(".")
if (total_size < 1024^2) {
  size_str <- paste0(round(total_size/1024, 1), " KB")
} else if (total_size < 1024^3) {
  size_str <- paste0(round(total_size/1024^2, 1), " MB")
} else {
  size_str <- paste0(round(total_size/1024^3, 2), " GB")
}

cat(sprintf("\n   Total size: %s\n", size_str))

# Show directory tree
cat("\n\n📁 DIRECTORY STRUCTURE:\n")
cat("----------------------\n")
cat(crayon::bold("hrd_music/\n"))
create_tree(".")

# List all R scripts with descriptions
cat("\n\n📝 R SCRIPTS OVERVIEW:\n")
cat("---------------------\n")

r_files <- list.files(".", pattern = "\\.R$|\\.r$", recursive = TRUE, full.names = TRUE)
r_files <- r_files[order(r_files)]

for (file in r_files) {
  cat(sprintf("\n%s:\n", crayon::bold(file)))
  
  # Read first few lines to find description
  lines <- readLines(file, n = 10, warn = FALSE)
  desc_line <- grep("^#[^#]", lines, value = TRUE)[1]
  if (!is.na(desc_line)) {
    cat(sprintf("   %s\n", trimws(gsub("^#", "", desc_line))))
  }
}

# Show results structure
cat("\n\n📊 RESULTS STRUCTURE:\n")
cat("--------------------\n")

if (dir.exists("results")) {
  results_dirs <- list.dirs("results", recursive = FALSE, full.names = FALSE)
  for (rd in results_dirs) {
    cat(sprintf("\nresults/%s/:\n", rd))
    n_files <- length(list.files(file.path("results", rd), recursive = TRUE))
    cat(sprintf("   Contains %d files\n", n_files))
  }
}

# Show data structure
cat("\n\n💾 DATA STRUCTURE:\n")
cat("-----------------\n")

if (dir.exists("data")) {
  data_files <- list.files("data", pattern = "HRD_final\\.txt$", 
                           recursive = TRUE, full.names = TRUE)
  cat(sprintf("Found %d HRD data files:\n", length(data_files)))
  for (df in data_files) {
    size <- file.info(df)$size
    cat(sprintf("   %s (%d KB)\n", df, round(size/1024, 1)))
  }
}

# Identify potentially redundant files
cat("\n\n🔍 POTENTIAL ISSUES:\n")
cat("-------------------\n")

# Check for multiple analysis scripts
analysis_scripts <- r_files[grepl("analys|Analys|ANALYS", r_files)]
if (length(analysis_scripts) > 3) {
  cat(sprintf("⚠️  Found %d analysis scripts - might need consolidation\n", 
              length(analysis_scripts)))
}

# Check for large results folders
if (dir.exists("results")) {
  results_size <- get_dir_size("results")
  if (results_size > 100 * 1024^2) {  # > 100 MB
    cat(sprintf("⚠️  Results folder is large: %.1f MB\n", results_size/1024^2))
  }
}

# Check for duplicate named files
all_files <- list.files(".", recursive = TRUE)
file_names <- basename(all_files)
duplicates <- file_names[duplicated(file_names)]
if (length(unique(duplicates)) > 0) {
  cat(sprintf("⚠️  Found %d files with duplicate names\n", length(unique(duplicates))))
  for (dup in unique(duplicates)) {
    cat(sprintf("     - %s\n", dup))
  }
}

cat("\n\n✅ Project structure analysis complete!\n")
cat("Save this output for reference when cleaning up.\n\n")

# Save to file
output_file <- paste0("project_structure_", format(Sys.Date(), "%Y%m%d"), ".txt")
cat(sprintf("Saving output to: %s\n", output_file))

sink(output_file)
cat("HRD MUSIC PROJECT STRUCTURE\n")
cat("Generated on:", format(Sys.time()), "\n")
cat("================================\n\n")
create_tree(".")
sink()

cat("Done! Check", output_file, "for the saved structure.\n")