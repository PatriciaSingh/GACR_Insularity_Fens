# Convert abundance data to presence-absence data
# Script: convert_to_presence_absence.R
# Purpose: Convert numeric values >0 to 1, keep 0 as 0, save as new PA files
# Load required libraries
library(readr)
library(dplyr)

# Set working directory
setwd("Spiders/data")

# Function to convert abundance to presence-absence
convert_to_pa <- function(file_path) {
  cat("Processing file:", file_path, "\n")
  
  # Read the CSV file without column type guessing
  data <- read_csv(file_path, col_names = FALSE, col_types = cols(.default = "c"), show_col_types = FALSE)
  
  # Create a copy for modification
  pa_data <- data
  
  # Convert numeric columns to presence-absence
  # Start from row 3 (rows 1-2 are species names) and column 5 (columns 1-4 are site info)
  for (i in 3:nrow(pa_data)) {
    for (j in 5:ncol(pa_data)) {  # Start from column 5 (species data)
      value <- pa_data[[i, j]]
      
      # Skip if NA or empty
      if (is.na(value) || value == "" || value == "NA") {
        next
      }
      
      # Try to convert to numeric
      numeric_value <- suppressWarnings(as.numeric(value))
      
      # If it's a valid number, convert to presence-absence
      if (!is.na(numeric_value)) {
        # Convert: 0 stays 0, anything >0 becomes 1
        pa_data[[i, j]] <- as.character(ifelse(numeric_value > 0, 1, 0))
      }
      # Non-numeric values stay unchanged
    }
  }
  
  # Create new filename with "PA" at the end
  file_name <- tools::file_path_sans_ext(file_path)
  file_ext <- tools::file_ext(file_path)
  new_file_path <- paste0(file_name, "PA.", file_ext)
  
  # Save the presence-absence data
  write_csv(pa_data, new_file_path, col_names = FALSE)
  
  cat("Saved presence-absence data to:", new_file_path, "\n\n")
  
  return(pa_data)
}

# List of files to process
csv_files <- c(
  "Spider_pasti_cerven_final.csv",
  "Spider_pasti_kveten_final.csv",
  "Spider_smyk_cerven_final.csv",
  "Spider_smyk_kveten_final.csv"
)

# Process each file
for (file in csv_files) {
  if (file.exists(file)) {
    convert_to_pa(file)
  } else {
    cat("Warning: File not found:", file, "\n")
  }
}

cat("All files converted to presence-absence data!\n")
cat("New files created:\n")
for (file in csv_files) {
  file_name <- tools::file_path_sans_ext(file)
  file_ext <- tools::file_ext(file)
  new_name <- paste0(file_name, "PA.", file_ext)
  cat("- ", new_name, "\n")
}
