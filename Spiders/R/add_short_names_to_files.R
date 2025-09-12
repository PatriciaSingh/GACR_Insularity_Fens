# Add short names to multiple CSV files based on Latin names
# Script: add_short_names_to_files.R
# Purpose: Add short name rows to CSV files by matching Latin names from lookup table
# Load required libraries
library(readr)
library(dplyr)

# Set working directory
setwd("Oribatida/data")

# Step 1: Read the file with short names to create lookup table
species_lookup <- read_csv("Oribatida_Species_Specializace_Short_names.csv")

# Create a lookup table: Latin name -> short name
lookup_table <- setNames(species_lookup$short_name, species_lookup$Species)

# Step 2: Function to add short names row to a CSV file
add_short_names_row <- function(file_path) {
  cat("Processing file:", file_path, "\n")
  
  # Read the CSV file
  data <- read_csv(file_path, col_names = FALSE)
  
  # Get the first row (Latin names)
  latin_names <- as.character(data[1, ])
  
  # Create short names row by matching Latin names
  short_names <- sapply(latin_names, function(name) {
    if (!is.na(name) && name %in% names(lookup_table)) {
      return(lookup_table[[name]])
    } else {
      return(NA)  # or return the original name if no match
    }
  })
  
  # Convert to data frame row
  short_names_row <- as.data.frame(t(short_names), stringsAsFactors = FALSE)
  names(short_names_row) <- names(data)
  
  # Add the short names as the second row
  updated_data <- rbind(data[1, ], short_names_row, data[-1, ])
  
  # Save back to the same file
  write_csv(updated_data, file_path, col_names = FALSE)
  
  cat("Added short names row to:", file_path, "\n")
  cat("Example matches found:", sum(!is.na(short_names)), "out of", length(short_names), "\n\n")
  
  return(updated_data)
}

# Step 3: List your CSV files to process
csv_files <- c("Oribatida_original_clean_names.csv")

# Step 4: Process each file
for (file in csv_files) {
  if (file.exists(file)) {
    add_short_names_row(file)
  } else {
    cat("Warning: File not found:", file, "\n")
  }
}

cat("All files processed!\n")
