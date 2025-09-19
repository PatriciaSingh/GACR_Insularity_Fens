# generate_diatom_short_names.R
# Script to add short names row to diatom CSV files
# Handles files with specialization metadata rows at top

# Load required libraries
library(readr)
library(dplyr)
library(stringr)

# =============================================================================
# CONFIGURATION
# =============================================================================

# Files to process
input_files <- c("Diatoms_A_spring_final.csv", "Diatoms_A_spring_finalPA.csv")

cat("=== DIATOM SHORT NAMES GENERATOR ===\n\n")

# Function to create short names from Latin diatom names
create_short_name <- function(latin_name) {
  if (is.na(latin_name) || latin_name == "" || latin_name == "Species") {
    return(latin_name)  # Keep "Species" as is for the first column
  }
  
  # Split the Latin name into genus and species
  parts <- str_split(latin_name, " ")[[1]]
  
  if (length(parts) >= 2) {
    genus <- parts[1]
    species <- parts[2]
    
    # Take first 3 letters of genus and species
    genus_short <- str_to_title(str_sub(genus, 1, 3))
    species_short <- str_to_lower(str_sub(species, 1, 3))
    
    # Combine them and make 4th letter uppercase
    short_name <- paste0(genus_short, species_short)
    # Make the 4th character uppercase
    if (nchar(short_name) >= 4) {
      substr(short_name, 4, 4) <- toupper(substr(short_name, 4, 4))
    }
    
    return(short_name)
  } else {
    return(latin_name)  # Return original if it doesn't have both genus and species
  }
}

# Process each file
for (filename in input_files) {
  cat("Processing file:", filename, "\n")
  
  # Check if file exists
  if (!file.exists(filename)) {
    cat("⚠️  File not found:", filename, "- skipping\n\n")
    next
  }
  
  # Read the CSV file
  diatom_data <- read.csv(filename, fileEncoding = "CP1252", 
                          stringsAsFactors = FALSE, check.names = FALSE)
  
  cat("Original dimensions:", nrow(diatom_data), "rows ×", ncol(diatom_data), "columns\n")
  
  # Create short names from column headers (species names)
  cat("Generating short names from column headers...\n")
  column_names <- colnames(diatom_data)
  short_names <- sapply(column_names, create_short_name, USE.NAMES = FALSE)
  
  # Create a new row with short names - this will become row 3
  short_names_row <- short_names
  short_names_row[1] <- "Short_names"  # Label for the first column (sample IDs)
  
  # Insert the short names as a new row after existing data
  # Convert to data frame row format
  short_names_df <- data.frame(t(short_names_row), stringsAsFactors = FALSE, check.names = FALSE)
  colnames(short_names_df) <- column_names
  
  # Insert short names as 3rd row
  if (nrow(diatom_data) >= 2) {
    # Take first 2 rows (specialization data)  
    top_rows <- diatom_data[1:2, ]
    
    # Add remaining data rows
    if (nrow(diatom_data) > 2) {
      remaining_rows <- diatom_data[3:nrow(diatom_data), ]
      new_data <- rbind(top_rows, short_names_df, remaining_rows)
    } else {
      new_data <- rbind(top_rows, short_names_df)
    }
  } else {
    # If less than 2 rows, just add short names as next row
    new_data <- rbind(diatom_data, short_names_df)
  }
  
  # Check for duplicate short names (excluding first column which contains sample IDs)
  species_short_names <- short_names[-1]  # Remove first element (sample ID column)
  species_column_names <- column_names[-1]  # Remove first element
  
  duplicates <- species_short_names[duplicated(species_short_names)]
  
  cat("Short names generated:", length(species_short_names), "\n")
  if (length(duplicates) > 0) {
    cat("⚠️  Duplicate short names found:", length(unique(duplicates)), "\n")
    cat("Duplicates:", paste(unique(duplicates), collapse = ", "), "\n")
  } else {
    cat("✅ No duplicates found!\n")
  }
  
  # Create output filename
  base_name <- tools::file_path_sans_ext(filename)
  output_filename <- paste0(base_name, "_with_short_names.csv")
  
  # Save the updated data
  write.csv(new_data, output_filename, row.names = FALSE, fileEncoding = "UTF-8")
  
  cat("✅ Updated file saved as:", output_filename, "\n")
  
  # Show structure of updated file
  cat("New file structure:\n")
  cat("Row 1 (Specialization):", new_data[1, 1], "\n")
  if (nrow(new_data) >= 2) cat("Row 2 (Descriptions):", new_data[2, 1], "\n")
  if (nrow(new_data) >= 3) cat("Row 3 (Short names):", new_data[3, 1], "\n")
  if (nrow(new_data) >= 4) cat("Row 4+ (Data):", new_data[4, 1], "\n")
  
  # Show first few short names examples (skip first column which is sample IDs)
  cat("First 5 short name examples:\n")
  example_indices <- 2:min(6, length(column_names))  # Start from column 2 (skip sample ID column)
  for (i in example_indices) {
    cat(sprintf("%-30s -> %s\n", column_names[i], short_names[i]))
  }
  
  cat("\n", paste(rep("-", 50), collapse = ""), "\n\n")
}

cat("🎉 All files processed successfully!\n")
cat("Files created:\n")
for (filename in input_files) {
  if (file.exists(filename)) {
    base_name <- tools::file_path_sans_ext(filename)
    output_filename <- paste0(base_name, "_with_short_names.csv")
    cat("- ", output_filename, "\n")
  }
}

