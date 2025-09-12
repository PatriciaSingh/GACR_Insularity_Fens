# add_specialization_rows.R
# R Script to add Specialization row to Oribatida data files
# Matches species column names to specialization data and adds as first row

# Set working directory
setwd("Oribatida/data")

# Load required libraries
library(readr)
library(dplyr)

# Read the files
file1 <- read_csv("Oribatida_final.csv")
file2 <- read_csv("Oribatida_finalPA.csv") 
specialization_data <- read_csv("Oribatida_Species_Specializace_Short_names.csv")

# Function to add specialization row
add_specialization_row <- function(data_file, spec_data) {
  
  # Get column names (excluding first column "Locality")
  species_columns <- colnames(data_file)[-1]
  
  # Create specialization row
  spec_row <- data.frame(Locality = "Specialization")
  
  # For each species column, find matching specialization
  for(col in species_columns) {
    # Find matching specialization value
    spec_value <- spec_data$Specialization[spec_data$short_name == col]
    
    # If no match found, use NA
    if(length(spec_value) == 0) {
      spec_row[[col]] <- NA
    } else {
      spec_row[[col]] <- spec_value[1]  # Take first match
    }
  }
  
  # Add specialization row as first row
  result <- rbind(spec_row, data_file)
  return(result)
}

# Add specialization rows to both files
file1_with_spec <- add_specialization_row(file1, specialization_data)
file2_with_spec <- add_specialization_row(file2, specialization_data)

# Save the updated files
write_csv(file1_with_spec, "Oribatida_final_with_specialization.csv")
write_csv(file2_with_spec, "Oribatida_finalPA_with_specialization.csv")

# Print summary
cat("Files updated successfully!\n")
cat("File 1 rows:", nrow(file1_with_spec), "\n")
cat("File 2 rows:", nrow(file2_with_spec), "\n")

# Show first few rows to verify
cat("\nFirst few rows of updated file 1:\n")
print(file1_with_spec[1:3, 1:6])

# Optional: Show which species have specialization data
matched_species <- intersect(colnames(file1)[-1], specialization_data$short_name)
unmatched_species <- setdiff(colnames(file1)[-1], specialization_data$short_name)

cat("\nMatched species:", length(matched_species), "\n")
cat("Unmatched species:", length(unmatched_species), "\n")
if(length(unmatched_species) > 0) {
  cat("Unmatched species names:", paste(head(unmatched_species, 10), collapse = ", "), "\n")
}