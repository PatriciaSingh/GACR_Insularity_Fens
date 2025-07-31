# Add species trait information to all spider files
# Script: add_trait_rows.R
# Purpose: Add family, specialization, dispersal, and protection status rows to all files
# Load required libraries
library(readr)
library(dplyr)

# Set working directory
setwd("Spiders/data")

# Step 1: Read the trait lookup file and create lookup tables
cat("Reading trait lookup file...\n")
trait_data <- read_csv("Spides_Species_Specializace_Sireni_Ohrozeni.csv", show_col_types = FALSE)

# Create lookup tables for each trait
family_lookup <- setNames(trait_data$Family, trait_data$Species)
specialization_lookup <- setNames(trait_data$Specialization, trait_data$Species)
dispersal_lookup <- setNames(trait_data$Disperzal, trait_data$Species)  
protection_lookup <- setNames(trait_data$Protection_Status, trait_data$Species)

cat("Created lookup tables for:", nrow(trait_data), "species\n")

# Function to add trait rows to a file
add_trait_rows <- function(file_path) {
  cat("Processing:", file_path, "\n")
  
  # Read the file
  data <- read_csv(file_path, col_names = FALSE, col_types = cols(.default = "c"), show_col_types = FALSE)
  
  # Get species names from row 1 (columns 5+, where species data starts)
  species_names <- as.character(data[1, 5:ncol(data)])
  
  # Create trait rows by matching species names
  family_row <- sapply(species_names, function(name) {
    if (!is.na(name) && name %in% names(family_lookup)) {
      return(family_lookup[[name]])
    } else {
      return(NA)
    }
  })
  
  specialization_row <- sapply(species_names, function(name) {
    if (!is.na(name) && name %in% names(specialization_lookup)) {
      return(specialization_lookup[[name]])
    } else {
      return(NA)
    }
  })
  
  dispersal_row <- sapply(species_names, function(name) {
    if (!is.na(name) && name %in% names(dispersal_lookup)) {
      return(dispersal_lookup[[name]])
    } else {
      return(NA)
    }
  })
  
  protection_row <- sapply(species_names, function(name) {
    if (!is.na(name) && name %in% names(protection_lookup)) {
      return(protection_lookup[[name]])
    } else {
      return(NA)
    }
  })
  
  # Create complete trait rows (with site info columns filled with appropriate labels)
  n_cols <- ncol(data)
  
  # Family row
  family_complete <- c(rep(NA, 4), family_row)
  family_complete <- c(family_complete, rep(NA, n_cols - length(family_complete)))
  family_complete[1] <- "Family"
  
  # Specialization row
  specialization_complete <- c(rep(NA, 4), specialization_row)
  specialization_complete <- c(specialization_complete, rep(NA, n_cols - length(specialization_complete)))
  specialization_complete[1] <- "Specialization"
  
  # Dispersal row  
  dispersal_complete <- c(rep(NA, 4), dispersal_row)
  dispersal_complete <- c(dispersal_complete, rep(NA, n_cols - length(dispersal_complete)))
  dispersal_complete[1] <- "Dispersal"
  
  # Protection row
  protection_complete <- c(rep(NA, 4), protection_row)
  protection_complete <- c(protection_complete, rep(NA, n_cols - length(protection_complete)))
  protection_complete[1] <- "Protection_Status"
  
  # Convert to data frame rows
  family_df <- as.data.frame(t(family_complete), stringsAsFactors = FALSE)
  specialization_df <- as.data.frame(t(specialization_complete), stringsAsFactors = FALSE)
  dispersal_df <- as.data.frame(t(dispersal_complete), stringsAsFactors = FALSE)
  protection_df <- as.data.frame(t(protection_complete), stringsAsFactors = FALSE)
  
  # Set column names to match original data
  names(family_df) <- names(data)
  names(specialization_df) <- names(data)
  names(dispersal_df) <- names(data)
  names(protection_df) <- names(data)
  
  # Insert trait rows after the short names (row 2)
  # New structure: Row 1=Species, Row 2=Short names, Row 3=Family, Row 4=Specialization, Row 5=Dispersal, Row 6=Protection, Row 7+=Data
  updated_data <- rbind(
    data[1:2, ],           # Species names and short names
    family_df,             # Family
    specialization_df,     # Specialization
    dispersal_df,          # Dispersal
    protection_df,         # Protection status
    data[3:nrow(data), ]   # Original data
  )
  
  # Save back to the same file
  write_csv(updated_data, file_path, col_names = FALSE)
  
  # Count matches
  matches <- sum(!is.na(family_row))
  cat("  - Added trait rows for", matches, "out of", length(species_names), "species\n")
  
  return(updated_data)
}

# List all files to process
all_files <- c(
  # Original abundance files (CSV format)
  "Spider_pasti_cerven_final.csv",
  "Spider_pasti_kveten_final.csv", 
  "Spider_smyk_cerven_final.csv",
  "Spider_smyk_kveten_final.csv",
  
  # PA files
  "Spider_pasti_cerven_finalPA.csv",
  "Spider_pasti_kveten_finalPA.csv",
  "Spider_smyk_cerven_finalPA.csv", 
  "Spider_smyk_kveten_finalPA.csv",
  
  # Combined PA files
  "Spider_pasti_combinedPA.csv",
  "Spider_smyk_combinedPA.csv",
  "Spider_cerven_combinedPA.csv",
  "Spider_kveten_combinedPA.csv", 
  "Spider_complete_combinedPA.csv"
)

# Process each file
cat("\n", paste(rep("=", 60), collapse=""), "\n")
cat("ADDING TRAIT ROWS TO ALL FILES\n")
cat(paste(rep("=", 60), collapse=""), "\n")

for (file in all_files) {
  if (file.exists(file)) {
    add_trait_rows(file)
  } else {
    cat("Warning: File not found:", file, "\n")
  }
}

cat("\n", paste(rep("=", 60), collapse=""), "\n")
cat("TRAIT ADDITION COMPLETE!\n")
cat("Added trait information to", length(all_files), "files\n")
cat("New file structure:\n")
cat("Row 1: Species names\n")
cat("Row 2: Short names\n") 
cat("Row 3: Family\n")
cat("Row 4: Specialization\n")
cat("Row 5: Dispersal (Disperzat)\n") 
cat("Row 6: Protection Status\n")
cat("Row 7+: Site data\n")
cat(paste(rep("=", 60), collapse=""), "\n")

