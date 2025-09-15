# Add short names to data matrix - FIXED VERSION
# Script: add_short_names_fixed.R
# Purpose: Replace full species names with short names in data matrix (ONE OUTPUT ONLY)

library(readr)
library(dplyr)
library(stringr)

# =============================================================================
# CONFIGURATION
# =============================================================================

# Get taxa name from command line argument or set manually
args <- commandArgs(trailingOnly = TRUE)

if (length(args) > 0) {
  TAXA_NAME <- args[1]
  cat("Using taxa from command line argument:", TAXA_NAME, "\n")
} else {
  # CHANGE THIS LINE for different taxa:
  TAXA_NAME <- "Collembola"  # Change to "Oribatida", "Araneae", "Diptera", etc.
  cat("Using taxa from script configuration:", TAXA_NAME, "\n")
}

# File names
SHORT_NAMES_FILE <- paste0(TAXA_NAME, "_Species_Short_Names.csv")  # Generated from previous script
ORIGINAL_DATA_FILE <- paste0(TAXA_NAME, "_original_clean_names.csv")  # Your data matrix
OUTPUT_FILE <- paste0(TAXA_NAME, "_with_short_names.csv")  # Output file

# =============================================================================
# MAIN PROCESSING
# =============================================================================

# Set working directory
taxa_path <- file.path(TAXA_NAME, "data")
if (!dir.exists(taxa_path)) {
  stop(paste("Directory", taxa_path, "does not exist!"))
}
setwd(taxa_path)

cat("Working in directory:", getwd(), "\n")

# Check if required files exist
if (!file.exists(SHORT_NAMES_FILE)) {
  stop(paste("Short names file not found:", SHORT_NAMES_FILE, 
             "\nPlease run the short names generation script first!"))
}

if (!file.exists(ORIGINAL_DATA_FILE)) {
  stop(paste("Original data file not found:", ORIGINAL_DATA_FILE))
}

# Read the short names lookup table
cat("Reading short names from:", SHORT_NAMES_FILE, "\n")
short_names_df <- read_csv(SHORT_NAMES_FILE, show_col_types = FALSE)

# Check if Specializace column exists
if ("Specializace" %in% colnames(short_names_df)) {
  use_specializace <- TRUE
  cat("✅ Found 'Specializace' column - will include specialization info\n")
} else {
  use_specializace <- FALSE
  cat("ℹ️  No 'Specializace' column found - proceeding with short names only\n")
}

# CLEAN the species names by removing extra whitespace and non-breaking spaces
short_names_df$Species_Clean <- str_trim(str_replace_all(short_names_df$Species, "[\u00A0\u2000-\u200B\u2028\u2029]", " "))
short_names_df$Species_Clean <- str_squish(short_names_df$Species_Clean)  # Remove multiple spaces

cat("🧹 Cleaned species names from whitespace issues\n")

# Create lookup tables using CLEAN names
short_name_lookup <- setNames(short_names_df$short_name, short_names_df$Species_Clean)
short_name_lookup <- short_name_lookup[!is.na(short_name_lookup)]

if (use_specializace) {
  specializace_lookup <- setNames(short_names_df$Specializace, short_names_df$Species_Clean)
  cat("📊 Loaded", length(short_name_lookup), "short name mappings and specialization data\n")
} else {
  cat("📊 Loaded", length(short_name_lookup), "short name mappings\n")
}

# Read the original data matrix
cat("📖 Reading original data from:", ORIGINAL_DATA_FILE, "\n")
original_data <- read_csv(ORIGINAL_DATA_FILE, show_col_types = FALSE)

cat("📏 Data dimensions:", nrow(original_data), "rows x", ncol(original_data), "columns\n")

# Get and clean the column names
original_colnames <- colnames(original_data)
locality_col <- original_colnames[1]
species_cols <- original_colnames[-1]

# Clean the column names the same way
species_cols_clean <- str_trim(str_replace_all(species_cols, "[\u00A0\u2000-\u200B\u2028\u2029]", " "))
species_cols_clean <- str_squish(species_cols_clean)

cat("🏠 Locality column:", locality_col, "\n")
cat("🦟 Species columns:", length(species_cols), "\n")

# Create new column names and track specializations
new_colnames <- original_colnames
matched_count <- 0
unmatched_species <- c()
specialization_row <- c(locality_col)  # Start with locality column

# Process each species column (skip the first locality column)
for (i in 1:length(species_cols)) {
  original_name <- species_cols[i]
  clean_name <- species_cols_clean[i]
  
  # Try to find exact match with cleaned names
  if (clean_name %in% names(short_name_lookup)) {
    short_name <- short_name_lookup[clean_name]
    new_colnames[i + 1] <- short_name  # +1 because we skip locality column
    matched_count <- matched_count + 1
    
    # Add specialization if available
    if (use_specializace) {
      spec_value <- specializace_lookup[clean_name]
      specialization_row <- c(specialization_row, if(is.na(spec_value)) "Unknown" else spec_value)
    }
  } else {
    # Try partial matching with cleaned names
    possible_matches <- names(short_name_lookup)[grepl(clean_name, names(short_name_lookup), ignore.case = TRUE, fixed = TRUE)]
    
    if (length(possible_matches) == 1) {
      short_name <- short_name_lookup[possible_matches[1]]
      new_colnames[i + 1] <- short_name
      matched_count <- matched_count + 1
      cat("📝 Partial match:", original_name, "->", possible_matches[1], "->", short_name, "\n")
      
      if (use_specializace) {
        spec_value <- specializace_lookup[possible_matches[1]]
        specialization_row <- c(specialization_row, if(is.na(spec_value)) "Unknown" else spec_value)
      }
    } else {
      # Keep original name if no match found
      unmatched_species <- c(unmatched_species, original_name)
      if (use_specializace) {
        specialization_row <- c(specialization_row, "No_match")
      }
    }
  }
}

# Update column names
colnames(original_data) <- new_colnames

# Add specialization row if available
if (use_specializace) {
  # Create specialization row as first row
  spec_row_df <- data.frame(matrix(specialization_row, nrow = 1))
  colnames(spec_row_df) <- new_colnames
  
  # Combine with original data
  original_data <- bind_rows(spec_row_df, original_data)
  cat("✅ Added specialization row as first row of data\n")
}

# Display results
cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("COLUMN NAME REPLACEMENT RESULTS\n")
cat(paste(rep("=", 60), collapse = ""), "\n")
cat("Total species columns:", length(species_cols), "\n")
cat("Successfully matched:", matched_count, "\n")
cat("Unmatched species:", length(unmatched_species), "\n")
cat("Match rate:", round(matched_count/length(species_cols)*100, 1), "%\n\n")

if (length(unmatched_species) > 0 && length(unmatched_species) <= 10) {
  cat("⚠️  UNMATCHED SPECIES:\n")
  for (species in unmatched_species) {
    cat("-", species, "\n")
  }
} else if (length(unmatched_species) > 10) {
  cat("⚠️  UNMATCHED SPECIES (showing first 10):\n")
  for (species in unmatched_species[1:10]) {
    cat("-", species, "\n")
  }
  cat("... and", length(unmatched_species) - 10, "more\n")
}

# Show specialization summary if available
if (use_specializace) {
  spec_counts <- table(specialization_row[-1], useNA = "ifany")  # Exclude locality column
  cat("\n🏷️  SPECIALIZATION SUMMARY:\n")
  for (spec_type in names(spec_counts)) {
    cat("   -", spec_type, ":", spec_counts[spec_type], "species\n")
  }
}

# Save the updated data (SINGLE OUTPUT FILE)
write_csv(original_data, OUTPUT_FILE)
cat("\n✅ Data with short names", if(use_specializace) "and specialization row" else "", "saved to:", OUTPUT_FILE, "\n")

# Display data preview
cat("\n📊 DATA PREVIEW:\n")
if (use_specializace) {
  cat("First row = Specialization, Second row onwards = Data\n")
  preview_data <- original_data[1:min(3, nrow(original_data)), 1:min(6, ncol(original_data))]
} else {
  preview_data <- original_data[1:min(5, nrow(original_data)), 1:min(6, ncol(original_data))]
}
print(preview_data)

# Summary
cat("\n📋 FINAL SUMMARY:\n")
cat("- Input matrix:", ORIGINAL_DATA_FILE, "\n")
cat("- Short names source:", SHORT_NAMES_FILE, "\n")
cat("- Output file:", OUTPUT_FILE, "\n")
cat("- Localities:", if(use_specializace) nrow(original_data) - 1 else nrow(original_data), "\n")
cat("- Species columns:", length(species_cols), "\n")
cat("- Successfully converted:", matched_count, "species names (", round(matched_count/length(species_cols)*100, 1), "%)\n")

if (use_specializace) {
  cat("- ✅ Specialization row added as first row\n")
}

if (length(unmatched_species) == 0) {
  cat("- ✅ All species names successfully converted!\n")
} else {
  cat("- ⚠️ ", length(unmatched_species), "species names kept as original\n")
}

# Return to original directory
setwd("../..")

cat("\n🎉 Processing complete! Single output file created.\n")

