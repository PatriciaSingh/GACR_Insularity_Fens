# Add short names to data matrix
# Script: add_short_names_to_data_matrix.R
# Purpose: Replace full species names with short names in data matrix

library(readr)
library(dplyr)

# =============================================================================
# CONFIGURATION - CHANGE THIS FOR DIFFERENT TAXA
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

# File naming pattern (adjust if your files have different naming conventions)
SHORT_NAMES_FILE <- paste0(TAXA_NAME, "_Species_Short_Names.csv")  # Generated from previous script
ORIGINAL_DATA_FILE <- paste0(TAXA_NAME, "_original_clean_names.csv")  # Your data matrix
OUTPUT_FILE <- paste0(TAXA_NAME, "_with_short_names.csv")  # Output file

# Optional: Custom file names (uncomment and modify if needed)
# SHORT_NAMES_FILE <- "custom_short_names_file.csv"
# ORIGINAL_DATA_FILE <- "custom_data_matrix.csv"
# OUTPUT_FILE <- "custom_output.csv"

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

# Create a lookup table: full name -> short name
lookup_table <- setNames(short_names_df$short_name, short_names_df$Species)
# Remove any NA values from lookup
lookup_table <- lookup_table[!is.na(lookup_table)]

cat("Loaded", length(lookup_table), "short name mappings\n")

# Read the original data matrix
cat("Reading original data from:", ORIGINAL_DATA_FILE, "\n")
original_data <- read_csv(ORIGINAL_DATA_FILE, show_col_types = FALSE)

cat("Original data dimensions:", nrow(original_data), "rows x", ncol(original_data), "columns\n")
cat("First few column names:\n")
print(head(colnames(original_data), 10))

# Get the column names (which should include species names)
original_colnames <- colnames(original_data)

# The first column should be locality, so we start from column 2
locality_col <- original_colnames[1]
species_cols <- original_colnames[-1]  # All columns except the first

cat("\nLocality column:", locality_col, "\n")
cat("Number of species columns:", length(species_cols), "\n")

# Create new column names by replacing full names with short names
new_colnames <- original_colnames
matched_count <- 0
unmatched_species <- c()

# Process each species column (skip the first locality column)
for (i in 2:length(original_colnames)) {
  full_name <- original_colnames[i]
  
  # Try to find exact match first
  if (full_name %in% names(lookup_table)) {
    short_name <- lookup_table[full_name]
    new_colnames[i] <- short_name
    matched_count <- matched_count + 1
  } else {
    # Try to find partial matches or similar names
    # Look for species that might have slightly different formatting
    possible_matches <- names(lookup_table)[grepl(gsub("\\.", " ", full_name), names(lookup_table), ignore.case = TRUE)]
    
    if (length(possible_matches) == 1) {
      short_name <- lookup_table[possible_matches[1]]
      new_colnames[i] <- short_name
      matched_count <- matched_count + 1
      cat("Partial match found:", full_name, "->", possible_matches[1], "->", short_name, "\n")
    } else {
      # Keep original name if no match found
      unmatched_species <- c(unmatched_species, full_name)
    }
  }
}

# Update column names
colnames(original_data) <- new_colnames

# Display results
cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("COLUMN NAME REPLACEMENT RESULTS\n")
cat(paste(rep("=", 60), collapse = ""), "\n")
cat("Total species columns:", length(species_cols), "\n")
cat("Successfully matched:", matched_count, "\n")
cat("Unmatched species:", length(unmatched_species), "\n")
cat("Match rate:", round(matched_count/length(species_cols)*100, 1), "%\n\n")

if (length(unmatched_species) > 0) {
  cat("⚠️  UNMATCHED SPECIES (kept original names):\n")
  for (species in unmatched_species) {
    cat("-", species, "\n")
  }
  cat("\n💡 These might need manual checking or the species names might be formatted differently\n\n")
}

# Show some examples of the transformation
cat("📝 COLUMN NAME TRANSFORMATION EXAMPLES:\n")
example_indices <- c(2:min(6, ncol(original_data)))  # Show first few species columns
for (i in example_indices) {
  if (original_colnames[i] != new_colnames[i]) {
    cat("✅", original_colnames[i], "->", new_colnames[i], "\n")
  } else {
    cat("⚪", original_colnames[i], "(unchanged)\n")
  }
}

# Save the updated data
write_csv(original_data, OUTPUT_FILE)
cat("\n✅ Data with short names saved to:", OUTPUT_FILE, "\n")

# Display data preview
cat("\n📊 DATA PREVIEW (first 5 rows and columns):\n")
preview_data <- original_data[1:min(5, nrow(original_data)), 1:min(6, ncol(original_data))]
print(preview_data)

# Summary
cat("\n📋 SUMMARY:\n")
cat("- Input file:", ORIGINAL_DATA_FILE, "\n")
cat("- Short names source:", SHORT_NAMES_FILE, "\n")
cat("- Output file:", OUTPUT_FILE, "\n")
cat("- Localities:", nrow(original_data), "\n")
cat("- Species columns:", length(species_cols), "\n")
cat("- Successfully converted:", matched_count, "species names\n")

if (length(unmatched_species) == 0) {
  cat("- ✅ All species names successfully converted!\n")
} else {
  cat("- ⚠️ ", length(unmatched_species), "species names need manual review\n")
}

# Return to original directory
setwd("../..")

cat("\n🎉 Processing complete!\n")

# Optional: Create a mapping report
cat("\nCreating mapping report...\n")
setwd(taxa_path)

mapping_report <- data.frame(
  Original_Name = original_colnames[-1],  # Exclude locality column
  Short_Name = new_colnames[-1],
  Matched = original_colnames[-1] != new_colnames[-1],
  stringsAsFactors = FALSE
)

# Add specialization info to mapping report if available
if (use_specializace && length(specializace_info) > 0) {
  mapping_report$Specializace <- specializace_info
}

report_filename <- paste0(TAXA_NAME, "_name_mapping_report.csv")
write_csv(mapping_report, report_filename)
cat("📄 Mapping report saved to:", report_filename, "\n")

setwd("../..")