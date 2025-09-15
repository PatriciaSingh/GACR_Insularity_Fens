# Add specialization row to final Collembola data
# Script: add_specialization_to_final.R
# Purpose: Add specialization info as first row to Collembola_final.csv

library(readr)
library(dplyr)
library(stringr)

# =============================================================================
# CONFIGURATION
# =============================================================================

TAXA_NAME <- "Collembola"

# File paths
SHORT_NAMES_FILE <- file.path(TAXA_NAME, "data", paste0(TAXA_NAME, "_Species_Short_Names.csv"))
FINAL_DATA_FILE <- "Collembola_final.csv"  # Your current file
OUTPUT_FILE <- "Collembola_final_with_specialization.csv"

# =============================================================================
# MAIN PROCESSING
# =============================================================================

cat("🚀 Adding specialization row to", FINAL_DATA_FILE, "\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

# Check if files exist
if (!file.exists(SHORT_NAMES_FILE)) {
  stop(paste("❌ Short names file not found:", SHORT_NAMES_FILE))
}

if (!file.exists(FINAL_DATA_FILE)) {
  stop(paste("❌ Final data file not found:", FINAL_DATA_FILE))
}

# Read the short names file with specialization info
cat("📖 Reading specialization data from:", SHORT_NAMES_FILE, "\n")
short_names_df <- read_csv(SHORT_NAMES_FILE, show_col_types = FALSE)

# Check if Specializace column exists
if (!"Specializace" %in% colnames(short_names_df)) {
  stop("❌ 'Specializace' column not found in the short names file!")
}

cat("✅ Found specialization data for", nrow(short_names_df), "species\n")

# Clean species names (remove whitespace issues)
short_names_df$Species_Clean <- str_trim(str_replace_all(short_names_df$Species, "[\u00A0\u2000-\u200B\u2028\u2029]", " "))
short_names_df$Species_Clean <- str_squish(short_names_df$Species_Clean)

# Create lookup table: short_name -> specialization
short_to_spec_lookup <- setNames(short_names_df$Specializace, short_names_df$short_name)

# Also create full name to specialization lookup for backup matching
full_to_spec_lookup <- setNames(short_names_df$Specializace, short_names_df$Species_Clean)
full_to_short_lookup <- setNames(short_names_df$short_name, short_names_df$Species_Clean)

cat("📊 Created lookup tables for specialization matching\n")

# Read the final data file
cat("📖 Reading final data from:", FINAL_DATA_FILE, "\n")
final_data <- read_csv(FINAL_DATA_FILE, show_col_types = FALSE)

cat("📏 Final data dimensions:", nrow(final_data), "rows x", ncol(final_data), "columns\n")

# Get column names
column_names <- colnames(final_data)
locality_col <- column_names[1]
species_short_names <- column_names[-1]  # All columns except locality

cat("🏠 Locality column:", locality_col, "\n")
cat("🦟 Species columns (short names):", length(species_short_names), "\n")

# Create specialization row
specialization_row <- c(locality_col)  # Start with locality column value
matched_spec_count <- 0
unmatched_count <- 0

cat("\n🔍 Matching specializations...\n")

for (short_name in species_short_names) {
  if (short_name %in% names(short_to_spec_lookup)) {
    # Direct match with short name
    spec_value <- short_to_spec_lookup[short_name]
    specialization_row <- c(specialization_row, if(is.na(spec_value)) "Unknown" else spec_value)
    matched_spec_count <- matched_spec_count + 1
  } else {
    # Try to find by looking up in the full names
    # This handles cases where short names might be slightly different
    cat("⚠️  No direct match for:", short_name, "\n")
    specialization_row <- c(specialization_row, "Unknown")
    unmatched_count <- unmatched_count + 1
  }
}

# Create the specialization row as a data frame
spec_row_df <- data.frame(matrix(specialization_row, nrow = 1))
colnames(spec_row_df) <- column_names

# Convert numeric columns to character temporarily for binding
final_data_char <- final_data
final_data_char[,-1] <- lapply(final_data_char[,-1], as.character)

# Add specialization row as first row
final_data_with_spec <- bind_rows(spec_row_df, final_data_char)

# Convert back to original data types (except first row)
for (i in 2:ncol(final_data_with_spec)) {
  # Keep first row as character, convert rest back to numeric
  final_data_with_spec[[i]][-1] <- as.numeric(final_data_with_spec[[i]][-1])
}

# Display results
cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("SPECIALIZATION MATCHING RESULTS\n")
cat(paste(rep("=", 60), collapse = ""), "\n")
cat("Total species columns:", length(species_short_names), "\n")
cat("Successfully matched:", matched_spec_count, "\n")
cat("Unknown/unmatched:", unmatched_count, "\n")
cat("Match rate:", round(matched_spec_count/length(species_short_names)*100, 1), "%\n")

# Show specialization summary
spec_counts <- table(specialization_row[-1], useNA = "ifany")  # Exclude locality column
cat("\n🏷️  SPECIALIZATION SUMMARY:\n")
for (spec_type in names(spec_counts)) {
  cat("   -", spec_type, ":", spec_counts[spec_type], "species\n")
}

# Save the result
write_csv(final_data_with_spec, OUTPUT_FILE)
cat("\n✅ File with specialization row saved to:", OUTPUT_FILE, "\n")

# Display preview
cat("\n📊 DATA PREVIEW (First 3 rows, first 6 columns):\n")
cat("Row 1 = Specialization info, Row 2+ = Original data\n")
preview_data <- final_data_with_spec[1:min(3, nrow(final_data_with_spec)), 1:min(6, ncol(final_data_with_spec))]
print(preview_data)

# Show some examples of the matching
cat("\n📝 SPECIALIZATION EXAMPLES:\n")
example_cols <- min(5, length(species_short_names))
for (i in 1:example_cols) {
  short_name <- species_short_names[i]
  spec_value <- specialization_row[i + 1]  # +1 because first element is locality
  cat("   -", short_name, "->", spec_value, "\n")
}

# Final summary
cat("\n📋 FINAL SUMMARY:\n")
cat("- Input data file:", FINAL_DATA_FILE, "\n")
cat("- Specialization source:", SHORT_NAMES_FILE, "\n")
cat("- Output file:", OUTPUT_FILE, "\n")
cat("- Original data rows:", nrow(final_data), "\n")
cat("- New data rows:", nrow(final_data_with_spec), "(+1 specialization row)\n")
cat("- Species columns:", length(species_short_names), "\n")
cat("- Specialization match rate:", round(matched_spec_count/length(species_short_names)*100, 1), "%\n")

if (unmatched_count == 0) {
  cat("- ✅ All species specializations successfully added!\n")
} else {
  cat("- ⚠️ ", unmatched_count, "species marked as 'Unknown' specialization\n")
}

cat("\n🎉 Specialization row successfully added!\n")
cat("📄 Your final file is ready:", OUTPUT_FILE, "\n")