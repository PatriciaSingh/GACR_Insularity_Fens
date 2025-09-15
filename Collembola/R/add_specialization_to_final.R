# Simple script to add specialization row to Collembola_final.csv
# Run this script from Collembola/data directory

library(readr)
library(dplyr)
library(stringr)

# Check current directory
cat("Current directory:", getwd(), "\n")

# =============================================================================
# File paths - all files are in current directory (Collembola/data)
# =============================================================================

SPEC_FILE <- "Collembola_Species_Specializace_Short_Names.csv"  # Has short_name + Specializace
FINAL_DATA_FILE <- "Collembola_finalPA.csv"  # Your final data with short column names
OUTPUT_FILE <- "Collembola_finalPA_with_specialization.csv"  # Save in same directory

cat("Looking for files:\n")
cat("- Specialization source:", SPEC_FILE, "\n")
cat("- Final data:", FINAL_DATA_FILE, "\n")

# Check if files exist
if (!file.exists(SPEC_FILE)) {
  stop("ERROR: Cannot find ", SPEC_FILE)
}

if (!file.exists(FINAL_DATA_FILE)) {
  stop("ERROR: Cannot find ", FINAL_DATA_FILE)
}

# =============================================================================
# Read the data
# =============================================================================

# Read specialization data (should have short_name and Specializace columns)
cat("\nReading specialization data...\n")
spec_data <- read_csv(SPEC_FILE, show_col_types = FALSE)

# Check required columns
if (!"short_name" %in% colnames(spec_data)) {
  stop("ERROR: No 'short_name' column found!")
}

if (!"Specialization" %in% colnames(spec_data)) {
  stop("ERROR: No 'Specializace' column found!")
}

cat("Found", nrow(spec_data), "species with specialization data\n")
cat("Columns available:", paste(colnames(spec_data), collapse = ", "), "\n")

# Create lookup: short_name -> specialization
short_to_spec <- setNames(spec_data$Specialization, spec_data$short_name)
short_to_spec <- short_to_spec[!is.na(names(short_to_spec))]

cat("Created lookup table for", length(short_to_spec), "species\n")

# Read final data
cat("\nReading final data file...\n")
final_data <- read_csv(FINAL_DATA_FILE, show_col_types = FALSE)

cat("Final data:", nrow(final_data), "rows x", ncol(final_data), "columns\n")

# =============================================================================
# Create specialization row
# =============================================================================

column_names <- colnames(final_data)
locality_col <- column_names[1]
species_short_names <- column_names[-1]

cat("Locality column:", locality_col, "\n")
cat("Species columns:", length(species_short_names), "\n")

# Build specialization row
specialization_row <- c("Specialization")  # First column header for specialization
matched_count <- 0

for (short_name in species_short_names) {
  if (short_name %in% names(short_to_spec)) {
    spec_value <- short_to_spec[short_name]
    specialization_row <- c(specialization_row, if(is.na(spec_value)) "Unknown" else spec_value)
    matched_count <- matched_count + 1
  } else {
    specialization_row <- c(specialization_row, "Unknown")
  }
}

cat("\nMatched", matched_count, "out of", length(species_short_names), "species (", 
    round(matched_count/length(species_short_names)*100, 1), "%)\n")

# =============================================================================
# Combine data and save
# =============================================================================

# Create specialization row as data frame
spec_row_df <- data.frame(matrix(specialization_row, nrow = 1))
colnames(spec_row_df) <- column_names

# Convert final data to character temporarily for binding
final_data_char <- final_data
final_data_char[, -1] <- lapply(final_data_char[, -1], as.character)

# Combine: specialization row first, then original data
final_with_spec <- bind_rows(spec_row_df, final_data_char)

# Convert back to numeric (except first row)
for (i in 2:ncol(final_with_spec)) {
  final_with_spec[[i]][-1] <- as.numeric(final_with_spec[[i]][-1])
}

# Save result
write_csv(final_with_spec, OUTPUT_FILE)

# =============================================================================
# Results
# =============================================================================

cat("\n", paste(rep("=", 50), collapse = ""), "\n")
cat("RESULTS\n")
cat(paste(rep("=", 50), collapse = ""), "\n")

# Show specialization summary
spec_counts <- table(specialization_row[-1], useNA = "ifany")
cat("Specialization summary:\n")
for (spec_type in names(spec_counts)) {
  cat("  -", spec_type, ":", spec_counts[spec_type], "species\n")
}

cat("\nFiles:\n")
cat("- Input:", FINAL_DATA_FILE, "\n")
cat("- Output:", OUTPUT_FILE, "\n")
cat("- Rows added: 1 (specialization row)\n")
cat("- Total rows now:", nrow(final_with_spec), "\n")

# Show preview
cat("\nPreview (first 3 rows, first 5 columns):\n")
preview <- final_with_spec[1:min(3, nrow(final_with_spec)), 1:min(5, ncol(final_with_spec))]
print(preview)

cat("\nDone! Your file with specialization row is:", OUTPUT_FILE, "\n")

