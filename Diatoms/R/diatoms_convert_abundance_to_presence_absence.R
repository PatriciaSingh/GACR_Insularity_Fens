# convert_diatom_abundance_with_short_names.R
# Script to convert diatom abundance data to presence-absence
# Handles the structure with metadata rows and short names row at the top

# Read the CSV file with CP1252 encoding
abundance_data <- read.csv("Diatoms_A_spring_final_with_short_names.csv",
                           fileEncoding = "CP1252", 
                           stringsAsFactors = FALSE,
                           check.names = FALSE)

cat("Original data loaded successfully!\n")
cat("Dimensions:", nrow(abundance_data), "rows ×", ncol(abundance_data), "columns\n")

# Display the structure to understand what we have
cat("\nFirst few rows to understand structure:\n")
print(abundance_data[1:4, 1:5])

# The structure now has:
# Row 1: Specialization categories
# Row 2: Specialization descriptions  
# Row 3: Short names
# Row 4+: Abundance data

# Create a copy to work with
presence_absence <- abundance_data

cat("\nKeeping metadata rows and converting abundance data:\n")
cat("Row 1 (Specialization):", abundance_data[1, 1], "- keeping as is\n")
cat("Row 2 (Specialization descriptions):", abundance_data[2, 1], "- keeping as is\n")
cat("Row 3 (Short names):", abundance_data[3, 1], "- keeping as is\n")
cat("Rows 4+: Converting abundance data to presence-absence\n")

# Convert only rows 4 onwards (the actual abundance data)
if(nrow(abundance_data) > 3) {
  for(row in 4:nrow(abundance_data)) {
    for(col in 2:ncol(abundance_data)) {
      # Convert to numeric (this will handle any text values)
      numeric_value <- as.numeric(abundance_data[row, col])
      
      # Convert to presence-absence: >0 becomes 1, 0 or NA becomes 0
      presence_absence[row, col] <- ifelse(is.na(numeric_value) | numeric_value == 0, 0, 1)
    }
  }
}

cat("\nConversion to presence-absence completed!\n")

# Show sample of the final data structure
cat("Final data structure:\n")
cat("Row 1 (Specialization):\n")
print(presence_absence[1, 1:5])
cat("Row 2 (Descriptions):\n") 
print(presence_absence[2, 1:5])
cat("Row 3 (Short names):\n")
print(presence_absence[3, 1:5])
cat("Row 4+ (Presence-absence data, first 3 samples):\n")
print(presence_absence[4:6, 1:5])

# Summary statistics (only for abundance rows - starting from row 4)
if(nrow(presence_absence) > 3) {
  abundance_rows <- presence_absence[4:nrow(presence_absence), ]
  cat("\nSummary (abundance data only):\n")
  cat("Number of samples (rows):", nrow(abundance_rows), "\n")
  cat("Number of species (columns - 1):", ncol(abundance_rows) - 1, "\n")
  
  # Count total presences in abundance data only
  numeric_cols <- abundance_rows[, -1]  # All columns except first column
  total_presences <- sum(sapply(numeric_cols, function(x) sum(as.numeric(x), na.rm = TRUE)))
  total_cells <- nrow(abundance_rows) * (ncol(abundance_rows) - 1)
  cat("Total presences:", total_presences, "out of", total_cells, "cells\n")
  cat("Percentage of presences:", round(100 * total_presences / total_cells, 2), "%\n")
}

# Save the results
output_file <- "Diatoms_A_spring_final_PA_with_short_names.csv"
write.csv(presence_absence, output_file, row.names = FALSE, fileEncoding = "UTF-8")

cat("\n✓ Presence-absence data saved to:", output_file, "\n")
cat("Script completed successfully!\n")

