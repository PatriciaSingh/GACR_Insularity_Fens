# Diagnostic script for presence-absence combinations
# Script: diagnostic_pa_combinations.R
# Purpose: Verify that PA file combinations are working correctly
# Load required libraries
library(readr)
library(dplyr)

# Set working directory
setwd("Spiders/data")

# Function to read PA data (only the species data part)
read_pa_data <- function(file_path) {
  data <- read_csv(file_path, col_names = FALSE, col_types = cols(.default = "c"), show_col_types = FALSE)
  
  # Extract species names (row 2, columns 5+)
  species_names <- as.character(data[2, 5:ncol(data)])
  
  # Extract numeric data (rows 3+, columns 5+)
  numeric_data <- data[3:nrow(data), 5:ncol(data)]
  
  # Convert to numeric matrix
  numeric_matrix <- apply(numeric_data, c(1,2), function(x) {
    num_val <- suppressWarnings(as.numeric(x))
    ifelse(is.na(num_val), 0, num_val)
  })
  
  # Add species names as column names
  colnames(numeric_matrix) <- species_names
  
  return(numeric_matrix)
}

# Function to check combination logic
check_combination <- function(file1, file2, combined_file, combination_name) {
  cat("\n", paste(rep("=", 60), collapse=""), "\n")
  cat("CHECKING:", combination_name, "\n")
  cat("File 1:", file1, "\n")
  cat("File 2:", file2, "\n") 
  cat("Combined:", combined_file, "\n")
  cat(paste(rep("=", 60), collapse=""), "\n")
  
  # Read data
  data1 <- read_pa_data(file1)
  data2 <- read_pa_data(file2)
  combined <- read_pa_data(combined_file)
  
  # Check dimensions
  cat("Dimensions check:\n")
  cat("File 1:", nrow(data1), "sites x", ncol(data1), "species\n")
  cat("File 2:", nrow(data2), "sites x", ncol(data2), "species\n")
  cat("Combined:", nrow(combined), "sites x", ncol(combined), "species\n")
  
  # Check logic for first few sites and species
  cat("\nLogic verification (first 5 sites, first 10 species):\n")
  cat("Format: Site | Species | File1 | File2 | Combined | Correct?\n")
  cat(paste(rep("-", 60), collapse=""), "\n")
  
  logic_errors <- 0
  total_checks <- 0
  
  for (i in 1:min(5, nrow(data1))) {
    for (j in 1:min(10, ncol(data1))) {
      val1 <- data1[i, j]
      val2 <- data2[i, j]
      combined_val <- combined[i, j]
      expected <- ifelse(val1 > 0 | val2 > 0, 1, 0)
      is_correct <- (combined_val == expected)
      
      if (!is_correct) logic_errors <- logic_errors + 1
      total_checks <- total_checks + 1
      
      cat(sprintf("S%d | Sp%d | %d | %d | %d | %s\n", 
                  i, j, val1, val2, combined_val, 
                  ifelse(is_correct, "✓", "✗")))
    }
  }
  
  # Overall statistics
  cat("\nOverall Statistics:\n")
  
  # Count total presences
  total1 <- sum(data1 > 0)
  total2 <- sum(data2 > 0)
  total_combined <- sum(combined > 0)
  
  cat("Total presences in File 1:", total1, "\n")
  cat("Total presences in File 2:", total2, "\n")
  cat("Total presences in Combined:", total_combined, "\n")
  cat("Expected range: [", max(total1, total2), "-", total1 + total2, "]\n")
  
  # Species-level statistics
  species_in_1 <- colSums(data1 > 0)
  species_in_2 <- colSums(data2 > 0)
  species_combined <- colSums(combined > 0)
  
  cat("\nSpecies occurrence summary:\n")
  cat("Species only in File 1:", sum(species_in_1 > 0 & species_in_2 == 0), "\n")
  cat("Species only in File 2:", sum(species_in_1 == 0 & species_in_2 > 0), "\n")
  cat("Species in both files:", sum(species_in_1 > 0 & species_in_2 > 0), "\n")
  cat("Species in combined file:", sum(species_combined > 0), "\n")
  
  # Logic verification summary
  cat("\nLogic Verification:\n")
  cat("Checked", total_checks, "combinations\n")
  cat("Logic errors found:", logic_errors, "\n")
  cat("Accuracy:", round((total_checks - logic_errors) / total_checks * 100, 2), "%\n")
  
  return(list(
    logic_errors = logic_errors,
    total_checks = total_checks,
    total_presences = c(total1, total2, total_combined)
  ))
}

# Function to check multiple file combination
check_multiple_combination <- function(input_files, combined_file, combination_name) {
  cat("\n", paste(rep("=", 60), collapse=""), "\n")
  cat("CHECKING MULTIPLE COMBINATION:", combination_name, "\n")
  cat("Input files:", paste(input_files, collapse = ", "), "\n")
  cat("Combined:", combined_file, "\n")
  cat(paste(rep("=", 60), collapse=""), "\n")
  
  # Read all data
  all_data <- lapply(input_files, read_pa_data)
  combined <- read_pa_data(combined_file)
  
  # Check dimensions
  cat("Dimensions:\n")
  for (i in 1:length(all_data)) {
    cat("File", i, ":", nrow(all_data[[i]]), "sites x", ncol(all_data[[i]]), "species\n")
  }
  cat("Combined:", nrow(combined), "sites x", ncol(combined), "species\n")
  
  # Total presences
  cat("\nTotal presences:\n")
  totals <- sapply(all_data, function(x) sum(x > 0))
  for (i in 1:length(totals)) {
    cat("File", i, ":", totals[i], "\n")
  }
  cat("Combined:", sum(combined > 0), "\n")
  cat("Expected range: [", max(totals), "-", sum(totals), "]\n")
  
  return(totals)
}

# Run diagnostics
cat("PRESENCE-ABSENCE COMBINATION DIAGNOSTICS\n")
cat("========================================\n")

# Check method combinations
check_combination("Spider_pasti_cerven_finalPA.csv", 
                  "Spider_pasti_kveten_finalPA.csv",
                  "Spider_pasti_combinedPA.csv",
                  "PASTI METHOD (cerven + kveten)")

check_combination("Spider_smyk_cerven_finalPA.csv", 
                  "Spider_smyk_kveten_finalPA.csv",
                  "Spider_smyk_combinedPA.csv", 
                  "SMYK METHOD (cerven + kveten)")

# Check month combinations  
check_combination("Spider_pasti_cerven_finalPA.csv",
                  "Spider_smyk_cerven_finalPA.csv",
                  "Spider_cerven_combinedPA.csv",
                  "CERVEN MONTH (pasti + smyk)")

check_combination("Spider_pasti_kveten_finalPA.csv",
                  "Spider_smyk_kveten_finalPA.csv", 
                  "Spider_kveten_combinedPA.csv",
                  "KVETEN MONTH (pasti + smyk)")

# Check complete combination
all_files <- c("Spider_pasti_cerven_finalPA.csv",
               "Spider_pasti_kveten_finalPA.csv",
               "Spider_smyk_cerven_finalPA.csv", 
               "Spider_smyk_kveten_finalPA.csv")

check_multiple_combination(all_files,
                           "Spider_complete_combinedPA.csv",
                           "COMPLETE (all 4 files)")

cat("\n", paste(rep("=", 60), collapse=""), "\n")
cat("DIAGNOSTIC COMPLETE!\n")
cat(paste(rep("=", 60), collapse=""), "\n")

