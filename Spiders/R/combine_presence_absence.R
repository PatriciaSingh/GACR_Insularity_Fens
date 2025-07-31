# Combine presence-absence data files
# Script: combine_presence_absence.R
# Purpose: Combine PA files by methods and months using OR logic (1 if present in any file)
# Load required libraries
library(readr)
library(dplyr)

# Set working directory
setwd("Spiders/data")

# Function to combine two PA files using OR logic
combine_pa_files <- function(file1_path, file2_path, output_name) {
  cat("Combining:", file1_path, "and", file2_path, "\n")
  
  # Read both files
  data1 <- read_csv(file1_path, col_names = FALSE, col_types = cols(.default = "c"), show_col_types = FALSE)
  data2 <- read_csv(file2_path, col_names = FALSE, col_types = cols(.default = "c"), show_col_types = FALSE)
  
  # Start with the first file structure
  combined_data <- data1
  
  # Combine the data starting from row 3, column 5 (where species data starts)
  for (i in 3:nrow(combined_data)) {
    for (j in 5:ncol(combined_data)) {
      # Get values from both files
      val1 <- combined_data[[i, j]]
      val2 <- if(i <= nrow(data2) && j <= ncol(data2)) data2[[i, j]] else "0"
      
      # Convert to numeric, treating NA/empty as 0
      num1 <- suppressWarnings(as.numeric(val1))
      num2 <- suppressWarnings(as.numeric(val2))
      num1 <- ifelse(is.na(num1), 0, num1)
      num2 <- ifelse(is.na(num2), 0, num2)
      
      # OR logic: 1 if either is 1, 0 only if both are 0
      combined_value <- ifelse(num1 > 0 | num2 > 0, 1, 0)
      combined_data[[i, j]] <- as.character(combined_value)
    }
  }
  
  # Save combined file
  write_csv(combined_data, output_name, col_names = FALSE)
  cat("Saved combined file:", output_name, "\n\n")
  
  return(combined_data)
}

# Function to combine multiple files (for the complete combination)
combine_multiple_pa_files <- function(file_paths, output_name) {
  cat("Combining multiple files:", paste(file_paths, collapse = ", "), "\n")
  
  # Read all files
  all_data <- lapply(file_paths, function(path) {
    read_csv(path, col_names = FALSE, col_types = cols(.default = "c"), show_col_types = FALSE)
  })
  
  # Start with the first file structure
  combined_data <- all_data[[1]]
  
  # Combine all data starting from row 3, column 5
  for (i in 3:nrow(combined_data)) {
    for (j in 5:ncol(combined_data)) {
      # Get values from all files
      values <- sapply(all_data, function(data) {
        if(i <= nrow(data) && j <= ncol(data)) {
          val <- data[[i, j]]
          num_val <- suppressWarnings(as.numeric(val))
          return(ifelse(is.na(num_val), 0, num_val))
        } else {
          return(0)
        }
      })
      
      # OR logic: 1 if ANY file has 1, 0 only if ALL are 0
      combined_value <- ifelse(any(values > 0), 1, 0)
      combined_data[[i, j]] <- as.character(combined_value)
    }
  }
  
  # Save combined file
  write_csv(combined_data, output_name, col_names = FALSE)
  cat("Saved combined file:", output_name, "\n\n")
  
  return(combined_data)
}

# 1. Combine by methods (months together)
cat("=== COMBINING BY METHODS ===\n")
combine_pa_files("Spider_pasti_cerven_finalPA.csv", 
                 "Spider_pasti_kveten_finalPA.csv", 
                 "Spider_pasti_combinedPA.csv")

combine_pa_files("Spider_smyk_cerven_finalPA.csv", 
                 "Spider_smyk_kveten_finalPA.csv", 
                 "Spider_smyk_combinedPA.csv")

# 2. Combine by months (methods together)
cat("=== COMBINING BY MONTHS ===\n")
combine_pa_files("Spider_pasti_cerven_finalPA.csv", 
                 "Spider_smyk_cerven_finalPA.csv", 
                 "Spider_cerven_combinedPA.csv")

combine_pa_files("Spider_pasti_kveten_finalPA.csv", 
                 "Spider_smyk_kveten_finalPA.csv", 
                 "Spider_kveten_combinedPA.csv")

# 3. Combine everything
cat("=== COMBINING ALL FILES ===\n")
all_files <- c("Spider_pasti_cerven_finalPA.csv",
               "Spider_pasti_kveten_finalPA.csv", 
               "Spider_smyk_cerven_finalPA.csv",
               "Spider_smyk_kveten_finalPA.csv")

combine_multiple_pa_files(all_files, "Spider_complete_combinedPA.csv")

cat("All combinations completed!\n")
cat("Files created:\n")
cat("- Spider_pasti_combinedPA.csv (pasti cerven + kveten)\n")
cat("- Spider_smyk_combinedPA.csv (smyk cerven + kveten)\n") 
cat("- Spider_cerven_combinedPA.csv (cerven pasti + smyk)\n")
cat("- Spider_kveten_combinedPA.csv (kveten pasti + smyk)\n")
cat("- Spider_complete_combinedPA.csv (all 4 files combined)\n")

