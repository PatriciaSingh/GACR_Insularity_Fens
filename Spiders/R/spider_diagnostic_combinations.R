# comprehensive_data_diagnostic.R
# Comprehensive diagnostic script to detect potential issues in spider data

library(dplyr)
library(readr)

cat("=== COMPREHENSIVE SPIDER DATA DIAGNOSTIC ===\n\n")

# File paths
original_files <- c(
  "Spider_pasti_cerven_finalPA_forcombining.csv",
  "Spider_smyk_kveten_finalPA_forcombining.csv", 
  "Spider_smyk_cerven_finalPA_forcombining.csv",
  "Spider_pasti_kveten_finalPA_forcombining.csv"
)

combined_files <- c(
  "Spider_combined_all_methods_PA.csv",
  "Spider_combined_smyk_PA.csv",
  "Spider_combined_pasti_PA.csv",
  "Spider_combined_kveten_PA.csv",
  "Spider_combined_cerven_PA.csv"
)

# Function to read and analyze file
analyze_file <- function(file_path, file_type = "original") {
  if (!file.exists(file_path)) {
    cat("❌ File not found:", file_path, "\n")
    return(NULL)
  }
  
  data <- read_csv(file_path, show_col_types = FALSE)
  
  cat("=== ANALYZING:", basename(file_path), "===\n")
  cat("Type:", file_type, "\n")
  cat("Dimensions:", nrow(data), "rows x", ncol(data), "columns\n")
  
  results <- list(
    file = file_path,
    data = data,
    issues = c()
  )
  
  # Check 1: Data structure
  locality_col <- data[[1]]
  species_cols <- data[, -1]
  
  # Check 2: Locality column issues
  cat("\n--- LOCALITY CHECKS ---\n")
  
  # Empty localities
  empty_localities <- sum(is.na(locality_col) | locality_col == "" | locality_col == " ")
  if (empty_localities > 0) {
    cat("⚠️  Empty/NA localities:", empty_localities, "\n")
    results$issues <- c(results$issues, paste("Empty localities:", empty_localities))
  } else {
    cat("✅ No empty localities\n")
  }
  
  # Duplicate localities
  dup_localities <- sum(duplicated(locality_col))
  if (dup_localities > 0) {
    cat("⚠️  Duplicate localities:", dup_localities, "\n")
    dup_names <- locality_col[duplicated(locality_col)]
    cat("    Duplicated names:", paste(unique(dup_names), collapse = ", "), "\n")
    results$issues <- c(results$issues, paste("Duplicate localities:", dup_localities))
  } else {
    cat("✅ No duplicate localities\n")
  }
  
  # Locality name patterns
  unusual_chars <- grepl("[^A-Za-z0-9_.-]", locality_col)
  if (any(unusual_chars)) {
    cat("⚠️  Localities with unusual characters:", sum(unusual_chars), "\n")
    results$issues <- c(results$issues, paste("Unusual locality chars:", sum(unusual_chars)))
  } else {
    cat("✅ Locality names look clean\n")
  }
  
  # Check 3: Species data issues
  cat("\n--- SPECIES DATA CHECKS ---\n")
  
  # Convert to numeric matrix for analysis
  species_matrix <- as.matrix(species_cols)
  mode(species_matrix) <- "numeric"
  
  # Values outside 0-1 range
  invalid_values <- !is.na(species_matrix) & (species_matrix < 0 | species_matrix > 1)
  if (any(invalid_values)) {
    invalid_count <- sum(invalid_values)
    cat("❌ Values outside 0-1 range:", invalid_count, "\n")
    
    # Show examples
    invalid_positions <- which(invalid_values, arr.ind = TRUE)
    if (nrow(invalid_positions) > 0) {
      cat("    Examples:\n")
      for (i in 1:min(5, nrow(invalid_positions))) {
        row_idx <- invalid_positions[i, 1]
        col_idx <- invalid_positions[i, 2]
        value <- species_matrix[row_idx, col_idx]
        locality <- locality_col[row_idx]
        species <- names(species_cols)[col_idx]
        cat(sprintf("      %s - %s: %s\n", locality, species, value))
      }
    }
    results$issues <- c(results$issues, paste("Invalid values:", invalid_count))
  } else {
    cat("✅ All values are 0 or 1\n")
  }
  
  # NA values
  na_count <- sum(is.na(species_matrix))
  if (na_count > 0) {
    cat("⚠️  NA values found:", na_count, "\n")
    
    # Which columns have most NAs
    na_by_col <- colSums(is.na(species_matrix))
    problematic_cols <- na_by_col[na_by_col > 0]
    if (length(problematic_cols) > 0) {
      cat("    Columns with NAs:\n")
      for (i in 1:min(10, length(problematic_cols))) {
        cat(sprintf("      %s: %d NAs\n", names(problematic_cols)[i], problematic_cols[i]))
      }
    }
    results$issues <- c(results$issues, paste("NA values:", na_count))
  } else {
    cat("✅ No NA values\n")
  }
  
  # Check 4: Species column issues
  cat("\n--- SPECIES COLUMN CHECKS ---\n")
  
  # Duplicate column names
  dup_cols <- sum(duplicated(names(species_cols)))
  if (dup_cols > 0) {
    cat("❌ Duplicate species names:", dup_cols, "\n")
    dup_names <- names(species_cols)[duplicated(names(species_cols))]
    cat("    Duplicated names:", paste(unique(dup_names), collapse = ", "), "\n")
    results$issues <- c(results$issues, paste("Duplicate species names:", dup_cols))
  } else {
    cat("✅ No duplicate species names\n")
  }
  
  # Empty species columns (all zeros)
  empty_species <- colSums(species_matrix, na.rm = TRUE) == 0
  empty_count <- sum(empty_species)
  if (empty_count > 0) {
    cat("⚠️  Species with no occurrences:", empty_count, "\n")
    empty_names <- names(species_cols)[empty_species]
    if (length(empty_names) <= 10) {
      cat("    Species:", paste(empty_names, collapse = ", "), "\n")
    } else {
      cat("    First 10 species:", paste(empty_names[1:10], collapse = ", "), "...\n")
    }
    results$issues <- c(results$issues, paste("Empty species:", empty_count))
  } else {
    cat("✅ All species have at least one occurrence\n")
  }
  
  # Species with unusual names
  unusual_species <- grepl("\\.\\.\\.|^X\\d+|^NA$", names(species_cols))
  if (any(unusual_species)) {
    cat("⚠️  Species with unusual names:", sum(unusual_species), "\n")
    unusual_names <- names(species_cols)[unusual_species]
    cat("    Names:", paste(unusual_names, collapse = ", "), "\n")
    results$issues <- c(results$issues, paste("Unusual species names:", sum(unusual_species)))
  } else {
    cat("✅ Species names look normal\n")
  }
  
  # Check 5: Data distribution
  cat("\n--- DATA DISTRIBUTION ---\n")
  
  total_presences <- sum(species_matrix, na.rm = TRUE)
  total_cells <- nrow(species_matrix) * ncol(species_matrix)
  presence_rate <- round(total_presences / total_cells * 100, 2)
  
  cat("Total presences:", total_presences, "\n")
  cat("Total possible cells:", total_cells, "\n")
  cat("Presence rate:", presence_rate, "%\n")
  
  # Localities with no species
  localities_no_species <- rowSums(species_matrix, na.rm = TRUE) == 0
  empty_localities_count <- sum(localities_no_species)
  if (empty_localities_count > 0) {
    cat("⚠️  Localities with no species:", empty_localities_count, "\n")
    empty_locality_names <- locality_col[localities_no_species]
    cat("    Localities:", paste(empty_locality_names, collapse = ", "), "\n")
    results$issues <- c(results$issues, paste("Empty localities:", empty_localities_count))
  } else {
    cat("✅ All localities have at least one species\n")
  }
  
  # Most common species
  species_freq <- colSums(species_matrix, na.rm = TRUE)
  top_species <- sort(species_freq, decreasing = TRUE)[1:min(5, length(species_freq))]
  cat("Top species:\n")
  for (i in 1:length(top_species)) {
    cat(sprintf("  %d. %s: %d occurrences\n", i, names(top_species)[i], top_species[i]))
  }
  
  cat("\n", paste(rep("-", 60), collapse = ""), "\n\n")
  return(results)
}

# Function to compare files
compare_files <- function(results_list) {
  cat("=== CROSS-FILE COMPARISON ===\n")
  
  # Get all localities across files
  all_localities <- c()
  all_species <- c()
  
  for (result in results_list) {
    if (!is.null(result)) {
      localities <- result$data[[1]]
      species <- names(result$data)[-1]
      all_localities <- c(all_localities, localities)
      all_species <- c(all_species, species)
    }
  }
  
  unique_localities <- unique(all_localities)
  unique_species <- unique(all_species)
  
  cat("Total unique localities across all files:", length(unique_localities), "\n")
  cat("Total unique species across all files:", length(unique_species), "\n\n")
  
  # Check locality consistency
  cat("--- LOCALITY CONSISTENCY ---\n")
  for (i in 1:(length(results_list)-1)) {
    for (j in (i+1):length(results_list)) {
      if (!is.null(results_list[[i]]) && !is.null(results_list[[j]])) {
        file1 <- basename(results_list[[i]]$file)
        file2 <- basename(results_list[[j]]$file)
        
        localities1 <- results_list[[i]]$data[[1]]
        localities2 <- results_list[[j]]$data[[1]]
        
        common <- intersect(localities1, localities2)
        only1 <- setdiff(localities1, localities2)
        only2 <- setdiff(localities2, localities1)
        
        cat(sprintf("%s vs %s:\n", file1, file2))
        cat(sprintf("  Common localities: %d\n", length(common)))
        if (length(only1) > 0) {
          cat(sprintf("  Only in %s: %d\n", file1, length(only1)))
        }
        if (length(only2) > 0) {
          cat(sprintf("  Only in %s: %d\n", file2, length(only2)))
        }
        cat("\n")
      }
    }
  }
}

# Run analysis on all files
cat("ANALYZING ORIGINAL FILES...\n")
cat(paste(rep("=", 80), collapse = ""), "\n")

original_results <- list()
for (file in original_files) {
  original_results[[file]] <- analyze_file(file, "original")
}

cat("\nANALYZING COMBINED FILES...\n")
cat(paste(rep("=", 80), collapse = ""), "\n")

combined_results <- list()
for (file in combined_files) {
  combined_results[[file]] <- analyze_file(file, "combined")
}

# Cross-file comparisons
compare_files(original_results)

# Summary of all issues
cat("=== ISSUES SUMMARY ===\n")
all_issues <- c()

cat("\nOriginal files issues:\n")
for (result in original_results) {
  if (!is.null(result) && length(result$issues) > 0) {
    cat(sprintf("  %s:\n", basename(result$file)))
    for (issue in result$issues) {
      cat(sprintf("    - %s\n", issue))
    }
    all_issues <- c(all_issues, result$issues)
  }
}

cat("\nCombined files issues:\n")
for (result in combined_results) {
  if (!is.null(result) && length(result$issues) > 0) {
    cat(sprintf("  %s:\n", basename(result$file)))
    for (issue in result$issues) {
      cat(sprintf("    - %s\n", issue))
    }
    all_issues <- c(all_issues, result$issues)
  }
}

if (length(all_issues) == 0) {
  cat("\n🎉 NO ISSUES FOUND! Your data looks clean! 🎉\n")
} else {
  cat(sprintf("\n⚠️  Total issues found: %d\n", length(all_issues)))
  cat("Review the detailed output above for specific problems.\n")
}

cat("\n=== DIAGNOSTIC COMPLETE ===\n")