# spider_dca_analysis.R
# DCA (Detrended Correspondence Analysis) for spider data comparison

library(vegan)
library(ggplot2)
library(dplyr)
library(readr)
library(gridExtra)

cat("=== SPIDER DCA ANALYSIS ===\n\n")

# Create organized output directories
output_base <- "Spiders/figures/DCA_analysis"
plot_dir <- file.path(output_base, "plots")
table_dir <- file.path(output_base, "tables") 
results_dir <- file.path(output_base, "results")

# Create directories if they don't exist
for (dir in c(output_base, plot_dir, table_dir, results_dir)) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    cat("Created directory:", dir, "\n")
  }
}

cat("Output will be saved to:", output_base, "\n")
cat("├── plots/    - All DCA visualization plots\n")
cat("├── tables/   - Summary tables and data exports\n")
cat("└── results/  - R objects for further analysis\n\n")

# Define all files for analysis
files_list <- list(
  # Original files
  "Pasti Cerven" = "Spider_pasti_cerven_finalPA_forcombining.csv",
  "Smyk Kveten" = "Spider_smyk_kveten_finalPA_forcombining.csv",
  "Smyk Cerven" = "Spider_smyk_cerven_finalPA_forcombining.csv",
  "Pasti Kveten" = "Spider_pasti_kveten_finalPA_forcombining.csv",
  
  # Combined files
  "All Combined" = "Spider_combined_all_methods_PA.csv",
  "Smyk Method" = "Spider_combined_smyk_PA.csv",
  "Pasti Method" = "Spider_combined_pasti_PA.csv",
  "Kveten Month" = "Spider_combined_kveten_PA.csv",
  "Cerven Month" = "Spider_combined_cerven_PA.csv"
)

# Function to prepare data for DCA
prepare_dca_data <- function(file_path, file_name) {
  cat("Processing:", file_name, "\n")
  
  if (!file.exists(file_path)) {
    cat("❌ File not found:", file_path, "\n")
    return(NULL)
  }
  
  # Read data
  data <- read_csv(file_path, show_col_types = FALSE)
  
  # Extract locality names and species matrix
  localities <- data[[1]]
  species_matrix <- data[, -1]
  
  # Convert to numeric matrix
  species_matrix <- as.matrix(species_matrix)
  mode(species_matrix) <- "numeric"
  rownames(species_matrix) <- localities
  
  # Check for and handle missing values
  na_count <- sum(is.na(species_matrix))
  if (na_count > 0) {
    cat("  ⚠️  Found", na_count, "NA values - replacing with 0\n")
    species_matrix[is.na(species_matrix)] <- 0
  }
  
  # Check for negative values
  neg_count <- sum(species_matrix < 0, na.rm = TRUE)
  if (neg_count > 0) {
    cat("  ⚠️  Found", neg_count, "negative values - replacing with 0\n")
    species_matrix[species_matrix < 0] <- 0
  }
  
  # Remove species with no occurrences (all zeros)
  species_sums <- colSums(species_matrix, na.rm = TRUE)
  empty_species <- species_sums == 0
  if (any(empty_species)) {
    empty_count <- sum(empty_species)
    cat("  🗑️  Removing", empty_count, "species with no occurrences\n")
    species_matrix <- species_matrix[, !empty_species]
  }
  
  # Remove localities with no species
  locality_sums <- rowSums(species_matrix, na.rm = TRUE)
  empty_localities <- locality_sums == 0
  if (any(empty_localities)) {
    empty_loc_count <- sum(empty_localities)
    cat("  🗑️  Removing", empty_loc_count, "localities with no species\n")
    species_matrix <- species_matrix[!empty_localities, ]
  }
  
  # Final check - ensure we have enough data for DCA
  if (nrow(species_matrix) < 3) {
    cat("  ❌ Too few localities (", nrow(species_matrix), ") for DCA analysis\n")
    return(NULL)
  }
  
  if (ncol(species_matrix) < 3) {
    cat("  ❌ Too few species (", ncol(species_matrix), ") for DCA analysis\n")
    return(NULL)
  }
  
  # Verify no NAs or negative values remain
  if (any(is.na(species_matrix))) {
    cat("  ❌ NAs still present after cleaning!\n")
    return(NULL)
  }
  
  if (any(species_matrix < 0)) {
    cat("  ❌ Negative values still present after cleaning!\n")
    return(NULL)
  }
  
  cat(sprintf("  ✅ Final dimensions: %d localities, %d species\n", 
              nrow(species_matrix), ncol(species_matrix)))
  cat(sprintf("  ✅ Total presences: %d\n", sum(species_matrix)))
  cat(sprintf("  ✅ Data range: %g to %g\n", min(species_matrix), max(species_matrix)))
  
  return(species_matrix)
}

# Function to run DCA and create plot
run_dca <- function(species_matrix, dataset_name) {
  cat("\n--- Running DCA for:", dataset_name, "---\n")
  
  if (is.null(species_matrix)) {
    cat("❌ No data provided for DCA\n")
    return(NULL)
  }
  
  if (nrow(species_matrix) < 3 || ncol(species_matrix) < 3) {
    cat("❌ Insufficient data for DCA (need at least 3 localities and 3 species)\n")
    return(NULL)
  }
  
  # Final data validation before DCA
  cat("  Data validation:\n")
  cat(sprintf("    Matrix dimensions: %d × %d\n", nrow(species_matrix), ncol(species_matrix)))
  cat(sprintf("    Value range: %g to %g\n", min(species_matrix), max(species_matrix)))
  cat(sprintf("    NA values: %d\n", sum(is.na(species_matrix))))
  cat(sprintf("    Negative values: %d\n", sum(species_matrix < 0)))
  
  # Try to run DCA with error handling
  tryCatch({
    dca_result <- decorana(species_matrix)
    cat("  ✅ DCA completed successfully\n")
  }, error = function(e) {
    cat("  ❌ DCA failed with error:", e$message, "\n")
    return(NULL)
  })
  
  if (!exists("dca_result")) {
    return(NULL)
  }
  
  # Print summary
  cat("DCA Summary:\n")
  cat(sprintf("  Eigenvalues: DCA1=%.3f, DCA2=%.3f, DCA3=%.3f, DCA4=%.3f\n",
              dca_result$evals[1], dca_result$evals[2], 
              dca_result$evals[3], dca_result$evals[4]))
  
  # Extract site scores
  site_scores <- scores(dca_result, display = "sites", choices = c(1, 2))
  site_df <- data.frame(
    Locality = rownames(site_scores),
    DCA1 = site_scores[, 1],
    DCA2 = site_scores[, 2],
    Dataset = dataset_name
  )
  
  # Extract species scores
  species_scores <- scores(dca_result, display = "species", choices = c(1, 2))
  species_df <- data.frame(
    Species = rownames(species_scores),
    DCA1 = species_scores[, 1],
    DCA2 = species_scores[, 2],
    Dataset = dataset_name
  )
  
  # Create plot
  p <- ggplot() +
    # Add locality points
    geom_point(data = site_df, aes(x = DCA1, y = DCA2), 
               color = "blue", size = 2, alpha = 0.7) +
    # Add species points (smaller, in red)
    geom_point(data = species_df, aes(x = DCA1, y = DCA2), 
               color = "red", size = 1, alpha = 0.5) +
    # Add locality labels (optional - can be removed if too crowded)
    geom_text(data = site_df, aes(x = DCA1, y = DCA2, label = Locality), 
              vjust = -0.5, hjust = 0.5, size = 2.5, alpha = 0.8) +
    labs(
      title = paste("DCA Ordination:", dataset_name),
      subtitle = paste("Eigenvalues: DCA1 =", round(dca_result$evals[1], 3), 
                       ", DCA2 =", round(dca_result$evals[2], 3)),
      x = paste("DCA1 (", round(dca_result$evals[1], 3), ")"),
      y = paste("DCA2 (", round(dca_result$evals[2], 3), ")"),
      caption = "Blue = Localities, Red = Species"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12, face = "bold"),
      plot.subtitle = element_text(size = 10),
      axis.title = element_text(size = 10),
      legend.position = "none"
    )
  
  # Return results
  return(list(
    dca = dca_result,
    sites = site_df,
    species = species_df,
    plot = p,
    eigenvalues = dca_result$evals
  ))
}

# Process all files and run DCA
dca_results <- list()
all_plots <- list()

cat("PROCESSING ALL FILES...\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

for (dataset_name in names(files_list)) {
  file_path <- files_list[[dataset_name]]
  
  # Prepare data
  species_matrix <- prepare_dca_data(file_path, dataset_name)
  
  if (!is.null(species_matrix)) {
    # Run DCA
    dca_result <- run_dca(species_matrix, dataset_name)
    
    if (!is.null(dca_result)) {
      dca_results[[dataset_name]] <- dca_result
      all_plots[[dataset_name]] <- dca_result$plot
    }
  }
  
  cat(paste(rep("-", 40), collapse = ""), "\n")
}

# Create comparison plots
cat("\nCREATING COMPARISON PLOTS...\n")

# Save individual plots
for (name in names(all_plots)) {
  filename <- file.path(plot_dir, paste0("DCA_", gsub("[^A-Za-z0-9]", "_", name), ".png"))
  ggsave(filename, all_plots[[name]], width = 8, height = 6, dpi = 300)
  cat("Saved:", filename, "\n")
}

# Create combined plot grids
if (length(all_plots) > 0) {
  
  # Original files comparison
  original_plots <- all_plots[c("Pasti Cerven", "Smyk Kveten", "Smyk Cerven", "Pasti Kveten")]
  original_plots <- original_plots[!sapply(original_plots, is.null)]
  
  if (length(original_plots) > 1) {
    comparison_file <- file.path(plot_dir, "DCA_Original_Files_Comparison.png")
    combined_original <- do.call(grid.arrange, c(original_plots, ncol = 2))
    ggsave(comparison_file, combined_original, width = 12, height = 10, dpi = 300)
    cat("Saved:", comparison_file, "\n")
  }
  
  # Combined files comparison
  combined_plots <- all_plots[c("All Combined", "Smyk Method", "Pasti Method", 
                                "Kveten Month", "Cerven Month")]
  combined_plots <- combined_plots[!sapply(combined_plots, is.null)]
  
  if (length(combined_plots) > 1) {
    comparison_file <- file.path(plot_dir, "DCA_Combined_Files_Comparison.png")
    combined_combined <- do.call(grid.arrange, c(combined_plots, ncol = 2))
    ggsave(comparison_file, combined_combined, width = 12, height = 12, dpi = 300)
    cat("Saved:", comparison_file, "\n")
  }
}

# Summary table of eigenvalues
cat("\n=== DCA EIGENVALUES COMPARISON ===\n")
eigenvalue_summary <- data.frame(
  Dataset = character(),
  DCA1 = numeric(),
  DCA2 = numeric(),
  DCA3 = numeric(),
  DCA4 = numeric(),
  stringsAsFactors = FALSE
)

for (name in names(dca_results)) {
  if (!is.null(dca_results[[name]])) {
    eigenvals <- dca_results[[name]]$eigenvalues
    eigenvalue_summary <- rbind(eigenvalue_summary, data.frame(
      Dataset = name,
      DCA1 = round(eigenvals[1], 3),
      DCA2 = round(eigenvals[2], 3),
      DCA3 = round(eigenvals[3], 3),
      DCA4 = round(eigenvals[4], 3)
    ))
  }
}

print(eigenvalue_summary)

# Export eigenvalue summary
eigenvalue_file <- file.path(table_dir, "DCA_Eigenvalues_Summary.csv")
write_csv(eigenvalue_summary, eigenvalue_file)
cat("\nSaved eigenvalue summary:", eigenvalue_file, "\n")

# Save detailed site and species scores for each analysis
cat("\nSaving detailed DCA results...\n")
for (name in names(dca_results)) {
  if (!is.null(dca_results[[name]])) {
    # Save site scores
    site_file <- file.path(table_dir, paste0("DCA_Sites_", gsub("[^A-Za-z0-9]", "_", name), ".csv"))
    write_csv(dca_results[[name]]$sites, site_file)
    
    # Save species scores  
    species_file <- file.path(table_dir, paste0("DCA_Species_", gsub("[^A-Za-z0-9]", "_", name), ".csv"))
    write_csv(dca_results[[name]]$species, species_file)
    
    # Save R object for further analysis
    rdata_file <- file.path(results_dir, paste0("DCA_", gsub("[^A-Za-z0-9]", "_", name), ".RData"))
    save(dca_results[[name]], file = rdata_file)
    
    cat("Saved DCA results for", name, "\n")
  }
}

cat("\n=== DCA ANALYSIS COMPLETE ===\n")
cat("All outputs saved to:", output_base, "\n")
cat("📁 Plots saved to:", plot_dir, "\n")
cat("📊 Tables saved to:", table_dir, "\n") 
cat("💾 R objects saved to:", results_dir, "\n")
cat("\nFiles created:\n")
cat("• Individual DCA plots (9 PNG files)\n")
cat("• Comparison grids (2 PNG files)\n") 
cat("• Eigenvalue summary (1 CSV file)\n")
cat("• Site scores tables (9 CSV files)\n")
cat("• Species scores tables (9 CSV files)\n")
cat("• R data objects (9 RData files)\n")

# Interpretation guide
cat("\n=== INTERPRETATION GUIDE ===\n")
cat("• Higher DCA1 eigenvalues indicate stronger primary gradient\n")
cat("• Eigenvalues > 0.5 suggest strong ecological gradients\n")
cat("• Eigenvalues < 0.3 suggest weak gradients or random patterns\n")
cat("• Compare eigenvalues between methods to see which captures more variation\n")
cat("• Look at plot patterns to see if methods show similar community structure\n")

