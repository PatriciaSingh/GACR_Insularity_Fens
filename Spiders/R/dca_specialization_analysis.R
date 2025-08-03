# dca_specialization_analysis.R
# DCA analysis with species colored by specialization level

getwd()
library(vegan)
library(ggplot2)
library(dplyr)
library(readr)
library(gridExtra)
library(ggrepel)  # For better text labels

cat("=== DCA ANALYSIS WITH SPECIES SPECIALIZATION COLORING ===\n\n")

# Create organized output directories
output_base <- "Spiders/results/DCA_specialization_analysis"
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
cat("├── plots/    - DCA plots colored by specialization\n")
cat("├── tables/   - Species specialization data with DCA scores\n")
cat("└── results/  - R objects for further analysis\n\n")

# Read species specialization data
specialization_file <- "Spiders_Species_Specializace_Sireni_Ohrozeni.csv"
cat("Reading specialization data from:", specialization_file, "\n")

if (!file.exists(specialization_file)) {
  stop("❌ Specialization file not found: ", specialization_file)
}

spec_data <- read_csv(specialization_file, show_col_types = FALSE)
cat("✅ Loaded specialization data for", nrow(spec_data), "species\n\n")

# Check specialization levels
spec_levels <- table(spec_data$Specialization)
cat("Specialization levels found:\n")
print(spec_levels)
cat("\n")

# Define color palette for specialization levels (0-3 only, no NA)
specialization_colors <- c(
  "0" = "#2E8B57",    # Sea green - generalists
  "1" = "#4169E1",    # Royal blue - low specialization  
  "2" = "#FF8C00",    # Dark orange - moderate specialization
  "3" = "#DC143C"     # Crimson - high specialization
)

specialization_labels <- c(
  "0" = "Generalist (0)",
  "1" = "Low specialist (1)", 
  "2" = "Moderate specialist (2)",
  "3" = "High specialist (3)"
)

# Define all files for analysis (using cleaned files)
files_list <- list(
  # Original files
  "Pasti Cerven" = "Spider_pasti_cerven_finalPA_forcombining_okZeros.csv",
  "Smyk Kveten" = "Spider_smyk_kveten_finalPA_forcombining_okZeros.csv",
  "Smyk Cerven" = "Spider_smyk_cerven_finalPA_forcombining_okZeros.csv",
  "Pasti Kveten" = "Spider_pasti_kveten_finalPA_forcombining_okZeros.csv",
  
  # Combined files
  "All Combined" = "Spider_combined_all_methods_PA_okZeros.csv",
  "Smyk Method" = "Spider_combined_smyk_PA_okZeros.csv",
  "Pasti Method" = "Spider_combined_pasti_PA_okZeros.csv",
  "Kveten Month" = "Spider_combined_kveten_PA_okZeros.csv",
  "Cerven Month" = "Spider_combined_cerven_PA_okZeros.csv"
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
  
  # Handle any remaining issues
  species_matrix[is.na(species_matrix)] <- 0
  species_matrix[species_matrix < 0] <- 0
  
  # Remove species with no occurrences
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
  
  # Final validation
  if (nrow(species_matrix) < 3 || ncol(species_matrix) < 3) {
    cat("  ❌ Insufficient data for DCA\n")
    return(NULL)
  }
  
  cat(sprintf("  ✅ Final dimensions: %d localities, %d species\n", 
              nrow(species_matrix), ncol(species_matrix)))
  
  return(species_matrix)
}

# Function to run DCA and create specialized plots
run_specialized_dca <- function(species_matrix, dataset_name, spec_data) {
  cat("\n--- Running Specialized DCA for:", dataset_name, "---\n")
  
  if (is.null(species_matrix)) {
    cat("❌ No data provided for DCA\n")
    return(NULL)
  }
  
  # Run DCA
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
  
  # Extract species scores and match with specialization data
  species_scores <- scores(dca_result, display = "species", choices = c(1, 2))
  species_df <- data.frame(
    Short_name = rownames(species_scores),
    DCA1 = species_scores[, 1],
    DCA2 = species_scores[, 2],
    Dataset = dataset_name
  )
  
  # Merge with specialization data and remove NA species
  species_df <- species_df %>%
    left_join(spec_data %>% select(Short_name, Species, Specialization), 
              by = "Short_name") %>%
    filter(!is.na(Specialization))  # Remove species without specialization data
  
  # Check if we still have enough species after filtering
  if (nrow(species_df) < 3) {
    cat("  ❌ Too few species with specialization data (", nrow(species_df), ") for meaningful analysis\n")
    return(NULL)
  }
  
  # Convert specialization to factor for proper coloring
  species_df$Specialization <- as.character(species_df$Specialization)
  species_df$Specialization <- factor(species_df$Specialization, 
                                      levels = c("0", "1", "2", "3"))
  
  # Count species by specialization
  spec_counts <- table(species_df$Specialization)
  cat("  Species with known specialization:\n")
  total_species_analyzed <- sum(spec_counts)
  for (i in names(spec_counts)) {
    if (spec_counts[i] > 0) {
      cat(sprintf("    %s: %d species\n", specialization_labels[i], spec_counts[i]))
    }
  }
  cat(sprintf("  Total species analyzed: %d (excluding %d species with unknown specialization)\n", 
              total_species_analyzed, 
              length(rownames(species_scores)) - total_species_analyzed))
  
  # Create base plot
  p <- ggplot() +
    # Add locality points (smaller, gray)
    geom_point(data = site_df, aes(x = DCA1, y = DCA2), 
               color = "gray60", size = 1.5, alpha = 0.6) +
    # Add locality labels (smaller, gray)
    geom_text_repel(data = site_df, aes(x = DCA1, y = DCA2, label = Locality), 
                    size = 2, color = "gray40", alpha = 0.8,
                    max.overlaps = 15, force = 2) +
    # Add species points colored by specialization
    geom_point(data = species_df, aes(x = DCA1, y = DCA2, color = Specialization), 
               size = 2.5, alpha = 0.8) +
    # Add species labels
    geom_text_repel(data = species_df, aes(x = DCA1, y = DCA2, label = Short_name, 
                                           color = Specialization), 
                    size = 2.2, alpha = 0.9, max.overlaps = 20, force = 3) +
    # Color scale
    scale_color_manual(values = specialization_colors,
                       labels = specialization_labels,
                       name = "Specialization Level",
                       guide = guide_legend(override.aes = list(size = 4))) +
    # Labels and theme
    labs(
      title = paste("DCA Ordination with Species Specialization:", dataset_name),
      subtitle = paste("Eigenvalues: DCA1 =", round(dca_result$evals[1], 3), 
                       ", DCA2 =", round(dca_result$evals[2], 3)),
      x = paste("DCA1 (", round(dca_result$evals[1], 3), ")"),
      y = paste("DCA2 (", round(dca_result$evals[2], 3), ")"),
      caption = "Gray = Localities | Colored = Species by specialization level"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12, face = "bold"),
      plot.subtitle = element_text(size = 10),
      axis.title = element_text(size = 10),
      legend.position = "right",
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 9),
      panel.grid.minor = element_blank()
    )
  
  # Create species-only plot (cleaner for species patterns)
  p_species <- ggplot(species_df, aes(x = DCA1, y = DCA2)) +
    geom_point(aes(color = Specialization), size = 3, alpha = 0.8) +
    geom_text_repel(aes(label = Short_name, color = Specialization), 
                    size = 2.5, alpha = 0.9, max.overlaps = 25, force = 4) +
    scale_color_manual(values = specialization_colors,
                       labels = specialization_labels,
                       name = "Specialization Level",
                       guide = guide_legend(override.aes = list(size = 4))) +
    labs(
      title = paste("Species Specialization Patterns:", dataset_name),
      subtitle = paste("DCA1 =", round(dca_result$evals[1], 3), 
                       "| DCA2 =", round(dca_result$evals[2], 3)),
      x = paste("DCA1 (", round(dca_result$evals[1], 3), ")"),
      y = paste("DCA2 (", round(dca_result$evals[2], 3), ")"),
      caption = "Species colored by habitat specialization level"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12, face = "bold"),
      plot.subtitle = element_text(size = 10),
      axis.title = element_text(size = 10),
      legend.position = "right",
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 9),
      panel.grid.minor = element_blank()
    )
  
  # Return results
  return(list(
    dca = dca_result,
    sites = site_df,
    species = species_df,
    plot_combined = p,
    plot_species_only = p_species,
    eigenvalues = dca_result$evals
  ))
}

# Process all files and run specialized DCA
dca_results <- list()
all_plots_combined <- list()
all_plots_species <- list()

cat("PROCESSING ALL FILES WITH SPECIALIZATION ANALYSIS...\n")
cat(paste(rep("=", 70), collapse = ""), "\n")

for (dataset_name in names(files_list)) {
  file_path <- files_list[[dataset_name]]
  
  # Prepare data
  species_matrix <- prepare_dca_data(file_path, dataset_name)
  
  if (!is.null(species_matrix)) {
    # Run specialized DCA
    dca_result <- run_specialized_dca(species_matrix, dataset_name, spec_data)
    
    if (!is.null(dca_result)) {
      dca_results[[dataset_name]] <- dca_result
      all_plots_combined[[dataset_name]] <- dca_result$plot_combined
      all_plots_species[[dataset_name]] <- dca_result$plot_species_only
    }
  }
  
  cat(paste(rep("-", 50), collapse = ""), "\n")
}

# Save individual plots
cat("\nSAVING SPECIALIZATION PLOTS...\n")

# Save combined plots (localities + species)
for (name in names(all_plots_combined)) {
  filename <- file.path(plot_dir, paste0("DCA_Combined_", gsub("[^A-Za-z0-9]", "_", name), "_Specialization.png"))
  ggsave(filename, all_plots_combined[[name]], width = 12, height = 8, dpi = 300)
  cat("Saved combined plot:", filename, "\n")
}

# Save species-only plots
for (name in names(all_plots_species)) {
  filename <- file.path(plot_dir, paste0("DCA_Species_", gsub("[^A-Za-z0-9]", "_", name), "_Specialization.png"))
  ggsave(filename, all_plots_species[[name]], width = 12, height = 8, dpi = 300)
  cat("Saved species plot:", filename, "\n")
}

# Create comparison grids
if (length(all_plots_species) > 0) {
  
  # Original files comparison (species only)
  original_plots <- all_plots_species[c("Pasti Cerven", "Smyk Kveten", "Smyk Cerven", "Pasti Kveten")]
  original_plots <- original_plots[!sapply(original_plots, is.null)]
  
  if (length(original_plots) > 1) {
    comparison_file <- file.path(plot_dir, "DCA_Original_Files_Species_Specialization_Comparison.png")
    combined_original <- do.call(grid.arrange, c(original_plots, ncol = 2))
    ggsave(comparison_file, combined_original, width = 16, height = 12, dpi = 300)
    cat("Saved original files comparison:", comparison_file, "\n")
  }
  
  # Combined files comparison (species only)
  combined_plots <- all_plots_species[c("All Combined", "Smyk Method", "Pasti Method", 
                                        "Kveten Month", "Cerven Month")]
  combined_plots <- combined_plots[!sapply(combined_plots, is.null)]
  
  if (length(combined_plots) > 1) {
    comparison_file <- file.path(plot_dir, "DCA_Combined_Files_Species_Specialization_Comparison.png")
    combined_combined <- do.call(grid.arrange, c(combined_plots, ncol = 2))
    ggsave(comparison_file, combined_combined, width = 16, height = 15, dpi = 300)
    cat("Saved combined files comparison:", comparison_file, "\n")
  }
}

# Save species data with DCA scores and specialization
cat("\nSaving species data with DCA scores...\n")
for (name in names(dca_results)) {
  if (!is.null(dca_results[[name]])) {
    # Save species scores with specialization data
    species_file <- file.path(table_dir, paste0("Species_Specialization_DCA_", gsub("[^A-Za-z0-9]", "_", name), ".csv"))
    write_csv(dca_results[[name]]$species, species_file)
    
    # Save R object for further analysis
    rdata_file <- file.path(results_dir, paste0("DCA_Specialization_", gsub("[^A-Za-z0-9]", "_", name), ".RData"))
    dca_obj <- dca_results[[name]]
    save(dca_obj, file = rdata_file)
    
    cat("Saved specialization results for", name, "\n")
  }
}

cat("\n=== SPECIALIZATION DCA ANALYSIS COMPLETE ===\n")
cat("All outputs saved to:", output_base, "\n")
cat("📁 Plots saved to:", plot_dir, "\n")
cat("📊 Tables saved to:", table_dir, "\n") 
cat("💾 R objects saved to:", results_dir, "\n")
cat("\nFiles created:\n")
cat("• Individual DCA plots - combined view (9 PNG files)\n")
cat("• Individual DCA plots - species only (9 PNG files)\n")
cat("• Comparison grids (2 PNG files)\n") 
cat("• Species specialization tables (9 CSV files)\n")
cat("• R data objects (9 RData files)\n")

cat("\n=== SPECIALIZATION INSIGHTS ===\n")
cat("🔴 High specialists (3): Habitat-specific species\n")
cat("🟠 Moderate specialists (2): Prefer certain habitats\n") 
cat("🔵 Low specialists (1): Some habitat preferences\n")
cat("🟢 Generalists (0): Found in many habitat types\n")
cat("\nNote: Species without specialization data (NA) have been excluded from analysis\n")

