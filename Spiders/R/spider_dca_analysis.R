# robust_dca_oribatida.R
# DCA (Detrended Correspondence Analysis) for Oribatida data with specialization
# Comprehensive analysis with error handling and data validation

# Set working directory
setwd("data")

# Load required libraries
library(vegan)
library(ggplot2)
library(dplyr)
library(readr)

cat("=== ORIBATIDA DCA ANALYSIS ===\n\n")

# Create results directory if it doesn't exist
if(!dir.exists("../results")) {
  dir.create("../results")
  cat("Created directory: ../results\n")
}

# Function to prepare data for DCA
prepare_dca_data <- function(file_path, file_name) {
  cat("Processing:", file_name, "\n")
  
  if (!file.exists(file_path)) {
    cat("  ❌ File not found:", file_path, "\n")
    return(NULL)
  }
  
  # Read data
  data <- read_csv(file_path, show_col_types = FALSE)
  cat(sprintf("  📊 Original dimensions: %d rows, %d columns\n", nrow(data), ncol(data)))
  
  # Check if first row is specialization data
  if (data[1, 1] == "Specialization") {
    cat("  🔍 Detected specialization row - extracting and removing\n")
    specialization_row <- data[1, -1]  # Remove Locality column
    data <- data[-1, ]  # Remove specialization row
  } else {
    specialization_row <- NULL
    cat("  ℹ️  No specialization row detected\n")
  }
  
  # Extract locality names and species matrix
  localities <- data[[1]]
  species_matrix <- data[, -1]
  
  # Convert to numeric matrix with proper row names
  species_matrix <- as.data.frame(species_matrix)
  row.names(species_matrix) <- localities
  species_matrix <- as.matrix(sapply(species_matrix, as.numeric))
  
  cat(sprintf("  📊 Community data dimensions: %d localities, %d species\n", 
              nrow(species_matrix), ncol(species_matrix)))
  
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
    if (!is.null(specialization_row)) {
      specialization_row <- specialization_row[!empty_species]
    }
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
  
  cat(sprintf("  ✅ Final dimensions: %d localities, %d species\n", 
              nrow(species_matrix), ncol(species_matrix)))
  cat(sprintf("  ✅ Total counts: %d\n", sum(species_matrix)))
  cat(sprintf("  ✅ Data range: %g to %g\n", min(species_matrix), max(species_matrix)))
  
  return(list(
    matrix = species_matrix,
    specialization = specialization_row
  ))
}

# Function to run DCA and create plots
run_dca_with_specialization <- function(data_obj, dataset_name) {
  cat("\n--- Running DCA for:", dataset_name, "---\n")
  
  if (is.null(data_obj)) {
    cat("❌ No data provided for DCA\n")
    return(NULL)
  }
  
  species_matrix <- data_obj$matrix
  specialization_row <- data_obj$specialization
  
  # Final data validation before DCA
  cat("  Data validation:\n")
  cat(sprintf("    Matrix dimensions: %d × %d\n", nrow(species_matrix), ncol(species_matrix)))
  cat(sprintf("    Value range: %g to %g\n", min(species_matrix), max(species_matrix)))
  cat(sprintf("    NA values: %d\n", sum(is.na(species_matrix))))
  
  # Try to run DCA with error handling
  dca_result <- NULL
  tryCatch({
    dca_result <- decorana(species_matrix)
    cat("  ✅ DCA completed successfully\n")
  }, error = function(e) {
    cat("  ❌ DCA failed with error:", e$message, "\n")
    return(NULL)
  })
  
  if (is.null(dca_result)) {
    return(NULL)
  }
  
  # Print summary
  eigenvals <- dca_result$evals
  cat("DCA Summary:\n")
  cat(sprintf("  Eigenvalues: DCA1=%.3f, DCA2=%.3f, DCA3=%.3f, DCA4=%.3f\n",
              eigenvals[1], eigenvals[2], eigenvals[3], eigenvals[4]))
  cat(sprintf("  Total inertia: %.3f\n", sum(eigenvals)))
  cat(sprintf("  Proportion explained by axes 1-2: %.1f%%\n", 
              sum(eigenvals[1:2])/sum(eigenvals) * 100))
  
  # Extract site scores using different approach
  tryCatch({
    site_scores <- scores(dca_result, display = "sites", choices = c(1, 2))
    if (nrow(site_scores) == 0) {
      # Alternative extraction method
      site_scores <- dca_result$rproj[, 1:2]
      colnames(site_scores) <- c("DCA1", "DCA2")
    }
  }, error = function(e) {
    # Fallback extraction
    site_scores <- dca_result$rproj[, 1:2]
    colnames(site_scores) <- c("DCA1", "DCA2")
  })
  
  site_df <- data.frame(
    Locality = rownames(site_scores),
    DCA1 = site_scores[, 1],
    DCA2 = site_scores[, 2],
    Site_Type = ifelse(grepl("_A$", rownames(site_scores)), "Pristine (A)", "Degraded (B)")
  )
  
  # Extract species scores using different approach
  tryCatch({
    species_scores <- scores(dca_result, display = "species", choices = c(1, 2))
    if (nrow(species_scores) == 0) {
      # Alternative extraction method
      species_scores <- dca_result$cproj[, 1:2]
      colnames(species_scores) <- c("DCA1", "DCA2")
    }
  }, error = function(e) {
    # Fallback extraction
    species_scores <- dca_result$cproj[, 1:2]
    colnames(species_scores) <- c("DCA1", "DCA2")
  })
  
  species_df <- data.frame(
    Species = rownames(species_scores),
    DCA1 = species_scores[, 1],
    DCA2 = species_scores[, 2]
  )
  
  # Add specialization info if available
  if (!is.null(specialization_row)) {
    # Match species names to specialization values
    spec_values <- as.numeric(specialization_row[match(species_df$Species, names(specialization_row))])
    species_df$Specialization <- spec_values
    species_df$Specialization_Level <- case_when(
      spec_values == 1 ~ "Fen specialists (1)",
      spec_values == 2 ~ "Fen tolerant (2)", 
      spec_values == 3 ~ "Generalists (3)",
      is.na(spec_values) ~ "Unknown"
    )
    species_df$Specialization_Level <- factor(species_df$Specialization_Level, 
                                              levels = c("Fen specialists (1)", "Fen tolerant (2)", 
                                                         "Generalists (3)", "Unknown"))
  }
  
  # Create basic DCA plot (sites and species)
  p_basic <- ggplot() +
    # Add species points (red)
    geom_point(data = species_df, aes(x = DCA1, y = DCA2), 
               color = "#E57373", alpha = 0.6, size = 1.5) +
    # Add site points (blue) with labels
    geom_point(data = site_df, aes(x = DCA1, y = DCA2), 
               color = "#1976D2", size = 3) +
    geom_text(data = site_df, aes(x = DCA1, y = DCA2, label = Locality), 
              color = "#1976D2", size = 2.5, fontface = "bold", vjust = -0.5) +
    labs(
      title = paste("DCA Ordination:", dataset_name),
      subtitle = paste("Eigenvalues: DCA1 =", round(eigenvals[1], 3), 
                       ", DCA2 =", round(eigenvals[2], 3)),
      x = paste("DCA1 (", round(eigenvals[1], 3), ")"),
      y = paste("DCA2 (", round(eigenvals[2], 3), ")"),
      caption = "Blue = Localities, Red = Species"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.title = element_text(size = 11),
      plot.caption = element_text(size = 10, hjust = 0)
    )
  
  # Create specialization plot if specialization data exists
  p_specialization <- NULL
  if (!is.null(specialization_row) && "Specialization_Level" %in% names(species_df)) {
    p_specialization <- ggplot() +
      # Add site points (gray) with labels
      geom_point(data = site_df, aes(x = DCA1, y = DCA2), 
                 color = "gray50", size = 2, alpha = 0.8) +
      geom_text(data = site_df, aes(x = DCA1, y = DCA2, label = Locality), 
                color = "gray30", size = 2.2, vjust = -0.8) +
      # Add species points colored by specialization
      geom_point(data = species_df, aes(x = DCA1, y = DCA2, color = Specialization_Level), 
                 size = 2.5, alpha = 0.8) +
      geom_text(data = species_df, aes(x = DCA1, y = DCA2, label = Species, color = Specialization_Level), 
                size = 1.8, vjust = -0.5) +
      scale_color_manual(
        values = c(
          "Fen specialists (1)" = "#81C784",      # Green
          "Fen tolerant (2)" = "#FFB74D",         # Orange
          "Generalists (3)" = "#E57373",          # Red
          "Unknown" = "gray60"                    # Gray
        ),
        name = "Specialization Level"
      ) +
      labs(
        title = paste("DCA Ordination with Species Specialization:", dataset_name),
        subtitle = paste("Eigenvalues: DCA1 =", round(eigenvals[1], 3), 
                         ", DCA2 =", round(eigenvals[2], 3)),
        x = paste("DCA1 (", round(eigenvals[1], 3), ")"),
        y = paste("DCA2 (", round(eigenvals[2], 3), ")"),
        caption = "Gray = Localities | Colored = Species by specialization level"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 13, face = "bold"),
        plot.subtitle = element_text(size = 11),
        axis.title = element_text(size = 10),
        plot.caption = element_text(size = 9, hjust = 0),
        legend.position = "right",
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 9)
      ) +
      guides(color = guide_legend(override.aes = list(size = 4)))
  }
  
  # Return results
  return(list(
    dca = dca_result,
    sites = site_df,
    species = species_df,
    plot_basic = p_basic,
    plot_specialization = p_specialization,
    eigenvalues = eigenvals
  ))
}

# Process files
files_to_analyze <- list(
  "Count Data" = "Oribatida_final_with_specialization.csv",
  "Presence-Absence Data" = "Oribatida_finalPA_with_specialization.csv"
)

dca_results <- list()

cat("PROCESSING ORIBATIDA FILES...\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

for (dataset_name in names(files_to_analyze)) {
  file_path <- files_to_analyze[[dataset_name]]
  
  # Prepare data
  data_obj <- prepare_dca_data(file_path, dataset_name)
  
  if (!is.null(data_obj)) {
    # Run DCA
    dca_result <- run_dca_with_specialization(data_obj, dataset_name)
    
    if (!is.null(dca_result)) {
      dca_results[[dataset_name]] <- dca_result
      
      # Save basic DCA plot
      basic_filename <- paste0("../results/dca_", gsub("[^A-Za-z0-9]", "_", tolower(dataset_name)), ".png")
      ggsave(basic_filename, dca_result$plot_basic, width = 12, height = 8, dpi = 300)
      cat("  💾 Saved basic DCA plot:", basic_filename, "\n")
      
      # Save specialization plot if available
      if (!is.null(dca_result$plot_specialization)) {
        spec_filename <- paste0("../results/dca_specialization_", gsub("[^A-Za-z0-9]", "_", tolower(dataset_name)), ".png")
        ggsave(spec_filename, dca_result$plot_specialization, width = 14, height = 10, dpi = 300)
        cat("  💾 Saved specialization plot:", spec_filename, "\n")
      }
    }
  }
  
  cat(paste(rep("-", 40), collapse = ""), "\n")
}

# Summary table of eigenvalues
if (length(dca_results) > 0) {
  cat("\n=== DCA EIGENVALUES COMPARISON ===\n")
  eigenvalue_summary <- data.frame(
    Dataset = character(),
    DCA1 = numeric(),
    DCA2 = numeric(),
    DCA3 = numeric(),
    DCA4 = numeric(),
    Total_Inertia = numeric(),
    Axes1_2_Percent = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (name in names(dca_results)) {
    eigenvals <- dca_results[[name]]$eigenvalues
    eigenvalue_summary <- rbind(eigenvalue_summary, data.frame(
      Dataset = name,
      DCA1 = round(eigenvals[1], 3),
      DCA2 = round(eigenvals[2], 3),
      DCA3 = round(eigenvals[3], 3),
      DCA4 = round(eigenvals[4], 3),
      Total_Inertia = round(sum(eigenvals), 3),
      Axes1_2_Percent = round(sum(eigenvals[1:2])/sum(eigenvals) * 100, 1)
    ))
  }
  
  print(eigenvalue_summary)
  
  # Save eigenvalue summary
  write_csv(eigenvalue_summary, "../results/dca_eigenvalues_summary.csv")
  cat("\n💾 Saved eigenvalue summary: ../results/dca_eigenvalues_summary.csv\n")
  
  # Save site and species scores
  for (name in names(dca_results)) {
    # Site scores
    site_file <- paste0("../results/dca_sites_", gsub("[^A-Za-z0-9]", "_", tolower(name)), ".csv")
    write_csv(dca_results[[name]]$sites, site_file)
    
    # Species scores
    species_file <- paste0("../results/dca_species_", gsub("[^A-Za-z0-9]", "_", tolower(name)), ".csv")
    write_csv(dca_results[[name]]$species, species_file)
    
    cat("💾 Saved scores for", name, "\n")
  }
}

cat("\n=== ORIBATIDA DCA ANALYSIS COMPLETE ===\n")
cat("📁 All outputs saved to: Oribatida/results/\n")

if (length(dca_results) > 0) {
  cat("\nFiles created:\n")
  cat("• DCA ordination plots (basic)\n")
  cat("• DCA specialization plots (if specialization data available)\n")
  cat("• Eigenvalue comparison table\n")
  cat("• Site and species score tables\n")
  
  cat("\n=== INTERPRETATION GUIDE ===\n")
  cat("• Higher DCA eigenvalues (>0.5) indicate strong ecological gradients\n")
  cat("• Eigenvalues <0.3 suggest weak gradients or random patterns\n")
  cat("• Compare count vs presence-absence data patterns\n")
  cat("• Look for clustering of pristine (A) vs degraded (B) sites\n")
  cat("• Species specialization colors show habitat preferences\n")
} else {
  cat("❌ No successful DCA analyses completed - check your data files\n")
}

