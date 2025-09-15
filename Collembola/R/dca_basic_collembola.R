# Simple DCA Analysis for Oribatida Data
# Two analyses: Abundance (Hellinger) + Presence-Absence

# Load libraries
library(vegan)
library(ggplot2)
library(readr)

# Set working directory and create results folder
setwd("Oribatida/data")
if(!dir.exists("../results")) dir.create("../results")

cat("=== ORIBATIDA DCA ANALYSIS ===\n")

# Function to run DCA analysis
run_dca <- function(file_name, dataset_name, use_hellinger = FALSE) {
  cat("\nProcessing:", dataset_name, "\n")
  
  # Read data
  data <- read_csv(file_name, show_col_types = FALSE)
  
  # Extract localities and species matrix
  localities <- data[[1]]
  species_matrix <- as.matrix(data[, -1])
  rownames(species_matrix) <- localities
  
  # Replace NA with 0
  species_matrix[is.na(species_matrix)] <- 0
  
  # Apply Hellinger transformation if needed
  if (use_hellinger) {
    cat("  Applying Hellinger transformation\n")
    species_matrix <- decostand(species_matrix, method = "hellinger")
  }
  
  # Run DCA
  dca_result <- decorana(species_matrix)
  eigenvals <- dca_result$evals
  
  cat(sprintf("  Eigenvalues: DCA1=%.3f, DCA2=%.3f\n", eigenvals[1], eigenvals[2]))
  
  # Get scores
  site_scores <- scores(dca_result, display = "sites", choices = c(1, 2))
  species_scores <- scores(dca_result, display = "species", choices = c(1, 2))
  
  # Create site dataframe with colors
  site_df <- data.frame(
    Locality = rownames(site_scores),
    DCA1 = site_scores[, 1],
    DCA2 = site_scores[, 2],
    Site_Type = ifelse(grepl("_A$", rownames(site_scores)), "Pristine (A)", "Degraded (B)")
  )
  
  # Create species dataframe
  species_df <- data.frame(
    Species = rownames(species_scores),
    DCA1 = species_scores[, 1],
    DCA2 = species_scores[, 2]
  )
  
  # Create plot
  transformation_text <- if(use_hellinger) "Hellinger" else "None"
  
  p <- ggplot() +
    # Species points (red, small)
    geom_point(data = species_df, aes(x = DCA1, y = DCA2), 
               color = "#FF6B6B", alpha = 0.6, size = 2) +
    # Site points (colored by type)
    geom_point(data = site_df, aes(x = DCA1, y = DCA2, color = Site_Type), 
               size = 4) +
    # Site labels
    geom_text(data = site_df, aes(x = DCA1, y = DCA2, label = Locality, color = Site_Type), 
              size = 3, fontface = "bold", vjust = -0.8) +
    # Colors
    scale_color_manual(values = c("Pristine (A)" = "#2E7D32", "Degraded (B)" = "#D32F2F")) +
    # Labels
    labs(
      title = paste("DCA Ordination:", dataset_name),
      subtitle = paste("Transformation:", transformation_text, "| DCA1 =", round(eigenvals[1], 3), 
                       "DCA2 =", round(eigenvals[2], 3)),
      x = paste("DCA1 (", round(eigenvals[1], 3), ")"),
      y = paste("DCA2 (", round(eigenvals[2], 3), ")"),
      color = "Site Type"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # Save plot
  plot_name <- paste0("../results/dca_", gsub(" ", "_", tolower(dataset_name)), ".png")
  ggsave(plot_name, p, width = 10, height = 8, dpi = 300)
  cat("  Saved:", plot_name, "\n")
  
  return(list(dca = dca_result, plot = p, eigenvals = eigenvals))
}

# Run both analyses
results <- list()

# 1. Abundance data with Hellinger
results$abundance <- run_dca("Oribatida_final.csv", "Abundance Data", use_hellinger = TRUE)

# 2. Presence-absence data
results$presence <- run_dca("Oribatida_finalPa.csv", "Presence-Absence Data", use_hellinger = FALSE)

# Summary table
cat("\n=== SUMMARY ===\n")
summary_df <- data.frame(
  Dataset = c("Abundance (Hellinger)", "Presence-Absence"),
  DCA1 = c(round(results$abundance$eigenvals[1], 3), round(results$presence$eigenvals[1], 3)),
  DCA2 = c(round(results$abundance$eigenvals[2], 3), round(results$presence$eigenvals[2], 3)),
  Variance_Explained = c(
    round(sum(results$abundance$eigenvals[1:2])/sum(results$abundance$eigenvals) * 100, 1),
    round(sum(results$presence$eigenvals[1:2])/sum(results$presence$eigenvals) * 100, 1)
  )
)
print(summary_df)

cat("\n📁 Results saved to: Oribatida/results/\n")
cat("Files created:\n")
cat("• dca_abundance_data.png\n")
cat("• dca_presence-absence_data.png\n")

cat("\n🎨 Plot colors:\n")
cat("• Green = Pristine sites (A)\n")
cat("• Red = Degraded sites (B) \n")
cat("• Pink = Species\n")

