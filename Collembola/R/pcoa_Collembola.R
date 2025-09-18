# Simple PCOA Analysis with Species Specialization and Bray-Curtis
# Two analyses: Abundance + Presence-Absence
# Species colored by specialization level

# Load libraries
library(vegan)
library(ggplot2)
library(readr)
library(dplyr)
getwd()
# Set working directory and create results folder
setwd("Collembola/data")
if(!dir.exists("../results")) dir.create("../results")

cat("=== Collembola PCOA WITH SPECIALIZATION (Bray-Curtis) ===\n")

# Function to run PCOA analysis with specialization
run_pcoa_specialization <- function(file_name, dataset_name) {
  cat("\nProcessing:", dataset_name, "\n")
  
  # Read data
  data <- read_csv(file_name, show_col_types = FALSE)
  
  # Extract specialization row (second row)
  specialization_row <- data[1, -1]  # Remove Locality column
  
  # Remove specialization row for PCOA analysis
  community_data <- data[-1, ]
  
  # Extract localities and species matrix
  localities <- community_data[[1]]
  species_matrix <- as.matrix(community_data[, -1])
  rownames(species_matrix) <- localities
  
  # Replace NA with 0
  species_matrix[is.na(species_matrix)] <- 0
  
  # Remove empty species and sites
  species_matrix <- species_matrix[, colSums(species_matrix) > 0]
  species_matrix <- species_matrix[rowSums(species_matrix) > 0, ]
  
  # Calculate Bray-Curtis dissimilarity
  cat("  Calculating Bray-Curtis dissimilarity\n")
  bray_dist <- vegdist(species_matrix, method = "bray")
  
  # Run PCOA
  pcoa_result <- cmdscale(bray_dist, k = 2, eig = TRUE)
  eigenvals <- pcoa_result$eig
  
  # Calculate percentage of variance explained
  percent_var <- round((eigenvals / sum(eigenvals)) * 100, 2)
  
  cat(sprintf("  Eigenvalues: PCo1=%.3f (%.1f%%), PCo2=%.3f (%.1f%%)\n", 
              eigenvals[1], percent_var[1], eigenvals[2], percent_var[2]))
  
  # Get site scores
  site_scores <- pcoa_result$points
  colnames(site_scores) <- c("PCo1", "PCo2")
  
  # Create site dataframe
  site_df <- data.frame(
    Locality = rownames(site_scores),
    PCo1 = site_scores[, 1],
    PCo2 = site_scores[, 2],
    Site_Type = ifelse(grepl("_A$", rownames(site_scores)), "Pristine (A)", "Degraded (B)")
  )
  
  # Calculate species scores using weighted averages
  cat("  Calculating species scores using weighted averages\n")
  species_scores <- wascores(site_scores, species_matrix)
  
  # Create species dataframe with specialization
  species_names <- rownames(species_scores)
  spec_values <- as.numeric(specialization_row[match(species_names, names(specialization_row))])
  
  species_df <- data.frame(
    Species = species_names,
    PCo1 = species_scores[, 1],
    PCo2 = species_scores[, 2],
    Specialization = spec_values
  ) %>%
    mutate(
      Specialization_Level = case_when(
        Specialization == 1 ~ "Tirfobionti (1)",
        Specialization == 2 ~ "Neustonicke_hygrofilni (2)", 
        Specialization == 3 ~ "Ostatni (3)",
        is.na(Specialization) ~ "Unknown"
      ),
      Specialization_Level = factor(Specialization_Level, 
                                    levels = c("Tirfobionti (1)", "Neustonicke_hygrofilni (2)", 
                                               "Ostatni (3)", "Unknown"))
    )
  
  # Create plot with specialization
  p <- ggplot() +
    # Site points (gray)
    geom_point(data = site_df, aes(x = PCo1, y = PCo2), 
               color = "gray50", size = 2.5, alpha = 0.8) +
    # Site labels
    geom_text(data = site_df, aes(x = PCo1, y = PCo2, label = Locality), 
              color = "gray30", size = 2.5, vjust = -0.8) +
    # Species points colored by specialization
    geom_point(data = species_df, aes(x = PCo1, y = PCo2, color = Specialization_Level), 
               size = 2.5, alpha = 0.8) +
    # Species labels
    geom_text(data = species_df, aes(x = PCo1, y = PCo2, label = Species, color = Specialization_Level), 
              size = 1.8, vjust = -0.5) +
    # Specialization colors
    scale_color_manual(
      values = c(
        "Tirfobionti (1)" = "#81C784",      # Green
        "Neustonicke_hygrofilni (2)" = "#FFB74D",         # Orange
        "Ostatni (3)" = "#E57373",          # Red
        "Unknown" = "gray60"                    # Gray
      ),
      name = "Specialization Level"
    ) +
    # Labels
    labs(
      title = paste("PCOA with Species Specialization (Bray-Curtis):", dataset_name),
      subtitle = paste("PCo1 =", percent_var[1], "% | PCo2 =", percent_var[2], "%"),
      x = paste("PCo1 (", percent_var[1], "%)"),
      y = paste("PCo2 (", percent_var[2], "%)"),
      caption = "Gray = Localities | Colored = Species by specialization level"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      legend.position = "right",
      legend.title = element_text(size = 11, face = "bold")
    ) +
    guides(color = guide_legend(override.aes = list(size = 4)))
  
  # Save plot with error handling
  tryCatch({
    plot_name <- paste0("../results/pcoa_bray_specialization_", gsub(" ", "_", tolower(dataset_name)), ".png")
    ggsave(plot_name, p, width = 12, height = 8, dpi = 300)
    cat("  ✅ Plot saved:", plot_name, "\n")
  }, error = function(e) {
    cat("  ❌ SAVE ERROR:", e$message, "\n")
  })
  
  # Print specialization summary
  cat("  Specialization counts:\n")
  print(table(species_df$Specialization_Level, useNA = "ifany"))
  
  return(list(pcoa = pcoa_result, plot = p, eigenvals = eigenvals, 
              percent_var = percent_var, species = species_df, sites = site_df))
}

# Run both analyses with specialization
results <- list()

# 1. Abundance data
results$abundance <- run_pcoa_specialization("Collembola_final_with_specialization.csv", 
                                             "Abundance Data")

# 2. Presence-absence data
results$presence <- run_pcoa_specialization("Collembola_finalPA_with_specialization.csv", 
                                            "Presence-Absence Data")

# Summary table
cat("\n=== SUMMARY ===\n")
summary_df <- data.frame(
  Dataset = c("Abundance", "Presence-Absence"),
  PCo1_Eigenvalue = c(round(results$abundance$eigenvals[1], 3), round(results$presence$eigenvals[1], 3)),
  PCo1_Percent = c(results$abundance$percent_var[1], results$presence$percent_var[1]),
  PCo2_Eigenvalue = c(round(results$abundance$eigenvals[2], 3), round(results$presence$eigenvals[2], 3)),
  PCo2_Percent = c(results$abundance$percent_var[2], results$presence$percent_var[2]),
  Total_Variance = c(
    round(sum(results$abundance$percent_var[1:2]), 1),
    round(sum(results$presence$percent_var[1:2]), 1)
  )
)
print(summary_df)

cat("\n📁 Results saved to: Collembola/results/\n")
cat("Files created:\n")
cat("• pcoa_bray_specialization_abundance_data.png\n")
cat("• pcoa_bray_specialization_presence-absence_data.png\n")

cat("\n🎨 Specialization colors:\n")
cat("• Green = Tirfobionti (1)\n")
cat("• Orange = Neustonicke_hygrofilni (2)\n")
cat("• Red = Ostatni (3)\n")
cat("• Gray = Sites and Unknown species\n")

cat("\n📊 Method: Bray-Curtis dissimilarity with PCOA ordination\n")