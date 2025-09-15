# Simple DCA Analysis with Species Specialization
# Two analyses: Abundance (Hellinger) + Presence-Absence
# Species colored by specialization level

# Load libraries
library(vegan)
library(ggplot2)
library(readr)
library(dplyr)

# Set working directory and create results folder
setwd("Collembola/data")
if(!dir.exists("../results")) dir.create("../results")

cat("=== Collembola DCA WITH SPECIALIZATION ===\n")

# Function to run DCA analysis with specialization
run_dca_specialization <- function(file_name, dataset_name, use_hellinger = FALSE) {
  cat("\nProcessing:", dataset_name, "\n")
  
  # Read data
  data <- read_csv(file_name, show_col_types = FALSE)
  
  # Extract specialization row (second row)
  specialization_row <- data[1, -1]  # Remove Locality column
  
  # Remove specialization row for DCA analysis
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
  
  # Create site dataframe
  site_df <- data.frame(
    Locality = rownames(site_scores),
    DCA1 = site_scores[, 1],
    DCA2 = site_scores[, 2],
    Site_Type = ifelse(grepl("_A$", rownames(site_scores)), "Pristine (A)", "Degraded (B)")
  )
  
  # Create species dataframe with specialization
  species_names <- rownames(species_scores)
  spec_values <- as.numeric(specialization_row[match(species_names, names(specialization_row))])
  
  species_df <- data.frame(
    Species = species_names,
    DCA1 = species_scores[, 1],
    DCA2 = species_scores[, 2],
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
  transformation_text <- if(use_hellinger) "Hellinger" else "None"
  
  p <- ggplot() +
    # Site points (gray)
    geom_point(data = site_df, aes(x = DCA1, y = DCA2), 
               color = "gray50", size = 2.5, alpha = 0.8) +
    # Site labels
    geom_text(data = site_df, aes(x = DCA1, y = DCA2, label = Locality), 
              color = "gray30", size = 2.5, vjust = -0.8) +
    # Species points colored by specialization
    geom_point(data = species_df, aes(x = DCA1, y = DCA2, color = Specialization_Level), 
               size = 2.5, alpha = 0.8) +
    # Species labels
    geom_text(data = species_df, aes(x = DCA1, y = DCA2, label = Species, color = Specialization_Level), 
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
      title = paste("DCA with Species Specialization:", dataset_name),
      subtitle = paste("Transformation:", transformation_text, "| DCA1 =", round(eigenvals[1], 3), 
                       "DCA2 =", round(eigenvals[2], 3)),
      x = paste("DCA1 (", round(eigenvals[1], 3), ")"),
      y = paste("DCA2 (", round(eigenvals[2], 3), ")"),
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
    plot_name <- paste0("../results/dca_specialization_", gsub(" ", "_", tolower(dataset_name)), ".png")
    ggsave(plot_name, p, width = 12, height = 8, dpi = 300)
    cat("  ✅ Plot saved:", plot_name, "\n")
  }, error = function(e) {
    cat("  ❌ SAVE ERROR:", e$message, "\n")
  })
  
  # Print specialization summary
  cat("  Specialization counts:\n")
  print(table(species_df$Specialization_Level, useNA = "ifany"))
  
  return(list(dca = dca_result, plot = p, eigenvals = eigenvals, species = species_df))
}

# Run both analyses with specialization
results <- list()

# 1. Abundance data with Hellinger
results$abundance <- run_dca_specialization("Collembola_final_with_specialization.csv", 
                                            "Abundance Data", use_hellinger = TRUE)

# 2. Presence-absence data
results$presence <- run_dca_specialization("Collembola_finalPA_with_specialization.csv", 
                                           "Presence-Absence Data", use_hellinger = FALSE)

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

cat("\n📁 Results saved to: Collembola/results/\n")
cat("Files created:\n")
cat("• dca_specialization_abundance_data.png\n")
cat("• dca_specialization_presence-absence_data.png\n")

cat("\n🎨 Specialization colors:\n")
cat("• Green = Tirfobionti (1)\n")
cat("• Orange = Neustonicke_hygrofilni (2)\n")
cat("• Red = Ostatni (3)\n")
cat("• Gray = Sites and Unknown species\n")

