# Diatoms PCOA Analysis with Species Specialization
# Two analyses: Abundance (Bray-Curtis) + Presence-Absence (Jaccard)
# Species colored by specialization level

# Load libraries
library(vegan)
library(ggplot2)
library(ggrepel)
library(readr)
library(dplyr)
getwd()
# Set working directory and create results folder
setwd("Diatoms/data")
if(!dir.exists("../results")) dir.create("../results")

cat("=== Diatoms PCOA WITH SPECIALIZATION ===\n")

# Load specialization data
cat("Loading specialization data...\n")
specialization_data <- read_csv("Diatoms_A_spring_Specializace.csv", show_col_types = FALSE)

# Function to run PCOA analysis with specialization
run_pcoa_specialization <- function(file_name, dataset_name, is_presence_absence = FALSE) {
  cat("\nProcessing:", dataset_name, "\n")
  
  # Read data
  data <- read_csv(file_name, show_col_types = FALSE)
  
  # Extract localities and species matrix
  localities <- data$Short_names
  species_matrix <- as.matrix(data[, -1])  # Remove Short_names column
  rownames(species_matrix) <- localities
  
  # Replace NA with 0
  species_matrix[is.na(species_matrix)] <- 0
  
  # Remove empty species and sites
  species_matrix <- species_matrix[, colSums(species_matrix) > 0]
  species_matrix <- species_matrix[rowSums(species_matrix) > 0, ]
  
  cat(sprintf("  Matrix dimensions: %d sites x %d species\n", 
              nrow(species_matrix), ncol(species_matrix)))
  
  # Calculate dissimilarity matrix
  if (is_presence_absence) {
    cat("  Calculating Jaccard dissimilarity\n")
    dist_matrix <- vegdist(species_matrix, method = "jaccard")
    distance_method <- "Jaccard"
  } else {
    cat("  Calculating Bray-Curtis dissimilarity\n")
    dist_matrix <- vegdist(species_matrix, method = "bray")
    distance_method <- "Bray-Curtis"
  }
  
  # Run PCOA
  pcoa_result <- cmdscale(dist_matrix, k = 2, eig = TRUE)
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
    PCo2 = site_scores[, 2]
  )
  
  # Calculate species scores using weighted averages
  cat("  Calculating species scores using weighted averages\n")
  species_scores <- wascores(site_scores, species_matrix)
  
  # Create species dataframe with specialization
  species_names <- rownames(species_scores)
  
  # Match species with specialization data
  species_df <- data.frame(
    Species = species_names,
    PCo1 = species_scores[, 1],
    PCo2 = species_scores[, 2]
  ) %>%
    left_join(specialization_data %>% select(Short_names, Specializatio_by_words), 
              by = c("Species" = "Short_names")) %>%
    mutate(
      Specialization_Level = case_when(
        is.na(Specializatio_by_words) ~ "Unknown",
        TRUE ~ Specializatio_by_words
      ),
      Specialization_Level = factor(Specialization_Level)
    )
  
  # Get unique specialization levels for color assignment
  spec_levels <- levels(species_df$Specialization_Level)
  cat("  Specialization levels found:", paste(spec_levels, collapse = ", "), "\n")
  
  # Create color palette
  n_levels <- length(spec_levels)
  if("Unknown" %in% spec_levels) {
    # Assign gray to Unknown, distinct colors to others
    colors <- rainbow(n_levels - 1, s = 0.7, v = 0.8)
    color_palette <- setNames(c(colors, "gray60"), 
                              c(setdiff(spec_levels, "Unknown"), "Unknown"))
  } else {
    color_palette <- setNames(rainbow(n_levels, s = 0.7, v = 0.8), spec_levels)
  }
  
  # Create plot with specialization
  p <- ggplot() +
    # Site points (gray)
    geom_point(data = site_df, aes(x = PCo1, y = PCo2), 
               color = "gray50", size = 2.5, alpha = 0.8) +
    # Site labels with repelling
    geom_text_repel(data = site_df, aes(x = PCo1, y = PCo2, label = Locality), 
                    color = "gray30", size = 2.5, 
                    box.padding = 0.3, point.padding = 0.2,
                    segment.color = "gray70", segment.size = 0.3,
                    max.overlaps = Inf) +
    # Species points colored by specialization
    geom_point(data = species_df, aes(x = PCo1, y = PCo2, color = Specialization_Level), 
               size = 2.5, alpha = 0.8) +
    # Species labels with repelling (smaller text to avoid overcrowding)
    geom_text_repel(data = species_df, aes(x = PCo1, y = PCo2, label = Species, color = Specialization_Level), 
                    size = 1.8, fontface = "bold",
                    box.padding = 0.2, point.padding = 0.1,
                    segment.color = "gray80", segment.size = 0.2,
                    max.overlaps = 20) +  # Limit overlaps to avoid overcrowding
    # Specialization colors
    scale_color_manual(values = color_palette, name = "Specialization Level") +
    # Labels
    labs(
      title = paste("PCOA with Species Specialization:", dataset_name),
      subtitle = paste("Distance method:", distance_method, "| PCo1 =", percent_var[1], "% | PCo2 =", percent_var[2], "%"),
      x = paste("PCo1 (", percent_var[1], "%)"),
      y = paste("PCo2 (", percent_var[2], "%)"),
      caption = "Gray = Sites | Colored = Species by specialization level"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      legend.position = "right",
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 9)
    ) +
    guides(color = guide_legend(override.aes = list(size = 4)))
  
  # Save plot with error handling
  tryCatch({
    method_suffix <- if(is_presence_absence) "jaccard" else "bray"
    plot_name <- paste0("../results/pcoa_", method_suffix, "_specialization_", gsub(" ", "_", tolower(dataset_name)), ".png")
    ggsave(plot_name, p, width = 16, height = 12, dpi = 300)
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

# 1. Abundance data (Bray-Curtis)
results$abundance <- run_pcoa_specialization("Diatoms_A_spring_final_with_short_names.csv", 
                                             "Abundance Data", is_presence_absence = FALSE)

# 2. Presence-absence data (Jaccard)
results$presence <- run_pcoa_specialization("Diatoms_A_spring_final_PA_with_short_names.csv", 
                                            "Presence-Absence Data", is_presence_absence = TRUE)

# Summary table
cat("\n=== SUMMARY ===\n")
summary_df <- data.frame(
  Dataset = c("Abundance (Bray-Curtis)", "Presence-Absence (Jaccard)"),
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

# Save species data with specialization for further analysis
cat("\nSaving species data with specialization...\n")
write_csv(results$abundance$species, "../results/species_scores_abundance_with_specialization.csv")
write_csv(results$presence$species, "../results/species_scores_presence_with_specialization.csv")
write_csv(results$abundance$sites, "../results/site_scores_abundance.csv")
write_csv(results$presence$sites, "../results/site_scores_presence.csv")

cat("\n📁 Results saved to: Diatoms/results/\n")
cat("Files created:\n")
cat("• pcoa_bray_specialization_abundance_data.png\n")
cat("• pcoa_jaccard_specialization_presence-absence_data.png\n")
cat("• species_scores_abundance_with_specialization.csv\n")
cat("• species_scores_presence_with_specialization.csv\n")
cat("• site_scores_abundance.csv\n")
cat("• site_scores_presence.csv\n")

cat("\n🎨 Specialization colors:\n")
cat("• Colors automatically assigned to each specialization category\n")
cat("• Gray = Sites and Unknown species\n")

cat("\n📊 Methods:\n")
cat("• Abundance data: Bray-Curtis dissimilarity with PCOA ordination\n")
cat("• Presence-Absence data: Jaccard dissimilarity with PCOA ordination\n")
cat("• Species specialization from separate file: Diatoms_A_spring_Specializace.csv\n")

cat("\n🔍 Next steps:\n")
cat("• Check the specialization summary to see the distribution of categories\n")
cat("• Consider filtering rare species if plots are overcrowded\n")
cat("• Use the CSV outputs for further statistical analysis\n")

