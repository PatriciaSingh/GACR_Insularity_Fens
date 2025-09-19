# pcoa_analysis_separated_files.R
# PCoA analysis using properly separated files

# Load libraries
library(vegan)
library(ggplot2)
library(ggrepel)
library(dplyr)

# Create results folder
if(!dir.exists("Diatoms")) dir.create("Diatoms")
if(!dir.exists("Diatoms/results")) dir.create("Diatoms/results")

cat("=== DIATOMS PCOA WITH SEPARATED FILES ===\n")

# Read species specialization data
species_traits <- read.csv("Diatoms_A_spring_Specializace.csv", stringsAsFactors = FALSE)
cat("Species traits loaded:", nrow(species_traits), "species\n")

# Clean up specialization categories
species_traits <- species_traits %>%
  mutate(
    Specialization_Clean = case_when(
      Specializatio_by_words == "Fen_specialist" ~ "Fen Specialist",
      Specializatio_by_words == "Generalist" ~ "Generalist",
      Specializatio_by_words == "Bog_specialist" ~ "Bog Specialist",
      is.na(Specializatio_by_words) | Specializatio_by_words == "" ~ "Unknown",
      TRUE ~ as.character(Specializatio_by_words)
    ),
    Specialization_Clean = factor(Specialization_Clean, 
                                  levels = c("Bog Specialist", "Fen Specialist", "Generalist", "Unknown"))
  )

cat("Specialization summary:\n")
print(table(species_traits$Specialization_Clean))

# Function to run PCoA analysis
run_pcoa_analysis <- function(abundance_file, dataset_name, is_presence_absence = FALSE) {
  cat("\nProcessing:", dataset_name, "\n")
  
  # Read abundance/PA data
  abundance_data <- read.csv(abundance_file, stringsAsFactors = FALSE, check.names = FALSE)
  cat("  Data dimensions:", nrow(abundance_data), "samples ×", ncol(abundance_data)-1, "species\n")
  
  # Extract sample names and species matrix
  sample_names <- abundance_data$Short_names
  species_matrix <- as.matrix(abundance_data[, -1])  # Remove first column
  rownames(species_matrix) <- sample_names
  
  # Get species short names from column headers
  species_short_names <- colnames(species_matrix)
  
  # Ensure matrix is numeric
  storage.mode(species_matrix) <- "numeric"
  
  # Remove species with no occurrences and samples with no species
  species_matrix <- species_matrix[, colSums(species_matrix, na.rm = TRUE) > 0]
  species_matrix <- species_matrix[rowSums(species_matrix, na.rm = TRUE) > 0, ]
  
  cat("  After filtering:", nrow(species_matrix), "samples ×", ncol(species_matrix), "species\n")
  
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
  
  # Run PCoA
  pcoa_result <- cmdscale(dist_matrix, k = 2, eig = TRUE)
  eigenvals <- pcoa_result$eig
  percent_var <- round((eigenvals / sum(eigenvals)) * 100, 2)
  
  cat(sprintf("  PCoA results: PCo1=%.1f%%, PCo2=%.1f%%\n", percent_var[1], percent_var[2]))
  
  # Create site dataframe
  site_scores <- pcoa_result$points
  colnames(site_scores) <- c("PCo1", "PCo2")
  
  site_df <- data.frame(
    Sample = rownames(site_scores),
    PCo1 = site_scores[, 1],
    PCo2 = site_scores[, 2],
    Site_Type = ifelse(grepl("_SP", rownames(site_scores)), "Spring (SP)", "High Mountain (HM)")
  )
  
  # Calculate species scores using weighted averages
  species_scores <- wascores(site_scores, species_matrix)
  remaining_species_short <- rownames(species_scores)
  
  # Match species traits to remaining species by short names
  species_df <- data.frame(
    Short_Name = remaining_species_short,
    PCo1 = species_scores[, 1],
    PCo2 = species_scores[, 2]
  ) %>%
    left_join(species_traits, by = c("Short_Name" = "Short_names")) %>%
    mutate(
      Specialization_Clean = factor(Specialization_Clean, 
                                    levels = c("Bog Specialist", "Fen Specialist", "Generalist", "Unknown"))
    )
  
  # Handle any unmatched species
  species_df$Specialization_Clean[is.na(species_df$Specialization_Clean)] <- "Unknown"
  
  cat("  Species in ordination:", nrow(species_df), "\n")
  cat("  Specialization counts:\n")
  print(table(species_df$Specialization_Clean, useNA = "ifany"))
  
  # Filter species for plotting to reduce clutter
  if(nrow(species_df) > 50) {
    species_loadings <- sqrt(species_df$PCo1^2 + species_df$PCo2^2)
    high_loading_threshold <- quantile(species_loadings, 0.75, na.rm = TRUE)
    species_to_plot <- species_df[species_loadings >= high_loading_threshold, ]
    plot_subtitle_extra <- paste("| Showing top 25% most influential species (n =", nrow(species_to_plot), ")")
  } else {
    species_to_plot <- species_df
    plot_subtitle_extra <- paste("| All species shown (n =", nrow(species_to_plot), ")")
  }
  
  cat("  Species plotted:", nrow(species_to_plot), "\n")
  
  # Create plot
  p <- ggplot() +
    # Site points (triangles)
    geom_point(data = site_df, aes(x = PCo1, y = PCo2), 
               color = "gray20", size = 4, alpha = 0.8, shape = 17) +
    geom_text_repel(data = site_df, aes(x = PCo1, y = PCo2, label = Sample), 
                    color = "gray20", size = 3, fontface = "bold",
                    box.padding = 0.4, point.padding = 0.3,
                    segment.color = "gray60", segment.size = 0.4,
                    max.overlaps = Inf) +
    # Species points (circles)
    geom_point(data = species_to_plot, aes(x = PCo1, y = PCo2, color = Specialization_Clean), 
               size = 3, alpha = 0.8) +
    geom_text_repel(data = species_to_plot, aes(x = PCo1, y = PCo2, label = Short_Name, color = Specialization_Clean), 
                    size = 2.2, fontface = "bold",
                    box.padding = 0.2, point.padding = 0.2,
                    segment.color = "gray80", segment.size = 0.2,
                    max.overlaps = 15, min.segment.length = 0.1) +
    # Color scheme
    scale_color_manual(
      values = c(
        "Bog Specialist" = "#2E7D32",    # Dark green
        "Fen Specialist" = "#F57C00",    # Orange  
        "Generalist" = "#C62828",        # Dark red
        "Unknown" = "gray50"             # Gray
      ),
      name = "Habitat Specialization"
    ) +
    # Labels and theme
    labs(
      title = paste("PCoA with Habitat Specialization:", dataset_name),
      subtitle = paste("Distance:", distance_method, "| PCo1 =", percent_var[1], "% | PCo2 =", percent_var[2], "%", plot_subtitle_extra),
      x = paste("PCo1 (", percent_var[1], "%)"),
      y = paste("PCo2 (", percent_var[2], "%)"),
      caption = "Triangles = Sites | Circles = Species (short names) colored by habitat specialization"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 11),
      axis.title = element_text(size = 12, face = "bold"),
      legend.position = "right",
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 10),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA)
    ) +
    guides(color = guide_legend(override.aes = list(size = 5, alpha = 1)))
  
  # Save plot
  method_suffix <- if(is_presence_absence) "jaccard" else "bray"
  plot_name <- paste0("Diatoms/results/pcoa_", method_suffix, "_", gsub(" ", "_", tolower(dataset_name)), ".png")
  ggsave(plot_name, p, width = 16, height = 12, dpi = 300, bg = "white")
  cat("  Plot saved:", plot_name, "\n")
  
  return(list(pcoa = pcoa_result, plot = p, eigenvals = eigenvals, 
              percent_var = percent_var, species = species_df, sites = site_df))
}

# Run both analyses
results <- list()

# Abundance data (Bray-Curtis)
results$abundance <- run_pcoa_analysis("Diatoms_A_spring_final_with_short_names.csv", 
                                       "Abundance Data", 
                                       is_presence_absence = FALSE)

# Presence-absence data (Jaccard)
results$presence <- run_pcoa_analysis("Diatoms_A_spring_final_PA_with_short_names.csv",
                                      "Presence-Absence Data", 
                                      is_presence_absence = TRUE)

# Summary table
cat("\n=== ANALYSIS SUMMARY ===\n")
summary_df <- data.frame(
  Dataset = c("Abundance (Bray-Curtis)", "Presence-Absence (Jaccard)"),
  PCo1_Percent = c(results$abundance$percent_var[1], results$presence$percent_var[1]),
  PCo2_Percent = c(results$abundance$percent_var[2], results$presence$percent_var[2]),
  Total_Variance = c(
    round(sum(results$abundance$percent_var[1:2]), 1),
    round(sum(results$presence$percent_var[1:2]), 1)
  ),
  Species_Count = c(nrow(results$abundance$species), nrow(results$presence$species)),
  Sites_Count = c(nrow(results$abundance$sites), nrow(results$presence$sites))
)
print(summary_df)

cat("\n=== RESULTS ===\n")
cat("Files created in Diatoms/results/ folder:\n")
cat("• pcoa_bray-curtis_abundance_data.png\n")
cat("• pcoa_jaccard_presence-absence_data.png\n")

cat("\nSpecialization color scheme:\n")
cat("• Dark Green = Bog Specialist (most specialized)\n") 
cat("• Orange = Fen Specialist (intermediate specialization)\n")
cat("• Dark Red = Generalist (least specialized)\n")
cat("• Gray = Unknown specialization\n")

cat("\nAnalysis completed successfully!\n")

