# Fungi PCOA Analysis
# Two analyses: Abundance (Bray-Curtis) + Presence-Absence (Jaccard)
# Adapted from diatoms script for fungi data without specialization

# Load libraries
library(vegan)
library(ggplot2)
library(ggrepel)
library(readr)
library(dplyr)
library(stringr)  # Added for str_to_title function

# Set working directory and create results folder
setwd("Fungi/data")
if(!dir.exists("../results")) dir.create("../results")

cat("=== Fungi PCOA ANALYSIS ===\n")

# Function to create plot with specific labeling strategy
create_pcoa_plot <- function(site_df, species_df, dataset_name, distance_method, percent_var, labeling_version = "extreme") {
  
  # Calculate statistics for labeling explanations
  total_species <- nrow(species_df)
  
  # Base plot with sites and species points
  p <- ggplot() +
    # Site points (blue)
    geom_point(data = site_df, aes(x = PCo1, y = PCo2), 
               color = "steelblue", size = 3, alpha = 0.8) +
    # Site labels with repelling
    geom_text_repel(data = site_df, aes(x = PCo1, y = PCo2, label = Locality), 
                    color = "steelblue", size = 2.5, fontface = "bold",
                    box.padding = 0.3, point.padding = 0.2,
                    segment.color = "lightblue", segment.size = 0.3,
                    max.overlaps = Inf) +
    # Species points (red) - always show all
    geom_point(data = species_df, aes(x = PCo1, y = PCo2), 
               color = "firebrick", size = 2, alpha = 0.7)
  
  # Add species labels based on version
  if (labeling_version == "extreme") {
    # OPTION 1: Only most extreme species (top 15%)
    extreme_species <- species_df %>% 
      filter(abs(PCo1) > quantile(abs(PCo1), 0.85) | abs(PCo2) > quantile(abs(PCo2), 0.85))
    n_labeled <- nrow(extreme_species)
    
    p <- p + geom_text_repel(data = extreme_species, 
                             aes(x = PCo1, y = PCo2, label = Species), 
                             color = "darkred", size = 2, fontface = "italic",
                             box.padding = 0.2, point.padding = 0.1,
                             segment.color = "pink", segment.size = 0.2,
                             max.overlaps = 20)
    
    caption_text <- sprintf("Blue = Sites | Red = Fungal OTUs | Labels: %d/%d species (top 15%% most extreme)\nExtreme = species with PCo1 or PCo2 coordinates in the top 15%% furthest from origin (0,0)", 
                            n_labeled, total_species)
    
  } else if (labeling_version == "moderate") {
    # OPTION 2: Moderately extreme species (top 50%)
    moderate_species <- species_df %>% 
      filter(abs(PCo1) > quantile(abs(PCo1), 0.5) | abs(PCo2) > quantile(abs(PCo2), 0.5))
    n_labeled <- nrow(moderate_species)
    
    p <- p + geom_text_repel(data = moderate_species, 
                             aes(x = PCo1, y = PCo2, label = Species), 
                             color = "darkred", size = 1.8, fontface = "italic",
                             box.padding = 0.15, point.padding = 0.08,
                             segment.color = "pink", segment.size = 0.15,
                             max.overlaps = 60)
    
    caption_text <- sprintf("Blue = Sites | Red = Fungal OTUs | Labels: %d/%d species (top 50%% most extreme)\nExtreme = species with PCo1 or PCo2 coordinates in the top 50%% furthest from origin (0,0)", 
                            n_labeled, total_species)
    
  } else if (labeling_version == "all") {
    # OPTION 3: All species
    p <- p + geom_text_repel(data = species_df, 
                             aes(x = PCo1, y = PCo2, label = Species), 
                             color = "darkred", size = 1.5, fontface = "italic",
                             box.padding = 0.1, point.padding = 0.05,
                             segment.color = "pink", segment.size = 0.1,
                             max.overlaps = Inf)
    
    caption_text <- sprintf("Blue = Sites | Red = Fungal OTUs | Labels: ALL %d species shown\nNote: Overlapping labels indicate high species density in ordination space", 
                            total_species)
  }
  
  # Add labels and theme
  p <- p + labs(
    title = paste("PCOA Ordination:", dataset_name, "-", str_to_title(labeling_version), "Labeling"),
    subtitle = paste("Distance method:", distance_method, "| PCo1 =", percent_var[1], "% | PCo2 =", percent_var[2], "%"),
    x = paste("PCo1 (", percent_var[1], "%)"),
    y = paste("PCo2 (", percent_var[2], "%)"),
    caption = caption_text
  ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.title = element_text(size = 11),
      plot.caption = element_text(size = 9, color = "gray60", hjust = 0)
    )
  
  return(p)
}

# Function to run PCOA analysis and create all three versions
run_pcoa_fungi <- function(data, dataset_name, is_presence_absence = FALSE) {
  cat("\nProcessing:", dataset_name, "\n")
  
  # Extract localities and species matrix
  localities <- data$Locality
  species_matrix <- as.matrix(data[, -1])  # Remove Locality column
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
  
  # Create species dataframe
  species_df <- data.frame(
    Species = rownames(species_scores),
    PCo1 = species_scores[, 1],
    PCo2 = species_scores[, 2]
  )
  
  # Create and save all three versions
  method_suffix <- if(is_presence_absence) "jaccard" else "bray"
  dataset_suffix <- gsub(" ", "_", tolower(dataset_name))
  
  versions <- c("extreme", "moderate", "all")
  plots <- list()
  
  for (version in versions) {
    cat(sprintf("  Creating %s labeling version\n", version))
    
    p <- create_pcoa_plot(site_df, species_df, dataset_name, distance_method, percent_var, version)
    plots[[version]] <- p
    
    # Save plot
    tryCatch({
      plot_name <- paste0("../results/pcoa_fungi_", method_suffix, "_", dataset_suffix, "_", version, "_labels.png")
      ggsave(plot_name, p, width = 16, height = 12, dpi = 300)
      cat("  ✅ Plot saved:", plot_name, "\n")
    }, error = function(e) {
      cat("  ❌ SAVE ERROR:", e$message, "\n")
    })
  }
  
  return(list(pcoa = pcoa_result, plots = plots, eigenvals = eigenvals, 
              percent_var = percent_var, species = species_df, sites = site_df))
}

# Read the fungi data
cat("Loading fungi data...\n")
fungi_data <- read_csv("Fungi_OTU_final.csv", show_col_types = FALSE)

cat(sprintf("Data loaded: %d sites x %d OTUs\n", 
            nrow(fungi_data), ncol(fungi_data) - 1))

# Create presence-absence version of the data
fungi_data_pa <- fungi_data
fungi_data_pa[, -1] <- ifelse(fungi_data_pa[, -1] > 0, 1, 0)

# Run both analyses
results <- list()

# 1. Abundance data (Bray-Curtis)
cat("\n" , "="*50, "\n")
results$abundance <- run_pcoa_fungi(fungi_data, "Abundance Data", is_presence_absence = FALSE)

# 2. Presence-absence data (Jaccard)
cat("\n", "="*50, "\n")
results$presence <- run_pcoa_fungi(fungi_data_pa, "Presence-Absence Data", is_presence_absence = TRUE)

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

# Save results
cat("\nSaving results...\n")
write_csv(results$abundance$species, "../results/species_scores_abundance_fungi.csv")
write_csv(results$presence$species, "../results/species_scores_presence_fungi.csv")
write_csv(results$abundance$sites, "../results/site_scores_abundance_fungi.csv")
write_csv(results$presence$sites, "../results/site_scores_presence_fungi.csv")

# Save summary table
write_csv(summary_df, "../results/pcoa_summary_fungi.csv")

cat("\n📁 Results saved to: Fungi/results/\n")
cat("Files created:\n")
cat("ABUNDANCE DATA (Bray-Curtis):\n")
cat("• pcoa_fungi_bray_abundance_data_extreme_labels.png (top 15% most extreme species)\n")
cat("• pcoa_fungi_bray_abundance_data_moderate_labels.png (top 50% most extreme species)\n")
cat("• pcoa_fungi_bray_abundance_data_all_labels.png (ALL species labeled)\n")
cat("PRESENCE-ABSENCE DATA (Jaccard):\n")
cat("• pcoa_fungi_jaccard_presence-absence_data_extreme_labels.png (top 15% most extreme species)\n")
cat("• pcoa_fungi_jaccard_presence-absence_data_moderate_labels.png (top 50% most extreme species)\n")
cat("• pcoa_fungi_jaccard_presence-absence_data_all_labels.png (ALL species labeled)\n")
cat("DATA FILES:\n")
cat("• species_scores_abundance_fungi.csv\n")
cat("• species_scores_presence_fungi.csv\n")
cat("• site_scores_abundance_fungi.csv\n")
cat("• site_scores_presence_fungi.csv\n")
cat("• pcoa_summary_fungi.csv\n")

cat("\n📊 Methods:\n")
cat("• Abundance data: Bray-Curtis dissimilarity with PCOA ordination\n")
cat("• Presence-Absence data: Jaccard dissimilarity with PCOA ordination\n")
cat("• Species scores calculated using weighted averages (wascores)\n")

cat("\n🎨 Labeling strategies explained:\n")
cat("• EXTREME (15%): Shows species with PCo1 or PCo2 coordinates in the top 15% furthest from origin\n")
cat("  - These are the species that most strongly drive the ordination patterns\n")
cat("  - Best for identifying key species that differentiate communities\n")
cat("• MODERATE (50%): Shows species with coordinates in the top 50% furthest from origin\n")
cat("  - Balances detail with readability\n")
cat("  - Good compromise for seeing more species without total overcrowding\n")
cat("• ALL (100%): Shows every single fungal OTU\n")
cat("  - Complete information but may have overlapping labels\n")
cat("  - Use when you need to see every species or for detailed inspection\n")

cat("\n🔍 What 'extreme' means:\n")
cat("• Distance from origin (0,0) calculated as: abs(PCo1) and abs(PCo2)\n")
cat("• Species further from center have stronger association with ordination axes\n")
cat("• These species contribute most to the patterns separating your sites\n")
cat("• Origin (0,0) represents the 'average' community composition\n")

cat("\n📋 Next steps:\n")
cat("• Compare all three labeling versions to choose your preferred level of detail\n")
cat("• Extreme version: Best for presentations and identifying key driver species\n")
cat("• Moderate version: Good for detailed analysis while maintaining readability\n")
cat("• All version: Use for comprehensive species identification and complete data inspection\n")
cat("• Check which species appear consistently in extreme positions across both analyses\n")

