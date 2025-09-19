# Diatom Locality Comparison - Species Richness Analysis
# Simple visualization showing which localities are most species-rich
# Input: Diatoms/data/diatom_specialist_summary_by_locality.csv
# Output: Diatoms/results/

# Load libraries
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(gridExtra)
library(grid)
library(forcats)

# Set working directory and create results folder
setwd("Diatoms/data")
if(!dir.exists("../results")) dir.create("../results")

cat("=== LOCALITY SPECIES RICHNESS COMPARISON ===\n")

# Load locality data
locality_data <- read_csv("diatom_specialist_summary_by_locality.csv", show_col_types = FALSE)

cat("Data loaded successfully!\n")
cat("Total localities:", nrow(locality_data), "\n")
cat("Sites per locality range:", min(locality_data$N_sites), "-", max(locality_data$N_sites), "\n")

# Add pattern classification based on site names
locality_data <- locality_data %>%
  mutate(
    # Determine if locality has HM, SP, or mixed sites
    Pattern_Type = case_when(
      grepl("HM", Sites, ignore.case = TRUE) & !grepl("SP", Sites, ignore.case = TRUE) ~ "HM only",
      grepl("SP", Sites, ignore.case = TRUE) & !grepl("HM", Sites, ignore.case = TRUE) ~ "SP only", 
      grepl("HM", Sites, ignore.case = TRUE) & grepl("SP", Sites, ignore.case = TRUE) ~ "Mixed",
      TRUE ~ "Other"
    ),
    # Calculate total species across all categories
    Total_AllSpecies_calc = Total_Spec_0_Generalist + Total_Spec_1_Wetland_tolerant + 
      Total_Spec_2_Wetland_specialist + Total_Spec_3_Fen_specialist
  )

# Check pattern distribution
cat("\nPattern distribution:\n")
print(table(locality_data$Pattern_Type))

# Create color palette for localities
pattern_colors <- c("HM only" = "#E31A1C", "SP only" = "#1F78B4", 
                    "Mixed" = "#FF7F00", "Other" = "#999999")

# A. Species richness by locality (ranked) - no sample size labels
p1 <- locality_data %>%
  arrange(desc(Mean_total_species)) %>%
  mutate(Locality_Code = fct_reorder(Locality_Code, Mean_total_species)) %>%
  ggplot(aes(x = Locality_Code, y = Mean_total_species, fill = Pattern_Type)) +
  geom_col(alpha = 0.8, color = "black", size = 0.2) +
  geom_errorbar(aes(ymin = pmax(0, Mean_total_species - SD_total_species), 
                    ymax = Mean_total_species + SD_total_species),
                width = 0.2, size = 0.3) +
  scale_fill_manual(values = pattern_colors, name = "Site Pattern") +
  labs(
    title = "A) Mean Species Richness by Locality",
    subtitle = "Ranked from highest to lowest; error bars show ±1 SD",
    x = "Locality Code",
    y = "Mean Species per Site"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "gray60"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

# B. Total species counts by locality (absolute numbers) - no sample size labels
p2 <- locality_data %>%
  arrange(desc(Total_AllSpecies_calc)) %>%
  mutate(Locality_Code = fct_reorder(Locality_Code, Total_AllSpecies_calc)) %>%
  ggplot(aes(x = Locality_Code, y = Total_AllSpecies_calc, fill = Pattern_Type)) +
  geom_col(alpha = 0.8, color = "black", size = 0.2) +
  scale_fill_manual(values = pattern_colors, name = "Site Pattern") +
  labs(
    title = "B) Total Species Occurrences by Locality",
    subtitle = "Absolute counts across all sites in locality",
    x = "Locality Code",
    y = "Total Species Occurrences"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "gray60"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )

# C. Specialization composition by top localities - expand to show more localities
# Select top 20 localities by mean species richness for better overview
top_localities <- locality_data %>%
  arrange(desc(Mean_total_species)) %>%
  head(20)

# Prepare data for stacked bar chart
specialization_data <- top_localities %>%
  select(Locality_Code, Pattern_Type, 
         Total_Spec_0_Generalist, Total_Spec_1_Wetland_tolerant,
         Total_Spec_2_Wetland_specialist, Total_Spec_3_Fen_specialist) %>%
  pivot_longer(cols = starts_with("Total_Spec_"), 
               names_to = "Specialization", 
               values_to = "Count") %>%
  mutate(
    Spec_Category = case_when(
      Specialization == "Total_Spec_0_Generalist" ~ "Generalist",
      Specialization == "Total_Spec_1_Wetland_tolerant" ~ "Wetland tolerant",
      Specialization == "Total_Spec_2_Wetland_specialist" ~ "Wetland specialist", 
      Specialization == "Total_Spec_3_Fen_specialist" ~ "Fen specialist"
    ),
    Spec_Category = factor(Spec_Category, levels = c("Generalist", "Wetland tolerant", 
                                                     "Wetland specialist", "Fen specialist"))
  ) %>%
  arrange(desc(Count)) %>%
  mutate(Locality_Code = fct_reorder(Locality_Code, Count, sum))

# Colors for specialization
spec_colors <- c("Generalist" = "#2166AC", "Wetland tolerant" = "#5AAE61", 
                 "Wetland specialist" = "#FDB863", "Fen specialist" = "#D73027")

p3 <- ggplot(specialization_data, aes(x = Locality_Code, y = Count, fill = Spec_Category)) +
  geom_col(position = "stack", alpha = 0.8, color = "white", size = 0.3) +
  scale_fill_manual(values = spec_colors, name = "Specialization") +
  labs(
    title = "C) Top 20 Localities: Specialization Composition",
    subtitle = "Stacked bars show total counts by specialization category",
    x = "Locality Code (Top 20 by species richness)",
    y = "Total Species Occurrences"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "gray60"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    panel.grid.minor = element_blank()
  )

# Combine plots - now 3 panels only
combined_plot <- grid.arrange(
  p1,  # Top: Main ranking chart (full width)
  arrangeGrob(p2, p3, ncol = 2, widths = c(1.2, 1.8)),  # Bottom row: totals + composition
  ncol = 1,
  heights = c(1.2, 1),
  top = textGrob("Diatom Species Richness by Locality: Comprehensive Analysis", 
                 gp = gpar(fontsize = 16, fontface = "bold"))
)

# Save the combined figure
ggsave("../results/diatom_locality_species_richness_comparison.png", combined_plot, 
       width = 16, height = 12, dpi = 300)
cat("✅ Saved: ../results/diatom_locality_species_richness_comparison.png\n")

# Create summary tables
cat("\n=== TOP 10 MOST SPECIES-RICH LOCALITIES ===\n")
top10 <- locality_data %>%
  arrange(desc(Mean_total_species)) %>%
  head(10) %>%
  select(Locality_Code, N_sites, Mean_total_species, SD_total_species, 
         Pattern_Type, Total_AllSpecies_calc)

print(top10)

cat("\n=== BOTTOM 5 LOCALITIES (LOWEST SPECIES RICHNESS) ===\n")
bottom5 <- locality_data %>%
  arrange(Mean_total_species) %>%
  head(5) %>%
  select(Locality_Code, N_sites, Mean_total_species, SD_total_species, 
         Pattern_Type, Total_AllSpecies_calc)

print(bottom5)

# Pattern comparison - simplified without statistical tests
cat("\n=== PATTERN DISTRIBUTION ===\n")
pattern_summary <- locality_data %>%
  group_by(Pattern_Type) %>%
  summarise(
    N_localities = n(),
    Mean_species_richness = round(mean(Mean_total_species), 1),
    SD_species_richness = round(sd(Mean_total_species), 1),
    .groups = 'drop'
  )

print(pattern_summary)

# Save summary tables
write_csv(top10, "../results/top10_species_rich_localities.csv")
write_csv(locality_data %>% arrange(desc(Mean_total_species)), 
          "../results/all_localities_ranked_by_species_richness.csv")

cat("\n📁 Files saved to Diatoms/results/:\n")
cat("• diatom_locality_species_richness_comparison.png (main visualization)\n")
cat("• top10_species_rich_localities.csv (top performers)\n") 
cat("• all_localities_ranked_by_species_richness.csv (complete ranking)\n")

cat("\n📊 Three-panel analysis shows:\n")
cat("• Panel A: All localities ranked by mean species richness (with pattern colors)\n")
cat("• Panel B: All localities ranked by total species occurrences\n")
cat("• Panel C: Specialization composition for top 20 localities\n")

cat("\n🏆 Focus on locality performance and composition patterns\n")
cat("• Colors in Panel A show which localities are HM, SP, Mixed, or Other\n")
cat("• Panel C shows whether high-diversity localities are driven by specialists or generalists\n")

cat("\n🏆 Most species-rich localities identified!\n")

