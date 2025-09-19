# Simple HM vs SP Comparison for Diatom Specialization
# Clean visualization showing key differences between site patterns
# Input: Diatoms/data/diatom_specialist_summary_per_site.csv
# Output: Diatoms/results/

# Load libraries
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(gridExtra)
library(grid)

# Set working directory and create results folder
setwd("Diatoms/data")
if(!dir.exists("../results")) dir.create("../results")

cat("=== SIMPLE HM vs SP COMPARISON ===\n")

# Load per-site data
site_data <- read_csv("diatom_specialist_summary_per_site.csv", show_col_types = FALSE)

cat("Data loaded successfully!\n")
cat("Total sites:", nrow(site_data), "\n")
cat("HM sites:", sum(site_data$Pattern == "HM"), "\n")
cat("SP sites:", sum(site_data$Pattern == "SP"), "\n")

# Prepare data for visualization
viz_data <- site_data %>%
  select(Site_Name, Pattern, Total_species, 
         Spec_0_Generalist, Spec_1_Wetland_tolerant, 
         Spec_2_Wetland_specialist, Spec_3_Fen_specialist) %>%
  # Convert to long format for easier plotting
  pivot_longer(cols = starts_with("Spec_"), 
               names_to = "Specialization", 
               values_to = "Count") %>%
  mutate(
    Spec_Category = case_when(
      Specialization == "Spec_0_Generalist" ~ "Generalist",
      Specialization == "Spec_1_Wetland_tolerant" ~ "Wetland tolerant", 
      Specialization == "Spec_2_Wetland_specialist" ~ "Wetland specialist",
      Specialization == "Spec_3_Fen_specialist" ~ "Fen specialist"
    ),
    Spec_Category = factor(Spec_Category, levels = c("Generalist", "Wetland tolerant", 
                                                     "Wetland specialist", "Fen specialist"))
  )

# Create color palette
colors <- c("Generalist" = "#2166AC",
            "Wetland tolerant" = "#5AAE61", 
            "Wetland specialist" = "#FDB863",
            "Fen specialist" = "#D73027")

# A. Box plots comparing each specialization category
p1 <- ggplot(viz_data, aes(x = Spec_Category, y = Count, fill = Pattern)) +
  geom_boxplot(alpha = 0.7, outlier.size = 1) +
  scale_fill_manual(values = c("HM" = "#E31A1C", "SP" = "#1F78B4"), 
                    name = "Site Pattern") +
  labs(
    title = "A) Species Counts per Site by Specialization Category",
    subtitle = "Boxes show median, 25th-75th percentiles; dots are outliers",
    x = "Specialization Category",
    y = "Number of Species per Site"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "gray60"),
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

# B. Total species comparison
total_species_data <- site_data %>%
  select(Site_Name, Pattern, Total_species)

p2 <- ggplot(total_species_data, aes(x = Pattern, y = Total_species, fill = Pattern)) +
  geom_boxplot(alpha = 0.7, outlier.size = 1) +
  geom_jitter(width = 0.2, alpha = 0.5, size = 1) +
  scale_fill_manual(values = c("HM" = "#E31A1C", "SP" = "#1F78B4")) +
  labs(
    title = "B) Total Species Richness per Site",
    subtitle = "Each dot represents one site",
    x = "Site Pattern",
    y = "Total Number of Species per Site"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "gray60"),
    legend.position = "none",
    panel.grid.minor = element_blank()
  ) +
  # Add sample size labels
  annotate("text", x = 1, y = max(total_species_data$Total_species) * 0.95, 
           label = paste("n =", sum(site_data$Pattern == "HM")), size = 3.5) +
  annotate("text", x = 2, y = max(total_species_data$Total_species) * 0.95, 
           label = paste("n =", sum(site_data$Pattern == "SP")), size = 3.5)

# C. Mean comparison with error bars
summary_stats <- viz_data %>%
  group_by(Pattern, Spec_Category) %>%
  summarise(
    Mean = mean(Count),
    SD = sd(Count),
    SE = sd(Count) / sqrt(n()),
    .groups = 'drop'
  )

p3 <- ggplot(summary_stats, aes(x = Spec_Category, y = Mean, fill = Pattern)) +
  geom_col(position = position_dodge(width = 0.8), alpha = 0.8, color = "black", size = 0.3) +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE),
                position = position_dodge(width = 0.8), width = 0.2, size = 0.4) +
  scale_fill_manual(values = c("HM" = "#E31A1C", "SP" = "#1F78B4"), 
                    name = "Site Pattern") +
  labs(
    title = "C) Mean Species per Site (±SE)",
    subtitle = "Standard error bars show precision of means",
    x = "Specialization Category",
    y = "Mean Number of Species per Site"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "gray60"),
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  ) +
  geom_text(aes(label = round(Mean, 1)), 
            position = position_dodge(width = 0.8), vjust = -0.5, size = 2.5)

# D. Stacked bar showing proportions within each pattern
prop_data <- viz_data %>%
  group_by(Pattern, Spec_Category) %>%
  summarise(Total = sum(Count), .groups = 'drop') %>%
  group_by(Pattern) %>%
  mutate(Proportion = Total / sum(Total) * 100)

p4 <- ggplot(prop_data, aes(x = Pattern, y = Proportion, fill = Spec_Category)) +
  geom_col(position = "stack", alpha = 0.8, color = "white", size = 0.5) +
  scale_fill_manual(values = colors, name = "Specialization") +
  labs(
    title = "D) Relative Composition (%)",
    subtitle = "Proportion of each specialization category",
    x = "Site Pattern",
    y = "Percentage (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "gray60"),
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    legend.text = element_text(size = 8),
    panel.grid.minor = element_blank()
  ) +
  geom_text(aes(label = paste0(round(Proportion, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "white", fontface = "bold", size = 2.5) +
  # Add sample size labels
  annotate("text", x = 1, y = -8, 
           label = paste("n =", sum(site_data$Pattern == "HM"), "sites"), size = 3) +
  annotate("text", x = 2, y = -8, 
           label = paste("n =", sum(site_data$Pattern == "SP"), "sites"), size = 3) +
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(10, 10, 25, 10))

# Combine all plots into one comprehensive figure
combined_plot <- grid.arrange(
  arrangeGrob(p1, p2, ncol = 2),  # Top row
  arrangeGrob(p3, p4, ncol = 2),  # Bottom row
  ncol = 1,
  heights = c(1, 1),
  top = textGrob("Diatom Specialization Patterns: HM vs SP Site Comparison", 
                 gp = gpar(fontsize = 16, fontface = "bold"))
)

# Save the combined figure
ggsave("../results/diatom_HM_vs_SP_simple_comparison.png", combined_plot, 
       width = 14, height = 10, dpi = 300)
cat("✅ Saved: ../results/diatom_HM_vs_SP_simple_comparison.png\n")

# Print summary statistics for reference
cat("\n=== SUMMARY STATISTICS ===\n")

# Overall comparison
hm_data <- site_data[site_data$Pattern == "HM", ]
sp_data <- site_data[site_data$Pattern == "SP", ]

cat("HM sites (n =", nrow(hm_data), "):\n")
cat("  Total species - Mean:", round(mean(hm_data$Total_species), 1), 
    "± SD:", round(sd(hm_data$Total_species), 1), "\n")

cat("SP sites (n =", nrow(sp_data), "):\n")
cat("  Total species - Mean:", round(mean(sp_data$Total_species), 1), 
    "± SD:", round(sd(sp_data$Total_species), 1), "\n")

# Specialization category comparison
cat("\nSpecialization category means (per site):\n")
for(spec in c("Spec_0_Generalist", "Spec_1_Wetland_tolerant", 
              "Spec_2_Wetland_specialist", "Spec_3_Fen_specialist")) {
  hm_mean <- round(mean(hm_data[[spec]]), 1)
  sp_mean <- round(mean(sp_data[[spec]]), 1)
  spec_name <- gsub("Spec_[0-9]_", "", spec)
  cat(sprintf("  %-20s: HM = %4.1f, SP = %4.1f\n", spec_name, hm_mean, sp_mean))
}

cat("\n📁 Simple comparison visualization saved to: Diatoms/results/\n")
cat("File created: diatom_HM_vs_SP_simple_comparison.png\n")
cat("\n📊 Four-panel figure shows:\n")
cat("• Panel A: Box plots for each specialization category\n")
cat("• Panel B: Total species richness comparison\n")
cat("• Panel C: Mean comparison with standard errors\n")
cat("• Panel D: Relative composition percentages\n")
cat("\n💡 Clear, simple visualization ready for collaborators!\n")

