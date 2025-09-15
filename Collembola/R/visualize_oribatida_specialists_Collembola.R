# visualize_Collembola_Species.R
# Create visualizations of Species categories for Collembola data
# Produces plots similar to pristine vs degraded site comparisons

# Set working directory
setwd("Collembola/data")

# Load required libraries
library(readr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(tidyr)

# Create results directory if it doesn't exist
if(!dir.exists("../results")) {
  dir.create("../results")
}

# Read the Species summary data
counts_data <- read_csv("locality_species_summary_counts.csv")
pa_data <- read_csv("locality_species_summary_PA.csv")

# Extract site type from locality names ending with "_A" or "_B"
counts_data$Site_Type <- ifelse(grepl("_A$", counts_data$Locality), 
                                "Pristine (A)", "Degraded (B)")

# Verify the classification
cat("Site type classification:\n")
table(counts_data$Site_Type)
cat("\n")

# Prepare data for plotting
plot_data <- counts_data %>%
  pivot_longer(cols = c(Tirfobionti, Neustonicke_hygrofilni, Ostatni), 
               names_to = "Species_Category", 
               values_to = "Count") %>%
  mutate(Species_Category = case_when(
    Species_Category == "Tirfobionti" ~ "Tirfobionti (1)",
    Species_Category == "Neustonicke_hygrofilni" ~ "Neustonicke_hygrofilni (2)", 
    Species_Category == "Ostatni" ~ "Ostatni (3)"
  ))

# 1. Species Categories Comparison (Boxplot)
p1 <- ggplot(plot_data, aes(x = Species_Category, y = Count, fill = Site_Type)) +
  geom_boxplot(alpha = 0.7, position = position_dodge(width = 0.8)) +
  geom_point(position = position_jitterdodge(dodge.width = 0.8, jitter.width = 0.2), 
             alpha = 0.6, size = 1.5) +
  scale_fill_manual(values = c("Degraded (B)" = "#E57373", "Pristine (A)" = "#81C784")) +
  labs(title = "Species Categories Comparison: All Combined",
       subtitle = "Pristine (A) vs Degraded (B) plots",
       x = "Species Category",
       y = "Number of Species",
       fill = "Plot Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 2. Mean Species Counts (Bar chart with error bars)
mean_data <- plot_data %>%
  group_by(Species_Category, Site_Type) %>%
  summarise(
    mean_count = mean(Count, na.rm = TRUE),
    se_count = sd(Count, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'
  )

p2 <- ggplot(mean_data, aes(x = Species_Category, y = mean_count, fill = Site_Type)) +
  geom_col(position = position_dodge(width = 0.8), alpha = 0.7) +
  geom_errorbar(aes(ymin = mean_count - se_count, ymax = mean_count + se_count),
                position = position_dodge(width = 0.8), width = 0.2) +
  scale_fill_manual(values = c("Degraded (B)" = "#E57373", "Pristine (A)" = "#81C784")) +
  labs(title = "Mean Species Counts: All Combined",
       subtitle = "Error bars show standard error",
       x = "Species Category",
       y = "Mean Number of Species",
       fill = "Plot Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 3. Total Species (All categories combined)
total_Species <- counts_data %>%
  mutate(Total_Species = Tirfobionti + Neustonicke_hygrofilni + Ostatni)

p3 <- ggplot(total_Species, aes(x = Site_Type, y = Total_Species, fill = Site_Type)) +
  geom_boxplot(alpha = 0.7) +
  geom_point(position = position_jitter(width = 0.2), alpha = 0.6, size = 2) +
  scale_fill_manual(values = c("Degraded (B)" = "#E57373", "Pristine (A)" = "#81C784")) +
  labs(title = "Total Species: All Combined",
       subtitle = "All Species levels combined (1+2+3)",
       x = "Plot Type",
       y = "Total Number of Species") +
  theme_minimal() +
  guides(fill = "none")

# 4. Paired Site Comparison (if you have paired sites)
# Create wide format for comparison
comparison_data <- counts_data %>%
  select(Locality, Site_Type, Total_species, Tirfobionti, Neustonicke_hygrofilni, Ostatni) %>%
  mutate(Total_Species = Tirfobionti + Neustonicke_hygrofilni + Ostatni)

# Scatter plot comparing site types
p4 <- ggplot(comparison_data, aes(x = Tirfobionti, y = Total_species)) +
  geom_point(aes(color = Site_Type), size = 3, alpha = 0.7) +
  geom_text(aes(label = substr(Locality, 1, 3)), vjust = -0.5, size = 2.5) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "red") +
  scale_color_manual(values = c("Degraded (B)" = "#E57373", "Pristine (A)" = "#81C784")) +
  labs(title = "Site Comparison: All Combined",
       subtitle = "Each point represents one site (A vs B plot)",
       x = "Tirfobionti",
       y = "Total Number of Species",
       color = "Plot Type") +
  theme_minimal()

# Combine all plots
combined_plot <- grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2)

# Save the combined plot
ggsave("../results/Collembola_Species_analysis.png", combined_plot, 
       width = 16, height = 12, dpi = 300)

# Save individual plots
ggsave("../results/Species_categories_boxplot.png", p1, width = 10, height = 6, dpi = 300)
ggsave("../results/mean_Species_counts.png", p2, width = 10, height = 6, dpi = 300)
ggsave("../results/total_Species_boxplot.png", p3, width = 8, height = 6, dpi = 300)
ggsave("../results/site_comparison_scatter.png", p4, width = 10, height = 6, dpi = 300)

# Print summary statistics
cat("SUMMARY STATISTICS\n")
cat("==================\n\n")

summary_stats <- counts_data %>%
  group_by(Site_Type) %>%
  summarise(
    n_sites = n(),
    mean_Tirfobionti = round(mean(Tirfobionti, na.rm = TRUE), 2),
    mean_Neustonicke_hygrofilni = round(mean(Neustonicke_hygrofilni, na.rm = TRUE), 2),
    mean_Ostatni = round(mean(Ostatni, na.rm = TRUE), 2),
    mean_total_Species = round(mean(Tirfobionti, na.rm = TRUE), 2),  # Only Tirfobionti
    .groups = 'drop'
  )

print(summary_stats)

cat("\nPlots saved to Collembola/results/\n")
cat("- Collembola_Species_analysis.png (combined)\n")
cat("- Species_categories_boxplot.png\n")
cat("- mean_Species_counts.png\n") 
cat("- total_Species_boxplot.png\n")
cat("- site_comparison_scatter.png\n")

