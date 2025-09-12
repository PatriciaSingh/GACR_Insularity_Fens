# visualize_oribatida_specialists.R
# Create visualizations of specialist categories for Oribatida data
# Produces plots similar to pristine vs degraded site comparisons

# Set working directory
setwd("Oribatida/data")

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

# Read the specialist summary data
counts_data <- read_csv("locality_specialist_summary_counts.csv")
pa_data <- read_csv("locality_specialist_summary_PA.csv")

# Extract site type from locality names ending with "_A" or "_B"
counts_data$Site_Type <- ifelse(grepl("_A$", counts_data$Locality), 
                                "Pristine (A)", "Degraded (B)")

# Verify the classification
cat("Site type classification:\n")
table(counts_data$Site_Type)
cat("\n")

# Prepare data for plotting
plot_data <- counts_data %>%
  pivot_longer(cols = c(Fen_specialists, Fen_tolerant, Generalists), 
               names_to = "Specialist_Category", 
               values_to = "Count") %>%
  mutate(Specialist_Category = case_when(
    Specialist_Category == "Fen_specialists" ~ "Fen Specialists (1)",
    Specialist_Category == "Fen_tolerant" ~ "Fen Tolerant (2)", 
    Specialist_Category == "Generalists" ~ "Generalists (3)"
  ))

# 1. Specialist Categories Comparison (Boxplot)
p1 <- ggplot(plot_data, aes(x = Specialist_Category, y = Count, fill = Site_Type)) +
  geom_boxplot(alpha = 0.7, position = position_dodge(width = 0.8)) +
  geom_point(position = position_jitterdodge(dodge.width = 0.8, jitter.width = 0.2), 
             alpha = 0.6, size = 1.5) +
  scale_fill_manual(values = c("Degraded (B)" = "#E57373", "Pristine (A)" = "#81C784")) +
  labs(title = "Specialist Categories Comparison: All Combined",
       subtitle = "Pristine (A) vs Degraded (B) plots",
       x = "Specialist Category",
       y = "Number of Species",
       fill = "Plot Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 2. Mean Specialist Counts (Bar chart with error bars)
mean_data <- plot_data %>%
  group_by(Specialist_Category, Site_Type) %>%
  summarise(
    mean_count = mean(Count, na.rm = TRUE),
    se_count = sd(Count, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'
  )

p2 <- ggplot(mean_data, aes(x = Specialist_Category, y = mean_count, fill = Site_Type)) +
  geom_col(position = position_dodge(width = 0.8), alpha = 0.7) +
  geom_errorbar(aes(ymin = mean_count - se_count, ymax = mean_count + se_count),
                position = position_dodge(width = 0.8), width = 0.2) +
  scale_fill_manual(values = c("Degraded (B)" = "#E57373", "Pristine (A)" = "#81C784")) +
  labs(title = "Mean Specialist Counts: All Combined",
       subtitle = "Error bars show standard error",
       x = "Specialist Category",
       y = "Mean Number of Species",
       fill = "Plot Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 3. Total Specialists (All categories combined)
total_specialists <- counts_data %>%
  mutate(Total_Specialists = Fen_specialists + Fen_tolerant + Generalists)

p3 <- ggplot(total_specialists, aes(x = Site_Type, y = Total_Specialists, fill = Site_Type)) +
  geom_boxplot(alpha = 0.7) +
  geom_point(position = position_jitter(width = 0.2), alpha = 0.6, size = 2) +
  scale_fill_manual(values = c("Degraded (B)" = "#E57373", "Pristine (A)" = "#81C784")) +
  labs(title = "Total Specialists: All Combined",
       subtitle = "All specialist levels combined (1+2+3)",
       x = "Plot Type",
       y = "Total Number of Specialists") +
  theme_minimal() +
  guides(fill = "none")

# 4. Paired Site Comparison (if you have paired sites)
# Create wide format for comparison
comparison_data <- counts_data %>%
  select(Locality, Site_Type, Total_species, Fen_specialists, Fen_tolerant, Generalists) %>%
  mutate(Total_Specialists = Fen_specialists + Fen_tolerant + Generalists)

# Scatter plot comparing site types
p4 <- ggplot(comparison_data, aes(x = Fen_specialists, y = Total_species)) +
  geom_point(aes(color = Site_Type), size = 3, alpha = 0.7) +
  geom_text(aes(label = substr(Locality, 1, 3)), vjust = -0.5, size = 2.5) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "red") +
  scale_color_manual(values = c("Degraded (B)" = "#E57373", "Pristine (A)" = "#81C784")) +
  labs(title = "Site Comparison: All Combined",
       subtitle = "Each point represents one site (A vs B plot)",
       x = "Fen specialists",
       y = "Total Number of Species",
       color = "Plot Type") +
  theme_minimal()

# Combine all plots
combined_plot <- grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2)

# Save the combined plot
ggsave("../results/oribatida_specialist_analysis.png", combined_plot, 
       width = 16, height = 12, dpi = 300)

# Save individual plots
ggsave("../results/specialist_categories_boxplot.png", p1, width = 10, height = 6, dpi = 300)
ggsave("../results/mean_specialist_counts.png", p2, width = 10, height = 6, dpi = 300)
ggsave("../results/total_specialists_boxplot.png", p3, width = 8, height = 6, dpi = 300)
ggsave("../results/site_comparison_scatter.png", p4, width = 10, height = 6, dpi = 300)

# Print summary statistics
cat("SUMMARY STATISTICS\n")
cat("==================\n\n")

summary_stats <- counts_data %>%
  group_by(Site_Type) %>%
  summarise(
    n_sites = n(),
    mean_fen_specialists = round(mean(Fen_specialists, na.rm = TRUE), 2),
    mean_fen_tolerant = round(mean(Fen_tolerant, na.rm = TRUE), 2),
    mean_generalists = round(mean(Generalists, na.rm = TRUE), 2),
    mean_total_specialists = round(mean(Fen_specialists, na.rm = TRUE), 2),  # Only fen specialists
    .groups = 'drop'
  )

print(summary_stats)

cat("\nPlots saved to Oribatida/results/\n")
cat("- oribatida_specialist_analysis.png (combined)\n")
cat("- specialist_categories_boxplot.png\n")
cat("- mean_specialist_counts.png\n") 
cat("- total_specialists_boxplot.png\n")
cat("- site_comparison_scatter.png\n")

