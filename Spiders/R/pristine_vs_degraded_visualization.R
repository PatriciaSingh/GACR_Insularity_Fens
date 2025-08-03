# pristine_vs_degraded_visualization.R
# Compare specialist counts between pristine (A) and degraded (B) plots

library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(gridExtra)

cat("=== PRISTINE vs DEGRADED PLOTS SPECIALIST COMPARISON ===\n\n")

# Read the specialist counts data
data_file <- "data/Specialist_counts/Specialist_Counts_by_Locality_and_Dataset.csv"

if (!file.exists(data_file)) {
  # Try alternative paths
  alternative_paths <- c(
    "Specialist_counts/Specialist_Counts_by_Locality_and_Dataset.csv",
    "data/Specialist_counts/Specialist_Counts_by_Locality_and_Dataset.csv",
    "Spiders/data/Specialist_counts/Specialist_Counts_by_Locality_and_Dataset.csv"
  )
  
  for (path in alternative_paths) {
    if (file.exists(path)) {
      data_file <- path
      break
    }
  }
}

if (!file.exists(data_file)) {
  stop("❌ Could not find Specialist_Counts_by_Locality_and_Dataset.csv")
}

cat("✅ Reading specialist counts from:", data_file, "\n")
specialist_data <- read_csv(data_file, show_col_types = FALSE)

# Add plot type (A = pristine, B = degraded)
specialist_data <- specialist_data %>%
  mutate(
    Plot_Type = case_when(
      grepl("_A$", Locality) ~ "Pristine (A)",
      grepl("_B$", Locality) ~ "Degraded (B)",
      TRUE ~ "Other"
    ),
    Site_Name = gsub("_[AB]$", "", Locality)  # Remove _A or _B to get site name
  ) %>%
  filter(Plot_Type != "Other")  # Only include A and B plots

cat("✅ Found", sum(specialist_data$Plot_Type == "Pristine (A)"), "pristine plots (A)\n")
cat("✅ Found", sum(specialist_data$Plot_Type == "Degraded (B)"), "degraded plots (B)\n\n")

# Create output directory
output_dir <- "data/Specialist_counts/visualizations"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  cat("Created output directory:", output_dir, "\n")
}

# Function to create comparison plots
create_comparison_plots <- function(data, dataset_name) {
  
  # Filter data for this dataset
  dataset_data <- data %>% filter(Dataset == dataset_name)
  
  if (nrow(dataset_data) == 0) {
    cat("No data found for dataset:", dataset_name, "\n")
    return(NULL)
  }
  
  cat("Creating plots for:", dataset_name, "\n")
  
  # Prepare data for plotting
  plot_data <- dataset_data %>%
    select(Locality, Plot_Type, Site_Name, 
           Generalist_0, Low_Specialist_1, Moderate_Specialist_2, High_Specialist_3, 
           Total_Specialists, Total_Species) %>%
    pivot_longer(cols = c(Generalist_0, Low_Specialist_1, Moderate_Specialist_2, High_Specialist_3), 
                 names_to = "Specialist_Category", values_to = "Count") %>%
    mutate(
      Specialist_Category = factor(Specialist_Category, 
                                   levels = c("Generalist_0", "Low_Specialist_1", "Moderate_Specialist_2", "High_Specialist_3"),
                                   labels = c("Generalists (0)", "Low Specialists (1)", "Moderate Specialists (2)", "High Specialists (3)"))
    )
  
  # Color palette
  colors <- c("Pristine (A)" = "#2E8B57", "Degraded (B)" = "#DC143C")
  category_colors <- c("Generalists (0)" = "#2E8B57", "Low Specialists (1)" = "#4169E1", 
                       "Moderate Specialists (2)" = "#FF8C00", "High Specialists (3)" = "#DC143C")
  
  # 1. Box plot comparison by specialist category
  p1 <- ggplot(plot_data, aes(x = Specialist_Category, y = Count, fill = Plot_Type)) +
    geom_boxplot(alpha = 0.7, position = position_dodge(0.8)) +
    geom_point(position = position_jitterdodge(dodge.width = 0.8), alpha = 0.6, size = 1.5) +
    scale_fill_manual(values = colors) +
    labs(
      title = paste("Specialist Categories Comparison:", dataset_name),
      subtitle = "Pristine (A) vs Degraded (B) plots",
      x = "Specialist Category",
      y = "Number of Species",
      fill = "Plot Type"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
  
  # 2. Bar plot showing means
  summary_data <- plot_data %>%
    group_by(Plot_Type, Specialist_Category) %>%
    summarise(
      Mean_Count = mean(Count),
      SE = sd(Count) / sqrt(n()),
      .groups = 'drop'
    )
  
  p2 <- ggplot(summary_data, aes(x = Specialist_Category, y = Mean_Count, fill = Plot_Type)) +
    geom_col(position = position_dodge(0.8), alpha = 0.8) +
    geom_errorbar(aes(ymin = Mean_Count - SE, ymax = Mean_Count + SE), 
                  position = position_dodge(0.8), width = 0.3) +
    scale_fill_manual(values = colors) +
    labs(
      title = paste("Mean Specialist Counts:", dataset_name),
      subtitle = "Error bars show standard error",
      x = "Specialist Category", 
      y = "Mean Number of Species",
      fill = "Plot Type"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
  
  # 3. Total specialists comparison
  total_data <- dataset_data %>%
    select(Locality, Plot_Type, Total_Specialists, Total_Species)
  
  p3 <- ggplot(total_data, aes(x = Plot_Type, y = Total_Specialists, fill = Plot_Type)) +
    geom_boxplot(alpha = 0.7) +
    geom_point(position = position_jitter(width = 0.2), alpha = 0.6, size = 2) +
    scale_fill_manual(values = colors) +
    labs(
      title = paste("Total Specialists:", dataset_name),
      subtitle = "All specialist levels combined (1+2+3)",
      x = "Plot Type",
      y = "Total Number of Specialists",
      fill = "Plot Type"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12, face = "bold"),
      legend.position = "none"
    )
  
  # 4. Paired comparison (same sites)
  paired_data <- dataset_data %>%
    select(Site_Name, Plot_Type, Total_Specialists) %>%
    pivot_wider(names_from = Plot_Type, values_from = Total_Specialists, names_prefix = "Plot_") %>%
    filter(!is.na(`Plot_Pristine (A)`) & !is.na(`Plot_Degraded (B)`))
  
  if (nrow(paired_data) > 0) {
    p4 <- ggplot(paired_data, aes(x = `Plot_Degraded (B)`, y = `Plot_Pristine (A)`)) +
      geom_point(size = 3, alpha = 0.7, color = "#4169E1") +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
      geom_text(aes(label = Site_Name), vjust = -0.5, hjust = 0.5, size = 2.5) +
      labs(
        title = paste("Paired Site Comparison:", dataset_name),
        subtitle = "Each point represents one site (A vs B plot)",
        x = "Specialists in Degraded Plot (B)",
        y = "Specialists in Pristine Plot (A)",
        caption = "Points above red line: Pristine > Degraded"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(size = 12, face = "bold"))
  } else {
    p4 <- ggplot() + 
      annotate("text", x = 0.5, y = 0.5, label = "No paired sites available", size = 6) +
      theme_void()
  }
  
  # Combine plots
  combined_plot <- grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)
  
  # Save plots
  plot_filename <- file.path(output_dir, paste0("Pristine_vs_Degraded_", gsub("[^A-Za-z0-9]", "_", dataset_name), ".png"))
  ggsave(plot_filename, combined_plot, width = 14, height = 10, dpi = 300)
  cat("✅ Saved plot:", plot_filename, "\n")
  
  return(list(p1 = p1, p2 = p2, p3 = p3, p4 = p4))
}

# Statistical comparison function
perform_statistical_tests <- function(data, dataset_name) {
  dataset_data <- data %>% filter(Dataset == dataset_name)
  
  if (nrow(dataset_data) == 0) return(NULL)
  
  cat("\n--- Statistical Tests for", dataset_name, "---\n")
  
  # Test each specialist category
  categories <- c("Generalist_0", "Low_Specialist_1", "Moderate_Specialist_2", "High_Specialist_3", "Total_Specialists")
  category_names <- c("Generalists", "Low Specialists", "Moderate Specialists", "High Specialists", "Total Specialists")
  
  results <- data.frame(
    Category = category_names,
    Pristine_Mean = NA,
    Degraded_Mean = NA,
    Difference = NA,
    P_Value = NA,
    Significant = NA
  )
  
  for (i in 1:length(categories)) {
    cat_col <- categories[i]
    
    pristine_vals <- dataset_data %>% filter(Plot_Type == "Pristine (A)") %>% pull(!!sym(cat_col))
    degraded_vals <- dataset_data %>% filter(Plot_Type == "Degraded (B)") %>% pull(!!sym(cat_col))
    
    if (length(pristine_vals) > 0 & length(degraded_vals) > 0) {
      # Wilcoxon test (non-parametric)
      test_result <- wilcox.test(pristine_vals, degraded_vals)
      
      pristine_mean <- mean(pristine_vals)
      degraded_mean <- mean(degraded_vals)
      difference <- pristine_mean - degraded_mean
      
      results$Pristine_Mean[i] <- round(pristine_mean, 2)
      results$Degraded_Mean[i] <- round(degraded_mean, 2)
      results$Difference[i] <- round(difference, 2)
      results$P_Value[i] <- round(test_result$p.value, 4)
      results$Significant[i] <- ifelse(test_result$p.value < 0.05, "Yes", "No")
      
      cat(sprintf("%s: Pristine=%.2f, Degraded=%.2f, Diff=%.2f, p=%.4f %s\n", 
                  category_names[i], pristine_mean, degraded_mean, difference, 
                  test_result$p.value, ifelse(test_result$p.value < 0.05, "*", "")))
    }
  }
  
  return(results)
}

# Analyze each dataset
datasets_to_analyze <- unique(specialist_data$Dataset)
all_plots <- list()
all_stats <- list()

cat("CREATING VISUALIZATIONS...\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

for (dataset in datasets_to_analyze) {
  plots <- create_comparison_plots(specialist_data, dataset)
  stats <- perform_statistical_tests(specialist_data, dataset)
  
  if (!is.null(plots)) all_plots[[dataset]] <- plots
  if (!is.null(stats)) all_stats[[dataset]] <- stats
  
  cat(paste(rep("-", 40), collapse = ""), "\n")
}

# Save statistical results
if (length(all_stats) > 0) {
  stats_combined <- bind_rows(all_stats, .id = "Dataset")
  stats_file <- file.path(output_dir, "Statistical_Tests_Pristine_vs_Degraded.csv")
  write_csv(stats_combined, stats_file)
  cat("\n✅ Saved statistical test results:", stats_file, "\n")
}

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("📁 All visualizations saved to:", output_dir, "\n")
cat("📊 Created comparison plots for", length(all_plots), "datasets\n")
cat("📈 Statistical tests performed for all specialist categories\n")

cat("\n=== INTERPRETATION GUIDE ===\n")
cat("• Look for higher specialist counts in Pristine (A) plots\n")
cat("• Paired site comparison shows individual site effects\n")
cat("• Statistical significance indicates reliable differences\n")
cat("• Points above diagonal line = Pristine > Degraded\n")

