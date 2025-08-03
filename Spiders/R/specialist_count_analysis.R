# specialist_count_analysis.R
# Calculate number of specialists in each category for each locality

library(dplyr)
library(readr)
library(tidyr)

cat("=== SPIDER SPECIALIST COUNT ANALYSIS ===\n\n")

# Read species specialization data - try multiple possible paths
possible_paths <- c(
  "Spiders/data/Spiders_Species_Specializace_Sireni_Ohrozeni.csv",
  "data/Spiders_Species_Specializace_Sireni_Ohrozeni.csv", 
  "Spiders_Species_Specializace_Sireni_Ohrozeni.csv",
  "./Spiders/data/Spiders_Species_Specializace_Sireni_Ohrozeni.csv"
)

specialization_file <- NULL
for (path in possible_paths) {
  if (file.exists(path)) {
    specialization_file <- path
    break
  }
}

if (is.null(specialization_file)) {
  stop("❌ Specialization file not found")
}

spec_data <- read_csv(specialization_file, show_col_types = FALSE)
cat("✅ Loaded specialization data for", nrow(spec_data), "species\n")

# Remove species with NA specialization
spec_data <- spec_data %>% filter(!is.na(Specialization))
cat("✅ Species with known specialization:", nrow(spec_data), "\n\n")

# Define all files for analysis
files_list <- list(
  # Original files
  "Pasti Cerven" = "Spider_pasti_cerven_finalPA_forcombining_okZeros.csv",
  "Smyk Kveten" = "Spider_smyk_kveten_finalPA_forcombining_okZeros.csv",
  "Smyk Cerven" = "Spider_smyk_cerven_finalPA_forcombining_okZeros.csv",
  "Pasti Kveten" = "Spider_pasti_kveten_finalPA_forcombining_okZeros.csv",
  
  # Combined files
  "All Combined" = "Spider_combined_all_methods_PA_okZeros.csv",
  "Smyk Method" = "Spider_combined_smyk_PA_okZeros.csv",
  "Pasti Method" = "Spider_combined_pasti_PA_okZeros.csv",
  "Kveten Month" = "Spider_combined_kveten_PA_okZeros.csv",
  "Cerven Month" = "Spider_combined_cerven_PA_okZeros.csv"
)

# Find the correct data directory
data_file_names <- unlist(files_list)
data_dir <- NULL
possible_data_dirs <- c("Spiders/data/", "data/", "./", "./Spiders/data/", "")

for (dir in possible_data_dirs) {
  test_file <- if (dir == "") data_file_names[1] else file.path(dir, basename(data_file_names[1]))
  if (file.exists(test_file)) {
    data_dir <- dir
    break
  }
}

if (is.null(data_dir)) {
  stop("❌ Could not find data files")
}

cat("✅ Found data files in directory:", if(data_dir == "") "current directory" else data_dir, "\n\n")

# Update file paths
if (data_dir != "") {
  files_list <- lapply(files_list, function(x) file.path(data_dir, basename(x)))
}

# Function to calculate specialist counts for a dataset
calculate_specialist_counts <- function(file_path, dataset_name, spec_data) {
  cat("Processing:", dataset_name, "\n")
  
  if (!file.exists(file_path)) {
    cat("❌ File not found:", file_path, "\n")
    return(NULL)
  }
  
  # Read data
  data <- read_csv(file_path, show_col_types = FALSE)
  
  # Extract locality names and species matrix
  localities <- data[[1]]
  species_data <- data[, -1]
  
  # Convert to numeric matrix
  species_matrix <- as.matrix(species_data)
  mode(species_matrix) <- "numeric"
  species_matrix[is.na(species_matrix)] <- 0
  rownames(species_matrix) <- localities
  
  # Get species present in this dataset that have specialization data
  species_in_dataset <- colnames(species_matrix)
  species_with_spec <- spec_data$Short_name[spec_data$Short_name %in% species_in_dataset]
  
  cat("  Species with specialization data in this dataset:", length(species_with_spec), "\n")
  
  # Create results dataframe
  results <- data.frame(
    Locality = localities,
    Dataset = dataset_name,
    Generalist_0 = 0,
    Low_Specialist_1 = 0,
    Moderate_Specialist_2 = 0,
    High_Specialist_3 = 0,
    Total_Specialists = 0,
    Total_Species = 0
  )
  
  # Calculate counts for each locality
  for (i in 1:length(localities)) {
    locality <- localities[i]
    
    # Get species present at this locality (value = 1)
    present_species <- species_in_dataset[species_matrix[i, ] == 1]
    
    # Match with specialization data
    present_with_spec <- present_species[present_species %in% species_with_spec]
    
    if (length(present_with_spec) > 0) {
      # Get specialization levels for present species
      spec_levels <- spec_data$Specialization[spec_data$Short_name %in% present_with_spec]
      
      # Count by specialization level
      results$Generalist_0[i] <- sum(spec_levels == 0)
      results$Low_Specialist_1[i] <- sum(spec_levels == 1)
      results$Moderate_Specialist_2[i] <- sum(spec_levels == 2)
      results$High_Specialist_3[i] <- sum(spec_levels == 3)
      results$Total_Specialists[i] <- sum(spec_levels > 0)  # 1, 2, 3
      results$Total_Species[i] <- length(present_with_spec)
    }
  }
  
  cat("  ✅ Calculated specialist counts for", nrow(results), "localities\n")
  return(results)
}

# Process all files
all_results <- list()

cat("PROCESSING ALL FILES...\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

for (dataset_name in names(files_list)) {
  file_path <- files_list[[dataset_name]]
  
  result <- calculate_specialist_counts(file_path, dataset_name, spec_data)
  
  if (!is.null(result)) {
    all_results[[dataset_name]] <- result
  }
  
  cat(paste(rep("-", 40), collapse = ""), "\n")
}

# Combine all results into one big table
cat("\nCOMBINING RESULTS...\n")
combined_results <- do.call(rbind, all_results)

# Create output directories
output_dir <- "Spiders/results"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Save the complete results
output_file <- file.path(output_dir, "Specialist_Counts_by_Locality_and_Dataset.csv")
write_csv(combined_results, output_file)
cat("✅ Saved complete results to:", output_file, "\n")

# Create summary statistics
cat("\nCREATING SUMMARY STATISTICS...\n")

# Summary by dataset
summary_by_dataset <- combined_results %>%
  group_by(Dataset) %>%
  summarise(
    Localities = n(),
    Avg_Generalists = round(mean(Generalist_0), 2),
    Avg_Low_Specialists = round(mean(Low_Specialist_1), 2),
    Avg_Moderate_Specialists = round(mean(Moderate_Specialist_2), 2),
    Avg_High_Specialists = round(mean(High_Specialist_3), 2),
    Avg_Total_Specialists = round(mean(Total_Specialists), 2),
    Avg_Total_Species = round(mean(Total_Species), 2),
    .groups = 'drop'
  )

print(summary_by_dataset)

# Save summary
summary_file <- file.path(output_dir, "Specialist_Counts_Summary_by_Dataset.csv")
write_csv(summary_by_dataset, summary_file)
cat("✅ Saved summary to:", summary_file, "\n")

# Create wide format table (localities as rows, datasets as columns)
cat("\nCREATING WIDE FORMAT TABLES...\n")

# Get all unique localities
all_localities <- unique(combined_results$Locality)
cat("Total unique localities:", length(all_localities), "\n")

# Create wide format for each specialist type
create_wide_table <- function(column_name, description) {
  wide_table <- combined_results %>%
    select(Locality, Dataset, !!sym(column_name)) %>%
    pivot_wider(names_from = Dataset, values_from = !!sym(column_name), values_fill = 0)
  
  wide_file <- file.path(output_dir, paste0("Specialist_Counts_", column_name, "_Wide.csv"))
  write_csv(wide_table, wide_file)
  cat("✅ Saved", description, "wide table to:", wide_file, "\n")
  
  return(wide_table)
}

# Create wide tables for each specialist category
generalists_wide <- create_wide_table("Generalist_0", "Generalists (0)")
low_spec_wide <- create_wide_table("Low_Specialist_1", "Low specialists (1)")
mod_spec_wide <- create_wide_table("Moderate_Specialist_2", "Moderate specialists (2)")
high_spec_wide <- create_wide_table("High_Specialist_3", "High specialists (3)")
total_spec_wide <- create_wide_table("Total_Specialists", "Total specialists")
total_species_wide <- create_wide_table("Total_Species", "Total species")

# Print some example results
cat("\n=== EXAMPLE RESULTS ===\n")
cat("First 5 localities with specialist counts:\n")
example_results <- combined_results %>%
  filter(Dataset == "All Combined") %>%
  select(Locality, Generalist_0, Low_Specialist_1, Moderate_Specialist_2, High_Specialist_3, Total_Species) %>%
  head(5)
print(example_results)

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("📁 All results saved to:", output_dir, "\n")
cat("📊 Files created:\n")
cat("• Complete results: Specialist_Counts_by_Locality_and_Dataset.csv\n")
cat("• Summary by dataset: Specialist_Counts_Summary_by_Dataset.csv\n")
cat("• Wide format tables: 6 CSV files (one for each specialist category)\n")

cat("\n=== INTERPRETATION ===\n")
cat("• Generalist_0: Species found in many habitat types\n")
cat("• Low_Specialist_1: Species with some habitat preferences\n")
cat("• Moderate_Specialist_2: Species preferring certain habitats\n")
cat("• High_Specialist_3: Habitat-specific species\n")
cat("• Total_Specialists: Sum of levels 1, 2, and 3\n")
cat("• Total_Species: All species with known specialization\n")

