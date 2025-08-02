# Spider Data Combining Script
# This script combines presence-absence data from multiple CSV files

library(dplyr)
library(readr)

# Set working directory to the data folder
setwd("Spiders/data")

# Define file paths
files <- c(
  "Spider_pasti_cerven_finalPA_forcombining.csv",
  "Spider_smyk_kveten_finalPA_forcombining.csv", 
  "Spider_smyk_cerven_finalPA_forcombining.csv",
  "Spider_pasti_kveten_finalPA_forcombining.csv"
)

# Function to read and process each file
read_and_process <- function(file_path) {
  # Read the CSV file
  data <- read_csv(file_path, show_col_types = FALSE)
  
  # Convert first column to character (locality names)
  data[[1]] <- as.character(data[[1]])
  
  # Convert all other columns to numeric (0/1 presence-absence)
  data[, -1] <- lapply(data[, -1], function(x) as.numeric(as.character(x)))
  
  return(data)
}

# Read all files
cat("Reading files...\n")
data_list <- list()
for (i in seq_along(files)) {
  if (file.exists(files[i])) {
    cat("Reading:", files[i], "\n")
    data_list[[i]] <- read_and_process(files[i])
    names(data_list)[i] <- gsub("\\.csv$", "", files[i])
  } else {
    cat("Warning: File not found:", files[i], "\n")
  }
}

# Remove NULL entries if any files were missing
data_list <- data_list[!sapply(data_list, is.null)]

if (length(data_list) == 0) {
  stop("No files were successfully read!")
}

# Get all unique localities and species
all_localities <- unique(unlist(lapply(data_list, function(x) x[[1]])))
all_species <- unique(unlist(lapply(data_list, function(x) names(x)[-1])))

cat("Found", length(all_localities), "unique localities\n")
cat("Found", length(all_species), "unique species\n")

# Function to combine data with proper presence-absence logic
combine_data <- function(data_list, localities, species) {
  # Create empty matrix
  combined_matrix <- matrix(0, nrow = length(localities), ncol = length(species))
  rownames(combined_matrix) <- localities
  colnames(combined_matrix) <- species
  
  # Fill matrix with data from each file
  for (data in data_list) {
    locality_col <- data[[1]]
    
    for (species_name in names(data)[-1]) {
      if (species_name %in% species) {
        for (i in seq_along(locality_col)) {
          locality <- locality_col[i]
          if (!is.na(locality) && locality %in% localities) {
            value <- data[[species_name]][i]
            if (!is.na(value) && value > 0) {
              combined_matrix[locality, species_name] <- 1
            }
          }
        }
      }
    }
  }
  
  # Convert to data frame
  result <- data.frame(Locality = localities, combined_matrix, check.names = FALSE)
  return(result)
}

# Combine all data
cat("Combining all data...\n")
combined_all <- combine_data(data_list, all_localities, all_species)

# Create method-specific combinations
# Smyk method (combining cerven and kveten)
smyk_files <- data_list[grepl("smyk", names(data_list))]
if (length(smyk_files) > 0) {
  cat("Combining smyk method data...\n")
  combined_smyk <- combine_data(smyk_files, all_localities, all_species)
}

# Pasti method (combining cerven and kveten)
pasti_files <- data_list[grepl("pasti", names(data_list))]
if (length(pasti_files) > 0) {
  cat("Combining pasti method data...\n")
  combined_pasti <- combine_data(pasti_files, all_localities, all_species)
}

# Create month-specific combinations
# Kveten month (combining smyk and pasti)
kveten_files <- data_list[grepl("kveten", names(data_list))]
if (length(kveten_files) > 0) {
  cat("Combining kveten month data...\n")
  combined_kveten <- combine_data(kveten_files, all_localities, all_species)
}

# Cerven month (combining smyk and pasti)
cerven_files <- data_list[grepl("cerven", names(data_list))]
if (length(cerven_files) > 0) {
  cat("Combining cerven month data...\n")
  combined_cerven <- combine_data(cerven_files, all_localities, all_species)
}

# Save results
cat("Saving results...\n")

# Save combined data
write_csv(combined_all, "Spider_combined_all_methods_PA.csv")
cat("Saved: Spider_combined_all_methods_PA.csv\n")

if (exists("combined_smyk")) {
  write_csv(combined_smyk, "Spider_combined_smyk_PA.csv")
  cat("Saved: Spider_combined_smyk_PA.csv\n")
}

if (exists("combined_pasti")) {
  write_csv(combined_pasti, "Spider_combined_pasti_PA.csv")
  cat("Saved: Spider_combined_pasti_PA.csv\n")
}

if (exists("combined_kveten")) {
  write_csv(combined_kveten, "Spider_combined_kveten_PA.csv")
  cat("Saved: Spider_combined_kveten_PA.csv\n")
}

if (exists("combined_cerven")) {
  write_csv(combined_cerven, "Spider_combined_cerven_PA.csv")
  cat("Saved: Spider_combined_cerven_PA.csv\n")
}

# Print summary statistics
cat("\n=== SUMMARY ===\n")
cat("Total localities:", nrow(combined_all), "\n")
cat("Total species:", ncol(combined_all) - 1, "\n")
cat("Total presences in combined data:", sum(combined_all[, -1]), "\n")

# Calculate species occurrence frequencies
species_freq <- colSums(combined_all[, -1])
cat("Most common species:\n")
print(head(sort(species_freq, decreasing = TRUE), 10))

cat("\nScript completed successfully!\n")

