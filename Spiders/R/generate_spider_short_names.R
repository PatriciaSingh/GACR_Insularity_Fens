# Generate short names from Latin spider names
# Script: generate_spider_short_names.R
# Purpose: Create 6-letter codes from spider species names (3 letters genus + 3 letters species)
# Load required libraries
library(readr)
library(dplyr)
library(stringr)
# Set working directory to the data folder (where this script is located)
setwd("Spiders/data")
# Read the CSV file
spider_data <- read_csv("Spides_Species_Specializace_Sireni_Ohrozeni.csv")
# Function to create short names
create_short_name <- function(latin_name) {
  # Split the Latin name into genus and species
  parts <- str_split(latin_name, " ")[[1]]
  
  if (length(parts) >= 2) {
    genus <- parts[1]
    species <- parts[2]
    
    # Take first 3 letters of genus and species, convert to title case
    genus_short <- str_to_title(str_sub(genus, 1, 3))
    species_short <- str_to_lower(str_sub(species, 1, 3))
    
    # Combine them and make 4th letter uppercase
    short_name <- paste0(genus_short, species_short)
    # Make the 4th character uppercase
    if (nchar(short_name) >= 4) {
      substr(short_name, 4, 4) <- toupper(substr(short_name, 4, 4))
    }
    
    return(short_name)
  } else {
    return(NA)  # Return NA if the name doesn't have both genus and species
  }
}
# Apply the function to create short names
# The Latin names are in the "Species" column
spider_data$short_name <- sapply(spider_data$Species, create_short_name)
# Check for duplicates
duplicates <- spider_data %>%
  group_by(short_name) %>%
  summarise(count = n(), .groups = 'drop') %>%
  filter(count > 1)
# Display results
cat("Short names generated!\n")
cat("Number of species processed:", nrow(spider_data), "\n")
cat("Number of duplicates found:", nrow(duplicates), "\n\n")
if (nrow(duplicates) > 0) {
  cat("Duplicate short names:\n")
  print(duplicates)
  
  cat("\nSpecies with duplicate short names:\n")
  duplicate_species <- spider_data %>%
    filter(short_name %in% duplicates$short_name) %>%
    arrange(short_name)
  print(duplicate_species[, c("Species", "short_name")])
}
# Save the updated data back to the original file
write_csv(spider_data, "Spides_Species_Specializace_Sireni_Ohrozeni.csv")
cat("\nShort names column added to original file: Spides_Species_Specializace_Sireni_Ohrozeni.csv\n")
# Display first few examples
cat("\nFirst 10 examples:\n")
examples <- spider_data[1:min(10, nrow(spider_data)), c("Species", "short_name")]
print(examples)
