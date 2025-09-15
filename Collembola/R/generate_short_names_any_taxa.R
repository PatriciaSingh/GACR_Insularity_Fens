# Generate short names from Latin species names - Universal Script
# Script: generate_short_names_any_taxa.R
# Usage: 
#   - Set TAXA_NAME variable below to your target group
#   - Or run from command line: Rscript generate_short_names_any_taxa.R Collembola

# Load required libraries
library(readr)
library(dplyr)
library(stringr)

# =============================================================================
# CONFIGURATION - Change this for different taxa
# =============================================================================

# Get taxa name from command line argument or set manually
args <- commandArgs(trailingOnly = TRUE)

if (length(args) > 0) {
  TAXA_NAME <- args[1]
  cat("Using taxa from command line argument:", TAXA_NAME, "\n")
} else {
  # CHANGE THIS LINE for different taxa:
  TAXA_NAME <- "Collembola"  # Change to "Oribatida", "Collembola", "Araneae", etc.
  cat("Using taxa from script configuration:", TAXA_NAME, "\n")
}

# Optional: customize these if your file structure differs
SPECIES_COLUMN <- "Species"  # Name of the column containing Latin names
INPUT_FILENAME <- paste0(TAXA_NAME, "_Species_Specializace.csv")  # Auto-generated filename

# =============================================================================
# MAIN PROCESSING
# =============================================================================

# Set working directory to the taxa data folder
taxa_path <- file.path(TAXA_NAME, "data")
cat("Looking for data in:", taxa_path, "\n")

if (!dir.exists(taxa_path)) {
  stop(paste("ERROR: Directory", taxa_path, "does not exist!"))
}

setwd(taxa_path)

# Check if input file exists
if (!file.exists(INPUT_FILENAME)) {
  stop(paste("ERROR: File", INPUT_FILENAME, "does not exist!"))
}

# Read the CSV file
cat("Reading file:", INPUT_FILENAME, "\n")
taxa_data <- read_csv(INPUT_FILENAME, show_col_types = FALSE)

# Verify species column exists
if (!SPECIES_COLUMN %in% colnames(taxa_data)) {
  stop(paste("ERROR: Column", SPECIES_COLUMN, "not found! Available columns:", 
             paste(colnames(taxa_data), collapse = ", ")))
}

# Function to create short names
create_short_name <- function(latin_name) {
  if (is.na(latin_name) || latin_name == "") {
    return(NA)
  }
  
  # Split the Latin name into genus and species
  parts <- str_split(latin_name, " ")[[1]]
  
  if (length(parts) >= 2) {
    genus <- parts[1]
    species <- parts[2]
    
    # Take first 3 letters of genus and species
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
cat("Generating short names...\n")
taxa_data$short_name <- sapply(taxa_data[[SPECIES_COLUMN]], create_short_name)

# Check for duplicates
duplicates <- taxa_data %>%
  filter(!is.na(short_name)) %>%
  group_by(short_name) %>%
  summarise(count = n(), .groups = 'drop') %>%
  filter(count > 1)

# Display results
cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("SHORT NAMES GENERATION RESULTS FOR", toupper(TAXA_NAME), "\n")
cat(paste(rep("=", 60), collapse = ""), "\n")
cat("Total species processed:", nrow(taxa_data), "\n")
cat("Valid short names created:", sum(!is.na(taxa_data$short_name)), "\n")
cat("Invalid/missing names:", sum(is.na(taxa_data$short_name)), "\n")
cat("Duplicate short names found:", nrow(duplicates), "\n\n")

# Handle duplicates if found
if (nrow(duplicates) > 0) {
  cat("⚠️  DUPLICATE SHORT NAMES DETECTED:\n")
  print(duplicates)
  
  cat("\n📋 Species with duplicate short names:\n")
  duplicate_species <- taxa_data %>%
    filter(short_name %in% duplicates$short_name) %>%
    arrange(short_name) %>%
    select(all_of(c(SPECIES_COLUMN, "short_name")))
  print(duplicate_species)
  
  cat("\n💡 Consider adding additional characters or manual correction for duplicates.\n\n")
}

# Create output filename
output_filename <- paste0(TAXA_NAME, "_Species_Short_Names.csv")

# Save the updated data
write_csv(taxa_data, output_filename)
cat("✅ Results saved to:", output_filename, "\n")

# Display examples
cat("\n📝 First 10 examples:\n")
examples <- taxa_data[1:min(10, nrow(taxa_data)), c(SPECIES_COLUMN, "short_name")]
print(examples)

# Summary statistics
cat("\n📊 SUMMARY:\n")
cat("- Input file:", INPUT_FILENAME, "\n")
cat("- Output file:", output_filename, "\n")
cat("- Taxa processed:", TAXA_NAME, "\n")
cat("- Species column used:", SPECIES_COLUMN, "\n")
cat("- Success rate:", round(sum(!is.na(taxa_data$short_name))/nrow(taxa_data)*100, 1), "%\n")

if (nrow(duplicates) > 0) {
  cat("- ⚠️  Manual review needed for", nrow(duplicates), "duplicate short names\n")
} else {
  cat("- ✅ No duplicates found - ready to use!\n")
}

# Return to original directory
setwd("../..")

cat("\n🎉 Processing complete!\n")