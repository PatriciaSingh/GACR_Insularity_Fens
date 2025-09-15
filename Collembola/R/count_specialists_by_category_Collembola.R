# count_specialists_by_category.R
# Count specialist species by category (1=Tirfobionti, 2=Neustonicke_hygrofilni, 3=Ostatni)
# Creates summary statistics for preliminary analysis

# Set working directory
setwd("Collembola/data")

# Load required libraries
library(readr)
library(dplyr)

# Read the files with specialization data
file1_spec <- read_csv("Collembola_final_with_specialization.csv")
file2_spec <- read_csv("Collembola_finalPA_with_specialization.csv")

# Create summary for each locality
create_locality_summary <- function(data_file) {
  # Remove specialization row for analysis
  locality_data <- data_file[-1, ]
  
  # Get specialization values
  spec_values <- as.numeric(data_file[1, -1])
  
  # Create summary for each locality
  locality_summary <- data.frame(
    Locality = locality_data$Locality,
    Total_species = rowSums(locality_data[, -1] > 0, na.rm = TRUE),
    Tirfobionti = NA,
    Neustonicke_hygrofilni = NA,
    Ostatni = NA
  )
  
  # Calculate counts for each category per locality
  for(i in 1:nrow(locality_data)) {
    row_data <- as.numeric(locality_data[i, -1])
    
    # Count species present (>0) by specialization category
    locality_summary$Tirfobionti[i] <- sum((row_data > 0) & (spec_values == 1), na.rm = TRUE)
    locality_summary$Neustonicke_hygrofilni[i] <- sum((row_data > 0) & (spec_values == 2), na.rm = TRUE) 
    locality_summary$Ostatni[i] <- sum((row_data > 0) & (spec_values == 3), na.rm = TRUE)
  }
  
  return(locality_summary)
}

# Create locality summaries
locality_summary_file1 <- create_locality_summary(file1_spec)
locality_summary_file2 <- create_locality_summary(file2_spec)

# Save summaries
write_csv(locality_summary_file1, "locality_specialist_summary_counts.csv")
write_csv(locality_summary_file2, "locality_specialist_summary_PA.csv")

cat("Files created:\n")
cat("- locality_specialist_summary_counts.csv (count data by locality)\n") 
cat("- locality_specialist_summary_PA.csv (presence/absence data by locality)\n")

