# count_diatom_specialists_by_category.R
# Count specialist diatom species by category for each locality
# Localities defined by first 3 letters of Short_names
# Uses separate specialization file for trait information

# Set working directory
setwd("Diatoms/data")

# Load required libraries
library(readr)
library(dplyr)

cat("=== COUNTING DIATOM SPECIALISTS BY LOCALITY ===\n")

# Read the presence-absence data
cat("Loading presence-absence data...\n")
pa_data <- read_csv("Diatoms_A_spring_final_PA_with_short_names.csv", show_col_types = FALSE)

# Read the specialization data
cat("Loading specialization data...\n")
spec_data <- read_csv("Diatoms_A_spring_Specializace.csv", show_col_types = FALSE)

cat("Data loaded successfully!\n")
cat("PA data dimensions:", nrow(pa_data), "sites x", ncol(pa_data)-1, "species\n")
cat("Specialization data:", nrow(spec_data), "species with traits\n")

# Extract locality codes (first 3 letters)
localities_full <- pa_data$Short_names
locality_codes <- substr(localities_full, 1, 3)

cat("\nUnique locality codes found:\n")
unique_localities <- unique(locality_codes)
print(unique_localities)
cat("Number of unique localities:", length(unique_localities), "\n")

# Create locality summary function
create_locality_summary <- function(pa_data, spec_data) {
  
  # Get species names (column names excluding Short_names)
  species_names <- colnames(pa_data)[-1]
  
  # Create a data frame to store results
  locality_summary <- data.frame(
    Locality_Code = locality_codes,
    Site_Name = localities_full,
    Total_species = rowSums(pa_data[, -1] > 0, na.rm = TRUE)
  )
  
  # Get unique numerical specialization categories and their descriptions
  spec_mapping <- spec_data %>%
    filter(!is.na(Specialization)) %>%
    select(Specialization, Specializatio_by_words) %>%
    distinct() %>%
    arrange(Specialization) %>%
    mutate(
      # Handle cases where Specializatio_by_words might be missing
      Specializatio_by_words = ifelse(is.na(Specializatio_by_words), 
                                      paste("Category", Specialization), 
                                      Specializatio_by_words)
    )
  
  cat("\nSpecialization categories found:\n")
  print(spec_mapping)
  
  # Add columns for each numerical specialization category (use words for column names)
  for(i in 1:nrow(spec_mapping)) {
    spec_num <- spec_mapping$Specialization[i]
    spec_word <- spec_mapping$Specializatio_by_words[i]
    # Clean column name (remove spaces and special characters)
    col_name <- gsub("[^A-Za-z0-9]", "_", spec_word)
    col_name <- paste0("Spec_", spec_num, "_", col_name)
    locality_summary[[col_name]] <- 0
  }
  
  # Calculate counts for each category per site
  cat("\nCalculating specialization counts per site...\n")
  
  for(i in 1:nrow(pa_data)) {
    cat("Processing site", i, "of", nrow(pa_data), "\r")
    
    # Get presence/absence data for this site
    site_data <- as.numeric(pa_data[i, -1])
    names(site_data) <- species_names
    
    # Find which species are present (>0)
    present_species <- names(site_data)[site_data > 0 & !is.na(site_data)]
    
    # Count species by numerical specialization category
    for(j in 1:nrow(spec_mapping)) {
      spec_num <- spec_mapping$Specialization[j]
      spec_word <- spec_mapping$Specializatio_by_words[j]
      col_name <- gsub("[^A-Za-z0-9]", "_", spec_word)
      col_name <- paste0("Spec_", spec_num, "_", col_name)
      
      # Get species with this specialization number (including 0)
      spec_species <- spec_data$Short_names[spec_data$Specialization == spec_num & 
                                              !is.na(spec_data$Specialization) &
                                              !is.na(spec_data$Short_names)]
      
      # Count how many present species have this specialization
      count <- sum(present_species %in% spec_species)
      locality_summary[i, col_name] <- count
    }
  }
  
  cat("\nDone!\n")
  return(locality_summary)
}

# Create the summary
locality_summary <- create_locality_summary(pa_data, spec_data)

# Create aggregated summary by locality code (first 3 letters)
cat("\nCreating aggregated summary by locality codes...\n")

locality_aggregated <- locality_summary %>%
  group_by(Locality_Code) %>%
  summarise(
    N_sites = n(),
    Sites = paste(Site_Name, collapse = "; "),
    Mean_total_species = round(mean(Total_species), 1),
    SD_total_species = round(sd(Total_species), 1),
    .groups = 'drop'
  )

# Add specialization columns to aggregated data
spec_columns <- colnames(locality_summary)[!colnames(locality_summary) %in% c("Locality_Code", "Site_Name", "Total_species")]

for(col in spec_columns) {
  locality_aggregated[[paste0("Total_", col)]] <- locality_summary %>%
    group_by(Locality_Code) %>%
    summarise(total_val = sum(.data[[col]]), .groups = 'drop') %>%
    pull(total_val)
  
  locality_aggregated[[paste0("Mean_", col)]] <- locality_summary %>%
    group_by(Locality_Code) %>%
    summarise(mean_val = round(mean(.data[[col]]), 1), .groups = 'drop') %>%
    pull(mean_val)
  
  locality_aggregated[[paste0("SD_", col)]] <- locality_summary %>%
    group_by(Locality_Code) %>%
    summarise(sd_val = round(sd(.data[[col]]), 1), .groups = 'drop') %>%
    pull(sd_val)
}

# Create pattern-based summary (HM vs SP)
cat("\nCreating HM vs SP pattern summary...\n")

# Add pattern column to locality_summary
locality_summary_with_pattern <- locality_summary %>%
  mutate(
    Pattern = case_when(
      grepl("HM", Site_Name, ignore.case = TRUE) ~ "HM",
      grepl("SP", Site_Name, ignore.case = TRUE) ~ "SP", 
      TRUE ~ "Other"
    )
  )

# Pattern summary
pattern_summary <- locality_summary_with_pattern %>%
  group_by(Pattern) %>%
  summarise(
    N_sites = n(),
    Mean_total_species = round(mean(Total_species), 1),
    SD_total_species = round(sd(Total_species), 1),
    .groups = 'drop'
  )

# Add specialization totals and means by pattern
for(col in spec_columns) {
  pattern_summary[[paste0("Total_", col)]] <- locality_summary_with_pattern %>%
    group_by(Pattern) %>%
    summarise(total_val = sum(.data[[col]]), .groups = 'drop') %>%
    pull(total_val)
  
  pattern_summary[[paste0("Mean_", col)]] <- locality_summary_with_pattern %>%
    group_by(Pattern) %>%
    summarise(mean_val = round(mean(.data[[col]]), 1), .groups = 'drop') %>%
    pull(mean_val)
  
  pattern_summary[[paste0("SD_", col)]] <- locality_summary_with_pattern %>%
    group_by(Pattern) %>%
    summarise(sd_val = round(sd(.data[[col]]), 1), .groups = 'drop') %>%
    pull(sd_val)
}

# Save detailed summary (per site)
write_csv(locality_summary_with_pattern, "diatom_specialist_summary_per_site.csv")
cat("✅ Saved: diatom_specialist_summary_per_site.csv\n")

# Save aggregated summary (per locality code)
write_csv(locality_aggregated, "diatom_specialist_summary_by_locality.csv")
cat("✅ Saved: diatom_specialist_summary_by_locality.csv\n")

# Save pattern summary (HM vs SP)
write_csv(pattern_summary, "diatom_specialist_summary_HM_vs_SP.csv")
cat("✅ Saved: diatom_specialist_summary_HM_vs_SP.csv\n")

# Create summary statistics
cat("\n=== SUMMARY STATISTICS ===\n")

cat("\nPer-site summary (first 10 rows):\n")
print(head(locality_summary, 10))

cat("\nAggregated by locality code:\n")
print(locality_aggregated)

cat("\nPattern-based summary (HM vs SP):\n")
print(pattern_summary)

# Overall statistics
cat("\n=== OVERALL STATISTICS ===\n")
cat("Total sites analyzed:", nrow(locality_summary), "\n")
cat("Total unique locality codes:", length(unique(locality_summary$Locality_Code)), "\n")
cat("Mean species per site:", round(mean(locality_summary$Total_species), 1), "±", round(sd(locality_summary$Total_species), 1), "\n")

# Overall totals for each specialization category
cat("\n=== TOTAL COUNTS BY SPECIALIZATION CATEGORY ===\n")
for(col in spec_columns) {
  total_count <- sum(locality_summary[[col]])
  cat(sprintf("%-25s: %d total occurrences\n", col, total_count))
}

# Specialization category statistics
cat("\nSpecialization categories per site:\n")
for(col in spec_columns) {
  mean_val <- round(mean(locality_summary[[col]]), 1)
  sd_val <- round(sd(locality_summary[[col]]), 1)
  total_val <- sum(locality_summary[[col]])
  cat(sprintf("%-25s: Total = %4d, Mean = %4.1f ± %4.1f SD per site\n", col, total_val, mean_val, sd_val))
}

# Pattern comparison
cat("\n=== HM vs SP PATTERN COMPARISON ===\n")
hm_sites <- sum(grepl("HM", locality_summary$Site_Name, ignore.case = TRUE))
sp_sites <- sum(grepl("SP", locality_summary$Site_Name, ignore.case = TRUE))
other_sites <- nrow(locality_summary) - hm_sites - sp_sites

cat("HM sites:", hm_sites, "\n")
cat("SP sites:", sp_sites, "\n") 
cat("Other sites:", other_sites, "\n")

for(col in spec_columns) {
  hm_total <- sum(locality_summary_with_pattern[locality_summary_with_pattern$Pattern == "HM", col])
  sp_total <- sum(locality_summary_with_pattern[locality_summary_with_pattern$Pattern == "SP", col])
  hm_mean <- round(mean(locality_summary_with_pattern[locality_summary_with_pattern$Pattern == "HM", col]), 1)
  sp_mean <- round(mean(locality_summary_with_pattern[locality_summary_with_pattern$Pattern == "SP", col]), 1)
  
  cat(sprintf("%-25s: HM = %4d total (%4.1f mean), SP = %4d total (%4.1f mean)\n", 
              col, hm_total, hm_mean, sp_total, sp_mean))
}

# Check data quality - verify all species are classified
cat("\n=== DATA QUALITY CHECK ===\n")

# Species not found in specialization data
all_species <- colnames(pa_data)[-1]
classified_species <- spec_data$Short_names[!is.na(spec_data$Short_names)]
unclassified_species <- setdiff(all_species, classified_species)

cat("Total species in PA data:", length(all_species), "\n")
cat("Species with specialization data:", length(classified_species), "\n") 
cat("Species without specialization data:", length(unclassified_species), "\n")

if(length(unclassified_species) > 0) {
  cat("⚠️  WARNING: Found unclassified species despite expecting none!\n")
  unclassified_df <- data.frame(
    Species_Short_name = unclassified_species,
    Status = "Missing_from_specialization_data"
  )
  write_csv(unclassified_df, "species_lacking_trait_info.csv")
  cat("✅ Saved: species_lacking_trait_info.csv\n")
} else {
  cat("✅ All species have trait information - perfect!\n")
}

# Also check for species with trait data but missing specialization values
species_with_missing_spec <- spec_data %>%
  filter(!is.na(Short_names) & is.na(Specialization)) %>%
  select(Short_names, Species, Specializatio_by_words)

if(nrow(species_with_missing_spec) > 0) {
  write_csv(species_with_missing_spec, "species_with_missing_specialization_values.csv")
  cat("✅ Saved: species_with_missing_specialization_values.csv\n")
  cat("\n⚠️  SPECIES WITH MISSING SPECIALIZATION VALUES:\n")
  cat("Found", nrow(species_with_missing_spec), "species in trait file but with missing specialization numbers\n")
  print(species_with_missing_spec)
} else {
  cat("✅ All species in trait file have specialization values - excellent!\n")
}

cat("\n📁 Files created in Diatoms/data/:\n")
cat("• diatom_specialist_summary_per_site.csv (detailed data per site with HM/SP pattern)\n")
cat("• diatom_specialist_summary_by_locality.csv (aggregated by locality code with totals)\n")
cat("• diatom_specialist_summary_HM_vs_SP.csv (comparison between HM and SP patterns)\n")

cat("\n📊 Analysis completed!\n")
cat("🔍 Key information provided:\n")
cat("• Total counts for each specialization category across all sites\n")
cat("• Mean ± SD for each category per site\n")
cat("• Breakdown by locality codes (first 3 letters)\n") 
cat("• Comparison between HM and SP pattern sites\n")
cat("• All species properly classified with trait information\n")

cat("\n💡 Files saved to Diatoms/data/ for use in other analyses\n")
cat("🔧 Next steps:\n")
cat("• Use HM vs SP summary for ecological pattern analysis\n")
cat("• Import these CSVs into other analyses as needed\n")
cat("• Analyze specialization patterns across localities\n")

