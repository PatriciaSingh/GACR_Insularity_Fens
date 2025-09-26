# Simplified Multi-file PCOA Analysis for Spider Data
# Direct processing of specified files

library(vegan)
library(ggplot2)
library(ggrepel)
library(readr)
library(dplyr)
getwd()
# Set working directory
setwd("Spiders/data")
cat("Working directory:", getwd(), "\n")

# Create results directory
if(!dir.exists("../results")) dir.create("../results")

# List of files to process - DIRECTLY SPECIFIED
files_to_process <- c(
  "Spider_combined_all_methods_PA.csv",
  "Spider_combined_cerven_PA.csv", 
  "Spider_combined_kveten_PA.csv",
  "Spider_combined_pasti_PA.csv",
  "Spider_combined_smyk_PA.csv",
  "Spider_pasti_cerven_final.csv",
  "Spider_pasti_cerven_finalPA.csv",
  "Spider_pasti_kveten_final.csv",
  "Spider_pasti_kveten_finalPA.csv", 
  "Spider_smyk_cerven_final.csv",
  "Spider_smyk_cerven_finalPA.csv",
  "Spider_smyk_kveten_final.csv",
  "Spider_smyk_kveten_finalPA.csv"
)

cat("=== FILES TO PROCESS ===\n")
for(i in seq_along(files_to_process)) {
  exists_status <- if(file.exists(files_to_process[i])) "✅" else "❌"
  cat(sprintf("%d. %s %s\n", i, files_to_process[i], exists_status))
}

# Load specialization data (optional - if not available, will proceed without it)
spec_data <- NULL
spec_file <- "Spiders_Species_Specializace_Sireni_Ohrozeni.csv"
if(file.exists(spec_file)) {
  tryCatch({
    spec_data <- read_csv(spec_file, show_col_types = FALSE)
    cat("\n✅ Specialization data loaded\n")
  }, error = function(e) {
    cat("\n❌ Could not load specialization data:", e$message, "\n")
    spec_data <- NULL
  })
} else {
  cat("\n⚠️  Specialization file not found - proceeding without specialization coloring\n")
}

# Function to get specialization (simplified)
get_specialization <- function(species_name, spec_data) {
  if(is.null(spec_data)) return("No_Data")
  
  # Try to find species in specialization data
  if("Short_name" %in% names(spec_data)) {
    match_row <- spec_data[spec_data$Short_name == species_name, ]
    if(nrow(match_row) > 0) {
      # Look for specialization column (try different possible names)
      spec_cols <- c("Specialization_words", "Specialization", "Category", "Type")
      for(col in spec_cols) {
        if(col %in% names(match_row) && !is.na(match_row[[col]])) {
          return(as.character(match_row[[col]]))
        }
      }
    }
  }
  return("Unknown")
}

# Main analysis function
analyze_file <- function(filename) {
  cat("\n", paste(rep("=", 60), collapse=""), "\n")
  cat("PROCESSING:", filename, "\n")
  cat(paste(rep("=", 60), collapse=""), "\n")
  
  # Check if file exists
  if(!file.exists(filename)) {
    cat("❌ File not found:", filename, "\n")
    return(NULL)
  }
  
  # Load data
  tryCatch({
    data <- read_csv(filename, show_col_types = FALSE)
    cat("✅ Data loaded:", nrow(data), "rows,", ncol(data), "columns\n")
  }, error = function(e) {
    cat("❌ Error loading data:", e$message, "\n")
    return(NULL)
  })
  
  # Prepare data matrix
  # Assume first column is localities, rest are species
  if(ncol(data) < 4) {
    cat("❌ Not enough columns for analysis\n")
    return(NULL)
  }
  
  localities <- data[[1]]
  species_matrix <- as.matrix(data[, -1])
  rownames(species_matrix) <- localities
  
  # Clean data
  species_matrix[is.na(species_matrix)] <- 0
  
  # Remove empty rows and columns
  species_sums <- colSums(species_matrix)
  site_sums <- rowSums(species_matrix)
  species_matrix <- species_matrix[site_sums > 0, species_sums > 0]
  
  cat("Data after cleaning:", nrow(species_matrix), "sites,", ncol(species_matrix), "species\n")
  
  if(nrow(species_matrix) < 3 || ncol(species_matrix) < 3) {
    cat("❌ Insufficient data for analysis\n")
    return(NULL)
  }
  
  # Determine if this is presence-absence data based on filename
  is_PA <- grepl("PA", filename, ignore.case = TRUE)
  
  if(is_PA) {
    # Convert to presence-absence if not already
    species_matrix[species_matrix > 0] <- 1
    distance_method <- "jaccard"
    analysis_type <- "Presence-Absence"
    cat("Analysis type: Presence-Absence (Jaccard)\n")
  } else {
    distance_method <- "bray"
    analysis_type <- "Abundance"
    cat("Analysis type: Abundance (Bray-Curtis)\n")
  }
  
  # Calculate distance and run PCOA
  tryCatch({
    dist_matrix <- vegdist(species_matrix, method = distance_method)
    pcoa_result <- cmdscale(dist_matrix, k = 2, eig = TRUE)
    
    # Calculate variance explained
    eigenvals <- pcoa_result$eig
    positive_eigs <- eigenvals[eigenvals > 0]
    percent_var <- round((eigenvals[1:2] / sum(positive_eigs)) * 100, 2)
    
    cat("Variance explained: PCo1 =", percent_var[1], "%, PCo2 =", percent_var[2], "%\n")
    
  }, error = function(e) {
    cat("❌ PCOA calculation failed:", e$message, "\n")
    return(NULL)
  })
  
  # Prepare data for plotting
  site_scores <- as.data.frame(pcoa_result$points)
  colnames(site_scores) <- c("PCo1", "PCo2")
  site_scores$Locality <- rownames(site_scores)
  
  # Species scores using weighted averages
  species_scores <- wascores(site_scores[, 1:2], species_matrix)
  species_df <- data.frame(
    Species = colnames(species_matrix),
    PCo1 = species_scores[, 1],
    PCo2 = species_scores[, 2],
    Specialization = sapply(colnames(species_matrix), function(x) get_specialization(x, spec_data))
  )
  
  # Create color palette
  unique_specs <- unique(species_df$Specialization)
  n_specs <- length(unique_specs)
  
  if(n_specs > 1) {
    colors <- rainbow(n_specs, start = 0.1, end = 0.9)
    names(colors) <- unique_specs
  } else {
    colors <- c("gray60")
    names(colors) <- unique_specs[1]
  }
  
  # Create plot
  dataset_name <- tools::file_path_sans_ext(filename)
  
  p <- ggplot() +
    # Sites (gray points)
    geom_point(data = site_scores, aes(x = PCo1, y = PCo2), 
               color = "gray50", size = 3, alpha = 0.7) +
    geom_text_repel(data = site_scores, aes(x = PCo1, y = PCo2, label = Locality), 
                    color = "gray30", size = 2.5, max.overlaps = Inf) +
    # Species (colored points)
    geom_point(data = species_df, aes(x = PCo1, y = PCo2, color = Specialization), 
               size = 2.5, alpha = 0.8) +
    geom_text_repel(data = species_df, aes(x = PCo1, y = PCo2, label = Species, color = Specialization), 
                    size = 2, fontface = "bold", max.overlaps = Inf) +
    scale_color_manual(values = colors, name = "Specialization") +
    labs(
      title = paste("PCOA Analysis:", dataset_name),
      subtitle = paste(analysis_type, "| PCo1 =", percent_var[1], "% | PCo2 =", percent_var[2], "%"),
      x = paste("PCo1 (", percent_var[1], "%)"),
      y = paste("PCo2 (", percent_var[2], "%)"),
      caption = "Gray = Sites | Colored = Species"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      legend.position = "right"
    )
  
  # Save plot
  plot_filename <- paste0("../results/pcoa_", gsub("[^A-Za-z0-9]", "_", dataset_name), ".png")
  
  tryCatch({
    ggsave(plot_filename, p, width = 12, height = 8, dpi = 300)
    cat("✅ Plot saved:", plot_filename, "\n")
  }, error = function(e) {
    cat("❌ Plot save failed:", e$message, "\n")
  })
  
  # Print summary
  cat("Specialization summary:\n")
  print(table(species_df$Specialization))
  
  return(list(
    dataset = dataset_name,
    analysis_type = analysis_type,
    percent_var = percent_var,
    n_sites = nrow(site_scores),
    n_species = nrow(species_df),
    plot = p
  ))
}

# MAIN EXECUTION
cat("\n🕷️  STARTING SPIDER PCOA ANALYSIS\n")

results <- list()
summary_table <- data.frame()

# Process each file
for(filename in files_to_process) {
  result <- analyze_file(filename)
  
  if(!is.null(result)) {
    results[[result$dataset]] <- result
    
    # Add to summary
    summary_table <- rbind(summary_table, data.frame(
      Dataset = result$dataset,
      Analysis_Type = result$analysis_type,
      PCo1_Percent = result$percent_var[1],
      PCo2_Percent = result$percent_var[2],
      Total_Variance = round(sum(result$percent_var), 1),
      N_Sites = result$n_sites,
      N_Species = result$n_species,
      stringsAsFactors = FALSE
    ))
  }
}

# Final summary
cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("🎯 FINAL SUMMARY\n")
cat(paste(rep("=", 70), collapse=""), "\n")

if(nrow(summary_table) > 0) {
  print(summary_table)
  
  # Save summary
  write_csv(summary_table, "../results/analysis_summary.csv")
  cat("\n📊 Summary saved to: ../results/analysis_summary.csv\n")
  
  # List created files
  result_files <- list.files("../results", pattern = "*.png")
  cat("\n📈 Plots created:\n")
  for(f in result_files) {
    cat("  ", f, "\n")
  }
  
  cat("\n✅ ANALYSIS COMPLETE!\n")
  cat("📁 Check the ../results folder for all outputs\n")
  
} else {
  cat("❌ No successful analyses completed\n")
  cat("Check the error messages above for each file\n")
}

# Return results for further use if needed
invisible(results)

