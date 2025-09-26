# GLM Analysis Script for Species Richness Data (Insularity Fens Research)
# Load required libraries
library(tidyverse)
library(ggplot2)
library(broom)

# Set working directory and create results folder
setwd("C:/Users/patricia/PATA/SCIENCE/RESEARCH/3_INSULARITY_FENS/3_EXCEL_CSV")
dir.create("GLM_Results", showWarnings = FALSE)

# Load data
data <- read.csv("Data_VIF_HamVer_CarCho_out.csv")

# Transform variables for better model performance
cat("Transforming variables based on ecological context...\n")
data <- data %>%
  mutate(
    # Core environmental variables (original 6)
    DNSS_log = log(DNSS + 1),        # Distance to nearest species source (m)
    NND_log = log(NND + 1),          # Nearest neighbor distance (m) 
    Area_log = log(Area + 1),        # Area (m²) - always log-transformed in ecology
    Age_sqrt = sqrt(Age),            # Age (years) - square root for temporal data
    
    # Landscape variables (add based on VIF results)
    NIB5_log = if("NIB5" %in% names(data)) log(NIB5 + 1) else NA,     # Number of islands in 5km buffer
    
    # Add other landscape variables that passed VIF test (update based on your VIF results)
    # Uncomment and modify based on which variables have VIF < 10:
    # NIB10_log = if("NIB10" %in% names(data)) log(NIB10 + 1) else NA,
    # AIB5_log = if("AIB5" %in% names(data)) log(AIB5 + 1) else NA,
    # AgeReg5km_sqrt = if("AgeReg5km" %in% names(data)) sqrt(AgeReg5km) else NA,
    
    # Standardize all variables for comparison (z-scores: mean=0, sd=1)
    DNSS_std = scale(DNSS_log)[,1],
    NND_std = scale(NND_log)[,1],
    Area_std = scale(Area_log)[,1],
    Age_std = scale(Age_sqrt)[,1],
    pHadj_std = scale(pHadj)[,1],    # pHadj already on log scale, just standardize
    WTDmed_std = scale(WTDmed)[,1],  # Water table depth (cm), just standardize
    
    # Standardize landscape variables
    NIB5_std = if("NIB5" %in% names(data)) scale(NIB5_log)[,1] else NA,
    
    # Add standardized versions of other landscape variables:
    # NIB10_std = if("NIB10" %in% names(data)) scale(NIB10_log)[,1] else NA,
    # AIB5_std = if("AIB5" %in% names(data)) scale(AIB5_log)[,1] else NA,
    # AgeReg5km_std = if("AgeReg5km" %in% names(data)) scale(AgeReg5km_sqrt)[,1] else NA,
    
    # Keep original variables for reference
    DNSS_orig = DNSS,
    NND_orig = NND,
    Area_orig = Area,
    Age_orig = Age,
    pHadj_orig = pHadj,
    WTDmed_orig = WTDmed
  )

# Define response variables and factors (using transformed variables)
response_vars <- c("IslBryVas2", "PlotBryVas2", "IslBry2", "PlotBry2", "IslVas2", "PlotVas2")

# Core factors (original 6)
core_factors <- c("WTDmed_std", "NND_std", "Age_std", "DNSS_std", "Area_std", "pHadj_std")

# Landscape factors (add based on VIF < 10 results)
landscape_factors <- c("NIB5_std")  # Update this list based on your VIF results
# Add more landscape variables that passed VIF test:
# landscape_factors <- c("NIB5_std", "NIB10_std", "AIB5_std", "AgeReg5km_std")

# Combine all factors, removing any that are NA (not available in data)
available_landscape <- landscape_factors[!is.na(landscape_factors)]
factors <- c(core_factors, available_landscape)

# Remove any factors that resulted in all NA values
factors <- factors[sapply(factors, function(x) !all(is.na(data[[x]])))]

# Original factor names for reference
factors_original <- c("WTDmed", "NND", "Age", "DNSS", "Area", "pHadj", "NIB5")  # Update based on included variables

cat("Variables included in analysis:\n")
cat("Core factors:", paste(core_factors, collapse = ", "), "\n")
cat("Landscape factors:", paste(available_landscape, collapse = ", "), "\n")
cat("Total factors:", length(factors), "\n")

# Initialize results storage
full_model_results <- list()
residual_model_results <- list()
variable_importance <- list()

# Function to calculate variable importance (based on t-values)
calculate_importance <- function(model_summary) {
  coef_df <- model_summary$coefficients
  
  # Check if t value column exists and has proper dimensions
  if(is.null(coef_df) || ncol(coef_df) < 4) {
    warning("Model coefficients missing or incomplete")
    return(NULL)
  }
  
  # Check if t value column exists by name or position
  if("t value" %in% colnames(coef_df)) {
    importance <- abs(coef_df[, "t value"])
  } else if(ncol(coef_df) >= 3) {
    importance <- abs(coef_df[, 3])  # t-value is typically 3rd column
  } else {
    warning("t-values not found in model summary")
    return(NULL)
  }
  
  names(importance) <- rownames(coef_df)
  
  # Remove intercept if present
  if("(Intercept)" %in% names(importance)) {
    importance <- importance[-which(names(importance) == "(Intercept)")]
  }
  
  return(importance)
}

# Function to perform GLM analysis
perform_glm_analysis <- function(response_var, data, factors) {
  # Create formula
  formula_str <- paste(response_var, "~", paste(factors, collapse = " + "))
  formula_obj <- as.formula(formula_str)
  
  # Fit GLM model with error handling
  tryCatch({
    model <- glm(formula_obj, data = data, family = poisson())
    
    # Check for convergence
    if(!model$converged) {
      warning(paste("Model for", response_var, "did not converge"))
    }
    
    # Get model summary
    model_summary <- summary(model)
    
    # Calculate variable importance
    importance <- calculate_importance(model_summary)
    
    if(is.null(importance)) {
      # Return empty results if importance calculation failed
      results_df <- data.frame(
        Variable = character(0),
        Importance = numeric(0),
        Response = character(0),
        stringsAsFactors = FALSE
      )
    } else {
      # Create results data frame (clean variable names for output)
      results_df <- data.frame(
        Variable = str_remove(names(importance), "_std"),  # Remove _std suffix for cleaner output
        Importance = importance,
        Response = response_var,
        stringsAsFactors = FALSE
      )
    }
    
    return(list(model = model, summary = model_summary, importance = results_df))
    
  }, error = function(e) {
    warning(paste("Error fitting model for", response_var, ":", e$message))
    # Return empty results
    empty_df <- data.frame(
      Variable = character(0),
      Importance = numeric(0),
      Response = character(0),
      stringsAsFactors = FALSE
    )
    return(list(model = NULL, summary = NULL, importance = empty_df))
  })
}

# Function to perform residual GLM analysis (uses Gaussian for residuals)
perform_residual_glm_analysis <- function(response_var, data, factors) {
  # Create formula
  formula_str <- paste(response_var, "~", paste(factors, collapse = " + "))
  formula_obj <- as.formula(formula_str)
  
  # Fit GLM model with Gaussian family for residuals (can handle negative values)
  tryCatch({
    model <- glm(formula_obj, data = data, family = gaussian())
    
    # Check for convergence
    if(!model$converged) {
      warning(paste("Residual model for", response_var, "did not converge"))
    }
    
    # Get model summary
    model_summary <- summary(model)
    
    # Calculate variable importance
    importance <- calculate_importance(model_summary)
    
    if(is.null(importance)) {
      # Return empty results if importance calculation failed
      results_df <- data.frame(
        Variable = character(0),
        Importance = numeric(0),
        Response = character(0),
        stringsAsFactors = FALSE
      )
    } else {
      # Create results data frame (clean variable names for output)
      results_df <- data.frame(
        Variable = str_remove(names(importance), "_std"),  # Remove _std suffix for cleaner output
        Importance = importance,
        Response = response_var,
        stringsAsFactors = FALSE
      )
    }
    
    return(list(model = model, summary = model_summary, importance = results_df))
    
  }, error = function(e) {
    warning(paste("Error fitting residual model for", response_var, ":", e$message))
    # Return empty results
    empty_df <- data.frame(
      Variable = character(0),
      Importance = numeric(0),
      Response = character(0),
      stringsAsFactors = FALSE
    )
    return(list(model = NULL, summary = NULL, importance = empty_df))
  })
}

# Analyze each response variable
cat("Performing GLM analysis for each response variable...\n")

for (response_var in response_vars) {
  cat(paste("Analyzing:", response_var, "\n"))
  
  # Full model analysis
  full_analysis <- perform_glm_analysis(response_var, data, factors)
  full_model_results[[response_var]] <- full_analysis
  
  # Store variable importance (only if successful)
  if(nrow(full_analysis$importance) > 0) {
    variable_importance[[paste(response_var, "full", sep = "_")]] <- full_analysis$importance
  }
  
  # Create residuals excluding pHadj effect
  factors_no_ph <- factors[factors != "pHadj_std"]  # Remove pHadj completely
  
  # Fit model without pHadj to get residuals
  tryCatch({
    formula_no_ph <- as.formula(paste(response_var, "~", paste(factors_no_ph, collapse = " + ")))
    model_no_ph <- glm(formula_no_ph, data = data, family = poisson())
    
    # Get residuals
    residuals_data <- data
    residuals_data[[response_var]] <- residuals(model_no_ph)
    
    # IMPORTANT: Analyze residuals WITHOUT pHadj (only the remaining factors)
    residual_analysis <- perform_residual_glm_analysis(response_var, residuals_data, factors_no_ph)
    residual_model_results[[response_var]] <- residual_analysis
    
    # Store variable importance for residuals (only if successful)
    if(nrow(residual_analysis$importance) > 0) {
      variable_importance[[paste(response_var, "residual", sep = "_")]] <- residual_analysis$importance
    }
    
  }, error = function(e) {
    warning(paste("Error in residual analysis for", response_var, ":", e$message))
    residual_model_results[[response_var]] <- list(model = NULL, summary = NULL, importance = data.frame())
  })
}

# Combine all importance results (with error checking)
if(length(variable_importance) > 0) {
  all_importance <- bind_rows(variable_importance, .id = "Analysis")
  
  # Continue with analysis if we have results
  if(length(variable_importance) > 0 && nrow(all_importance) > 0) {
    # Separate full model and residual analysis
    full_importance <- all_importance %>%
      filter(str_detect(Analysis, "_full")) %>%
      mutate(Analysis_Type = "Full Model",
             Response_Var = str_remove(Analysis, "_full"))
    
    residual_importance <- all_importance %>%
      filter(str_detect(Analysis, "_residual")) %>%
      mutate(Analysis_Type = "Residual Analysis",
             Response_Var = str_remove(Analysis, "_residual"))
    
    combined_importance <- bind_rows(full_importance, residual_importance)
    
    # Create visualization for variable importance
    importance_plot <- ggplot(combined_importance, aes(x = reorder(Variable, Importance), 
                                                       y = Importance, 
                                                       fill = Analysis_Type)) +
      geom_col(position = "dodge", alpha = 0.7) +
      facet_wrap(~Response_Var, scales = "free") +
      coord_flip() +
      labs(title = "Variable Importance Across Species Richness Variables",
           subtitle = "Full Model: All factors | Residual Analysis: pHadj effect REMOVED completely",
           x = "Variables",
           y = "Importance (|t-value|)",
           fill = "Analysis Type") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Save the plot
    ggsave("GLM_Results/variable_importance_plot.png", importance_plot, 
           width = 14, height = 10, dpi = 300)
    
  } else {
    cat("Warning: No successful model results to plot\n")
  }
} else {
  cat("Warning: No variable importance results calculated\n")
}

# Create summary tables
cat("Creating summary tables...\n")

# Most influential variable for each response variable
most_influential_full <- combined_importance %>%
  filter(Analysis_Type == "Full Model") %>%
  group_by(Response_Var) %>%
  slice_max(Importance, n = 1) %>%
  select(Response_Var, Most_Influential = Variable, Importance_Score = Importance)

most_influential_residual <- combined_importance %>%
  filter(Analysis_Type == "Residual Analysis") %>%
  group_by(Response_Var) %>%
  slice_max(Importance, n = 1) %>%
  select(Response_Var, Most_Influential = Variable, Importance_Score = Importance)

# Save detailed results
cat("Saving detailed results...\n")

# Create transformation summary
transformation_summary <- data.frame(
  Variable = factors_original,
  Description = c("Water table depth (cm)", "Nearest neighbor distance (m)", 
                  "Patch age (years)", "Distance to species source (m)", 
                  "Patch area (m²)", "pH adjusted by Ca (log scale)",
                  "Number of islands in 5km buffer"),  # Add descriptions for landscape variables
  Original_Min = sapply(factors_original, function(x) if(x %in% names(data)) min(data[[x]], na.rm = TRUE) else NA),
  Original_Max = sapply(factors_original, function(x) if(x %in% names(data)) max(data[[x]], na.rm = TRUE) else NA),
  Original_Range = sapply(factors_original, function(x) if(x %in% names(data)) max(data[[x]], na.rm = TRUE) - min(data[[x]], na.rm = TRUE) else NA),
  Transformation = c("Standardized only", "Log + Standardized", "Square root + Standardized", 
                     "Log + Standardized", "Log + Standardized", "Standardized only (already log scale)",
                     "Log + Standardized"),  # Add transformations for landscape variables
  Rationale = c("Linear depth measure", "Spatial distance (ecological standard)", 
                "Temporal variable (moderate transformation)", "Spatial distance (ecological standard)",
                "Area variable (always log in ecology)", "Already on logarithmic scale",
                "Count data (ecological standard)"),  # Add rationales for landscape variables
  Variable_Type = c("Chemical", "Spatial", "Temporal", "Spatial", "Spatial", "Chemical", "Landscape"),
  stringsAsFactors = FALSE
)

write.csv(transformation_summary, "GLM_Results/transformation_summary.csv", row.names = FALSE)

# Save variable importance data
write.csv(combined_importance, "GLM_Results/variable_importance_detailed.csv", row.names = FALSE)
write.csv(most_influential_full, "GLM_Results/most_influential_full_models.csv", row.names = FALSE)
write.csv(most_influential_residual, "GLM_Results/most_influential_residual_models.csv", row.names = FALSE)

# Save model summaries
for (response_var in response_vars) {
  # Full model summary
  capture.output(
    print(full_model_results[[response_var]]$summary),
    file = paste0("GLM_Results/", response_var, "_full_model_summary.txt")
  )
  
  # Residual model summary
  capture.output(
    print(residual_model_results[[response_var]]$summary),
    file = paste0("GLM_Results/", response_var, "_residual_model_summary.txt")
  )
}

# Create a comprehensive summary report
cat("Creating comprehensive summary report...\n")

sink("GLM_Results/analysis_summary_report.txt")
cat("GLM ANALYSIS SUMMARY REPORT\n")
cat("===========================\n\n")

cat("DATASET INFORMATION:\n")
cat("- File: Data_VIF_HamVer_CarCho_out.csv\n")
cat("- Number of localities:", nrow(data), "\n")
cat("- Response variables:", paste(response_vars, collapse = ", "), "\n")
cat("- Original factors:", paste(factors_original, collapse = ", "), "\n")
cat("- Transformed factors used:", paste(factors, collapse = ", "), "\n")
cat("- Transformations applied:\n")
cat("  * DNSS (distance to species source): log(x+1) + standardized\n")
cat("  * NND (nearest neighbor distance): log(x+1) + standardized\n") 
cat("  * Area (patch area): log(x+1) + standardized\n")
cat("  * Age (patch age): sqrt(x) + standardized\n")
cat("  * pHadj (pH adjusted by Ca): standardized only (already log scale)\n")
cat("  * WTDmed (water table depth): standardized only\n")
cat("  * NIB5 (island count 5km): log(x+1) + standardized\n")
cat("- Model families: Poisson for original data, Gaussian for residual analysis\n")
cat("- Residual analysis: Effect of pHadj COMPLETELY REMOVED, then remaining factors analyzed\n")
cat("- Residual model includes: WTDmed, NND, Age, DNSS, Area, NIB5 (NO pHadj)\n\n")

cat("MOST INFLUENTIAL VARIABLES (Full Models):\n")
print(most_influential_full)
cat("\n")

cat("MOST INFLUENTIAL VARIABLES (Residual Analysis - pHadj COMPLETELY EXCLUDED):\n")
print(most_influential_residual)
cat("\n")

cat("VARIABLE IMPORTANCE RANKING (Average across all response variables):\n")
avg_importance <- combined_importance %>%
  group_by(Variable, Analysis_Type) %>%
  summarise(Avg_Importance = mean(Importance), .groups = "drop") %>%
  arrange(Analysis_Type, desc(Avg_Importance))

print(avg_importance)

sink()

# INTERACTION ANALYSIS
cat("\n" %+% "="*50 %+% "\n")
cat("STARTING INTERACTION ANALYSIS\n")
cat("="*50 %+% "\n")

# Interaction Analysis
cat("Performing interaction analysis for significant variables...\n")

# Initialize interaction results storage
interaction_results <- list()
interaction_summary <- list()

# Define potential interactions to test
# Based on ecological hypotheses: connectivity-chemistry, spatial-chemistry, etc.
interaction_pairs <- list(
  c("NIB5_std", "pHadj_std"),     # Landscape connectivity × Chemistry
  c("DNSS_std", "pHadj_std"),     # Distance to source × Chemistry  
  c("Area_std", "pHadj_std"),     # Patch size × Chemistry
  c("NND_std", "pHadj_std"),      # Nearest neighbor × Chemistry
  c("Age_std", "pHadj_std"),      # Age × Chemistry
  c("WTDmed_std", "pHadj_std"),   # Water table × pH
  c("NIB5_std", "Area_std"),      # Connectivity × Patch size
  c("NIB5_std", "DNSS_std"),      # Connectivity × Distance to source
  c("Area_std", "DNSS_std")       # Patch size × Distance to source
)

# Function to test interactions
test_interactions <- function(response_var, data, base_factors, interaction_pairs) {
  
  cat(paste("Testing interactions for:", response_var, "\n"))
  
  # Base model (additive effects only)
  base_formula <- as.formula(paste(response_var, "~", paste(base_factors, collapse = " + ")))
  base_model <- glm(base_formula, data = data, family = poisson())
  
  interaction_tests <- data.frame(
    Interaction = character(),
    AIC_base = numeric(),
    AIC_interaction = numeric(),
    Delta_AIC = numeric(),
    Chi_sq = numeric(),
    p_value = numeric(),
    Significant = logical(),
    Response = character(),
    stringsAsFactors = FALSE
  )
  
  # Test each interaction
  for(pair in interaction_pairs) {
    var1 <- pair[1]
    var2 <- pair[2]
    
    # Check if both variables are in the model
    if(var1 %in% base_factors && var2 %in% base_factors) {
      
      tryCatch({
        # Create interaction term
        interaction_term <- paste(var1, var2, sep = ":")
        interaction_formula <- as.formula(paste(response_var, "~", 
                                                paste(base_factors, collapse = " + "), 
                                                "+", interaction_term))
        
        # Fit interaction model
        interaction_model <- glm(interaction_formula, data = data, family = poisson())
        
        # Compare models
        anova_result <- anova(base_model, interaction_model, test = "Chisq")
        
        # Extract results
        delta_aic <- AIC(interaction_model) - AIC(base_model)
        chi_sq <- anova_result$Deviance[2]
        p_val <- anova_result$`Pr(>Chi)`[2]
        
        # Add to results
        interaction_tests <- rbind(interaction_tests, data.frame(
          Interaction = paste(str_remove(var1, "_std"), "×", str_remove(var2, "_std")),
          AIC_base = AIC(base_model),
          AIC_interaction = AIC(interaction_model),
          Delta_AIC = delta_aic,
          Chi_sq = chi_sq,
          p_value = p_val,
          Significant = p_val < 0.05,
          Response = response_var
        ))
        
      }, error = function(e) {
        warning(paste("Failed to test interaction", var1, "×", var2, "for", response_var, ":", e$message))
      })
    }
  }
  
  return(interaction_tests)
}

# Test interactions for each response variable
for (response_var in response_vars) {
  # First, identify significant variables from the full model
  full_analysis <- full_model_results[[response_var]]
  
  if(!is.null(full_analysis$summary)) {
    # Extract significant variables (p < 0.05)
    coeffs <- full_analysis$summary$coefficients
    significant_vars <- rownames(coeffs)[coeffs[, "Pr(>|z|)"] < 0.05 & rownames(coeffs) != "(Intercept)"]
    
    if(length(significant_vars) >= 2) {
      cat(paste("Significant variables for", response_var, ":", paste(significant_vars, collapse = ", "), "\n"))
      
      # Test interactions among significant variables
      interaction_tests <- test_interactions(response_var, data, significant_vars, interaction_pairs)
      
      if(nrow(interaction_tests) > 0) {
        interaction_summary[[response_var]] <- interaction_tests
      }
    } else {
      cat(paste("Insufficient significant variables for interaction testing in", response_var, "\n"))
    }
  }
}

# Combine and analyze interaction results
if(length(interaction_summary) > 0) {
  all_interactions <- bind_rows(interaction_summary)
  
  # Save detailed results
  write.csv(all_interactions, "GLM_Results/interaction_analysis_results.csv", row.names = FALSE)
  
  # Summarize significant interactions
  significant_interactions <- all_interactions %>%
    filter(Significant == TRUE) %>%
    arrange(p_value)
  
  if(nrow(significant_interactions) > 0) {
    cat("\nSIGNIFICANT INTERACTIONS FOUND:\n")
    print(significant_interactions %>% select(Response, Interaction, Delta_AIC, p_value))
    
    write.csv(significant_interactions, "GLM_Results/significant_interactions.csv", row.names = FALSE)
    
    # Visualization of interaction effects
    interaction_plot <- ggplot(all_interactions, aes(x = reorder(Interaction, -Delta_AIC), 
                                                     y = Delta_AIC, fill = Significant)) +
      geom_col(alpha = 0.7) +
      geom_hline(yintercept = -2, linetype = "dashed", color = "red") +  # AIC improvement threshold
      facet_wrap(~Response, scales = "free_x") +
      scale_fill_manual(values = c("FALSE" = "lightblue", "TRUE" = "red")) +
      coord_flip() +
      labs(title = "Interaction Effects: Model Improvement (ΔAIC)",
           subtitle = "Negative values indicate better fit with interaction term",
           x = "Variable Interactions",
           y = "ΔAIC (Interaction - Base Model)",
           fill = "Significant\n(p < 0.05)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggsave("GLM_Results/interaction_analysis_plot.png", interaction_plot, 
           width = 14, height = 10, dpi = 300)
    
    # Summary table of interaction patterns
    interaction_summary_table <- all_interactions %>%
      group_by(Interaction) %>%
      summarise(
        N_responses = n(),
        N_significant = sum(Significant),
        Mean_Delta_AIC = mean(Delta_AIC),
        Min_p_value = min(p_value),
        .groups = "drop"
      ) %>%
      arrange(desc(N_significant), Mean_Delta_AIC)
    
    write.csv(interaction_summary_table, "GLM_Results/interaction_summary_table.csv", row.names = FALSE)
    
    cat("\nINTERACTION SUMMARY TABLE:\n")
    print(interaction_summary_table)
    
  } else {
    cat("\nNo significant interactions detected across all response variables\n")
  }
  
  # Create models with best interactions for each response variable
  cat("\nFitting final models with significant interactions...\n")
  
  final_models <- list()
  for(response_var in names(interaction_summary)) {
    response_interactions <- interaction_summary[[response_var]] %>%
      filter(Significant == TRUE) %>%
      arrange(p_value)
    
    if(nrow(response_interactions) > 0) {
      # Get the best interaction
      best_interaction <- response_interactions$Interaction[1]
      cat(paste("Best interaction for", response_var, ":", best_interaction, "\n"))
      
      # Fit final model with best interaction
      # This would need to be implemented based on specific interaction terms
      final_models[[response_var]] <- best_interaction
    }
  }
  
  cat("\nInteraction analysis completed!\n")
  cat("Results saved:\n")
  cat("- interaction_analysis_results.csv: All interaction tests\n")
  cat("- significant_interactions.csv: Only significant interactions\n")  
  cat("- interaction_summary_table.csv: Summary across response variables\n")
  cat("- interaction_analysis_plot.png: Visualization of interaction effects\n")
  
} else {
  cat("No interaction tests could be performed\n")
}

cat("\n" %+% "="*50 %+% "\n")
cat("INTERACTION ANALYSIS COMPLETED\n")
cat("="*50 %+% "\n")

# Print completion message
cat("\n" %+% "="*50 %+% "\n")
cat("ANALYSIS COMPLETED SUCCESSFULLY!\n")
cat("="*50 %+% "\n")
cat("Results saved in 'GLM_Results' folder:\n")
cat("- variable_importance_plot.png: Visualization of variable importance\n")
cat("- variable_importance_detailed.csv: Detailed importance scores\n")
cat("- most_influential_full_models.csv: Top variables for full models\n")
cat("- most_influential_residual_models.csv: Top variables for residual analysis\n")
cat("- interaction_analysis_results.csv: All interaction tests\n")
cat("- significant_interactions.csv: Only significant interactions\n")
cat("- interaction_analysis_plot.png: Interaction effects visualization\n")
cat("- analysis_summary_report.txt: Comprehensive summary\n")
cat("- Individual model summaries for each response variable\n")
cat("="*50 %+% "\n")

# Display quick summary
cat("QUICK SUMMARY:\n")
print(most_influential_full)
cat("\nRESIDUAL ANALYSIS (pHadj excluded):\n")
print(most_influential_residual)

