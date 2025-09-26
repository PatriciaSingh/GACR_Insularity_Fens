# FINAL GLM Analysis Script for Species Richness Data (Insularity Fens Research)
# Load required libraries
library(tidyverse)
library(ggplot2)
library(broom)
library(gridExtra)

getwd()
# Set working directory and create final results folder
setwd("C:/Users/patricia/PATA/SCIENCE/RESEARCH/3_INSULARITY_FENS/GACR_Insularity_Fens/1_Insularity_paper")
dir.create("results/Glm_SpeRich", showWarnings = FALSE)

# Load data
data <- read.csv("data/Data_VIF_HamVer_CarCho_out.csv")

# Transform variables 
data <- data %>%
  mutate(
    # Core environmental variables
    DNSS_log = log(DNSS + 1),        # Distance to nearest species source (m)
    NND_log = log(NND + 1),          # Nearest neighbor distance (m) 
    Area_log = log(Area + 1),        # Area (m²) - always log-transformed in ecology
    Age_sqrt = sqrt(Age),            # Age (years) - square root for temporal data
    NIB5_log = if("NIB5" %in% names(data)) log(NIB5 + 1) else NA,     # Number of islands in 5km buffer
    
    # Standardize all variables for comparison (z-scores: mean=0, sd=1)
    DNSS_std = scale(DNSS_log)[,1],
    NND_std = scale(NND_log)[,1],
    Area_std = scale(Area_log)[,1],
    Age_std = scale(Age_sqrt)[,1],
    pHadj_std = scale(pHadj)[,1],    # pHadj already on log scale, just standardize
    WTDmed_std = scale(WTDmed)[,1],  # Water table depth (cm), just standardize
    NIB5_std = if("NIB5" %in% names(data)) scale(NIB5_log)[,1] else NA,
    
    # Keep original variables for reference
    DNSS_orig = DNSS,
    NND_orig = NND,
    Area_orig = Area,
    Age_orig = Age,
    pHadj_orig = pHadj,
    WTDmed_orig = WTDmed
  )

# Define response variables and potential predictors
response_vars <- c("IslBryVas2", "PlotBryVas2", "IslBry2", "PlotBry2", "IslVas2", "PlotVas2")
all_predictors <- c("WTDmed_std", "NND_std", "Age_std", "DNSS_std", "Area_std", "pHadj_std", "NIB5_std")

# Remove any predictors with all NA values
available_predictors <- all_predictors[sapply(all_predictors, function(x) !all(is.na(data[[x]])))]

# Define which response variables need interaction terms (based on previous analysis)
interaction_responses <- c("IslVas2", "IslBryVas2")  # Update based on your interaction results
additive_responses <- setdiff(response_vars, interaction_responses)

# Function to get significant predictors from full model
get_significant_predictors <- function(response_var, data, predictors, alpha = 0.05) {
  # Fit full model
  formula_full <- as.formula(paste(response_var, "~", paste(predictors, collapse = " + ")))
  
  tryCatch({
    model_full <- glm(formula_full, data = data, family = poisson())
    
    # Extract coefficients and p-values
    coeffs <- summary(model_full)$coefficients
    p_values <- coeffs[, "Pr(>|z|)"]
    
    # Get significant predictors (excluding intercept)
    significant_vars <- names(p_values)[p_values < alpha & names(p_values) != "(Intercept)"]
    
    return(list(
      significant_vars = significant_vars,
      full_model = model_full,
      coefficients = coeffs
    ))
    
  }, error = function(e) {
    warning(paste("Error in significance testing for", response_var, ":", e$message))
    return(list(significant_vars = character(0), full_model = NULL, coefficients = NULL))
  })
}

# Function to fit final models
fit_final_model <- function(response_var, data, significant_vars, include_interaction = FALSE) {
  
  if(length(significant_vars) == 0) {
    warning(paste("No significant variables for", response_var))
    return(NULL)
  }
  
  # Check if both NIB5 and pHadj are significant (needed for interaction)
  has_interaction_vars <- "NIB5_std" %in% significant_vars && "pHadj_std" %in% significant_vars
  
  if(include_interaction && has_interaction_vars) {
    # Model with interaction
    formula_str <- paste(response_var, "~", paste(significant_vars, collapse = " + "), "+ NIB5_std:pHadj_std")
  } else {
    # Additive model only
    formula_str <- paste(response_var, "~", paste(significant_vars, collapse = " + "))
  }
  
  formula_obj <- as.formula(formula_str)
  
  tryCatch({
    final_model <- glm(formula_obj, data = data, family = poisson())
    
    return(list(
      model = final_model,
      formula = formula_str,
      type = ifelse(include_interaction && has_interaction_vars, "Interaction", "Additive"),
      significant_vars = significant_vars,
      n_predictors = length(significant_vars)
    ))
    
  }, error = function(e) {
    warning(paste("Error fitting final model for", response_var, ":", e$message))
    return(NULL)
  })
}

# Step 1: Identify significant predictors for each response variable

significance_results <- list()
for(response_var in response_vars) {
  cat(paste("Testing significance for:", response_var, "\n"))
  
  sig_result <- get_significant_predictors(response_var, data, available_predictors)
  significance_results[[response_var]] <- sig_result
  
  if(length(sig_result$significant_vars) > 0) {
    cat(paste("  Significant variables:", paste(sig_result$significant_vars, collapse = ", "), "\n"))
  } else {
    cat("  No significant variables found\n")
  }
}

# Step 2: Fit final models

final_models <- list()
model_summaries <- list()

for(response_var in response_vars) {
  cat(paste("Fitting final model for:", response_var, "\n"))
  
  sig_vars <- significance_results[[response_var]]$significant_vars
  include_interaction <- response_var %in% interaction_responses
  
  final_model <- fit_final_model(response_var, data, sig_vars, include_interaction)
  
  if(!is.null(final_model)) {
    final_models[[response_var]] <- final_model
    
    # Create model summary
    model_summary <- summary(final_model$model)
    model_summaries[[response_var]] <- model_summary
    
    cat(paste("  Model type:", final_model$type, "\n"))
    cat(paste("  Formula:", final_model$formula, "\n"))
    cat(paste("  AIC:", round(AIC(final_model$model), 2), "\n"))
    cat(paste("  Deviance explained:", round((1 - final_model$model$deviance/final_model$model$null.deviance) * 100, 1), "%\n"))
  }
  cat("\n")
}

# Step 3: Create comprehensive results tables
# Model comparison table
model_comparison <- data.frame(
  Response_Variable = character(),
  Model_Type = character(),
  N_Predictors = numeric(),
  AIC = numeric(),
  Deviance_Explained_Percent = numeric(),
  Formula = character(),
  stringsAsFactors = FALSE
)

for(response_var in names(final_models)) {
  model_info <- final_models[[response_var]]
  if(!is.null(model_info)) {
    dev_explained <- (1 - model_info$model$deviance/model_info$model$null.deviance) * 100
    
    model_comparison <- rbind(model_comparison, data.frame(
      Response_Variable = response_var,
      Model_Type = model_info$type,
      N_Predictors = model_info$n_predictors,
      AIC = AIC(model_info$model),
      Deviance_Explained_Percent = dev_explained,
      Formula = model_info$formula
    ))
  }
}

write.csv(model_comparison, "results/GLM_SpeRich/model_comparison_table.csv", row.names = FALSE)

# Coefficients table for all final models
all_coefficients <- data.frame(
  Response_Variable = character(),
  Variable = character(),
  Estimate = numeric(),
  Std_Error = numeric(),
  z_value = numeric(),
  p_value = numeric(),
  Significant = logical(),
  stringsAsFactors = FALSE
)

for(response_var in names(model_summaries)) {
  coeffs <- model_summaries[[response_var]]$coefficients
  
  for(var_name in rownames(coeffs)) {
    if(var_name != "(Intercept)") {
      all_coefficients <- rbind(all_coefficients, data.frame(
        Response_Variable = response_var,
        Variable = str_remove(var_name, "_std|:"),
        Estimate = coeffs[var_name, "Estimate"],
        Std_Error = coeffs[var_name, "Std. Error"],
        z_value = coeffs[var_name, "z value"],
        p_value = coeffs[var_name, "Pr(>|z|)"],
        Significant = coeffs[var_name, "Pr(>|z|)"] < 0.05
      ))
    }
  }
}

write.csv(all_coefficients, "results/GLM_SpeRich/final_model_coefficients.csv", row.names = FALSE)

# Step 4: Create visualizations

# 1. Model comparison plot
comparison_plot <- ggplot(model_comparison, aes(x = Response_Variable, y = Deviance_Explained_Percent, fill = Model_Type)) +
  geom_col(alpha = 0.8) +
  scale_fill_manual(values = c("Additive" = "lightblue", "Interaction" = "red")) +
  labs(title = "Final Model Performance by Response Variable",
       subtitle = "Percentage of deviance explained by final species-specific models",
       x = "Response Variables",
       y = "Deviance Explained (%)",
       fill = "Model Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("results/GLM_SpeRich/model_comparison_plot.png", comparison_plot, width = 12, height = 8, dpi = 300)

# 2. Coefficient plot for all models
coeff_plot <- ggplot(all_coefficients, aes(x = Variable, y = Estimate, color = Significant)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  facet_wrap(~Response_Variable, scales = "free") +
  scale_color_manual(values = c("FALSE" = "gray", "TRUE" = "red")) +
  coord_flip() +
  labs(title = "Final Model Coefficients Across Response Variables",
       subtitle = "Red points indicate significant effects (p < 0.05)",
       x = "Variables",
       y = "Coefficient Estimate",
       color = "Significant") +
  theme_minimal()

ggsave("results/GLM_SpeRich/final_coefficients_plot.png", coeff_plot, width = 14, height = 10, dpi = 300)

# 3. Interaction plot for species with interactions
if(length(interaction_responses) > 0) {
  for(response_var in interaction_responses) {
    if(response_var %in% names(final_models)) {
      model_info <- final_models[[response_var]]
      
      if(model_info$type == "Interaction") {
        # Create interaction visualization
        # Generate prediction data
        pred_data <- expand.grid(
          NIB5_std = seq(-2, 2, length.out = 20),
          pHadj_std = c(-1, 0, 1),  # Low, medium, high pH
          DNSS_std = 0,  # Set other variables to mean
          Area_std = 0,
          Age_std = 0,
          WTDmed_std = 0,
          NND_std = 0
        )
        
        # Only include variables that are in the model
        pred_vars <- intersect(names(pred_data), model_info$significant_vars)
        pred_data_subset <- pred_data[, pred_vars, drop = FALSE]
        
        tryCatch({
          predictions <- predict(model_info$model, newdata = pred_data_subset, type = "response")
          pred_data$Predicted_Richness <- predictions
          pred_data$pH_Level <- factor(pred_data$pHadj_std, labels = c("Low pH", "Medium pH", "High pH"))
          
          interaction_plot <- ggplot(pred_data, aes(x = NIB5_std, y = Predicted_Richness, color = pH_Level)) +
            geom_line(size = 1.2) +
            labs(title = paste("Connectivity × Chemistry Interaction -", response_var),
                 subtitle = "Predicted species richness across connectivity and pH gradients",
                 x = "Landscape Connectivity (NIB5, standardized)",
                 y = "Predicted Species Richness",
                 color = "pH Level") +
            theme_minimal() +
            theme(legend.position = "bottom")
          
          ggsave(paste0("results/GLM_SpeRich/interaction_", response_var, ".png"), 
                 interaction_plot, width = 10, height = 8, dpi = 300)
          
        }, error = function(e) {
          warning(paste("Could not create interaction plot for", response_var, ":", e$message))
        })
      }
    }
  }
}

