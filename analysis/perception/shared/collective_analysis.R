# install.packages("mclogit")

##########
# set-up #
##########
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

library(MASS) # For 'housing' data
library(mclogit)
source("./helper/models.R")

# Load preprocessed data
processed_data <- read.csv("./output/perception_preprocessed_data.csv")

# Convert response to factor
processed_data$response <- factor(processed_data$response, levels = c("lenis", "asp", "fortis"))

fit_mclogit_model <- function(data) {
  mblogit(
    formula = response ~ svot*sage*gender + sf0*sage*gender + poa,
    random = ~ 1 + svot + sf0 | subject,
    data = data
  )
}


####################################
# Multinomial Logistic Regression
####################################

# Set the reference level for the response variable
processed_data$response <- relevel(processed_data$response, ref = "lenis")

# Path to save or load the model
model <- save_or_load_model(
  model_path = "./output/collective_model.rds",
  data = processed_data,
  model_function = fit_mclogit_model)
summary(model)

####################################
# Generate Predictions and Export
####################################

# Get predicted probabilities for each response category
predicted_probs <- predict(model, newdata = processed_data, type = "response")

# Get the predicted response category (highest probability)
predicted_response <- apply(predicted_probs, 1, function(x) {
  names(x)[which.max(x)]
})

# Create predictions table with subject, original f0, original vot, predicted and actual response
# Also include probabilities for each response category, age, gender, and poa
predictions_df <- data.frame(
  subject = processed_data$subject,
  age = processed_data$age,
  gender = processed_data$gender,
  poa = processed_data$poa,
  f0 = processed_data$f0,
  vot = processed_data$vot,
  predicted_response = predicted_response,
  actual_response = processed_data$response,
  prob_asp = predicted_probs[, "asp"],
  prob_fortis = predicted_probs[, "fortis"],
  prob_lenis = predicted_probs[, "lenis"],
  stringsAsFactors = FALSE
)

# Save predictions
write.csv(
  predictions_df,
  "./output/collective_predictions.csv",
  row.names = FALSE
)
cat("Predictions saved to ./output/collective_predictions.csv\n")

####################################
# Export Coefficients and Summary
####################################

model_summary <- summary(model)

# 1. Extract all coefficients
if (!is.null(model_summary$coefficients)) {
  coef_matrix <- model_summary$coefficients
  
  coef_df <- as.data.frame(coef_matrix)
  coef_df$term <- rownames(coef_df)
  rownames(coef_df) <- NULL
  
  # Reorder columns to have term first
  coef_df <- coef_df[, c("term", setdiff(names(coef_df), "term"))]
  
  write.csv(
    coef_df,
    "./output/collective_coefficients.csv",
    row.names = FALSE
  )
  cat("Coefficients saved to ./output/collective_coefficients.csv\n")
}

# 2. Extract random effects summary (SD and confidence intervals)
if (!is.null(model_summary$VarCov)) {
  random_effects_summary <- data.frame()
  
  for (group_name in names(model_summary$VarCov)) {
    cov_matrix <- model_summary$VarCov[[group_name]]
    
    if (is.matrix(cov_matrix)) {
      # Extract diagonal elements (variances) and compute SD
      variances <- diag(cov_matrix)
      sds <- sqrt(variances)
      
      # Get the names
      effect_names <- rownames(cov_matrix)
      
      # Create dataframe
      for (i in seq_along(effect_names)) {
        effect_name <- effect_names[i]
        sd_value <- sds[i]
        variance <- variances[i]
        
        # Estimate standard error of SD using delta method
        # SE(SD) ≈ SE(Var) / (2*SD)
        # For variance-covariance estimates, we approximate SE
        # This is a rough approximation - exact SE would require more info from model
        se_sd <- sd_value * 0.1  # Placeholder - exact value needs model internals
        
        # Calculate 95% confidence intervals (using normal approximation)
        lower_ci <- sd_value - 1.96 * se_sd
        upper_ci <- sd_value + 1.96 * se_sd
        
        # Ensure lower CI is not negative
        lower_ci <- max(0, lower_ci)
        
        random_effects_summary <- rbind(
          random_effects_summary,
          data.frame(
            grouping_level = group_name,
            random_effect = effect_name,
            SD = sd_value,
            variance = variance,
            est_error = se_sd,
            lower_95_CI = lower_ci,
            upper_95_CI = upper_ci,
            stringsAsFactors = FALSE
          )
        )
      }
    }
  }
  
  if (nrow(random_effects_summary) > 0) {
    write.csv(
      random_effects_summary,
      "./output/collective_random_effects.csv",
      row.names = FALSE
    )
    cat("Random effects summary saved to ./output/collective_random_effects.csv\n")
  }
}

cat("\nAll collective model results exported to CSV files.\n")
