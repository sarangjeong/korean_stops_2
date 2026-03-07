# Analysis of experimental results of
# Korean stop contrast, perception - Individual Analysis
# Model training component
# created by Sarang Jeong on June 6, 2021

##########
# set-up #
##########

this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

library(mclogit)
source("./helper/models.R")

# Load preprocessed data
processed_data <- read.csv("./output/perception_preprocessed_data.csv")

# Convert response to factor
processed_data$response <- factor(processed_data$response, levels = c("lenis", "asp", "fortis"))

####################################
# Model fitting
####################################

fit_mclogit_model <- function(data) {
  mblogit(
    formula = response ~ svot + sf0 + poa,
    data = data
  )
}

# Set the reference level for the response variable
processed_data$response <- relevel(processed_data$response, ref = "lenis")

subject_list <- unique(processed_data$subject)

# Ensure output directory exists
output_dir <- "./output/individual"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  cat("Created directory:", output_dir, "\n")
}

# Initialize data frames to collect all data
all_coef <- data.frame()

# Fit and save models for all subjects
for (subj in subject_list) {
  subject_data <- processed_data[processed_data$subject == subj, ]
  
  tryCatch({
    cat("Processing subject:", subj, "\n")
    
    # Fit and save model for this subject
    model <- save_or_load_model(
      model_path = sprintf("./output/individual/%s.rds", subj),
      data = subject_data,
      model_function = fit_mclogit_model)
    
    model_summary <- summary(model)
    
    # 1. Extract all coefficients
    if (!is.null(model_summary$coefficients)) {
      coef_matrix <- model_summary$coefficients
      
      coef_df <- as.data.frame(coef_matrix)
      coef_df$subject <- subj
      coef_df$term <- rownames(coef_df)
      rownames(coef_df) <- NULL
      
      all_coef <- rbind(all_coef, coef_df)
    }
    
  }, error = function(e) {
    cat("Error processing subject", subj, ":", e$message, "\n")
  })
}

# Save all coefficients
if (nrow(all_coef) > 0) {
  # Reorder columns to have subject and term first
  col_order <- c("subject", "term", setdiff(names(all_coef), c("subject", "term")))
  all_coef <- all_coef[, col_order]
  
  write.csv(
    all_coef,
    "./output/individual_mclogit_coefficients.csv",
    row.names = FALSE
  )
  cat("Coefficients saved to ./output/individual_mclogit_coefficients.csv\n")
}

cat("\nAll individual models have been fitted and results exported to CSV files.\n")
