# Production Individual Analysis with LDA (MASS)
# Model: phonation ~ normed_vot + normed_f0 (per participant, per contrast)
# Extract LDA weights as production cue strengths
install.packages(c("tidyverse", "MASS"))
library(tidyverse)
library(MASS)

# Set working directory to script location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# ============================================================================
# 1. Load Data
# ============================================================================

df <- read_csv("output/production_cleaned_data.csv")

cat("\n=== Data Summary ===\n")
cat("Total observations:", nrow(df), "\n")
cat("Number of participants:", n_distinct(df$prolific_id), "\n")
cat("\nPhonation types:\n")
print(table(df$phonation))
cat("\nPOA types:\n")
print(table(df$poa))

# Check which normalized VOT column exists
if ("scaled_vot" %in% colnames(df)) {
  df$normed_vot <- df$scaled_vot
  cat("\nUsing 'scaled_vot' for normalized VOT\n")
} else if ("scaled_vot_by_rest_duration" %in% colnames(df)) {
  df$normed_vot <- df$scaled_vot_by_rest_duration
  cat("\nUsing 'scaled_vot_by_rest_duration' for normalized VOT\n")
}

# Ensure factors are properly set
df <- df %>%
  mutate(
    phonation = factor(phonation, levels = c("lenis", "tense", "aspirated")),
    poa = factor(poa, levels = c("labial", "coronal", "dorsal")),
    prolific_id = as.character(prolific_id)
  ) %>%
  filter(!is.na(normed_vot), !is.na(normed_f0), !is.na(phonation), !is.na(poa))

cat("\nData after removing missing values:", nrow(df), "\n")

# ============================================================================
# 2. Check Data Per Participant
# ============================================================================

participant_summary <- df %>%
  group_by(prolific_id) %>%
  summarise(
    n_obs = n(),
    n_phonation = n_distinct(phonation),
    n_poa = n_distinct(poa),
    .groups = "drop"
  )

cat("\n=== Participant Data Summary ===\n")
print(summary(participant_summary$n_obs))

# Filter participants with sufficient data
valid_participants <- participant_summary %>%
  filter(n_obs >= 10, n_phonation >= 2, n_poa >= 2) %>%
  pull(prolific_id)

cat("\nParticipants with sufficient data:", length(valid_participants), "\n")

# ============================================================================
# 3. LDA Settings
# ============================================================================

cat("\n=== LDA Settings ===\n")
cat("Standardize VOT/F0 within participant: TRUE\n")

# ============================================================================
# 4. Fit Individual LDA Models
# ============================================================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("Fitting LDA models for each participant and contrast...\n")
cat(rep("=", 70), "\n", sep = "")

# Storage for results
lda_results <- list()
coefficient_summaries <- list()
model_summaries <- list()
failed_participants <- c()

contrasts <- list(
  list(name = "lenis_vs_tense", levels = c("lenis", "tense")),
  list(name = "lenis_vs_aspirated", levels = c("lenis", "aspirated"))
)

for (participant in valid_participants) {
  cat("\n--- Participant:", participant, "---\n")
  
  # Get participant data
  p_data <- df %>% filter(prolific_id == participant)
  
  # Standardize normed_vot and normed_f0 within each participant
  p_data <- p_data %>%
    mutate(
      normed_vot = scale(normed_vot)[,1],
      normed_f0 = scale(normed_f0)[,1]
    )
  
  cat("N observations:", nrow(p_data), "\n")
  cat("Phonation types:", paste(unique(p_data$phonation), collapse = ", "), "\n")
  cat("POA types:", paste(unique(p_data$poa), collapse = ", "), "\n")
  
  for (ctr in contrasts) {
    contrast_name <- ctr$name
    contrast_levels <- ctr$levels

    c_data <- p_data %>% filter(phonation %in% contrast_levels)
    if (nrow(c_data) < 6 || n_distinct(c_data$phonation) < 2) {
      cat("Skipping", participant, contrast_name, ": insufficient data\n")
      next
    }

    tryCatch({
      # Fit LDA model: phonation ~ normed_vot + normed_f0
      model <- lda(phonation ~ normed_vot + normed_f0, data = c_data)

      # Store model
      lda_results[[paste(participant, contrast_name, sep = "__")]] <- model

      # Extract LDA coefficients (scaling) for each discriminant dimension
      scaling_df <- as.data.frame(model$scaling)
      scaling_df$predictor <- rownames(scaling_df)
      coef_long <- scaling_df %>%
        pivot_longer(
          cols = -predictor,
          names_to = "discriminant",
          values_to = "coefficient"
        ) %>%
        mutate(
          participant = participant,
          contrast = contrast_name,
          .before = predictor
        )

      coefficient_summaries[[paste(participant, contrast_name, sep = "__")]] <- coef_long

      # Store model summary info
      prop_trace <- model$svd^2 / sum(model$svd^2)
      model_summaries[[paste(participant, contrast_name, sep = "__")]] <- data.frame(
        participant = participant,
        contrast = contrast_name,
        n_obs = nrow(c_data),
        n_phonation = n_distinct(c_data$phonation),
        n_discriminants = length(model$svd),
        prop_trace_ld1 = ifelse(length(prop_trace) >= 1, prop_trace[1], NA_real_),
        row.names = NULL
      )

      cat("✓ Model fitted successfully:", contrast_name, "\n")

    }, error = function(e) {
      cat("✗ Error fitting model:", participant, contrast_name, ":", conditionMessage(e), "\n")
      failed_participants <<- c(failed_participants, paste(participant, contrast_name, sep = "__"))
    })
  }
}

cat("\n", rep("=", 70), "\n", sep = "")
cat("Model fitting complete!\n")
cat("Successful models:", length(lda_results), "\n")
cat("Failed models:", length(failed_participants), "\n")
if (length(failed_participants) > 0) {
  cat("Failed participants:", paste(failed_participants, collapse = ", "), "\n")
}
cat(rep("=", 70), "\n", sep = "")

# ============================================================================
# 5. Combine and Save Results
# ============================================================================

if (length(coefficient_summaries) > 0) {
  
  # Combine all coefficient summaries
  all_coefficients <- bind_rows(coefficient_summaries)

  # Keep LD1 as the primary cue dimension
  coef_ld1 <- all_coefficients %>%
    filter(discriminant == "LD1") %>%
    dplyr::select(participant, contrast, predictor, coefficient)

  # Wide format for quick per-cue comparisons
  coef_wide <- coef_ld1 %>%
    pivot_wider(names_from = predictor, values_from = coefficient)

  # Combine model summaries
  model_summary_df <- bind_rows(model_summaries)
  
  # Save results
  dir.create("output", showWarnings = FALSE)
  
  write_csv(all_coefficients, "output/production_lda_coefficients_long.csv")
  write_csv(coef_wide, "output/production_lda_coefficients_wide.csv")
  write_csv(model_summary_df, "output/production_lda_model_summary.csv")
  
  cat("\n=== Results saved ===\n")
  cat("- output/production_lda_coefficients_long.csv\n")
  cat("- output/production_lda_coefficients_wide.csv\n")
  cat("- output/production_lda_model_summary.csv\n")
  
  # ============================================================================
  # 6. Summary Statistics
  # ============================================================================
  
  cat("\n", rep("=", 70), "\n", sep = "")
  cat("LD1 COEFFICIENT SUMMARY ACROSS PARTICIPANTS\n")
  cat(rep("=", 70), "\n", sep = "")
  
  summary_stats <- coef_ld1 %>%
    group_by(contrast, predictor) %>%
    summarise(
      ld1_mean = mean(coefficient, na.rm = TRUE),
      ld1_sd = sd(coefficient, na.rm = TRUE),
      n_participants = n(),
      .groups = "drop"
    )

  print(summary_stats, n = Inf)

  write_csv(summary_stats, "output/production_lda_summary_statistics.csv")
  cat("\n- output/production_lda_summary_statistics.csv\n")
  
  # ============================================================================
  # 7. Create Coefficient Visualization
  # ============================================================================
  
  cat("\n=== Creating visualization ===\n")
  
  library(ggplot2)
  
  ld1_vot <- coef_ld1 %>% filter(predictor == "normed_vot")
  ld1_f0 <- coef_ld1 %>% filter(predictor == "normed_f0")

  p1 <- ggplot(ld1_vot, aes(x = participant, y = coefficient)) +
    geom_point(size = 3, color = "#1f77b4") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    labs(
      title = "LD1 VOT Weights by Participant (Production)",
      subtitle = "LDA: phonation ~ VOT + F0 (per contrast)",
      x = "Participant",
      y = "LD1 Weight"
    ) +
    facet_wrap(~contrast, scales = "free_x") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(face = "bold", size = 14)
    )

  p2 <- ggplot(ld1_f0, aes(x = participant, y = coefficient)) +
    geom_point(size = 3, color = "#ff7f0e") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    labs(
      title = "LD1 F0 Weights by Participant (Production)",
      subtitle = "LDA: phonation ~ VOT + F0 (per contrast)",
      x = "Participant",
      y = "LD1 Weight"
    ) +
    facet_wrap(~contrast, scales = "free_x") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(face = "bold", size = 14)
    )

  # Save plots
  ggsave("output/production_lda_vot_weights.png", p1, width = 12, height = 6, dpi = 300)
  ggsave("output/production_lda_f0_weights.png", p2, width = 12, height = 6, dpi = 300)

  cat("- output/production_lda_vot_weights.png\n")
  cat("- output/production_lda_f0_weights.png\n")
  
} else {
  cat("\nNo successful models to summarize.\n")
}

# ============================================================================
# 8. Instructions for Perception-Production Comparison
# ============================================================================

cat("\n=== Next Steps: Perception-Production Comparison ===\n")
cat("1. Load perception coefficients (from your previous analysis)\n")
cat("2. Merge with production coefficients by participant and contrast\n")
cat("3. Calculate correlations:\n")
cat("   - Perception VOT coef vs Production VOT coef\n")
cat("   - Perception F0 coef vs Production F0 coef\n")
cat("4. Create scatter plots showing alignment\n")
cat("\nExample code:\n")
cat('  perc <- read_csv("path/to/perception_coefficients.csv")\n')
cat('  prod <- read_csv("output/production_lda_coefficients_wide.csv")\n')
cat('  merged <- inner_join(perc, prod, by = c("participant", "contrast"))\n')
cat('  cor.test(merged$perc_vot_coef, merged$prod_vot_coef)\n')
cat(rep("=", 70), "\n", sep = "")
