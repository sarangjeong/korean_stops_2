# Participant 703 Individual Analysis
# Quick check of data and model for participant 703

library(tidyverse)
library(brms)

# Set working directory to script location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# ============================================================================
# Load Data
# ============================================================================

df <- read_csv("output/production_cleaned_data.csv")

# Check which normalized VOT column exists
if ("scaled_vot" %in% colnames(df)) {
  df$normed_vot <- df$scaled_vot
} else if ("scaled_vot_by_rest_duration" %in% colnames(df)) {
  df$normed_vot <- df$scaled_vot_by_rest_duration
}

# Ensure factors are properly set
df <- df %>%
  mutate(
    phonation = factor(phonation, levels = c("lenis", "tense", "aspirated")),
    poa = factor(poa, levels = c("labial", "coronal", "dorsal")),
    prolific_id = as.character(prolific_id)
  ) %>%
  filter(!is.na(normed_vot), !is.na(normed_f0), !is.na(phonation), !is.na(poa))

# ============================================================================
# Extract Participant 703 Data
# ============================================================================

p703 <- df %>% filter(prolific_id == "703")

cat("\n=== Participant 703 Data Summary ===\n")
cat("N observations:", nrow(p703), "\n")
cat("\nPhonation types:\n")
print(table(p703$phonation))
cat("\nPOA types:\n")
print(table(p703$poa))

cat("\n=== Raw Data Statistics (before scaling) ===\n")
cat("VOT - Mean:", mean(p703$normed_vot, na.rm = TRUE), "SD:", sd(p703$normed_vot, na.rm = TRUE), "\n")
cat("VOT - Range:", min(p703$normed_vot, na.rm = TRUE), "to", max(p703$normed_vot, na.rm = TRUE), "\n")
cat("F0 - Mean:", mean(p703$normed_f0, na.rm = TRUE), "SD:", sd(p703$normed_f0, na.rm = TRUE), "\n")
cat("F0 - Range:", min(p703$normed_f0, na.rm = TRUE), "to", max(p703$normed_f0, na.rm = TRUE), "\n")

# Show first few rows
cat("\n=== First 10 rows of data ===\n")
print(p703 %>% select(prolific_id, phonation, poa, normed_vot, normed_f0) %>% head(10))

# ============================================================================
# Visualize Data
# ============================================================================

library(ggplot2)

p1 <- ggplot(p703, aes(x = normed_vot, y = normed_f0, color = phonation, shape = poa)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(
    title = "Participant 703: VOT vs F0 by Phonation Type and POA",
    x = "Normalized VOT",
    y = "Normalized F0",
    color = "Phonation",
    shape = "POA"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 12))

print(p1)

# Save plot
ggsave("output/participant_703_data_plot.png", p1, width = 10, height = 6, dpi = 300)
cat("\nPlot saved: output/participant_703_data_plot.png\n")

# ============================================================================
# Standardize within participant
# ============================================================================

p703_scaled <- p703 %>%
  mutate(
    normed_vot = scale(normed_vot)[,1],
    normed_f0 = scale(normed_f0)[,1]
  )

cat("\n=== Scaled Data Statistics (after scaling) ===\n")
cat("VOT - Mean:", mean(p703_scaled$normed_vot, na.rm = TRUE), "SD:", sd(p703_scaled$normed_vot, na.rm = TRUE), "\n")
cat("F0 - Mean:", mean(p703_scaled$normed_f0, na.rm = TRUE), "SD:", sd(p703_scaled$normed_f0, na.rm = TRUE), "\n")

# ============================================================================
# Fit Model for Participant 703
# ============================================================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("Fitting Bayesian model for Participant 703...\n")
cat(rep("=", 70), "\n", sep = "")

# Set brms options
options(mc.cores = parallel::detectCores())
n_chains <- 4
n_iter <- 2000
n_warmup <- 1000

# Fit model
model_703 <- brm(
  phonation ~ normed_vot + normed_f0 + poa,
  data = p703_scaled,
  family = categorical(link = "logit", refcat = "lenis"),
  chains = n_chains,
  iter = n_iter,
  warmup = n_warmup,
  seed = 123
)

cat("\n=== Model Summary ===\n")
print(summary(model_703))

# Extract fixed effects
fixed_effects <- fixef(model_703)

cat("\n=== Fixed Effects (Coefficients) ===\n")
print(fixed_effects)

# Save model
saveRDS(model_703, "output/participant_703_model.rds")
cat("\nModel saved: output/participant_703_model.rds\n")

cat("\n", rep("=", 70), "\n", sep = "")
cat("Analysis complete for Participant 703!\n")
cat(rep("=", 70), "\n", sep = "")
