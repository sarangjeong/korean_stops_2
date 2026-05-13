# ===== SET UP =====

# set working directory
this.dir <- tryCatch(
  dirname(normalizePath(sys.frame(1)$ofile)),
  error = function(e) {
    tryCatch(
      dirname(rstudioapi::getSourceEditorContext()$path),
      error = function(e2) getwd()
    )
  }
)
setwd(this.dir)

# load libraries
library(tidyverse)
library(lme4)
library(lmerTest)
library(broom.mixed)
library(patchwork)
library(pbkrtest)
library(broom.mixed)
library(dplyr)
library(ggeffects)
library(patchwork)
library(effects)
library(emmeans)
library(overlapping)

# ===== FLAGS =====
exclude_gyeongsang <- TRUE  # Set to FALSE to include Gyeongsang dialect speakers
gyeongsang_prolific_ids <- c(211, 621)  # Participant IDs to exclude

# All exported files (csv/png) are written here.
output_dir <- "./output"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Add suffix to filenames based on flags
suffix <- if (exclude_gyeongsang) "_no_gyeongsang" else ""
custom_out_file <- function(filename) {
  # Replace .csv, .rds, or .png extension with suffix + extension
  if (grepl("\\.(csv|rds|png)$", filename)) {
    filename <- sub("(\\.(csv|rds|png))$", paste0(suffix, "\\1"), filename)
  } else {
    filename <- paste0(filename, suffix)
  }
  file.path(output_dir, filename)
}

# 1) 깨끗하게 다시 로드
data <- read.csv("./output/production_preprocessed_data.csv", stringsAsFactors = FALSE)

# 2) 전처리
# 경상 방언 사용자 제외 (if flagged) - prolific_id 211, 621 제외
if (exclude_gyeongsang) {
  cat("Excluding Gyeongsang dialect speakers (prolific_id", paste(gyeongsang_prolific_ids, collapse=", "), ")...\n")
  cat("Before filtering: ", nrow(data), " rows\n")
  data <- data[!(data$prolific_id %in% gyeongsang_prolific_ids), ]
  cat("After filtering: ", nrow(data), " rows\n")
}

data$prolific_id <- as.factor(data$prolific_id)
data$phonation <- factor(data$phonation, levels = c("aspirated", "fortis", "lenis"))
data$phonation <- relevel(data$phonation, ref = "lenis")
data$gender <- factor(data$gender, levels = c("Female", "Male"), labels = c("female", "male"))
# Ensure rime is a factor with 'i' as reference level
if ('rime' %in% names(data)) {
  data$rime <- factor(data$rime)
  if ('i' %in% levels(data$rime)) data$rime <- relevel(data$rime, ref = 'i')
}


# 확인 및 NA 제거
sum(is.na(data$phonation))  # 0 이어야 함
data <- data[!is.na(data$vot), ]       # vot NA 제거
data <- data[!is.na(data$semitone), ] # semitone NA 제거

# 3) overlap용 벡터 생성
young_female_asp_vot <- with(data, vot[phonation == "aspirated" & gender == "female" & age < 40])
young_female_lenis_vot <- with(data, vot[phonation == "lenis" & gender == "female" & age < 40])

vot_list <- list(
  Aspirated = young_female_asp_vot,
  Lenis = young_female_lenis_vot
)

vot_ovl_asp_lenis_young_female <- overlap(vot_list)$OV

# Calculate overlap
old_female_asp_vot <- data$vot[data$phonation == "aspirated" & data$gender == "female" & data$age > 59]
old_female_lenis_vot <- data$vot[data$phonation == "lenis" & data$gender == "female" & data$age > 59]
vot_list <- list(
  Aspirated = old_female_asp_vot,
  Lenis = old_female_lenis_vot
)
vot_ovl_asp_lenis_old_female <- overlap(vot_list)$OV

# Compare
cat("Young Female Overlap:", vot_ovl_asp_lenis_young_female, "\n")
cat("Older Female Overlap:", vot_ovl_asp_lenis_old_female, "\n")

## OLD MALE VOT: ASP vs LENIS
old_male_asp_vot <- data$vot[data$phonation == "aspirated" & data$gender == "male" & data$age > 59]
old_male_lenis_vot <- data$vot[data$phonation == "lenis" & data$gender == "male" & data$age > 59]

# Create a named list
vot_list <- list(Aspirated = old_male_asp_vot, Lenis = old_male_lenis_vot)

# Calculate overlap
vot_ovl_asp_lenis_old_male <- overlap(vot_list)$OV

# Compare
cat("Young Female Overlap:", vot_ovl_asp_lenis_young_female, "\n")
cat("Older Male Overlap:", vot_ovl_asp_lenis_old_male, "\n")

## YOUNG MALE VOT: ASP vs LENIS
young_male_asp_vot <- data$vot[data$phonation == "aspirated" & data$gender == "male" & data$age < 40]
young_male_lenis_vot <- data$vot[data$phonation == "lenis" & data$gender == "male" & data$age < 40]

# Create a named list
vot_list <- list(Aspirated = young_male_asp_vot, Lenis = young_male_lenis_vot)

# Calculate overlap
vot_ovl_asp_lenis_young_male <- overlap(vot_list)$OV

# Compare
cat("Young Female Overlap:", vot_ovl_asp_lenis_young_female, "\n")
cat("Young Male Overlap:", vot_ovl_asp_lenis_young_male, "\n")

# == ASP vs LENIS SEMITONE ==

## YOUNG FEAMLE semitone: ASP vs LENIS
young_female_asp_semitone <- data$semitone[data$phonation == "aspirated" & data$gender == "female" & data$age < 40]
young_female_lenis_semitone <- data$semitone[data$phonation == "lenis" & data$gender == "female" & data$age < 40]

# Create a named list
semitone_list <- list(Aspirated = young_female_asp_semitone, Lenis = young_female_lenis_semitone)

# Calculate overlap
semitone_ovl_asp_lenis_young_female <- overlap(semitone_list)$OV

## OLD FEAMLE semitone: ASP vs LENIS
old_female_asp_semitone <- data$semitone[data$phonation == "aspirated" & data$gender == "female" & data$age > 59]
old_female_lenis_semitone <- data$semitone[data$phonation == "lenis" & data$gender == "female" & data$age > 59]

# Create a named list
semitone_list <- list(Aspirated = old_female_asp_semitone, Lenis = old_female_lenis_semitone)

# Calculate overlap
semitone_ovl_asp_lenis_old_female <- overlap(semitone_list)$OV

# Compare
cat("Young Female Overlap:", semitone_ovl_asp_lenis_young_female, "\n")
cat("Older Female Overlap:", semitone_ovl_asp_lenis_old_female, "\n")

## OLD MALE semitone: ASP vs LENIS
old_male_asp_semitone <- data$semitone[data$phonation == "aspirated" & data$gender == "male" & data$age > 59]
old_male_lenis_semitone <- data$semitone[data$phonation == "lenis" & data$gender == "male" & data$age > 59]

# Create a named list
semitone_list <- list(Aspirated = old_male_asp_semitone, Lenis = old_male_lenis_semitone)

# Calculate overlap
semitone_ovl_asp_lenis_old_male <- overlap(semitone_list)$OV

# Compare
cat("Young Female Overlap:", semitone_ovl_asp_lenis_young_female, "\n")
cat("Older Male Overlap:", semitone_ovl_asp_lenis_old_male, "\n")

## YOUNG MALE semitone: ASP vs LENIS
young_male_asp_semitone <- data$semitone[data$phonation == "aspirated" & data$gender == "male" & data$age < 40]
young_male_lenis_semitone <- data$semitone[data$phonation == "lenis" & data$gender == "male" & data$age < 40]

# Create a named list
semitone_list <- list(Aspirated = young_male_asp_semitone, Lenis = young_male_lenis_semitone)

# Calculate overlap
semitone_ovl_asp_lenis_young_male <- overlap(semitone_list)$OV

# Compare
cat("Young Female Overlap:", semitone_ovl_asp_lenis_young_female, "\n")
cat("Young Male Overlap:", semitone_ovl_asp_lenis_young_male, "\n")

# == ASP vs FORTIS SEMITONE ==

## YOUNG FEAMLE semitone: ASP vs fortis
young_female_asp_semitone <- data$semitone[data$phonation == "aspirated" & data$gender == "female" & data$age < 40]
young_female_fortis_semitone <- data$semitone[data$phonation == "fortis" & data$gender == "female" & data$age < 40]

# Create a named list
semitone_list <- list(Aspirated = young_female_asp_semitone, fortis = young_female_fortis_semitone)

# Calculate overlap
semitone_ovl_asp_fortis_young_female <- overlap(semitone_list)$OV

## OLD FEAMLE semitone: ASP vs fortis
old_female_asp_semitone <- data$semitone[data$phonation == "aspirated" & data$gender == "female" & data$age > 59]
old_female_fortis_semitone <- data$semitone[data$phonation == "fortis" & data$gender == "female" & data$age > 59]

# Create a named list
semitone_list <- list(Aspirated = old_female_asp_semitone, fortis = old_female_fortis_semitone)

# Calculate overlap
semitone_ovl_asp_fortis_old_female <- overlap(semitone_list)$OV

# Compare
cat("Young Female Overlap:", semitone_ovl_asp_fortis_young_female, "\n")
cat("Older Female Overlap:", semitone_ovl_asp_fortis_old_female, "\n")

## OLD MALE semitone: ASP vs fortis
old_male_asp_semitone <- data$semitone[data$phonation == "aspirated" & data$gender == "male" & data$age > 59]
old_male_fortis_semitone <- data$semitone[data$phonation == "fortis" & data$gender == "male" & data$age > 59]

# Create a named list
semitone_list <- list(Aspirated = old_male_asp_semitone, fortis = old_male_fortis_semitone)

# Calculate overlap
semitone_ovl_asp_fortis_old_male <- overlap(semitone_list)$OV

# Compare
cat("Young Female Overlap:", semitone_ovl_asp_fortis_young_female, "\n")
cat("Older Male Overlap:", semitone_ovl_asp_fortis_old_male, "\n")

## YOUNG MALE semitone: ASP vs fortis
young_male_asp_semitone <- data$semitone[data$phonation == "aspirated" & data$gender == "male" & data$age < 40]
young_male_fortis_semitone <- data$semitone[data$phonation == "fortis" & data$gender == "male" & data$age < 40]

# Create a named list
semitone_list <- list(Aspirated = young_male_asp_semitone, fortis = young_male_fortis_semitone)

# Calculate overlap
semitone_ovl_asp_fortis_young_male <- overlap(semitone_list)$OV

# Compare
cat("Young Female Overlap:", semitone_ovl_asp_fortis_young_female, "\n")
cat("Young Male Overlap:", semitone_ovl_asp_fortis_young_male, "\n")





# ===== REGRESSION MODELS =====

# Q Do males have overall longer VOT than females across stop categories?
m_vot_gender <- lmer(
  log_vot ~ gender * phonation + 
    (1 | prolific_id) + (1 | item),
  data = data
)

emmeans(m_vot_gender, specs = pairwise ~ gender | phonation, type = "response")

# === VOT MODELS ===

# 1. Compare random slope vs no
m_vot_no_random_slope  <- lmer(
  log_vot ~ phonation * gender * normed_age + poa + normed_word_duration +
    (1 | prolific_id) + (1 | item),
  data = data
)

m_vot_yes_random_slope <- lmer(
  log_vot ~ phonation * gender * normed_age + poa + normed_word_duration +
    (1 + phonation | prolific_id) + (1 | item),
  data = data
)

anova(m_vot_no_random_slope, m_vot_yes_random_slope)
# TAKEWAY: use the one WITH random slope

# 2. Compare by-item random effect vs no
m_vot_yes_item_random_effect <- lmer(
  log_vot ~ phonation * gender * normed_age + poa + normed_word_duration +
    (1 + phonation | prolific_id) + (1 | item),
  data = data
)
summary(m_vot_yes_item_random_effect)

m_vot_no_item_random_effect <- lmer(
  log_vot ~ phonation * gender * normed_age + poa + normed_word_duration +
    (1 + phonation | prolific_id),
  data = data
)

anova(m_vot_yes_item_random_effect, m_vot_no_item_random_effect)
# TAKEAWAY: use the one WITH by-item random effect
# Note: the smaller model did not converge when y = raw VOT. It did converge when y = log_vot.

# 3. Compare all interactions vs fewer interactions
m_vot_with_all_interactions <- lmer(
  log_vot ~ phonation + gender + normed_age 
  + phonation:gender + phonation:normed_age + gender:normed_age 
  + phonation:gender:normed_age 
  + poa  + normed_word_duration
  + (1 + phonation | prolific_id) + (1 | item),
  data = data
)

m_vot_no_three_way_interaction <- lmer(
  log_vot ~ phonation + gender + normed_age 
  + phonation:gender + phonation:normed_age + gender:normed_age 
#  + phonation:gender:normed_age 
  + poa  + normed_word_duration
  + (1 + phonation | prolific_id) + (1 | item),
  data = data
)

anova(m_vot_no_three_way_interaction, m_vot_with_all_interactions)
# The larger model is not significantly better than the smaller model.
# TAKEAWAY: use the one WITHOUT three-way interaction

m_vot_no_gender_age_interaction <- lmer(
  log_vot ~ phonation + gender + normed_age 
  + phonation:gender + phonation:normed_age # + gender:normed_age 
  #  + phonation:gender:normed_age 
  + poa  + normed_word_duration
  + (1 + phonation | prolific_id) + (1 | item),
  data = data
)

anova(m_vot_no_three_way_interaction, m_vot_no_gender_age_interaction)
# The larger model is not significantly better than the smaller model.
# TAKEAWAY: use the one WITHOUT gender:age

m_vot_no_phon_gender_interaction <- lmer(
  log_vot ~ phonation + gender + normed_age + 
    phonation:normed_age +
    poa + normed_word_duration +
    (1 + phonation | prolific_id) + (1 | item),
  data = data
)

anova(m_vot_no_phon_gender_interaction, m_vot_no_gender_age_interaction)
# The larger model IS better than the smaller model.
# TAKEAWAY: keep phonation:gender!

m_vot_no_phon_age_interaction <- lmer(
  log_vot ~ phonation + gender + normed_age + 
    phonation:gender +
    poa + normed_word_duration +
    (1 + phonation | prolific_id) + (1 | item),
  data = data
)

anova(m_vot_no_phon_age_interaction, m_vot_no_gender_age_interaction)
# The larger model is MARGINALLY better than the smaller model. (y=vot)
# NOTE: after changing y to log_vot, the larger model (no_gender_age) is SIGNIFICANTLY BETTER than the smaller model (no_phon_age)!!
# TAKEAWAY: keep phonation:age!

# so far the best: m_vot_no_gender_age_interaction

# Frequency as predictor
m_vot_lexical_freq <- lmer(
  log_vot ~ phonation + gender + normed_age 
  + phonation:gender + phonation:normed_age
  + poa  + normed_word_duration + z_log_morpheme_freq + rime
  + (1 + phonation | prolific_id) + (1 | item),
  data = data,
  REML = FALSE # for AIC, BIC comparisons
)
m_vot_syllable_freq <- lmer(
  log_vot ~ phonation + gender + normed_age 
  + phonation:gender + phonation:normed_age
  + poa  + normed_word_duration + z_log_syllable_freq
  + (1 + phonation | prolific_id) + (1 | item),
  data = data,
  REML = FALSE # for AIC, BIC comparisons
)
AIC(m_vot_lexical_freq, m_vot_syllable_freq) # lexical_freq is better
BIC(m_vot_lexical_freq, m_vot_syllable_freq) # lexical_freq is better

anova(m_vot_lexical_freq, m_vot_no_gender_age_interaction)
# larger model is better

# WINNER: m_vot_lexical_freq
m_vot <- m_vot_lexical_freq
summary(m_vot)

# phon:age is either insignificant or only marginally significant
# so let's try removing it
m_vot_lexical_freq_no_phon_age_interaction  <- lmer(
  log_vot ~ phonation + gender + normed_age 
  + phonation:gender
  + poa + normed_word_duration + z_log_morpheme_freq
  + (1 + phonation | prolific_id) + (1 | item),
  data = data,
  control = lmerControl(
    optimizer = "bobyqa",           # 기본값
    optCtrl = list(maxfun = 2e5)    # 반복 횟수 증가 for convergence
  )
)

anova(m_vot_lexical_freq_no_phon_age_interaction, m_vot_lexical_freq)
# larger model is marginally better
# NOTE: when y = log_vot, larger model is SIGNIFICANTLY BETTER
# but phonation:age is theoretically important
# so we keep the larger model

# WINNER is still m_vot_lexical_freq
m_vot <- m_vot_lexical_freq
summary(m_vot)

# phonation:freq? (NOT USED)
m_vot_phon_freq_interaction <- lmer(
  log_vot ~ phonation + gender + normed_age 
  + phonation:gender + phonation:normed_age
  + poa  + normed_word_duration + z_log_morpheme_freq + rime
  + z_log_morpheme_freq:phonation
  + (1 + phonation | prolific_id) + (1 | item),
  data = data
)
summary(m_vot_phon_freq_interaction)
anova(m_vot_lexical_freq, m_vot_phon_freq_interaction)
# TAKEAWAY: phonation:freq makes the model worse

# EXPORT RESULT TO CSV
# 1. 모델 결과를 데이터 프레임으로 변환 (Fixed effects만 추출)
m_vot_result <- tidy(m_vot, effects = "fixed")

# 2. p-value를 기준으로 별표(significance) 추가하는 함수 적용
m_vot_result <- m_vot_result %>%
  dplyr::mutate(
    # p-value가 0.001보다 작으면 "<.001"로, 아니면 소수점 3자리까지 표시
    p_simple = ifelse(p.value < 0.001, "<.001", round(p.value, 3)),
    significance = dplyr::case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01  ~ "**",
    p.value < 0.05  ~ "*",
    p.value < 0.1   ~ ".",
    TRUE            ~ ""
  ))

# 3. CSV로 저장
write.csv(m_vot_result, custom_out_file("vot_lmer_results_with_p_values.csv"), row.names = FALSE)

# === Semitone MODELS ===

m_semitone <- lmer(
  semitone ~ phonation * gender * normed_age + poa  + normed_word_duration +
    (1 + phonation | prolific_id) + (1 | item),
  data = data
)

m_semitone_no_poa <- lmer(
  semitone ~ phonation * gender * normed_age + normed_word_duration +
    (1 + phonation | prolific_id) + (1 | item),
  data = data
)

anova(m_semitone_no_poa, m_semitone)
# TAKEAWAY: DON'T include POA

m_semitone_no_poa_no_dur <- lmer(
  semitone ~ phonation + gender + normed_age
  + phonation:gender + phonation:normed_age + gender:normed_age
  + phonation:gender:normed_age 
  + rime
  + (1 + phonation | prolific_id) + (1 | item),
  data = data
)

anova(m_semitone_no_poa, m_semitone_no_poa_no_dur)
# TAKEAWAY: DON'T include WORD_DURATION

m_semitone_no_three_way_interaction <- lmer(
  semitone ~ phonation + gender + normed_age
  + phonation:gender + phonation:normed_age + gender:normed_age 
  + (1 + phonation | prolific_id) + (1 | item),
  data = data
)

anova(m_semitone_no_three_way_interaction, m_semitone_no_poa_no_dur)
# Larger model is significantly better than smaller model.
# TAKEAWAY: keep three-way interaction

# Q. But the three-way interactions were all insignificant!
# A. test their overall significance using type 3 anova

library(car)
Anova(m_semitone_no_poa_no_dur, type = 3)
# TAKEAWAY: three-way interaction IS significant

# WINNER: m_semitone_no_poa_no_dur
m_semitone <- m_semitone_no_poa_no_dur
summary(m_semitone)

# FREQUENCY (NOT USED) 
m_semitone_lexical_freq <- lmer(
  semitone ~ phonation + gender + normed_age
  + phonation:gender + phonation:normed_age + gender:normed_age
  + phonation:gender:normed_age
  + z_log_morpheme_freq
  + (1 + phonation | prolific_id) + (1 | item),
  data = data
)

m_semitone_syllable_freq <- lmer(
  semitone ~ phonation + gender + normed_age
  + phonation:gender + phonation:normed_age + gender:normed_age
  + phonation:gender:normed_age
  + z_log_syllable_freq
  + (1 + phonation | prolific_id) + (1 | item),
  data = data,
  control = lmerControl(
    optimizer = "bobyqa",           # 기본값
    optCtrl = list(maxfun = 2e5)    # 반복 횟수 증가
  )
)

anova(m_semitone, m_semitone_lexical_freq)
anova(m_semitone, m_semitone_syllable_freq)

# TAKEAWAY: BOTH FREQ are bad predictors

# ===== Predictions =====

# m_vot is fit on log_vot, so convert fitted values back to ms for plotting.
data$vot_fitted <- exp(fitted(m_vot))
data$semitone_fitted <- fitted(m_semitone)

# 나이 평균/표준편차 계산
age_mean <- mean(data$age, na.rm = TRUE)
age_sd <- sd(data$age, na.rm = TRUE)

# 시나리오 생성 (OUTDATED) /////
# scenarios <- expand.grid(
#   phonation = c("lenis", "aspirated", "fortis"),
#   gender = c("female", "male"),
#   normed_age = c(-1, 0, 1),  # -1SD, mean, +1SD
#   poa = "coronal",
#   normed_word_duration = 0,
#   z_log_morpheme_freq = 0
# )

# 시나리오 생성 (실제 나이 사용)
scenarios <- expand.grid(
  phonation = c("aspirated", "fortis", "lenis"),
  gender = c("female", "male"),
  age = 20:72,  # 20세부터 72세까지 모든 정수
  poa = "coronal",
  normed_word_duration = 0,
  z_log_morpheme_freq = 0
) %>%
  dplyr::mutate(
    normed_age = (age - age_mean) / age_sd,  # normed_age 계산
    phonation = factor(phonation, levels = c("lenis", "aspirated", "fortis"))
  )

# 예측
# m_vot predictions are on log scale; back-transform to VOT ms.
scenarios$predicted_vot <- exp(predict(m_vot, newdata = scenarios, re.form = NA))
scenarios$predicted_semitone <- predict(m_semitone, newdata = scenarios, re.form = NA)

scenarios
write.csv(scenarios, custom_out_file("vot_semitone_predictions.csv"), row.names = FALSE)

# 또는 더 깔끔하게 정리해서 저장
scenarios_clean <- scenarios %>%
  dplyr::mutate(
    predicted_vot = round(predicted_vot, 2),
    predicted_semitone = round(predicted_semitone, 2)
  ) %>%
  dplyr::arrange(gender, phonation, normed_age)

write.csv(scenarios_clean, custom_out_file("vot_semitone_predictions_clean.csv"), row.names = FALSE)

# ===== Lenis stops만 =====

# 1. Lenis만 필터링
data_lenis <- data %>%
  dplyr::filter(phonation == "lenis")

m_vot_lenis <- lmer(
  vot ~ gender:normed_age
  + poa  + normed_word_duration + z_log_morpheme_freq + rime
  + (1 | prolific_id) + (1 | item),
  data = data_lenis
)
summary(m_vot_lenis)
# freq is insignificant

m_vot_lenis_no_freq <- lmer(
  vot ~ gender:normed_age
  + poa  + normed_word_duration 
  + (1 | prolific_id) + (1 | item),
  data = data_lenis
)

anova(m_vot_lenis_no_freq, m_vot_lenis)
# For lenis data, having frequency is not better than not having it
# TAKEAWAY: frequency doesn't matter in lenis data!

# ===== Aspirated stops만 =====

# 1. Asp만 필터링
data_asp <- data %>%
  dplyr::filter(phonation == "aspirated")

m_vot_asp <- lmer(
  vot ~ gender:normed_age
  + poa  + normed_word_duration + z_log_morpheme_freq + rime
  + (1 | prolific_id) + (1 | item),
  data = data_asp
)
summary(m_vot_asp)
# freq is insignificant

m_vot_asp_no_freq <- lmer(
  vot ~ gender:normed_age
  + poa  + normed_word_duration 
  + (1 | prolific_id) + (1 | item),
  data = data_asp
)

anova(m_vot_asp_no_freq, m_vot_asp)
# For asp data, having frequency is only marginally better than not having it
# TAKEAWAY: frequency doesn't really matter in asp data!

# ===== Fortis stops만 =====

# 1. fortis만 필터링
data_fortis <- data %>%
  dplyr::filter(phonation == "fortis")

m_vot_fortis <- lmer(
  vot ~ gender:normed_age
  + poa  + normed_word_duration + z_log_morpheme_freq + rime
  + (1 | prolific_id) + (1 | item),
  data = data_fortis
)
summary(m_vot_fortis)
# freq is insignificant

m_vot_fortis_no_freq <- lmer(
  vot ~ gender:normed_age
  + poa  + normed_word_duration 
  + (1 | prolific_id) + (1 | item),
  data = data_fortis
)

anova(m_vot_fortis_no_freq, m_vot_fortis)
# For fortis data, having frequency IS better than not having it
# TAKEAWAY: for fortis-initial words, more frequent words have longer VOT

# ===== SAVE RESULTS =====
# === Fixed effects 표 ===
vot_fixed_effects <- tidy(m_vot, effects = "fixed")

# CSV 저장
write.csv(vot_fixed_effects, custom_out_file("vot_model_fixed_effects.csv"), row.names = FALSE)

# 예쁘게 정리
vot_fixed_effects_clean <- vot_fixed_effects %>%
  dplyr::mutate(
    estimate = round(estimate, 3),
    std.error = round(std.error, 3),
    statistic = round(statistic, 3),
    p.value = round(p.value, 4),
    sig = dplyr::case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      p.value < 0.1 ~ ".",
      TRUE ~ ""
    )
  ) %>%
  dplyr::select(term, estimate, std.error, statistic, p.value, sig)

write.csv(vot_fixed_effects_clean, custom_out_file("vot_model_fixed_effects_clean.csv"), row.names = FALSE)

# === Random effects 표 ===
# VarCorr를 DataFrame으로
vot_random_effects <- as.data.frame(VarCorr(m_vot))

# 열 이름 정리
vot_random_effects <- vot_random_effects %>%
  dplyr::rename(
    groups = grp,
    variance = vcov
  ) %>%
  dplyr::mutate(
    std_dev = sdcor,
    variance = round(variance, 3),
    std_dev = round(std_dev, 3)
  ) %>%
  dplyr::select(groups, var1, var2, variance, std_dev) %>%
  dplyr::rename(
    term = var1,
    term2 = var2
  )

write.csv(vot_random_effects, custom_out_file("vot_model_random_effects.csv"), row.names = FALSE)



# TODO: remove word duration from VOT model
# TODO: DISCUSS W INSEONG - think about reference level and coding method (dummy, difference, helmert, etc)
# TODO: DISCUSS W INSEONG - regression without interaction? maybe predict VOT & semitone using stop_category only,
#       and then predict the coefficients of each stop_category using gender & age?

# === Fixed effects 표 ===
semitone_fixed_effects <- tidy(m_semitone, effects = "fixed")

# CSV 저장
write.csv(semitone_fixed_effects, custom_out_file("semitone_model_fixed_effects.csv"), row.names = FALSE)

# 예쁘게 정리
semitone_fixed_effects_clean <- semitone_fixed_effects %>%
  dplyr::mutate(
    estimate = round(estimate, 3),
    std.error = round(std.error, 3),
    statistic = round(statistic, 3),
    p.value = round(p.value, 4),
    sig = dplyr::case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      p.value < 0.1 ~ ".",
      TRUE ~ ""
    )
  ) %>%
  dplyr::select(term, estimate, std.error, statistic, p.value, sig)

write.csv(semitone_fixed_effects_clean, custom_out_file("semitone_model_fixed_effects_clean.csv"), row.names = FALSE)

# === Random effects 표 ===
# VarCorr를 DataFrame으로
semitone_random_effects <- as.data.frame(VarCorr(m_semitone))

# 열 이름 정리
semitone_random_effects <- semitone_random_effects %>%
  dplyr::rename(
    groups = grp,
    variance = vcov
  ) %>%
  dplyr::mutate(
    std_dev = sdcor,
    variance = round(variance, 3),
    std_dev = round(std_dev, 3)
  ) %>%
  dplyr::select(groups, var1, var2, variance, std_dev) %>%
  dplyr::rename(
    term = var1,
    term2 = var2
  )

write.csv(semitone_random_effects, custom_out_file("semitone_model_random_effects.csv"), row.names = FALSE)

