# ===== SET UP =====

# set working directory
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# Run modeling + CSV export from shared script first.
source("../shared/regression.R")
setwd(this.dir)

# Plot outputs are written under production/final_viz/output.
output_dir <- "./output"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
# custom_out_file function is already defined in regression.R with suffix support

library(ggh4x)
library(patchwork)

# ===== Visualizations =====

# Convert model outputs to plotting tables.
data$vot_fitted <- exp(fitted(m_vot))
data$semitone_fitted <- fitted(m_semitone)

scenarios_long <- scenarios %>%
  pivot_longer(cols = c(predicted_vot, predicted_semitone),
               names_to = "measure",
               values_to = "value") %>%
  mutate(phonation = factor(phonation,
                            levels = c("aspirated", "fortis", "lenis"))) %>%
  mutate(measure = factor(measure,
                          levels = c("predicted_vot", "predicted_semitone"),
                          labels = c("Predicted VOT (ms)", "Predicted Semitone (ST)")))

# Predicted trajectories.
ggplot(scenarios_long, aes(x = age, y = value,
                           color = phonation, linetype = gender)) +
  geom_line(linewidth = 0.5) +
  facet_wrap(~ measure, ncol = 2, scales = "free_y") +
  facetted_pos_scales(
    y = list(
      `Predicted VOT (ms)` = scale_y_continuous(limits = c(0, 100)),
      `Predicted Semitone (ST)` = scale_y_continuous()
    )
  ) +
  labs(
    title = "Predicted VOT and Semitone by Phonation, Gender, and Age",
    x = "Age (years)",
    y = "",
    color = "Stop Category",
    linetype = "Gender"
  ) +
  scale_color_manual(values = c("aspirated" = "#2ca02c",
                                "fortis" = "#ff7f0e",
                                "lenis" = "#1f77b4")) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA))

ggsave(custom_out_file("predicted_vot_semitone.png"), width = 5, height = 5, dpi = 300)

# Observed trajectories.
data_long <- data %>%
  pivot_longer(cols = c(vot, semitone),
               names_to = "measure",
               values_to = "value") %>%
  mutate(measure = factor(measure,
                          levels = c("vot", "semitone"),
                          labels = c("VOT (ms)", "Semitone (ST)")))

ggplot(data_long, aes(x = age, y = value,
                      color = phonation, linetype = gender)) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.5) +
  facet_wrap(~ measure, ncol = 2, scales = "free_y") +
  facetted_pos_scales(
    y = list(
      `VOT (ms)` = scale_y_continuous(limits = c(0, 100)),
      `Semitone (ST)` = scale_y_continuous()
    )
  ) +
  labs(
    title = "VOT and Semitone by Phonation, Gender, and Age",
    x = "Age (years)",
    y = "",
    color = "Stop Category",
    linetype = "Gender"
  ) +
  scale_color_manual(values = c("lenis" = "#1f77b4",
                                "aspirated" = "#2ca02c",
                                "fortis" = "#ff7f0e")) +
  guides(linetype = guide_legend(override.aes = list(color = "black"))) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)) +
  xlim(20, 72)

ggsave(custom_out_file("observed_vot_semitone.png"), width = 5, height = 5, dpi = 300)

# ===== Observed vs Predicted Lines =====

data_vot <- data_long %>% filter(measure == "VOT (ms)")
data_semitone <- data_long %>% filter(measure == "Semitone (ST)")

scenarios_vot <- scenarios_long %>% filter(measure == "Predicted VOT (ms)")
scenarios_semitone <- scenarios_long %>% filter(measure == "Predicted Semitone (ST)")

p1 <- ggplot(data_vot, aes(x = age, y = value, color = phonation)) +
  geom_smooth(aes(linetype = gender), method = "lm", se = FALSE, linewidth = 0.5) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(x = "", y = "Mean VOT (ms)", color = "Stop Category", linetype = "Gender") +
  scale_color_manual(values = c("lenis" = "#1f77b4",
                                "aspirated" = "#2ca02c",
                                "fortis" = "#ff7f0e")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlim(20, 72)

p2 <- ggplot(scenarios_vot, aes(x = age, y = value,
                                color = phonation, linetype = gender)) +
  geom_line(linewidth = 0.5) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(x = "", y = "VOT (ms)", color = "Stop Category", linetype = "Gender") +
  scale_color_manual(values = c("lenis" = "#1f77b4",
                                "aspirated" = "#2ca02c",
                                "fortis" = "#ff7f0e")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlim(20, 72)

p3 <- ggplot(data_semitone, aes(x = age, y = value, color = phonation)) +
  geom_smooth(aes(linetype = gender), method = "lm", se = FALSE, linewidth = 0.5) +
  labs(x = "Age", y = "Mean Semitone (ST)", color = "Stop Category", linetype = "Gender") +
  scale_color_manual(values = c("lenis" = "#1f77b4",
                                "aspirated" = "#2ca02c",
                                "fortis" = "#ff7f0e")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlim(20, 72)

p4 <- ggplot(scenarios_semitone, aes(x = age, y = value,
                               color = phonation, linetype = gender)) +
  geom_line(linewidth = 0.5) +
  labs(x = "Age", y = "Semitone (ST)", color = "Stop Category", linetype = "Gender") +
  scale_color_manual(values = c("lenis" = "#1f77b4",
                                "aspirated" = "#2ca02c",
                                "fortis" = "#ff7f0e")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlim(20, 72)

p1 / p3 +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Observed",
    theme = theme(plot.title = element_text(hjust = 0.3))
  ) &
  guides(linetype = guide_legend(override.aes = list(color = "black", linewidth = 0.5)))

ggsave(custom_out_file("vot_semitone_observed_lines.png"), width = 3, height = 5, dpi = 300, bg = "white")

p2 / p4 +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Predicted",
    theme = theme(plot.title = element_text(hjust = 0.3))
  ) &
  guides(linetype = guide_legend(override.aes = list(color = "black", linewidth = 0.5)))

ggsave(custom_out_file("vot_semitone_predicted_lines.png"), width = 3, height = 5, dpi = 300, bg = "white")

# ===== Model Predictions With CI =====
pred_vot <- ggpredict(m_vot,
                      terms = c("normed_age [-2:2, by=0.1]",
                                "phonation",
                                "gender"))

pred_vot_df <- as.data.frame(pred_vot) %>%
  mutate(
    predicted = exp(predicted),
    conf.low = exp(conf.low),
    conf.high = exp(conf.high),
    group = factor(group, levels = c("lenis", "aspirated", "fortis")),
    facet = factor(facet, levels = c("female", "male"))
  )

cat("\n===== VOT Predictions Debug =====\n")
cat("pred_vot_df dimensions:", nrow(pred_vot_df), "x", ncol(pred_vot_df), "\n")
cat("pred_vot_df columns:", paste(names(pred_vot_df), collapse = ", "), "\n")
print(table(pred_vot_df$facet, pred_vot_df$group))

p1 <- ggplot(pred_vot_df, aes(x = x, y = predicted, color = group, fill = group)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, group = interaction(group, facet)),
              alpha = 0.15, color = NA) +
  geom_line(aes(group = interaction(group, facet)), linewidth = 0.5) +
  facet_wrap(~ facet, nrow = 1) +
  scale_x_continuous(breaks = seq(-2, 2, by = 1), limits = c(-2, 2)) +
  scale_color_manual(
    name = "Stop Category",
    values = c("lenis" = "#1f77b4",
               "aspirated" = "#2ca02c",
               "fortis" = "#ff7f0e")
  ) +
  scale_fill_manual(
    name = "Stop Category",
    values = c("lenis" = "#1f77b4",
               "aspirated" = "#2ca02c",
               "fortis" = "#ff7f0e")
  ) +
  labs(title = "VOT", x = "Age (SD from mean)", y = "VOT (ms)") +
  theme_minimal()

pred_semitone <- ggpredict(m_semitone,
                     terms = c("normed_age [-2:2, by=0.1]",
                               "phonation",
                               "gender"))

pred_semitone_df <- as.data.frame(pred_semitone) %>%
  mutate(
    group = factor(group, levels = c("lenis", "aspirated", "fortis")),
    facet = factor(facet, levels = c("female", "male"))
  )

cat("\n===== Semitone Predictions Debug =====\n")
cat("pred_semitone_df dimensions:", nrow(pred_semitone_df), "x", ncol(pred_semitone_df), "\n")
cat("pred_semitone_df columns:", paste(names(pred_semitone_df), collapse = ", "), "\n")
print(table(pred_semitone_df$facet, pred_semitone_df$group))

p2 <- ggplot(pred_semitone_df, aes(x = x, y = predicted, color = group, fill = group)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, group = interaction(group, facet)),
              alpha = 0.15, color = NA) +
  geom_line(aes(group = interaction(group, facet)), linewidth = 0.5) +
  facet_wrap(~ facet, nrow = 1) +
  scale_x_continuous(breaks = seq(-2, 2, by = 1), limits = c(-2, 2)) +
  scale_color_manual(
    name = "Stop Category",
    values = c("lenis" = "#1f77b4",
               "aspirated" = "#2ca02c",
               "fortis" = "#ff7f0e")
  ) +
  scale_fill_manual(
    name = "Stop Category",
    values = c("lenis" = "#1f77b4",
               "aspirated" = "#2ca02c",
               "fortis" = "#ff7f0e")
  ) +
  labs(title = "Semitone", x = "Age (SD from mean)", y = "Semitone (ST)") +
  theme_minimal()

p1 + p2 +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Model Predictions with 95% CI",
    theme = theme(plot.title = element_text(hjust = 0.5))
  ) &
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5)) &
  guides(
    fill = guide_legend(
      override.aes = list(
        color = c("lenis" = "#1f77b4",
                  "aspirated" = "#2ca02c",
                  "fortis" = "#ff7f0e")
      )
    ),
    color = guide_legend(
      override.aes = list(
        fill = c("lenis" = "#1f77b4",
                 "aspirated" = "#2ca02c",
                 "fortis" = "#ff7f0e")
      )
    )
  )

ggsave(custom_out_file("observed_vs_predicted_vot_semitone.png"),
       width = 12, height = 6, dpi = 300, bg = "white")

# ===== Observed vs Predicted Scatter =====

ggplot(data, aes(x = vot_fitted, y = vot, color = phonation)) +
  geom_point(alpha = 0.2, size = 1) +
  geom_abline(slope = 1, intercept = 0,
              color = "black", linetype = "dashed", linewidth = 1) +
  scale_color_manual(values = c("lenis" = "#E69F00",
                                "aspirated" = "#56B4E9",
                                "fortis" = "#009E73")) +
  coord_equal(xlim = c(0, 200), ylim = c(0, 200)) +
  labs(
    title = "Observed vs Predicted VOT",
    subtitle = "Dashed line = perfect prediction",
    x = "Predicted VOT (ms)",
    y = "Observed VOT (ms)",
    color = "Phonation"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(custom_out_file("vot_observed_vs_predicted.png"), width = 5, height = 5, dpi = 300)

p1 <- ggplot(data, aes(x = vot_fitted, y = vot, color = phonation)) +
  geom_point(alpha = 0.2, size = 1) +
  geom_abline(slope = 1, intercept = 0,
              color = "black", linetype = "dashed", linewidth = 1) +
  scale_color_manual(values = c("lenis" = "#1f77b4",
                                "aspirated" = "#2ca02c",
                                "fortis" = "#ff7f0e")) +
  coord_equal(xlim = c(0, 200), ylim = c(0, 200)) +
  labs(
    title = "VOT",
    x = "Predicted VOT (ms)",
    y = "Observed VOT (ms)",
    color = "Phonation"
  ) +
  theme_minimal()

p2 <- ggplot(data, aes(x = semitone_fitted, y = semitone, color = phonation)) +
  geom_point(alpha = 0.2, size = 1) +
  geom_abline(slope = 1, intercept = 0,
              color = "black", linetype = "dashed", linewidth = 1) +
  scale_color_manual(values = c("lenis" = "#1f77b4",
                                "aspirated" = "#2ca02c",
                                "fortis" = "#ff7f0e")) +
  coord_equal() +
  labs(
    title = "Semitone",
    x = "Predicted Semitone (ST)",
    y = "Observed Semitone (ST)",
    color = "Phonation"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

p1 + p2 +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Observed vs Predicted VOT and Semitone",
    subtitle = "Dashed line = perfect prediction"
  ) &
  theme(legend.position = "bottom")

ggsave(custom_out_file("observed_vs_predicted_vot_semitone_scatter.png"),
       width = 12, height = 6, dpi = 300, bg = "white")

# ===== Visualize Models =====

m_vot_effects <- allEffects(m_vot,
                            multiline = TRUE,
                            lines = list(col = c("#1f77b4", "#2ca02c", "#ff7f0e")))

plot(m_vot_effects)
names(m_vot_effects)
emmip(m_vot, gender ~ normed_age | phonation,
      cov.reduce = range)

emmip(m_vot, phonation ~ gender,
      cov.reduce = range)

emmip(m_vot, phonation ~ normed_age,
      cov.reduce = range)

emmip(m_vot, ~ poa,
      cov.reduce = range)

emmip(m_vot, ~ normed_word_duration,
      cov.reduce = range)

emmip(m_vot, ~ z_log_morpheme_freq,
      cov.reduce = range)

plot(allEffects(m_semitone),
     multiline = TRUE)

emmip(m_semitone, gender ~ normed_age | phonation,
      cov.reduce = range)

emmip(m_semitone, phonation ~ gender,
      cov.reduce = range)

# meaningless bc of big semitone diff btwn female & male
emmip(m_semitone, phonation ~ normed_age,
      cov.reduce = range)

