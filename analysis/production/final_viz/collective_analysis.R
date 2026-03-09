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
out_file <- function(filename) file.path(output_dir, filename)

library(ggh4x)
library(patchwork)

# ===== Visualizations =====

# Convert model outputs to plotting tables.
data$vot_fitted <- exp(fitted(m_vot))
data$f0_fitted <- fitted(m_f0)

scenarios_long <- scenarios %>%
  pivot_longer(cols = c(predicted_vot, predicted_f0),
               names_to = "measure",
               values_to = "value") %>%
  mutate(phonation = factor(phonation,
                            levels = c("aspirated", "fortis", "lenis"))) %>%
  mutate(measure = factor(measure,
                          levels = c("predicted_vot", "predicted_f0"),
                          labels = c("Predicted VOT (ms)", "Predicted F0 (Hz)")))

# Predicted trajectories.
ggplot(scenarios_long, aes(x = age, y = value,
                           color = phonation, linetype = gender)) +
  geom_line(linewidth = 0.5) +
  facet_wrap(~ measure, ncol = 2, scales = "free_y") +
  facetted_pos_scales(
    y = list(
      `Predicted VOT (ms)` = scale_y_continuous(limits = c(0, 100)),
      `Predicted F0 (Hz)` = scale_y_continuous(limits = c(100, 300))
    )
  ) +
  labs(
    title = "Predicted VOT and F0 by Phonation, Gender, and Age",
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

ggsave(out_file("predicted_vot_f0.png"), width = 5, height = 5, dpi = 300)

# Observed trajectories.
data_long <- data %>%
  pivot_longer(cols = c(vot, f0),
               names_to = "measure",
               values_to = "value") %>%
  mutate(measure = factor(measure,
                          levels = c("vot", "f0"),
                          labels = c("VOT (ms)", "F0 (Hz)")))

ggplot(data_long, aes(x = age, y = value,
                      color = phonation, linetype = gender)) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.5) +
  facet_wrap(~ measure, ncol = 2, scales = "free_y") +
  facetted_pos_scales(
    y = list(
      `VOT (ms)` = scale_y_continuous(limits = c(0, 100)),
      `F0 (Hz)` = scale_y_continuous(limits = c(100, 300))
    )
  ) +
  labs(
    title = "VOT and F0 by Phonation, Gender, and Age",
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

ggsave(out_file("observed_vot_f0.png"), width = 5, height = 5, dpi = 300)

# ===== Observed vs Predicted Lines =====

data_vot <- data_long %>% filter(measure == "VOT (ms)")
data_f0 <- data_long %>% filter(measure == "F0 (Hz)")

scenarios_vot <- scenarios_long %>% filter(measure == "Predicted VOT (ms)")
scenarios_f0 <- scenarios_long %>% filter(measure == "Predicted F0 (Hz)")

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

p3 <- ggplot(data_f0, aes(x = age, y = value, color = phonation)) +
  geom_smooth(aes(linetype = gender), method = "lm", se = FALSE, linewidth = 0.5) +
  scale_y_continuous(limits = c(100, 300)) +
  labs(x = "Age", y = "Mean f0 (Hz)", color = "Stop Category", linetype = "Gender") +
  scale_color_manual(values = c("lenis" = "#1f77b4",
                                "aspirated" = "#2ca02c",
                                "fortis" = "#ff7f0e")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlim(20, 72)

p4 <- ggplot(scenarios_f0, aes(x = age, y = value,
                               color = phonation, linetype = gender)) +
  geom_line(linewidth = 0.5) +
  scale_y_continuous(limits = c(100, 300)) +
  labs(x = "Age", y = "f0 (Hz)", color = "Stop Category", linetype = "Gender") +
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

ggsave(out_file("vot_f0_observed_lines.png"), width = 3, height = 5, dpi = 300, bg = "white")

p2 / p4 +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Predicted",
    theme = theme(plot.title = element_text(hjust = 0.3))
  ) &
  guides(linetype = guide_legend(override.aes = list(color = "black", linewidth = 0.5)))

ggsave(out_file("vot_f0_predicted_lines.png"), width = 3, height = 5, dpi = 300, bg = "white")

# ===== Model Predictions With CI =====

pred_vot <- ggpredict(m_vot,
                      terms = c("normed_age [-2:2, by=0.1]",
                                "phonation",
                                "gender"))

pred_vot <- pred_vot %>%
  mutate(
    predicted = exp(predicted),
    conf.low = exp(conf.low),
    conf.high = exp(conf.high)
  )

p1 <- plot(pred_vot) +
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

pred_f0 <- ggpredict(m_f0,
                     terms = c("normed_age [-2:2, by=0.1]",
                               "phonation",
                               "gender"))

p2 <- plot(pred_f0) +
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
  labs(title = "f0", x = "Age (SD from mean)", y = "f0 (Hz)") +
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

ggsave(out_file("observed_vs_predicted_vot_f0.png"),
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

ggsave(out_file("vot_observed_vs_predicted.png"), width = 5, height = 5, dpi = 300)

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

p2 <- ggplot(data, aes(x = f0_fitted, y = f0, color = phonation)) +
  geom_point(alpha = 0.2, size = 1) +
  geom_abline(slope = 1, intercept = 0,
              color = "black", linetype = "dashed", linewidth = 1) +
  scale_color_manual(values = c("lenis" = "#1f77b4",
                                "aspirated" = "#2ca02c",
                                "fortis" = "#ff7f0e")) +
  coord_equal(xlim = c(0, 400), ylim = c(0, 400)) +
  labs(
    title = "F0",
    x = "Predicted F0 (Hz)",
    y = "Observed F0 (Hz)",
    color = "Phonation"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

p1 + p2 +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Observed vs Predicted VOT and F0",
    subtitle = "Dashed line = perfect prediction"
  ) &
  theme(legend.position = "bottom")

ggsave(out_file("observed_vs_predicted_vot_f0_scatter.png"),
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

plot(allEffects(m_f0),
     multiline = TRUE)

emmip(m_f0, gender ~ normed_age | phonation,
      cov.reduce = range)

emmip(m_f0, phonation ~ gender,
      cov.reduce = range)

# meaningless bc of big f0 diff btwn female & male
emmip(m_f0, phonation ~ normed_age,
      cov.reduce = range)
