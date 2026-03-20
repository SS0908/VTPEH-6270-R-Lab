# ============================================================
# VTPEH 6270 - Check Point 04
# Data Simulation
# Author: Sowmya Srinivasan
# Date: 2026-03-05
# ============================================================

# --- Libraries ----------------------------------------------
library(readxl)
library(dplyr)
library(ggplot2)
library(knitr)
library(kableExtra)
library(tidyr)
library(purrr)
library(scales)

# --- Data Import --------------------------------------------
setwd("/Users/sowmya427/Desktop/VTPEH-6270-R-Lab")

data_raw <- read.csv("data/R Lab_Vaccine_Hesitancy.csv")

# Rename columns by position
colnames(data_raw)[4] <- "hesitant"
colnames(data_raw)[7] <- "svi"
colnames(data_raw)[8] <- "svi_cat"

data <- data_raw %>%
  mutate(hesitant = as.numeric(gsub("%", "", hesitant)) / 100) %>%
  filter(!is.na(svi), !is.na(hesitant))

# Verify
nrow(data)
head(data$hesitant)

# --- Description Table --------------------------------------
var_table <- data.frame(
  `Variable Name` = c("Estimated hesitant", "Social Vulnerability Index (SVI)", "SVI Category"),
  `Variable Type` = c("Continuous (proportion)", "Continuous (index 0-1)", "Ordered categorical"),
  `R Class`       = c("numeric", "numeric", "character"),
  check.names     = FALSE
)
print(var_table)

# --- SVI Colors ---------------------------------------------
svi_colors <- c(
  "Very Low Vulnerability"  = "#2C7BB6",
  "Low Vulnerability"       = "#74C476",
  "Moderate Vulnerability"  = "#FEC44F",
  "High Vulnerability"      = "#F16913",
  "Very High Vulnerability" = "#CB181D"
)

data$svi_cat <- factor(data$svi_cat, levels = names(svi_colors))

# --- Plot 1: Scatter Plot -----------------------------------
scatter_fig <- ggplot(data, aes(x = svi, y = hesitant, color = svi_cat)) +
  geom_point(alpha = 0.35, size = 0.8) +
  geom_smooth(method = "lm", color = "black", linewidth = 1, se = TRUE) +
  scale_color_manual(values = svi_colors, name = "SVI Category") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "SVI vs. Vaccine Hesitancy by U.S. County",
    x     = "Social Vulnerability Index (SVI)",
    y     = "Estimated Proportion Hesitant"
  ) +
  theme_minimal()

print(scatter_fig)

# --- Plot 2: Box Plot ---------------------------------------
boxplot_fig <- ggplot(data, aes(x = svi_cat, y = hesitant, fill = svi_cat)) +
  geom_boxplot(alpha = 0.75, outlier.size = 0.5) +
  scale_fill_manual(values = svi_colors) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Vaccine Hesitancy by SVI Category",
    x     = "SVI Category",
    y     = "Estimated Proportion Hesitant"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x     = element_text(angle = 20, hjust = 1)
  )

print(boxplot_fig)

# --- Parameters of Interest ---------------------------------
fit     <- lm(hesitant ~ svi, data = data)
a_est   <- round(coef(fit)[2], 4)
b_est   <- round(coef(fit)[1], 4)
eps_est <- round(sigma(fit), 4)

param_table <- data.frame(
  Parameter   = c("Slope", "Intercept", "Noise"),
  Symbol      = c("a", "b", "e"),
  Description = c(
    paste0("Increase in hesitancy per 1-unit increase in SVI (est: ", a_est, ")"),
    paste0("Baseline hesitancy when SVI = 0 (est: ", b_est, ")"),
    paste0("Residual standard deviation - unexplained variability (est: ", eps_est, ")")
  ),
  check.names = FALSE
)
print(param_table)

# --- Section 3: Simulation ----------------------------------
set.seed(42)

simulate_hesitancy <- function(n = 500, slope = 0.08, intercept = 0.085, noise = 0.03) {
  svi_sim      <- runif(n, min = 0, max = 1)
  hesitant_sim <- intercept + slope * svi_sim + rnorm(n, mean = 0, sd = noise)
  hesitant_sim <- pmax(0, pmin(1, hesitant_sim))
  data.frame(svi = svi_sim, hesitant = hesitant_sim)
}

sim_example <- simulate_hesitancy(n = 500, slope = 0.08, noise = 0.03)
head(sim_example)

# --- Simulation Output --------------------------------------
summarize_sim <- function(sim_data) {
  sim_data %>%
    mutate(svi_quartile = ntile(svi, 4)) %>%
    group_by(svi_quartile) %>%
    summarise(
      mean_hesitant   = mean(hesitant),
      median_hesitant = median(hesitant),
      sd_hesitant     = sd(hesitant),
      n               = n(),
      .groups         = "drop"
    )
}

print(summarize_sim(sim_example))

# --- Simulation Function ------------------------------------
run_simulation <- function(n = 500, slope = 0.08, intercept = 0.085, noise = 0.03) {
  sim_data    <- simulate_hesitancy(n, slope, intercept, noise)
  sim_summary <- summarize_sim(sim_data)
  list(data = sim_data, summary = sim_summary)
}

print(run_simulation(n = 200, slope = 0.10, noise = 0.04)$summary)

# --- Simulation Automation ----------------------------------
effect_sizes <- seq(0, 0.18, length.out = 10)
sample_sizes <- c(30, 60, 100, 200, 350, 500, 750, 1000, 2000, 3200)
noise_levels <- c(0.015, 0.03, 0.06)

param_grid <- expand.grid(slope = effect_sizes, n = sample_sizes, noise = noise_levels)

set.seed(123)
sim_results <- pmap(param_grid, function(slope, n, noise) {
  run_simulation(n = n, slope = slope, intercept = 0.085, noise = noise)
})

cat("Total simulations:", length(sim_results), "\n")

# --- Section 4: Visualization -------------------------------
sim_summary_data <- param_grid %>%
  mutate(run_id = row_number()) %>%
  bind_cols(
    map_dfr(sim_results, function(r) {
      r$summary %>%
        summarise(diff_q4_q1 = mean_hesitant[svi_quartile == 4] -
                    mean_hesitant[svi_quartile == 1])
    })
  )

# Plot 3 - Heatmap
heatmap_fig <- sim_summary_data %>%
  mutate(noise_label = paste0("Noise SD = ", noise)) %>%
  ggplot(aes(x = factor(n), y = factor(round(slope, 3)), fill = diff_q4_q1)) +
  geom_tile(color = "white", linewidth = 0.3) +
  facet_wrap(~noise_label, ncol = 1) +
  scale_fill_gradient2(
    low = "#2C7BB6", mid = "#FFFFBF", high = "#D7191C",
    midpoint = 0.09, name = "Hesitancy\nGap (Q4-Q1)",
    labels = percent_format(accuracy = 1)
  ) +
  labs(
    title = "Simulated Hesitancy Gap by Effect Size, Sample Size & Noise",
    x     = "Sample Size (n)",
    y     = "Effect Size (slope)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1),
    strip.text  = element_text(face = "bold"),
    panel.grid  = element_blank()
  )

print(heatmap_fig)

# Plot 4 - Line Plot
lineplot_fig <- param_grid %>%
  mutate(run_id = row_number()) %>%
  filter(n == 500, noise == 0.03) %>%
  rowwise() %>%
  mutate(summary = list(sim_results[[run_id]]$summary %>%
                          rename(count = n))) %>%
  unnest(summary) %>%
  ggplot(aes(x = svi_quartile, y = mean_hesitant,
             group = factor(slope), color = slope)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  scale_color_gradient(low = "#AED6F1", high = "#C0392B", name = "Slope (a)") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_continuous(
    breaks = 1:4,
    labels = c("Q1 (Low SVI)", "Q2", "Q3", "Q4 (High SVI)")
  ) +
  labs(
    title    = "Mean Hesitancy Across SVI Quartiles by Effect Size",
    subtitle = "n = 500, noise SD = 0.03",
    x        = "SVI Quartile",
    y        = "Mean Estimated Hesitancy"
  ) +
  theme_minimal()

print(lineplot_fig)

# --- Save All Figures as PDF (Nature Formatting) ------------

# Figure 1 - Scatter Plot
nature_fig1 <- ggplot(data, aes(x = svi, y = hesitant, color = svi_cat)) +
  geom_point(alpha = 0.35, size = 0.5) +
  geom_smooth(method = "lm", color = "black", linewidth = 0.8, se = TRUE) +
  scale_color_manual(values = svi_colors, name = "SVI Category") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "SVI vs. Vaccine Hesitancy by U.S. County",
    x     = "Social Vulnerability Index (SVI)",
    y     = "Estimated Proportion Hesitant"
  ) +
  theme_classic(base_size = 7) +
  theme(
    legend.key.size = unit(0.3, "cm"),
    legend.text     = element_text(size = 6),
    legend.title    = element_text(size = 7),
    plot.title      = element_text(size = 7, face = "bold"),
    axis.text       = element_text(size = 6),
    axis.title      = element_text(size = 7)
  )

ggsave(
  filename   = "figures/Figure1_Scatter_SVI_Hesitancy.pdf",
  plot       = nature_fig1,
  width      = 183, height = 120, units = "mm", device = "pdf", dpi = 300,
  create.dir = TRUE
)
message("Figure 1 saved: figures/Figure1_Scatter_SVI_Hesitancy.pdf")

# Figure 2 - Box Plot
nature_fig2 <- ggplot(data, aes(x = svi_cat, y = hesitant, fill = svi_cat)) +
  geom_boxplot(alpha = 0.75, outlier.size = 0.5) +
  scale_fill_manual(values = svi_colors) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Vaccine Hesitancy by SVI Category",
    x     = "SVI Category",
    y     = "Estimated Proportion Hesitant"
  ) +
  theme_classic(base_size = 7) +
  theme(
    legend.position = "none",
    axis.text.x     = element_text(angle = 20, hjust = 1, size = 6),
    axis.text.y     = element_text(size = 6),
    axis.title      = element_text(size = 7),
    plot.title      = element_text(size = 7, face = "bold")
  )

ggsave(
  filename   = "figures/Figure2_Boxplot_SVI_Hesitancy.pdf",
  plot       = nature_fig2,
  width      = 89, height = 70, units = "mm", device = "pdf", dpi = 300,
  create.dir = TRUE
)
message("Figure 2 saved: figures/Figure2_Boxplot_SVI_Hesitancy.pdf")

# Figure 3 - Heatmap
nature_fig3 <- sim_summary_data %>%
  mutate(noise_label = paste0("Noise SD = ", noise)) %>%
  ggplot(aes(x = factor(n), y = factor(round(slope, 3)), fill = diff_q4_q1)) +
  geom_tile(color = "white", linewidth = 0.3) +
  facet_wrap(~noise_label, ncol = 1) +
  scale_fill_gradient2(
    low = "#2C7BB6", mid = "#FFFFBF", high = "#D7191C",
    midpoint = 0.09, name = "Hesitancy\nGap (Q4-Q1)",
    labels = percent_format(accuracy = 1)
  ) +
  labs(
    title = "Simulated Hesitancy Gap by Effect Size, Sample Size & Noise",
    x     = "Sample Size (n)",
    y     = "Effect Size (slope)"
  ) +
  theme_minimal(base_size = 7) +
  theme(
    axis.text.x  = element_text(angle = 35, hjust = 1, size = 6),
    axis.text.y  = element_text(size = 6),
    axis.title   = element_text(size = 7),
    plot.title   = element_text(size = 7, face = "bold"),
    strip.text   = element_text(face = "bold", size = 6),
    panel.grid   = element_blank(),
    legend.text  = element_text(size = 6),
    legend.title = element_text(size = 7)
  )

ggsave(
  filename   = "figures/Figure3_Heatmap_Simulation.pdf",
  plot       = nature_fig3,
  width      = 183, height = 200, units = "mm", device = "pdf", dpi = 300,
  create.dir = TRUE
)
message("Figure 3 saved: figures/Figure3_Heatmap_Simulation.pdf")

# Figure 4 - Line Plot
nature_fig4 <- param_grid %>%
  mutate(run_id = row_number()) %>%
  filter(n == 500, noise == 0.03) %>%
  rowwise() %>%
  mutate(summary = list(sim_results[[run_id]]$summary %>%
                          rename(count = n))) %>%
  unnest(summary) %>%
  ggplot(aes(x = svi_quartile, y = mean_hesitant,
             group = factor(slope), color = slope)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  scale_color_gradient(low = "#AED6F1", high = "#C0392B", name = "Slope (a)") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_continuous(
    breaks = 1:4,
    labels = c("Q1 (Low SVI)", "Q2", "Q3", "Q4 (High SVI)")
  ) +
  labs(
    title    = "Mean Hesitancy Across SVI Quartiles by Effect Size",
    subtitle = "n = 500, noise SD = 0.03",
    x        = "SVI Quartile",
    y        = "Mean Estimated Hesitancy"
  ) +
  theme_classic(base_size = 7) +
  theme(
    axis.text.x   = element_text(size = 6),
    axis.text.y   = element_text(size = 6),
    axis.title    = element_text(size = 7),
    plot.title    = element_text(size = 7, face = "bold"),
    plot.subtitle = element_text(size = 6),
    legend.text   = element_text(size = 6),
    legend.title  = element_text(size = 7)
  )

ggsave(
  filename   = "figures/Figure4_Lineplot_EffectSize.pdf",
  plot       = nature_fig4,
  width      = 89, height = 70, units = "mm", device = "pdf", dpi = 300,
  create.dir = TRUE
)
message("Figure 4 saved: figures/Figure4_Lineplot_EffectSize.pdf")
