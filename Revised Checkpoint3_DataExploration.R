# ============================================================
# VTPEH 6270 - Check Point 03
# Data Exploration
# Author: Sowmya Srinivasan
# Date: 2026-02-20
# ============================================================
# Research Question:
# Is COVID-19 vaccine hesitancy higher in counties with
# higher Social Vulnerability Index (SVI)?
# ============================================================

# --- Libraries ----------------------------------------------
library(tidyverse)
library(readr)
library(knitr)
library(kableExtra)

# --- Data Import --------------------------------------------
# Update this path to match where your CSV is located
data <- read.csv(
  "R Lab_Vaccine_Hesitancy.csv",
  check.names = TRUE
)

# --- Data Cleaning ------------------------------------------
data_clean <- data %>%
  mutate(
    hesitancy_pct = parse_number(as.character(Estimated.hesitant)),
    svi_category  = trimws(as.character(SVI.Category))
  ) %>%
  filter(
    !is.na(hesitancy_pct),
    !is.na(svi_category),
    svi_category != ""
  ) %>%
  mutate(
    svi_category = factor(svi_category)
  )

# Reorder SVI categories from lowest to highest vulnerability
data_clean$svi_category <- factor(
  data_clean$svi_category,
  levels = c(
    "Very Low Vulnerability",
    "Low Vulnerability",
    "Moderate Vulnerability",
    "High Vulnerability",
    "Very High Vulnerability"
  )
)

# Confirm data dimensions and structure
dim(data_clean)
summary(data_clean$hesitancy_pct)
table(data_clean$svi_category)

# --- Grouped Summary Statistics -----------------------------
summary_table <- data_clean %>%
  group_by(svi_category) %>%
  summarise(
    N            = n(),
    `Mean (%)`   = round(mean(hesitancy_pct), 2),
    SD           = round(sd(hesitancy_pct), 2),
    `Median (%)` = round(median(hesitancy_pct), 2),
    IQR          = round(IQR(hesitancy_pct), 2),
    .groups = "drop"
  )

print(summary_table)

# --- Visualization 1: Boxplot -------------------------------
ggplot(data_clean, aes(x = svi_category, y = hesitancy_pct, fill = svi_category)) +
  geom_boxplot(alpha = 0.75) +
  labs(
    x     = "SVI Category",
    y     = "Estimated COVID-19 Vaccine Hesitancy (%)",
    title = "Estimated Vaccine Hesitancy by SVI Category"
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.text.x     = element_text(angle = 25, hjust = 1),
    plot.title      = element_text(hjust = 0.5)
  )

# Save Boxplot as PDF - Nature Formatting Guidelines
nature_boxplot <- ggplot(data_clean, aes(x = svi_category, y = hesitancy_pct, fill = svi_category)) +
  geom_boxplot(alpha = 0.75) +
  labs(
    x     = "SVI Category",
    y     = "Estimated COVID-19 Vaccine Hesitancy (%)",
    title = "Estimated Vaccine Hesitancy by SVI Category"
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.text.x     = element_text(angle = 25, hjust = 1, size = 7),
    axis.text.y     = element_text(size = 7),
    axis.title      = element_text(size = 8),
    plot.title      = element_text(hjust = 0.5, size = 9),
    line            = element_line(linewidth = 0.5)
  )

ggsave(
  filename = "Figure1_Hesitancy_by_SVI.pdf",
  plot     = nature_boxplot,
  width    = 89,
  height   = 70,
  units    = "mm",
  device   = "pdf",
  dpi      = 300
)
message("Figure 1 saved as Figure1_Hesitancy_by_SVI.pdf")

# --- Visualization 2: Mean +/- 95% CI Plot ------------------
plot_df <- data_clean %>%
  group_by(svi_category) %>%
  summarise(
    Mean  = mean(hesitancy_pct),
    SD    = sd(hesitancy_pct),
    N     = n(),
    SE    = SD / sqrt(N),
    Lower = Mean - 1.96 * SE,
    Upper = Mean + 1.96 * SE,
    .groups = "drop"
  )

ggplot(plot_df, aes(x = svi_category, y = Mean)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2) +
  labs(
    x     = "SVI Category",
    y     = "Mean Estimated Hesitancy (%)",
    title = "Mean Estimated Hesitancy (95% CI) by SVI Category"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1),
    plot.title  = element_text(hjust = 0.5)
  )

# Save Mean CI Plot as PDF - Nature Formatting Guidelines
nature_ci <- ggplot(plot_df, aes(x = svi_category, y = Mean)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2) +
  labs(
    x     = "SVI Category",
    y     = "Mean Estimated Hesitancy (%)",
    title = "Mean Estimated Hesitancy (95% CI) by SVI Category"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1, size = 7),
    axis.text.y = element_text(size = 7),
    axis.title  = element_text(size = 8),
    plot.title  = element_text(hjust = 0.5, size = 9),
    line        = element_line(linewidth = 0.5)
  )

ggsave(
  filename = "Figure2_MeanHesitancy_CI.pdf",
  plot     = nature_ci,
  width    = 89,
  height   = 70,
  units    = "mm",
  device   = "pdf",
  dpi      = 300
)
message("Figure 2 saved as Figure2_MeanHesitancy_CI.pdf")
