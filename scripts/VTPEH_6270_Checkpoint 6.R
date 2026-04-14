# =============================================================================
# VTPEH 6270 - Checkpoint 6
# Author: Sowmya Srinivasan
# Date: 2026-04-14
# Research Question: Is COVID-19 vaccine hesitancy higher in counties with
#                    higher Social Vulnerability Index (SVI)?
# =============================================================================

# -----------------------------------------------------------------------------
# 1. Load Libraries
# -----------------------------------------------------------------------------
library(readxl)
library(dplyr)
library(ggplot2)
library(knitr)
library(kableExtra)
library(tidyr)

# -----------------------------------------------------------------------------
# 2. Load Data
# -----------------------------------------------------------------------------
setwd("/Users/sowmya427/Desktop/VTPEH-6270-R-Lab/data")

raw <- read.csv("R Lab_Vaccine_Hesitancy.csv", check.names = FALSE)

cat("Column names:\n")
print(names(raw))

# -----------------------------------------------------------------------------
# 3. Clean and Rename Variables
# -----------------------------------------------------------------------------
df <- raw %>%
  rename(
    fips               = `FIPS Code`,
    county             = `County Name`,
    state              = `State`,
    hesitant           = `Estimated hesitant`,
    hesitant_or_unsure = `Estimated hesitant or unsure`,
    strongly_hesitant  = `Estimated strongly hesitant`,
    svi                = `Social Vulnerability Index (SVI)`,
    svi_cat            = `SVI Category`,
    vax_rate           = `Percent adults fully vaccinated against COVID-19 (as of 6/10/21)`,
    pct_hispanic       = `Percent Hispanic`,
    pct_black          = `Percent non-Hispanic Black`,
    pct_white          = `Percent non-Hispanic White`
  ) %>%
  mutate(
    hesitant_pct = as.numeric(gsub("%", "", hesitant)),
    vax_rate_pct = as.numeric(gsub("%", "", vax_rate)),
    svi_cat = factor(
      svi_cat,
      levels = c(
        "Very Low Vulnerability",
        "Low Vulnerability",
        "Moderate Vulnerability",
        "High Vulnerability",
        "Very High Vulnerability"
      )
    )
  ) %>%
  filter(!is.na(svi), !is.na(hesitant_pct))

cat("Final sample size:", nrow(df), "counties\n")

# -----------------------------------------------------------------------------
# 4. Create Figures Output Directory
# -----------------------------------------------------------------------------
dir.create("figures/CP06", recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# 5. Nature Theme
# -----------------------------------------------------------------------------
theme_nature <- function(base_size = 10) {
  theme_classic(base_size = base_size) +
    theme(
      text             = element_text(family = "sans"),
      axis.title       = element_text(size = base_size, color = "black"),
      axis.text        = element_text(size = base_size - 2, color = "black"),
      axis.line        = element_line(color = "black", linewidth = 0.5),
      axis.ticks       = element_line(color = "black", linewidth = 0.5),
      legend.text      = element_text(size = base_size - 2),
      legend.title     = element_text(size = base_size - 1, face = "bold"),
      legend.key.size  = unit(0.4, "cm"),
      plot.title       = element_text(size = base_size + 1, face = "bold", hjust = 0),
      plot.caption     = element_text(size = base_size - 3, color = "gray50", hjust = 0),
      panel.border     = element_blank(),
      strip.background = element_blank()
    )
}

# -----------------------------------------------------------------------------
# 6. Descriptive Statistics Table
# -----------------------------------------------------------------------------
summary_tbl <- df %>%
  group_by(svi_cat) %>%
  summarise(
    n      = n(),
    Mean   = round(mean(hesitant_pct), 2),
    SD     = round(sd(hesitant_pct), 2),
    Median = round(median(hesitant_pct), 2),
    Min    = round(min(hesitant_pct), 2),
    Max    = round(max(hesitant_pct), 2),
    .groups = "drop"
  )

print(summary_tbl)

# -----------------------------------------------------------------------------
# 7. Figure 1 - Boxplot
# -----------------------------------------------------------------------------
p_box <- ggplot(df, aes(x = svi_cat, y = hesitant_pct, fill = svi_cat)) +
  geom_boxplot(
    alpha         = 0.80,
    outlier.size  = 0.6,
    outlier.alpha = 0.35,
    linewidth     = 0.4
  ) +
  scale_fill_manual(
    name   = "SVI Category",
    values = c(
      "Very Low Vulnerability"  = "#4393C3",
      "Low Vulnerability"       = "#92C5DE",
      "Moderate Vulnerability"  = "#D9D9D9",
      "High Vulnerability"      = "#F4A582",
      "Very High Vulnerability" = "#D6604D"
    )
  ) +
  scale_x_discrete(
    labels = c("Very Low", "Low", "Moderate", "High", "Very High")
  ) +
  labs(
    title   = "COVID-19 Vaccine Hesitancy by Social Vulnerability Category",
    x       = "SVI Category",
    y       = "Estimated Hesitancy (%)",
    caption = "Source: CDC Vaccine Hesitancy County Estimates"
  ) +
  theme_nature()

p_box
ggsave("figures/CP06/CP06_boxplot.pdf", plot = p_box, width = 7, height = 5)

# -----------------------------------------------------------------------------
# 8. Figure 2 - Scatterplot
# -----------------------------------------------------------------------------
p_scatter <- ggplot(df, aes(x = svi, y = hesitant_pct)) +
  geom_point(
    aes(color = "County data"),
    alpha = 0.15,
    size  = 0.6
  ) +
  geom_smooth(
    aes(color = "LOESS smoother"),
    method    = "loess",
    linewidth = 1,
    se        = TRUE,
    fill      = "#F4A582",
    alpha     = 0.3
  ) +
  scale_color_manual(
    name   = "Legend",
    values = c(
      "County data"     = "#4393C3",
      "LOESS smoother"  = "#D6604D"
    )
  ) +
  labs(
    title   = "Social Vulnerability Index vs. COVID-19 Vaccine Hesitancy",
    x       = "Social Vulnerability Index (SVI, 0-1)",
    y       = "Estimated Hesitancy (%)",
    caption = "Source: CDC Vaccine Hesitancy County Estimates"
  ) +
  theme_nature()

p_scatter
ggsave("figures/CP06/CP06_scatterplot.pdf", plot = p_scatter, width = 7, height = 5)

# -----------------------------------------------------------------------------
# 9. Figure 3 - Histogram (Normality Check)
# -----------------------------------------------------------------------------
p_hist <- ggplot(df, aes(x = hesitant_pct)) +
  geom_histogram(
    aes(fill = "Counties (n = 3,141)"),
    bins  = 40,
    color = "white",
    alpha = 0.85
  ) +
  geom_vline(
    aes(xintercept = mean(hesitant_pct, na.rm = TRUE), color = "Mean"),
    linewidth = 1,
    linetype  = "dashed"
  ) +
  geom_vline(
    aes(xintercept = median(hesitant_pct, na.rm = TRUE), color = "Median"),
    linewidth = 1,
    linetype  = "dotted"
  ) +
  scale_fill_manual(
    name   = "Distribution",
    values = c("Counties (n = 3,141)" = "#4393C3")
  ) +
  scale_color_manual(
    name   = "Reference Lines",
    values = c("Mean" = "#D6604D", "Median" = "#4DAF4A")
  ) +
  labs(
    title = "Distribution of COVID-19 Vaccine Hesitancy Rates",
    x     = "Estimated Hesitancy (%)",
    y     = "Number of Counties"
  ) +
  theme_nature() +
  theme(legend.position = "right")

p_hist
ggsave("figures/CP06/CP06_histogram.pdf", plot = p_hist, width = 7, height = 5)

# -----------------------------------------------------------------------------
# 10. Figure 4 - Q-Q Plot (Normality Check)
# -----------------------------------------------------------------------------
p_qq <- ggplot(df, aes(sample = hesitant_pct)) +
  stat_qq(
    aes(color = "Sample data"),
    alpha = 0.3,
    size  = 0.8
  ) +
  stat_qq_line(
    aes(color = "Normal reference line"),
    linewidth = 1
  ) +
  scale_color_manual(
    name   = "Legend",
    values = c(
      "Sample data"          = "#4393C3",
      "Normal reference line" = "#D6604D"
    )
  ) +
  labs(
    title = "Q-Q Plot of Vaccine Hesitancy Rates",
    x     = "Theoretical Quantiles",
    y     = "Sample Quantiles (%)"
  ) +
  theme_nature() +
  theme(legend.position = "bottom")

p_qq
ggsave("figures/CP06/CP06_qqplot.pdf", plot = p_qq, width = 7, height = 5)

# -----------------------------------------------------------------------------
# 11. Shapiro-Wilk Normality Test
# -----------------------------------------------------------------------------
sw <- shapiro.test(df$hesitant_pct)

sw_tbl <- data.frame(
  Test    = "Shapiro-Wilk",
  W       = round(sw$statistic, 4),
  p_value = ifelse(sw$p.value < 0.001, "< 0.001", round(sw$p.value, 4)),
  Result  = ifelse(sw$p.value < 0.05,
                   "Non-normal - use non-parametric tests",
                   "Normal distribution"),
  check.names = FALSE
)

print(sw_tbl)

# -----------------------------------------------------------------------------
# 12. Kruskal-Wallis Test
# -----------------------------------------------------------------------------
kw <- kruskal.test(hesitant_pct ~ svi_cat, data = df)

kw_tbl <- data.frame(
  Test         = "Kruskal-Wallis Rank-Sum Test",
  Chi_squared  = round(kw$statistic, 3),
  df           = kw$parameter,
  p_value      = ifelse(kw$p.value < 0.001, "< 0.001", round(kw$p.value, 4)),
  Result       = "Significant difference in hesitancy across SVI categories",
  check.names  = FALSE
)

print(kw_tbl)

# -----------------------------------------------------------------------------
# 13. Pairwise Wilcoxon Test (Post-Hoc)
# -----------------------------------------------------------------------------
pw <- pairwise.wilcox.test(
  x              = df$hesitant_pct,
  g              = df$svi_cat,
  p.adjust.method = "bonferroni"
)

pw_tbl <- as.data.frame(pw$p.value) %>%
  tibble::rownames_to_column(var = "Group") %>%
  mutate(across(where(is.numeric), ~ case_when(
    .  < 0.001 ~ "< 0.001",
    .  < 0.05  ~ as.character(round(., 3)),
    is.na(.)   ~ "--",
    TRUE       ~ as.character(round(., 3))
  )))

print(pw_tbl)

# -----------------------------------------------------------------------------
# 14. Spearman Correlation
# -----------------------------------------------------------------------------
sp <- cor.test(df$svi, df$hesitant_pct, method = "spearman")

sp_tbl <- data.frame(
  Test      = "Spearman Rank Correlation",
  rho       = round(sp$estimate, 3),
  S         = round(sp$statistic, 1),
  p_value   = ifelse(sp$p.value < 0.001, "< 0.001", round(sp$p.value, 4)),
  Direction = "Positive association",
  Result    = "Higher SVI linked to higher hesitancy",
  check.names = FALSE
)

print(sp_tbl)
