#' =============================================================================
#' VTPEH 6270 Final Report
#' Author: Sowmya Srinivasan
#' Date: 2026-05-03
#' =============================================================================
#'
#' This is the .R script version of the .Rmd final report.
#' Narrative text from the report is preserved as roxygen-style comments (#').
#' Table-rendering code (kable / kable_styling) is replaced with plain print()
#' so output is readable in the R console; the underlying summary data frames
#' are unchanged.


#' =============================================================================
#' 1. INTRODUCTION, RESEARCH QUESTION, PLAUSIBILITY AND CONTEXT
#' =============================================================================

#' ## Introduction
#'
#' This analysis uses the COVID-19 Vaccine Hesitancy for County and Local
#' Estimates data set published by the U.S. Centers for Disease Control and
#' Prevention (CDC) [1]. The dataset contains county-level estimates of vaccine
#' hesitancy among adults, vaccination coverage as of June 10, 2021, and
#' measures of social vulnerability, including the Social Vulnerability Index
#' (SVI) and demographic characteristics. Each observation represents a U.S.
#' county and includes both continuous and categorical variables suitable for
#' exploratory data analysis and visualization.
#'
#' Dataset URL:
#' https://data.cdc.gov/Vaccinations/Vaccine-Hesitancy-for-COVID-19-County-and-local-es/q9mh-h2tw/about_data

#' ## Research Question
#'
#' Is COVID-19 vaccine hesitancy higher in counties with higher Social
#' Vulnerability Index (SVI)?
#'
#' - Variable A (Outcome, continuous): Estimated hesitant — percentage of
#'   adults estimated to be hesitant toward COVID-19 vaccination
#' - Variable B (Predictor, categorical): SVI category — categorized measure
#'   of county-level social vulnerability

#' ## Scientific Plausibility and Public Health Context
#'
#' COVID-19 vaccine hesitancy directly undermined efforts to reduce
#' transmission and mortality during the pandemic, making it one of the most
#' consequential public health challenges of the past decade. The CDC/ATSDR
#' Social Vulnerability Index (SVI) captures community-level conditions —
#' socioeconomic status, minority status, household composition, and
#' housing/transportation access — that shape how communities experience and
#' respond to public health crises [2].
#'
#' There are strong reasons to expect higher SVI to correlate with greater
#' hesitancy: reduced healthcare access, structural barriers to vaccination,
#' and historically rooted mistrust of medical institutions among communities
#' that have faced documented mistreatment [3, 4]. Sociodemographic and
#' socioeconomic characteristics, along with health literacy, have been shown
#' to be important predictors of vaccine hesitancy [5]. If this association
#' holds, public health agencies can target outreach and resources to high-SVI
#' counties to close vaccination gaps and reduce disparities in future
#' pandemic responses.


#' =============================================================================
#' 2. MATERIAL & METHODS
#' =============================================================================

#' ## 2.1 Loading Data and Data Description

# ---- Libraries --------------------------------------------------------------
library(readxl)
library(dplyr)
library(ggplot2)
library(knitr)
library(kableExtra)
library(tidyr)


# ---- Load Data --------------------------------------------------------------

# Set working directory
setwd("/Users/sowmya427/Desktop/VTPEH-6270-R-Lab/data")

# Load file
raw <- read.csv("R Lab_Vaccine_Hesitancy.csv", check.names = FALSE)

# Confirm columns loaded correctly
cat("Column names:\n")
print(names(raw))

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


# ---- Nature Theme -----------------------------------------------------------
dir.create("figures/CP06", recursive = TRUE, showWarnings = FALSE)

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


# ---- Description Table ------------------------------------------------------
summary_tbl <- df %>%
  group_by(svi_cat) %>%
  summarise(
    n      = n(),
    Mean   = round(mean(hesitant_pct), 2),
    SD     = round(sd(hesitant_pct),   2),
    Median = round(median(hesitant_pct), 2),
    Min    = round(min(hesitant_pct),  2),
    Max    = round(max(hesitant_pct),  2),
    .groups = "drop"
  )

cat("\nDescriptive statistics of estimated COVID-19 vaccine hesitancy (%) by SVI category:\n")
print(summary_tbl)


#' ## 2.2 Statistical Analysis
#'
#' To examine whether COVID-19 vaccine hesitancy rates differ across Social
#' Vulnerability Index (SVI) categories, the following statistical approach
#' was used.
#'
#' Assumption Checks:
#' Prior to selecting a statistical test, the normality of the outcome
#' variable (estimated vaccine hesitancy rate, %) was assessed using the
#' Shapiro-Wilk test, where a p-value < 0.05 indicates a significant departure
#' from normality. A histogram and Q-Q plot were also used to visually inspect
#' the distribution of the data. Normality is a required assumption for
#' parametric tests such as one-way ANOVA, and failure to meet this assumption
#' requires the use of non-parametric alternatives.
#'
#' Model Selection:
#' Because the Shapiro-Wilk test confirmed that the hesitancy rate data were
#' non-normally distributed, a Kruskal-Wallis rank-sum test was selected to
#' compare hesitancy rates across the five SVI categories (Very Low, Low,
#' Moderate, High, and Very High Vulnerability). The Kruskal-Wallis test is
#' the non-parametric equivalent of a one-way ANOVA and does not require the
#' assumption of normality. In addition, Spearman's rank correlation was used
#' to quantify the strength and direction of the monotonic association between
#' the continuous SVI score and vaccine hesitancy rate. Spearman's correlation
#' was chosen over Pearson's correlation as it does not assume normality and
#' is appropriate for non-normally distributed continuous variables.
#'
#' Post-Hoc Analysis:
#' A significant Kruskal-Wallis result indicates that at least one SVI
#' category differs from the others, but does not specify which pairs.
#' Therefore, pairwise Wilcoxon rank-sum tests were conducted as a post-hoc
#' analysis to identify which specific SVI category pairs showed statistically
#' significant differences in vaccine hesitancy rates. P-values were adjusted
#' using the Bonferroni correction to control for multiple comparisons and
#' reduce the risk of Type I error. Statistical significance was set at
#' alpha = 0.05 for all analyses.

#' ## 2.3 Code
#'
#' All code and data used in this analysis are available at:
#' https://github.com/SS0908/VTPEH-6270-R-Lab

#' ## 2.4 Interactive Shiny Application
#'
#' An interactive Shiny dashboard accompanying this report is available at:
#' https://ss0908.shinyapps.io/ShinyAppHW1/
#'
#' The application allows users to explore the relationship between Social
#' Vulnerability Index (SVI) and COVID-19 vaccine hesitancy interactively,
#' including filtering by SVI category and viewing county-level distributions.


#' =============================================================================
#' 3. RESULTS
#' =============================================================================

#' ## 3.1 Data Visualization

# ---- Boxplot ----------------------------------------------------------------
#' Distribution of estimated COVID-19 vaccine hesitancy (%) by SVI category.
#' Boxes span the interquartile range; horizontal lines show medians;
#' dots are outliers.

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

print(p_box)

ggsave("figures/CP06/CP06_boxplot.pdf",
       plot = p_box, width = 7, height = 4.5, device = "pdf")


# ---- Scatterplot ------------------------------------------------------------
#' Association between continuous SVI score and estimated vaccine hesitancy
#' (%) across U.S. counties (n = 3,141). Each point is one county. Red curve
#' is a LOESS smoother with 95% CI shaded.

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
      "County data"    = "#4393C3",
      "LOESS smoother" = "#D6604D"
    )
  ) +
  labs(
    title   = "Social Vulnerability Index vs. COVID-19 Vaccine Hesitancy",
    x       = "Social Vulnerability Index (SVI, 0-1)",
    y       = "Estimated Hesitancy (%)",
    caption = "Source: CDC Vaccine Hesitancy County Estimates"
  ) +
  theme_nature()

print(p_scatter)

ggsave("figures/CP06/CP06_scatterplot.pdf",
       plot = p_scatter, width = 7, height = 4.5, device = "pdf")


# ---- Normality; Plot 1 - Histogram ------------------------------------------
#' Histogram of estimated COVID-19 vaccine hesitancy rates (%) across U.S.
#' counties. Dashed red line = mean; dotted green line = median.

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

print(p_hist)

ggsave("figures/CP06/CP06_histogram.pdf",
       plot = p_hist, width = 7, height = 4, device = "pdf")


# ---- Normality; Plot 2 - Q-Q plot -------------------------------------------
#' Q-Q plot of estimated vaccine hesitancy rates. Blue points = sample data;
#' red line = expected values under a normal distribution. Deviations at the
#' tails indicate non-normality.

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
      "Sample data"           = "#4393C3",
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

print(p_qq)

ggsave("figures/CP06/CP06_qqplot.pdf",
       plot = p_qq, width = 6, height = 5, device = "pdf")


#' ## 3.2 Statistical Analysis

# ---- Shapiro-Wilk Test ------------------------------------------------------
sw <- shapiro.test(df$hesitant_pct)

sw_tbl <- data.frame(
  Test        = "Shapiro-Wilk",
  W           = round(sw$statistic, 4),
  `p-value`   = ifelse(sw$p.value < 0.001, "< 0.001", round(sw$p.value, 4)),
  Result      = ifelse(sw$p.value < 0.05,
                       "Non-normal - use non-parametric tests",
                       "Normal distribution"),
  check.names = FALSE
)

cat("\nShapiro-Wilk normality test for vaccine hesitancy rates:\n")
print(sw_tbl, row.names = FALSE)

#' The Shapiro-Wilk test returned a W statistic and a p-value < 0.001,
#' indicating that the distribution of vaccine hesitancy rates departs
#' significantly from normality. This means we cannot assume the data follow
#' a normal distribution, and therefore non-parametric tests must be used for
#' all subsequent group comparisons rather than parametric alternatives such
#' as ANOVA or a t-test.

cat(sprintf("\nW = %.4f, p-value = %s\n",
            sw$statistic,
            ifelse(sw$p.value < 0.001, "< 0.001",
                   format.pval(sw$p.value, digits = 4))))


# ---- Kruskal-Wallis Test ----------------------------------------------------
kw <- kruskal.test(hesitant_pct ~ svi_cat, data = df)

kw_tbl <- data.frame(
  Test          = "Kruskal-Wallis Rank-Sum Test",
  `Chi-squared` = round(kw$statistic, 3),
  df            = kw$parameter,
  `p-value`     = ifelse(kw$p.value < 0.001, "< 0.001", round(kw$p.value, 4)),
  Result        = "Significant difference in hesitancy across SVI categories",
  check.names   = FALSE
)
rownames(kw_tbl) <- NULL

cat("\nKruskal-Wallis test comparing vaccine hesitancy rates across SVI categories:\n")
print(kw_tbl, row.names = FALSE)

#' The Kruskal-Wallis rank-sum test revealed a statistically significant
#' difference in vaccine hesitancy rates across the five SVI categories.
#' This result allows us to reject the null hypothesis that hesitancy rates
#' are equal across SVI categories. In other words, social vulnerability
#' level is associated with differences in COVID-19 vaccine hesitancy at the
#' county level. However, this test alone does not tell us which specific
#' categories differ from one another, which is addressed by the pairwise
#' comparisons below.

cat(sprintf("\nX^2 = %.2f, df = %d, p-value = %s\n",
            kw$statistic,
            kw$parameter,
            ifelse(kw$p.value < 0.001, "< 0.001",
                   format.pval(kw$p.value, digits = 4))))


# ---- Pairwise Wilcoxon ------------------------------------------------------
pw <- pairwise.wilcox.test(
  x               = df$hesitant_pct,
  g               = df$svi_cat,
  p.adjust.method = "bonferroni"
)

# Format the p-value matrix into a clean table
pw_tbl <- as.data.frame(pw$p.value) %>%
  tibble::rownames_to_column(var = "Group") %>%
  mutate(across(where(is.numeric), ~ case_when(
    . < 0.001 ~ "< 0.001",
    . < 0.05  ~ as.character(round(., 3)),
    is.na(.)  ~ "--",
    TRUE      ~ as.character(round(., 3))
  )))

cat("\nPairwise Wilcoxon rank-sum tests across SVI categories (Bonferroni correction):\n")
print(pw_tbl, row.names = FALSE)

#' Pairwise Wilcoxon rank-sum tests with Bonferroni correction were conducted
#' to identify which specific SVI category pairs drive the overall significant
#' Kruskal-Wallis result. The results show that most pairwise comparisons
#' between SVI categories were statistically significant (p < 0.001) after
#' correction for multiple testing. Counties in higher vulnerability
#' categories consistently showed greater vaccine hesitancy than those in
#' lower vulnerability categories. Comparisons between adjacent categories
#' (e.g., Very Low vs. Low Vulnerability) may show weaker or non-significant
#' differences, reflecting the gradual nature of the relationship between
#' social vulnerability and hesitancy.


# ---- Spearman Correlation ---------------------------------------------------
sp <- cor.test(df$svi, df$hesitant_pct, method = "spearman")

sp_tbl <- data.frame(
  Test        = "Spearman Rank Correlation",
  `rho (p)`   = round(sp$estimate, 3),
  S           = round(sp$statistic, 1),
  `p-value`   = ifelse(sp$p.value < 0.001, "< 0.001", round(sp$p.value, 4)),
  Direction   = "Positive association",
  Result      = "Higher SVI linked to higher hesitancy",
  check.names = FALSE
)
rownames(sp_tbl) <- NULL

cat("\nSpearman rank correlation between SVI score and vaccine hesitancy (%):\n")
print(sp_tbl, row.names = FALSE)

#' Spearman's rank correlation between the continuous SVI score and vaccine
#' hesitancy rate yielded a statistically significant positive monotonic
#' association (p < 0.001). This means that as the Social Vulnerability Index
#' increases across counties, estimated vaccine hesitancy also tends to
#' increase. The magnitude of the correlation suggests a moderate positive
#' relationship, supporting the conclusion that social vulnerability is
#' meaningfully — though not perfectly — linked to COVID-19 vaccine hesitancy
#' at the county level. This finding directly addresses our research question
#' and is consistent with the patterns observed visually in the scatterplot
#' and boxplot.

cat(sprintf("\nrho = %.3f, S = %.1f, p-value = %s\n",
            sp$estimate,
            sp$statistic,
            ifelse(sp$p.value < 0.001, "< 0.001",
                   format.pval(sp$p.value, digits = 4))))


#' =============================================================================
#' 4. CONCLUSION
#' =============================================================================
#'
#' The results of this analysis support the hypothesis that COVID-19 vaccine
#' hesitancy is higher in counties with greater social vulnerability. The
#' Kruskal-Wallis test identified statistically significant differences in
#' hesitancy rates across SVI categories (p < 0.001), and pairwise Wilcoxon
#' comparisons confirmed that higher-vulnerability counties showed greater
#' hesitancy than lower-vulnerability counties across most category pairs.
#' The Spearman correlation (p < 0.001) further supports a moderate positive
#' association between the continuous SVI score and estimated hesitancy rate.
#'
#' These findings align with the research question posed in the Introduction
#' — counties with higher social vulnerability do tend to have higher
#' estimated COVID-19 vaccine hesitancy. The moderate strength of the
#' association suggests that while social vulnerability is a meaningful
#' predictor of hesitancy, other factors likely contribute as well. These
#' results highlight the importance of targeting socially vulnerable
#' communities in public health vaccination campaigns.


#' =============================================================================
#' 5. AI USE DISCLOSURE STATEMENT
#' =============================================================================
#'
#' This document was created with the assistance of Claude (Anthropic) to
#' help structure and generate the R code, which was reviewed and adjusted
#' by the author. Interpretations and written sections were composed by the
#' author.


#' =============================================================================
#' 6. REFERENCES
#' =============================================================================
#'
#' 1. Centers for Disease Control and Prevention. Vaccine Hesitancy for
#'    COVID-19: County and Local Estimates.
#'    https://data.cdc.gov/Vaccinations/Vaccine-Hesitancy-for-COVID-19-County-and-local-es/q9mh-h2tw/about_data
#'
#' 2. Centers for Disease Control and Prevention/Agency for Toxic Substances
#'    and Disease Registry (CDC/ATSDR). CDC/ATSDR Social Vulnerability Index
#'    (SVI). Atlanta, GA: U.S. Department of Health and Human Services.
#'    https://www.atsdr.cdc.gov/place-health/php/svi/index.html
#'
#' 3. Lee J, Huang Y. COVID-19 Vaccine Hesitancy: The Role of Socioeconomic
#'    Factors and Spatial Effects. Vaccines. 2022;10(3):352.
#'    doi:10.3390/vaccines10030352
#'
#' 4. Nguyen VT, et al. Factors related to COVID-19 vaccine hesitancy among
#'    middle- and low-income adults in the U.S. J Epidemiol Community Health.
#'    2023;77(5):328-335.
#'
#' 5. Lamot M, Kirbiš A. Understanding Vaccine Hesitancy: A Comparison of
#'    Sociodemographic and Socioeconomic Predictors with Health Literacy
#'    Dimensions. Vaccines. 2024;12(10):1141. doi:10.3390/vaccines12101141
