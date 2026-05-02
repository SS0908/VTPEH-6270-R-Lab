# VTPEH-6270-R-Lab

## Project Title
COVID-19 Vaccine Hesitancy and Social Vulnerability Index (SVI): A County-Level Analysis

## Brief Description
This repository contains data, analysis scripts, figures, and reports for a county-level investigation of the association between Social Vulnerability Index (SVI) and estimated COVID-19 vaccine hesitancy across U.S. counties. The project includes exploratory data analysis, data simulation, and visualization of simulation results.

---

## Author
**Sowmya Srinivasan**  
Cornell University  
Contact: ss4537@cornell.edu

---

## Research Question / Objectives
Is COVID-19 vaccine hesitancy higher in counties with higher Social Vulnerability Index (SVI)?

- **Variable A**: Social Vulnerability Index (SVI) — continuous variable (index 0–1)
- **Variable B**: Estimated proportion of hesitant adults — continuous variable (proportion)

---

## Data Source and Description
- **Source**: U.S. Centers for Disease Control and Prevention (CDC)
- **Dataset**: COVID-19 Vaccine Hesitancy for County and Local Estimates
- **URL**: https://data.cdc.gov/Vaccinations/Vaccine-Hesitancy-for-COVID-19-County-and-local-es/q9mh-h2tw/about_data
- **Unit of observation**: U.S. county (n = 3,141)
- **Key variables**:
  - `Estimated hesitant`: Percentage of adults estimated to be hesitant toward COVID-19 vaccination
  - `SVI`: Social Vulnerability Index score (0–1 scale)
  - `SVI Category`: Ordered categorical classification of SVI (Very Low to Very High Vulnerability)

---

## Repository Structure

```
VTPEH-6270-R-Lab/
├── data/
│   └── R Lab_Vaccine_Hesitancy.csv     # Original CDC dataset
├── scripts/
│   ├── Revised Checkpoint3_DataExploration.R   # Data exploration script
│   ├── Revised Checkpoint4_Simulation.R        # Data simulation script
│   ├── Shinyapp.R                              # Interactive Shiny app
│   ├── VTPEH_6270_Checkpoint 6.R               # Checkpoint 6 script
│   └── VTPEH_6270_Final_Report.R               # Final report script
├── Output/
│   ├── figures/                        # Generated figures (Nature format)
│   └── reports/                        # Rendered reports
├── README.md
└── .gitignore
```

---

## Analysis Scripts

| Script | Description |
|--------|-------------|
| `Revised Checkpoint3_DataExploration.R` | Data cleaning, grouped summary statistics, and comparative visualizations of vaccine hesitancy by SVI category |
| `Checkpoint4_Simulation.R` | Linear model fitting, data simulation across effect sizes/sample sizes/noise levels, and visualization of simulation results |

---

## Output Files
- **Figures**: All figures saved as PDFs following Nature journal formatting guidelines (single column: 89mm, double column: 183mm, font size 7pt)
- **Reports**: Full analysis reports knitted from R Markdown files

---


## Live Shiny App
View the interactive app here: [Vaccine Hesitancy & Social Vulnerability](https://ss0908.shinyapps.io/ShinyAppHW1/)

---


## AI Tool Disclosure
This project was completed with assistance from Claude (Anthropic) and ChatGPT to help structure reports and draft R code. All code was reviewed, executed, and adjusted by the author. Interpretations and written sections were composed by the author.

---

## References
1. Lee J, Huang Y. COVID-19 Vaccine Hesitancy: The Role of Socioeconomic Factors and Spatial Effects. *Vaccines*. 2022;10(3):352. doi:10.3390/vaccines10030352
2. Lamot M, Kirbiš A. Understanding Vaccine Hesitancy: A Comparison of Sociodemographic and Socioeconomic Predictors with Health Literacy Dimensions. *Vaccines*. 2024;12(10):1141. doi:10.3390/vaccines12101141
3. Nguyen VT, et al. Factors related to COVID-19 vaccine hesitancy among middle- and low-income adults in the U.S. *J Epidemiol Community Health*. 2023;77(5):328-335.
