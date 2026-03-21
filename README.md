# Chronic vs Healthy Patients: Healthcare Encounters, Mortality,Racial Disparities, and Geographic Patterns

## Project Description
This project investigates differences between patients with chronic
conditions (diabetes and/or hypertension) and healthy patients using health record data. We explore how chronic condition status relates to healthcare utilization, mortality rates, racial disparities in disease prevalence, and geographic distribution across the United States.

## Research Questions
- Do patients with chronic conditions (diabetes or hypertension) have significantly more healthcare encounters than healthy patients?
- Are mortality rates higher among chronically ill patients?
- Are there racial disparities in chronic condition prevalence?
- How are chronic and healthy patients distributed geographically?
## Data

This project provided as part of the STA220 course exercises. The following datasets are used:

| File                  | Description                                   |
|-----------------------|-----------------------------------------------|
| `patients.parquet`    | Demographics, location, birth/death dates     |
| `conditions.parquet`  | Diagnosed conditions per patient              |
| `encounters.parquet`  | Healthcare encounter records                  |


## Repository Structure

.
├── README.md
├── _targets.R            # {targets} pipeline definition
├── report.qmd            # Quarto report presenting results
├── R/
│   ├── load_data.R       # load_patients(), load_conditions(),
│   │                     #   load_encounters()
│   ├── process_data.R    # build_analysis_data()
│   └── analysis.R        # Summary tables, statistical tests,
│                         #   and all plot functions
├── data/                 # Raw data files (not tracked by git)
└── _targets/             # Auto-generated targets cache (not tracked)
```

## How to Reproduce

Uses targets package to manage the reproducible workflow.
### Requirements

- R version 4.5.2
- The following R packages used:
install.packages(c("targets", "tarchetypes", "arrow", "tidyverse",
                   "leaflet", "htmlwidgets", "quarto"))
```

### Running the pipeline

1. Clone this repository.
2. Place the three data files in the `data/` folder.
3. Open R in the project root and run:
```r
library(targets)
tar_make()
```


The pipeline will execute all steps in the correct order and render
`report.qmd` as the final output. To inspect individual targets:
```r
tar_read(summary_table)   # view the summary table
tar_read(p_encounters)    # view a specific plot
tar_visnetwork()          # visualise the full dependency graph
```

## Analysis Overview

The pipeline is organised into four stages:
### 1. Load (`load_data.R`)
Raw parquet files are read into R as targets:
`patients`, `conditions`, `encounters`.

### 2. Process (`process_data.R`)
`build_analysis_data()` links the three datasets, flags chronic
patients, derives age and age group
variables, and produces the central `analysis_data` target used
by all downstream steps.

### 3. Analyse (`analysis.R`)

| Target | Description |
|---|---|
| `summary_table` | Mean/median encounters and age by chronic status and gender |
| `race_table` | Chronic condition prevalence by race |
| `mortality_table` | Mortality rates by chronic status |
| `state_table` | Chronic patient counts and prevalence by state |
| `stats_results` | Wilcoxon rank-sum test (encounters) and chi-square test (mortality) |
| `p_encounters` | Boxplot: encounters by chronic status |
| `p_gender` | Boxplot: encounters by chronic status and gender |
| `p_age` | Boxplot: encounters by age group and chronic status |
| `p_encounter_types` | Bar chart: encounter class breakdown |
| `p_race` | Bar chart: chronic prevalence by race |
| `p_mortality` | Bar chart: mortality rate comparison |
| `p_state_count` | Bar chart: top states by chronic patient count |
| `p_state_pct` | Bar chart: top states by chronic prevalence (%) |

### 4. Report
`tar_quarto(report, "report.qmd")` renders the final Quarto report,
incorporating all tables and figures produced above.

The main report is report.html .
GitHub cant't preview it due to size; plaease download the file and open it in a browser. Because of this we have pushed the individual plots and tables for ease of viewing.