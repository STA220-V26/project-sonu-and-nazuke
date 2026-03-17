library(targets)
library(tarchetypes)

tar_option_set(packages = c("arrow", "tidyverse"))

source("R/load_data.R")
source("R/process_data.R")
source("R/analysis.R")

list(
  #  LOAD
  tar_target(patients,   load_patients()),
  tar_target(conditions, load_conditions()),
  tar_target(encounters, load_encounters()),

  #  PROCESS 
  tar_target(analysis_data,
             build_analysis_data(patients, conditions, encounters)),

  #  ANALYSE
  tar_target(summary_table,   make_summary_table(analysis_data)),
  tar_target(race_table,      make_race_table(analysis_data)),
  tar_target(mortality_table, make_mortality_table(analysis_data)),
  tar_target(state_table,     make_state_table(analysis_data)),
  tar_target(stats_results,   run_stats(analysis_data)),

  #  PLOTS 
  tar_target(p_encounters,      plot_encounters(analysis_data)),
  tar_target(p_gender,          plot_encounters_gender(analysis_data)),
  tar_target(p_age,             plot_encounters_age(analysis_data)),
  tar_target(p_encounter_types, plot_encounter_types(analysis_data, encounters)),
  tar_target(p_race,            plot_race(race_table)),
  tar_target(p_mortality,       plot_mortality(mortality_table)),
  tar_target(p_state_count,     plot_state_count(state_table)),
  tar_target(p_state_pct,       plot_state_pct(state_table)),

  # REPORT 
  tar_quarto(report, "report.qmd")
)

