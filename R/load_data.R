library(arrow)
library(tidyverse)

load_patients <- function() {
  read_parquet("data-parquet/patients.parquet")
}

load_conditions <- function() {
  read_parquet("data-parquet/conditions.parquet")
}

load_encounters <- function() {
  read_parquet("data-parquet/encounters.parquet")
}

