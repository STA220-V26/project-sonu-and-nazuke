build_analysis_data <- function(patients, conditions, encounters) {

  chronic_patients <- conditions |>
    filter(str_detect(description, regex("diabetes|hypertension",
                                         ignore_case = TRUE))) |>
    distinct(patient) |>
    mutate(has_chronic = TRUE)

  encounter_counts <- encounters |>
    group_by(patient) |>
    summarise(n_encounters = n(), .groups = "drop")

  patients |>
    select(patient = id, birthdate, gender, race,
           lat, lon, state, city, deathdate) |>
    left_join(encounter_counts, by = "patient") |>
    left_join(chronic_patients, by = "patient") |>
    mutate(
      has_chronic  = replace_na(has_chronic, FALSE),
      n_encounters = replace_na(n_encounters, 0),
      age          = as.numeric(difftime(Sys.Date(), birthdate,
                                          units = "days")) / 365.25,
      age_group    = cut(age,
                         breaks = c(0, 18, 40, 60, Inf),
                         labels = c("0-18", "19-40", "41-60", "60+"),
                         right  = TRUE),
      deceased     = !is.na(deathdate),
      group_label  = ifelse(has_chronic, "Chronic", "Healthy")
    )
}

