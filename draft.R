
# Research Question: Chronic vs Healthy Patients: Healthcare Encounters, Mortality Rates, Racial Disparities and Geographic Patterns


library(arrow)
library(tidyverse)

# 1. LOAD DATA

patients   <- read_parquet("data-parquet/patients.parquet")
conditions <- read_parquet("data-parquet/conditions.parquet")
encounters <- read_parquet("data-parquet/encounters.parquet")


# 2. IDENTIFY CHRONIC PATIENTS 


chronic_patients <- conditions |>
  filter(str_detect(description, regex("diabetes|hypertension", ignore_case = TRUE))) |>
  distinct(patient) |>
  mutate(has_chronic = TRUE)

cat("Patients with chronic conditions:", nrow(chronic_patients), "\n")

conditions |>
  filter(str_detect(description, regex("diabetes|hypertension", ignore_case = TRUE))) |>
  count(description, sort = TRUE) |>
  print(n = 20)


# 3. COUNT ENCOUNTERS PER PATIENT


encounter_counts <- encounters |>
  group_by(patient) |>
  summarise(n_encounters = n(), .groups = "drop")

cat("Patients with encounter records:", nrow(encounter_counts), "\n")


# 4. BUILD ANALYSIS DATASET


analysis_data <- patients |>
  select(patient = id, birthdate, gender) |>
  left_join(encounter_counts, by = "patient") |>
  left_join(chronic_patients, by = "patient") |>
  mutate(
    has_chronic   = replace_na(has_chronic, FALSE),
    n_encounters  = replace_na(n_encounters, 0),
    age           = as.numeric(difftime(Sys.Date(), birthdate, units = "days")) / 365.25,
    age_group     = cut(age,
                        breaks = c(0, 18, 40, 60, Inf),
                        labels = c("0-18", "19-40", "41-60", "60+"),
                        right  = TRUE)
  )

cat("Final dataset rows:", nrow(analysis_data), "\n")
glimpse(analysis_data)
# Verify: are all patients in the analysis from patients.parquet?
cat("Total patients in patients.parquet:", nrow(patients), "\n")
cat("Total patients in analysis_data:", nrow(analysis_data), "\n")

# Check for any patient IDs 
orphan_conditions <- conditions |>
  filter(!patient %in% patients$id)
cat("Conditions with no matching patient:", nrow(orphan_conditions), "\n")

# Check for any patient IDs in encounters
orphan_encounters <- encounters |>
  filter(!patient %in% patients$id)
cat("Encounters with no matching patient:", nrow(orphan_encounters), "\n")
# ------------------------------------------------------------
# 5. SUMMARY TABLE
# ------------------------------------------------------------

summary_table <- analysis_data |>
  group_by(has_chronic, gender) |>
  summarise(
    n                 = n(),
    mean_encounters   = round(mean(n_encounters), 1),
    median_encounters = median(n_encounters),
    mean_age          = round(mean(age), 1),
    .groups = "drop"
  )

print(summary_table)

# ------------------------------------------------------------
# 6. PLOTS
# ------------------------------------------------------------

# Plot 1: Encounters by chronic condition status
ggplot(analysis_data, aes(x = has_chronic, y = n_encounters, fill = has_chronic)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.2) +
  scale_x_discrete(labels = c("Healthy", "Chronic Condition")) +
  scale_fill_manual(values = c("#4CAFF0", "#F07B4C")) +
  labs(
    title   = "Healthcare Encounters: Chronic vs Healthy Patients",
    x       = "",
    y       = "Number of Encounters",
    fill    = "Has Chronic Condition"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Plot 2: Encounters by chronic condition + gender
ggplot(analysis_data, aes(x = has_chronic, y = n_encounters, fill = gender)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.2) +
  scale_x_discrete(labels = c("Healthy", "Chronic Condition")) +
  scale_fill_manual(values = c("#F07B4C", "#4CAFF0")) +
  labs(
    title = "Healthcare Encounters by Condition Status and Gender",
    x     = "",
    y     = "Number of Encounters",
    fill  = "Gender"
  ) +
  theme_minimal()

# Plot 3: Encounters by age group + chronic condition
ggplot(analysis_data, aes(x = age_group, y = n_encounters, fill = has_chronic)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.2) +
  scale_fill_manual(
    values = c("#4CAFF0", "#F07B4C"),
    labels = c("Healthy", "Chronic Condition")
  ) +
  labs(
    title = "Healthcare Encounters by Age Group and Condition Status",
    x     = "Age Group",
    y     = "Number of Encounters",
    fill  = ""
  ) +
  theme_minimal()


# 7. STATISTICAL  SIGNIFICANCE FOR HEALTHY Vs CHRONIC

healthy  <- analysis_data |> filter(!has_chronic) |> pull(n_encounters)
chronic  <- analysis_data |> filter(has_chronic)  |> pull(n_encounters)

test_result <- wilcox.test(chronic, healthy)
cat("\nWilcoxon test (chronic vs healthy encounters):\n")
print(test_result)
cat("Conclusion: p =", round(test_result$p.value, 4), "\n")

# 8. ENCOUNTER TYPES IN CHRONIC VS HEALTHY PATIENTS


encounter_types <- encounters |>
  left_join(
    analysis_data |> select(patient, has_chronic),
    by = "patient"
  ) |>
  filter(!is.na(has_chronic)) |>
  group_by(has_chronic, encounterclass) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(has_chronic) |>
  mutate(
    pct = round(100 * n / sum(n), 1),
    has_chronic = ifelse(has_chronic, "Chronic", "Healthy")
  )

print(encounter_types)

ggplot(encounter_types, aes(x = reorder(encounterclass, -pct), y = pct, fill = has_chronic)) +
  geom_col(position = "dodge", alpha = 0.8) +
  scale_fill_manual(values = c("#F07B4C", "#4CAFF0")) +
  labs(
    title = "Types of Healthcare Encounters: Chronic vs Healthy Patients",
    x     = "Encounter Type",
    y     = "Percentage of Encounters (%)",
    fill  = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# 9. RACE AND CHRONIC CONDITION PREVALENCE
race_analysis <- analysis_data |>
  left_join(
    patients |> select(patient = id, race),
    by = "patient"
  ) |>
  group_by(race) |>
  summarise(
    n             = n(),
    n_chronic     = sum(has_chronic),
    pct_chronic   = round(100 * mean(has_chronic), 1),
    .groups = "drop"
  ) |>
  arrange(desc(pct_chronic))

print(race_analysis)

ggplot(race_analysis, aes(x = reorder(race, pct_chronic), y = pct_chronic, fill = pct_chronic)) +
  geom_col(alpha = 0.85) +
  geom_text(aes(label = paste0(pct_chronic, "%")), 
            hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_fill_gradient(low = "#4CAFF0", high = "#F07B4C") +
  labs(
    title = "Chronic Condition Prevalence by Race",
    x     = "",
    y     = "% of Patients with Chronic Condition"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  ylim(0, max(race_analysis$pct_chronic) + 10)


# 10. MORTALITY BY CHRONIC STATUS


mortality_analysis <- analysis_data |>
  left_join(
    patients |> select(patient = id, deathdate),
    by = "patient"
  ) |>
  mutate(deceased = !is.na(deathdate)) |>
  group_by(has_chronic) |>
  summarise(
    n             = n(),
    n_deceased    = sum(deceased),
    pct_deceased  = round(100 * mean(deceased), 1),
    .groups = "drop"
  ) |>
  mutate(has_chronic = ifelse(has_chronic, "Chronic", "Healthy"))

print(mortality_analysis)

# Bar chart of mortality rate
ggplot(mortality_analysis, aes(x = has_chronic, y = pct_deceased, fill = has_chronic)) +
  geom_col(alpha = 0.85, width = 0.5) +
  geom_text(aes(label = paste0(pct_deceased, "%")),
            vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_manual(values = c("#F07B4C", "#4CAFF0")) +
  labs(
    title = "Mortality Rate: Chronic vs Healthy Patients",
    x     = "",
    y     = "% of Patients Deceased"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Is mortality significantly significant in chronic
mortality_table <- analysis_data |>
  left_join(
    patients |> select(patient = id, deathdate),
    by = "patient"
  ) |>
  mutate(deceased = !is.na(deathdate)) |>
  select(has_chronic, deceased) |>
  table()

cat("\nChi-square test (mortality vs chronic condition):\n")
chisq_result <- chisq.test(mortality_table)
print(chisq_result)
cat("Conclusion: p =", round(chisq_result$p.value, 4), "\n")

# 11. LEAFLET MAP -  Distribution of Chronic Patients


library(leaflet)

map_data <- analysis_data |>
  left_join(
    patients |> select(patient = id, lat, lon, city, state),
    by = "patient"
  ) |>
  filter(!is.na(lat) & !is.na(lon)) |>
  mutate(
    color = ifelse(has_chronic, "#F07B4C", "#4CAFF0"),
    label = paste0(
      "<b>", ifelse(has_chronic, "Chronic Patient", "Healthy Patient"), "</b><br>",
      "City: ", city, ", ", state, "<br>",
      "Age: ", round(age, 0), "<br>",
      "Gender: ", gender, "<br>",
      "Encounters: ", n_encounters
    )
  )

# MAP BUILDING
leaflet_map <- leaflet(map_data) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addCircleMarkers(
    lng        = ~lon,
    lat        = ~lat,
    radius     = 4,
    color      = ~color,
    fillColor  = ~color,
    fillOpacity = 0.6,
    stroke     = FALSE,
    popup      = ~label
  ) |>
  addLegend(
    position = "bottomright",
    colors   = c("#F07B4C", "#4CAFF0"),
    labels   = c("Chronic Patient", "Healthy Patient"),
    title    = "Condition Status",
    opacity  = 0.8
  ) |>
  addControl(
    html     = "<b>Geographic Distribution of Chronic vs Healthy Patients</b>",
    position = "topright"
  )


leaflet_map

# 12. TOP STATES BY CHRONIC PATIENT COUNT 


state_analysis <- analysis_data |>
  left_join(
    patients |> select(patient = id, state),
    by = "patient"
  ) |>
  filter(!is.na(state)) |>
  group_by(state) |>
  summarise(
    total_patients = n(),
    n_chronic      = sum(has_chronic),
    pct_chronic    = round(100 * mean(has_chronic), 1),
    .groups = "drop"
  ) |>
  arrange(desc(n_chronic))

# Top 15 states of chronic patients
state_analysis |>
  slice_head(n = 15) |>
  ggplot(aes(x = reorder(state, n_chronic), y = n_chronic, fill = n_chronic)) +
  geom_col(alpha = 0.85) +
  geom_text(aes(label = n_chronic), hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_fill_gradient(low = "#4CAFF0", high = "#F07B4C") +
  labs(
    title = "Top 15 States by Number of Chronic Patients",
    x     = "",
    y     = "Number of Chronic Patients"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none") +
  ylim(0, max(state_analysis$n_chronic) + 80)
print(state_analysis, n = 30)
