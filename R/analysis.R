# ── SUMMARY TABLE ───────────────────────────────────────
make_summary_table <- function(analysis_data) {
  analysis_data |>
    group_by(group_label, gender) |>
    summarise(
      n                 = n(),
      mean_encounters   = round(mean(n_encounters), 1),
      median_encounters = median(n_encounters),
      mean_age          = round(mean(age), 1),
      .groups = "drop"
    )
}

# ── PLOT 1: Encounter frequency ──────────────────────────
plot_encounters <- function(analysis_data) {
  ggplot(analysis_data,
         aes(x = group_label, y = n_encounters, fill = group_label)) +
    geom_boxplot(alpha = 0.8, outlier.alpha = 0.2) +
    scale_fill_manual(values = c("#F07B4C", "#4CAFF0")) +
    labs(title = "Healthcare Encounters by Condition Status",
         x = "", y = "Number of Encounters") +
    theme_minimal(base_size = 13) +
    theme(legend.position = "none")
}

# ── PLOT 2: Encounters by gender ─────────────────────────
plot_encounters_gender <- function(analysis_data) {
  ggplot(analysis_data,
         aes(x = group_label, y = n_encounters, fill = gender)) +
    geom_boxplot(alpha = 0.8, outlier.alpha = 0.2) +
    scale_fill_manual(values = c("#F07B4C", "#4CAFF0")) +
    labs(title = "Encounters by Condition Status and Gender",
         x = "", y = "Number of Encounters", fill = "Gender") +
    theme_minimal(base_size = 13)
}

# ── PLOT 3: Encounters by age group ─────────────────────
plot_encounters_age <- function(analysis_data) {
  ggplot(analysis_data,
         aes(x = age_group, y = n_encounters, fill = group_label)) +
    geom_boxplot(alpha = 0.8, outlier.alpha = 0.2) +
    scale_fill_manual(values = c("#F07B4C", "#4CAFF0"),
                      labels = c("Chronic", "Healthy")) +
    labs(title = "Encounters by Age Group and Condition Status",
         x = "Age Group", y = "Number of Encounters", fill = "") +
    theme_minimal(base_size = 13)
}

# ── PLOT 4: Encounter types ──────────────────────────────
plot_encounter_types <- function(analysis_data, encounters) {
  encounter_types <- encounters |>
    left_join(analysis_data |> select(patient, group_label),
              by = "patient") |>
    filter(!is.na(group_label)) |>
    group_by(group_label, encounterclass) |>
    summarise(n = n(), .groups = "drop") |>
    group_by(group_label) |>
    mutate(pct = round(100 * n / sum(n), 1))

  ggplot(encounter_types,
         aes(x = reorder(encounterclass, -pct), y = pct,
             fill = group_label)) +
    geom_col(position = "dodge", alpha = 0.85) +
    scale_fill_manual(values = c("#F07B4C", "#4CAFF0")) +
    labs(title = "Types of Encounters by Condition Status",
         x = "Encounter Type",
         y = "Percentage of Encounters (%)", fill = "") +
    theme_minimal(base_size = 13) +
    theme(axis.text.x = element_text(angle = 40, hjust = 1))
}

# ── PLOT 5: Race ─────────────────────────────────────────
make_race_table <- function(analysis_data) {
  analysis_data |>
    group_by(race) |>
    summarise(
      n           = n(),
      n_chronic   = sum(has_chronic),
      pct_chronic = round(100 * mean(has_chronic), 1),
      .groups = "drop"
    ) |>
    arrange(desc(pct_chronic))
}

plot_race <- function(race_table) {
  ggplot(race_table,
         aes(x = reorder(race, pct_chronic),
             y = pct_chronic, fill = pct_chronic)) +
    geom_col(alpha = 0.85) +
    geom_text(aes(label = paste0(pct_chronic, "%")),
              hjust = -0.1, size = 3.5) +
    coord_flip() +
    scale_fill_gradient(low = "#4CAFF0", high = "#F07B4C") +
    labs(title = "Chronic Condition Prevalence by Race",
         x = "", y = "% with Chronic Condition") +
    theme_minimal(base_size = 13) +
    theme(legend.position = "none") +
    ylim(0, max(race_table$pct_chronic) + 12)
}

# ── PLOT 6: Mortality ────────────────────────────────────
make_mortality_table <- function(analysis_data) {
  analysis_data |>
    group_by(group_label) |>
    summarise(
      n            = n(),
      n_deceased   = sum(deceased),
      pct_deceased = round(100 * mean(deceased), 1),
      .groups = "drop"
    )
}

plot_mortality <- function(mortality_table) {
  ggplot(mortality_table,
         aes(x = group_label, y = pct_deceased, fill = group_label)) +
    geom_col(alpha = 0.85, width = 0.5) +
    geom_text(aes(label = paste0(pct_deceased, "%")),
              vjust = -0.5, size = 5, fontface = "bold") +
    scale_fill_manual(values = c("#F07B4C", "#4CAFF0")) +
    labs(title = "Mortality Rate by Condition Status",
         x = "", y = "% of Patients Deceased") +
    theme_minimal(base_size = 13) +
    theme(legend.position = "none")
}

# ── PLOT 7 & 8: State analysis ───────────────────────────
make_state_table <- function(analysis_data) {
  analysis_data |>
    filter(!is.na(state)) |>
    group_by(state) |>
    summarise(
      total_patients = n(),
      n_chronic      = sum(has_chronic),
      pct_chronic    = round(100 * mean(has_chronic), 1),
      .groups = "drop"
    ) |>
    arrange(desc(n_chronic))
}

plot_state_count <- function(state_table) {
  state_table |>
    slice_head(n = 15) |>
    ggplot(aes(x = reorder(state, n_chronic),
               y = n_chronic, fill = n_chronic)) +
    geom_col(alpha = 0.85) +
    geom_text(aes(label = n_chronic), hjust = -0.1, size = 3.5) +
    coord_flip() +
    scale_fill_gradient(low = "#4CAFF0", high = "#F07B4C") +
    labs(title = "Top 15 States by Number of Chronic Patients",
         x = "", y = "Number of Chronic Patients") +
    theme_minimal(base_size = 13) +
    theme(legend.position = "none") +
    ylim(0, max(state_table$n_chronic) + 80)
}

plot_state_pct <- function(state_table) {
  state_table |>
    filter(total_patients >= 30) |>
    arrange(desc(pct_chronic)) |>
    slice_head(n = 15) |>
    ggplot(aes(x = reorder(state, pct_chronic),
               y = pct_chronic, fill = pct_chronic)) +
    geom_col(alpha = 0.85) +
    geom_text(aes(label = paste0(pct_chronic, "%")),
              hjust = -0.1, size = 3.5) +
    coord_flip() +
    scale_fill_gradient(low = "#4CAFF0", high = "#F07B4C") +
    labs(title = "Top 15 States by Chronic Condition Prevalence",
         subtitle = "(States with at least 30 patients)",
         x = "", y = "% of Patients with Chronic Condition") +
    theme_minimal(base_size = 13) +
    theme(legend.position = "none") +
    ylim(0, max(state_table$pct_chronic) + 10)
}

# ── STATISTICAL TESTS ────────────────────────────────────
run_stats <- function(analysis_data) {
  healthy <- analysis_data |> filter(!has_chronic) |> pull(n_encounters)
  chronic <- analysis_data |> filter(has_chronic)  |> pull(n_encounters)
  wilcox  <- wilcox.test(chronic, healthy)

  mort_tbl <- table(analysis_data$has_chronic, analysis_data$deceased)
  chisq    <- chisq.test(mort_tbl)

  data.frame(
    test = c("Wilcoxon rank-sum (encounter frequency)",
             "Chi-square (mortality vs chronic status)"),
    statistic = c(round(wilcox$statistic, 1),
                  round(chisq$statistic, 3)),
    p_value = c(format.pval(wilcox$p.value, digits = 4),
                format.pval(chisq$p.value,  digits = 4)),
    conclusion = c(
      ifelse(wilcox$p.value < 0.05,
             "Significant difference in encounter frequency",
             "No significant difference"),
      ifelse(chisq$p.value < 0.05,
             "Significant association with mortality",
             "No significant association")
    )
  )
}
#Leaflet map
make_patient_map <- function(analysis_data, patients) {
  library(leaflet)
  library(dplyr)
  library(tidyr)

  map_data <- analysis_data |>
    # no left_join needed because analysis_data already has lat/lon/city/state
    drop_na(lat, lon) |>
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

  leaflet(map_data) |>
    addProviderTiles(providers$CartoDB.Positron) |>
    addCircleMarkers(
      lng = ~lon, lat = ~lat, radius = 4,
      color = ~color, fillColor = ~color,
      fillOpacity = 0.6, stroke = FALSE, popup = ~label
    ) |>
    addLegend(
      position = "bottomright",
      colors = c("#F07B4C", "#4CAFF0"),
      labels = c("Chronic Patient", "Healthy Patient"),
      title = "Condition Status", opacity = 0.8
    ) |>
    addControl(
      html = "<b>Geographic Distribution of Chronic vs Healthy Patients</b>",
      position = "topright"
    )
}







