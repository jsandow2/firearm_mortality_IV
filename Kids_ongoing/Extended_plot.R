library(tidyverse)

# ── helper: shared cleaning/labelling logic ──────────────────────────────────
label_deaths <- function(df) {
  df |>
    filter(!is.na(Year) & !is.na(`ICD-10 113 Cause List`)) |>
    mutate(
      `Crude Rate` = as.numeric(`Crude Rate`),
      cause_label  = case_when(
        grepl("Motor vehicle", `ICD-10 113 Cause List`)                   ~ "Motor vehicle crash",
        grepl("Malignant neoplasms", `ICD-10 113 Cause List`)             ~ "Malignant neoplasm",
        grepl("cardiovascular", `ICD-10 113 Cause List`)                  ~ "Heart disease",
        grepl("Chronic lower respiratory", `ICD-10 113 Cause List`)       ~ "Chronic respiratory disease",
        grepl("Congenital malformations", `ICD-10 113 Cause List`)        ~ "Congenital anomalies",
        grepl("drowning", `ICD-10 113 Cause List`, ignore.case = TRUE)    ~ "Drowning",
        grepl("smoke, fire", `ICD-10 113 Cause List`, ignore.case = TRUE) ~ "Fire or burns",
        grepl("poisoning", `ICD-10 113 Cause List`, ignore.case = TRUE)   ~ "Drug overdose and poisoning",
        grepl("suicide.*firearm|firearm.*suicide",
              `ICD-10 113 Cause List`, ignore.case = TRUE)                ~ "Firearm suicide",
        grepl("homicide.*firearm|firearm.*homicide|Assault.*firearm",
              `ICD-10 113 Cause List`, ignore.case = TRUE)                ~ "Firearm homicide",
        grepl("Accidental discharge", `ICD-10 113 Cause List`)            ~ "Firearm unintentional",
        grepl("undetermined intent", `ICD-10 113 Cause List`,
              ignore.case = TRUE)                                          ~ "Firearm undetermined",
        TRUE                                                               ~ NA_character_
      )
    ) |>
    filter(!is.na(cause_label))
}

# ── load and label both files ────────────────────────────────────────────────
d1 <- read_csv("kids_Deaths_1999_2020.csv", show_col_types = FALSE) |> label_deaths()
d2 <- read_csv("kids_Deaths_2018_2024.csv", show_col_types = FALSE) |> label_deaths()

# flag which cause_labels are gun-related
gun_labels <- c("Firearm suicide", "Firearm homicide",
                "Firearm unintentional", "Firearm undetermined")

# ── merge function ────────────────────────────────────────────────────────────
# overlap_rule: "gun_conservative" = min for guns / max for others
#               "average"          = mean for all overlapping rows
merge_deaths <- function(d1, d2, overlap_rule = c("gun_conservative", "average")) {
  overlap_rule <- match.arg(overlap_rule)
  
  # join keys — adjust if your data also has a State column
  keys <- c("Year", "ICD-10 113 Cause List", "cause_label")
  
  overlap_years <- intersect(d1$Year, d2$Year)
  
  # non-overlapping rows come straight from each source
  non_overlap <- bind_rows(
    d1 |> filter(!Year %in% overlap_years),
    d2 |> filter(!Year %in% overlap_years)
  )
  
  # overlapping rows: full join then resolve
  resolved <- full_join(
    d1 |> filter(Year %in% overlap_years),
    d2 |> filter(Year %in% overlap_years),
    by      = keys,
    suffix  = c("_1999", "_2018")
  ) |>
    mutate(
      `Crude Rate` = case_when(
        # only one source has a value — take it
        is.na(`Crude Rate_1999`) ~ `Crude Rate_2018`,
        is.na(`Crude Rate_2018`) ~ `Crude Rate_1999`,
        # both present: apply chosen rule
        overlap_rule == "gun_conservative" & cause_label %in% gun_labels ~
          pmin(`Crude Rate_1999`, `Crude Rate_2018`, na.rm = TRUE),
        overlap_rule == "gun_conservative" ~
          pmax(`Crude Rate_1999`, `Crude Rate_2018`, na.rm = TRUE),
        overlap_rule == "average" ~
          (`Crude Rate_1999` + `Crude Rate_2018`) / 2
      )
    ) |>
    select(-`Crude Rate_1999`, -`Crude Rate_2018`)
  
  bind_rows(non_overlap, resolved) |>
    arrange(Year, cause_label)
}

# ── produce both merged datasets ─────────────────────────────────────────────
kids_deaths_conservative <- merge_deaths(d1, d2, overlap_rule = "gun_conservative")
kids_deaths_average       <- merge_deaths(d1, d2, overlap_rule = "average")

plot_kids_deaths <- function(data,
                             title = "Leading Causes of Death Among Children and Adolescents") {
  
  gun_labels <- c("Firearm suicide", "Firearm homicide",
                  "Firearm unintentional", "Firearm undetermined")
  
  cause_colors <- c(
    "Motor vehicle crash"         = "#1f77b4",
    "Firearm-related injury"      = "#ff7f0e",
    "Malignant neoplasm"          = "#7f7f7f",
    "Drug overdose and poisoning" = "#bcbd22",
    "Congenital anomalies"        = "#17becf",
    "Heart disease"               = "#9467bd",
    "Drowning"                    = "#8c564b",
    "Fire or burns"               = "#e377c2",
    "Chronic respiratory disease" = "#2ca02c"
  )
  
  firearm_total <- data |>
    filter(cause_label %in% gun_labels) |>
    group_by(Year) |>
    summarise(`Crude Rate` = sum(`Crude Rate`, na.rm = TRUE),
              cause_label  = "Firearm-related injury",
              .groups      = "drop")
  
  plot_data <- bind_rows(
    data |>
      filter(!cause_label %in% gun_labels) |>
      select(Year, `Crude Rate`, cause_label),
    firearm_total
  ) |>
    filter(cause_label %in% names(cause_colors))
  
  year_range <- range(plot_data$Year, na.rm = TRUE)
  auto_title <- glue::glue("{title}, {year_range[1]}\u2013{year_range[2]}")
  
  ggplot(plot_data, aes(x = Year, y = `Crude Rate`, color = cause_label)) +
    geom_line(linewidth = 0.9) +
    scale_color_manual(values = cause_colors) +
    labs(
      title = auto_title,
      x     = "Year",
      y     = "Deaths per 100,000 Children and Adolescents",
      color = ""
    ) +
    theme_gray() +
    theme(legend.position = "right")
}

plot_kids_deaths(kids_deaths_conservative)
plot_kids_deaths(kids_deaths_average)

# override the title stub if needed
plot_kids_deaths(kids_deaths_conservative,
                 title = "Leading Causes of Child Death (Conservative Firearm Estimate)")

rescale_d2_to_d1 <- function(d1, d2) {
  keys <- c("Year", "ICD-10 113 Cause List", "cause_label")
  
  overlap_years <- intersect(d1$Year, d2$Year)
  
  # compute per-cause scaling factor from overlap years
  scale_factors <- inner_join(
    d1 |> filter(Year %in% overlap_years),
    d2 |> filter(Year %in% overlap_years),
    by     = keys,
    suffix = c("_d1", "_d2")
  ) |>
    group_by(cause_label) |>
    summarise(
      scale = mean(`Crude Rate_d1`, na.rm = TRUE) /
        mean(`Crude Rate_d2`, na.rm = TRUE),
      .groups = "drop"
    )
  
  # rescale d2's post-2020 rows to d1's rate scale
  d2_rescaled <- d2 |>
    filter(!Year %in% overlap_years) |>          # only the new years
    left_join(scale_factors, by = "cause_label") |>
    mutate(`Crude Rate` = `Crude Rate` * scale) |>
    select(-scale)
  
  # keep d1 intact; append only the rescaled extension
  bind_rows(d1, d2_rescaled) |>
    arrange(Year, cause_label)
}

kids_deaths_rescaled <- rescale_d2_to_d1(d1, d2)
plot_kids_deaths(kids_deaths_rescaled)
