# =============================================================================
# guns_county_CDC.R
# Causal Analysis of Household Firearm Ownership Rates and Firearm Mortality
# Author: Joe Sandow, University of Nebraska-Lincoln
# =============================================================================

# -----------------------------------------------------------------------------
# 0. Setup: Clear environment and load libraries
# -----------------------------------------------------------------------------

rm(list = ls())
graphics.off()

library(readr)
library(readxl)
library(haven)
library(labelled)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(broom)
library(plm)
library(lmtest)
library(AER)
library(ivreg)
library(sandwich)
library(ivmodel)
library(stargazer)
library(tinytex)
library(prettyR)
library(eventstudyr)

# -----------------------------------------------------------------------------
# 1. Load raw data
# -----------------------------------------------------------------------------

load("crimeall.RData")

load("LEAIC.rda")
leaic <- da35158.0001
rm(da35158.0001)

load("all_arrests_15_20.RData")
load("RAND_gun.RData")
load("CDC_gun_cleandl.RData")

adjacency2010 <- read_csv(
  "county_adjacency2010.csv",
  col_types = cols(
    fipscounty  = col_number(),
    fipsneighbor = col_number()
  )
)

Florida_crime <- read_csv(
  "Florida_crime.csv",
  col_types = cols(
    Population       = col_number(),
    Total_Index_Crime = col_number()
  ),
  na = c("", "NA", "--")
)

Pop_estimates_US_99_23 <- read_csv(
  "Pop_estimates_US_99_23.csv",
  show_col_types = FALSE
)

state_wcr <- read_csv(
  "state_wcr.csv",
  col_types = cols(
    FIPS_ST = col_number(),
    YEAR    = col_number(),
    st_wcr  = col_number(),
    Year    = col_number()
  )
)

CDC_self_harm <- read_delim(
  "CDC_self_harm_1999_2020.txt",
  delim        = "\t",
  escape_double = FALSE,
  trim_ws      = TRUE,
  show_col_types = FALSE
) |>
  filter(!is.na(Year))

CDC_other_gun_deaths <- read_delim(
  "CDC_other_gun_deaths_1999_2020.txt",
  delim        = "\t",
  escape_double = FALSE,
  trim_ws      = TRUE,
  show_col_types = FALSE
) |>
  filter(!is.na(Year))

guns_county_CDCnt <- read_delim(
  "Underlying Cause of Death, 1999-2020 (9).txt",
  delim        = "\t",
  escape_double = FALSE,
  trim_ws      = TRUE,
  show_col_types = FALSE
) |>
  filter(!is.na(Year))

# -----------------------------------------------------------------------------
# 2. Clean and prepare Florida crime data
# -----------------------------------------------------------------------------

Florida_crime <- Florida_crime %>%
  mutate(WAR = Murder) %>%
  rename(fipsneighbor = fipscounty, YEAR = Year) %>%
  select(fipsneighbor, YEAR, WAR, Population) %>%
  arrange(fipsneighbor, YEAR) %>%
  rename(c_pop = Population)

# -----------------------------------------------------------------------------
# 3. Build panel adjacency table (1999-2020)
# -----------------------------------------------------------------------------

years <- 1999:2020
adjacency <- data.frame()

for (year in years) {
  temp_adjacency <- adjacency2010
  temp_adjacency$YEAR <- year
  adjacency <- rbind(adjacency, temp_adjacency)
}

# -----------------------------------------------------------------------------
# 4. Prepare off-year arrest data (2015, 2017-2020) via LEAIC crosswalk
# -----------------------------------------------------------------------------

# Isolate murder arrests (OFFENSE code "01A") from agency-level data
arrests$t_arrests <- ifelse(arrests$OFFENSE == "01A", arrests$OCCUR, 0)

arrests_w <- arrests %>%
  group_by(ORI7, YEAR, POP) %>%
  summarise(arrests_ag_year = sum(t_arrests), .groups = "drop")

# Standardize ORI7 identifiers for join
arrests_w$ORI7 <- trimws(toupper(as.character(arrests_w$ORI7)))
leaic$ORI7     <- trimws(toupper(as.character(leaic$ORI7)))

# Join to LEAIC crosswalk to obtain FIPS codes
# Remove Florida agencies and state/tribal agencies (NA FIPS)
arrests_w <- left_join(arrests_w, leaic, by = "ORI7") %>%
  filter(FIPS_ST != 12)

# Remove DC FIPS range filter and aggregate to county-year level
arrests_w <- arrests_w %>%
  mutate(FIPS = as.numeric(as.character(FIPS))) %>%
  filter(!(FIPS <= 11999 & FIPS >= 13000))

arrests_off_year <- arrests_w %>%
  group_by(FIPS, YEAR) %>%
  summarise(
    t_cty_arrests_y = sum(arrests_ag_year),
    t_cty_pop       = sum(POP),
    .groups         = "drop"
  ) %>%
  mutate(WAR = t_cty_arrests_y) %>%
  select(-t_cty_arrests_y) %>%
  rename(fipsneighbor = FIPS, c_pop = t_cty_pop) %>%
  arrange(fipsneighbor, YEAR)

# -----------------------------------------------------------------------------
# 5. Prepare main crime data (1999-2014, 2016 from crimeALL)
# -----------------------------------------------------------------------------

crimeALL$WAR <- crimeALL$MURDER
crimeALL_T   <- crimeALL

# Remove tribal (FIPS_CTY 999, 777) and Florida observations
crimeALL_T <- crimeALL_T %>%
  filter(!FIPS_CTY %in% c("999", "777")) %>%
  filter(FIPS_ST != 12)

# Construct numeric FIPS county code
crimeALL_T$FIPS_CTY <- formatC(crimeALL_T$FIPS_CTY, digits = 3, flag = "0", width = 0)
crimeALL_T$FIPS_ST  <- formatC(crimeALL_T$FIPS_ST,  digits = 2, flag = "0", width = 0)

crimeALL_T <- crimeALL_T %>%
  unite(fipsneighbor, FIPS_ST, FIPS_CTY, sep = "", remove = FALSE)

crimeALL_T$fipsneighbor <- as.numeric(crimeALL_T$fipsneighbor)

crime <- crimeALL_T[, c("fipsneighbor", "YEAR", "WAR", "CPOPARST")] %>%
  rename(c_pop = CPOPARST)

# -----------------------------------------------------------------------------
# 6. Construct county_WAR: combine all arrest data sources
# -----------------------------------------------------------------------------

county_WAR <- rbind(crime, arrests_off_year, Florida_crime)
county_WAR$fipsneighbor <- as.double(county_WAR$fipsneighbor)

# -----------------------------------------------------------------------------
# 6a. Construct adjacency variants for robustness checks
# -----------------------------------------------------------------------------

# Within-state adjacency: restrict to neighbors sharing the same state FIPS
adjacency2010_instate <- adjacency2010 %>%
  mutate(
    st_county   = substr(formatC(fipscounty,  width = 5, flag = "0"), 1, 2),
    st_neighbor = substr(formatC(fipsneighbor, width = 5, flag = "0"), 1, 2)
  ) %>%
  filter(st_county == st_neighbor) %>%
  select(countyname, fipscounty, neighborname, fipsneighbor)

# Border county identification: counties with at least one cross-state neighbor
border_counties <- adjacency2010 %>%
  mutate(
    st_county   = substr(formatC(fipscounty,  width = 5, flag = "0"), 1, 2),
    st_neighbor = substr(formatC(fipsneighbor, width = 5, flag = "0"), 1, 2)
  ) %>%
  filter(st_county != st_neighbor) %>%
  pull(fipscounty) %>%
  unique()

# -----------------------------------------------------------------------------
# 7. Construct leave-one-out WAR instrument (WAR_it)
# -----------------------------------------------------------------------------

# Join adjacency table to arrest/population data
# Remove self-neighbors (county i cannot be its own neighbor)
adjacency_w <- left_join(adjacency, county_WAR, by = c("fipsneighbor", "YEAR")) %>%
  filter(fipsneighbor != fipscounty)

# Compute total neighboring county population (denominator for weights)
# na.rm = TRUE ensures counties with some missing neighbors are not dropped entirely
sum_c_pop <- adjacency_w %>%
  group_by(fipscounty, YEAR) %>%
  summarise(neighboring_counties_pop = sum(c_pop, na.rm = TRUE), .groups = "drop")

adjacency_w <- left_join(adjacency_w, sum_c_pop, by = c("fipscounty", "YEAR"))

# Compute population-weighted murder arrest rate and sum across neighbors
# Scaled to per 100,000 population
adjacency_w <- adjacency_w %>%
  mutate(WAR_weighted = WAR / neighboring_counties_pop) %>%
  group_by(fipscounty, YEAR) %>%
  summarise(WAR = sum(WAR_weighted), .groups = "drop") %>%
  filter(!is.na(WAR))

# -----------------------------------------------------------------------------
# 8. Clean CDC WONDER county-level gun death data
# -----------------------------------------------------------------------------

# Remove metadata columns and footer rows (already filtered by !is.na(Year))
guns_county_CDCnt <- guns_county_CDCnt[, -c(1, 7, 10)]

colnames(guns_county_CDCnt) <- c(
  "state", "fipsstate", "County", "fipscounty", "YEAR", "deaths", "population"
)

# Suppressed cells ("Suppressed") and missing populations ("Missing")
# are coerced to NA — this is expected and valid
guns_county_CDCnt$deaths     <- as.double(guns_county_CDCnt$deaths)
guns_county_CDCnt$population <- as.double(guns_county_CDCnt$population)
guns_county_CDCnt$d_rate     <- (guns_county_CDCnt$deaths / guns_county_CDCnt$population)

# -----------------------------------------------------------------------------
# 9. Clean CDC WONDER national self-harm and other gun death data
# -----------------------------------------------------------------------------

# Remove metadata columns and retain first 44 rows (22 years x 2 sexes)
CDC_self_harm        <- CDC_self_harm[, -c(1, 8)]
CDC_self_harm        <- slice(CDC_self_harm, 1:44)
CDC_other_gun_deaths <- CDC_other_gun_deaths[, -c(1, 8)]
CDC_other_gun_deaths <- slice(CDC_other_gun_deaths, 1:44)

colnames(CDC_self_harm)        <- c("Year", "year_code", "gender", "gender_code", "sh_deaths", "Population")
colnames(CDC_other_gun_deaths) <- c("Year", "year_code", "gender", "gender_code", "deaths",    "Population")

self_harm_data <- CDC_self_harm %>%
  group_by(Year) %>%
  summarise(total_deaths = sum(sh_deaths), .groups = "drop") %>%
  mutate(category = "Self-Harm")

all_other_data <- CDC_other_gun_deaths %>%
  group_by(Year) %>%
  summarise(total_deaths = sum(deaths), .groups = "drop") %>%
  mutate(category = "All Other")

combined_data <- rbind(self_harm_data, all_other_data)
result        <- inner_join(self_harm_data, all_other_data, by = "Year")

# -----------------------------------------------------------------------------
# 10. Join RAND HFR to CDC state-level data
# -----------------------------------------------------------------------------

CDC_gun_cleandl          <- left_join(CDC_gun_cleandl, RAND_gun, by = c("Year", "STATE"))
CDC_gun_cleandl$FIPS_ST  <- CDC_gun_cleandl$FIP

# -----------------------------------------------------------------------------
# 11. Descriptive figures
# -----------------------------------------------------------------------------

# National average HFR by year (unweighted state mean)
NatAvg <- aggregate(HFR ~ Year, data = CDC_gun_cleandl, mean)

ggplot(data = NatAvg, aes(x = Year)) +
  geom_line(aes(y = HFR), color = "blue") +
  geom_point(aes(y = HFR), color = "blue", size = 3) +
  scale_x_continuous(limits = c(1999, 2016)) +
  labs(
    title = "Household Firearm Ownership Rate by Year",
    x     = "Year",
    y     = "Household Firearm Ownership Rate"
  ) +
  theme_gray()

# National average gun death rate by year
NatAvg_D <- aggregate(RATE ~ Year, data = CDC_gun_cleandl, mean)

ggplot(data = NatAvg_D, aes(x = Year)) +
  geom_line(aes(y = RATE), color = "blue") +
  geom_point(aes(y = RATE), color = "blue", size = 3) +
  labs(
    title = "Mean Deaths Per 100,000 by Year",
    x     = "Year",
    y     = "Deaths Per 100,000"
  ) +
  theme_gray()

# National average gun death rate by year
# Two series: CDC state-level rate (unsuppressed) vs county estimation 
# sample mean (suppression-affected)
# County-level mean computed from guns_county_CDCnt which is available here
# d_rate is deaths/population (per capita), multiply by 100000 for rate

NatAvg_D <- CDC_gun_cleandl %>%
  filter(!is.na(RATE)) %>%
  group_by(Year) %>%
  summarise(
    state_mean = mean(RATE),
    .groups    = "drop"
  )

county_mean_by_year <- guns_county_CDCnt %>%
  filter(!is.na(d_rate), YEAR >= 1999, YEAR <= 2020) %>%
  group_by(YEAR) %>%
  summarise(
    county_mean = mean(d_rate * 100000, na.rm = TRUE),
    .groups     = "drop"
  ) %>%
  rename(Year = YEAR)

plot_data_D <- left_join(NatAvg_D, county_mean_by_year, by = "Year")

ggplot(data = plot_data_D, aes(x = Year)) +
  geom_line(aes(y = state_mean, color = "State-Level CDC Rate"),
            linewidth = 0.9) +
  geom_point(aes(y = state_mean, color = "State-Level CDC Rate"), size = 3) +
  geom_line(aes(y = county_mean, color = "County Sample Mean"),
            linewidth = 0.9, linetype = "dashed") +
  geom_point(aes(y = county_mean, color = "County Sample Mean"), size = 3) +
  scale_color_manual(
    values = c(
      "State-Level CDC Rate" = "blue",
      "County Sample Mean"   = "red"
    )
  ) +
  labs(
    title = "Mean Firearm Deaths Per 100,000 by Year",
    x     = "Year",
    y     = "Deaths Per 100,000",
    color = ""
  ) +
  theme_gray() +
  theme(legend.position = "bottom")

# Self-harm vs. all other gun deaths by year
ggplot(data = combined_data, aes(x = Year, y = total_deaths, fill = category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Self-Harm vs. All Other Gun Deaths by Year",
    x     = "Year",
    y     = "Total Deaths",
    fill  = "Category"
  ) +
  scale_fill_manual(values = c("Self-Harm" = "blue", "All Other" = "red")) +
  theme_gray()

# Self-harm by sex assigned at birth
ggplot(data = CDC_self_harm, aes(x = Year, y = sh_deaths, fill = gender_code)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Self-Harm by Sex Assigned at Birth",
    x     = "Year",
    y     = "Total Deaths",
    fill  = "Gender"
  ) +
  scale_fill_manual(values = c("M" = "blue", "F" = "red")) +
  theme_gray()

# Leading causes of death among children and adolescents, 1999-2020
# Replicates the structure of Goldstick et al. (2022), NEJM
# Requires: kids_Deaths_1999_2020.csv (CDC WONDER extract, ages 1-19)
kids_deaths <- read_csv(
  "kids_Deaths_1999_2020.csv",
  show_col_types = FALSE
) |>
  filter(!is.na(Year) & !is.na(`ICD-10 113 Cause List`))

kids_deaths <- kids_deaths %>%
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
  ) %>%
  filter(!is.na(cause_label))

# Aggregate all firearm intents into single "Firearm-related injury" series
firearm_total <- kids_deaths %>%
  filter(cause_label %in% c("Firearm suicide", "Firearm homicide",
                             "Firearm unintentional", "Firearm undetermined")) %>%
  group_by(Year) %>%
  summarise(
    `Crude Rate` = sum(`Crude Rate`, na.rm = TRUE),
    cause_label  = "Firearm-related injury",
    .groups      = "drop"
  )

non_firearm <- kids_deaths %>%
  filter(!cause_label %in% c("Firearm suicide", "Firearm homicide",
                              "Firearm unintentional", "Firearm undetermined"))

kids_plot_data <- bind_rows(
  non_firearm %>% select(Year, `Crude Rate`, cause_label),
  firearm_total
)

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

ggplot(
  kids_plot_data %>% filter(cause_label %in% names(cause_colors)),
  aes(x = Year, y = `Crude Rate`, color = cause_label)
) +
  geom_line(linewidth = 0.9) +
  scale_color_manual(values = cause_colors) +
  labs(
    title = "Leading Causes of Death Among Children and Adolescents, 1999-2020",
    x     = "Year",
    y     = "Deaths per 100,000 Children and Adolescents",
    color = ""
  ) +
  theme_gray() +
  theme(legend.position = "right")

# -----------------------------------------------------------------------------
# 12. State-level firearm suicide vs. homicide ratios
# -----------------------------------------------------------------------------

# Load state-level CDC WONDER extract (suicide vs homicide by state and year)
cdc_sh_homicide <- read_csv(
  "guns_sh_vs_homicide_1999_2020.csv",
  show_col_types = FALSE
) |>
  filter(!is.na(State) & !is.na(`ICD-10 113 Cause List`))

# Classify cause and reshape to wide format
cdc_sh_homicide <- cdc_sh_homicide %>%
  mutate(
    cause  = ifelse(grepl("self-harm", `ICD-10 113 Cause List`), "Suicide", "Homicide"),
    Deaths = as.numeric(Deaths)
  ) %>%
  select(State, Year, cause, Deaths) %>%
  pivot_wider(names_from = cause, values_from = Deaths) %>%
  mutate(ratio = Suicide / Homicide)

# Restrict to Idaho and Montana and reshape to long format for grouped bar chart
plot_data <- cdc_sh_homicide %>%
  filter(State %in% c("Idaho", "Montana")) %>%
  select(State, Year, Suicide, Homicide) %>%
  pivot_longer(cols = c(Suicide, Homicide), names_to = "cause", values_to = "Deaths")

# Side-by-side bar charts: one facet per state
ggplot(plot_data, aes(x = Year, y = Deaths, fill = cause)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ State, ncol = 2) +
  labs(
    title = "Firearm Suicides vs. Firearm Homicides, 1999-2020",
    x     = "Year",
    y     = "Deaths",
    fill  = ""
  ) +
  scale_fill_manual(values = c("Suicide" = "blue", "Homicide" = "red")) +
  theme_gray() +
  theme(
    legend.position = "bottom",
    strip.text      = element_text(face = "bold")
  )

# -----------------------------------------------------------------------------
# Number of unsuppressed county-year observations by year
# Unsuppressed defined as non-missing d_rate (deaths > 9)
# Restricted to estimation period 1999-2016 for relevance
# but also showing full 1999-2020 range to see post-2016 trend

unsuppressed_by_year <- guns_county_CDCnt %>%
  group_by(YEAR) %>%
  summarise(
    total_obs       = n(),
    unsuppressed    = sum(!is.na(deaths)),
    suppressed      = sum(is.na(deaths)),
    pct_unsuppressed = round(unsuppressed / total_obs * 100, 1),
    .groups         = "drop"
  ) %>%
  arrange(YEAR)

print(unsuppressed_by_year, n = 22)

ggplot(data = unsuppressed_by_year, aes(x = YEAR)) +
  geom_line(aes(y = unsuppressed), color = "blue", linewidth = 0.9) +
  geom_point(aes(y = unsuppressed), color = "blue", size = 3) +
  geom_vline(xintercept = 2010, linetype = "dashed", color = "gray50") +
  annotate("text", x = 2010.3, y = max(unsuppressed_by_year$unsuppressed) * 0.95,
           label = "McDonald (2010)", hjust = 0, size = 3.5) +
  labs(
    title = "Number of Unsuppressed County-Year Observations by Year",
    x     = "Year",
    y     = "Unsuppressed Counties"
  ) +
  theme_gray()

# -----------------------------------------------------------------------------
# 13. Summary statistics: CDC WONDER county-level coverage
# -----------------------------------------------------------------------------

# Counties with at least one non-suppressed observation
x <- guns_county_CDCnt %>% filter(!is.na(deaths))
a <- unique(x$fipscounty)
b <- unique(guns_county_CDCnt$fipscounty)
c <- unique(guns_county_CDCnt$fipsstate)

# Counties with at least one non-zero death rate
nonzero_num <- x %>% filter(d_rate != 0)
d           <- unique(nonzero_num$fipscounty)

cat("Total counties in dataset:                  ", length(b), "\n")
cat("Counties with >= 1 non-suppressed obs:      ", length(a), "\n")
cat("Share of counties with non-suppressed obs:  ", round(length(a) / length(b), 4), "\n")
cat("Share of obs with non-zero death rate:      ", round(length(nonzero_num$state) / length(x$state), 4), "\n")
cat("Counties with >= 1 non-zero obs:            ", length(d), "\n")
cat("Share of counties with non-zero obs:        ", round(length(d) / length(b), 4), "\n")
cat("Number of states (incl. DC):                ", length(c), "\n")

# Annual population coverage (upper bound)
colnames(Pop_estimates_US_99_23)[1] <- "YEAR"

p <- x %>%
  group_by(YEAR) %>%
  summarise(Y_pop = sum(population, na.rm = TRUE), .groups = "drop") %>%
  left_join(Pop_estimates_US_99_23, by = "YEAR") %>%
  mutate(ratio = Y_pop / Pop)

pp <- mean(p$ratio)
cat("Mean annual population coverage (upper bound):", round(pp, 4), "\n")

ggplot(data = p, aes(x = YEAR)) +
  geom_line(aes(y = ratio), color = "blue") +
  geom_point(aes(y = ratio), color = "blue", size = 3) +
  labs(
    title = "Upper Bound of Population Represented in the Data",
    x     = "Year",
    y     = "Share of the Population"
  ) +
  theme_gray()

# -----------------------------------------------------------------------------
# 14. Merge WAR instrument with CDC county gun death data
# -----------------------------------------------------------------------------

guns_county_CDCnt$fipscounty <- as.double(guns_county_CDCnt$fipscounty)

WAR_county_year <- adjacency_w %>%
  group_by(fipscounty, YEAR) %>%
  summarise(c_WAR = mean(WAR), .groups = "drop")

D_WAR_county_year <- left_join(guns_county_CDCnt, WAR_county_year,
                                by = c("fipscounty", "YEAR"))

# -----------------------------------------------------------------------------
# 15. Construct balanced panel dataset
# -----------------------------------------------------------------------------

# Retain only counties observed in all 22 years (1999-2020)
county_year_counts <- aggregate(
  YEAR ~ fipscounty,
  data = D_WAR_county_year,
  FUN  = function(x) length(unique(x))
)

complete_county_ids  <- county_year_counts$fipscounty[county_year_counts$YEAR == 22]
complete_county_data <- D_WAR_county_year[D_WAR_county_year$fipscounty %in% complete_county_ids, ]

# Decision indicator: 1 if Heller/McDonald holds (post-2010 for most counties;
# post-2008 for DC FIPS codes 11000 and 11001)
complete_county_data$decision <- ifelse(
  (complete_county_data$YEAR > 2008) & (complete_county_data$fipscounty %in% c(11000, 11001)),
  1,
  ifelse(complete_county_data$YEAR > 2010, 1, 0)
)

# -----------------------------------------------------------------------------
# 16. Restrict estimation sample to 1999-2016 and join RAND HFR
# -----------------------------------------------------------------------------

complete_county_data0 <- complete_county_data %>%
  filter(YEAR <= 2016) %>%
  rename(FIPS_ST = fipsstate, Year = YEAR)

complete_county_data0$FIPS_ST <- as.double(complete_county_data0$FIPS_ST)

# Join RAND HFR and CDC state-level data; exclude national total rows (NA state)
complete_county_data0 <- left_join(
  complete_county_data0,
  CDC_gun_cleandl %>% filter(!is.na(FIPS_ST)),
  by = c("FIPS_ST", "Year")
)

# -----------------------------------------------------------------------------
# 17. Instrument validity checks: bivariate correlations
# -----------------------------------------------------------------------------

test  <- lm(c_WAR ~ HFR, data = complete_county_data0)
test1 <- lm(HFR   ~ c_WAR, data = complete_county_data0)
summary(test)
summary(test1)

# -----------------------------------------------------------------------------
# 18. First stage regression (for reporting and Anderson-Rubin test)
# -----------------------------------------------------------------------------

first_stage <- lm(
  HFR ~ c_WAR + universl + permit + factor(FIPS_ST) + factor(Year),
  data = complete_county_data0
)
summary(first_stage)

# Clustered first-stage standard errors
coeftest(
  first_stage,
  vcov. = vcovCL(first_stage, cluster = ~FIPS_ST, data = complete_county_data0)
)

# Anderson-Rubin test (weak-instrument-robust inference)
complete_county_data0_cc <- complete_county_data0[complete.cases(
  complete_county_data0[, c("d_rate", "HFR", "c_WAR", "universl", "permit", "FIPS_ST", "Year")]
), ]

Y <- complete_county_data0_cc$d_rate
D <- complete_county_data0_cc$HFR
Z <- complete_county_data0_cc$c_WAR
X <- model.matrix(~ universl + permit + factor(FIPS_ST) + factor(Year),
                  data = complete_county_data0_cc)[, -1]

iv_model <- ivmodel(Y = Y, D = D, Z = Z, X = X)
summary(iv_model)

# -----------------------------------------------------------------------------
# 19. PLM panel IV regressions (robustness checks)
# -----------------------------------------------------------------------------

IV_plm_no_fixed <- plm(
  d_rate ~ HFR | c_WAR,
  data  = complete_county_data0,
  index = c("fipscounty", "Year"),
  model = "pooling"
)
summary(IV_plm_no_fixed)

IV_plm_fixed <- plm(
  d_rate ~ HFR | c_WAR,
  data   = complete_county_data0,
  index  = c("fipscounty", "Year"),
  model  = "within",
  effect = "twoways"
)
summary(IV_plm_fixed)

IV_plm_fixed1 <- plm(
  d_rate ~ HFR + universl | c_WAR + universl,
  data   = complete_county_data0,
  index  = c("fipscounty", "Year"),
  model  = "within",
  effect = "twoways"
)
summary(IV_plm_fixed1)

IV_plm_fixed2 <- plm(
  d_rate ~ HFR + universl + permit | c_WAR + universl + permit,
  data   = complete_county_data0,
  index  = c("fipscounty", "Year"),
  model  = "within",
  effect = "twoways"
)
summary(IV_plm_fixed2)

IV_plm_fixed3 <- plm(
  d_rate ~ HFR + universl + permit + decision | c_WAR + universl + permit + decision,
  data  = complete_county_data0,
  index = c("fipscounty", "Year"),
  model = "within"
)
summary(IV_plm_fixed3)

# Clustered robust standard errors for PLM models
iv_robust  <- coeftest(IV_plm_fixed,  vcov. = function(x) vcovHC(x, type = "sss", cluster = "group"))
iv1_robust <- coeftest(IV_plm_fixed1, vcov. = function(x) vcovHC(x, type = "sss", cluster = "group"))
iv2_robust <- coeftest(IV_plm_fixed2, vcov. = function(x) vcovHC(x, type = "sss", cluster = "group"))
iv3_robust <- coeftest(IV_plm_fixed3, vcov. = function(x) vcovHC(x, type = "sss", cluster = "group"))

# PLM robustness check tables
stargazer(
  list(IV_plm_no_fixed, IV_plm_fixed1, IV_plm_fixed2, IV_plm_fixed3),
  type      = "text",
  keep.stat = "n",
  omit      = "Constant"
)

stargazer(
  list(iv_robust, iv1_robust, iv2_robust, iv3_robust),
  type      = "text",
  keep.stat = "n",
  omit      = "Constant"
)

# -----------------------------------------------------------------------------
# 20. Primary ivreg IV regressions (main results)
# -----------------------------------------------------------------------------

IV_no_fixed <- ivreg(
  d_rate ~ HFR | c_WAR,
  data = complete_county_data0
)
summary(IV_no_fixed)

IV_fixed <- ivreg(
  d_rate ~ HFR + factor(FIPS_ST) + factor(Year) | c_WAR + factor(FIPS_ST) + factor(Year),
  data = complete_county_data0
)
summary(IV_fixed)

IV_no_fixed_1 <- ivreg(
  d_rate ~ HFR + universl + permit | c_WAR + universl + permit,
  data = complete_county_data0
)
summary(IV_no_fixed_1)

IV_fixed_1 <- ivreg(
  d_rate ~ HFR + universl + permit + factor(FIPS_ST) + factor(Year) |
    c_WAR + universl + permit + factor(FIPS_ST) + factor(Year),
  data = complete_county_data0
)
summary(IV_fixed_1)

IV_fixed_2 <- ivreg(
  d_rate ~ HFR + universl + permit + decision + factor(FIPS_ST) |
    c_WAR + universl + permit + decision + factor(FIPS_ST),
  data = complete_county_data0
)
summary(IV_fixed_2)

# -----------------------------------------------------------------------------
# 21. Clustered standard errors for ivreg models
# -----------------------------------------------------------------------------

iv_robust_no_fixed_1 <- coeftest(
  IV_no_fixed_1,
  vcov. = vcovCL(IV_no_fixed_1, cluster = ~FIPS_ST, data = complete_county_data0)
)

iv_robust_fixed <- coeftest(
  IV_fixed,
  vcov. = vcovCL(IV_fixed, cluster = ~FIPS_ST, data = complete_county_data0)
)

iv_robust_fixed1 <- coeftest(
  IV_fixed_1,
  vcov. = vcovCL(IV_fixed_1, cluster = ~FIPS_ST, data = complete_county_data0)
)

iv_robust_fixed2 <- coeftest(
  IV_fixed_2,
  vcov. = vcovCL(IV_fixed_2, cluster = ~FIPS_ST, data = complete_county_data0)
)

# -----------------------------------------------------------------------------
# 22. Main results table
# -----------------------------------------------------------------------------

# SEs are computed inline inside the stargazer call to ensure they correspond
# exactly to the model objects above — do not pre-compute and store separately

stargazer(
  list(IV_no_fixed_1, IV_fixed_1, IV_fixed_2),
  type           = "text",
  keep.stat      = "n",
  omit           = c("Constant", "factor"),
  dep.var.labels = "Firearm Death Rate",
  column.labels  = c("Pooled IV", "Two-Way FE", "State FE + Decision"),
  se = list(
    sqrt(diag(vcovCL(IV_no_fixed_1, cluster = ~FIPS_ST, data = complete_county_data0))),
    sqrt(diag(vcovCL(IV_fixed_1,    cluster = ~FIPS_ST, data = complete_county_data0))),
    sqrt(diag(vcovCL(IV_fixed_2,    cluster = ~FIPS_ST, data = complete_county_data0)))
  ),
  add.lines = list(
    c("State FE",           "No",         "Yes",        "Yes"),
    c("Year FE",            "No",         "Yes",        "No"),
    c("SE Clustering",      "State",      "State",      "State"),
    c("First-Stage F",      "60.09",      "16.08",      "38.28"),
    c("Wu-Hausman p-value", "<0.001",     "<0.001",     "<0.001"),
    c("Sargan",             "Exactly ID", "Exactly ID", "Exactly ID")
  )
)

stargazer(
  list(IV_no_fixed_1, IV_fixed_1, IV_fixed_2),
  type           = "latex",
  keep.stat      = "n",
  omit           = c("Constant", "factor"),
  dep.var.labels = "Firearm Death Rate",
  column.labels  = c("Pooled IV", "Two-Way FE", "State FE + Decision"),
  se = list(
    sqrt(diag(vcovCL(IV_no_fixed_1, cluster = ~FIPS_ST, data = complete_county_data0))),
    sqrt(diag(vcovCL(IV_fixed_1,    cluster = ~FIPS_ST, data = complete_county_data0))),
    sqrt(diag(vcovCL(IV_fixed_2,    cluster = ~FIPS_ST, data = complete_county_data0)))
  ),
  add.lines = list(
    c("State FE",           "No",         "Yes",        "Yes"),
    c("Year FE",            "No",         "Yes",        "No"),
    c("SE Clustering",      "State",      "State",      "State"),
    c("First-Stage F",      "60.09",      "16.08",      "38.28"),
    c("Wu-Hausman p-value", "<0.001",     "<0.001",     "<0.001"),
    c("Sargan",             "Exactly ID", "Exactly ID", "Exactly ID")
  ),
  label = "tab:main_results",
  title = "IV Estimates of the Effect of Household Firearm Ownership on Firearm Mortality"
)

# -----------------------------------------------------------------------------
# 23. OLS baseline regressions (for comparison)
# -----------------------------------------------------------------------------

OLS       <- lm(d_rate ~ c_WAR, data = D_WAR_county_year)
OLS_fixed <- lm(d_rate ~ c_WAR + fipsstate + factor(YEAR), data = D_WAR_county_year)
summary(OLS)
summary(OLS_fixed)

# -----------------------------------------------------------------------------
# 24. PLM regressions on full 1999-2020 panel (without RAND HFR constraint)
# -----------------------------------------------------------------------------

plm_no_fixed <- plm(d_rate ~ c_WAR, data = complete_county_data)
summary(plm_no_fixed)

plm_fixed <- plm(
  d_rate ~ c_WAR,
  data   = complete_county_data,
  index  = c("fipsstate", "YEAR"),
  model  = "within",
  effect = "twoways"
)
summary(plm_fixed)

plm_fixed_1 <- plm(
  d_rate ~ c_WAR + decision,
  data  = complete_county_data,
  index = c("fipsstate"),
  model = "within"
)
summary(plm_fixed_1)

# -----------------------------------------------------------------------------
# 25. RAND state mean HFR summary
# -----------------------------------------------------------------------------

RAND_state_mean <- RAND_gun %>%
  filter(Year > 1998) %>%
  group_by(FIP, STATE) %>%
  summarise(HFR_m = mean(HFR), .groups = "drop")

# -----------------------------------------------------------------------------
# 26. Robustness
# -----------------------------------------------------------------------------
# Reduced form check (coefficient of WAR from (first_stage/rf_robust):

RF_fixed <- ivreg(
  d_rate ~ c_WAR + universl + permit + factor(FIPS_ST) + factor(Year) |
    c_WAR + universl + permit + factor(FIPS_ST) + factor(Year),
  data = complete_county_data0
)

rf_robust <- coeftest(
  RF_fixed,
  vcov. = vcovCL(RF_fixed, cluster = ~FIPS_ST, data = complete_county_data0)
)

rf_robust

# Robustness: Within-state WAR instrument
# Build panel adjacency table using within-state neighbors only
adjacency_instate <- data.frame()

for (year in years) {
  temp <- adjacency2010_instate
  temp$YEAR <- year
  adjacency_instate <- rbind(adjacency_instate, temp)
}

# Construct within-state leave-one-out WAR
adjacency_w_instate <- left_join(
  adjacency_instate, county_WAR, 
  by = c("fipsneighbor", "YEAR")
) %>%
  filter(fipsneighbor != fipscounty)

sum_c_pop_instate <- adjacency_w_instate %>%
  group_by(fipscounty, YEAR) %>%
  summarise(
    neighboring_counties_pop = sum(c_pop, na.rm = TRUE), 
    .groups = "drop"
  )

adjacency_w_instate <- left_join(
  adjacency_w_instate, sum_c_pop_instate, 
  by = c("fipscounty", "YEAR")
)

adjacency_w_instate <- adjacency_w_instate %>%
  mutate(WAR_weighted = WAR / neighboring_counties_pop) %>%
  group_by(fipscounty, YEAR) %>%
  summarise(WAR_instate = sum(WAR_weighted), .groups = "drop") %>%
  filter(!is.na(WAR_instate))

# Join to estimation sample
WAR_instate_county_year <- adjacency_w_instate %>%
  group_by(fipscounty, YEAR) %>%
  summarise(c_WAR_instate = mean(WAR_instate), .groups = "drop")

complete_county_data0 <- left_join(
  complete_county_data0,
  WAR_instate_county_year %>% rename(Year = YEAR),
  by = c("fipscounty", "Year")
)

# Run IV with fixed effects using the instate WAR
IV_fixed_1_instate <- ivreg(
  d_rate ~ HFR + universl + permit + factor(FIPS_ST) + factor(Year) |
    c_WAR_instate + universl + permit + factor(FIPS_ST) + factor(Year),
  data = complete_county_data0
)
summary(IV_fixed_1_instate)

iv_robust_fixed1_instate <- coeftest(
  IV_fixed_1_instate,
  vcov. = vcovCL(IV_fixed_1_instate, cluster = ~FIPS_ST, data = complete_county_data0)
)

iv_robust_fixed1_instate

# -----------------------------------------------------------------------------
# Filter to remove the border counties
complete_county_data0_interior <- complete_county_data0 %>%
  filter(!fipscounty %in% border_counties)

# Run IV with fixed effects using only non-border counties
IV_fixed_1_interior <- ivreg(
  d_rate ~ HFR + universl + permit + factor(FIPS_ST) + factor(Year) |
    c_WAR + universl + permit + factor(FIPS_ST) + factor(Year),
  data = complete_county_data0_interior
)
summary(IV_fixed_1_interior)

iv_robust_fixed1_interior <- coeftest(
  IV_fixed_1_interior,
  vcov. = vcovCL(IV_fixed_1_interior, cluster = ~FIPS_ST, data = complete_county_data0_interior)
)

iv_robust_fixed1_interior

# -----------------------------------------------------------------------------
# Robustness Table
# -----------------------------------------------------------------------------
# LaTeX table for robustness checks
# Extract clustered SE vectors for each specification

se_main <- sqrt(diag(vcovCL(
  IV_fixed_1, cluster = ~FIPS_ST, data = complete_county_data0
)))

se_instate <- sqrt(diag(vcovCL(
  IV_fixed_1_instate, cluster = ~FIPS_ST, data = complete_county_data0
)))

se_interior <- sqrt(diag(vcovCL(
  IV_fixed_1_interior, cluster = ~FIPS_ST, data = complete_county_data0_interior
)))

stargazer(
  list(IV_fixed_1, IV_fixed_1_instate, IV_fixed_1_interior),
  type           = "latex",
  keep.stat      = "n",
  omit           = c("Constant", "factor"),
  dep.var.labels = "Firearm Death Rate",
  column.labels  = c(
    "Main Specification",
    "Within-State Instrument",
    "Interior Counties"
  ),
  se = list(se_main, se_instate, se_interior),
  add.lines = list(
    c("State FE",             "Yes",        "Yes",        "Yes"),
    c("Year FE",              "Yes",        "Yes",        "Yes"),
    c("SE Clustering",        "State",      "State",      "State"),
    c("Instrument",           "$WAR_{it}$", "$WAR^{in}_{it}$", "$WAR_{it}$"),
    c("Sample",               "Full",       "Full",       "Interior"),
    c("First-Stage F",        "16.08",      "16.81",      "16.29"),
    c("Wu-Hausman p-value",   "<0.001",     "<0.001",     "0.002"),
    c("Sargan",               "Exactly ID", "Exactly ID", "Exactly ID")
  ),
  keep      = c("HFR", "universl", "permit"),
  label     = "tab:robustness",
  title     = paste0(
    "Robustness Checks: IV Estimates of the Effect of Household ",
    "Firearm Ownership on Firearm Mortality"
  ),
  notes = paste0(
    "All specifications instrument household firearm ownership rates ",
    "($HFR_{s,t}$) and include state and year fixed effects. ",
    "Column (1) reproduces the preferred two-way fixed effects specification. ",
    "Column (2) restricts the instrument to within-state neighbors only, ",
    "excluding cross-state neighbor counties from the construction of $WAR_{it}$. ",
    "Column (3) excludes all counties with at least one cross-state neighbor ",
    "from the estimation sample. ",
    "Standard errors clustered at the state level in parentheses. ",
    "$^{*}p<0.1$, $^{**}p<0.05$, $^{***}p<0.01$."
  ),
  notes.align = "l"
)

# -----------------------------------------------------------------------------
# 27. Suppression diagnostics: sample representativeness and missing deaths
# -----------------------------------------------------------------------------

# The CDC WONDER database suppresses county-year observations with fewer than
# 10 recorded deaths to protect individual confidentiality. This suppression
# is not random with respect to the variables of interest. Small, rural counties
# — which tend to have higher household firearm ownership rates and meaningful
# per capita firearm death rates driven primarily by suicide — are
# disproportionately suppressed. The consequence is a two-directional bias:
# suppression pulls the estimation sample mean death rate below the true
# national mean (downward bias on the baseline), while simultaneously removing
# high-ownership, high-per-capita-mortality counties from the sample in a way
# that likely inflates the IV point estimate (upward bias on the coefficient).
# The following diagnostics quantify both dimensions of this bias.

# True national mean firearm death rate (unsuppressed benchmark)
# Uses CDC-computed state-level rates from CDC_gun_cleandl, which are not
# subject to county-level suppression and cover all firearm deaths at the
# state level. The unweighted mean across states is computed for consistency
# with the existing descriptive figures. The estimation sample mean of 7.64
# is the mean of d_rate * 100000 across non-suppressed county-year
# observations in complete_county_data0, which is downward biased due to
# the systematic exclusion of small rural counties by CDC suppression.

true_national_rate <- CDC_gun_cleandl %>%
  filter(!is.na(RATE), Year >= 1999, Year <= 2016) %>%
  group_by(Year) %>%
  summarise(annual_mean = mean(RATE), .groups = "drop") %>%
  summarise(mean_national_rate = round(mean(annual_mean), 2))

estimation_sample_mean <- round(
  mean(complete_county_data0$d_rate * 100000, na.rm = TRUE), 2
)

cat("True national mean firearm death rate (1999-2016):",
    true_national_rate$mean_national_rate, "per 100,000\n")
cat("Estimation sample mean firearm death rate:         ",
    estimation_sample_mean, "per 100,000\n")
cat("Suppression-induced downward bias in sample mean:  ",
    round(true_national_rate$mean_national_rate - estimation_sample_mean, 2),
    "per 100,000\n")

# --- Table 1: Suppression rates by state ------------------------------------
# For each state we compute total county-year observations in the estimation
# period (1999-2016), the number suppressed (deaths is NA), the number
# unsuppressed, the suppression rate, and total reported deaths. This table
# identifies which states contribute least to identification due to suppression
# despite potentially having the strongest signal — high-ownership rural states
# where the causal relationship between ownership and mortality should be most
# visible are systematically underrepresented.

suppression_by_state <- guns_county_CDCnt %>%
  filter(YEAR >= 1999, YEAR <= 2016) %>%
  group_by(fipsstate) %>%
  summarise(
    total_obs       = n(),
    suppressed      = sum(is.na(deaths)),
    unsuppressed    = sum(!is.na(deaths)),
    pct_suppressed  = round(suppressed / total_obs * 100, 1),
    reported_deaths = sum(deaths, na.rm = TRUE),
    .groups         = "drop"
  ) %>%
  arrange(desc(pct_suppressed))

print(suppression_by_state, n = 10)

# --- Table 2: True vs. reported deaths, five highest-ownership states -------
# The five states with the highest mean household firearm ownership rates over
# 1999-2016 are Montana, Wyoming, Alaska, West Virginia, and Idaho (from RAND).
# For each we compare total deaths reported in the county-level estimation
# sample against the true state total from CDC_gun_cleandl, which is compiled
# at the state level and not subject to suppression. The gap between true and
# reported deaths directly quantifies how many firearm deaths are invisible to
# the county-level analysis. The true death rate for each state contextualizes
# its mortality burden relative to the national mean and illustrates that the
# suppressed counties are not low-mortality counties — they are high-per-capita-
# mortality rural counties whose small populations keep annual counts below the
# CDC threshold.

# True state-level totals and rates (no suppression)
true_deaths_high_ownership <- CDC_gun_cleandl %>%
  filter(FIPS_ST %in% c(2, 16, 30, 54, 56), Year >= 1999, Year <= 2016) %>%
  group_by(FIPS_ST, STATE) %>%
  summarise(
    true_total_deaths = sum(Deaths,     na.rm = TRUE),
    true_total_pop    = sum(Population, na.rm = TRUE),
    .groups           = "drop"
  ) %>%
  mutate(true_rate = round(true_total_deaths / true_total_pop * 100000, 1))

# Reported deaths from county-level estimation sample (suppression-affected)
# Note: fipsstate is a zero-padded two-digit character string in guns_county_CDCnt
reported_deaths_high_ownership <- guns_county_CDCnt %>%
  filter(YEAR >= 1999, YEAR <= 2016) %>%
  filter(fipsstate %in% c("02", "16", "30", "54", "56")) %>%
  group_by(fipsstate) %>%
  summarise(
    reported_deaths = sum(deaths, na.rm = TRUE),
    pct_suppressed  = round(sum(is.na(deaths)) / n() * 100, 1),
    .groups         = "drop"
  ) %>%
  mutate(FIPS_ST = as.double(fipsstate))

# Join true and reported for direct comparison
suppression_high_ownership <- left_join(
  true_deaths_high_ownership,
  reported_deaths_high_ownership,
  by = "FIPS_ST"
) %>%
  mutate(
    missing_deaths = true_total_deaths - reported_deaths,
    pct_missing    = round(missing_deaths / true_total_deaths * 100, 1)
  ) %>%
  select(STATE, true_total_deaths, reported_deaths,
         missing_deaths, pct_missing, pct_suppressed, true_rate)

print(suppression_high_ownership)

# --- Directional bias summary -----------------------------------------------
# The suppression_high_ownership table establishes that the counties being
# suppressed in high-ownership states are not low-mortality counties. Each of
# the five highest-ownership states has a true death rate substantially above
# the national unsuppressed mean of 11.77 per 100,000, yet between one-third 
# and two-thirds of their true deaths are invisible to the estimation sample. 
# This pattern is consistent with the argument that suppression induces upward 
# bias in the IV point estimate: the estimation sample overrepresents low-HFR 
# urban counties with above-average absolute death counts, while the instrument 
# c_WAR has more power in those urban counties. Including the suppressed rural 
# counties — which have high HFR but moderate absolute death counts — would 
# raise the sample mean toward the true national rate while attenuating the 
# point estimate, as the additional observations would show that high-HFR 
# counties do not always exhibit proportionally higher death rates once 
# population size is accounted for.

cat("\nSuppression bias summary for five highest-ownership states:\n")
cat("Total true deaths (1999-2016):    ",
    sum(suppression_high_ownership$true_total_deaths), "\n")
cat("Total reported deaths (1999-2016):",
    sum(suppression_high_ownership$reported_deaths), "\n")
cat("Total missing deaths:             ",
    sum(suppression_high_ownership$missing_deaths), "\n")
cat("Share of true deaths missing:     ",
    round(sum(suppression_high_ownership$missing_deaths) /
            sum(suppression_high_ownership$true_total_deaths) * 100, 1), "%\n")

# --- LaTeX output for suppression tables ------------------------------------

# Table 1: Top 10 most suppressed states
# Join state names for readability — fipsstate is character in guns_county_CDCnt
# but FIPS_ST is numeric in CDC_gun_cleandl, so we build a state name lookup
state_lookup <- CDC_gun_cleandl %>%
  filter(!is.na(FIPS_ST)) %>%
  mutate(fipsstate = formatC(FIPS_ST, width = 2, flag = "0")) %>%
  select(fipsstate, STATE) %>%
  distinct()

suppression_by_state_named <- suppression_by_state %>%
  left_join(state_lookup, by = "fipsstate") %>%
  select(STATE, total_obs, suppressed, unsuppressed, 
         pct_suppressed, reported_deaths) %>%
  slice(1:10)

stargazer(
  as.data.frame(suppression_by_state_named),
  type      = "latex",
  summary   = FALSE,
  rownames  = FALSE,
  digits    = 1,
  title     = "County-Year Suppression Rates by State, 1999--2016 (Ten Most Suppressed)",
  label     = "tab:suppression_by_state",
  covariate.labels = c(
    "State", "Total Obs.", "Suppressed", "Unsuppressed",
    "\\% Suppressed", "Reported Deaths"
  ),
  notes = paste0(
    "Suppression occurs when county-year firearm death counts fall below 10, ",
    "per CDC WONDER confidentiality policy. Reported deaths reflect the sum of ",
    "unsuppressed county-year death counts within the estimation sample."
  ),
  notes.align = "l"
)

# Table 2: True vs. reported deaths, five highest-ownership states
stargazer(
  as.data.frame(suppression_high_ownership),
  type      = "latex",
  summary   = FALSE,
  rownames  = FALSE,
  digits    = 1,
  title     = paste0(
    "True versus Reported Firearm Deaths, Five Highest-Ownership States, ",
    "1999--2016"
  ),
  label     = "tab:suppression_high_ownership",
  covariate.labels = c(
    "State",
    "True Deaths",
    "Reported Deaths",
    "Missing Deaths",
    "\\% Missing",
    "\\% Suppressed",
    "True Rate (per 100,000)"
  ),
  notes = paste0(
    "True deaths are drawn from state-level CDC WONDER data not subject ",
    "to suppression. Reported deaths reflect unsuppressed county-year ",
    "observations in the CDC WONDER county-level extract. ",
    "True rate is computed as true total deaths divided by total state ",
    "population over the estimation period, expressed per 100,000. ",
    "The national unsuppressed mean firearm death rate over 1999--2016 ",
    "is ", true_national_rate$mean_national_rate, " per 100,000, computed ",
    "as the unweighted mean of CDC state-level firearm death rates. ",
    "States are ranked by mean household firearm ownership rate over the ",
    "estimation period per RAND Corporation estimates."
  ),
  notes.align = "l"
)



#rstudioapi::getActiveDocumentContext()$path
#file.info("C:/Users/joesa/OneDrive - University of Nebraska/Rworkingfile/guns_county_CDC_formatted.R")$mtime