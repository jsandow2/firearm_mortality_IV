# Firearm Mortality IV

## Do Guns Make Us Safe? Causal Evidence from a County-Level Panel

**Author:** Joe Sandow  
**Institution:** University of Nebraska-Lincoln  
**Status:** Dissertation Chapter 1 (in progress)

---

## Overview

This repository contains the R code and dissertation LaTeX files for a causal 
analysis of the effect of household firearm ownership rates on total firearm 
mortality using a county-level panel dataset spanning 1999--2016. The analysis 
employs a leave-one-out instrumental variable strategy, using the 
population-weighted mean murder arrest rate of neighboring counties as an 
instrument for state-level household firearm ownership rates.

---

## Main Finding

The preferred two-way fixed effects IV specification indicates that a one 
percentage point increase in the household firearm ownership rate causally 
increases the firearm death rate by approximately 0.00653 deaths per capita, 
equivalent to approximately 6.53 additional firearm deaths per 100,000 
population per percentage point increase in HFR. Results are robust to 
weak-instrument concerns under Anderson-Rubin inference (F = 56.68, p < 0.001), 
with a 95% confidence interval of [3.98, 13.09] additional deaths per 100,000 
per percentage point increase in HFR, converted from the per capita units of 
the estimation.

---

## Repository Structure

```
firearm_mortality_IV/
├── guns_county_CDC_formatted.R           # Main analysis script
├── crime_adjacency.R                     # County adjacency construction
├── ReadCrimeData.R                       # Crime data reading utilities
├── dissertation_template.tex             # UNL nuthesis dissertation template
├── bibliography.bib                      # BibTeX bibliography
├── aea.bst                               # AEA bibliography style file
├── nuthesis.cls                          # UNL dissertation class file
├── firearm_mortality_IV.Rproj            # RStudio project file
├── .gitignore                            # Git ignore rules
├── README.md                             # This file
│
├── Figures/
│   ├── HFR.pdf                           # Household firearm ownership rate by year
│   ├── mean_deaths_per_hund.pdf          # Mean firearm deaths per 100,000 by year;
│   │                                     # two series: state-level CDC rate
│   │                                     # (unsuppressed) and county estimation
│   │                                     # sample mean (suppression-affected)
│   ├── unsuppressed_county_year_         # Number of unsuppressed county-year
│   │   observations.pdf                  # observations by year, 1999--2020;
│   │                                     # documents the suppression mechanism
│   │                                     # and its post-2010 partial correction
│   ├── self_harm_v_other.pdf             # Self-harm vs other gun deaths
│   ├── SH_gender.pdf                     # Self-harm by sex assigned at birth
│   ├── idaho_mon_suicides_homicides.pdf  # Idaho and Montana suicide vs homicide
│   ├── NEJM_reproduction.pdf             # Leading causes of death ages 1--19
│   └── percent_covered.pdf              # Population coverage by year
```

---

## Data Sources

Data files are not committed to this repository. The following files are 
required to run the analysis:

| File | Source | Description |
|------|--------|-------------|
| `crimeall.RData` | ICPSR/FBI UCR | County-level arrests 1999--2014, 2016 |
| `all_arrests_15_20.RData` | FBI UCR agency-level via LEAIC | County-level arrests 2015, 2017--2020 |
| `LEAIC.rda` | ICPSR | Law Enforcement Agency Identifiers Crosswalk |
| `RAND_gun.RData` | RAND Corporation | State-level household firearm ownership estimates 1980--2016 |
| `CDC_gun_cleandl.RData` | CDC WONDER | Cleaned state-level gun death data |
| `county_adjacency2010.csv` | U.S. Census Bureau | 2010 county adjacency file |
| `Florida_crime.csv` | Florida Dept. of Law Enforcement | Florida county crime data 1999--2020 |
| `Pop_estimates_US_99_23.csv` | U.S. Census Bureau | Annual U.S. population estimates |
| `state_wcr.csv` | Constructed | State weighted crime rates |
| `CDC_self_harm_1999_2020.txt` | CDC WONDER | National firearm self-harm deaths by sex 1999--2020 |
| `CDC_other_gun_deaths_1999_2020.txt` | CDC WONDER | National other firearm deaths by sex 1999--2020 |
| `Underlying Cause of Death, 1999-2020 (9).txt` | CDC WONDER | County-level firearm deaths 1999--2020 |
| `guns_sh_vs_homicide_1999_2020.csv` | CDC WONDER | State-level firearm suicide vs homicide 1999--2020 |
| `kids_Deaths_1999_2020.csv` | CDC WONDER | Leading causes of death ages 1--19, 1999--2020 |
| `Total_Index_Crime_by_County.xlsx` | Florida DCLE | Florida Total Index Crime by county |

---

## Requirements

### R Packages
```r
readr, readxl, haven, labelled, tidyverse, dplyr, ggplot2, broom,
plm, lmtest, AER, ivreg, sandwich, ivmodel, stargazer, tinytex,
prettyR, eventstudyr
```

### R Version
Developed under R 4.x

### LaTeX
Requires the `nuthesis.cls` class file (included in repo) and a standard 
LaTeX distribution. Compiled and maintained via Overleaf.

---

## Script Structure

The main analysis script `guns_county_CDC_formatted.R` is organized into 
27 sections:

| Section | Description |
|---------|-------------|
| 0 | Setup and library loading |
| 1 | Load raw data |
| 2 | Clean Florida crime data |
| 3 | Build panel adjacency table (1999--2020) |
| 4 | Prepare off-year arrest data via LEAIC crosswalk |
| 5 | Prepare main crime data (crimeALL) |
| 6 | Construct county_WAR |
| 6a | Construct adjacency variants for robustness checks |
| 7 | Construct leave-one-out WAR instrument |
| 8 | Clean CDC WONDER county-level gun death data |
| 9 | Clean CDC WONDER national self-harm and other gun death data |
| 10 | Join RAND HFR to CDC state-level data |
| 11 | Descriptive figures |
| 12 | State-level firearm suicide vs homicide ratios |
| 13 | Summary statistics: CDC WONDER county-level coverage |
| 14 | Merge WAR instrument with CDC county gun death data |
| 15 | Construct panel dataset |
| 16 | Restrict estimation sample to 1999--2016 and join RAND HFR |
| 17 | Instrument validity checks: bivariate correlations |
| 18 | First stage regression and Anderson-Rubin test |
| 19 | PLM panel IV regressions |
| 20 | Primary ivreg IV regressions (main results) |
| 21 | Clustered standard errors for ivreg models |
| 22 | Main results table |
| 23 | OLS baseline regressions |
| 24 | PLM regressions on full 1999--2020 panel |
| 25 | RAND state mean HFR summary |
| 26 | Robustness checks (reduced form, within-state instrument, interior counties) |
| 27 | Suppression diagnostics: sample representativeness and missing deaths |

---

## Dissertation

The LaTeX dissertation is maintained in Overleaf and synced to this 
repository. The document uses the University of Nebraska-Lincoln `nuthesis` 
class. To compile locally ensure `nuthesis.cls` and `aea.bst` are in the 
same directory as `firearms.tex`.

---

## Contact

Joe Sandow  
jsandow2@huskers.unl.edu  
University of Nebraska-Lincoln
