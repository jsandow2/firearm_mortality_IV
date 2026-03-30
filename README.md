# Firearm Mortality IV

## Do Guns Make Us Safe? Causal Evidence from a County-Level Panel

**Author:** Joe Sandow  
**Institution:** University of Nebraska-Lincoln  
**Status:** Dissertation Chapter 2 (in progress)

---

## Overview

This repository contains the R code for a causal analysis of the effect 
of household firearm ownership rates on total firearm mortality using a 
county-level panel dataset spanning 1999--2016. The analysis employs a 
leave-one-out instrumental variable strategy, using the population-weighted 
mean murder arrest rate of neighboring counties as an instrument for 
state-level household firearm ownership rates.

---

## Main Finding

The preferred two-way fixed effects IV specification indicates that a one 
percentage point increase in the household firearm ownership rate causally 
increases the firearm death rate by approximately 0.065 additional deaths 
per 100,000 population, an increase of roughly 0.85% relative to the 
sample mean. Results are robust to weak-instrument concerns under 
Anderson-Rubin inference.

---

## Repository Structure
```
firearm_mortality_IV/
├── guns_county_CDC_formatted.R   # Main analysis script
├── crime_adjacency.R             # County adjacency construction
├── ReadCrimeData.R               # Crime data reading utilities
├── firearm_mortality_IV.Rproj    # RStudio project file
├── .gitignore                    # Git ignore rules
└── README.md                     # This file
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

---

## Identification Strategy

The instrument is the population-weighted leave-one-out mean murder arrest 
rate of counties neighboring county *i*:

WAR_it = Σ_{j≠i} p_jt × ar_jt

where ar_jt is the murder arrest rate in county j at time t and p_jt is 
the population weight of county j among all counties adjacent to i.

---

## Contact

Joe Sandow  
jsandow2@huskers.unl.edu  
University of Nebraska-Lincoln