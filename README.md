# Firearm Mortality IV

## Do Guns Make Us Safe? Causal Evidence from a County-Level Panel

**Author:** Joe Sandow  
**Institution:** University of Nebraska-Lincoln  
**Status:** Dissertation Chapter 2 (in progress)

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

CDC suppression of county-year observations with nine or fewer deaths 
disproportionately excludes small rural counties with high ownership rates, 
generating a downward-biased estimation sample mean of 7.64 per 100,000 
relative to the true national unsuppressed mean of 11.77 per 100,000. Three 
robustness checks — a reduced-form specification, restriction of the instrument 
to within-state neighbors only, and exclusion of border counties — confirm the 
directional finding but reveal systematic attenuation of the point estimate 
(from 0.00653 to 0.00434 to 0.00288) as sources of cross-state spatial 
variation are progressively removed. This attenuation is partly attributable 
to the suppression-driven sample composition, which overrepresents large urban 
counties where the instrument has the most power.

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

## Identification Strategy

The instrument is the population-weighted leave-one-out mean murder arrest 
rate of counties neighboring county *i*:

WAR_it = Σ_{j≠i} p_jt × ar_jt

where ar_jt is the murder arrest rate in county j at time t and p_jt is 
the population weight of county j among all counties adjacent to i.

Murder arrest rates are used in preference to murder rates for three reasons. 
First, studies using UCR-based murder rates typically exclude Florida, as the 
state reports crime statistics through the Florida Department of Law Enforcement 
rather than directly to the FBI UCR system. Florida is the third most populous 
state and home to several of the largest metropolitan areas in the sample; 
excluding it would constitute a substantial and non-random omission. The 
administrative act of arresting an individual for murder is defined consistently 
in statute regardless of which state-level reporting system records it, making 
arrest data portable across reporting systems. Second, if firearm ownership 
deters crime, higher local gun ownership would reduce murders in neighboring 
counties, attenuating the first-stage relationship; any bias from this channel 
runs against finding a positive effect of ownership on mortality, making the 
estimates conservative. Third, murder rates are subject to variation in 
reporting practices and case clearance rates across jurisdictions, while arrest 
rates reflect a more uniformly defined administrative event.

---

## Robustness

Three robustness checks address spatial autocorrelation concerns:

1. **Reduced form:** Direct regression of the firearm death rate on WAR_it 
   confirms the instrument operates on the outcome in the expected direction 
   (coefficient = 0.123, p = 0.003, state-clustered).

2. **Within-state instrument:** Restricting WAR_it to within-state neighbors 
   only removes cross-state spatial correlation as a potential source of bias. 
   The HFR coefficient attenuates from 0.00653 to 0.00434 (34% reduction) and 
   loses significance under state-clustered standard errors, though the sign 
   is preserved and the Wu-Hausman test continues to reject exogeneity.

3. **Interior counties only:** Excluding all counties with at least one 
   cross-state neighbor reduces the sample from 19,922 to 13,224 observations. 
   The HFR coefficient attenuates further to 0.00288 (56% reduction from the 
   main specification). The sign remains positive and the Wu-Hausman test 
   rejects exogeneity (p = 0.002).

The monotone attenuation pattern is partly attributable to the suppression-driven 
sample composition, which overrepresents large urban counties where cross-state 
spatial variation is most pronounced.

---

## Suppression and Sample Representativeness

The CDC WONDER database suppresses county-year observations with nine or fewer 
deaths, disproportionately excluding small rural counties with high firearm 
ownership rates. Key findings from the suppression diagnostics:

- The estimation sample mean of 7.64 deaths per 100,000 substantially 
  understates the true national unsuppressed mean of 11.77 per 100,000.
- Across the five highest-ownership states (Montana, Wyoming, Alaska, West 
  Virginia, and Idaho), 7,523 firearm deaths — 51.2% of the true total of 
  14,707 — are invisible to the county-level estimation sample.
- Wyoming loses 66.8% of its true deaths to suppression; West Virginia 59.8%; 
  Montana 49.6%.
- Each of these states has a true firearm death rate substantially above the 
  national unsuppressed mean: Alaska at 18.4, Wyoming at 16.1, Montana at 15.9, 
  West Virginia at 14.4, and Idaho at 12.5 per 100,000.
- The number of unsuppressed county-year observations rises by approximately 
  10% after 2010, consistent with rising firearm mortality pulling previously 
  suppressed observations above the nine-death threshold.

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
same directory as `dissertation_template.tex`.

---

## Contact

Joe Sandow  
jsandow2@huskers.unl.edu  
University of Nebraska-Lincoln
