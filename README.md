# UK-HW2026

This repository contains the analysis of heat-related excess mortality in
England and Wales during summer 2026.

The study period is 1 May to 15 September 2026. The analysis estimates daily
and period-total impacts by age group, region and Lower Layer Super Output Area
(LSOA). Empirical 95% confidence intervals are calculated where required using
500 simulations of the temperature-mortality coefficients.

## Temperature input

The main temperature file is:

    download/LSOA_daily_ERA5_2026_t2m.parquet

The file was downloaded through Open-Meteo. It uses ERA5-Land where available
and ERA5 for coastal LSOAs not covered by ERA5-Land.

The file must contain the following variables:

- `LSOA11CD`: 2011 LSOA code
- `date`: observation date
- `t2m`: daily mean temperature in degrees Celsius

It must contain one complete observation for every LSOA and date from 1 May to
15 September 2026. The file is not tracked by Git and must be placed in the
`download` folder before running the analysis.

The wide LSOA-by-date temperature object created in `01.prep.R` is called
`tmeandat`.

## Main analytical periods

The main analysis uses five prespecified broad heatwave periods:

1. 22-28 May 2026
2. 18-28 June 2026
3. 6-17 July 2026
4. 29 July-5 August 2026
5. 11-15 August 2026

These are analytical periods selected to capture the main heat episodes. They
should not be described as five officially defined national heatwaves. The
dates are specified in `hwperiod` in `01.prep.R`.

An LSOA-specific heatwave definition is retained as a sensitivity analysis. A
local heatwave is defined as at least three consecutive days above that LSOA's
90th percentile of daily mean temperature during the study period.

## Run order

Open `UK-HW2026.Rproj`, then run the scripts in the following order:

    source("00.pkg.R")
    source("01.prep.R")
    source("02a.impact_totals.R")
    source("02b.daily_timeseries.R")
    source("02c.lsoa_impacts.R")
    source("03.tables.R")
    source("04.maps.R")
    source("05.external_comparison.R")

Run the three `02` scripts sequentially. Each script uses parallel processing,
so running them simultaneously would cause unnecessary competition for CPU and
memory.

## Script descriptions

- `00.pkg.R` loads the required packages and creates output folders.
- `01.prep.R` prepares model parameters, population, mortality, temperature,
  geography and heatwave-period data.
- `02a.impact_totals.R` calculates study-period and event totals by region and
  age group, including empirical 95% confidence intervals.
- `02b.daily_timeseries.R` calculates daily England-and-Wales results by age
  group and an England all-age series for comparison with UKHSA.
- `02c.lsoa_impacts.R` calculates daily and period-total LSOA point estimates
  for mapping.
- `03.tables.R` creates national and regional tables and the combined daily
  mortality-temperature figure.
- `04.maps.R` creates the study-period map, five-period maps and maps for the
  hottest consecutive week in July.
- `05.external_comparison.R` descriptively compares the model estimates with
  published Grantham Institute and UKHSA estimates over matching periods and
  geographical areas.

## Main outputs

Tables:

- `tables/total_age_2026.csv`
- `tables/study_region_age_wide_2026.csv`
- `tables/daily_age_2026.csv`
- `tables/external_comparison_2026.csv`

Figures:

- `figures/Fig1_daily_mort_t2m_2026.png`
- `figures/Fig2_map_lsoamort_2026.png`
- `figures/Fig3_map_five_hw_2026.png`
- `figures/Fig4_map_hottest_days_jul2026.png`
- `figures/external_comparison_2026.png`

Intermediate results are saved in the `temp` folder. The `temp`, `download`,
`tables` and `figures` folders are excluded from Git.

## Interpretation

The estimates are modelled heat-related excess deaths rather than counts taken
directly from 2026 death registrations. The risk model, 2020 population data
and 2022 regional mortality rates are retained from the original analysis
framework.

The external comparison is descriptive. The model, Grantham Institute and
UKHSA estimates use different methods, geographical coverage and definitions
and should not be interpreted as directly equivalent estimates.
