# UK-HW2026

This repository contains the analysis of heat-related excess mortality in
England and Wales from 1 May to 15 September 2026.

The analysis applies an established small-area temperature-mortality model to
34,753 Lower Layer Super Output Areas (LSOAs; 2011 boundaries). Results are
estimated by date, age group, region, LSOA and heat episode. Empirical 95%
confidence intervals are calculated from 500 simulations of the model
coefficients where required.

## Main inputs

The analysis uses:
  
- previously estimated LSOA- and age-specific model parameters in `data/`;
- 2020 LSOA population data (`data/population_2020.zip`);
- 2024 regional age-specific mortality rates (`data/deathrates_2024.csv`); and
- daily mean 2-m temperature data for every LSOA and study date.

The temperature file is:
  download/LSOA_daily_ERA5_2026_t2m.parquet
It was downloaded through the licensed Open-Meteo Historical Weather API. No
explicit `models` parameter was supplied, so the file uses Open-Meteo default
Best Match historical model selection. Open-Meteo documents Best Match as a 
combination of ECMWF IFS HRES, ERA5 and ERA5-Land. 

The extraction notebook is:
    download/extract_era5_lsoa_locns_open_meteo_daily_t2m_2026.ipynb

The temperature file must contain one complete record for every LSOA and date,
with the variables:
- `LSOA11CD`: 2011 LSOA code;
- `date`: observation date; and
- `t2m`: daily mean 2-m temperature in degrees Celsius.

The Open-Meteo API key must be supplied through the `OPEN_METEO_API_KEY`
environment variable.

## Analytical heat episodes

Five analytical windows are applied across all LSOAs:

1. 22-28 May 2026
2. 18-28 June 2026
3. 6-17 July 2026
4. 29 July-5 August 2026
5. 11-15 August 2026

These windows were informed by UKHSA heat-health information, contemporary
reports and the LSOA-averaged temperature series. They are analytical heat
episodes, not officially defined UK-wide or LSOA-specific heatwaves.

## Run order

Open `UK-HW2026.Rproj`, then run:

    source("00.pkg.R")
    source("01.prep.R")
    source("02a.impact_totals.R")
    source("02b.daily_timeseries.R")
    source("02c.lsoa_impacts.R")
    source("03.tables.R")
    source("04.maps_avrexc.R")
    source("05.external_comparison.R")

Run the three `02` scripts sequentially because each uses parallel processing.

## Scripts

- `00.pkg.R` loads packages and creates output folders.
- `01.prep.R` prepares model, geography, population, mortality and temperature
  data and defines the five heat episodes.
- `02a.impact_totals.R` estimates study-period and heat-episode totals by region
  and age, including empirical 95% confidence intervals.
- `02b.daily_timeseries.R` estimates daily results by age and an England
  all-age series for comparison with UKHSA.
- `02c.lsoa_impacts.R` estimates daily and study-period LSOA point estimates.
- `03.tables.R` creates manuscript tables and the daily mortality-temperature
  figure.
- `04.maps_avrexc.R` maps average daily excess deaths per million population
  during each heat episode. Dividing by the number of days permits comparison
  across episodes of different duration.
- `05.external_comparison.R` compares matched estimates descriptively with the
  rapid assessment (Barnes et al.) and UKHSA interim monitoring results.

## Main outputs

Figures:

- `figures/Fig1_daily_mort_t2m_2026.png`
- `figures/Fig2_map_avrexc_2026.png`

Tables:

- `tables/Table1_summary_2026.csv`
- `tables/TableS1_external_comparison_2026.csv`
- `tables/TableS2_region_age_2026.csv`

Detailed analytical outputs are also saved in `tables/`. Intermediate results
used between scripts are saved as RDS files in `temp/`.

## Interpretation

The results are expected heat-related excess deaths under the model rather
than deaths identified directly from complete 2026 registrations. The analysis
uses risk functions estimated from 2000-2019 data, 2020 population data and
2024 regional mortality rates.

Negative LSOA point estimates are set to zero for mapping only. This does not
alter national or regional estimates. Comparisons with Barnes et al. and UKHSA
are descriptive because the assessments differ in data and methods.
