# UK-HW2026

This repository contains the code and analysis inputs for the assessment of
heat-related excess mortality in England and Wales from 1 May to 30 September
2026.

The analysis applies an established small-area temperature-mortality framework
to 34,753 Lower Layer Super Output Areas (2011 boundaries). It estimates daily
and cumulative mortality by age group, region, LSOA and five identified
heatwave periods. Empirical 95% confidence intervals are based on 500 Monte
Carlo samples of the model coefficients.

## Repository contents

- `00.pkg.R` loads the required R packages and creates output folders.
- `01.prep.R` reads and validates all inputs and prepares the model objects.
- `02a.impact_totals.R` estimates study-period and heatwave-period totals by
  region and age, including empirical confidence intervals.
- `02b.daily_timeseries.R` estimates daily results by age and the England
  all-age series used in the UKHSA comparisons.
- `02c.lsoa_impacts.R` estimates daily LSOA point estimates used in Figure 2.
- `03.tables.R` creates Table 1, Table S2, detailed result files and Figure 1.
- `04.maps_avrexc.R` creates Figure 2 and its underlying LSOA-period data.
- `05.external_comparison.R` creates Table S1.
- `06.exposure_summary.R` creates the temperature statistics reported in the
  Results and Figure S1.
- `run_all.R` runs the complete analysis in order.
- `data/` contains model, population, mortality and geography inputs.
- `download/` contains only the analysis-ready daily mean and maximum
  temperature files used in the manuscript.
- `data-preparation/` contains the two notebooks used to retrieve the daily
  mean and maximum temperature data.
- `temp/` contains intermediate model results.
- `tables/` and `figures/` contain the generated outputs.

## Software requirements

Run the analysis in R with the following packages installed:

`dlnm`, `data.table`, `dplyr`, `tidyr`, `foreach`, `doParallel`, `MASS`,
`abind`, `ggplot2`, `sf`, `patchwork`, and `arrow`.

The temperature extraction notebooks require Python with `pandas`, `pyarrow`,
`requests`, and `timezonefinder`.

## Analysis inputs

The mortality analysis uses:

- previously estimated LSOA- and age-specific model parameters in `data/`;
- 2020 LSOA population data in `data/population_2020.zip`;
- 2024 regional age-specific mortality rates in
  `data/deathrates_2024.csv`;
- daily mean 2-m temperature in
  `download/LSOA_daily_ERA5_2026m2s_t2m.parquet`.

Daily maximum 2-m temperature in
`download/LSOA_daily_ERA5_2026m2s_tmax.parquet` is used only for the
descriptive exposure statistics and Figure S1. Both temperature files cover
every LSOA and date from 1 May to 30 September 2026. They were retrieved at
LSOA centroids through the Open-Meteo Historical Weather API, using its
historical best-match series: ERA5-Land where available and ERA5 where
ERA5-Land was unavailable. Dates were adjusted to Europe/London local time.

The supplied parquet files are sufficient to reproduce the analysis. The
notebooks
`data-preparation/extract_era5_lsoa_open_meteo_daily_t2m_2026.ipynb` and
`data-preparation/extract_era5_lsoa_open_meteo_daily_tmax_2026.ipynb`
document the extraction of the daily mean and daily maximum temperature files,
respectively. They use relative project paths and require the user's own
Open-Meteo customer API key in the `OPEN_METEO_API_KEY` environment variable.
They are kept separate from the R analysis so that routine reproduction does
not make thousands of API calls.

## Identified heatwave periods

The same five analytical windows are applied across all LSOAs:

1. 22-28 May 2026
2. 18-28 June 2026
3. 6-17 July 2026
4. 29 July-5 August 2026
5. 11-15 August 2026

The periods were informed by UKHSA heat-health alerts and corresponding peaks
in the LSOA-averaged temperature series. They are analytical periods rather
than official UK-wide heatwave definitions.

## Running the analysis

Open `UK-HW2026.Rproj` and run:

```r
source("run_all.R")
```

Alternatively, source scripts `00` through `06` in numerical order. Run the
three `02` scripts sequentially because each uses parallel processing. Script
`06` depends only on objects prepared by `01` and may be run separately after
`00` and `01`.

The simulation seed is fixed in scripts `02a` and `02b`. The three `02`
scripts are the computationally intensive part of the workflow. Supplied
intermediate RDS files in `temp/` allow the table and figure scripts to be
reviewed without repeating those calculations.

## Manuscript outputs

Figures:

- `figures/Fig1_daily_mort_t2m_2026.png`
- `figures/Fig2_map_avrexc_2026.png`
- `figures/FigS1_temperature_extremes_2026.png`

Tables:

- `tables/Table1_summary_2026.csv`
- `tables/TableS1_external_comparison_2026.csv`
- `tables/TableS2_region_age_2026.csv`

The `tables/` folder contains only the three manuscript-facing tables listed
above. Intermediate results are retained as R objects or in `temp/` and are
recreated by the analysis scripts when needed.

## Interpretation

The results are modelled heat-related excess deaths rather than deaths
identified from complete 2026 mortality registrations. The analysis uses
temperature-mortality functions estimated from 2000-2019 data, 2020
population data and 2024 regional mortality rates. Negative LSOA point
estimates are set to zero when calculating the mapped period-specific values;
this does not alter national or regional estimates. Comparisons with Barnes et
al. and UKHSA are descriptive because the assessments differ in data and
methods.
