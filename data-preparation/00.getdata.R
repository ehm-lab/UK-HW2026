################################################################################
# UK-HW2026: PREPARE SEASONAL DAILY MORTALITY WEIGHTS
################################################################################

# Download public ONS daily mortality data, estimates a smooth day-of-year 
# pattern for each region and age group (reshaped data from ONS).
#
# Data source: Office for National Statistics. Daily deaths occurrences, England 
# and Wales, 1981 and 2020. Reference number 14173.

################################################################################
# LOAD PACKAGES

library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
# install.packages("pbs")
library(pbs) ; library(splines)

################################################################################
# SET PARAMETERS

# A recent pre-pandemic decade is used.
analysis_years <- 2010:2019

ons_url <- paste0(
  "https://www.ons.gov.uk/file?uri=%2Fpeoplepopulationandcommunity%2F",
  "birthsdeathsandmarriages%2Fdeaths%2Fadhocs%2F",
  "14173dailydeathsoccurrencesenglandandwales1981and2020%2F",
  "dailydeaths19812020adhocfinal.xlsx")

raw_file <- file.path("download", "ons_daily_deaths_1981_2020.xlsx")
output_file <- file.path("data", "mortality_doy.csv")

################################################################################
# DOWNLOAD AND RESHAPE THE PUBLIC ONS DATA
dir.create("download", showWarnings=F)
if(!file.exists(raw_file)) download.file(ons_url, raw_file, mode="wb", quiet=F)
mortality <- read_excel(raw_file, sheet="Table 2", skip=4) 
mortality <- mortality |>
  dplyr::rename(RGN11CD=`Region code`, agegr=Age, month=Month, day=Day) |>
  dplyr::select(RGN11CD,agegr,month,day,all_of(as.character(analysis_years))) |>
  pivot_longer(cols=all_of(as.character(analysis_years)), names_to="year", 
    values_to="deaths") |>
  dplyr::mutate(year=as.integer(year),
    date=lubridate::make_date(year,month,day)) |>
  # Remove 29 February so every year maps to days 1-365.
  filter(!is.na(date), !is.na(deaths), 
    !(lubridate::month(date)==2L & lubridate::mday(date)==29L)) |>
  dplyr::mutate(doy=lubridate::yday(date)-if_else(lubridate::leap_year(date) &
      lubridate::month(date)>2L, 1L, 0L),
    RGN11CD=if_else(RGN11CD=="W99999999", "W92000004", RGN11CD),
    agegr=recode(agegr,"Under 65"="age064","65-74"="age6574","75-84"="age7584",
      "85 and over"="age85plus")) |>
  dplyr::select(RGN11CD, agegr, date, doy, deaths)

################################################################################
# SAVE THE COMPACT DATASET
write.csv(mortality, output_file, row.names=F, quote=F)
