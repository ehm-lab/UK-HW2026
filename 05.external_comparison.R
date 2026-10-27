################################################################################
# UK-HW2026: 2026 SUMMER HEAT-RELATED EXCESS MORTALITY
# SCRIPT 05: COMPARE RESULTS WITH PUBLISHED RAPID ESTIMATES
################################################################################

################################################################################
# LOAD DAILY RESULTS

# Load daily results for England and for England and Wales
dayage <- readRDS("temp/dayage_2026.rds")
dayeng <- readRDS("temp/dayeng_2026.rds")

# Parse stored date labels
daydate <- as.Date(dimnames(dayage)$date)
# daytxt <- dimnames(dayage)$date
# daydate <- suppressWarnings(as.Date(daytxt))
# needfmt <- is.na(daydate)
# daydate[needfmt] <- as.Date(daytxt[needfmt], format="%d %B %Y")

################################################################################
# DEFINE COMPARISON FUNCTIONS

# Summarise model estimates for specified dates and geography
getmodel <- function(periods, geography) {
  dayid <- rep(FALSE, length(daydate))
  
  for(i in seq_len(nrow(periods))) 
    dayid <- dayid | (daydate >= periods$start[i] & daydate <= periods$end[i])
  
  if(!any(dayid))
    stop("An external comparison period is outside the modelled dates")
  
  if(geography == "England and Wales") {
    values <- apply(dayage["all", dayid, , drop=F], 3, sum)
  } else if(geography == "England") {
    values <- colSums(dayeng[dayid, , drop=F])
  }
  
  c(estimate=unname(values[1]),
    lower=unname(quantile(values[-1], 0.025, names=F)),
    upper=unname(quantile(values[-1], 0.975, names=F)))
}

# Define one- and two-period date specifications
oneperiod <- function(start, end) 
  data.frame(start=as.Date(start), end=as.Date(end))

twoperiod <- function(start1, end1, start2, end2) 
  data.frame(start=as.Date(c(start1, start2)), end=as.Date(c(end1, end2)))

################################################################################
# ENTER PUBLISHED ESTIMATES

# Define source locations
granthamurl <- "https://doi.org/10.25560/131199"

ukhsaurl <- paste0(
  "https://www.gov.uk/government/publications/",
  "interim-heat-mortality-monitoring-report-england-may-and-june-2026/",
  "interim-heat-mortality-monitoring-report-england-may-and-june-2026")

# Enter the published estimates and matching comparison specifications
compspec <- list(
  list(
    comparison="Grantham: May heat episode",
    geography="England and Wales",
    dates="21-29 May 2026",
    periods=oneperiod("2026-05-21", "2026-05-29"),
    estimate=553,
    lower=426,
    upper=704,
    source="Barnes et al. rapid attribution estimate",
    url=granthamurl),
  list(
    comparison="Grantham: June heat episode",
    geography="England and Wales",
    dates="18-28 June 2026",
    periods=oneperiod("2026-06-18", "2026-06-28"),
    estimate=2183, lower=1582, upper=2724,
    source="Barnes et al. rapid attribution estimate",
    url=granthamurl
  ),
  list(
    comparison="UKHSA: May heat episode",
    geography="England",
    dates="24-27 May 2026",
    periods=oneperiod("2026-05-24", "2026-05-27"),
    estimate=753, lower=529, upper=977,
    source="UKHSA interim heat mortality monitoring",
    url=ukhsaurl),
  list(
    comparison="UKHSA: June heat episode",
    geography="England",
    dates="21-28 June 2026",
    periods=oneperiod("2026-06-21", "2026-06-28"),
    estimate=2124, lower=1758, upper=2491,
    source="UKHSA interim heat mortality monitoring",
    url=ukhsaurl),
  list(
    comparison="UKHSA: May and June combined",
    geography="England",
    dates="24-27 May and 21-28 June 2026",
    periods=twoperiod("2026-05-24", "2026-05-27", "2026-06-21", "2026-06-28"),
    estimate=2877, lower=2476, upper=3278,
    source="UKHSA interim heat mortality monitoring",
    url=ukhsaurl)
)

################################################################################
# CREATE COMPARISON TABLE

# Apply the model summary function to each comparison
comptab <- rbindlist(lapply(compspec, function(x) {
  model <- getmodel(x$periods, x$geography)
  
  data.frame(
    comparison=x$comparison,
    geography=x$geography,
    dates=x$dates,
    model=model["estimate"],
    modellower=model["lower"],
    modelupper=model["upper"],
    published=x$estimate,
    publishedlower=x$lower,
    publishedupper=x$upper,
    difference=model["estimate"]-x$estimate,
    ratio=model["estimate"]/x$estimate,
    source=x$source,
    url=x$url,
    stringsAsFactors=F)
}), fill=T) |>  as.data.frame()

################################################################################
# CREATE READER-FACING COMPARISON TABLE

# Retain geography and dates because comparison definitions differ
table_s1 <- data.frame(
  source=ifelse(grepl("^Grantham",comptab$comparison), "Barnes et al.", "UKHSA"),
  geography=comptab$geography,
  episode=comptab$dates,
  stringsAsFactors=F)

# Format model estimates and empirical intervals
table_s1$model <- paste0(
  formatC(comptab$model, format="f", big.mark=",", digits=0), " (",
  formatC(comptab$modellower, format="f", big.mark=",", digits=0), " to ",
  formatC(comptab$modelupper, format="f", big.mark=",", digits=0), ")")

# Format published estimates and reported intervals
table_s1$published <- formatC(comptab$published,format="f",big.mark=",",digits=0)
hasci <- !is.na(comptab$publishedlower) & !is.na(comptab$publishedupper)
table_s1$published[hasci] <- paste0(
  formatC(comptab$published[hasci],format="f",big.mark=",", digits=0)," (",
  formatC(comptab$publishedlower[hasci],format="f",big.mark=",",digits=0)," to ",
  formatC(comptab$publishedupper[hasci],format="f",big.mark=",",digits=0),")")

names(table_s1) <- c("Source", "Geography", "Heat episode",
  "UK-HW2026 model (95% eCI)", "Published rapid estimate (95% CI)")
write.csv(table_s1, "tables/TableS1_external_comparison_2026.csv", row.names=F)

# Table note: model estimates use the geography and dates shown.
# Barnes et al. reported eCIs; UKHSA reported CIs. Comparisons are descriptive 
# because the data and methods differ between assessments.
