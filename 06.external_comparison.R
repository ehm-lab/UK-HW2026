################################################################################
# UK-HW2026: 2026 SUMMER HEAT-RELATED EXCESS MORTALITY
# SCRIPT 06: COMPARE RESULTS WITH PUBLISHED RAPID ESTIMATES
################################################################################

################################################################################
# LOAD DAILY RESULTS

# Load daily results for England and for England and Wales
dayage <- readRDS("temp/dayage.rds")
dayeng <- readRDS("temp/dayeng.rds")

# Parse stored date labels
daydate <- as.Date(dimnames(dayage)$date)

################################################################################
# DEFINE COMPARISON FUNCTIONS

# Summarise model estimates for specified dates and geography
fgetmodel <- function(periods, geography) {
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
    geography="England and Wales",
    episode="21-29 May 2026",
    periods=data.frame(start=as.Date("2026-05-21"), end=as.Date("2026-05-29")),
    estimate=553, lower=426, upper=704,
    source="Barnes et al.",#"Barnes et al. rapid attribution estimate"
    url=granthamurl),
  list(
    geography="England and Wales",
    episode="18-28 June 2026",
    periods=data.frame(start=as.Date("2026-06-18"), end=as.Date("2026-06-28")),
    estimate=2183, lower=1582, upper=2724,
    source="Barnes et al.", url=granthamurl),
  list(
    geography="England",
    episode="24-27 May 2026",
    periods=data.frame(start=as.Date("2026-05-24"), end=as.Date("2026-05-27")),
    estimate=753, lower=529, upper=977,
    source="UKHSA",# "UKHSA interim heat mortality monitoring"
    url=ukhsaurl),
  list(
    geography="England",
    episode="21-28 June 2026",
    periods=data.frame(start=as.Date("2026-06-21"), end=as.Date("2026-06-28")),
    estimate=2124, lower=1758, upper=2491,
    source="UKHSA", url=ukhsaurl),
  list(
    geography="England",
    episode="24-27 May and 21-28 June 2026",
    periods=data.frame(start=as.Date(c("2026-05-24", "2026-06-21")),
      end=as.Date(c("2026-05-27", "2026-06-28"))),
    estimate=2877, lower=2476, upper=3278,
    source="UKHSA", url=ukhsaurl))

################################################################################
# CREATE COMPARISON TABLE

# Apply the model summary function to each comparison
comptab <- rbindlist(lapply(compspec, function(x) {
  model <- fgetmodel(x$periods, x$geography)
  
  data.frame(source=x$source,geography=x$geography,episode=x$episode,
    published=x$estimate, publishedlower=x$lower, publishedupper=x$upper,
    model=model["estimate"],modellower=model["lower"],modelupper=model["upper"],
    # difference=model["estimate"]-x$estimate,ratio=model["estimate"]/x$estimate,
    # url=x$url,
    stringsAsFactors=F)
}), fill=T) |>  as.data.frame()

################################################################################
# CREATE READER-FACING COMPARISON TABLE

comptab <- comptab |> 
  transmute(source=source, geography=geography, episode=episode, 
    # Format published estimates and reported intervals
    published=paste0(
      formatC(comptab$published, format="f", big.mark=",", digits=0), " (",
      formatC(comptab$publishedlower, format="f", big.mark=",", digits=0), " to ",
      formatC(comptab$publishedupper, format="f", big.mark=",", digits=0), ")"),
    # Format model estimates and empirical intervals
    model=paste0(
      formatC(comptab$model, format="f", big.mark=",", digits=0), " (",
      formatC(comptab$modellower, format="f", big.mark=",", digits=0), " to ",
      formatC(comptab$modelupper, format="f", big.mark=",", digits=0), ")")
  )

# Rename
names(comptab) <- c("Source", "Geography", "Heat episode",
  "Published rapid estimate (95% CI)", "UK-HW2026 model (95% eCI)")
write.csv(comptab, "tables/TableS1_external_comparison.csv", row.names=F)

# Table note: model estimates use the geography and dates shown.
# Barnes et al. reported eCIs; UKHSA reported CIs. Comparisons are descriptive 
# because the data and methods differ between assessments.
