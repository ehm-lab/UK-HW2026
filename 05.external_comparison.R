################################################################################
# UK-HW2026: 2026 SUMMER HEAT-RELATED EXCESS MORTALITY
# SCRIPT 05: COMPARE RESULTS WITH PUBLISHED RAPID ESTIMATES
################################################################################

################################################################################
# LOAD DAILY RESULTS

# This comparison is descriptive and does not recalibrate the model.
dayage <- readRDS("temp/dayage_2026.rds")
dayeng <- readRDS("temp/dayeng_2026.rds")

daytxt <- dimnames(dayage)$date
daydate <- suppressWarnings(as.Date(daytxt))
needfmt <- is.na(daydate)
daydate[needfmt] <- as.Date(daytxt[needfmt], format="%d %B %Y")

################################################################################
# DEFINE COMPARISON FUNCTIONS

getmodel <- function(periods, geography) {
  dayid <- rep(FALSE, length(daydate))
  
  for(i in seq_len(nrow(periods))) 
    dayid <- dayid | (daydate >= periods$start[i] & daydate <= periods$end[i])
  
  if(!any(dayid))
    stop("An external comparison period is outside the modelled dates")
  
  if(geography == "England and Wales") {
    values <- apply(dayage["all", dayid, , drop=FALSE], 3, sum)
  } else if(geography == "England") {
    values <- colSums(dayeng[dayid, , drop=F])
  } else {stop("Unknown comparison geography")}
  
  c(estimate=unname(values[1]),
    lower=unname(quantile(values[-1], 0.025, names=F)),
    upper=unname(quantile(values[-1], 0.975, names=F)))
}

oneperiod <- function(start, end) 
  data.frame(start=as.Date(start),end=as.Date(end))

twoperiod <- function(start1, end1, start2, end2) 
  data.frame(start=as.Date(c(start1, start2)), end=as.Date(c(end1, end2)))


################################################################################
# ENTER PUBLISHED ESTIMATES

granthamurl <- paste0(
  "https://www.metoffice.gov.uk/about-us/news-and-media/",
  "media-centre/weather-and-climate-news/2026/",
  "more-than-2700-excess-deaths-estimated-in-england-and-wales")

ukhsaurl <- paste0(
  "https://www.gov.uk/government/publications/",
  "interim-heat-mortality-monitoring-report-england-may-and-june-2026/",
  "interim-heat-mortality-monitoring-report-england-may-and-june-2026")

compspec <- list(
  list(
    comparison="Grantham: May heat episode",
    geography="England and Wales",
    dates="21-29 May 2026",
    periods=oneperiod("2026-05-21", "2026-05-29"),
    estimate=550,
    lower=NA_real_,
    upper=NA_real_,
    source="Grantham Institute rapid attribution estimate",
    url=granthamurl),
  list(
    comparison="Grantham: June heat episode",
    geography="England and Wales",
    dates="18-28 June 2026",
    periods=oneperiod("2026-06-18", "2026-06-28"),
    estimate=2200, lower=NA_real_, upper=NA_real_,
    source="Grantham Institute rapid attribution estimate",
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

write.csv(comptab,"tables/external_comparison_2026.csv",row.names=FALSE)

################################################################################
# PLOT COMPARISON

modelplot <- data.frame(
  comparison=comptab$comparison,
  source="UK-HW2026 model",
  estimate=comptab$model,
  lower=comptab$modellower,
  upper=comptab$modelupper)

publishedplot <- data.frame(
  comparison=comptab$comparison,
  source="Published rapid estimate",
  estimate=comptab$published,
  lower=comptab$publishedlower,
  upper=comptab$publishedupper)

plotdat <- rbind(modelplot, publishedplot)
plotdat$comparison <- factor(plotdat$comparison,levels=rev(comptab$comparison))

pcompare <- ggplot(plotdat, aes(x=comparison, y=estimate, colour=source)) +
  geom_errorbar(aes(ymin=lower, ymax=upper),position=position_dodge(width=0.55),
    width=0.18,linewidth=0.5, na.rm=TRUE) +
  geom_point(position=position_dodge(width=0.55), size=2.4) +
  coord_flip() +
  scale_colour_manual(
    values=c("UK-HW2026 model"="firebrick","Published rapid estimate"="black")) +
  labs(x=NULL,y="Heat-related excess deaths",colour=NULL,
    title="Comparison with published rapid heat-mortality estimates",
    subtitle=paste0("Model 95% CIs are shown for every comparison; ",
      "published CIs are shown where available")) +
  theme_bw() +
  theme(panel.grid.minor=element_blank(), legend.position="bottom")

print(pcompare)
ggsave("figures/external_comparison_2026.png",pcompare, 
  width=10, height=6.5, dpi=300)
