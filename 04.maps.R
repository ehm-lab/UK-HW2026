################################################################################
# UK-HW2026: 2026 SUMMER HEAT-RELATED EXCESS MORTALITY
# SCRIPT 04: CREATE LSOA IMPACT MAPS
################################################################################

################################################################################
# PREPARE MAP DATA

daylsoa <- readRDS("temp/daylsoa_2026.rds")
lsoatab <- read.csv("tables/lsoa_period_2026.csv")

ladmap <- merge(ladshp, unique(lookup[c("LAD11CD", "RGN11CD")]),by="LAD11CD",
  sort=F)

# Set pale-yellow to dark-red palette.
colval <- c("white",colorRampPalette(c("yellow", "orange", "red", "red4"))(11))

daytxt <- colnames(daylsoa)
daydate <- suppressWarnings(as.Date(daytxt))
needfmt <- is.na(daydate)
daydate[needfmt] <- as.Date(daytxt[needfmt], format="%d %B %Y")

################################################################################
# MAP THE COMPLETE STUDY PERIOD

studymap <- merge(lsoashp,lsoatab[c("LSOA11CD","ratestudy")],by="LSOA11CD",
  sort=F)

studylim <- unname(quantile(studymap$ratestudy, 0.99, na.rm=TRUE))
if(!is.finite(studylim) || studylim <= 0) studylim <- 1

studymax <- round(max(studymap$ratestudy, na.rm=TRUE))
studybreak <- pretty(c(0, studylim), n=6)
studybreak <- studybreak[studybreak >= 0 & studybreak < studylim]
studybreak <- sort(unique(c(studybreak, studylim)))
studylabel <- format(round(studybreak, 0), trim=TRUE)
studylabel[length(studylabel)] <- studymax
# studylabel[length(studylabel)] <- paste0( format(round(studylim, 0), trim=T),
#   "+ (max ",  format(round(studymax, 0), trim=TRUE),")")

pstudy <- ggplot(studymap) +
  geom_sf(aes(fill=ratestudy), colour=NA) +
  geom_sf(data=ladmap,fill=NA,colour="grey30",linewidth=0.08) +
  scale_fill_stepsn(colours=colval,limits=c(0, studylim),breaks=studybreak,
    labels=studylabel,oob=scales::squish, name="Excess deaths per million") +
  theme_void() +
  theme(legend.position="bottom") +
  guides(fill=guide_colorsteps(barwidth=14,barheight=0.6,title.position="top"))+
  labs(title="Heat-related excess mortality by LSOA, summer 2026",
    subtitle=paste0("All ages; total point estimate, ", periodlab))

print(pstudy)
ggsave("figures/Fig2_map_lsoamort_2026.png", pstudy,width=8,height=10,dpi=300)

################################################################################
# MAP THE FIVE HEATWAVE PERIODS

eventest <- matrix(NA_real_,nrow=nrow(daylsoa),ncol=nrow(hwperiod),
  dimnames=list(rownames(daylsoa), hwperiod$label))

for(k in seq_len(nrow(hwperiod))) {
  dayid <- which(daydate >= hwperiod$start[k] & daydate <= hwperiod$end[k])
  if(!length(dayid)) stop("A heatwave period is outside the daily LSOA results")
  eventest[, k] <- rowSums(daylsoa[, dayid, drop=F],na.rm=T)
}

eventdat <- as.data.table(as.table(eventest))
names(eventdat) <- c("LSOA11CD", "heatwave", "estimate")
eventdat$LSOA11CD <- as.character(eventdat$LSOA11CD)
eventdat$heatwave <- factor(as.character(eventdat$heatwave),levels=hwperiod$label)
eventdat <- merge(eventdat, lsoatab[c("LSOA11CD", "population")], by="LSOA11CD")
eventdat$rate <- eventdat$estimate/eventdat$population*10^6
write.csv(eventdat,"tables/lsoa_heatwaves_2026.csv",row.names=FALSE)


eventmap <- merge(lsoashp, eventdat, by="LSOA11CD",sort=FALSE)

eventlim <- unname(quantile(eventmap$rate, 0.99, na.rm=TRUE))
if(!is.finite(eventlim) || eventlim <= 0)  eventlim <- 1

eventmax <- round(max(eventmap$rate, na.rm=TRUE))
eventbreak <- pretty(c(0, eventlim), n=6)
eventbreak <- eventbreak[eventbreak >= 0 & eventbreak < eventlim]
eventbreak <- sort(unique(c(eventbreak, eventlim)))
eventlabel <- format(round(eventbreak, 1), trim=TRUE)
eventlabel[length(eventlabel)] <- eventmax

pevent <- ggplot(eventmap) +
  geom_sf(aes(fill=rate), colour=NA) +
  geom_sf(data=ladmap,fill=NA,colour="grey30",linewidth=0.05) +
  scale_fill_stepsn(colours=colval, limits=c(0, eventlim),breaks=eventbreak,
    labels=eventlabel, oob=scales::squish, name="Excess deaths per million") +
  facet_wrap(~heatwave, ncol=3) +
  theme_void() +
  theme(legend.position="bottom") +
  guides(fill=guide_colorsteps(barwidth=14,barheight=0.6,title.position="top"))+
  labs(title="Heat-related excess mortality in five heatwave periods, 2026",
    subtitle="All ages; period-total point estimates")

print(pevent)
ggsave("figures/Fig3_map_five_hw_2026.png",pevent,width=13,height=9,dpi=300)

# pmapmain <- pevent/pstudy + plot_layout(heights=c(1.7, 1))
# ggsave("figures/map_hw_study_2026.png",pmapmain,width=13,height=16,dpi=300)

################################################################################
# MAP THE SIX DAYS WITH THE LARGEST NATIONAL IMPACT

natday <- colSums(daylsoa, na.rm=TRUE)
mapid <- order(natday, decreasing = TRUE)[seq_len(min(nmapday, length(natday)))]
mapdate <- daydate[mapid]

mapdat <- as.data.table(as.table(daylsoa[, mapid, drop=F]))
names(mapdat) <- c("LSOA11CD", "daytext", "estimate")
mapdat$LSOA11CD <- as.character(mapdat$LSOA11CD)
mapdat$date <- daydate[match(as.character(mapdat$daytext), daytxt)]
mapdat$daytext <- NULL
mapdat <- merge(mapdat, lsoatab[c("LSOA11CD", "population")], by = "LSOA11CD")
mapdat$rate <- mapdat$estimate/mapdat$population*10^6

daymap <- merge(lsoashp, mapdat, by = "LSOA11CD", sort=F)

daylim <- unname(quantile(daymap$rate, 0.99, na.rm=T))
if(!is.finite(daylim) || daylim <= 0) daylim <- 1

daymax <- round(max(daymap$rate, na.rm=TRUE))
daybreak <- pretty(c(0, daylim), n=6)
daybreak <- daybreak[daybreak >= 0 & daybreak < daylim]
daybreak <- sort(unique(c(daybreak, daylim)))
daylabel <- format(round(daybreak, 1), trim=TRUE)
daylabel[length(daylabel)] <- daymax
# paste0(format(round(daylim,1),trim=T),"+ (max ",format(round(daymax,1),trim=T),")")

pday <- ggplot(daymap) +
  geom_sf(aes(fill=rate), colour=NA) +
  geom_sf(data=ladmap,fill=NA,colour="grey30",linewidth=0.05) +
  scale_fill_stepsn(colours=colval,limits=c(0, daylim),breaks=daybreak,
    labels=daylabel,oob=scales::squish, name="Excess deaths per million") +
  facet_wrap(~date, ncol=3) +
  theme_void() +
  theme(legend.position="bottom") +
  guides(fill=guide_colorsteps(barwidth=14,barheight=0.6,title.position="top"))+
  labs(title="Daily heat-related excess mortality by LSOA, 2026",
    subtitle="Six days with the largest national point estimates")

# print(pday)
# ggsave("figures/FigS1_map_daily_top6_2026.png", pday, width=13, height=10, dpi=300)

# write.csv(data.frame(date=mapdate, estimate=natday[mapid]),
#   "tables/map_daily_dates_2026.csv",row.names=F)

################################################################################
# MAP THE HOTTEST CONSECUTIVE WEEK IN JULY

tempdate <- dates
nattemp <- colMeans(data.matrix(tmeandat[-1]),na.rm=T)
julyid <- which(format(tempdate, "%Y-%m") == "2026-07")

startid <- julyid[seq_len(length(julyid)-6)]
weektemp <- sapply(startid, function(i) mean(nattemp[i:(i+6)], na.rm=TRUE))
weekstart <- startid[which.max(weektemp)]
weekid <- weekstart:(weekstart+6)
weekdate <- tempdate[weekid]

resultid <- match(weekdate, daydate)

weekdat <- as.data.table(as.table(daylsoa[, resultid, drop=F]))
names(weekdat) <- c("LSOA11CD", "daytext", "estimate")
weekdat$LSOA11CD <- as.character(weekdat$LSOA11CD)
weekdat$date <- daydate[match(as.character(weekdat$daytext), daytxt)]
weekdat$daytext <- NULL
weekdat <- merge(weekdat, lsoatab[c("LSOA11CD", "population")], by="LSOA11CD")
weekdat$rate <- weekdat$estimate/weekdat$population*10^6

weekmap <- merge(lsoashp,weekdat,by="LSOA11CD",sort=F)

weeklim <- unname(quantile(weekmap$rate, 0.99, na.rm=TRUE))
if(!is.finite(weeklim) || weeklim <= 0) weeklim <- 1

weekmax <- round(max(weekmap$rate, na.rm=TRUE))
weekbreak <- pretty(c(0, weeklim), n=6)
weekbreak <- weekbreak[weekbreak >= 0 & weekbreak < weeklim]
weekbreak <- sort(unique(c(weekbreak, weeklim)))
weeklabel <- format(round(weekbreak, 1), trim=T)
weeklabel[length(weeklabel)] <- weekmax

pweek <- ggplot(weekmap) +
  geom_sf(aes(fill=rate), colour=NA) +
  geom_sf(data=ladmap,fill=NA,colour="grey30",linewidth=0.05) +
  scale_fill_stepsn(colours=colval,limits=c(0, weeklim),breaks=weekbreak,
    labels=weeklabel, oob=scales::squish, name="Excess deaths per million") +
  facet_wrap(~date, ncol=4) +
  theme_void() +
  theme(legend.position="bottom") +
  guides(fill=guide_colorsteps(barwidth=14,barheight=0.6,title.position="top"))+
  labs(title=paste0("Daily heat-related excess mortality rate ",
      "during the hottest days of July 2026"),
    subtitle=paste(format(min(weekdate), "%d %B %Y"),"to",
      format(max(weekdate), "%d %B %Y")))
print(pweek)
ggsave("figures/Fig4_map_hottest_days_jul2026.png",pweek,width=14,height=9,dpi=300)

# write.csv(data.frame(date=weekdate, meantemp=round(nattemp[weekid], 2)),
#   "tables/hottest_days_july_2026.csv",row.names=F)
