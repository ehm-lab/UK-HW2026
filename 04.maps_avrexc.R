################################################################################
# UK-HW2026: 2026 SUMMER HEAT-RELATED EXCESS MORTALITY
# SCRIPT 04: CREATE LSOA IMPACT MAPS: excess deaths per million person-days
################################################################################

################################################################################
# PREPARE MAP DATA

daylsoa <- readRDS("temp/daylsoa_2026.rds")
lsoatab <- read.csv("tables/lsoa_period_2026.csv")
daydate <- as.Date(colnames(daylsoa))
totlab <- paste0("Total: ", "1 May-15 Sep")

ladmap <- merge(ladshp,unique(lookup[c("LAD11CD","RGN11CD")]), by="LAD11CD",
  sort=F)

# PREPARE THE FIVE HEATWAVE PERIODS BY LSOA
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
# write.csv(eventdat,"tables/lsoa_hw_2026.csv",row.names=F)

# Set pale-yellow to dark-red palette
colval <- c("white",colorRampPalette(c("yellow", "orange", "red", "red4"))(11))

################################################################################
# MAP PERIOD-LENGTH-STANDARDISED LSOA BURDEN

# Calculate the duration of each period.
hwday <- setNames(as.integer(hwperiod$end-hwperiod$start)+1,hwperiod$label)
studyday <- as.integer(dateend-datestart)+1

# Calculate average daily burden for each heat episode.
eventplot <- data.frame(LSOA11CD=eventdat$LSOA11CD,
  period=as.character(eventdat$heatwave), estimate=pmax(eventdat$estimate, 0),
  population=eventdat$population)
eventplot$nday <- unname(hwday[eventplot$period])
eventplot$dayrate <- eventplot$estimate/
  (eventplot$population*eventplot$nday)*10^6

# Calculate average daily burden for the complete study period.
studyplot <- data.frame(LSOA11CD=lsoatab$LSOA11CD,
  period=totlab, estimate=lsoatab$eststudy,
  population=lsoatab$population, nday=studyday)
studyplot$dayrate <- studyplot$estimate/
  (studyplot$population*studyplot$nday)*10^6

# Combine the five episodes and complete study period.
plotdat <- rbind(eventplot[c("LSOA11CD", "period", "dayrate")],
  studyplot[c("LSOA11CD", "period", "dayrate")])
plotdat$period <- factor(plotdat$period,levels=c(hwperiod$label, totlab))

plotmap <- merge(lsoashp, plotdat, by="LSOA11CD", sort=F)

# Use a common colour scale based on the heat-episode distribution.
plotlim <- unname(quantile(eventplot$dayrate, 0.99, na.rm=T))

plotbreak <- pretty(c(0, plotlim), n=7)
plotbreak <- plotbreak[plotbreak >= 0 & plotbreak <= plotlim]
plotlabel <- format(round(plotbreak, 1), trim=T)
plotlabel[length(plotlabel)] <- ceiling(max(plotdat$dayrate))

# Candidate containing five episodes and the complete study period.
pmap6 <- ggplot(plotmap) +
  geom_sf(aes(fill=dayrate), colour=NA) +
  geom_sf(data=ladmap,fill=NA,colour="grey30",linewidth=0.05) +
  scale_fill_stepsn(colours=colval,limits=c(0, plotlim),breaks=plotbreak,
    labels=plotlabel, oob=scales::squish,
    name="Average daily excess deaths per million population") +
  facet_wrap(~period, ncol=3) +
  theme_void() +
  theme(legend.position="bottom", strip.text=element_text(face="bold")) +
  guides(fill=guide_colorsteps(barwidth=14,barheight=0.6,title.position="top")) +
  labs(title=paste0("Average daily heat-related excess mortality ",
    "by LSOA, summer 2026"))
# graphics.off()
# gc()
ggsave("figures/Fig2_map_avrexc_2026.png",pmap6, width=14, height=9, dpi=300)

# grid::grid.newpage()
# print(pmap6)
