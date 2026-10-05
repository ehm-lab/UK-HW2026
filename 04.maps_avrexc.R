################################################################################
# UK-HW2026: 2026 SUMMER HEAT-RELATED EXCESS MORTALITY
# SCRIPT 04: MAP AVERAGE DAILY LSOA IMPACTS FOR FIVE HEAT EPISODES
################################################################################

################################################################################
# PREPARE MAP DATA

daylsoa <- readRDS("temp/daylsoa_2026.rds")
lsoatab <- read.csv("tables/lsoa_period_2026.csv")
daydate <- as.Date(colnames(daylsoa))

ladmap <- merge(ladshp, unique(lookup[c("LAD11CD", "RGN11CD")]), by="LAD11CD",
  sort=F)

################################################################################
# CALCULATE IMPACTS FOR THE FIVE HEAT EPISODES

eventest <- matrix(NA_real_,nrow=nrow(daylsoa),ncol=nrow(hwperiod),
  dimnames=list(rownames(daylsoa), hwperiod$label))

for(k in seq_len(nrow(hwperiod))) {
  dayid <- which(daydate >= hwperiod$start[k] & daydate <= hwperiod$end[k])
  if(!length(dayid)) {
    stop("A heat-episode period is outside the daily LSOA results")
  }
  eventest[, k] <- pmax(rowSums(daylsoa[, dayid, drop=F],na.rm=T),0)
}

eventdat <- as.data.table(as.table(eventest))
names(eventdat) <- c("LSOA11CD", "period", "estimate")
eventdat$LSOA11CD <- as.character(eventdat$LSOA11CD)
eventdat$period <- factor(as.character(eventdat$period),levels=hwperiod$label)
eventdat <- merge(eventdat, lsoatab[c("LSOA11CD", "population")], by="LSOA11CD")

################################################################################
# CALCULATE AVERAGE DAILY BURDEN

hwday <- setNames(as.integer(hwperiod$end-hwperiod$start)+1,hwperiod$label)

eventdat$nday <- unname(hwday[as.character(eventdat$period)])
eventdat$dayrate <- eventdat$estimate/
  (eventdat$population*eventdat$nday)*10^6

plotdat <- eventdat[,c("LSOA11CD", "period", "dayrate")]
plotdat$period <- factor(plotdat$period,levels=hwperiod$label)

plotmap <- merge(lsoashp, plotdat, by="LSOA11CD", sort=F)

################################################################################
# MAP THE FIVE HEAT EPISODES

# Set a common pale-yellow to dark-red scale.
colval <- colorRampPalette(c("lightyellow","yellow","orange","red","red4"))(12)

plotlim <- unname(quantile(plotdat$dayrate, 0.99, na.rm=T))
# if(!is.finite(plotlim) || plotlim <= 0) plotlim <- 1

plotmax <- ceiling(max(plotdat$dayrate, na.rm=T))

# Use ordinary breaks and label the right endpoint with the maximum.
plotbreak <- pretty(c(0, plotlim), n=6)
plotbreak <- plotbreak[plotbreak >= 0 & plotbreak < plotlim]
plotbreak <- sort(unique(c(plotbreak, plotlim)))
plotlabel <- format(round(plotbreak, 0), trim=T)
plotlabel[length(plotlabel)] <- as.character(plotmax)

# Draw the map
pmap <- ggplot(plotmap) +
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

print(pmap)
ggsave("figures/Fig2_map_avrexc_2026.png", pmap, width=14, height=9, dpi=300)
