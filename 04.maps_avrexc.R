################################################################################
# UK-HW2026: 2026 SUMMER HEAT-RELATED EXCESS MORTALITY
# SCRIPT 04: MAP LSOA IMPACTS FOR FIVE IDENTIFIED HEATWAVE PERIODS
################################################################################

################################################################################
# PREPARE MAP DATA

# Load daily LSOA point estimates and define their dates
daylsoa <- readRDS("temp/daylsoa_2026.rds")
daydate <- as.Date(colnames(daylsoa))

# Add regional identifiers to LAD boundaries
ladmap <- merge(ladshp, unique(lookup[c("LAD11CD", "RGN11CD")]), by="LAD11CD",
  sort=F)

################################################################################
# CALCULATE IMPACTS FOR THE FIVE HEATWAVE PERIODS

# Set up one column per heatwave period
eventest <- matrix(NA_real_,nrow=nrow(daylsoa),ncol=nrow(hwperiod),
  dimnames=list(rownames(daylsoa), hwperiod$label))

# Sum daily estimates within each period and truncate negative totals at zero
for(k in seq_len(nrow(hwperiod))) {
  dayid <- which(daydate >= hwperiod$start[k] & daydate <= hwperiod$end[k])
  if(!length(dayid)) {
    stop("A heatwave period is outside the daily LSOA results")
  }
  eventest[, k] <- pmax(rowSums(daylsoa[, dayid, drop=F],na.rm=T),0)
}

# Reshape period estimates and add LSOA populations
eventdat <- as.data.table(as.table(eventest))
names(eventdat) <- c("LSOA11CD", "period", "estimate")
eventdat$LSOA11CD <- as.character(eventdat$LSOA11CD)
eventdat$period <- factor(as.character(eventdat$period),levels=hwperiod$label)
popdat <- subset(basedat, agegr == "all", select=c("LSOA11CD", "pop"))
names(popdat)[2] <- "population"
eventdat <- merge(eventdat, popdat, by="LSOA11CD")

################################################################################
# CALCULATE AVERAGE DAILY BURDEN

# Calculate the duration of each heatwave period
hwday <- setNames(as.integer(hwperiod$end-hwperiod$start)+1,hwperiod$label)

# Calculate excess deaths per million person-days
eventdat$nday <- unname(hwday[as.character(eventdat$period)])
eventdat$dayrate <- eventdat$estimate/
  (eventdat$population*eventdat$nday)*10^6

# Join period-specific rates to LSOA boundaries
plotdat <- eventdat[,c("LSOA11CD", "period", "dayrate")]
plotdat$period <- factor(plotdat$period,levels=hwperiod$label)

plotmap <- merge(lsoashp, plotdat, by="LSOA11CD", sort=F)

################################################################################
# MAP THE FIVE HEATWAVE PERIODS

# Define a common colour scale
colval <- colorRampPalette(c("lightyellow","yellow","orange","red","red4"))(12)

# Limit the colour scale at the 99th percentile
plotlim <- unname(quantile(plotdat$dayrate, 0.99, na.rm=T))
plotmax <- ceiling(max(plotdat$dayrate, na.rm=T))

# Define legend breaks and label the upper category with the maximum
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
    name="Excess deaths per million person-days") +
  facet_wrap(~period, ncol=3) +
  theme_void() +
  theme(legend.position="bottom", 
    strip.text=element_text(face="bold", margin=margin(t=3, b=4))) +
  guides(fill=guide_colorsteps(barwidth=14,barheight=0.6,title.position="top"))

print(pmap)
ggsave("figures/Fig2_map_avrexc_2026.png", pmap, width=14, height=9, dpi=1200)
