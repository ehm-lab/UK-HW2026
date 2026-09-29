################################################################################
# UK-HW2026: 2026 SUMMER HEAT-RELATED EXCESS MORTALITY
# SCRIPT 04: CREATE LSOA IMPACT MAPS
################################################################################

################################################################################
# PREPARE MAP DATA

daylsoa <- readRDS("temp/daylsoa_2026.rds")
lsoatab <- read.csv("tables/lsoa_period_2026.csv")

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
# write.csv(eventdat,"tables/lsoa_hw_2026.csv",row.names=FALSE)

# Set pale-yellow to dark-red palette
colval <- c("white",colorRampPalette(c("yellow", "orange", "red", "red4"))(11))

################################################################################ 
# MAP FIVE HEATWAVE PERIODS AND THE COMPLETE STUDY PERIOD

totlab <- paste0("Total: ", periodlab) 
eventplot <- data.frame(LSOA11CD=eventdat$LSOA11CD,
  period=as.character(eventdat$heatwave), rate=eventdat$rate)
studyplot <- data.frame(LSOA11CD=lsoatab$LSOA11CD,period=totlab,
  rate=lsoatab$ratestudy)
plotdat <- rbind(eventplot, studyplot)
plotdat$period <- factor(plotdat$period,levels=c(hwperiod$label, totlab)) 
plotmap <- merge(lsoashp, plotdat,by="LSOA11CD",sort=F) 

# Use the 99th percentile of total-period rates as the upper limit.
plotlim <- unname(quantile(studyplot$rate, 0.99, na.rm=TRUE))
plotelim <- unname(quantile(eventplot$rate, 0.99, na.rm=TRUE))
plotelim <- min(plotelim, plotlim)

# Use detailed breaks for individual heatwave periods.
plotebreak <- pretty(c(0, plotelim), n=4)
plotebreak <- plotebreak[plotebreak >= 0 & plotebreak <= plotelim]

# Use wider breaks for the upper range.
plotsplit <- max(plotebreak)
plottbreak <- pretty(c(plotsplit, plotlim), n=3)
plottbreak <- plottbreak[plottbreak > plotsplit & plottbreak < plotlim]

plotbreak <- sort(unique(c(plotebreak, plottbreak, plotlim)))

plotlabel <- format(round(plotbreak, 0), trim=T)
plotlabel[length(plotlabel)] <- ceiling(max(studyplot$rate))#paste0(round(plotlim),"+")

plotvalue <- scales::rescale(plotbreak, from=c(0, plotlim))

pmap <- ggplot(plotmap) +
  geom_sf(aes(fill=rate), colour=NA) +
  geom_sf(data=ladmap, fill=NA, colour="grey30", linewidth=0.05) +
  scale_fill_stepsn(colours=colval,values=plotvalue,limits=c(0, plotlim),
    breaks=plotbreak,labels=plotlabel, oob=scales::squish,
    name="Cumulative excess deaths per million population")+
  facet_wrap(~period, ncol=3) +
  theme_void() +
  theme(legend.position="bottom", strip.text=element_text(face="bold")) +
  guides(fill=guide_colorsteps(barwidth=14,barheight=0.6,title.position="top"))+
  labs(title="Heat-related excess mortality by LSOA, summer 2026"
    #, subtitle=paste0("Five prespecified heat episodes and total for ",periodlab)
  )
print(pmap) 
ggsave("figures/Fig2_map_hw_total_2026.png", pmap, width=14, height=9, dpi=300) 
