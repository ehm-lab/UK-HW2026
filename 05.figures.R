################################################################################
# UK-HW2026: 2026 SUMMER HEAT-RELATED EXCESS MORTALITY
# SCRIPT 05: MAP LSOA IMPACTS FOR FIVE IDENTIFIED HEATWAVE PERIODS
################################################################################

################################################################################
# PREPARE TIME SERIES DATA

# Read the daily national results 
tabday <- read.csv("output/national_mort_daily.csv", na.strings = "") |>
  dplyr::mutate(date=as.Date(date))

# Read the national mean temperature series
tempday <- read.csv("output/national_t2m_daily.csv", na.strings = "") |>
  dplyr::mutate(date=as.Date(date))

################################################################################
# PLOT DAILY MORTALITY AND TEMPERATURE

# Set the common figure theme and heatwave-period shading
themehw <- theme_bw() +
  theme(panel.grid.minor=element_blank(), legend.position="bottom")

hwrect <- hwperiod
hwrect$ymin <- -Inf
hwrect$ymax <- Inf

# Separate age-specific and all-age mortality series
tabday$age <- factor(tabday$age, levels=agelabel)
dayagesub <- subset(tabday, agegr != "all")
dayallsub <- subset(tabday, agegr == "all")

# Define age-group colours
agecol <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "black")
names(agecol) <- agelabel

# Plot daily heat-related mortality
pmort <- ggplot() +
  geom_rect(data=hwrect,aes(xmin=start, xmax=end, ymin=ymin, ymax=ymax),
    inherit.aes=F,fill="gold",alpha=0.12) +
  geom_ribbon(data=dayallsub, aes(x=date, ymin=lower, ymax=upper),
    inherit.aes=F,fill="grey55",alpha=0.30) +
  geom_line(data=dayagesub, aes(x=date, y=estimate, colour=age),linewidth=0.55) +
  geom_line(data=dayallsub, aes(x=date, y=estimate, colour=age),linewidth=1.05) +
  geom_hline(yintercept=0, colour="grey50",linewidth=0.25) +
  scale_colour_manual(values=agecol,name="Age group",drop=F) +
  labs(x=NULL, y="Daily heat-related excess deaths") +
  themehw

# Plot daily temperature series
ptemp <- ggplot(tempday, aes(x=date, y=meantemp)) +
  geom_rect(data=hwrect,aes(xmin=start, xmax=end, ymin=ymin, ymax=ymax),
    inherit.aes=FALSE,fill="gold",alpha=0.12) +
  geom_line(colour="#2C7FB8", linewidth=0.7) +
  labs(x=NULL, y=expression("National mean temperature ("*degree*C*")")) +
  themehw + theme(legend.position="none")

# Combine and save the mortality and temperature panels
pfigure <- patchwork::wrap_plots(pmort, ptemp, patchwork::guide_area(), ncol=1,
  heights=c(2, 1, 0.15), guides="collect")

# print(pfigure)
ggsave("figures/Fig1_daily_ts.png", plot=pfigure,
  width=2200, height=2000, units="px", dpi=300, bg="white")

################################################################################
# PREPARE MAP DATA

# Read LSOA results and select the all-age estimates
tablsoaage <- read.csv(gzfile("output/lsoa_age_hw_period.csv.gz"),na.strings="")
tablsoa <- subset(tablsoaage, agegr=="all")
tablsoa$period <- factor(tablsoa$period, levels=hwperiod$label)

# Add regional identifiers to LAD boundaries
ladmap <- merge(ladshp, unique(lookup[c("LAD11CD", "RGN11CD")]), by="LAD11CD",
  sort=F)

# Join the period-specific rates per million person-days to LSOA boundaries
plotmap <- merge(lsoashp, tablsoa, by="LSOA11CD", sort=F)

################################################################################
# MAP THE FIVE HEATWAVE PERIODS

# Define a common colour scale
colval <- colorRampPalette(c("lightyellow","yellow","orange","red","red4"))(12)

# Limit the colour scale at the 99th percentile
plotlim <- unname(quantile(tablsoa$rate, 0.99, na.rm=T))
plotmax <- ceiling(max(tablsoa$rate, na.rm=T))

# Define legend breaks and label the upper category with the maximum
plotbreak <- pretty(c(0, plotlim), n=6)
plotbreak <- plotbreak[plotbreak >= 0 & plotbreak < plotlim]
plotbreak <- sort(unique(c(plotbreak, plotlim)))
plotlabel <- format(round(plotbreak, 0), trim=T)
plotlabel[length(plotlabel)] <- as.character(plotmax)

# Draw the map
pmap <- ggplot(plotmap) +
  geom_sf(aes(fill=rate), colour=NA) +
  geom_sf(data=ladmap,fill=NA,colour="grey30",linewidth=0.05) +
  scale_fill_stepsn(colours=colval,limits=c(0, plotlim),breaks=plotbreak,
    labels=plotlabel, oob=scales::squish,
    name="Excess deaths per million person-days") +
  facet_wrap(~period, ncol=3) +
  theme_void() +
  theme(legend.position="bottom", 
    strip.text=element_text(face="bold", margin=margin(t=3, b=4))) +
  guides(fill=guide_colorsteps(barwidth=14,barheight=0.6,title.position="top"))

# print(pmap)
ggsave("figures/Fig2_map_excpd.png", plot=pmap,
  width=1920, height=1500, units="px", dpi=300, bg="white")
