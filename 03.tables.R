################################################################################
# UK-HW2026: 2026 SUMMER HEAT-RELATED EXCESS MORTALITY
# SCRIPT 03: CREATE MANUSCRIPT TABLES AND FIGURE 1
################################################################################

################################################################################
# PREPARE LABELS

# Define age-group codes and display labels
agecode <- c(agevarlab, "all")
agelabel <- c(agelab, "All ages")

################################################################################
# CREATE NATIONAL PERIOD TABLE

# Load national estimates and simulations
natage <- readRDS("temp/natage_2026.rds")

# Set up one row per age group and period
tabnat <- expand.grid(agegr=agecode,period=dimnames(natage)$period,
  stringsAsFactors=F)
tabnat$age <- agelabel[match(tabnat$agegr, agecode)]
tabnat$population <- NA_real_
tabnat$estimate <- NA_real_
tabnat$lower <- NA_real_
tabnat$upper <- NA_real_

# Extract point estimates, empirical intervals and populations
for(i in seq_len(nrow(tabnat))) {
  values <- natage[tabnat$agegr[i], tabnat$period[i], ]
  tabnat$population[i] <- sum(basedat$pop[as.character(basedat$agegr)==
      tabnat$agegr[i]])
  tabnat$estimate[i] <- values[1]
  tabnat$lower[i] <- quantile(values[-1], 0.025, names = F)
  tabnat$upper[i] <- quantile(values[-1], 0.975, names = F)
}

# Calculate cumulative burden per million population
tabnat$rate <- tabnat$estimate/tabnat$population*10^6
tabnat$ratelow <- tabnat$lower/tabnat$population*10^6
tabnat$ratehigh <- tabnat$upper/tabnat$population*10^6

# Summarise the study-period burden among people aged 75 years or older
age75sim <- colSums(natage[c("age7584", "age85plus"), periodlab, ])
age75est <- age75sim["est"]
age75ci <- quantile(age75sim[-1], c(0.025, 0.975), names = F)
age75pop <- sum(tabnat$population[tabnat$period == periodlab &
    tabnat$agegr %in% c("age7584", "age85plus")])
allest <- tabnat$estimate[tabnat$period == periodlab & tabnat$agegr == "all"]
age75pct <- age75est/allest*100

age75tab <- data.frame(
  agegr="age75plus",period=periodlab,age="75 and above",population=age75pop,
  estimate=unname(age75est),lower=age75ci[1],upper=age75ci[2],
  rate=unname(age75est)/age75pop*10^6,ratelow=age75ci[1]/age75pop*10^6,
  ratehigh=age75ci[2]/age75pop*10^6,proportion=unname(age75pct))
sprintf(paste0("People aged 75+ accounted for %.0f deaths (95%%eCI %.0f–%.0f), ",
  "representing %.1f%% of the total."),age75est,age75ci[1],age75ci[2],age75pct)

################################################################################
# CREATE REGION AND AGE TABLE

# Load regional estimates and simulations
regage <- readRDS("temp/regage_2026.rds")

# Set up one row per region, age group and period
tabreg <- expand.grid(agegr=agecode,RGN11CD=regcode,
  period=dimnames(regage)$period, stringsAsFactors=F)
tabreg$age <- agelabel[match(tabreg$agegr, agecode)]
tabreg$region <- regname[match(tabreg$RGN11CD, regcode)]
tabreg$population <- NA_real_
tabreg$estimate <- NA_real_
tabreg$lower <- NA_real_
tabreg$upper <- NA_real_

# Extract point estimates, empirical intervals and populations
for(i in seq_len(nrow(tabreg))) {
  values <- regage[tabreg$agegr[i], tabreg$RGN11CD[i], tabreg$period[i], ]
  reglsoa <- listlsoa[lsoareg == tabreg$RGN11CD[i]]
  tabreg$population[i] <- sum(basedat$pop[basedat$LSOA11CD%in%reglsoa &
      as.character(basedat$agegr)==tabreg$agegr[i]])
  tabreg$estimate[i] <- values[1]
  tabreg$lower[i] <- quantile(values[-1], 0.025,names=F)
  tabreg$upper[i] <- quantile(values[-1], 0.975,names=F)
}

# Calculate cumulative burden per million population
tabreg$rate <- tabreg$estimate/tabreg$population*10^6
tabreg$ratelow <- tabreg$lower/tabreg$population*10^6
tabreg$ratehigh <- tabreg$upper/tabreg$population*10^6

# Format study-period regional estimates for Table S2
tabmain <- subset(tabreg, period == periodlab)
tabmain$region <- factor(tabmain$region, levels=regname)
tabmain$age <- factor(tabmain$age, levels=agelabel)
tabmain <- tabmain[order(tabmain$region, tabmain$age), ]
tabmain$result <- paste0(formatC(tabmain$estimate,format="f",big.mark=",",
  digits=0)," (", formatC(tabmain$lower, format="f", big.mark=",", digits=0), 
  " to ", formatC(tabmain$upper, format="f", big.mark=",", digits=0),")")

tabwide <- pivot_wider(tabmain[c("region", "age", "result")],
  names_from=age, values_from=result) |>  as.data.frame()

# Add the England and Wales total
tabtotal <- subset(tabnat, period == periodlab)
tabtotal$region <- "Total"
tabtotal$age <- factor(tabtotal$age, levels=agelabel)
tabtotal$result <- paste0(formatC(tabtotal$estimate, format="f", big.mark=",", 
  digits=0)," (", formatC(tabtotal$lower, format="f", big.mark=",", digits=0),
  " to ",formatC(tabtotal$upper, format="f", big.mark=",", digits=0),")")
tabtotal <- pivot_wider(tabtotal[c("region", "age", "result")],
  names_from=age, values_from=result) |>  as.data.frame()

# Match columns before adding the total row
tabtotal <- tabtotal[names(tabwide)]
tabwide <- rbind(tabwide, tabtotal)
names(tabwide)[1] <- "Region"
write.csv(tabwide,"tables/TableS2_region_age_2026.csv",row.names=F)

################################################################################
# CREATE SUMMARY TABLE

# Select all-age regional estimates for the complete study period
regsum <- subset(tabreg, period == periodlab & agegr == "all")
regsum <- regsum[match(regname, regsum$region), ]
regsum$section <- "Region"
regsum$group <- regsum$region

# Select national age-specific estimates for the complete study period
agesum <- subset(tabnat, period == periodlab & agegr != "all")
agesum <- agesum[match(agevarlab, agesum$agegr), ]
agesum$section <- "Age group"
agesum$group <- agesum$age

# Select all-age national estimates for the five heatwave periods
eventsum <- subset(tabnat, period != periodlab & agegr == "all")
eventsum <- eventsum[match(hwperiod$label, eventsum$period), ]
eventsum$section <- "Heatwave period"
eventsum$group <- paste0(hwperiod$event, ": ",
  format(hwperiod$start, "%d %B"), " to ",
  format(hwperiod$end, "%d %B"))

# Select the all-age national estimate for the complete study period
totalsum <- subset(tabnat, period == periodlab & agegr == "all")
totalsum$section <- "Total (all regions and age groups)"
totalsum$group <- gsub("-", " to ", periodlab, fixed=T)

# Combine table sections
sumvar <- c("section", "group", "population", "estimate", "lower",
  "upper", "rate", "ratelow", "ratehigh")
tabsum <- rbind(regsum[sumvar], agesum[sumvar], eventsum[sumvar],
  totalsum[sumvar])

# Format estimates and empirical intervals for Table 1
tabsum$population <- formatC(tabsum$population, format="f", big.mark=",",
  digits=0)
tabsum$deaths <- paste0(
  formatC(tabsum$estimate, format="f", big.mark=",", digits=0), " (",
  formatC(tabsum$lower, format="f", big.mark=",", digits=0), " to ",
  formatC(tabsum$upper, format="f", big.mark=",", digits=0), ")")
tabsum$burden <- paste0(
  formatC(tabsum$rate, format="f", big.mark=",", digits=1), " (",
  formatC(tabsum$ratelow, format="f", big.mark=",", digits=1), " to ",
  formatC(tabsum$ratehigh, format="f", big.mark=",", digits=1), ")")

tabout <- tabsum[c("section", "group", "population", "deaths", "burden")]
names(tabout) <- c("Section", "Group", "Population", "Excess deaths (95% eCI)",
  "Excess deaths per million population (95% eCI)")

write.csv(tabout, "tables/Table1_summary_2026.csv", row.names=F)

################################################################################
# CREATE DAILY NATIONAL TABLE

# Load daily national estimates and simulations
dayage <- readRDS("temp/dayage_2026.rds")

# Parse stored date labels
daydate <- as.Date(dimnames(dayage)$date)
# daytxt <- dimnames(dayage)$date
# daydate <- suppressWarnings(as.Date(daytxt))
# needfmt <- is.na(daydate)
# daydate[needfmt] <- as.Date(daytxt[needfmt], format="%d %B %Y")

# Set up one row per age group and date
tabday <- expand.grid(agegr=agecode,dayid=seq_along(daydate),stringsAsFactors=F)
tabday$date <- daydate[tabday$dayid]
tabday$age <- agelabel[match(tabday$agegr, agecode)]
tabday$population <- NA_real_
tabday$estimate <- NA_real_
tabday$lower <- NA_real_
tabday$upper <- NA_real_

# Extract daily point estimates and empirical intervals
for(i in seq_len(nrow(tabday))) {
  values <- dayage[tabday$agegr[i], tabday$dayid[i], ]
  tabday$population[i] <- sum(basedat$pop[as.character(basedat$agegr)==
      tabday$agegr[i]])
  tabday$estimate[i] <- values[1]
  tabday$lower[i] <- quantile(values[-1],0.025,names=F)
  tabday$upper[i] <- quantile(values[-1],0.975,names=F)
}

# Calculate daily rates per million population
tabday$rate <- tabday$estimate/tabday$population*10^6
tabday$ratelow <- tabday$lower/tabday$population*10^6
tabday$ratehigh <- tabday$upper/tabday$population*10^6
tabday$dayid <- NULL

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

# Calculate and plot the national mean temperature series
tempfig <- data.frame(date=dates, 
  meantemp=colMeans(data.matrix(tmeandat[-1]),na.rm=T))

ptemp <- ggplot(tempfig, aes(x=date, y=meantemp)) +
  geom_rect(data=hwrect,aes(xmin=start, xmax=end, ymin=ymin, ymax=ymax),
    inherit.aes=FALSE,fill="gold",alpha=0.12) +
  geom_line(colour="#2C7FB8", linewidth=0.7) +
  labs(x=NULL, y=expression("National mean temperature ("*degree*C*")")) +
  themehw + theme(legend.position="none")

# Combine and save the mortality and temperature panels
pfigure <- patchwork::wrap_plots(pmort, ptemp, patchwork::guide_area(), ncol=1,
  heights=c(2, 1, 0.15),guides="collect") +
  patchwork::plot_annotation(
    title=paste0("Heat-related excess mortality and national mean temperature, ",
      "summer 2026"))

print(pfigure)
ggsave("figures/Fig1_daily_mort_t2m_2026.png",pfigure,width=11,height=8,dpi=1200)
