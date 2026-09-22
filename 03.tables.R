################################################################################
# UK-HW2026: 2026 SUMMER HEAT-RELATED EXCESS MORTALITY
# SCRIPT 03: CREATE TABLES AND DAILY TIME-SERIES FIGURES
################################################################################

################################################################################
# PREPARE LABELS

agecode <- c(agevarlab, "all")
agelabel <- c(agelab, "All ages")

################################################################################
# CREATE NATIONAL PERIOD TABLE

natage <- readRDS("temp/natage_2026.rds")

tabnat <- expand.grid(agegr=agecode,period=dimnames(natage)$period,
  stringsAsFactors=F)
tabnat$age <- agelabel[match(tabnat$agegr, agecode)]
tabnat$population <- NA_real_
tabnat$estimate <- NA_real_
tabnat$lower <- NA_real_
tabnat$upper <- NA_real_

for(i in seq_len(nrow(tabnat))) {
  values <- natage[tabnat$agegr[i], tabnat$period[i], ]
  tabnat$population[i] <- sum(basedat$pop[as.character(basedat$agegr)==
      tabnat$agegr[i]])
  tabnat$estimate[i] <- values[1]
  tabnat$lower[i] <- quantile(values[-1], 0.025, names = FALSE)
  tabnat$upper[i] <- quantile(values[-1], 0.975, names = FALSE)
}

tabnat$rate <- tabnat$estimate/tabnat$population*10^6
tabnat$ratelow <- tabnat$lower/tabnat$population*10^6
tabnat$ratehigh <- tabnat$upper/tabnat$population*10^6

write.csv(tabnat, "tables/total_age_2026.csv", row.names=F)

# 75+
age75sim <- colSums(natage[c("age7584", "age85plus"), periodlab, ])
age75est <- age75sim["est"]
age75ci <- quantile(age75sim[-1], c(0.025, 0.975), names = FALSE)
allest <- tabnat$estimate[tabnat$period == periodlab & tabnat$agegr == "all"]
age75pct <- age75est/allest*100
sprintf(paste0("People aged 75+ accounted for %.0f deaths (95%%eCI %.0f–%.0f), ",
    "representing %.1f%% of the total."),age75est,age75ci[1],age75ci[2],age75pct)

################################################################################
# CREATE REGION AND AGE TABLE

regage <- readRDS("temp/regage_2026.rds")

tabreg <- expand.grid(agegr=agecode,RGN11CD=regcode,
  period=dimnames(regage)$period, stringsAsFactors=F)
tabreg$age <- agelabel[match(tabreg$agegr, agecode)]
tabreg$region <- regname[match(tabreg$RGN11CD, regcode)]
tabreg$population <- NA_real_
tabreg$estimate <- NA_real_
tabreg$lower <- NA_real_
tabreg$upper <- NA_real_

for(i in seq_len(nrow(tabreg))) {
  values <- regage[tabreg$agegr[i], tabreg$RGN11CD[i], tabreg$period[i], ]
  reglsoa <- listlsoa[lsoareg == tabreg$RGN11CD[i]]
  tabreg$population[i] <- sum(basedat$pop[basedat$LSOA11CD%in%reglsoa &
      as.character(basedat$agegr)==tabreg$agegr[i]])
  tabreg$estimate[i] <- values[1]
  tabreg$lower[i] <- quantile(values[-1], 0.025,names=F)
  tabreg$upper[i] <- quantile(values[-1], 0.975,names=F)
}

tabreg$rate <- tabreg$estimate/tabreg$population*10^6
tabreg$ratelow <- tabreg$lower/tabreg$population*10^6
tabreg$ratehigh <- tabreg$upper/tabreg$population*10^6

write.csv(tabreg, "tables/total_region_age_2026.csv", row.names=F)

tabmain <- subset(tabreg, period == periodlab)
tabmain$region <- factor(tabmain$region, levels=regname)
tabmain$age <- factor(tabmain$age, levels=agelabel)
tabmain <- tabmain[order(tabmain$region, tabmain$age), ]
tabmain$result <- paste0(formatC(tabmain$estimate,format="f",big.mark=",",
  digits=0)," (", formatC(tabmain$lower, format="f", big.mark=",", digits=0), 
  " to ", formatC(tabmain$upper, format="f", big.mark=",", digits=0),")")

tabwide <- pivot_wider(tabmain[c("region", "age", "result")],
  names_from=age, values_from=result) |>  as.data.frame()

# ADD ENGLAND AND WALES TOTAL
tabtotal <- subset(tabnat, period == periodlab)
tabtotal$region <- "Total"
tabtotal$age <- factor(tabtotal$age, levels=agelabel)
tabtotal$result <- paste0(formatC(tabtotal$estimate, format="f", big.mark=",", 
  digits=0)," (", formatC(tabtotal$lower, format="f", big.mark=",", digits=0),
  " to ",formatC(tabtotal$upper, format="f", big.mark=",", digits=0),")")
tabtotal <- pivot_wider(tabtotal[c("region", "age", "result")],
  names_from=age, values_from=result) |>  as.data.frame()

# Match the columns before adding the Total row.
tabtotal <- tabtotal[names(tabwide)]
tabwide <- rbind(tabwide, tabtotal)
write.csv(tabwide,"tables/study_region_age_wide_2026.csv",row.names=F)

################################################################################
# CREATE DAILY NATIONAL TABLE

dayage <- readRDS("temp/dayage_2026.rds")

daytxt <- dimnames(dayage)$date
daydate <- suppressWarnings(as.Date(daytxt))
needfmt <- is.na(daydate)
daydate[needfmt] <- as.Date(daytxt[needfmt], format="%d %B %Y")

tabday <- expand.grid(agegr=agecode,dayid=seq_along(daydate),stringsAsFactors=F)
tabday$date <- daydate[tabday$dayid]
tabday$age <- agelabel[match(tabday$agegr, agecode)]
tabday$population <- NA_real_
tabday$estimate <- NA_real_
tabday$lower <- NA_real_
tabday$upper <- NA_real_

for(i in seq_len(nrow(tabday))) {
  values <- dayage[tabday$agegr[i], tabday$dayid[i], ]
  tabday$population[i] <- sum(basedat$pop[as.character(basedat$agegr)==
      tabday$agegr[i]])
  tabday$estimate[i] <- values[1]
  tabday$lower[i] <- quantile(values[-1],0.025,names=F)
  tabday$upper[i] <- quantile(values[-1],0.975,names=F)
}

tabday$rate <- tabday$estimate/tabday$population*10^6
tabday$ratelow <- tabday$lower/tabday$population*10^6
tabday$ratehigh <- tabday$upper/tabday$population*10^6
tabday$dayid <- NULL

write.csv(tabday, "tables/daily_age_2026.csv",row.names=F)

################################################################################
# PLOT DAILY MORTALITY AND TEMPERATURE

themehw <- theme_bw() +
  theme(panel.grid.minor=element_blank(), legend.position="bottom")

hwrect <- hwperiod
hwrect$ymin <- -Inf
hwrect$ymax <- Inf

tabday$age <- factor(tabday$age, levels=agelabel)
dayagesub <- subset(tabday, agegr != "all")
dayallsub <- subset(tabday, agegr == "all")

agecol <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "black")
names(agecol) <- agelabel

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

tempfig <- data.frame(date=dates, 
  meantemp=colMeans(data.matrix(tmeandat[-1]),na.rm=T))

ptemp <- ggplot(tempfig, aes(x=date, y=meantemp)) +
  geom_rect(data=hwrect,aes(xmin=start, xmax=end, ymin=ymin, ymax=ymax),
    inherit.aes=FALSE,fill="gold",alpha=0.12) +
  geom_line(colour="#2C7FB8", linewidth=0.7) +
  labs(x=NULL, y="National mean temperature (°C)") +
  themehw + theme(legend.position="none")

pfigure <- patchwork::wrap_plots(pmort, ptemp, patchwork::guide_area(), ncol=1,
  heights=c(2, 1, 0.15),guides="collect") +
  patchwork::plot_annotation(
    title=paste0("Heat-related excess mortality and national mean temperature, ",
      "summer 2026"),
    caption=paste0("National mean temperature is calculated as the daily mean ",
      "across all LSOAs. Gold shading indicates the five heatwave periods."))

print(pfigure)
ggsave("figures/Fig1_daily_mort_t2m_2026.png",pfigure,width=11, height=8, dpi=300)
