################################################################################
# UK-HW2026: 2026 SUMMER HEAT-RELATED EXCESS MORTALITY
# SCRIPT 03: CREATE MACHINE-READABLE RESULTS FOR FUTURE APPLICATIONS
################################################################################

################################################################################
# PREPARE LABELS

# Define age-group codes and display labels
agecode <- c(agevarlab, "all")
agelabel <- c(agelab, "All ages")

# Check that all intermediate result files are available
reqfile <- file.path("temp", c("natage.rds","regage.rds","dayage.rds",
  "daylsoa.rds"))
if(any(!file.exists(reqfile))) {
  stop("Missing intermediate results: ",
    paste(reqfile[!file.exists(reqfile)], collapse=", "))
}

################################################################################
# CREATE NATIONAL PERIOD TABLE

# Load national estimates and simulations
natage <- readRDS("temp/natage.rds")

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

# Save unformatted national results
write.csv(tabnat, file="output/national_age_period.csv", row.names=F, na="")

################################################################################
# CREATE REGION AND AGE TABLE

# Load regional estimates and simulations
regage <- readRDS("temp/regage.rds")

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

# Save unformatted regional results 
write.csv(tabreg, file="output/region_age_period.csv", row.names=F, na="")

################################################################################
# CREATE NATIONAL AND REGIONAL TABLES FOR PEOPLE AGED 75+

# Sum the two oldest age groups before calculating empirical intervals
nat75 <- apply(natage[c("age7584", "age85plus"), , , drop=F],
  c(2, 3), sum)
reg75 <- apply(regage[c("age7584", "age85plus"), , , , drop=F],
  c(2, 3, 4), sum)

# Calculate the national population aged 75+
age75pop <- sum(basedat$pop[
  as.character(basedat$agegr) %in% c("age7584", "age85plus")])

# Set up one national row per study or heatwave period
tab75nat <- data.frame(agegr="age75plus", period=dimnames(nat75)$period,
  age="75 and above", population=age75pop, estimate=NA_real_,
  lower=NA_real_, upper=NA_real_)

# Extract national point estimates and empirical intervals
for(i in seq_len(nrow(tab75nat))) {
  values <- nat75[tab75nat$period[i], ]
  tab75nat$estimate[i] <- values[1]
  tab75nat$lower[i] <- quantile(values[-1], 0.025, names=F)
  tab75nat$upper[i] <- quantile(values[-1], 0.975, names=F)
}

# Calculate national rates per million people aged 75+
tab75nat$rate <- tab75nat$estimate/tab75nat$population*10^6
tab75nat$ratelow <- tab75nat$lower/tab75nat$population*10^6
tab75nat$ratehigh <- tab75nat$upper/tab75nat$population*10^6

# Match the tabnat column order and save national 75+ results
tab75nat <- tab75nat[names(tabnat)]
write.csv(tab75nat, file="output/national_75plus_period.csv",row.names=F,na="")

# Set up one regional row per study or heatwave period
tab75reg <- expand.grid(RGN11CD=regcode, period=dimnames(reg75)$period,
  stringsAsFactors=F)
tab75reg$agegr <- "age75plus"
tab75reg$age <- "75 and above"
tab75reg$region <- regname[match(tab75reg$RGN11CD, regcode)]
tab75reg$population <- NA_real_
tab75reg$estimate <- NA_real_
tab75reg$lower <- NA_real_
tab75reg$upper <- NA_real_

# Extract regional populations, point estimates and empirical intervals
for(i in seq_len(nrow(tab75reg))) {
  reglsoa <- listlsoa[lsoareg == tab75reg$RGN11CD[i]]
  tab75reg$population[i] <- sum(basedat$pop[basedat$LSOA11CD %in% reglsoa &
      as.character(basedat$agegr) %in% c("age7584", "age85plus")])
  values <- reg75[tab75reg$RGN11CD[i], tab75reg$period[i], ]
  tab75reg$estimate[i] <- values[1]
  tab75reg$lower[i] <- quantile(values[-1], 0.025, names=F)
  tab75reg$upper[i] <- quantile(values[-1], 0.975, names=F)
}

# Calculate regional rates per million people aged 75+
tab75reg$rate <- tab75reg$estimate/tab75reg$population*10^6
tab75reg$ratelow <- tab75reg$lower/tab75reg$population*10^6
tab75reg$ratehigh <- tab75reg$upper/tab75reg$population*10^6

# Match the tabreg column order and save regional 75+ results
tab75reg <- tab75reg[names(tabreg)]
write.csv(tab75reg, file="output/region_75plus_period.csv", row.names=F, na="")

################################################################################
# CREATE DAILY NATIONAL TABLE

# Load daily national estimates and simulations
dayage <- readRDS("temp/dayage.rds")

# Parse stored date labels
daydate <- as.Date(dimnames(dayage)$date)

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

# Save unformatted daily national results 
write.csv(tabday, file="output/national_mort_daily.csv", row.names=F, na="")

# Calculate the unweighted LSOA-mean temperature for each study date
tempday <- data.frame(date=dates,
  meantemp=colMeans(data.matrix(tmeandat[-1]), na.rm=T))

# Save the temperature series used with the daily mortality results 
write.csv(tempday, file="output/national_t2m_daily.csv", row.names=F, na="")

################################################################################
# LSOA RESULTS FOR THE FIVE HEATWAVE PERIODS

# Load daily LSOA point estimates and recover their dates
daylsoa <- readRDS("temp/daylsoa.rds")
lsoadate <- as.Date(colnames(daylsoa))

# Prepare unique LSOA labels and all-age populations for joining
lsoalook <- unique(lookup[c(
  "LSOA11CD", "LSOA11NM", "LAD11CD", "LAD11NM", "RGN11CD", "RGN11NM"
)])
lsoapop <- subset(basedat,as.character(agegr)=="all",select=c("LSOA11CD","pop"))
names(lsoapop)[2] <- "population"

# Sum daily LSOA estimates separately for each heatwave period
tablsoa <- do.call(rbind, lapply(seq_len(nrow(hwperiod)), function(i) {
  # Identify daily-result columns within the current period
  dayid <- which(
    lsoadate >= hwperiod$start[i] & lsoadate <= hwperiod$end[i])

  # Sum each LSOA and apply the non-negative mapping rule
  est <- pmax(rowSums(daylsoa[, dayid, drop=F], na.rm=T), 0)
  
  # Return identifiers, period metadata and point estimates
  data.frame(LSOA11CD=rownames(daylsoa),event=hwperiod$event[i],
    period=hwperiod$label[i], start_date=hwperiod$start[i],
    end_date=hwperiod$end[i], n_days=length(dayid), estimate=est,
    stringsAsFactors=F)
}))

# Join labels and population
tablsoa <- merge(lsoalook, tablsoa, by="LSOA11CD", sort=F)
tablsoa <- merge(tablsoa, lsoapop, by="LSOA11CD", sort=F)

# Calculate average daily excess deaths per million person-days
tablsoa$rate <- tablsoa$estimate/(tablsoa$population*tablsoa$n_days)*10^6

# Restore heatwave-period and LSOA order after the joins
tablsoa <- tablsoa[order(match(tablsoa$period, hwperiod$label),
  match(tablsoa$LSOA11CD, listlsoa)),]

# Save unformatted LSOA period results
write.csv(tablsoa, file="output/lsoa_hw_period.csv", row.names=F, na="")

################################################################################
# CREATE LSOA POINT-ESTIMATE TABLE FOR THE COMPLETE STUDY PERIOD

# Sum daily point estimates over the complete study period
lsoatotal <- rowSums(daylsoa)

# Match LSOA identifiers and geographical labels to the result rows
tablsoaall <- lsoalook[match(rownames(daylsoa), lsoalook$LSOA11CD), ]

# Add populations and point estimates
tablsoaall$population <- lsoapop$population[
  match(tablsoaall$LSOA11CD, lsoapop$LSOA11CD)]
tablsoaall$estimate <- unname(lsoatotal[tablsoaall$LSOA11CD])

# Calculate cumulative excess deaths per million population
tablsoaall$excess_deaths_per_million <-
  tablsoaall$estimate/tablsoaall$population*10^6

# Retain the prepared LSOA order and save the complete table
tablsoaall <- tablsoaall[order(match(tablsoaall$LSOA11CD, listlsoa)), ]
rownames(tablsoaall) <- NULL

# Save unformatted LSOA study period results
write.csv(tablsoaall, file="output/lsoa_study_period.csv", row.names=F, na="")

################################################################################
# CREATE LAD POINT-ESTIMATE TABLE FOR THE COMPLETE STUDY PERIOD

# Match each LSOA result row to its 2011 local authority district
lsoalad <- lookup$LAD11CD[match(rownames(daylsoa), lookup$LSOA11CD)]

# Sum complete-study-period LSOA estimates within each LAD
ladtotal <- tapply(lsoatotal, lsoalad, sum)

# Prepare one lookup row per LAD and retain the LAD order from the totals
ladlook <- unique(lookup[c("LAD11CD", "LAD11NM", "RGN11CD", "RGN11NM")])
tablad <- ladlook[match(names(ladtotal), ladlook$LAD11CD), ]

# Aggregate the all-age LSOA populations to LAD level
ladpop <- merge(lsoapop,lsoalook[c("LSOA11CD","LAD11CD")],by="LSOA11CD",sort=F)
ladpop <- aggregate(population ~ LAD11CD, data=ladpop, sum)

# Add populations and point estimates
tablad$population <- ladpop$population[match(tablad$LAD11CD, ladpop$LAD11CD)]
tablad$estimate <- unname(ladtotal[tablad$LAD11CD])

# Calculate cumulative excess deaths per million population
tablad$excess_deaths_per_million <- tablad$estimate/tablad$population*10^6

# Order LADs by region and name
tablad <- tablad[order(match(tablad$RGN11CD, regcode), tablad$LAD11NM), ]
rownames(tablad) <- NULL

# Save unformatted LAD study-period results
write.csv(tablad, file="output/lad_study_period.csv", row.names=F, na="")
