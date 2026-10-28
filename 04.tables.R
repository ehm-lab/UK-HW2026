################################################################################
# UK-HW2026: 2026 SUMMER HEAT-RELATED EXCESS MORTALITY
# SCRIPT 04: CREATE MANUSCRIPT TABLES
################################################################################

# Load output
# Read unformatted national results
tabnat <- read.csv("output/national_age_period.csv", na.strings = "")

# Read unformatted regional results 
tabreg <- read.csv("output/region_age_period.csv", na.strings = "")

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

write.csv(tabout, "tables/Table1_summary.csv", row.names=F)


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
  names_from=age, values_from=result) |> as.data.frame()

# Match columns before adding the total row
tabtotal <- tabtotal[names(tabwide)]
tabwide <- rbind(tabwide, tabtotal)
names(tabwide)[1] <- "Region"
write.csv(tabwide,"tables/TableS2_region_age.csv",row.names=F)
