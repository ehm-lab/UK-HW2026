################################################################################
# UK-HW2026: 2026 SUMMER HEAT-RELATED EXCESS MORTALITY
# SCRIPT 01: PREPARE MODEL, POPULATION, TEMPERATURE AND GEOGRAPHY DATA
################################################################################
# setwd("V:/VolumeQ/AGteam/projects/UK-HWfcast")

################################################################################
# SET ANALYSIS OPTIONS

# Open UK-HW2026.Rproj before running this script.
datestart <- as.Date("2026-05-01")
dateend <- as.Date("2026-09-15")
hwprob <- 0.90
hwminday <- 3
nsim <- 500
ncore <- max(1, min(4, parallel::detectCores()-2))
chunksize <- 100
nmapday <- 6

# five prespecified heatwave periods
hwperiod <- data.frame(event=paste0("HW", 1:5),
  start=as.Date(c(
    "2026-05-22","2026-06-18","2026-07-06","2026-07-29","2026-08-11")),
  end=as.Date(c(
    "2026-05-28","2026-06-28","2026-07-17","2026-08-05","2026-08-15")))
hwperiod$label <- paste0(
  hwperiod$event, ": ", format(hwperiod$start, "%d %b"), "-",
  format(hwperiod$end, "%d %b"))
periodlab <- "1 May-15 September 2026"
write.csv(hwperiod, "tables/hwperiod_main_2026.csv",row.names=F)

################################################################################
# LOAD MODEL PARAMETERS

obj <- c("coefmeta", "vcovmeta", "lsoacomp", "lsoatmeanper")

for(x in obj)  assign(x, readRDS(paste0("data/", x, ".RDS")))

agelab <- c("0-64", "65-74", "75-84", "85 and above")
agevarlab <- c("age064", "age6574", "age7584", "age85plus")

varfun <- "bs"
varper <- c(10, 75, 90)
vardegree <- 2
vardf <- 5

################################################################################
# PREPARE GEOGRAPHY LOOKUP

unzip(zipfile="data/lookup.zip")
lookup <- read.csv("data/lookup.csv", fileEncoding="UTF-8-BOM")
file.remove("data/lookup.csv")

lookup <- lookup[with(lookup, order(LSOA11CD, LAD11CD)), ]

listlsoa <- unique(lookup$LSOA11CD)
listlad <- sort(unique(lookup$LAD11CD))

reglookup <- unique(lookup[c("RGN11CD", "RGN11NM")])
regcode <- c(paste0("E1200000", 1:9), "W92000004")
reglookup <- reglookup[match(regcode, reglookup$RGN11CD), ]
regname <- reglookup$RGN11NM
lsoareg <- lookup$RGN11CD[match(listlsoa, lookup$LSOA11CD)]

lsoacomp <- lsoacomp[match(listlsoa, lsoacomp$LSOA11CD), ]
lsoatmeanper <- lsoatmeanper[match(listlsoa, lsoatmeanper$LSOA11CD), ]

################################################################################
# PREPARE BASELINE POPULATION AND MORTALITY

unzip(zipfile="data/population_2020.zip")
pop <- read.csv("data/population_2020.csv")
file.remove("data/population_2020.csv")

names(pop)[1] <- "LSOA11CD"
names(pop)[-1] <- paste0("age", 0:90)

agecut1 <- c(0, 1, 1:18*5)
agecut2 <- c(0, 1:18*5-1, 90)

pop <- cbind(
  pop[1],
  mapply(":", agecut1, agecut2) |>
    lapply(function(x) rowSums(pop[paste0("age", x)])) |>
    Reduce(cbind, x=_)
)

names(pop)[-1] <- mapply(paste0, "age", sprintf("%02d", agecut1),
  sprintf("%02d", agecut2))

pop <- pivot_longer(pop, !LSOA11CD, names_to="agegr", values_to="pop") |>
  as.data.frame()

unzip(zipfile="data/deathrates_2022.zip")
deathrate <- read.csv("data/deathrates_2022.csv")
file.remove("data/deathrates_2022.csv")

deathrate[, 1] <- unique(pop$agegr)
names(deathrate)[1] <- "agegr"

deathrate <- pivot_longer(deathrate,-1,names_to="RGN11CD",values_to="rate") |>
  as.data.frame()

basedat <- lookup[c("LSOA11CD", "RGN11CD")] |>
  merge(pop) |>
  merge(deathrate) |>
  mutate(deathyear=rate*pop/10^5)

basedat$agegr <- factor(basedat$agegr, levels=unique(deathrate[, 1]))
levels(basedat$agegr) <- rep(agevarlab, c(14, 2, 2, 2))

basedat <- summarise(basedat, pop=sum(pop), deathyear=sum(deathyear),
  .by=c(LSOA11CD, agegr)) |> arrange(LSOA11CD, agegr)

basedat <- summarise(basedat,pop=sum(pop),deathyear=sum(deathyear),
  .by=LSOA11CD) |> mutate(agegr="all") |> rbind(basedat) |>
  arrange(LSOA11CD, agegr)

rm(pop, deathrate)

################################################################################
# PREPARE DAILY MEAN TEMPERATURE

tempdat <- read_parquet("download/LSOA_daily_ERA5_2026_t2m.parquet")
tempdat <- as.data.table(tempdat)
tempdat[, date := as.Date(date, tz="Europe/London")]
tempdat <- tempdat[date >= datestart & date <= dateend]
setorder(tempdat, LSOA11CD, date)

if(!all(listlsoa %in% tempdat$LSOA11CD)) 
  stop("Temperature data are missing one or more LSOAs")

dates <- seq(datestart, dateend, by="day")

if(!all(dates %in% tempdat$date)) 
  stop("Temperature data are missing one or more study dates")

tmeandat <- dcast(tempdat, LSOA11CD ~ date, value.var="t2m")
tmeandat <- tmeandat[match(listlsoa, LSOA11CD)]
tmeandat <- as.data.frame(tmeandat)
tmeandat <- tmeandat[c("LSOA11CD", as.character(dates))]
names(tmeandat)[-1] <- format(dates, "%d %B %Y")

################################################################################
# PREPARE SHAPEFILES

file.copy("data/lsoashp.zip", getwd())
unzip(zipfile="lsoashp.zip", exdir=getwd())
lsoashp <- st_read("lsoashp.shp")
file.remove(list.files()[grep("lsoashp", list.files(), fixed=TRUE)])
lsoashp <- lsoashp[match(listlsoa, lsoashp$LSOA11CD), ]

file.copy("data/ladshp.zip", getwd())
unzip(zipfile="ladshp.zip", exdir=getwd())
ladshp <- st_read("ladshp.shp")
file.remove(list.files()[grep("ladshp", list.files(), fixed=TRUE)])
ladshp <- ladshp[match(listlad, ladshp$LAD11CD), ]

################################################################################
# DEFINE THE LSOA EFFECT FUNCTION

lsoaeffect <- function(i, usesim=TRUE) {
  tmeanper <- lsoatmeanper[i, -1] |> as.matrix() |> drop()
  
  argvar <- list(fun=varfun,knots=tmeanper[paste0(varper, ".0%")],
    Bound=tmeanper[c("0.0%", "100.0%")],degree=vardegree)
  
  estlist <- lapply(seq_along(agevarlab), function(j) {
    lsoavar <- cbind(agegr=agevarlab[j], lsoacomp[i, ])
    form <- paste("~",paste(c("agegr", names(lsoacomp)[-1]), collapse="+")) |>
      formula()
    
    xdes <- model.matrix(form, data=lsoavar,xlev=list(agegr=agevarlab))
    xkron <- xdes %x% diag(vardf)
    fit <- drop(xkron %*% coefmeta)
    
    seqtemp <- seq(round(tmeanper["1.0%"],1),round(tmeanper["99.0%"],1),by=0.1)
    bvar <- do.call(onebasis, c(list(x=seqtemp), argvar))
    mmt <- seqtemp[which.min(bvar %*% fit)]
    
    temp <- pmax(as.numeric(tmeandat[i, -1]), mmt)
    bvar <- suppressWarnings(do.call(onebasis, c(list(x=temp), argvar)))
    cenvec <- do.call(onebasis, c(list(x=mmt), argvar))
    bvarcen <- scale(bvar, center=cenvec, scale=FALSE)
    
    deaths <- subset(basedat,LSOA11CD==listlsoa[i]&agegr==agevarlab[j])$deathyear/365.25
    
    estimate <- drop((1-exp(-bvarcen %*% fit))*deaths)
    
    if(!usesim) return(matrix(estimate, ncol=1))
    
    coefsim <- simcoef %*% t(xkron)
    sim <- (1-exp(-bvarcen %*% t(coefsim)))*deaths
    cbind(estimate, sim)
  })
  
  est <- abind(estlist, along=3) |> aperm(c(3, 1, 2))
  allage <- array(apply(est, 2:3, sum), dim=c(1, dim(est)[-1]))
  est <- abind(est, allage, along=1)
  
  dimnames(est) <- list(c(agevarlab, "all"),as.character(dates),
    c("est", if(usesim) paste0("sim", seq(nsim))))
  est
}

################################################################################
# PREPARE PARALLEL TASKS

chunks <- split(seq_along(listlsoa), ceiling(seq_along(listlsoa) / chunksize))

parobj <- c(
  "lsoaeffect", "agevarlab", "lsoatmeanper", "varfun", "varper",
  "vardegree", "lsoacomp", "coefmeta", "vardf", "tmeandat",
  "basedat", "listlsoa", "dates", "nsim", "simcoef", "hwday",
  "hwperiod", "periodlab", "regcode", "lsoareg"
)

################################################################################
# DEFINE LOCAL HEATWAVES FOR THE SENSITIVITY ANALYSIS

# Each LSOA uses its own study-period percentile and event dates.
tempmat <- as.matrix(tmeandat[-1])
hwcut <- apply(tempmat, 1, quantile, probs = hwprob, na.rm = T, names = F)

hwday <- matrix(FALSE,nrow=nrow(tempmat), ncol=ncol(tempmat),
  dimnames=list(listlsoa, as.character(dates)))
hwnevent <- integer(length(listlsoa))
hwperiods <- vector("list", length(listlsoa))

for(i in seq_along(listlsoa)) {
  above <- !is.na(tempmat[i, ]) & tempmat[i, ] > hwcut[i]
  runs <- rle(above)
  runend <- cumsum(runs$lengths)
  runstart <- runend-runs$lengths+1
  valid <- which(runs$values & runs$lengths >= hwminday)
  
  if(length(valid)) {
    hwnevent[i] <- length(valid)
    hwperiods[[i]] <- data.frame(LSOA11CD=listlsoa[i], event=seq_along(valid),
      start=dates[runstart[valid]], end=dates[runend[valid]],
      ndays=runs$lengths[valid], threshold=round(hwcut[i], 2))
    
    for(k in seq_along(valid)) {
      index <- seq(runstart[valid[k]], runend[valid[k]])
      hwday[i, index] <- TRUE
    }
  }
}

hwperiods <- rbindlist(hwperiods)

if(!nrow(hwperiods)) {
  hwperiods <- data.table(LSOA11CD=character(), event=integer(),
    start=as.Date(character()), end=as.Date(character()),
    ndays=integer(), threshold=numeric())
}

hwlsoa <- data.frame(LSOA11CD=listlsoa,threshold=round(hwcut, 2),
  nevent=hwnevent,ndays=rowSums(hwday))

hwdatesum <- data.frame(date=dates, nlsoa=colSums(hwday),
  proportion=colMeans(hwday))

write.csv(hwlsoa, "tables/hwdef_lsoa_2026.csv", row.names=F)
write.csv(hwperiods, "tables/hwperiod_lsoa_2026.csv", row.names=F)
# write.csv(hwdatesum, "tables/hwday_summary_2026.csv", row.names=F)
saveRDS(hwday, "temp/hwday_lsoa_2026.rds")

rm(tempmat)
