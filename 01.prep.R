################################################################################
# UK-HW2026: 2026 SUMMER HEAT-RELATED EXCESS MORTALITY
# SCRIPT 01: PREPARE MODEL, POPULATION, TEMPERATURE AND GEOGRAPHY DATA
################################################################################

################################################################################
# SET ANALYSIS OPTIONS

# Define study period and computation settings
datestart <- as.Date("2026-05-01")
dateend <- as.Date("2026-09-30")

# Prepare the parallelisation
ncore <- max(1, min(4, parallel::detectCores()-2))
chunksize <- 100

# Number of simulation for eCI of excess deaths
nsim <- 500

# Define five heatwave periods used in event-specific analyses
hwperiod <- data.frame(event=paste0("HW", 1:5),
  start=as.Date(c(
    "2026-05-22","2026-06-18","2026-07-06","2026-07-29","2026-08-11")),
  end=as.Date(c(
    "2026-05-28","2026-06-28","2026-07-17","2026-08-05","2026-08-15")))
hwperiod$label <- paste0(
  hwperiod$event, ": ", format(hwperiod$start, "%d %b"), "-",
  format(hwperiod$end, "%d %b"))

# Define complete study-period label
periodlab <- "1 May-30 Sept" 

################################################################################
# LOAD MODEL PARAMETERS

# Load model objects
obj <- c("coefmeta", "vcovmeta", "lsoacomp", "lsoatmeanper")
for(x in obj)  assign(x, readRDS(paste0("data/", x, ".RDS")))

# Define age groups
agelab <- c("0-64", "65-74", "75-84", "85 and above")
agevarlab <- c("age064", "age6574", "age7584", "age85plus")

# Set exposure-response basis parameters
varfun <- "bs"
varper <- c(10, 75, 90)
vardegree <- 2
vardf <- 5

################################################################################
# PREPARE GEOGRAPHY LOOKUP

# Load lookup using the specified encoding
unzip(zipfile="data/lookup.zip")
lookup <- read.csv("data/lookup.csv", fileEncoding="UTF-8-BOM")
file.remove("data/lookup.csv")

# Order lookup by LSOA and LAD
lookup <- lookup[with(lookup, order(LSOA11CD, LAD11CD)), ]

# Create ordered lists of LSOAs, LADs and regions
listlsoa <- unique(lookup$LSOA11CD)
listlad <- sort(unique(lookup$LAD11CD))

reglookup <- unique(lookup[c("RGN11CD", "RGN11NM")])
regcode <- c(paste0("E1200000", 1:9), "W92000004")
reglookup <- reglookup[match(regcode, reglookup$RGN11CD), ]
regname <- reglookup$RGN11NM
lsoareg <- lookup$RGN11CD[match(listlsoa, lookup$LSOA11CD)]

# Match model inputs to the LSOA order
lsoacomp <- lsoacomp[match(listlsoa, lsoacomp$LSOA11CD), ]
lsoatmeanper <- lsoatmeanper[match(listlsoa, lsoatmeanper$LSOA11CD), ]

################################################################################
# PREPARE BASELINE POPULATION (2020) AND MORTALITY RATES (2024)

# Load population data
unzip(zipfile="data/population_2020.zip")
pop <- read.csv("data/population_2020.csv")
file.remove("data/population_2020.csv")

# Label age columns
names(pop)[1] <- "LSOA11CD"
names(pop)[-1] <- paste0("age", 0:90)

# Aggregate population into five-year age groups
agecut1 <- c(0, 1, 1:18*5)
agecut2 <- c(0, 1:18*5-1, 90)

pop <- cbind(pop[1],
  mapply(":", agecut1, agecut2) |>
    lapply(function(x) rowSums(pop[paste0("age", x)])) |>
    Reduce(cbind, x=_))

names(pop)[-1] <- mapply(paste0, "age", sprintf("%02d", agecut1),
  sprintf("%02d", agecut2))

# Reshape population data to long format
pop <- pivot_longer(pop, !LSOA11CD, names_to="agegr", values_to="pop") |>
  as.data.frame()

# Load regional mortality rates
unzip(zipfile="data/deathrates_2024.zip")
deathrate <- read.csv("data/deathrates_2024.csv")
file.remove("data/deathrates_2024.csv")

# Label mortality-rate age groups
deathrate[, 1] <- unique(pop$agegr)
names(deathrate)[1] <- "agegr"

# Reshape mortality rates to long format
deathrate <- pivot_longer(deathrate,-1,names_to="RGN11CD",values_to="rate") |>
  as.data.frame()

# Combine population and mortality rates to estimate annual baseline deaths
basedat <- lookup[c("LSOA11CD", "RGN11CD")] |> merge(pop) |>
  merge(deathrate) |> mutate(deathyear=rate*pop/10^5)

# Aggregate by age groups
basedat$agegr <- factor(basedat$agegr, levels=unique(deathrate[, 1]))
levels(basedat$agegr) <- rep(agevarlab, c(14, 2, 2, 2))
basedat <- summarise(basedat, pop=sum(pop), deathyear=sum(deathyear),
  .by=c(LSOA11CD, agegr)) |> arrange(LSOA11CD, agegr)

# Add all-age totals
basedat <- summarise(basedat,pop=sum(pop),deathyear=sum(deathyear),
  .by=LSOA11CD) |> mutate(agegr="all") |> rbind(basedat) |>
  arrange(LSOA11CD, agegr)

rm(pop, deathrate)

################################################################################
# PREPARE DAILY MEAN TEMPERATURE

# Load and restrict daily mean temperatures to the study period
tempdat <- read_parquet("download/LSOA_daily_ERA5_2026m2s_t2m.parquet")
tempdat <- as.data.table(tempdat)
tempdat[, date := as.Date(date, tz="Europe/London")]
tempdat <- tempdat[date >= datestart & date <= dateend]
setorder(tempdat, LSOA11CD, date)
dates <- seq(datestart, dateend, by="day")

# Check LSOA-date uniqueness and coverage
if(anyDuplicated(tempdat, by=c("LSOA11CD", "date")) > 0L)
  stop("Daily mean-temperature data contain duplicate LSOA-date rows")

if(!setequal(unique(tempdat$LSOA11CD), listlsoa) ||
    !setequal(unique(tempdat$date), dates) ||
    nrow(tempdat) != length(listlsoa)*length(dates))
  stop("Daily mean-temperature data must contain one row per LSOA and date")

# Reshape temperatures to one row per LSOA
tmeandat <- dcast(tempdat, LSOA11CD ~ date, value.var="t2m")
tmeandat <- tmeandat[match(listlsoa, LSOA11CD)]
tmeandat <- as.data.frame(tmeandat)

# Order and label date columns
tmeandat <- tmeandat[c("LSOA11CD", as.character(dates))]
names(tmeandat)[-1] <- format(dates, "%d %B %Y")

################################################################################
# PREPARE SHAPEFILES

# Load and order generalised LSOA boundaries
file.copy("data/lsoashp.zip", getwd())
unzip(zipfile="lsoashp.zip", exdir=getwd())
lsoashp <- st_read("lsoashp.shp")
file.remove(list.files()[grep("lsoashp", list.files(), fixed=TRUE)])
lsoashp <- lsoashp[match(listlsoa, lsoashp$LSOA11CD), ]

# Load and order LAD boundaries
file.copy("data/ladshp.zip", getwd())
unzip(zipfile="ladshp.zip", exdir=getwd())
ladshp <- st_read("ladshp.shp")
file.remove(list.files()[grep("ladshp", list.files(), fixed=TRUE)])
ladshp <- ladshp[match(listlad, ladshp$LAD11CD), ]

################################################################################
# DEFINE THE LSOA EFFECT FUNCTION

# Estimate daily heat-related deaths by LSOA and age group
lsoaeffect <- function(i, usesim=TRUE) {
  
  # Extract local temperature percentiles
  tmeanper <- lsoatmeanper[i, -1] |> as.matrix() |> drop()
  
  # Define the local temperature basis
  argvar <- list(fun=varfun,knots=tmeanper[paste0(varper, ".0%")],
    Bound=tmeanper[c("0.0%", "100.0%")],degree=vardegree)
  
  # Estimate effects for each age group
  estlist <- lapply(seq_along(agevarlab), function(j) {
    
    # Generate age- and location-specific model coefficients
    lsoavar <- cbind(agegr=agevarlab[j], lsoacomp[i, ])
    form <- paste("~",paste(c("agegr", names(lsoacomp)[-1]), collapse="+")) |>
      formula()
    
    xdes <- model.matrix(form, data=lsoavar,xlev=list(agegr=agevarlab))
    xkron <- xdes %x% diag(vardf)
    fit <- drop(xkron %*% coefmeta)
    
    # Identify the local minimum-mortality temperature
    seqtemp <- seq(round(tmeanper["1.0%"],1),round(tmeanper["99.0%"],1),by=0.1)
    bvar <- do.call(onebasis, c(list(x=seqtemp), argvar))
    mmt <- seqtemp[which.min(bvar %*% fit)]
    
    # Centre the temperature basis at the minimum-mortality temperature
    temp <- pmax(as.numeric(tmeandat[i, -1]), mmt)
    bvar <- suppressWarnings(do.call(onebasis, c(list(x=temp), argvar)))
    cenvec <- do.call(onebasis, c(list(x=mmt), argvar))
    bvarcen <- scale(bvar, center=cenvec, scale=FALSE)
    
    # Calculate daily baseline deaths
    deaths <- subset(basedat,
      LSOA11CD==listlsoa[i]&agegr==agevarlab[j])$deathyear/365
    
    # Calculate point estimates of daily heat-related deaths
    estimate <- drop((1-exp(-bvarcen %*% fit))*deaths)
    
    # Return point estimates when simulations are not requested
    if(!usesim) return(matrix(estimate, ncol=1))
    
    # Calculate simulated daily heat-related deaths
    coefsim <- simcoef %*% t(xkron)
    sim <- (1-exp(-bvarcen %*% t(coefsim)))*deaths
    cbind(estimate, sim)
  })
  
  # Combine age-specific estimates and calculate all-age totals
  est <- abind(estlist, along=3) |> aperm(c(3, 1, 2))
  allage <- array(apply(est, 2:3, sum), dim=c(1, dim(est)[-1]))
  est <- abind(est, allage, along=1)
  
  # Label output dimensions
  dimnames(est) <- list(c(agevarlab, "all"),as.character(dates),
    c("est", if(usesim) paste0("sim", seq(nsim))))
  est
}

################################################################################
# PREPARE PARALLEL TASKS

# Divide LSOAs into processing chunks
chunks <- split(seq_along(listlsoa), ceiling(seq_along(listlsoa) / chunksize))

# List objects required by parallel workers
parobj <- c(
  "lsoaeffect", "agevarlab", "lsoatmeanper", "varfun", "varper",
  "vardegree", "lsoacomp", "coefmeta", "vardf", "tmeandat",
  "basedat", "listlsoa", "dates", "nsim", "simcoef",
  "hwperiod", "periodlab", "regcode", "lsoareg"
)
