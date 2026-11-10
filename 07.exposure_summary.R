################################################################################
# UK-HW2026: 2026 SUMMER HEAT-RELATED EXCESS MORTALITY
# SCRIPT 07: DESCRIBE DAILY MAXIMUM TEMPERATURE AND CREATE FIGURE S1
################################################################################

################################################################################
# CHECK REQUIRED OBJECTS AND STUDY DATES

# Run after 01.prep.R and confirm the expected study period
stopifnot(exists("dates"), exists("hwperiod"), exists("listlsoa"),
  min(dates) == as.Date("2026-05-01"), max(dates) == as.Date("2026-09-30"))

################################################################################
# PREPARE DAILY MAXIMUM TEMPERATURE

# Load daily maximum temperatures
tmax_file <- "download/LSOA_daily_ERA5_2026m2s_tmax.parquet"
tmaxdat <- as.data.table(read_parquet(tmax_file))

# Restrict data to the study dates and order observations
tmaxdat[, date := as.Date(date, tz="Europe/London")]
tmaxdat <- tmaxdat[date %in% dates]
setorder(tmaxdat, LSOA11CD, date)

# Check values, LSOA-date uniqueness and coverage
if(anyNA(tmaxdat$tmax) || anyDuplicated(tmaxdat, by=c("LSOA11CD", "date")))
  stop("Daily maximum-temperature data contain missing values or duplicate rows")

if(!setequal(unique(tmaxdat$LSOA11CD), listlsoa) ||
    !setequal(unique(tmaxdat$date), dates) ||
    nrow(tmaxdat) != length(listlsoa)*length(dates))
  stop("Daily maximum-temperature data must contain one row per LSOA and date")

# Summarise the daily distribution and spatial extent of high temperatures
tmax_daily <- tmaxdat[, .(
  median_tmax=median(tmax),
  p95_tmax=as.numeric(quantile(tmax, 0.95, names=F)),
  max_tmax=max(tmax),
  n_lsoa_gt30=sum(tmax > 30),
  pct_lsoa_gt30=mean(tmax > 30)*100
), by=date][order(date)]

# Identify the maximum temperature and widest spatial exceedance
peak_tmax <- tmax_daily[which.max(max_tmax)]
peak_extent <- tmax_daily[which.max(pct_lsoa_gt30)]

# Assemble the exposure statistics reported in the manuscript
exposure_summary <- data.table(
  metric=c(
    "Maximum LSOA-level daily maximum temperature",
    "Days with daily maximum temperature above 30 C in at least one LSOA",
    "Maximum number of LSOAs above 30 C",
    "Maximum percentage of LSOAs above 30 C"),
  value=c(
    peak_tmax$max_tmax,
    sum(tmax_daily$n_lsoa_gt30 > 0),
    peak_extent$n_lsoa_gt30,
    peak_extent$pct_lsoa_gt30),
  unit=c("degrees C", "days", "LSOAs", "percent"),
  date=as.Date(c(peak_tmax$date, NA, peak_extent$date, peak_extent$date))
)

################################################################################
# CREATE FIGURE S1

# Reshape daily temperature summaries for plotting
tmax_long <- melt(tmax_daily, id.vars="date",
  measure.vars=c("median_tmax", "p95_tmax", "max_tmax"),
  variable.name="summary", value.name="temperature")
tmax_long[, summary := factor(summary,
  levels=c("median_tmax", "p95_tmax", "max_tmax"),
  labels=c("Median", "95th percentile", "Maximum across LSOAs"))]

# Define heatwave-period shading for both panels
hwrect <- as.data.table(hwperiod)[, .(start, end)]
hwrect[, `:=`(ymin=-Inf, ymax=Inf)]
hwrect_pct <- as.data.table(hwperiod)[, .(start, end)]
hwrect_pct[, `:=`(ymin=0, ymax=100)]

# Set the common figure theme
theme_tmax <- theme_bw(base_size=11) +
  theme(panel.grid.minor=element_blank(), legend.position="bottom")

# Plot the daily temperature distribution
ptmax_distribution <- ggplot() +
  geom_rect(data=hwrect, aes(xmin=start, xmax=end, ymin=ymin, ymax=ymax),
    inherit.aes=F, fill="gold", alpha=0.12) +
  geom_hline(yintercept=30, linetype="dashed", colour="firebrick",
    linewidth=0.4) +
  geom_line(data=tmax_long,
    aes(x=date, y=temperature, colour=summary, linetype=summary),
    linewidth=0.65) +
  geom_point(data=peak_tmax, aes(x=date, y=max_tmax),
    colour="firebrick", size=2) +
  scale_colour_manual(values=c("#1B9E77", "#D95F02", "#4D4D4D")) +
  scale_linetype_manual(values=c("solid", "dashed", "solid")) +
  scale_x_date(date_breaks="1 month", date_labels="%b") +
  labs(x=NULL, y=expression("Daily maximum temperature ("*degree*C*")"),
    colour=NULL, linetype=NULL) +
  theme_tmax

# Plot the percentage of LSOAs exceeding 30 degrees C
pgt30 <- ggplot() +
  geom_rect(data=hwrect_pct, aes(xmin=start, xmax=end, ymin=ymin, ymax=ymax),
    inherit.aes=F, fill="gold", alpha=0.12) +
  geom_area(data=tmax_daily, aes(x=date, y=pct_lsoa_gt30),
    fill="#9ECAE1", alpha=0.7) +
  geom_line(data=tmax_daily, aes(x=date, y=pct_lsoa_gt30),
    colour="#176B87", linewidth=0.65) +
  geom_point(data=peak_extent, aes(x=date, y=pct_lsoa_gt30),
    colour="firebrick", size=2) +
  scale_x_date(date_breaks="1 month", date_labels="%b") +
  scale_y_continuous(limits=c(0, 100), breaks=seq(0, 100, 25),
    expand=expansion(mult=c(0, 0.08))) +
  labs(x=NULL, y=expression("LSOAs with Tmax >30"*degree*C*" (%)")) +
  theme_tmax

# Combine and save both panels
fig_tmax <- (ptmax_distribution/pgt30) + plot_layout(heights=c(1, 0.9))

# print(fig_tmax)
ggsave("figures/FigS1_tmax_extremes.png", plot=fig_tmax,
  width=2000, height=1600, units="px", dpi=300, bg="white")

# Print manuscript exposure statistics and remove the full input data
print(exposure_summary)
rm(tmaxdat)
