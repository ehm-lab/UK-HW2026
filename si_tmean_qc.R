################################################################################
# UK-HW2026: 2026 SUMMER HEAT-RELATED EXCESS MORTALITY
# SUPPLEMENTARY SCRIPT: COMPARE OPEN-METEO AND ERA5-LAND TEMPERATURE
################################################################################

################################################################################
# LOAD PACKAGES AND DATA
library(data.table)
library(ggplot2)
library(arrow)
era5data <- read_parquet(sprintf(
  "%s/LSOA_daily_ERA5_2026_t2m.parquet", "download"))
era5data2 <- read_parquet(sprintf(
  "%s/LSOA_polys_daily_ERA5Land_2026_t2m_upd.parquet", "download"))
d1 <- as.data.table(era5data)[, .(LSOA11CD, date=as.IDate(date), t2m_ERA5=t2m)]
d2 <- as.data.table(era5data2)[, .(LSOA11CD, date=as.IDate(date), 
  t2m_ERA5Land=t2m_e5l)]
# d1[, .N, by=.(LSOA11CD, date)][N > 1]
# d2[, .N, by=.(LSOA11CD, date)][N > 1]

d1[, .(n_lsoa=uniqueN(LSOA11CD), n_date=uniqueN(date), start=min(date), 
  end=max(date), n=.N)]
d2[, .(n_lsoa=uniqueN(LSOA11CD), n_date=uniqueN(date), start=min(date),
  end=max(date), n=.N)]
setkey(d1, LSOA11CD, date)
setkey(d2, LSOA11CD, date)

tempcompare <- merge(d1, d2, by=c("LSOA11CD", "date"), all=T)
tempcompare2 <- tempcompare[!is.na(t2m_ERA5) & !is.na(t2m_ERA5Land)]
tempcompare2[, .(n=.N,
  pearson_r=cor(t2m_ERA5, t2m_ERA5Land, method="pearson"),
  spearman_r=cor(t2m_ERA5, t2m_ERA5Land, method="spearman"),
  mean_ERA5=mean(t2m_ERA5),
  mean_ERA5Land=mean(t2m_ERA5Land),
  bias_ERA5_minus_ERA5Land =mean(t2m_ERA5 - t2m_ERA5Land),
  MAE=mean(abs(t2m_ERA5 - t2m_ERA5Land)),
  RMSE=sqrt(mean((t2m_ERA5 - t2m_ERA5Land)^2)))]
cor_by_lsoa <- tempcompare2[, .(n_days=.N,
  pearson_r=cor(t2m_ERA5, t2m_ERA5Land),
  bias=mean(t2m_ERA5 - t2m_ERA5Land),
  MAE=mean(abs(t2m_ERA5 - t2m_ERA5Land)),
  RMSE=sqrt(mean((t2m_ERA5 - t2m_ERA5Land)^2))), by=LSOA11CD]

summary(cor_by_lsoa$pearson_r)
summary(cor_by_lsoa$bias)
summary(cor_by_lsoa$RMSE)

################################################################################
# SELECT LSOAs

set.seed(123)
selected_lsoa <- sample(unique(tempcompare2$LSOA11CD), size=6)
# selected_lsoa <- c("E01000001", selected_lsoa)

################################################################################
# PREPARE PLOTTING DATA

tempplot <- tempcompare2[LSOA11CD %in% selected_lsoa]
tempplot <- melt(tempplot,id.vars=c("LSOA11CD", "date"),
  measure.vars=c("t2m_ERA5", "t2m_ERA5Land"),  variable.name="data_source",
  value.name="temperature")
tempplot$date <- as.Date(tempplot$date)
tempplot$data_source <- factor(tempplot$data_source,
  levels=c("t2m_ERA5", "t2m_ERA5Land"),
  labels=c("ERA5/Open-Meteo", "ERA5-Land/poly"))

p_tempcompare <- ggplot(tempplot,aes(x=date,y=temperature,colour=data_source)) +
  geom_line(linewidth=0.55, alpha=0.85) +
  facet_wrap(~LSOA11CD, ncol=2) +
  scale_colour_manual(values=c("ERA5/Open-Meteo"="#0072B2", 
    "ERA5-Land/poly"="#D55E00")) +
  scale_x_date(date_breaks="1 month",  date_labels="%d %b") +
  labs(x=NULL, y="Daily mean temperature (°C)",  colour="Data source",
    title="Comparison of daily temperatures from two data sources",
    subtitle="Selected LSOAs, May–August 2026") +
  theme_bw() +
  theme(legend.position="bottom", axis.text.x=element_text(angle=45, hjust=1))

print(p_tempcompare)
ggsave("figures/FigS2_t2m_compare.png", p_tempcompare, 
  width=14, height=9, dpi=300)
