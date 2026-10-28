################################################################################
# UK-HW2026: RUN THE COMPLETE ANALYSIS
################################################################################

# Open UK-HW2026.Rproj or set the working directory to the repository root.
# Scripts 02A-02C are computationally intensive and are run sequentially.

scripts <- c(
  "00.pkg.R",
  "01.prep.R",
  "02a.impact_totals.R",
  "02b.daily_timeseries.R",
  "02c.lsoa_impacts.R",
  "03.output.R",
  "04.tables.R",
  "05.figures.R",
  "06.external_comparison.R",
  "07.exposure_summary.R"
)

for(script in scripts) {
  message("Running ", script)
  source(script, echo=FALSE)
}
