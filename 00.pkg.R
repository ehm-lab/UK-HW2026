################################################################################
# UK-HW2026: 2026 HEAT-RELATED EXCESS MORTALITY ANALYSIS
################################################################################

# LOAD PACKAGES
library(dlnm); library(mixmeta)
library(data.table)
library(dplyr) ; library(tidyr); library(tibble) ; library(lubridate)
library(foreach) ; library(doParallel)
library(MASS)
library(abind)
library(ggplot2) ; library(sf); library(patchwork)
# library(rmapshaper) ; library(ggspatial)
library(arrow)

# OUTPUT FOLDERS
dir.create("temp", showWarnings=FALSE)
dir.create("tables", showWarnings=FALSE)
dir.create("figures", showWarnings=FALSE)
