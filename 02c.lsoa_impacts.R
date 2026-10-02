################################################################################
# UK-HW2026: 2026 SUMMER HEAT-RELATED EXCESS MORTALITY
# SCRIPT 02C: CALCULATE DAILY AND PERIOD RESULTS BY LSOA
################################################################################

################################################################################
# CALCULATE DAILY LSOA RESULTS

# Maps require point estimates only.
simcoef <- NULL

cl <- parallel::makeCluster(ncore)
registerDoParallel(cl)
parallel::clusterExport(cl, parobj, envir=.GlobalEnv)

daylsoa <- foreach(ids=chunks, .combine="rbind", .inorder=TRUE,
  .packages=c("dlnm", "data.table", "abind")
) %dopar% {
  out <- matrix(NA_real_, nrow=length(ids), ncol=length(dates))
  
  for(k in seq_along(ids)) {
    est <- lsoaeffect(ids[k], usesim=FALSE)
    out[k, ] <- est["all", , 1]
  }
  out
}

stopCluster(cl)

dimnames(daylsoa) <- list(listlsoa, as.character(dates))
saveRDS(daylsoa, "temp/daylsoa_2026.rds")

################################################################################
# CALCULATE STUDY-PERIOD LSOA RESULTS

# totstudy <- rowSums(daylsoa)
totstudy <- pmax(rowSums(daylsoa), 0)

popdat <- subset(basedat, agegr == "all")
popall <- popdat$pop[match(listlsoa, popdat$LSOA11CD)]

lsoatab <- data.frame(LSOA11CD=listlsoa,population=popall,eststudy=totstudy,
  ratestudy=totstudy/popall*10^6)

write.csv(lsoatab, "tables/lsoa_period_2026.csv", row.names=F)

rm(simcoef, popdat)
gc()
