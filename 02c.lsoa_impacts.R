################################################################################
# UK-HW2026: 2026 SUMMER HEAT-RELATED EXCESS MORTALITY
# SCRIPT 02C: CALCULATE DAILY LSOA POINT ESTIMATES
################################################################################

################################################################################
# CALCULATE DAILY LSOA RESULTS

cl <- parallel::makeCluster(ncore)
registerDoParallel(cl)
parallel::clusterExport(cl, parobj, envir=.GlobalEnv)

daylsoa <- foreach(ids=chunks, .combine="rbind", .inorder=TRUE, .packages=pkg
) %dopar% {
  out <- matrix(NA_real_, nrow=length(ids), ncol=length(dates))
  
  for(k in seq_along(ids)) {
    est <- flsoaeff(ids[k], usesim=FALSE)
    out[k, ] <- est["all", , 1]
  }
  out
}

stopCluster(cl)

dimnames(daylsoa) <- list(listlsoa, as.character(dates))
saveRDS(daylsoa, "temp/daylsoa_2026.rds")

rm(simcoef)
gc()
