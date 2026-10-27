################################################################################
# UK-HW2026: 2026 SUMMER HEAT-RELATED EXCESS MORTALITY
# SCRIPT 02B: CALCULATE DAILY RESULTS BY AGE
################################################################################

################################################################################
# CALCULATE DAILY RESULTS

cl <- parallel::makeCluster(ncore)
registerDoParallel(cl)
parallel::clusterExport(cl, parobj, envir=.GlobalEnv)

nage <- length(agevarlab)+1
nseries <- nage+1
ntype <- nsim+1

daycomb <- foreach(ids=chunks, .combine="+", .packages=pkg
) %dopar% {
  # The first five series cover England and Wales by age.
  # The last series is the England all-age result for UKHSA comparison.
  out <- array(0, dim=c(nseries, length(dates), ntype))
  
  for(i in ids) {
    est <- flsoaeff(i, usesim=TRUE)
    out[seq_len(nage), , ] <- out[seq_len(nage), , ] + est
    
    if(lsoareg[i] != "W92000004") {
      out[nseries, , ] <- out[nseries, , ] + est["all", , ]
    }
  }
  out
}

stopCluster(cl)

dimnames(daycomb) <- list(
  series=c(agevarlab, "all", "englandall"),
  date=as.character(dates),
  type=c("est", paste0("sim", seq(nsim)))
)

dayage <- daycomb[c(agevarlab, "all"), , , drop=FALSE]
dimnames(dayage)[[1]] <- c(agevarlab, "all")
names(dimnames(dayage))[1] <- "agegr"

dayeng <- daycomb["englandall", , ]

################################################################################
# SAVE RESULTS

saveRDS(dayage, "temp/dayage_2026.rds")
saveRDS(dayeng, "temp/dayeng_2026.rds")

rm(daycomb, dayage, dayeng, simcoef)
gc()
