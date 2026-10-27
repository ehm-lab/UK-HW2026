################################################################################
# UK-HW2026: 2026 SUMMER HEAT-RELATED EXCESS MORTALITY
# SCRIPT 02A: CALCULATE PERIOD TOTALS BY REGION AND AGE
################################################################################

################################################################################
# CALCULATE REGION AND AGE TOTALS

cl <- parallel::makeCluster(ncore)
registerDoParallel(cl)
parallel::clusterExport(cl, parobj, envir=.GlobalEnv)

nage <- length(agevarlab)+1
nreg <- length(regcode)
nperiod <- nrow(hwperiod)+1
ntype <- nsim+1

regage <- foreach(ids=chunks, .combine="+", .packages=pkg
) %dopar% {
  out <- array(0, dim=c(nage, nreg, nperiod, ntype))
  
  for(i in ids) {
    est <- flsoaeff(i, usesim=TRUE)
    regid <- match(lsoareg[i], regcode)
    
    out[, regid, 1, ] <- out[, regid, 1, ] + apply(est, c(1, 3), sum)
    
    for(k in seq_len(nrow(hwperiod))) {
      dayid <- which(dates >= hwperiod$start[k] & dates <= hwperiod$end[k])
      out[, regid, k+1, ] <- out[, regid, k+1, ] + 
        apply(est[, dayid, , drop=F], c(1, 3), sum)
    }
  }
  out
}

stopCluster(cl)

periods <- c(periodlab, hwperiod$label)
dimnames(regage) <- list(agegr=c(agevarlab, "all"), region=regcode,
  period=periods, type=c("est", paste0("sim", seq(nsim))))

natage <- apply(regage, c(1, 3, 4), sum)
dimnames(natage) <- list(agegr=c(agevarlab, "all"), period=periods,
  type=c("est", paste0("sim", seq(nsim))))

################################################################################
# SAVE RESULTS

saveRDS(regage, "temp/regage_2026.rds")
saveRDS(natage, "temp/natage_2026.rds")

rm(regage, natage, simcoef)
gc()
