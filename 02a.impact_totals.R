################################################################################
# UK-HW2026: 2026 SUMMER HEAT-RELATED EXCESS MORTALITY
# SCRIPT 02A: CALCULATE PERIOD TOTALS BY REGION AND AGE
################################################################################

################################################################################
# CALCULATE REGION AND AGE TOTALS

# Start a parallel cluster and copy the prepared model objects to each worker
cl <- parallel::makeCluster(ncore)
registerDoParallel(cl)
parallel::clusterExport(cl, parobj, envir=.GlobalEnv)

# Define dimensions of the regional result array
nage <- length(agevarlab)+1 # 4 age-specific groups + all ages; 
nreg <- length(regcode) # 10 regions;
nperiod <- nrow(hwperiod)+1 # full study period + 5 HW periods;
ntype <- nsim+1 # 1 point estimate + 500 simulations

# Process LSOAs in chunks and add the chunk-specific regional arrays together
regage <- foreach(ids=chunks, .combine="+", .packages=pkg
) %dopar% {
  # Create an age-by-region-by-period-by-estimate array for this chunk
  out <- array(0, dim=c(nage, nreg, nperiod, ntype))
  
  # Add each LSOA result to its corresponding region
  for(i in ids) {
    # Calculate age-specific daily point estimates and 500 simulations
    est <- flsoaeff(i, usesim=TRUE)
    
    # Match the current LSOA to the ordered region dimension
    regid <- match(lsoareg[i], regcode)
    
    # Sum all study days into the first period position
    out[, regid, 1, ] <- out[, regid, 1, ] + apply(est, c(1, 3), sum)
    
    # Sum the relevant days into each of the five HW periods
    for(k in seq_len(nrow(hwperiod))) {
      dayid <- which(dates >= hwperiod$start[k] & dates <= hwperiod$end[k])
      out[, regid, k+1, ] <- out[, regid, k+1, ] + 
        apply(est[, dayid, , drop=F], c(1, 3), sum)
    }
  }
  # Return the completed chunk array to foreach for addition
  out
}

# Close the parallel workers after all chunks have completed
stopCluster(cl)

# Label the four regional output dimensions
periods <- c(periodlab, hwperiod$label)
dimnames(regage) <- list(agegr=c(agevarlab, "all"), region=regcode,
  period=periods, type=c("est", paste0("sim", seq(nsim))))

# Sum the regional dimension to obtain national results
natage <- apply(regage, c(1, 3, 4), sum)

# Label the three national output dimensions
dimnames(natage) <- list(agegr=c(agevarlab, "all"), period=periods,
  type=c("est", paste0("sim", seq(nsim))))

################################################################################
# SAVE RESULTS

# Save arrays containing point estimates and simulations for later summaries
saveRDS(regage, "temp/regage.rds") # regional results
saveRDS(natage, "temp/natage.rds") # national results

# Remove large objects and request memory cleanup before the next script
rm(regage, natage)
gc()
