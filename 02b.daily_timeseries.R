################################################################################
# UK-HW2026: 2026 SUMMER HEAT-RELATED EXCESS MORTALITY
# SCRIPT 02B: CALCULATE DAILY RESULTS BY AGE
################################################################################

################################################################################
# CALCULATE DAILY RESULTS

# Start a new parallel cluster and copy model objects to each worker
cl <- parallel::makeCluster(ncore)
registerDoParallel(cl)
parallel::clusterExport(cl, parobj, envir=.GlobalEnv)

# Count five England-and-Wales age series plus one England all-age series
nage <- length(agevarlab)+1 # 4 age-specific groups + all ages; 
nseries <- nage+1 # + England all-age
ntype <- nsim+1 # 1 point estimate + 500 simulations

# Process LSOAs in chunks and add the chunk-specific daily arrays together
daycomb <- foreach(ids=chunks, .combine="+", .packages=pkg
) %dopar% {
  # Create a series-by-date-by-estimate array for this chunk
  out <- array(0, dim=c(nseries, length(dates), ntype))
  
  # Add each LSOA result to the national daily series
  for(i in ids) {
    # Calculate age-specific daily point estimates and 500 simulations
    est <- flsoaeff(i, usesim=TRUE)
    # Add all five age series to the England-and-Wales totals
    out[seq_len(nage), , ] <- out[seq_len(nage), , ] + est
    
    # Add non-Welsh LSOAs to the separate England all-age series
    if(lsoareg[i] != "W92000004") {
      out[nseries, , ] <- out[nseries, , ] + est["all", , ]
    }
  }
  # Return the completed chunk array to foreach for addition
  out
}

# Close the parallel workers after all chunks have completed
stopCluster(cl)

# Label the combined series, date and estimate dimensions
dimnames(daycomb) <- list(
  series=c(agevarlab, "all", "englandall"),
  date=as.character(dates),
  type=c("est", paste0("sim", seq(nsim)))
)

# Retain the five England-and-Wales age series for daily summaries and Figure 1
dayage <- daycomb[c(agevarlab, "all"), , , drop=FALSE]
dimnames(dayage)[[1]] <- c(agevarlab, "all")
names(dimnames(dayage))[1] <- "agegr"

# Retain the England all-age series for comparison with UKHSA estimates
dayeng <- daycomb["englandall", , ]

################################################################################
# SAVE RESULTS

# Save the daily arrays with their point estimates and simulations
saveRDS(dayage, "temp/dayage.rds")
saveRDS(dayeng, "temp/dayeng.rds")

# Remove large objects and request memory cleanup before the next script
rm(daycomb, dayage, dayeng)
gc()
