################################################################################
# UK-HW2026: 2026 SUMMER HEAT-RELATED EXCESS MORTALITY
# SCRIPT 02C: CALCULATE DAILY LSOA POINT ESTIMATES
################################################################################

################################################################################
# CALCULATE DAILY LSOA RESULTS

# Start a parallel cluster and copy point-estimate objects to each worker
cl <- parallel::makeCluster(ncore)
registerDoParallel(cl)
parallel::clusterExport(cl, parobj, envir=.GlobalEnv)

# Count four age-specific groups plus the all-age category
nage <- length(agevarlab)+1

# Process ordered LSOA chunks and combine their age-specific results by row
daylsoamat <- foreach(ids=chunks, .combine="rbind", .inorder=TRUE, .packages=pkg
) %dopar% {
  # Create one row per LSOA-age combination and one column per study date
  out <- matrix(NA_real_, nrow=length(ids)*nage, ncol=length(dates))
  
  # Calculate daily age-specific and all-age point estimates for each LSOA
  for(k in seq_along(ids)) {
    est <- flsoaeff(ids[k], usesim=FALSE)
    
    # Store all age-group series in the matching block of chunk rows
    rowid <- ((k-1)*nage+1):(k*nage)
    out[rowid, ] <- est[c(agevarlab, "all"), , 1]
  }
  
  # Return the completed chunk matrix for row binding by foreach
  out
}

# Close the parallel workers after all chunks have completed
stopCluster(cl)

# Reshape the combined matrix to LSOA-by-age-by-date order
daylsoaage <- aperm(
  array(daylsoamat, dim=c(nage, length(listlsoa), length(dates))),
  c(2, 1, 3))
dimnames(daylsoaage) <- list(LSOA11CD=listlsoa, agegr=c(agevarlab, "all"),
  date=as.character(dates))

# Save daily LSOA point estimates with the age dimension
saveRDS(daylsoaage, "temp/daylsoaage.rds") 

# Request memory cleanup
rm(daylsoamat)
gc()
