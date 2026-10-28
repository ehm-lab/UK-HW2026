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

# Process ordered LSOA chunks and combine their results by row
daylsoa <- foreach(ids=chunks, .combine="rbind", .inorder=TRUE, .packages=pkg
) %dopar% {
  # Create one row per LSOA and one column per study date in this chunk
  out <- matrix(NA_real_, nrow=length(ids), ncol=length(dates))
  
  # Calculate the daily all-age point estimate for each LSOA
  for(k in seq_along(ids)) {
    est <- flsoaeff(ids[k], usesim=FALSE)
    # Store the all-age point-estimate series in the matching chunk row
    out[k, ] <- est["all", , 1]
  }
  # Return the completed chunk matrix for row binding by foreach
  out
}

# Close the parallel workers after all chunks have completed
stopCluster(cl)

# Label rows with ordered LSOA codes and columns with dates
dimnames(daylsoa) <- list(listlsoa, as.character(dates))

# Save daily LSOA point estimates
saveRDS(daylsoa, "temp/daylsoa.rds")

# Request memory cleanup
gc()
