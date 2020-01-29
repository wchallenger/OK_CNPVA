#' Outcome Table
#'
#' @param run.set 
#'
#' @return
#' @export
#'
#' @examples
OutcomeTable <- function(run.set, verbose=FALSE) {
  
  # browser()

  message("NOTE: time periods used in OutcomeTable are hard coded.")
  # Helper Functions --------------------------------------------------------
  # Rolling Geometric Mean based on average life history generation time.
  RollGeomMean <- function(x, k = 4){exp(stats::filter(log(x), rep(1 / k, k), sides = 2))}
  
  # Estiamtes a trend
  Trend <- function(x){
    slope <- lm(log(x)~seq_len(length(x)))$coef[2]
    return(100*(exp(10*slope)-1))
  }
  
  
  result <- data.frame(Scenario = names(run.set))
  for (r in seq_along(run.set)) {
    
    run = names(run.set)[r]
    if (verbose) message("Processing: ", run, " (", r, " of ", length(run.set), ")")
    
    # Spawner Targets ----------------------------------------------
    
    # Compute 4-year geometric rolling mean of spawners for each simulation (column)
    spawners <- apply(run.set[[run]][['spawners']], 2, RollGeomMean, k=4)
    rownames(spawners) <- rownames(run.set[[run]][['spawners']])
    
    #--- Short-term (12 years) ---
    i <- which(rownames(spawners) %in% 2026:2037)
    
    # # average over  period
    # avg.spawners <- apply(spawners[i, ], 2, mean)
    # result$Prob.short[r] = mean(avg.spawners >= 1000)
    
    # alterative measure
    max.spawners <- apply(spawners[i, ], 2, max)   # maximum wihtin the 12 year window
    result$Prob.short[r] = mean(max.spawners >= 1000)
  
    #--- Long-term (30 years) ---
    
    i <- which(rownames(spawners) %in% 2026:2055)
    # Averaged over period
    # avg.spawners <- apply(spawners[i, ], 2, mean, na.rm=TRUE)
    # result$Prob.long[r] = mean(avg.spawners >= 1000)
    
    max.spawners <- apply(spawners[i, ], 2, max)  # maximum wihtin the 30 year window
    result$Prob.long[r] = mean(max.spawners >= 1000)
   
    
    # Population Trend (year, age, sim) -------------------------------
    # browser()
    # compute population totals (ocean) across age classes
    totals.raw <- apply(run.set[[run]][['adult.pop']], c(1,3), sum, na.rm=T)  
    
    # Compute 4-year geometric rolling mean of spawners for each simulation (column)
    adults <- apply(totals.raw, 2, RollGeomMean, k=4)
    rownames(adults) <- rownames(totals.raw)
    
    #--- Short-term (12 years) ---
    i <- which(rownames(adults) %in% 2026:2037)
    result$Trend.short[r] <- mean(apply(adults[i, ], 2, Trend))
    #--- Long-term (30 years) ---
    i <- which(rownames(adults) %in% 2026:2055)
    result$Trend.long[r] <- mean(apply(adults[i, ], 2,Trend))
    
  }
  result <- result %>%
    mutate(
      Outcome.short     = IPCCRiskCat(Prob.short),
      # Outcome.short.alt = IPCCRiskCat(Prob.short.alt),
      Outcome.long      = IPCCRiskCat(Prob.long),
      # Outcome.long.alt  = IPCCRiskCat(Prob.long.alt),
      TrendCat.short    = ifelse(Trend.short <= 0, "Negative", "Positive"),
      TrendCat.long     = ifelse(Trend.long <= 0, "Negative", "Positive")
    )
  result
}