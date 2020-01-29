#' Get Simulation Spawners
#'
#' @param sim.result 
#' @param type 
#'
#' @return
#' @export
#'
#' @examples
GetSpawners <- function(sim.result, type="summary", min.yr=2006, scenario = NULL) {

  # Summary by Year ---------------------------------------------------------
  if (tolower(type) %in% c("summary", "2050")) {
    result <- data.frame(
      Year = as.numeric(str_extract(dimnames(sim.result$spawners)[[1]], "[:digit:]{4}$")),
      Mean = apply(sim.result$spawners, 1, mean),
      Median = apply(sim.result$spawners, 1, median),
      Lower1 = apply(sim.result$spawners, 1, quantile, prob = 0.01),
      Upper99 = apply(sim.result$spawners, 1, quantile, prob = 0.99),
      Lower95 = apply(sim.result$spawners, 1, quantile, prob = 0.025),
      Upper95 = apply(sim.result$spawners, 1, quantile, prob = 0.975)
    )
    
    if (!is.null(scenario)) {
      result$Scenario <-  scenario
      # browser()
      result <- result %>% select(Scenario, Year:Upper95)
    }
    if (type == "2050") {
      return(filter(result, Year == 2050)) 
    } else {
      return(result)
    }
  } 
  
  if (tolower(type) %in% c("minimum", "mean")) {
    # browser()  
    yr <- as.numeric(str_extract(dimnames(sim.result$spawners)[[1]], "[:digit:]{4}$"))
    
    if (type == "minimum") {
      x <- apply(sim.result$spawners[yr >= min.yr,], 2, min) 
    } else {
      x <- apply(sim.result$spawners[yr >= min.yr,], 2, mean)
    }
   
    result <- data.frame(
      Scenario = scenario, 
      Mean = mean(x),
      SD = sd(x),
      Median = median(x),
      Lower1 = quantile(x, prob = 0.01),
      Upper99 = quantile(x, prob = 0.99),
      Lower95 = quantile(x, 1, prob = 0.025),
      Upper95 = quantile(x, 1, prob = 0.975)
      # minim 
    )
    rownames(result) <- NULL
    
    # browser()
    return(result)
  }
  
  
  
  if (tolower(type) == "raw") return(sim.result$spawners)
  
  # Else
  stop("Unknown summary type.")
}
  
  
  
  
  
  
