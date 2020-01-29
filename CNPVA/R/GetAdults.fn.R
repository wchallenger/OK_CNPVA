#' Get Adults Results from Simulation
#'
#' @param sim.result 
#' @param type 
#' @param scenario 
#'
#' @return
#' @export
#'
#' @examples
GetAdults <- function(sim.result, type = "performance", scenario=NULL) {
  

  
  sim.result <- sim.result[['adult.pop']]   # Year x Age X Sim #
  
  # If 'raw' return simulation output as is
  if (tolower(type) == "raw") return(sim.result)
  
  # Else compute total number of adults per year which will be used
  # in other calculations
  adults.tot <- apply(sim.result, c(1,3), mean, na.rm=TRUE)     # Average by Year and Sim
  
  # browser()
  
  # Else if 'totals' are requested return total yearly adults for each sim
  if (tolower(type) == "totals") return(adults.tot)
  
  # Else return performance metrics:
  #  - Minimum Population Size  - Population size only computed for complete
  #  - 2050 Population size  - 2050 population size is hard coded.
  if (tolower(type) == "performance") {
    # browser()
    result <- data.frame(
      MPS = apply(adults.tot, 2, min, na.rm=TRUE),
      POP2050 = adults.tot['2050', ],
      SimIteration = as.numeric(str_extract(colnames(adults.tot), "[:digit:]+$"))
    )
    rownames(result) <- NULL
    if (!is.null(scenario)) {
      result$Scenario <-  scenario
      result <- result[c(4,1:3)]
    }
    return(result)
  } 
  
  # Else if summary requested return the simulation mean and quantiles of the total
  if (type == "population size") {
    result <- data.frame(
      Year = as.numeric(str_extract(rownames(adults.tot), "[:digit:]{4}$")),
      Mean = apply(adults.tot, 1, mean, na.rm=TRUE),
      Median = apply(adults.tot, 1, median, na.rm=TRUE),
      Lower = apply(adults.tot, 1, quantile, prob = 0.025, na.rm=TRUE),
      Upper = apply(adults.tot, 1, quantile, prob = 0.975, na.rm=TRUE)
    )
    if (!is.null(scenario)) {
      result$Scenario <-  scenario
      result <- result[c(6,1:5)]
    }
    return(result)
  }
  
  # Else unrecognized output type requested
  stop("Output type unknown.")
}




