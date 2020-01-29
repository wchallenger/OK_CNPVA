#' Get Simulation Spawner Recruits
#'
#' @param sim.result 
#' @param type 
#'
#' @return
#' @export
#'
#' @examples
GetSpawnerRecruits <- function(sim.result, type="summary", scenario = NULL) {
  
  if (tolower(type) == "summary") {
    result <- data.frame(
      Year = as.numeric(str_extract(dimnames(sim.result$spawner.recruits)[[1]], "[:digit:]{4}$")),
      Mean = apply(sim.result$spawner.recruits, 1, mean),
      Median = apply(sim.result$spawner.recruits, 1, median),
      Lower = apply(sim.result$spawner.recruits, 1, quantile, prob = 0.025),
      Upper = apply(sim.result$spawner.recruits, 1, quantile, prob = 0.975)
    )
    if (!is.null(scenario)) {
      result$Scenario <-  scenario
      result <- result[c(6,1:5)]
    }
    return(result)
  }
  
  if (tolower(type) == "raw") return(sim.result$spawner.recruits)
  
  # Else
  stop("Unknown summary type.")
}


