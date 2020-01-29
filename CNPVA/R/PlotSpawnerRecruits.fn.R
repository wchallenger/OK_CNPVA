#' Plot Simulated Spawner Recruits
#'
#' @param runs named list object containing simulation runs to plot.
#' @param return 
#'
#' @return
#' @export
#'
#' @examples
PlotSpawnerRecruits <- function(runs, return=FALSE){
  
  # Retrieve spawner summaries from each simulation run.
  recruits <- NULL
  for (r in seq_along(runs)) {
    recruits <- rbind(recruits, GetSpawnerRecruits(runs[[r]], type="summary", scenario = names(runs)[r]))
  }
  recruits <-  recruits %>% mutate(Scenario = factor(Scenario, levels=names(runs)))
  
  p <- ggplot(recruits, aes(x=Year, y=Mean)) + geom_line() + 
    geom_ribbon(data = recruits, aes(ymin=Lower,ymax=Upper),alpha=0.3) + 
    facet_wrap(~Scenario) +
    scale_y_continuous(label=comma) +
    # scale_y_log10() + 
    # geom_hline(yintercept = 1)
    ylab("Number of Spawner Recruits")
  
  if (!return) {
    print(p)
  } else {
    return(p)
  }

}