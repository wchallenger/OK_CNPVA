#' Plot Simulated Spawners
#'
#' @param runs named list object containing simulation runs to plot.
#' @param return 
#'
#' @return
#' @export
#'
#' @examples
PlotSpawners <- function(runs, return=FALSE){
  
  # Retrieve spawner summaries from each simulation run.
  spawners <- NULL
  for (r in seq_along(runs)) {
    spawners <- rbind(spawners, GetSpawners(runs[[r]], type="summary", scenario = names(runs)[r]))
  }
  # browser()
  spawners <-  spawners %>% 
    mutate(Scenario = factor(Scenario, levels=names(runs))) %>%
    filter(Year > min(spawners$Year) + 4)
  
  p <- ggplot(spawners, aes(x=Year, y=Mean)) + geom_line() + 
    geom_ribbon(data = spawners, aes(ymin=Lower95,ymax=Upper95),alpha=0.3) + 
    facet_wrap(~Scenario) +
    scale_y_continuous(label=comma) +
    # scale_y_log10() + 
    # geom_hline(yintercept = 1)
    ylab("Number of Spawners")
  
  return(p)
  # if (!return) {
  #   print(p)
  # } else {
  #   return(p)
  # }

}