#' Title
#'
#' @param runs 
#'
#' @return
#' @export
#'
#' @examples
SummarizeSpawners <- function(runs, start.yr){
  
  # browser()
  #@TODO - fix the years used.  Return 12 year and longer. Geometric mean.
  

  # Minmum Spawners -------------------------------------------------------
  # Retrieve spawner summaries from each simulation run.
  spawners <- NULL
  for (r in seq_along(runs)) {
    spawners <- rbind(spawners, GetSpawners(runs[[r]], type="minimum", min.yr = start.yr, scenario = names(runs)[r]))
  }
  
  min.spawn <-  spawners %>% 
    mutate(
      Scenario = factor(Scenario, levels=names(runs)),
      Type = "Minimum Spawners")  %>%
    select(Type, Scenario, Mean:Upper95)
    # select(Type, Scenario, Mean, SD)  #%>%
    # rename(MinSpawn = Mean, MinSpawnSD = SD)
    

  # Mean Spawners ---------------------------------------------------------
  
  spawners <- NULL
  for (r in seq_along(runs)) {
    spawners <- rbind(spawners, GetSpawners(runs[[r]], type="mean", min.yr = start.yr, scenario = names(runs)[r]))
  }
  
  avg.spawn <-  spawners %>% 
    mutate(
      Scenario = factor(Scenario, levels=names(runs)),
      Type = "Average Spawners"
    )  %>%
    select(Type, Scenario, Mean:Upper95)
  
  return(
    list(
      "All"  = rbind(min.spawn, avg.spawn) %>% mutate(Type = factor(Type, levels = c("Minimum Spawners", "Average Spawners"))),
      "Minimum" = min.spawn,
      "Average" = avg.spawn
    )
  )
}
