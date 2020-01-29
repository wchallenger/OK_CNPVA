#' Title
#'
#' @param runs 
#'
#' @return
#' @export
#'
#' @examples
SummarizeMetrics <- function(runs) {
  
  if (!require(tidyverse)) stop("The 'tidyverse' package is required.")

  metrics <- NULL
  for (r in seq_along(runs)) {
    metrics <- rbind(metrics, GetAdults(runs[[r]], type="performance", scenario = names(runs)[r]))
  }
  
  # browser()
  result <-  metrics %>% mutate(Scenario = factor(Scenario, levels=names(runs)))  %>%
    gather(key = Type, value = Value, MPS, POP2050) %>%
    group_by(Type, Scenario) %>%
    summarize(
      Mean = mean(Value),
      SD   = sd(Value),
      Lower1 = quantile(Value, probs = 0.01), 
      Upper99 = quantile(Value, probs = 0.99), 
      Lower95 = quantile(Value, probs = 0.025),
      Upper95 = quantile(Value, probs = 0.975)
    ) %>% ungroup() %>%
    mutate(
      Type = factor(Type, levels = c("MPS", "POP2050"), labels = c("Minimum Population Size", "Population in 2050"))
    )
  
  return(
    list(
      All = result,
      MPS = result %>% filter(Type == "Minimum Population Size") %>% select(-Type),
      POP2050 = result %>% filter(Type == "Population in 2050") %>% select(-Type)
    )
  )
}


