#' Plot Adult Performance Metrics
#'
#' @param runs list object containing individual simulation runs, element names are used for panel labels.
#' @param bins numeric indicating the number of bins to display in the histograms
#' @param return logical indicating
#'
#' @return displays a plot if  `return = FALSE` (default), otherwise return a grob without drawing.
#' @examples
#' # Default  simulation settings
#' runs <- list()
#' runs[['Default']] <- PopSim(GetSettings('default'), n.sim=100)
#' 
#' # Improve Early Ocean, Ocean Harvest
#' settings <- GetSettings('default')
#' settings$hr.ocean <- settings$hr.ocean /2
#' settings$S.osar = 0.3
#' runs[['Improved Early Ocean Survival + Reduced Ocean Harvest']] <- PopSim(settings, n.sim=100)
#' 
#' # Supplementation
#' settings <- GetSettings('default')
#' settings$supplementation <-  1.75 * 10^6
#' settings$suppl.fitness <- 1
#' runs[['Supplementation (1.75 M)']] <- PopSim(settings, n.sim=100)
#' 
#' PlotMetrics(runs)
#' @export
PlotAdultMetrics <- function(runs, bins=45, return=FALSE) {
  
  if (!require(ggplot2)) stop("The 'ggplot2' package is required.")
  if (!require(scales)) stop("The 'scales' package is required.")
  if (!require(grid)) stop("The 'grid' package is required.")
  if (!require(gridExtra)) stop("The 'gridExtra' package is required.")
  
  # browser()
  metrics <- NULL
  for (r in seq_along(runs)) {
    metrics <- rbind(metrics, GetAdults(runs[[r]], type="performance", scenario = names(runs)[r]))
  }
  metrics <-  metrics %>% mutate(Scenario = factor(Scenario, levels=names(runs)))
  

  
  p.MPS <- ggplot(metrics, aes(x=MPS)) + 
    geom_histogram(bins = bins) +
    facet_wrap(~Scenario, ncol=1, scales="free_y") +
    ggtitle("A) Minimum Population Size") +
    ylab("Frequency") +
    xlab("MPS Across all Years")
  
  p.2050 <- ggplot(metrics, aes(x=POP2050)) + 
    geom_histogram(bins = bins) +
    facet_wrap(~Scenario, ncol=1, scales="free_y") +
    ggtitle("B) Population Size in 2050") +
    ylab("Frequency") +
    xlab("Adult Popultion Size")
  
  
  max.val <- max(apply(metrics[c('MPS', "POP2050")], 2, max, na.rm=TRUE), na.rm = TRUE)
  if (max.val >= 1000) {
    p.MPS <- p.MPS + scale_x_continuous(label=comma)
    p.2050 <- p.2050 + scale_x_continuous(label=comma)
  }
  
  if (!return) {
    grid.arrange(
      g <- arrangeGrob(
        p.MPS + theme(axis.title.y = element_blank()),
        p.2050  + theme(axis.title.y = element_blank()), 
        nrow=1, ncol=2,
        as.table = F,
        left = textGrob("Frequency", rot=90, gp=gpar(fontsize=14, fontface="plain")),
        clip=F
      )
    )
  } else {
    g <- arrangeGrob(
      p.MPS + theme(axis.title.y = element_blank()),
      p.2050  + theme(axis.title.y = element_blank()), 
      nrow=1, ncol=2,
      as.table = F,
      left = textGrob("Frequency", rot=90, gp=gpar(fontsize=14, fontface="plain")),
      clip=F
    )
    return(g)
  }
}
