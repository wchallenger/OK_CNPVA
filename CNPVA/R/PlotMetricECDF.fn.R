#' Plot Simulated Spawners
#'
#' @param runs named list object containing simulation runs to plot.
#' @param return 
#'
#' @return
#' @export
#'
#' @examples
PlotMetricECDF <- function(runs, type = "MPS", panel.widths = c(1, 1.35), return.plot=FALSE){
  
  if (!require(ggplot2)) stop("The 'ggplot2' package is required.")
  if (!require(scales)) stop("The 'scales' package is required.")
  if (!require(grid)) stop("The 'grid' package is required.")
  if (!require(gridExtra)) stop("The 'gridExtra' package is required.")
  
  # Extract performance metrics from simulation runs
  metrics <- NULL
  for (r in seq_along(runs)) {
    metrics <- rbind(metrics, GetAdults(runs[[r]], type="performance", scenario = names(runs)[r]))
  }
  metrics <-  metrics %>% mutate(Scenario = factor(Scenario, levels=names(runs)))
  
  # browser()
  max.val <- apply(metrics[c('MPS', "POP2050")], 2, max, na.rm=TRUE)
  
  title.txt <- c(
    MPS = "Minimum Population Size (MPS)",
    POP2050 = "Population Size in 2050"
  )
  # browser()
  
  # Empirical CDF for MPS metric
  if (tolower(type) %in% c("mps", "both")) {
    
    p.MPS <- ggplot(metrics, aes(x=MPS))  + 
      stat_ecdf(aes(linetype = Scenario, color = Scenario)) +
      coord_cartesian(xlim = c(0, max.val['MPS'])) +
      ggtitle(title.txt['MPS']) +
      ylab("Probability") +
      xlab("MPS Across all Years")
    
    if (max.val['MPS'] >= 1000)  
      p.MPS <- p.MPS + scale_x_continuous(label=comma)
    
    
  }
  # browser()
  # Empirical CDF for the POP2050 metric
  if (tolower(type) %in% c("pop2050", "both")) {
    
    p.2050 <- ggplot(metrics, aes(x=POP2050))  + 
      stat_ecdf(aes(linetype = Scenario, color = Scenario)) +
      coord_cartesian(xlim = c(0, max.val['POP2050'])) +
      ggtitle(title.txt['POP2050']) +
      ylab("Probability") +
      xlab("Adult Popultion Size")
    
    if (max.val['POP2050'] >= 1000)  
      p.2050 <- p.2050 + scale_x_continuous(label=comma)
   
  }
  
  
  # Else both figures were requested. 
  if (tolower(type) == "both") {
    # browser()
    if (!return.plot) {
      
      grid.arrange(
        p.MPS + theme(axis.title.y = element_blank(), legend.position = "none") + ggtitle(paste("A)", title.txt['MPS'])),
        p.2050  + theme(axis.title.y = element_blank()) + ggtitle(paste("B)", title.txt['POP2050'])), 
        nrow=1, ncol=2,
        as.table = F,
        left = textGrob("Probability", rot=90, gp=gpar(fontsize=14, fontface="plain")),
        widths = panel.widths,
        clip=F
      )
    } else {
     
      return(
       g <-  arrangeGrob(
          p.MPS + theme(axis.title.y = element_blank(), legend.position = "none") + ggtitle(paste("A)", title.txt['MPS'])),
          p.2050  + theme(axis.title.y = element_blank()) + ggtitle(paste("B)", title.txt['POP2050'])), 
          nrow=1, ncol=2,
          as.table = F,
          left = textGrob("Probability", rot=90, gp=gpar(fontsize=14, fontface="plain")),
          widths = panel.widths,
          clip=F
        )
      )
    }
  } else {
    if (tolower(type) == "mps") {
      if (!return.plot) {
        print(p.MPS)
      } else {
        return(p.MPS)
      }
    } else if (tolower(type) == "pop2050") {
      if (!return.plot) {
        print(p.2050)
      } else {
        return(p.2050)
      }
    } else {
      stop("Unkown output type.")
    }
  }
  
}