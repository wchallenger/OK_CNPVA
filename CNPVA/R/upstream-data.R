#' Upstream survival (Interdam Loss)
#'
#' Data from upstream PIT 
#'
#' @docType data
#'
#' @usage data(upstream)
#'
#' @format A data frame containing yearly upstream survival rates.
#'
#' @keywords datasets
#'
#' @references Moore et al. (2013) Genetics 195:1077-1086
#' (\href{http://www.fpc.org/survival/Survival_by_ReachQuery.html}{Fish Passage Center})
#'
#' @source \href{https://phenome.jax.org/projects/Moore1b}{QTL Archive}
#'
#' @examples
#' data(upstream)
#' ggplot(surv, aes(x=Year, y=`IDL.(Upstream.survival)`)) + 
#'   geom_point() +
#'   scale_y_continuous(trans=logit_trans(), breaks=c(seq(0.2, 0.8, by=0.2), 0.95, 0.99))  + 
#'   geom_smooth(method="lm", formula = y~poly(x,1), se = TRUE) +
#'   ylab("Upstream Survival")
"upstream"