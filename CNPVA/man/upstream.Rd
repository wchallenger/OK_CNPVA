% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/upstream-data.R
\docType{data}
\name{upstream}
\alias{upstream}
\title{Upstream survival (Interdam Loss)}
\format{A data frame containing yearly upstream survival rates.}
\source{
\href{https://phenome.jax.org/projects/Moore1b}{QTL Archive}
}
\usage{
data(upstream)
}
\description{
Data from upstream PIT
}
\examples{
data(upstream)
ggplot(surv, aes(x=Year, y=`IDL.(Upstream.survival)`)) + 
  geom_point() +
  scale_y_continuous(trans=logit_trans(), breaks=c(seq(0.2, 0.8, by=0.2), 0.95, 0.99))  + 
  geom_smooth(method="lm", formula = y~poly(x,1), se = TRUE) +
  ylab("Upstream Survival")
}
\references{
Moore et al. (2013) Genetics 195:1077-1086
(\href{http://www.fpc.org/survival/Survival_by_ReachQuery.html}{Fish Passage Center})
}
\keyword{datasets}
