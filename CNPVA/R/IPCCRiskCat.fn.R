#' IPCC Risk Category
#'
#' @param p numeric value containing the outcome probability.
#'
#' @return A character vector containing the IPCC Risk Category
#'
#' @details 
#' 
#' @references 
#' Mastrandrea, M. D., C. B. Field, T. F. Stocker, O. Edenhofer, K. L. Ebi, D. J. Frame, H. Held, E. Kriegler, K. J. Mach, P. R. Matschoss, G.-K. Plattner, G. W. Yohe, and F. W. Zwiers. 2010. Guidance note for lead authors of the IPCC fifth assessment report on consistent treatment of uncertainties. Intergovernmental Panel on Climate Change (IPCC).
#'
#' @examples
#' 
#' @export
IPCCRiskCat <- function(p) {
  c("Very Unlikely", "Unlikely", "About as likely as not", "Likely", "Very Likely")[findInterval(p, vec=c(0, 0.1, 0.33, 0.66, .90))]
}
