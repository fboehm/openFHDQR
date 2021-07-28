#' Perform proximal mapping of rho tau
#'
#' @param xi a single number
#' @param alpha a number
#' @param tau a number between 0 and 1, the quantile of interest
#' @return output of the proximal mapping of rho tau

proxR <- function(xi, alpha, tau){
  return(xi - max((tau - 1) / alpha, min(xi, tau / alpha)))
}
