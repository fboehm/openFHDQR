#' Soft shrinkage operator
#'
#' @param u a numeric vector of length one
#' @param alpha a numeric vector of length one
#' @return a numeric vector of length one, the output of the soft shrinkage operator
#' @export

shrinkR <- function(u, alpha){
  return(sign(u) * max(abs(u) - alpha, 0))
}
