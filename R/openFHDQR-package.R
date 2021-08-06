
## usethis namespace: start
#' @importFrom Rcpp sourceCpp
## usethis namespace: end
NULL

## usethis namespace: start
#' @useDynLib openFHDQR, .registration = TRUE
## usethis namespace: end
NULL


.onUnload <- function (libpath) {
  library.dynam.unload("openFHDQR", libpath)
}
