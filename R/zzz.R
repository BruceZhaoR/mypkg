
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to the package, developing by Pingjia Technology. \nIf you have any question, please email 'zhaowei@chinaubi.com'.")
}

#' @useDynLib mypkg, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL
