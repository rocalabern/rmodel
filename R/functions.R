# roxygen2::roxygenise()

#' r.abslog
#' @title r.abslog
#' @export
r.abslog <- function(x) {
  sign(x)*log10(1+abs(x))
}