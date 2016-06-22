#' r.abslog
#' @title r.abslog
#' @export
r.abslog <- function(x) {
  sign(x)*log10(pmax(1,abs(x), na.rm=TRUE))
}