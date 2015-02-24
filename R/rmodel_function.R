#' r.abslog
#' @export
r.abslog <- function(x) {  
  if (is.na(x)) return(0)
  else
    if (x>1) return(log10(x))
  else
    if (x< (-1)) return( -log10(-x))
  else return(0)
}