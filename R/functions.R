#' r.abslog
#' @title r.abslog
#' @export
r.abslog <- function(x) {
  ifelse(x==0,0,(x/abs(x))*log10(pmax(1,abs(x), na.rm=TRUE)))
}

#' r.abslog.notvec
#' @export
r.abslog.notvec <- function(x) {  
  if (is.na(x)) return(0)
  else
    if (x>1) return(log10(x))
  else
    if (x< (-1)) return( -log10(-x))
  else return(0)
}