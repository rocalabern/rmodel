# roxygen2::roxygenise()

#' @title r.toColumns
#' @export
r.toColumns <- function (
  y,
  autoT = T, trans = F)
{ 
  if (class(y)=='data.frame') {
    y = data.matrix(y)
  } else {
    if(class(y) != 'matrix') {
      y = cbind(y) # y[,1] array
    }
  }
  if ((dim(y)[1]==1 && autoT) || (trans)) {
    y = t(y)
  }
  return(y)
}

#' @title r.toFormula
#' @export
r.toFormula <- function (x, txtMatrix, txtY = NULL)
{ 
  xnam = paste(paste(txtMatrix, '[,', seo=''), 1:dim(x)[2],"]", sep="")
  xnam = paste(xnam, collapse= "+")
  
  if(missing(txtY) || is.null(txtY)) {
    fmla = xnam
  } else {
    fmla <- as.formula(paste(txtY, '~', xnam))
  } 
  return(fmla)
}

#' @title r.addYear
#' @export
r.addYear <- function(d, i=1) {
  tmp <- as.POSIXlt(d)
  tmp$year <- tmp$year+i
  return (as.Date(tmp))
}

#' @title r.min
#' @export
r.min <- function (var) {
  ind = intersect(which(!is.na(var)), which(!is.infinite(var)))
  if (length(ind)==0) return(NA)
  else return(min(var[ind]))
}

#' @title r.max
#' @export
r.max <- function (var) {
  ind = intersect(which(!is.na(var)), which(!is.infinite(var)))
  if (length(ind)==0) return(NA)
  else return(max(var[ind]))
}

#' @title r.mean
#' @export
r.mean <- function (var) {
  ind = intersect(which(!is.na(var)), which(!is.infinite(var)))
  if (length(ind)==0) return(NA)
  else return(mean(var[ind]))
}

#' @title r.rollmean
#' @export
r.rollmean <- function (var, outterExtrems = TRUE) {
  if (outterExtrems) return(
    c(
      var[1] - abs(diff(head(var,2))),
      0.5*var[-length(var)]+0.5*var[-1],
      var[length(var)] + abs(diff(tail(var,2)))
      )
    )
  else return( 0.5*var[-length(var)]+0.5*var[-1] )
}

#' @title r.missings
#' @export
r.missings <- function (array) {
  array = 0.5*array[-length(array)]+0.5*array[-1]
  return(length(which(is.na(var))))
}

#' @title r.normalize
#' @export
r.normalize <- function (x, ind=NULL, imin=0, imax=1)
{
  if (!is.null(ind)) {
    x = x[ind]
  }
  extrems = range(x)
  
  if (extrems[2]==extrems[1]) {
    return (rep(imin, length(ind)))
  } else {
    return (imin+(imax-imin)*((x-extrems[1])/(extrems[2]-extrems[1])))
  }
}

#' @title r.rescale.col
#' @export
r.rescale.col <- function (x)
{
  if (class(x)=='data.frame') {
    xres = data.matrix(x)
  } else {
    xres = x
  }
  
  if (class(x)!='matrix') {
    xres = xres-min(xres)
    m = max(xres)
    if (m>0) {
      xres = xres/m
    }
  } else {
    for (i in 1:dim(xres)[2]) {
      xres[,i] = xres[,i]-min(xres[,i])
      m = max(xres[,i])
      if (m>0) {
        xres[,i] = xres[,i]/m
      }
    }   
  }
  
  return(xres)
}

#' @title r.zeros
#' @export
r.zeros <- function (nrow = 1, ncol = 1)
{ 
  return (matrix(data=0, nrow=nrow, ncol=ncol))
}

#' @title r.arrayzeros
#' @export
r.arrayzeros <- function (nrow = 1)
{ 
  m = (matrix(data=0, nrow=nrow, ncol=1))
  return(m[,1])
}

#' @title r.randomData
#' @export
r.randomData <- function () {
  x = rbind(matrix(rnorm(100, sd = 0.3), ncol = 2),             
            matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2)) 
  return (x)
}

#' @title r.toClusterGroups
#' @export
r.toClusterGroups <- function (cl)
{ 
  if (class(cl) == 'kmeans') cl = cl$cluster
  return(cl)
}

#' @title r.tree.toClusters
#' @export
r.tree.toClusters <- function (arbre, clustReal)
{
  clustReal = r.toClusterGroups(clustReal)
  
  taula = table(predict(arbre, type = "node"), clustReal)
  n = dim(taula)[1]
  m = dim(taula)[2]
  rowMean = r.arrayzeros(n)
  for (k in 1:n) {
    rowMean[k] = 1
    for (c in 2:m) {
      if (taula[k,rowMean[k]]<=taula[k,c]) {
        rowMean[k] =  c
      }
    }
  }
  index = as.numeric(attributes(taula)$dimnames[[1]])
  minNode = min(predict(arbre, type = "node"))
  maxNode = max(predict(arbre, type = "node"))
  hashTable = r.arrayzeros(maxNode-minNode+1)
  hashTable[index-minNode+1] = rowMean
  clustArbre = hashTable[predict(arbre, type = "node")-minNode+1]
  
  return(clustArbre)
}