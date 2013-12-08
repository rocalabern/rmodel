#' r.toColumns
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

#' r.toFormula
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

#' r.addYear
#' @export
r.addYear <- function(d, i=1) {
  tmp <- as.POSIXlt(d)
  tmp$year <- tmp$year+i
  return (as.Date(tmp))
}

#' r.min
#' @export
r.min <- function (var) {
  ind = intersect(which(!is.na(var)), which(!is.infinite(var)))
  if (length(ind)==0) return(NA)
  else return(min(var[ind]))
}

#' r.max
#' @export
r.max <- function (var) {
  ind = intersect(which(!is.na(var)), which(!is.infinite(var)))
  if (length(ind)==0) return(NA)
  else return(max(var[ind]))
}

#' r.mean
#' @export
r.mean <- function (var) {
  ind = intersect(which(!is.na(var)), which(!is.infinite(var)))
  if (length(ind)==0) return(NA)
  else return(mean(var[ind]))
}

#' r.missings
#' @export
r.missings <- function (var) {
  return(length(which(is.na(var))))
}

#' r.normalize
#' @export
r.normalize <- function (x, ind=NULL, imin=0, imax=0)
{
  if (!is.null(ind)) {
    x = x[ind]
  }
  xmin = min(x)
  xmax = max(x)
  
  if (xmax==xmin) {
    return (rep(imin, length(ind)))
  } else {
    return (imin+(imax-imin)*((x-xmin)/(xmax-xmin)))
  }
}

#' r.rescale.col
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

#' r.zeros
#' @export
r.zeros <- function (nrow = 1, ncol = 1)
{ 
  return (matrix(data=0, nrow=nrow, ncol=ncol))
}

#' r.arrayzeros
#' @export
r.arrayzeros <- function (nrow = 1)
{ 
  m = (matrix(data=0, nrow=nrow, ncol=1))
  return(m[,1])
}

#' r.randomData
#' @export
r.randomData <- function () {
  x = rbind(matrix(rnorm(100, sd = 0.3), ncol = 2),             
            matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2)) 
  return (x)
}

#' r.toClusterGroups
#' @export
r.toClusterGroups <- function (cl)
{ 
  if (class(cl) == 'kmeans') cl = cl$cluster
  return(cl)
}

#' r.tree.toClusters
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