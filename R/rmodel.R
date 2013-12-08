setRefClass("ClassifierMetricsClass",
            fields = c("precision", "recall", "sensitivity", "specificity", "accuracy", "F1Score", "FBetaScore", "tp", "fp", "fn", "tn"))

classifierMetricsConstructor <- getRefClass("ClassifierMetricsClass")

#' r.pca
#' @export
r.pca <- function (x)
{
  require(stats)
  x = r.toColumns(x)
  return (prcomp(x))
}

#' r.nn
#' @export
r.nn <- function (formula, data, hidden=c(2,2), rep=1)
{
  require(neuralnet)
  return (neuralnet(formula, data, hidden=hidden, rep=rep))
}

#' r.reg.logistic
#' @export
r.reg.logistic <- function (y, x, intercept = T, main='Logistic Reg.',  cex=0.3, ...)
{
  x = r.toColumns(x)
  y = r.toColumns(y)
  y = y[,1]
  
  minY = min(y)
  maxY = max(y)
  
  if (maxY>minY) {
    if (intercept) {
      reglog = glm((y-minY)/(maxY-minY)~x, family=binomial(link="logit"), na.action=na.pass)
    } else {
      reglog = glm((y-minY)/(maxY-minY)~0+x, family=binomial(link="logit"), na.action=na.pass)
    }
  } else {
    if (intercept) {
      reglog = glm((y-minY)~x, family=binomial(link="logit"), na.action=na.pass)
    } else {
      reglog = glm((y-minY)~0+x, family=binomial(link="logit"), na.action=na.pass)
    }
  }
  summary(reglog)
  
  if (intercept) {
    xlin = reglog$coefficients[1]+rowSums(t(t(x)*reglog$coefficients[-1]))
  } else {
    xlin = rowSums(t(t(x)*reglog$coefficients))
  }
  
  r.plot(x=xlin, y=y, main=main, cex=cex, ...)
  
  yreg = minY + (maxY-minY)*exp(xlin)/(1+exp(xlin))
  r.plot.add(x=xlin, y=yreg, type='p', cex=cex, icol=3)
  
  print('Model:')
  print('  xlin = reglog$coefficients[1]+rowSums(t(t(x)*reglog$coefficients[-1]))')
  print('  minY = min(y)')
  print('  maxY = max(y)')
  print('  yreg = minY + (maxY-minY)*exp(xlin)/(1+exp(xlin))')
  return(reglog)
}

#' r.dist
#' @export
r.dist <- function (x, bandwidth=0.25, kernel = "epanech", range.x = NULL, ...)
{
  #Kernel density estimation (Parzen-Rosenblatt window method)
  require(KernSmooth)
  
  if(missing(range.x) || is.null(range.x)) {
    minX=min(x)
    maxX=max(x)
    if (minX!=round(minX)) {
      minX=minX-bandwidth
    }
    if (maxX!=round(maxX)) {
      maxX=maxX+bandwidth
    }
    range.x=c(minX,maxX)
  }
  
  mdist = bkde(x=x, kernel=kernel, bandwidth=bandwidth, range.x=range.x)
  #hist(x)
  r.plot(x=mdist[[1]], y=mdist[[2]], ...)
  return(mdist) 
}

#' r.R2
#' @export
r.R2 <- function (y, x) {
  SSerr=sum((y-x)^2)
  SStot=sum((mean(y)-y)^2)
  R2 = 1 - SSerr/SStot
  return(R2)
}

#' r.R2Adjusted
#' @export
r.R2Adjusted <- function (y, x, p, n) {
  SSerr=sum((y-x)^2)
  SStot=sum((mean(y)-y)^2)
  R2 = 1 - SSerr/SStot
  R2Adjusted = R2 - (1-R2)*p/(n-p-1)
  return(R2Adjusted)
}

#' r.validate
#' @export
r.validate <- function(score,fuga, perc=0.2, mode = 0) {
  if (mode==1) {
    indOpt = sort(fuga, decreasing=TRUE, index.return=TRUE)
    fuga = fuga[indOpt$ix]
    score = score[indOpt$ix]
  } else if (mode==-1){
    indOpt = sort(fuga, decreasing=FALSE, index.return=TRUE)
    fuga = fuga[indOpt$ix]
    score = score[indOpt$ix]
  }
  indSorted = sort(score, decreasing=TRUE, index.return=TRUE)
  pos = round(perc*length(fuga))
  return (sum(fuga[indSorted$ix[1:pos]])/sum(fuga)) 
}

#' r.classifierMetrics
#' @export
r.classifierMetrics <- function (
  clustReal = NULL, clustModel = NULL,
  beta = 1,
  selectedClass = NULL,
  ...)
{
  
  cMetrics <- classifierMetricsConstructor$new()
  
  clustReal = as.vector(clustReal)
  clustModel = as.vector(clustModel)
  
  if(missing(selectedClass) || is.null(selectedClass)) {
    selectedClass = max(clustReal)
  }
  
  setRealPositive = which(clustReal==selectedClass)
  setRealNegative = which(clustReal<selectedClass)
  setMdlPositive = which(clustModel==selectedClass)
  setMdlNegative = which(clustModel<selectedClass)
  
  arrayTruePos = intersect(setRealPositive, setMdlPositive)
  arrayFalsePos = intersect(setRealNegative, setMdlPositive)
  arrayFalseNeg = intersect(setRealPositive, setMdlNegative)
  arrayTrueNeg = intersect(setRealNegative, setMdlNegative)
  
  truePos = length(arrayTruePos)
  falsePos = length(arrayFalsePos)
  falseNeg = length(arrayFalseNeg)
  trueNeg = length(arrayTrueNeg)
  
  cMetrics$precision = truePos / (truePos + falsePos)
  cMetrics$recall = truePos / (truePos + falseNeg)
  cMetrics$sensitivity = cMetrics$recall
  cMetrics$specificity = trueNeg / (trueNeg + falsePos)
  cMetrics$accuracy = (truePos + trueNeg) / (truePos + falsePos + trueNeg + falseNeg)
  cMetrics$F1Score = 2 * (cMetrics$precision * cMetrics$recall) / (cMetrics$precision + cMetrics$recall)
  cMetrics$FBetaScore = (1+beta^2) * (cMetrics$precision * cMetrics$recall) / (beta^2*cMetrics$precision + cMetrics$recall)
  
  cMetrics$tp = truePos
  cMetrics$fp = falsePos
  cMetrics$fn = falseNeg
  cMetrics$tn = trueNeg
  
  print(table(clustModel, clustReal))
  
  return(cMetrics)
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

#' r.spline
#' @export
r.spline <- function (
  y,
  x = NULL,
  scale = 1,
  resolution = 100,
  center = F,
  norm = F)
{
  y = r.toColumns(y)
  y = y[,1]
  n <- length(y)
  
  if(missing(x)) {
    x = 1:n
  } else {
    x = r.toColumns(x)
    x = x[,1]   
  }
  
  spl =spline(scale*x, y, n=resolution)
  spl = spl$y
  if (center) {
    spl = spl-mean(spl)
  }
  if (norm) {
    spl = spl/max(abs(spl))
  }
  
  return (spl)
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

#' r.randomData
#' @export
r.randomData <- function () {
  x = rbind(matrix(rnorm(100, sd = 0.3), ncol = 2),             
            matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2)) 
  return (x)
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