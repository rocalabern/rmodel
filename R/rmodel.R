# roxygen2::roxygenise()

setRefClass("ClassifierMetricsClass",
            fields = c("tp", "fp", "fn", "tn",
                       "accuracy", 
                       "precision", "recall", 
                       "sensitivity", "specificity", 
                       "ScoreF1",
                       "ScoreG",
                       "ScoreBeta",
                       "ScorePhi"))

classifierMetricsConstructor <- getRefClass("ClassifierMetricsClass")

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

#' r.getDF.cleaned
#' @export
r.getDF.cleaned <- function(datosInput,
                            filterNA = TRUE,
                            filterNAN = TRUE,
                            filterInfinite = TRUE,
                            filterConstants = FALSE,
                            verbose = FALSE)
{
  if (verbose) cat("\nCleaning dataframe...")
  datosOutput = datosInput
  if (filterNA) {
    ind = 1:nrow(datosOutput)
    labels = names(datosOutput)
    for (ilabel in labels) {
      column = datosOutput[, ilabel]
      ind = intersect(ind, which(!is.na(column)))
    }
    datosOutput = datosOutput[ind, ]
  }
  if (filterNAN && !filterNA) {
    ind = 1:nrow(datosOutput)
    labels = names(datosOutput)
    for (ilabel in labels) {
      column = datosOutput[, ilabel]
      ind = intersect(ind, which(!is.nan(column)))
    }
    datosOutput = datosOutput[ind, ]   
  }
  if (filterInfinite) {
    ind = 1:nrow(datosOutput)
    labels = names(datosOutput)
    for (ilabel in labels) {
      column = datosOutput[, ilabel]
      ind = intersect(ind, which(!is.infinite(column)))
    }
    datosOutput = datosOutput[ind, ]    
  }
  if (filterConstants) {
    ind = 1:ncol(datosOutput)
    for (icol in 1:ncol(datosOutput)) {
      column = datosOutput[, icol]
      if (var(column)==0) ind = setdiff(ind, icol)
    }
    datosOutput = datosOutput[, ind]
  }
  if (verbose) {
    cat(paste0("\nFil=", nrow(datosInput), " | Col=", ncol(datosInput)))
    cat(paste0("\nFil=", nrow(datosOutput), " | Col=", ncol(datosOutput)))
  }
  return(datosOutput)
}

#' r.corr
#' @export
r.corr <- function(df,
                   methodCorr = "spearman")
{
  methodCorr = tolower(methodCorr)
  if (!(methodCorr %in% c("pearson", "kendall", "spearman"))) {
    methodCorr = "spearman"
  }
  
  if (is.data.frame(df)) {
    dfCleaned = r.getDF.cleaned(df,
                                filterNA = TRUE,
                                filterInfinite = TRUE,
                                filterConstants = TRUE)
    textLabels = names(dfCleaned)
    corrMatrix = cor(dfCleaned, method=methodCorr) 
    if (nrow(corrMatrix) == length(textLabels)) rownames(corrMatrix) = textLabels
    if (ncol(corrMatrix) == length(textLabels)) colnames(corrMatrix) = textLabels    
  } else {
    textLabels = colnames(df)
    corrMatrix = cor(df, method=methodCorr)
    if (nrow(corrMatrix) == length(textLabels)) rownames(corrMatrix) = textLabels
    if (ncol(corrMatrix) == length(textLabels)) colnames(corrMatrix) = textLabels      
  }
  
  return(corrMatrix)
}

#' r.getAdj
#' @export
r.getAdj <- function(adj,
                  quantileCutOff = 0.6,
                  absolute = TRUE,
                  normalize = TRUE,
                  removeAutoCycles = TRUE,
                  undirected = TRUE,
                  useColumnNames = TRUE)
{
  if (is.data.frame(adj)) {
    warning("Input is a dataframe.")
    dfCleaned = r.getDF.cleaned(adj,
                                filterNA = TRUE,
                                filterInfinite = TRUE)
    textLabels = names(dfCleaned)
    adj = as.matrix(dfCleaned)
    if (nrow(adj) == length(textLabels)) rownames(adj) = textLabels
    if (ncol(adj) == length(textLabels)) colnames(adj) = textLabels    
  }
  if (!is.matrix(adj)) {
    warning("Input is not a matrix.")
  }
  if (nrow(adj) != ncol(adj)) {
    warning("Input matrix is not symmetric.")
    n = min(nrow(adj), ncol(adj))
    adj = adj[1:n,1:n]
  }
  if (useColumnNames) rownames(adj) = colnames(adj)
  if (absolute) {
    adj = abs(adj)
  }
  if (normalize) {
    adj = r.normalize(adj, imin=0, imax=1)
  }
  if (0<quantileCutOff && quantileCutOff<1) {
    adj[which(adj<quantile(adj, probs=quantileCutOff))] = 0
  }
  if (removeAutoCycles) {
    for (i in 1:nrow(adj)) adj[i,i] = 0  
  }
  if (undirected) {
    adj = (adj+t(adj))/2
  }
  return(adj)
}

#' r.getAdj.corr
#' @export
r.getAdj.corr <- function(df,
                          methodCorr = "spearman",
                          quantileCutOff = 0.6,
                          absolute = TRUE,
                          normalize = TRUE,
                          removeAutoCycles = TRUE,
                          undirected = TRUE,
                          useColumnNames = TRUE)
{
  corrMatrix = r.corr(df, methodCorr)
  
  adj = r.getAdj(corrMatrix, 
                 quantileCutOff=quantileCutOff, 
                 absolute=absolute, 
                 normalize=normalize, 
                 removeAutoCycles=removeAutoCycles, 
                 undirected=undirected,
                 useColumnNames=useColumnNames)

  return(adj)
}