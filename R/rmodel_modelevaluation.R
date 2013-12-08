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

#' r.gain
#' @param mode 1 Optimista, -1 Pesimista, 0 Sense Modificar, 2 Promig, 3 Random
#' @export
r.gain <- function(score,fuga, perc=0.2, mode = 0) {
  pos = round(perc*length(fuga))
  if (mode==2) {
    ind = sort(fuga, decreasing=TRUE, index.return=TRUE)
    fuga = fuga[ind$ix]
    score = score[ind$ix]   
    indSorted = sort(score, decreasing=TRUE, index.return=TRUE)
    p20_Opt = (sum(fuga[indSorted$ix[1:pos]])/sum(fuga))   
    ind = sort(fuga, decreasing=FALSE, index.return=TRUE)
    fuga = fuga[ind$ix]
    score = score[ind$ix]   
    indSorted = sort(score, decreasing=TRUE, index.return=TRUE)
    p20_Pes = (sum(fuga[indSorted$ix[1:pos]])/sum(fuga))     
    return (0.5*p20_Opt+0.5*p20_Pes) 
  } else {
    if (mode==1) {
      ind = sort(fuga, decreasing=TRUE, index.return=TRUE)
      fuga = fuga[ind$ix]
      score = score[ind$ix]
    } else if (mode==-1) {
      ind = sort(fuga, decreasing=FALSE, index.return=TRUE)
      fuga = fuga[ind$ix]
      score = score[ind$ix]
    } else if (mode==3) {
      ind = sample(1:length(fuga))
      fuga = fuga[ind]
      score = score[ind]      
    }
    indSorted = sort(score, decreasing=TRUE, index.return=TRUE)
    return (sum(fuga[indSorted$ix[1:pos]])/sum(fuga)) 
  }
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