#' @title r.auc
#' @export
r.auc <- function(x,y) {
  id <- order(x)
  return (sum(diff(x[id])*zoo::rollmean(y[id],2)))
}

#' @title r.auc.roc
#' @export
r.auc.roc <- function(score, target) {
  pred <- ROCR::prediction(score, target)
  AUC = ROCR::performance(pred, "auc")@y.values[[1]]
  return (AUC)
}

#' @title r.auc.gain
#' @export
r.auc.gain <- function(score, target, npoints = 100, mode = "avg") {
  data = r.gains(score, target, npoints = npoints, mode = mode)
  AUC = r.auc(data$perc, data$gain)
  return (AUC)
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

#' r.gain
#' @param mode 
#' \cr "def" = As it is
#' \cr "rnd" = Random
#' \cr "pos" = Optimist/Positive (Maximum)
#' \cr "neg" = Pessimist/Negative (Minimum)
#' \cr "avg" = Average of Optimist and Pessimist
#' @export
r.gain <- function(score, target, perc=0.2, mode="def") {
  pos = round(perc*length(target))
  if (mode=="rnd") {
    # Random
    ind <- sample(1:length(target))
    target <- target[ind]
    score <- score[ind]
    
    targetOrd <- target[order(-score)]
    cummmSums <- cumsum(targetOrd)
    return (cummmSums[pos]/tail(cummmSums,1))
  } else if (mode=="pos") {
    # Optimist/Positive (Maximum)
    targetOrd <- target[order(-score, -target)]
    cummmSums <- cumsum(targetOrd)
    return (cummmSums[pos]/tail(cummmSums,1))
  } else if (mode=="neg") {
    # Pessimist/Negative (Minimum)
    targetOrd <- target[order(-score, target)]
    cummmSums <- cumsum(targetOrd)
    return (cummmSums[pos]/tail(cummmSums,1))
  } else if (mode=="avg") {
    # Average of Optimist and Pessimist
    targetOrd <- target[order(-score, -target)]
    cummmSums <- cumsum(targetOrd)
    percPos = cummmSums[pos]/tail(cummmSums,1)
    targetOrd <- target[order(-score, target)]
    cummmSums <- cumsum(targetOrd)
    percNeg = cummmSums[pos]/tail(cummmSums,1)    
    return (0.5*percPos + 0.5*percNeg) 
  } else {
    # As it is ("def")
    targetOrd <- target[order(-score)]
    cummmSums <- cumsum(targetOrd)
    return (cummmSums[pos]/tail(cummmSums,1))
  }
}

#' r.gains
#' @param mode 
#' \cr "def" = As it is
#' \cr "rnd" = Random
#' \cr "pos" = Optimist/Positive (Maximum)
#' \cr "neg" = Pessimist/Negative (Minimum)
#' \cr "avg" = Average of Optimist and Pessimist
#' @export
r.gains <- function(score, target, npoints=20, mode="def") {
  if (mode=="rnd") {
    # Random
    ind <- sample(1:length(target))
    target <- target[ind]
    score <- score[ind]
    
    targetOrd <- target[order(-score)]
    cummmSums <- cumsum(targetOrd)[round(seq(1,length(targetOrd),length.out=npoints+1))]
    cummmSums[1] <- 0
    return (data.frame(
      perc = seq(0,1,1/npoints), 
      gain = cummmSums/tail(cummmSums,1)
      ))
  } else if (mode=="pos") {
    # Optimist/Positive (Maximum)
    targetOrd <- target[order(-score, -target)]
    cummmSums <- cumsum(targetOrd)[round(seq(1,length(targetOrd),length.out=npoints+1))]
    cummmSums[1] <- 0
    return (data.frame(
      perc = seq(0,1,1/npoints), 
      gain = cummmSums/tail(cummmSums,1)
    ))
  } else if (mode=="neg") {
    # Pessimist/Negative (Minimum)
    targetOrd <- target[order(-score, target)]
    cummmSums <- cumsum(targetOrd)[round(seq(1,length(targetOrd),length.out=npoints+1))]
    cummmSums[1] <- 0
    return (data.frame(
      perc = seq(0,1,1/npoints), 
      gain = cummmSums/tail(cummmSums,1)
    ))
  } else if (mode=="avg") {
    # Average of Optimist and Pessimist
    targetOrd <- target[order(-score, -target)]
    cummmSums <- cumsum(targetOrd)[round(seq(1,length(targetOrd),length.out=npoints+1))]
    cummmSums[1] <- 0
    percPos <- cummmSums/tail(cummmSums,1)
    targetOrd <- target[order(-score, target)]
    cummmSums <- cumsum(targetOrd)[round(seq(1,length(targetOrd),length.out=npoints+1))]
    cummmSums[1] <- 0
    percNeg <- cummmSums/tail(cummmSums,1)    
    return (data.frame(
      perc = seq(0,1,1/npoints), 
      gain = 0.5*percPos + 0.5*percNeg
    ))
  } else {
    # As it is ("def")
    targetOrd <- target[order(-score)]
    cummmSums <- cumsum(targetOrd)[round(seq(1,length(targetOrd),length.out=npoints+1))]
    cummmSums[1] <- 0
    return (data.frame(
      perc = seq(0,1,1/npoints), 
      gain = cummmSums/tail(cummmSums,1)
    ))
  }
}

#' @title r.performance.metrics
#' @export
r.performance.metrics <- function(
  score,
  target,
  threshold = 0.5,
  beta = 1,
  round = 5,
  big.mark=",", 
  decimal.mark = ".",
  scientific = FALSE,
  printConfMat = TRUE,
  printF1 = TRUE,
  printMetrics = TRUE
) {
  predict = ifelse(score>threshold, 1, 0)
  t = table(predict, target)
  
  dfConfMat = data.frame(ACTUAL_0=numeric(4), ACTUAL_1=numeric(4), PREDICTED=numeric(4), PRECISION=numeric(4))
  rownames(dfConfMat) = c("PREDICTED_0", "PREDICTED_1", "ACTUAL", "RECALL")
  dfConfMat[1:2,1:2] = t
  dfConfMat[1,3] = dfConfMat[1,1] + dfConfMat[1,2]
  dfConfMat[2,3] = dfConfMat[2,1] + dfConfMat[2,2]
  dfConfMat[3,1] = dfConfMat[1,1] + dfConfMat[2,1]
  dfConfMat[3,2] = dfConfMat[1,2] + dfConfMat[2,2]
  dfConfMat[3,3] = sum(t)
  dfConfMat[1,4] = (dfConfMat[1,1] / dfConfMat[1,3])
  dfConfMat[2,4] = (dfConfMat[2,2] / dfConfMat[2,3])
  dfConfMat[4,1] = (dfConfMat[1,1] / dfConfMat[3,1])
  dfConfMat[4,2] = (dfConfMat[2,2] / dfConfMat[3,2])
  dfConfMat[3,4] = (dfConfMat[1,4] + dfConfMat[2,4]) / 2
  dfConfMat[4,3] = (dfConfMat[4,1] + dfConfMat[4,2]) / 2
  dfConfMat[4,4] = ((dfConfMat[1,1] + dfConfMat[2,2]) / dfConfMat[3,3])
  
  dfF1 = data.frame(F1=numeric(3), phi=numeric(3))
  dfF1[1,1] = 2*dfConfMat[1,4]*dfConfMat[4,1]/(dfConfMat[1,4]+dfConfMat[4,1])
  dfF1[2,1] = 2*dfConfMat[2,4]*dfConfMat[4,2]/(dfConfMat[2,4]+dfConfMat[4,2])
  dfF1[3,1] = (dfF1[1,1] + dfF1[2,1])/2
  dfF1[1,2] = ((dfConfMat[1,1]*dfConfMat[2,2])-(dfConfMat[1,2]*dfConfMat[2,1]))/sqrt((dfConfMat[1,1]+dfConfMat[1,2])*(dfConfMat[1,1]+dfConfMat[2,1])*(dfConfMat[2,2]+dfConfMat[1,2])*(dfConfMat[2,2]+dfConfMat[2,1]))
  dfF1[2,2] = dfF1[1,2]
  dfF1[3,2] = (dfF1[1,2] + dfF1[2,2])/2
  
  metrics <- classifierMetricsConstructor$new()
  tp = dfConfMat[2,2]
  fp = dfConfMat[2,1]
  fn = dfConfMat[1,2]
  tn = dfConfMat[1,1]
  metrics$tp = tp
  metrics$fp = fp
  metrics$fn = fn
  metrics$tn = tn
  metrics$accuracy = (tp + tn) / (tp + fp + tn + fn)
  metrics$precision = tp / (tp + fp)
  metrics$recall = tp / (tp + fn)
  metrics$sensitivity = metrics$recall
  metrics$specificity = tn / (tn + fp)
  metrics$ScoreF1 = 2 * (metrics$precision * metrics$recall) / (metrics$precision + metrics$recall)
  metrics$ScoreG = sqrt(metrics$precision*metrics$recall)
  metrics$ScoreBeta = (1+beta^2) * (metrics$precision * metrics$recall) / (beta^2*metrics$precision + metrics$recall)
  metrics$ScorePhi = (tp*tn - fp*fn) / sqrt((tp+fp)*(tp+fn)*(tn+fp)*(tn+fn))
  
  performanceMetrics = list(dfConfMat=dfConfMat, dfF1=dfF1, metrics = metrics)
  
  printPerformance(
    performanceMetrics,
    printConfMat = printConfMat, printF1 = printF1, printMetrics = printMetrics,
    round = round, big.mark = big.mark, decimal.mark = decimal.mark, scientific = scientific)
  
  invisible(performanceMetrics)
}

#' @title r.optimize.threshold
#' @export
r.optimize.threshold <- function (
  score,
  target,
  f_obj = function(threshold) {
    predict = numeric(length(score))
    predict[score>threshold] = 1
    t <- table(predict, target)
    if (nrow(t)==1)
      f1 = 0
    else
      f1 = 0.5*(2*t[1,1]/(2*t[1,1]+t[1,2]+t[2,1]))+0.5*(2*t[2,2]/(2*t[2,2]+t[1,2]+t[2,1]))
    return (-f1)
  },
  x0 = quantile(score, probs=0.5),
  xtol_rel = 1e-8,
  maxtime = 60,
  algorithm = "NLOPT_LN_COBYLA"
) {
  if (length(score)!=length(target)) stop("Arrays score and target with different length.")
  opt <- nloptr::nloptr(
    x0,
    f_obj,
    lb = 0,
    ub = 1,
    opts = list(
      "algorithm"=algorithm,
      "xtol_rel"=xtol_rel,
      "maxtime"=maxtime)
  )
  message(paste0("seed threshold = ", opt$x0))
  message(paste0("threshold = ", opt$solution))
  if (missing(f_obj)) {
    message(paste0("init F1 = ", -f_obj(x0)))
    message(paste0("opt  F1 = ", -f_obj(opt$solution)))
  } else {
    message(paste0("init metric = ", f_obj(x0)))
    message(paste0("opt  metric = ", f_obj(opt$solution)))
  }
  return (opt)
}