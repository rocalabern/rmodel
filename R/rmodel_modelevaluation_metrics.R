# roxygen2::roxygenise()

#' @title r.metric.logloss
#' @export
r.metric.logloss <- function(score, target, tol=10^-30, na.rm = TRUE) {
  if (tol>0) score = pmin(1-tol, pmax(tol, score))
  logloss = - mean(target*log(score) + (1-target)*log(1-score), na.rm = na.rm)
  return(logloss);
}

#' @title r.metric.auc.roc
#' @export
r.metric.auc.roc <- function(score, target) {
  pred <- ROCR::prediction(score, target)
  AUC = ROCR::performance(pred, "auc")@y.values[[1]]
  return (AUC)
}

#' @title r.metric.auc.gini
#' @export
r.metric.auc.gini <- function(score, target) {
  pred <- ROCR::prediction(score, target)
  AUC = ROCR::performance(pred, "auc")@y.values[[1]]
  GINI = r.metric.auc_to_gini(AUC)
  return (GINI)
}

#' @title r.metric.auc_to_gini
#' @export
r.metric.auc_to_gini <- function(AUC) {
  GINI = (AUC-0.5)/0.5
  return (GINI)
}

#' @title r.metric.auc.gain
#' @export
r.metric.auc.gain <- function(score, target, npoints = 100, mode = "avg") {
  data = r.gains(score, target, npoints = npoints, mode = mode)
  AUC = r.auc(data$perc, data$gain)
  return (AUC)
}

#' r.metric.R2
#' @export
r.metric.R2 <- function (y, x) {
  SSerr=sum((y-x)^2)
  SStot=sum((mean(y)-y)^2)
  R2 = 1 - SSerr/SStot
  return(R2)
}

#' r.metric.R2Adjusted
#' @export
r.metric.R2Adjusted <- function (y, x, p, n) {
  SSerr=sum((y-x)^2)
  SStot=sum((mean(y)-y)^2)
  R2 = 1 - SSerr/SStot
  R2Adjusted = R2 - (1-R2)*p/(n-p-1)
  return(R2Adjusted)
}
