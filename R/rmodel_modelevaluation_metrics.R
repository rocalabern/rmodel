#' @title r.metric.logloss
#' @export
r.metric.logloss <- function(score, target, tol=10^-30)
{
  eps = 1e-15;
  nr <- nrow(target)
  if (tol>0) target = pmin(1-tol, pmax(tol, target))
  target = matrix(sapply( target, function(x) max(eps,x)), nrow = nr)      
  target = matrix(sapply( target, function(x) min(1-eps,x)), nrow = nr)
  ll = sum(score*log(target) + (1-score)*log(1-target))
  ll = ll * -1/(nrow(score))      
  return(ll);
}

#' @title r.metric.auc.roc
#' @export
r.metric.auc.roc <- function(score, target) {
  pred <- ROCR::prediction(score, target)
  AUC = ROCR::performance(pred, "auc")@y.values[[1]]
  return (AUC)
}

#' @title r.metric.auc.gain
#' @export
r.metric.gain.auc <- function(score, target, npoints = 100, mode = "avg") {
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
