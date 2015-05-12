#' @title formatPrintInt
#' @export
formatPrintInt <- function (
  x, 
  big.mark=".", 
  decimal.mark = ",",
  scientific = FALSE
) {
  return (
    format(as.integer(round(x)), big.mark=big.mark, decimal.mark=decimal.mark, scientific=scientific)
  )
}

#' @title formatPrintDec
#' @export
formatPrintDec <- function (
  x, 
  round=4, 
  big.mark=".", 
  decimal.mark = ",",
  scientific = FALSE
) {
  return (
    format(round(x,round), big.mark=big.mark, decimal.mark=decimal.mark, scientific=scientific)
  )
}

#' @title formatPrintNumber
#' @export
formatPrintNumber <- function (
  x, 
  round=4, 
  big.mark=".", 
  decimal.mark = ",",
  scientific = FALSE
) {
  x = as.matrix(x)
  return (as.data.frame(ifelse(abs(x-as.integer(x))>0,
                               format(round(x,round), big.mark=big.mark, decimal.mark=decimal.mark, scientific=scientific),
                               format(as.integer(x), big.mark=big.mark, decimal.mark=decimal.mark, scientific=scientific)
  )))
}

#' @title printPerformance
#' @export
printPerformance <- function (
  listperformance,
  printConfMat = TRUE, printF1 = TRUE, printMetrics = TRUE,
  round = 5, 
  big.mark = ".", decimal.mark = ",", scientific = FALSE
) {
  dfConfMat = listperformance$dfConfMat
  dfF1 = listperformance$dfF1
  metrics = listperformance$metrics
  if (printConfMat) {
    strDFConfMat = formatPrintNumber(dfConfMat, round = round, 
                                     big.mark = big.mark, decimal.mark = decimal.mark, 
                                     scientific = scientific)
    print(strDFConfMat)
  }
  if (printF1) {
    strDFF1 = formatPrintNumber(dfF1, round = round, big.mark = big.mark, 
                                decimal.mark = decimal.mark, scientific = scientific)
    if (printConfMat) 
      cat("\n")
    print(strDFF1)
  }
  if (printMetrics) {
    strMetrics = data.frame(score = character(8), stringsAsFactors = FALSE)
    rownames(strMetrics) = c("Accuracy", "Precision", "Recall (sensitivity)", 
                             "specificity", "Score F1", "Score G", "Score Beta", 
                             "Score Phi (MCC)")
    strMetrics[1, 1] = formatPrintDec(metrics$accuracy, round = round, 
                                      big.mark = big.mark, decimal.mark = decimal.mark, 
                                      scientific = scientific)
    strMetrics[2, 1] = formatPrintDec(metrics$precision, 
                                      round = round, big.mark = big.mark, decimal.mark = decimal.mark, 
                                      scientific = scientific)
    strMetrics[3, 1] = formatPrintDec(metrics$recall, round = round, 
                                      big.mark = big.mark, decimal.mark = decimal.mark, 
                                      scientific = scientific)
    strMetrics[4, 1] = formatPrintDec(metrics$specificity, 
                                      round = round, big.mark = big.mark, decimal.mark = decimal.mark, 
                                      scientific = scientific)
    strMetrics[5, 1] = formatPrintDec(metrics$ScoreF1, round = round, 
                                      big.mark = big.mark, decimal.mark = decimal.mark, 
                                      scientific = scientific)
    strMetrics[6, 1] = formatPrintDec(metrics$ScoreG, round = round, 
                                      big.mark = big.mark, decimal.mark = decimal.mark, 
                                      scientific = scientific)
    strMetrics[7, 1] = formatPrintDec(metrics$ScoreBeta, 
                                      round = round, big.mark = big.mark, decimal.mark = decimal.mark, 
                                      scientific = scientific)
    strMetrics[8, 1] = formatPrintDec(metrics$ScorePhi, round = round, 
                                      big.mark = big.mark, decimal.mark = decimal.mark, 
                                      scientific = scientific)
    if (printConfMat || printF1) 
      cat("\n")
    print(strMetrics)
  }
}