# roxygen2::roxygenise()

#' r.data.missings
#' @export
r.data.missings <- function (
  dades,
  outputDFNumeric = "dfMissingsNumeric",
  outputDFCategorical = "dfMissingsCategorical")
{
  dfMissingsNumeric = data.frame(cbind(t(rep(NA, 6))), stringsAsFactors=FALSE)
  names(dfMissingsNumeric) = c("Variable", "miss", "neg.", "zeros", "pos.", "reg.")
  irow = 1
  for (icol in 1:ncol(dades)) {
    tryCatch({
      if (is.numeric(dades[,icol])) {
        strCol = names(dades)[icol]
        values = dades[,icol]
        nomissings = which(!is.na(values))
        
        dfMissingsNumeric[irow, 1] = toupper(strCol)
        dfMissingsNumeric[irow, 2] = nrow(dades)-length(nomissings)
        dfMissingsNumeric[irow, 3] = length(which(values[nomissings]<0))
        dfMissingsNumeric[irow, 4] = length(which(values[nomissings]==0))
        dfMissingsNumeric[irow, 5] = length(which(values[nomissings]>0))
        dfMissingsNumeric[irow, 6] = length(nomissings)
        irow=irow+1
      }
    },
             error=function(cond) {
               message(cond)
               return(NA)
             },
             warning=function(cond) {
               message(cond)
               return(NULL)
             })
  }
  rownames(dfMissingsNumeric) = dfMissingsNumeric[,1]
  
  dfMissingsCategorical = data.frame(cbind(t(rep(NA, 5))), stringsAsFactors=FALSE)
  names(dfMissingsCategorical) = c("Variable", "miss", "Vacio", "Niveles", "reg.")
  irow = 1
  for (icol in 1:ncol(dades)) {
    tryCatch({
      if (!is.numeric(dades[,icol])) {
        strCol = names(dades)[icol]
        values = dades[,icol]
        nomissings = which(!is.na(values))
        
        dfMissingsCategorical[irow, 1] = toupper(strCol)
        dfMissingsCategorical[irow, 2] = nrow(dades)-length(nomissings)
        dfMissingsCategorical[irow, 3] = length(which(values[nomissings]==""))
        dfMissingsCategorical[irow, 4] = length(unique(values[nomissings]))
        dfMissingsCategorical[irow, 5] = length(nomissings)
        irow=irow+1
      }
    },
             error=function(cond) {
               message(cond)
               return(NA)
             },
             warning=function(cond) {
               message(cond)
               return(NULL)
             })
  }
  rownames(dfMissingsCategorical) = dfMissingsCategorical[,1]
  
  assign(outputDFNumeric, dfMissingsNumeric, envir=globalenv())
  assign(outputDFCategorical, dfMissingsCategorical, envir=globalenv())
}

#' r.data.missingsCrossed
#' @export
r.data.missingsCrossed <- function (
  dades,
  variablesFil,
  variablesCol,
  outputDF = "dfMissingsCrossed",
  outputDFPerc = "dfMissingsCrossedPerc",
  outputDFTotals = "dfMissingsCrossedTotals")
{
  dfMissingsCrossed = data.frame(cbind(t(rep(NA, 2+length(variablesCol)))), stringsAsFactors=FALSE)
  dfMissingsCrossedPerc = data.frame(cbind(t(rep(NA, 2+length(variablesCol)))), stringsAsFactors=FALSE)
  dfMissingsCrossedTotals = data.frame(cbind(t(rep(NA, 2+length(variablesCol)))), stringsAsFactors=FALSE)
  names(dfMissingsCrossed) = c("Variable", "Nivel", variablesCol)
  names(dfMissingsCrossedPerc) = c("Variable", "Nivel", variablesCol)
  names(dfMissingsCrossedTotals) = c("Variable", "Nivel", variablesCol)
  irow = 1
  for (label in variablesFil) {
    valuesFil =  datos[, label]
    if (is.numeric(valuesFil)) {
      dfMissingsCrossed[irow, 1] = label
      dfMissingsCrossed[irow, 2] = "numeric"
      dfMissingsCrossedPerc[irow, 1] = label
      dfMissingsCrossedPerc[irow, 2] = "numeric"
      dfMissingsCrossedTotals[irow, 1] = label
      dfMissingsCrossedTotals[irow, 2] = "numeric"      
      icol = 3
      for (column in variablesCol) {
        valuesCol =  datos[, column]
        
        ind = which(!is.na(valuesFil))
        numerator = length(which(is.na(valuesCol[ind])))
        denominator = length(ind)
        dfMissingsCrossed[irow, icol] = numerator
        dfMissingsCrossedPerc[irow, icol] = numerator / denominator
        dfMissingsCrossedTotals[irow, icol] = denominator
        
        icol = icol + 1
      }
      irow = irow + 1
      
      dfMissingsCrossed[irow, 1] = label
      dfMissingsCrossed[irow, 2] = "missing"
      dfMissingsCrossedPerc[irow, 1] = label
      dfMissingsCrossedPerc[irow, 2] = "missing"
      dfMissingsCrossedTotals[irow, 1] = label
      dfMissingsCrossedTotals[irow, 2] = "missing"      
      icol = 3
      for (column in variablesCol) {
        valuesCol =  datos[, column]
        
        ind = which(is.na(valuesFil))
        numerator = length(which(is.na(valuesCol[ind])))
        denominator = length(ind)
        dfMissingsCrossed[irow, icol] = numerator
        dfMissingsCrossedPerc[irow, icol] = numerator / denominator
        dfMissingsCrossedTotals[irow, icol] = denominator
        
        icol = icol + 1
      }
      irow = irow + 1        
    } else {
      niveles = unique(valuesFil)
      for (nivel in niveles) {
        
        dfMissingsCrossed[irow, 1] = label
        dfMissingsCrossed[irow, 2] = nivel
        icol = 3
        for (column in variablesCol) {
          valuesCol =  datos[, column]
          
          ind = which(valuesFil==nivel)      
          numerator = length(which(is.na(valuesCol[ind])))
          denominator = length(ind)
          dfMissingsCrossed[irow, icol] = numerator
          dfMissingsCrossedPerc[irow, icol] = numerator / denominator
          dfMissingsCrossedTotals[irow, icol] = denominator
          
          icol = icol + 1
        }
        irow = irow + 1
      }
      
      dfMissingsCrossed[irow, 1] = label
      dfMissingsCrossed[irow, 2] = "missing"
      dfMissingsCrossedPerc[irow, 1] = label
      dfMissingsCrossedPerc[irow, 2] = "missing"
      dfMissingsCrossedTotals[irow, 1] = label
      dfMissingsCrossedTotals[irow, 2] = "missing"      
      icol = 3
      for (column in variablesCol) {
        valuesCol =  datos[, column]
        
        ind = which(is.na(valuesFil))
        numerator = length(which(is.na(valuesCol[ind])))
        denominator = length(ind)
        dfMissingsCrossed[irow, icol] = numerator
        dfMissingsCrossedPerc[irow, icol] = numerator / denominator
        dfMissingsCrossedTotals[irow, icol] = denominator
        
        icol = icol + 1
      }
      irow = irow + 1        
    }
  } 
  
  if (!is.na(outputDF) && outputDF!="") assign(outputDF, dfMissingsCrossed, envir=globalenv())
  if (!is.na(outputDFPerc) && outputDFPerc!="") assign(outputDFPerc, dfMissingsCrossedPerc, envir=globalenv())
  if (!is.na(outputDFTotals) && outputDFTotals!="") assign(outputDFTotals, dfMissingsCrossedTotals, envir=globalenv())
}