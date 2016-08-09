# roxygen2::roxygenise()

#' @title r.order_normalize
#' @export
r.order_normalize <- function(var, fill.na = TRUE) {
  normalized = numeric(length(var))
  
  ind = which(is.na(var))
  if (length(ind)>0) {
    if (fill.na) {
      normalized[ind] = 0.5
    } else {
      normalized[ind] = as.numeric(NA)
    }
  }
  
  ind = which(!is.na(var))
  if (length(ind)>0) {
    normalized[ind] = (order(var[ind])-1)/(length(ind)-1)
  }
}

#' @title r.quantile_normalize
#' @export
r.quantile_normalize <- function(var, nquantiles=10, probs=seq(0,1,by=1/nquantiles), normalize=TRUE, na.rm=TRUE, rightmost.closed = TRUE, all.inside = FALSE) {
  varSplits = quantile(var, probs=probs, na.rm=na.rm)
  if (any(duplicated(varSplits))) warning("r.quantile_normalize : Duplicated splits values using quantiles.")
  varSplits = unique(varSplits)
  nquantiles = length(varSplits)-1
 
  output = findInterval(var, varSplits, rightmost.closed = rightmost.closed, all.inside = all.inside)
  if (normalize) {
    output = (output-1)/(nquantiles-1)
  }
  return(output)
}

#' @title r.create_dummies
#' @export
r.create_dummies <- function(
  df, 
  list_var = colnames(df)[lapply(df, class) %in% c("factor", "character")], 
  df_input = df, 
  deleteOriginalVar = TRUE, 
  levelsOnly = FALSE, 
  fullRank = FALSE, 
  sep = "_"
) {
  library(caret)
  df_output = df
  for (var in list_var) {
    pos = match(var, colnames(df_input))
    if (length(pos)==0 || is.na(pos)) stop(paste0("Var ",var," not found."))
   
    oDummyVars = caret::dummyVars(formula(paste0("~ ",var)), data = df_input, levelsOnly = levelsOnly, fullRank = fullRank, sep = sep)
    dfDummyVars = caret::predict.dummyVars(oDummyVars, df_input)
   
    pos = match(var, colnames(df_output))
    if (length(pos)==0 || is.na(pos)) {
      df_output[[var]] = df_input[[var]]
      pos = match(var, colnames(df_output))
    }
    if (data.table::is.data.table(df_output)) {
      if (pos==1) {
        df_output = cbind(df_output[,1, with=FALSE], dfDummyVars, df_output[,2:(ncol(df_output)), with=FALSE])
      } else if (pos==ncol(df_output)) {
        df_output = cbind(df_output, dfDummyVars)
      } else {
        df_output = cbind(df_output[,1:(pos), with=FALSE], dfDummyVars, df_output[,(pos+1):(ncol(df_output)), with=FALSE])
      }
    } else {
      if (pos==1) {
        df_output = cbind(df_output[,1], dfDummyVars, df_output[,2:(ncol(df_output))])
      } else if (pos==ncol(df_output)) {
        df_output = cbind(df_output, dfDummyVars)
      } else {
        df_output = cbind(df_output[,1:(pos)], dfDummyVars, df_output[,(pos+1):(ncol(df_output))])
      }   
    }
    if (deleteOriginalVar) {
      df_output[[var]] = NULL
    }
  }
  invisible(df_output)
}

#' @title r.redistribution
#' @export
r.redistribution <- function(array, array_ref, probs = seq(0,1,0.05)) {
  q = quantile(array, probs = probs)
  q_ref = quantile(array_ref, probs = probs)

  q_ref[findInterval(array, q)]
}

#' @title r.quantileSplit
#' @export
r.quantileSplit <- function(var, nquantiles=10, probs=seq(0,1,by=1/nquantiles), na.rm=TRUE, rightmost.closed = TRUE, all.inside = FALSE, showSplits = FALSE) {
  varSplits = quantile(var, probs=probs, na.rm=na.rm)
  if (any(duplicated(varSplits))) warning("r.quantileSplit : Duplicated splits values using quantiles.")
  if (showSplits) {
    print(varSplits)
  }
  findInterval(var, varSplits, rightmost.closed = rightmost.closed, all.inside = all.inside)
}
