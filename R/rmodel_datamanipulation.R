#' r.percentils
#' @export
r.percentils <- function (var,
                          ndiv = 10) {
  return(quantile(var[which(!is.na(var))], seq(from=0, to=1, length.out=(ndiv+1))))
}

#' r.binning
#' @seealso \code{\link{r.binning}} \code{\link{r.segment}} \code{\link{r.segment.target}} \code{\link{r.segment.dataframe}} \code{\link{r.segment.target.table}}
#' @export
r.binning <- function (var,
                       div = NULL,
                       ndiv = 10,
                       labelText = FALSE,
                       labelNumeric = FALSE,
                       prefix = "Grup",
                       link1 = " ",
                       link2 = ", ")
{
  if(missing(div) || is.null(div) || is.na(div)) {
    if(is.null(ndiv) || is.na(ndiv)) ndiv = 10
    div = r.percentils(var, ndiv=ndiv)
    div = unique(div)
    if (length(div)<=1 || ndiv==1) div = r.max(var)
    else if (length(div)<ndiv+1) {
      div = div[1:(length(div)-1)]
    } else {
      div = div[2:(length(div)-1)]
    }
  }
  ndiv = length(div)+1
  
  output = rep(NA, length(var))
  
  for (i in ndiv:1) {
    if (i==1) {      
      ind = which(var<=div[1])
      sufix = paste0("x<=",formatC(div[1],width=max(nchar(as.character(div))),format="f",flag="0"))
    } else if (i==ndiv) {
      ind = which(div[length(div)]<var)
      sufix = paste0(formatC(div[length(div)],width=max(nchar(as.character(div))),format="f",flag="0"),"<x")
    } else {
      ind1 = which(div[i-1]<var)
      ind2 = which(var<=div[i])  
      ind = intersect(ind1, ind2)
      sufix = paste0(formatC(div[i-1],width=max(nchar(as.character(div))),format="f",flag="0"),"<x",
                     link2,
                     "x<=",formatC(div[i],width=max(nchar(as.character(div))),format="f",flag="0"))
    }
    if (labelText && labelNumeric)
      output[ind] = paste0(prefix, link1, formatC(i-1,width=ceiling(log10(ndiv)),format="d",flag="0"), link1, sufix)
    else if (labelText)
      output[ind] = paste0(prefix, link1, sufix)
    else if (labelNumeric)
      output[ind] = paste0(prefix, link1, formatC(i-1,width=ceiling(log10(ndiv)),format="d",flag="0"))
    else
      output[ind] = i
  }
  return (output)
}

#' r.segment
#' @param dades dataframe
#' @param segmentacion Array of strings. Names of the columns to create segmentation. Those variables must be categorical.
#' @return An array of numeric or labels according to segmentation.
#' @seealso \code{\link{r.binning}} \code{\link{r.segment}} \code{\link{r.segment.target}} \code{\link{r.segment.dataframe}} \code{\link{r.segment.target.table}}
#' @export
r.segment <- function(dades, segmentacion, 
                      prefix = "Grupo",
                      link = " ",
                      labelText = FALSE,
                      labelNumeric = FALSE,
                      missing=NA)
{
  nelements = NaN
  listValues = list()
  nGroups = 1
  for (ipos in 1:length(segmentacion)) {
    segment = segmentacion[ipos]
    var = dades[,segment]
    values = unique(var)
    listValues[[segment]] = values
    nelements[ipos] = length(values)
    nGroups = nGroups * length(values)
  }
  
  iGroup=1
  column = rep(missing, nrow(dades))
  finished = FALSE
  index = rep(1, length(segmentacion))
  while (!finished) {
    label = prefix
    if (labelNumeric) {
      if (label!="") label = paste0(label, link, formatC(iGroup-1,width=ceiling(log10(nGroups)),format="d",flag="0"))
      else label = formatC(iGroup-1,width=ceiling(log10(nGroups)),format="d",flag="0")
    }
    
    ind = 1:nrow(dades)
    for (ipos in 1:length(segmentacion)) {
      segment = segmentacion[ipos]
      ind = intersect(ind, which(dades[, segment]==listValues[[segment]][index[ipos]]))
      
      if (labelText) {
        if (label!="") label = paste0(label, link, segment, "-", listValues[[segment]][index[ipos]])
        else label = paste0(segment, "-", listValues[[segment]][index[ipos]])
      }      
    }
    if (!labelNumeric && !labelText) {
      label=iGroup
    }
    
    if (length(ind)>0) {
      column[ind] = label
    }
    
    ipos=1
    incrementFinished = FALSE
    while (ipos<=length(segmentacion) && !incrementFinished) {
      index[ipos] = index[ipos]+1
      if (index[ipos]>nelements[ipos]) {
        index[ipos] = 1
        ipos = ipos+1
      } else {
        incrementFinished = TRUE
      }
    }
    if (!incrementFinished) finished = TRUE
    iGroup = iGroup+1
  }
  return (column)
}

#' r.segment.target
#' @param dades dataframe
#' @param segmentacion Array of strings. Names of the columns to create segmentation. Those variables must be categorical.
#' @param target String. Name of the target column to calculate means for each segment. If targetValue is provided, then means are not calculated, and percentatge of counting targetValue is provided instead.
#' @param targetValue Numeric or Categorical. Value of target column to calculate counts instead of means.
#' @return An array of percentages of target for each segmentation.
#' @seealso \code{\link{r.binning}} \code{\link{r.segment}} \code{\link{r.segment.target}} \code{\link{r.segment.dataframe}} \code{\link{r.segment.target.table}}
#' @export
r.segment.target <- function(dades, segmentacion, target,
                             targetValue = NULL)
{
  nelements = NaN
  listValues = list()
  for (ipos in 1:length(segmentacion)) {
    segment = segmentacion[ipos]
    var = dades[,segment]
    values = unique(var)
    listValues[[segment]] = values
    nelements[ipos] = length(values)
  }
  
  column = rep(0, nrow(dades))
  finished = FALSE
  index = rep(1, length(segmentacion))
  while (!finished) {
    ind = 1:nrow(dades)
    for (ipos in 1:length(segmentacion)) {
      segment = segmentacion[ipos]
      ind = intersect(ind, which(dades[, segment]==listValues[[segment]][index[ipos]]))
    }
    
    if (length(ind)>0) {
      if (missing(targetValue) && is.numeric(dades[ind, target])) column[ind] = mean(dades[ind, target])
      else if (missing(targetValue) && !is.numeric(dades[ind, target])) column[ind] = mean(as.numeric(as.character(dades[ind, target])))
      else column[ind] = length(which(dades[ind, target]==targetValue))/length(ind)
    }
    
    ipos=1
    incrementFinished = FALSE
    while (ipos<=length(segmentacion) && !incrementFinished) {
      index[ipos] = index[ipos]+1
      if (index[ipos]>nelements[ipos]) {
        index[ipos] = 1
        ipos = ipos+1
      } else {
        incrementFinished = TRUE
      }
    }
    if (!incrementFinished) finished = TRUE
  }
  return (column)
}

#' r.segment.dataframe
#' @param dades dataframe
#' @param segmentacion Array of strings. Names of the columns to create segmentation. Those variables must be categorical.
#' @param target String. Name of the target column to calculate means for each segment. If targetValue is provided, then means are not calculated, and percentatge of counting targetValue is provided instead.
#' @param targetValue Numeric or Categorical. Value of target column to calculate counts instead of means.
#' @return A dataframe with segmentation as rows with some extra statistics columns.
#' @seealso \code{\link{r.binning}} \code{\link{r.segment}} \code{\link{r.segment.target}} \code{\link{r.segment.dataframe}} \code{\link{r.segment.target.table}}
#' @export
r.segment.dataframe <- function(dades, segmentacion, 
                                target = NULL,
                                targetValue = NULL,
                                prefix = "",
                                link = " ",
                                labelText = TRUE,
                                labelNumeric = FALSE,
                                missing=NA)
{
  if (!missing(target) && (missing(targetValue) || is.null(targetValue))) {
    stop("Si proporciones un target has de proporcionar tambe un targetValue")
  }
  if (!missing(targetValue) && (missing(target) || is.null(target))) {
    stop("Si proporciones un targetValue has de proporcionar tambe un target")
  }  
  nelements = NaN
  listValues = list()
  nGroups = 1
  for (ipos in 1:length(segmentacion)) {
    segment = segmentacion[ipos]
    var = dades[,segment]
    values = unique(var)
    listValues[[segment]] = values
    nelements[ipos] = length(values)
    nGroups = nGroups * length(values)
  }

  if (!missing(target) && !is.null(target)
      && !missing(targetValue) && !is.null(targetValue)) {
    dfOutput = data.frame(matrix(NA, nrow=nGroups , ncol=length(segmentacion)+7), stringsAsFactors=FALSE)
    names(dfOutput) = c("Segment", segmentacion, "Freq", 
                        paste0(target, "_Hits"),
                        paste0(target, "_Perc_Hits_HitsTotal"),
                        paste0(target, "_Perc_Hits_Segment"),
                        paste0(target, "_Perc_Hits_Total"),
                        paste0(target, "_Mean"))
  } else {
    dfOutput = data.frame(matrix(NA, nrow=nGroups , ncol=length(segmentacion)+2), stringsAsFactors=FALSE)
    names(dfOutput) = c("Segment", segmentacion, "Freq")
  }
  
  iGroup=1
  column = rep(missing, nrow(dades))
  finished = FALSE
  index = rep(1, length(segmentacion))
  while (!finished) {
    label = prefix
    if (labelNumeric) {
      if (label!="") label = paste0(label, link, formatC(iGroup-1,width=ceiling(log10(nGroups)),format="d",flag="0"))
      else label = formatC(iGroup-1,width=ceiling(log10(nGroups)),format="d",flag="0")
    }
    
    ind = 1:nrow(dades)
    for (ipos in 1:length(segmentacion)) {
      segment = segmentacion[ipos]
      ind = intersect(ind, which(dades[, segment]==listValues[[segment]][index[ipos]]))
      
      if (labelText) {
        if (label!="") label = paste0(label, link, segment, "-", listValues[[segment]][index[ipos]])
        else label = paste0(segment, "-", listValues[[segment]][index[ipos]])
      }      
    }
    if (!labelNumeric && !labelText) {
      label=iGroup
    }
    
    dfOutput[iGroup, 1] = label
    for (ipos in 1:length(segmentacion)) {
      segment = segmentacion[ipos]
      dfOutput[iGroup, 1+ipos] = as.character(listValues[[segment]][index[ipos]])
    }
    dfOutput[iGroup, 1+length(segmentacion)+1] = length(ind)
    if (!missing(target) && !is.null(target) 
        && !missing(targetValue) && !is.null(targetValue)) {
      
      dfOutput[iGroup, 1+length(segmentacion)+2] = length(which(dades[ind, target]==targetValue))
      
      dfOutput[iGroup, 1+length(segmentacion)+3] = length(which(dades[ind, target]==targetValue))/length(which(dades[, target]==targetValue))
      if (length(ind)==0) 
        dfOutput[iGroup, 1+length(segmentacion)+4] = 0
      else
        dfOutput[iGroup, 1+length(segmentacion)+4] = length(which(dades[ind, target]==targetValue))/length(ind)
      dfOutput[iGroup, 1+length(segmentacion)+5] = length(which(dades[ind, target]==targetValue))/nrow(dades)
      
      if (length(ind)==0) 
        dfOutput[iGroup, 1+length(segmentacion)+6] = 0
      else if (is.numeric(dades[ind, target])) 
        dfOutput[iGroup, 1+length(segmentacion)+6] = r.mean(dades[ind, target])
      else
        dfOutput[iGroup, 1+length(segmentacion)+6] = r.mean(as.numeric(as.character(dades[ind, target])))
    }
    
    ipos=1
    incrementFinished = FALSE
    while (ipos<=length(segmentacion) && !incrementFinished) {
      index[ipos] = index[ipos]+1
      if (index[ipos]>nelements[ipos]) {
        index[ipos] = 1
        ipos = ipos+1
      } else {
        incrementFinished = TRUE
      }
    }
    if (!incrementFinished) finished = TRUE
    iGroup = iGroup+1
  }
  
  rownames(dfOutput) = dfOutput[,1]
  return (dfOutput)
}

#' r.segment.target.table
#' @param dades dataframe
#' @param segmentacion Array of strings. Names of the columns to create segmentation. Those variables must be categorical.
#' @param target String. Name of the target column to calculate means for each segment. If targetValue is provided, then means are not calculated, and percentatge of counting targetValue is provided instead.
#' @param targetValue Numeric or Categorical. Value of target column to calculate counts instead of means.
#' @return An array of percentages of target for each segmentation with labels.
#' @seealso \code{\link{r.binning}} \code{\link{r.segment}} \code{\link{r.segment.target}} \code{\link{r.segment.dataframe}} \code{\link{r.segment.target.table}}
#' @export
r.segment.target.table <- function(dades, segmentacion, target,
                                   targetValue = NULL,
                                   prefix = "Grupo",
                                   link = " ",
                                   labelText = FALSE,
                                   labelNumeric = FALSE)
{
  nelements = NaN
  listValues = list()
  nGroups = 1
  for (ipos in 1:length(segmentacion)) {
    segment = segmentacion[ipos]
    var = dades[,segment]
    values = unique(var)
    listValues[[segment]] = values
    nelements[ipos] = length(values)
    nGroups = nGroups * length(values)
  }
  
  iGroup=1
  output = rep(0, nGroups)
  finished = FALSE
  index = rep(1, length(segmentacion))
  while (!finished) {
    label = prefix
    if (labelNumeric) {
      if (label!="") label = paste0(label, link, formatC(iGroup-1,width=ceiling(log10(nGroups)),format="d",flag="0"))
      else label = formatC(iGroup-1,width=ceiling(log10(nGroups)),format="d",flag="0")
    }
    
    ind = 1:nrow(dades)
    for (ipos in 1:length(segmentacion)) {
      segment = segmentacion[ipos]
      ind = intersect(ind, which(dades[, segment]==listValues[[segment]][index[ipos]]))
      
      if (labelText) {
        if (label!="") label = paste0(label, link, segment, "-", listValues[[segment]][index[ipos]])
        else label = paste0(segment, "-", listValues[[segment]][index[ipos]])
      }      
    }
    if (!labelNumeric && !labelText) {
      label=iGroup
    }
    
    if (length(ind)>0) {
      names(output)[iGroup] = label
      output[iGroup] = length(which(dades[ind, target]==1))/length(ind)
      if (missing(targetValue) && is.numeric(dades[ind, target])) output[iGroup] = mean(dades[ind, target])
      else if (missing(targetValue) && !is.numeric(dades[ind, target])) output[iGroup] = mean(as.numeric(as.character(dades[ind, target])))
      else output[iGroup] = length(which(dades[ind, target]==targetValue))/length(ind)
    } else {
      names(output)[iGroup] = label
      output[iGroup] = 0
    }
    
    ipos=1
    incrementFinished = FALSE
    while (ipos<=length(segmentacion) && !incrementFinished) {
      index[ipos] = index[ipos]+1
      if (index[ipos]>nelements[ipos]) {
        index[ipos] = 1
        ipos = ipos+1
      } else {
        incrementFinished = TRUE
      }
    }
    if (!incrementFinished) finished = TRUE
    iGroup = iGroup+1
  }
  return (output)
}