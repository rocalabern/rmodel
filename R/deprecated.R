r.abslog.v3 <- function(x) {
  sign(x)*log10(pmax(1,abs(x), na.rm=TRUE))
}

r.abslog.v2 <- function(x) {
  ifelse(x==0,0,(x/abs(x))*log10(pmax(1,abs(x), na.rm=TRUE)))
}

r.abslog.v1 <- function(x) {  
  if (is.na(x)) return(0)
  else
    if (x>1) return(log10(x))
  else
    if (x< (-1)) return( -log10(-x))
  else return(0)
}

r.gain.old <- function(score,fuga, perc=0.2, mode = 0) {
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
