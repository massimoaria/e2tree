dissimilarity <- function(y,s){

  dR <- y[s==0,s==0]
  dL <- y[s==1,s==1]
  n=dim(y)[1]
  nR=sum(s==0)
  nL=n-nR
  sR <- sum(dR)/(n*(nR-1))
  #sR <- ifelse(is.nan(sR),0,sR)

  sL <- sum(dL)/(n*(nL-1))
  #sL <- ifelse(is.nan(sL),0,sL)
  imp <- sR+sL

  return(imp)
}
