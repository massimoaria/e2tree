eImpurity <- function(y,index,S){
  S <- S[index,]
  #v <- var(S)
  n <- length(index)
  tab <- colSums(S)
  ind <- !(tab>1 & tab < (n-1))

  imp <- future.apply::future_apply(S,2,function(s){
         g <- dissimilarity(y[index,index],s)
    })
  imp[ind] <- Inf

  return(imp)
 }




