eStoppingRules <- function(y,index,t, setting, response){
  n <- length(index)

  if (n>1){
    impTotal <- meanDis(y[index,index])
    res <- moda(response[index])
  }else{impTotal <- 0
        res <- c(response[index[1]],1)
        }

  sRule <- isTRUE(impTotal<=setting$impTotal |
                    n<=setting$n |
                    res[2] > 0.95  | ### setting da inserire
                    (t*2)+1 > setting$tMax)
  results <- list(sRule=sRule,impTotal=impTotal,n=n)
  return(results)

}

############

meanDis <- function(dis){
  n <- nrow(dis)
  sum(dis)/(n*(n-1))
}

############

moda <- function(x) {
  if ( anyNA(x) ) x = x[!is.na(x)]
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  res <- c(ux[which.max(tab)[1]], max(tab)[1]/sum(tab))
  res[1] <- as.character(levels(x)[res[1]])
  return(res)

}
