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
