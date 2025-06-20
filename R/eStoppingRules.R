eStoppingRules <- function(y,index,t, setting, response, ensemble, vart1){
  n <- length(index)
  
  # create type object
  if (inherits(ensemble, "randomForest")) {
    type <- ensemble$type  # "classification" o "regression"
    
  } else if (inherits(ensemble, "ranger")) {
    # Convert "Classification" or "Regression" in lower case
    type <- tolower(ensemble$treetype)
  }
  
  if (n>1){
    impTotal <- meanDis(y[index,index])
    switch(type,
           classification = {
             res <- as.numeric(moda(response[index])[2])
           },
           regression={
             res <- 1-(variance(response[index])/vart1)
           }
    )
  } else {impTotal <- 0
  res <- 1
  }


    sRule <- isTRUE(impTotal<=setting$impTotal |
                      n<=setting$n |
                      res > 0.95  | ### if the variance is less than 5% of the variance in the root node, stop
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
