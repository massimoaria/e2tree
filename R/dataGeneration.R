dataGeneration <- function(n, p, errors.sd = 0.5, multicollinearity_strength = 0.8, mc_vars = NULL) {

  distr.labels <- c("normal", "uniform", "poisson(2)", "exponential(0.5)", "gamma(2,1)", "binomial(10,0.3)")

  ## distrb
  distr <- sample(x = 1:6, size = p, replace = TRUE)

  X <- matrix(NA, n, p)

  ### dependence links and strength
  strength <- runif(p, 0.6, 1)

  Y <- numeric(n)

  for (i in 1:p) {
    d <- distr[i]
    switch(d,
           "1" = {
             X[, i] <- rnorm(n)
           },
           "2" = {
             X[, i] <- runif(n)
           },
           "3" = {
             X[, i] <- rpois(n, lambda = 2)
           },
           "4" = {
             X[, i] <- rexp(n, rate = 0.5)
           },
           "5" = {
             X[, i] <- rgamma(n, shape = 2, rate = 1)
           },
           "6" = {
             X[, i] <- rbinom(n, size = 10, prob = 0.3)
           })
  }

  # Introduce multicollinearity for specified variables
  if (length(mc_vars) > 0 && length(mc_vars) <= p) {
    for (i in mc_vars[-1]) {
        X[, i] <- X[, i] + multicollinearity_strength * X[, mc_vars[1]]
    }
    names(mc_vars) = names(X)[mc_vars]
    mc_vars = mc_vars+1
  }

  Y <- X %*% strength

  errors <- rnorm(n, 0, errors.sd * sd(Y))

  Y <- Y + errors

  df <- data.frame(Y, X)
  distributions = distr.labels[distr]
  names(distributions) <- names(df)[-c(1, 2)]
  names(strength) <- names(df)[-c(1, 2)]
  results <- list(df = df, distributions = distributions, strength = strength, col_multicollinearity = mc_vars)
  return(results)
}


catTransformation <- function(df,cat.rate){
  #
  # Categorical set creation

  X <- df[,-1]
  Y <- df[,1]
  p <- ncol(X)
  pcat <- min(round(p*cat.rate),p)
  if (pcat>0){
    cat.levels <- sample(9,pcat,replace=TRUE)+1
    cat.cols <- sample(p,pcat,replace=FALSE)

    x.levels <- apply(X[,cat.cols], 2, function(x){
      length(unique(x))
    })

    ind <- which(x.levels - cat.levels<0)
    cat.levels[ind] <- x.levels[ind]

    for (i in 1:pcat){
      k <- cat.levels[i]
      h <- cat.cols[i]
      X[,h] <- (cut(X[,h],k, labels = letters[1:cat.levels[i]], ordered_result = TRUE))
      #X[,h] <- as.character(cut(X[,h],k, labels = letters[1:cat.levels[i]], ordered_result = TRUE))
    }
  }
  names(cat.cols) <- names(X)[cat.cols]
  names(cat.levels) <- names(cat.cols)
  results <- list(df_original=df, df_cat=data.frame(Y,X), X.cat=cat.cols, cat.levels=cat.levels)

  return(results)
}





# Data Selection Function
dataSelection <- function(data, n, p, cat.rate){
  if (cat.rate==0){
    df <- data[sample(1:nrow(data),size = n), c(1, sample(2:ncol(data), size = p))]
  }else{
    data <- catTransformation(data, cat.rate)
    df <- data$df_cat[sample(1:nrow(data$df_cat),size = n, replace = F), c(1, sample(2:ncol(data$df_cat), size = p, replace = F))]
  }
  return(df)
}





