#X is a data frame of predictors (numerical, ordered or factor classes)
#max_cat: maximum number of categories for exhaustive binary partitioning
#         variables with more categories fall back to ordinal (frequency-based) splits

split <- function(X, max_cat = 10){
  # check column class and generate splits for each column

  xLabels <- names(X)

  S <- lapply(X,function(x){
    type <- class(x)
    if (length(type)>1){
      if ("ordered" %in% type){
        type <- "ordered"  # if ordered is present, use it
      } else {
        type <- type[1]  # otherwise take the first class
      }
    }

    if (type %in% c("character","factor")){
      S <- catSplit(x, max_cat = max_cat)  # split for categorical variables
    } else if (type %in% c("numeric","ordered","integer")){
      S <- ordSplit(x)  # split for numeric or ordered variables
    }
    return(S)
  })
  # merge splits into a single data frame
  lab <- rep(names(X),lengths(S))
  l <- paste(lab,unlist(lapply(S,names)))
  S <- (do.call(cbind, S))
  names(S) <- l
  row.names(S) <- row.names(X)
  res <- list(S=S,lab=lab)
  return(res)
}

# Splitting of categorical variables
# max_cat: if number of categories > max_cat, use frequency-ordered
#          ordinal splits (k-1 splits) instead of exhaustive binary
#          partitions (2^(k-1)-1 splits) to avoid combinatorial explosion
catSplit <- function(x, max_cat = 10){
  # coercing factor to character variable
  x <- as.character(x)
  # take unique values
  values <- sort(unique(x))
  k <- length(values)

  if (k < 2) {
    message("\nYou selected a variable with constant value!!!!\n")
    return()
  }

  if (k == 2) {
    # Binary variables — single split
    Sx <- data.frame(rep(0, length(x)))
    Sx[x == values[1], 1] <- 1
    names(Sx) <- paste("%in% c('", values[1], "')", collapse = "", sep = "")
    return(Sx)
  }

  if (k > max_cat) {
    # HIGH-CARDINALITY FALLBACK: order categories by frequency, then
    # generate k-1 ordinal splits (cumulative from most frequent)
    freq_order <- names(sort(table(x), decreasing = TRUE))
    # Direct factor → numeric rank (1 = most frequent)
    x_rank <- as.numeric(factor(x, levels = freq_order))

    # Generate ordinal splits on the rank — vectorized with outer()
    thresholds <- seq_len(k - 1)
    Sx <- as.data.frame(outer(x_rank, thresholds, "<=") + 0L)

    # Label: list categories in the left group
    lab <- vapply(thresholds, function(thr) {
      cats_left <- freq_order[1:thr]
      paste("%in% c('", paste(cats_left, collapse = "', '"), "')", sep = "")
    }, character(1))
    colnames(Sx) <- lab
    return(Sx)
  }

  # STANDARD: exhaustive binary partitions for k <= max_cat
  # generate all partitions
  p <- partitions::listParts(k, do.set = FALSE)

  # remove not binary partitions
  s <- lapply(p, function(x){
    lab <- as.character(x)
    if (length(lab) == 2){
      x <- eval(parse(text = lab[1]))
    } else {x <- NA}
  })

  s <- s[lengths(s) > 1]

  # generate splits
  S <- lapply(s, function(l){
    cbind(x %in% values[l])
  })

  # labeling splits
  lab <- unlist(lapply(s, function(x){
    paste("%in% c('", paste(values[x], collapse = "', '"), "')", sep = "")
  }))

  Sx <- as.data.frame(do.call(cbind, S))
  colnames(Sx) <- lab
  return(Sx)
}


# Splitting of ordered variables
ordSplit <- function(x){

  # take unique values of X and create split list and labels
  values <- sort(unique(x))
  if (length(values)>1){
    values <- values[-length(values)]
    Sxlabel <- paste("<=",values,sep="")

    # build up the splits — vectorized with outer()
    Sx <- as.data.frame(outer(x, values, "<=") + 0L)
    colnames(Sx) <- Sxlabel

    return(Sx)}
  else{
    message("\nYou selected a variable with constant value!!!!\n")
    return()
  }
}

