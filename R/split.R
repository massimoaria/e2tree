#X is a data frame of predictors (numerical, ordered or factor classes)

split <- function(X){
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
      #print(colnames(x))
      S <- catSplit(x)  # split for categorical variables
      #names(S) <- gsub("VAR %in%",paste(xLabels[1]," %in%",collapse=""),names(S))
      #xLabels <- xLabels[-1]
    } else if (type %in% c("numeric","ordered","integer")){
      #print(colnames(x))
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
catSplit <- function(x){
  # coercing factor to character variable
  x <- as.character(x)
  # take unique values
  values <- sort(unique(x))

  # identify partitions
  if (length(values)>2){  #Multinomial variables

    # generate all partitions
    p <- partitions::listParts(length(values),do.set=F)

    # remove not binary partitions
    s <- lapply(p, function(x){
      lab <- as.character(x)
      if (length(lab)==2){
        x <- eval(parse(text=lab[1]))
      } else {x <- NA}

    })

    s <- s[lengths(s)>1]

    # generate splits
    S <- lapply(s, function(l){
      cbind(x %in% values[l])
    })

    # labeling splits
    lab <- unlist(lapply(s, function(x){
      paste("%in% c('",paste(values[x],collapse="', '"),"')",sep="")
    }))

    Sx <- as.data.frame(do.call(cbind, S))
    colnames(Sx) <- lab
  } else if (length(values)==2){  #Binary variables
    Sx <- data.frame(rep(0,length(x)))
    Sx[x==values[1],1] <- 1
    names(Sx) <- paste("%in% c('",values[1],"')", collapse="", sep="")
  } else if (length(values)<2){  #Single value variables
    message("\nYou selected a variable with constant value!!!!\n")
    return()
  }
  return(Sx)
}


# Splitting of ordered variables
ordSplit <- function(x){

  # take unique values of X and create split list and labels
  values <- sort(unique(x))
  if (length(values)>1){
    values <- values[-length(values)]
    Sxlabel <- paste("<=",values,sep="")

    # build up the splits
    s <- lapply(values,function(s){
      cbind(as.numeric(x<=s))
    })
    Sx <- as.data.frame((do.call(cbind, s)))
    colnames(Sx) <- Sxlabel

    return(Sx)}
  else{
    message("\nYou selected a variable with constant value!!!!\n")
    return()
  }
}

