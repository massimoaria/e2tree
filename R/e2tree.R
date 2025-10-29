utils::globalVariables(c("node", "Y", "p", "variable", "decImp", "splitLabel", "ID", "index", "Wt", "prob")) # to avoid CRAN check errors for tidyverse programming

#' Explainable Ensemble Tree
#'
#' It creates an explainable tree for Random Forest. Explainable Ensemble Trees (E2Tree) aimed to generate a “new tree” that can explain and represent the relational structure between the response variable and the predictors. This lead to providing a tree structure similar to those obtained for a decision tree exploiting the advantages of a dendrogram-like output.
#'
#' @param formula is a formula describing the model to be fitted, with a response but no interaction terms.
#' @param data a data frame containing the variables in the model. It is a data frame in which to interpret the variables named in the formula.
#' @param D is the dissimilarity matrix. This is a dissimilarity matrix measuring the discordance between two observations concerning a given classifier of a random forest model. The dissimilarity matrix is obtained with the \link{createDisMatrix} function.
#' @param ensemble is an ensemble tree object (for the moment ensemble works only with random forest objects)
#' @param setting is a list containing the set of stopping rules for the tree building procedure.
#' \tabular{lll}{
#' \code{impTotal}\tab   \tab The threshold for the impurity in the node\cr
#' \code{maxDec}\tab   \tab The threshold for the maximum impurity decrease of the node\cr
#' \code{n}\tab   \tab The minimum number of the observations in the node \cr
#' \code{level}\tab   \tab The maximum depth of the tree (levels) \cr}
#' Default is \code{setting=list(impTotal=0.1, maxDec=0.01, n=2, level=5)}.
#'
#' @return A e2tree object, which is a list with the following components:
#' \tabular{lll}{
#' \code{tree}\tab   \tab  A data frame representing the main structure of the tree aimed at explaining and graphically representing the relationships and interactions between the variables used to perform an ensemble method. \cr
#' \code{call}\tab   \tab The matched call\cr
#' \code{terms}\tab   \tab A list of terms and attributes \cr
#' \code{control}\tab   \tab A list containing the set of stopping rules for the tree building procedure  \cr
#' \code{varimp}\tab   \tab A list containing a table and a plot for the variable importance. Variable importance refers to a quantitative measure that assesses the contribution of individual variables within a predictive model towards accurate predictions. It quantifies the influence or impact that each variable has on the model's overall performance. Variable importance provides insights into the relative significance of different variables in explaining the observed outcomes and aids in understanding the underlying relationships and dynamics within the model \cr}
#'
#' @examples
#' \donttest{
#' ## Classification:
#' data(iris)
#'
#' # Create training and validation set:
#' smp_size <- floor(0.75 * nrow(iris))
#' train_ind <- sample(seq_len(nrow(iris)), size = smp_size)
#' training <- iris[train_ind, ]
#' validation <- iris[-train_ind, ]
#' response_training <- training[,5]
#' response_validation <- validation[,5]
#'
#' # Perform training:
#' ## "randomForest" package
#' ensemble <- randomForest::randomForest(Species ~ ., data=training,
#' importance=TRUE, proximity=TRUE)
#'
#' ## "ranger" package
#' ensemble <- ranger::ranger(Species ~ ., data = iris,
#' num.trees = 1000, importance = 'impurity')
#'
#' D <- createDisMatrix(ensemble, data=training, label = "Species",
#'                               parallel = list(active=FALSE, no_cores = 1))
#'
#' setting=list(impTotal=0.1, maxDec=0.01, n=2, level=5)
#' tree <- e2tree(Species ~ ., training, D, ensemble, setting)
#'
#'
#'
#' ## Regression
#' data("mtcars")
#'
#' # Create training and validation set:
#' smp_size <- floor(0.75 * nrow(mtcars))
#' train_ind <- sample(seq_len(nrow(mtcars)), size = smp_size)
#' training <- mtcars[train_ind, ]
#' validation <- mtcars[-train_ind, ]
#' response_training <- training[,1]
#' response_validation <- validation[,1]
#'
#' # Perform training
#' ## "randomForest" package
#' ensemble = randomForest::randomForest(mpg ~ ., data=training, ntree=1000,
#' importance=TRUE, proximity=TRUE)
#'
#' ## "ranger" package
#' ensemble <- ranger::ranger(formula = mpg ~ ., data = training,
#' num.trees = 1000, importance = "permutation")
#'
#' D = createDisMatrix(ensemble, data=training, label = "mpg",
#'                                parallel = list(active=FALSE, no_cores = 1))
#'
#' setting=list(impTotal=0.1, maxDec=(1*10^-6), n=2, level=5)
#' tree <- e2tree(mpg ~ ., training, D, ensemble, setting)
#'
#' }
#'
#' @export


e2tree <- function(formula, data, D, ensemble, setting=list(impTotal=0.1, maxDec=0.01, n=2, level=5)){

  # === Input Validation ===

  # Validate formula
  if (!inherits(formula, "formula")) {
    stop("Error: 'formula' must be a valid formula object.")
  }

  # Validate data
  if (!is.data.frame(data) || nrow(data) == 0) {
    stop("Error: 'data' must be a non-empty data frame.")
  }

  # Validate D (dissimilarity matrix)
  if (!is.matrix(D) || nrow(D) != ncol(D)) {
    stop("Error: 'D' must be a square dissimilarity matrix.")
  }

  # Validate ensemble
  if (inherits(ensemble, "randomForest")) {
    type <- ensemble$type
    if (!type %in% c("classification", "regression")) {
      stop("Error: 'type' in ensemble object must be 'classification' or 'regression'.")
    }

  } else if (inherits(ensemble, "ranger")) {
    type <- ensemble$treetype
    if (!type %in% c("Classification", "Regression")) {
      stop("Error: 'type' in ensemble object must be 'classification' or 'regression'.")
    }

  } else {
    stop("Error: 'ensemble' must be a trained 'randomForest' or 'ranger' model.")
  }

  # Validate setting
  if (!is.list(setting) || !all(c("impTotal", "maxDec", "n", "level") %in% names(setting))) {
    stop("Error: 'setting' must be a list with keys: 'impTotal', 'maxDec', 'n', and 'level'.")
  }

  # Ensure all setting values are numeric and positive
  if (!all(sapply(setting, is.numeric)) || any(unlist(setting) <= 0)) {
    stop("Error: All values in 'setting' must be positive numeric values.")
  }

  # === Proceed with the function ===

  row.names(data) = NULL
  Call <- match.call()
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())
  Terms <- attributes(mf)$terms

  response <- mf[,1]
  X <- ordered2factor(mf[,-1]) # Convert ordered factors to regular factors (to change!!!)

  # create type object
  if (inherits(ensemble, "randomForest")) {
    type <- ensemble$type  # "classification" or "regression"

  } else if (inherits(ensemble, "ranger")) {
    # Convert "Classification" or "Regression" in lower case
    type <- tolower(ensemble$treetype)
  }


  setting$tMax <- 1

  for (i in 1:setting$level) setting$tMax <- setting$tMax*2+1

  ## identify qualitative variable and the number of categories:
  # Determine classes of all variables in X
  #var_classes <- unlist(lapply(X,class))
  var_classes <- get_classes(X)
  # Identify indices of factors and character variables
  ind <- which(var_classes %in% c("factor","character"))
  # Calculate the number of unique categories for each factor and character variable
  ncat <- NULL
  if (length(ind)>0){
    for (i in 1:length(ind)){
      ncat[i] <- length(unique(X[,ind[i]]))
    }
    names(ncat) <- names(X)[ind]
  }
  #ncat <- (sapply(X[,ind], function(x) length(unique(x))))
  #ncat <- (apply(X[,ind],2, function(x) length(unique(x))))
  # Update var_classes with the number of categories for character and factor variables
  var_classes[names(ncat)] = ncat
  # Set other variable types to -1
  var_classes[!names(var_classes) %in% names(ncat)] = -1
  # Convert var_classes values to numeric
  var_classes <- as.numeric(var_classes)
  # Ensure the names of var_classes match those of X
  names(var_classes) <- names(X)

  ## Generate the split matrix S from the original predictor matrix X
  res <- split(X)
  S <- res$S
  l <- res$lab
  rm(res)

  # Initilize node vector
  nodes <- rep(1,nrow(S))

  # list of no-terminal nodes
  nterm <- 1
  m <- unique(response)

  if (type == "classification") {
  m_label <- paste("V", seq(1,length(m)*2), sep="")
  labels <- c("node","n", "pred", "prob", "impTotal", "impChildren","decImp","decImpSur","variable" ,"split", "splitLabel", "variableSur", "splitLabelSur","parent","children","terminal", "obs", "path", "ncat",m_label)
  info <- data.frame(matrix(NA,setting$tMax,length(labels)))
  names(info) <- labels
  indv <- (ncol(info)-length(m_label)+1):ncol(info)
  } else {
    labels <- c("node","n", "pred", "prob", "impTotal", "impChildren","decImp","decImpSur","variable" ,"split", "splitLabel", "variableSur", "splitLabelSur","parent","children","terminal", "obs", "path", "ncat")
    info <- data.frame(matrix(NA,setting$tMax,length(labels)))
    names(info) <- labels
    }

  N <- NULL

  ### variance in the root node
  vart1 = ifelse(type=="regression", variance(response), NA)

  while(length(nterm)>0){
    t <- tail(nterm,1)
    #print(t)
    N <- c(N,t)
    index <- which(nodes == t)

    ### check Stopping Rules----
    results <- eStoppingRules(D,index, t, setting, response, ensemble, vart1)

    ### Compute the response in the node
    switch(type,
           classification={
             res <- moda(response[index])
           },
           regression={
             res <- c(mean(response[index]), sum((response[index] - ensemble$predicted[index])^2)) # mean and MSE
           })
    ###

    info$pred[t] <- res[1]   # Mean for regression
    info$prob[t] <- as.numeric(res[2])  # MSE for regression
    info$node[t] <- t
    info$parent[t] <- floor(t/2)
    info$n[t] <- length(index)
    info$impTotal[t] <- results$impTotal

    ##### statistics
    if (type == "classification") {
    yval <- data.frame(Y=response[index]) %>%
      mutate(Y=factor(Y, levels=levels(m))) %>%
      group_by(Y) %>%
      summarize(n=n(),
                p=n/length(response[index])) %>%
      complete(Y, fill = list(n = 0, p = 0)) %>%
      select(n,p) %>%
      as.matrix() %>%
      c()

    info[t,indv] <- yval
    }

    ###

    if (isTRUE(results$sRule)){
      #list(imp=NA,decImp=NA, split=NA, splitLabel=NA, terminal=TRUE, impTotal=results$impTotal, obs=index, path=NA)
      info$terminal[t] <- TRUE
      #info$impTotal[t] <- results$impTotal
      info$obs[t] <- list(index)
      info$path[t] <- paths(info,t)

      #### add prediction
      #info[[t]]$pred <- prediction(Y[index])
      ####

    } else{
      imp<- eImpurity(D,index,S)
      s <- which.min(imp)
      v <- gsub(" <=.*| %in%.*","",names(s))
      s2 <- which.min(imp[!(l %in% v)])

      # max decrease of impurity
      decImp <- results$impTotal-imp[s]
      decImpSur <- results$impTotal-imp[s2]

      #check for max impurity stopping rule
      if (decImp<=setting$maxDec){
        #info[[t]] <- list(imp=NA,decImp=NA, split=NA, splitLabel=NA, terminal=TRUE, parent= floor(t/2), children=NA, impTotal=results$impTotal, obs=index, path=NA)
        #info$parent[t] <- floor(t/2)
        info$terminal[t] <- TRUE
        #info$impTotal[t] <- results$impTotal
        info$obs[t] <- list(index)
        info$path[t] <- paths(info,t)
      } else if (suppressWarnings(Wtest(Y=response[index], X=S[index,s], p.value=0.05, type = type))){
        # Stopping Rule with Mann-Whitney for regression case
        # if it is regression, check that the hypothesis that the two distributions in tL and tR are equal is rejected

        info$impChildren[t] <- as.numeric(imp[s])
        info$decImp[t] <- as.numeric(decImp)
        info$decImpSur[t] <- as.numeric(decImpSur)
        info$split[t] <- as.numeric(s)
        info$splitLabel[t] <- names(s)
        info$splitLabelSur[t] <- names(s2)
        info$terminal[t] <- FALSE
        info$parent[t] <- floor(t/2)
        info$children[t] <- list(c(t*2,t*2+1))
        info$impTotal[t] <- results$impTotal
        info$obs[t] <- list(index)
        info$path[t] <- paths(info,t)
        info$variable[t] <- gsub(" <=.*| %in%.*","",info$splitLabel[t])
        info$ncat[t] <- var_classes[info$variable[t]]

        nodes[index] <- (nodes[index]*2+1)-S[index,s]
        nterm <- c(nterm, sort(unique(nodes[index]), decreasing=T))
      }else{
        info$terminal[t] <- TRUE
        #info$impTotal[t] <- results$impTotal
        info$obs[t] <- list(index)
        info$path[t] <- paths(info,t)
      }
    }

    #print(info$path[t])
    nterm <- setdiff(nterm,t)

  }
  info <- info %>% drop_na(node)
  info$pred_val <- as.numeric(factor(info$pred))
  info$variableSur <- gsub(" <=.*| %in%.*","",info$splitLabelSur)

  if (type == "classification") {
  yval2 <- as.matrix(info[,indv])
  ypred <- info$pred_val
  nodeprob <- info$n/first(info$n)
  info <- info[,-indv]


  #info$yval2 <- cbind(ypred, yval2, nodeprob)
  yval2 <- cbind(ypred,yval2)
  #names(yval2) <- paste("V",seq(ncol(yval2)),sep="")
  attr(yval2,"dimnames")[[2]] <- paste("V",seq(ncol(yval2)),sep="")
  info$yval2 <- cbind(yval2, nodeprob)
  }
  #ylevels <- as.character(unique(response))
  ylevels <- levels(mf[[1]]) #### I need this to preserve the orginal attributes
  row.names(info) <- info$node
  info <- info[as.character(N),]

  ## normalized variance in nodes for regression
  if (type == "regression"){
    info$Wt <- NULL
    maxvar <- diff(range(response))^2L / 9L
    size <- length(response)
    for (i in info$node){
      indice <- unlist(eval(parse(text=info$obs[info$node==i])))
      v <- 1-(variance(response[indice])/(maxvar*length(indice)/size))
      info$Wt[info$node==i] <- ifelse(v<0,0,v)
    }
    info <- info %>% relocate(Wt, .after=prob)
  }


  object <- csplit_str(info,X,ncat, call=Call, terms=Terms, control=setting, ylevels=ylevels)

  #object$varimp <- vimp(object, data = data)
  ###### DOESN'T WORK!!

  object$N <- N

  class(object) <- c("list", "e2tree")

  return(object)
}


paths <- function(info,t){
  path <- NULL
  while(t[1]>1){
    tp <- floor(t[1]/2)
    symb <- ifelse((t[1] %% 2)==0,"","!")
    t <- c(tp,t)
    path <- c(paste(symb,info$splitLabel[tp],sep=""),path)
    #<- paste(path," AND ", symb,info[[tp]]$splitLabel, sep="",collapse="")
  }
  path <- paste(path,sep="",collapse=" & ")
  return(path)
}


### creation objects for rpart.plot ----
csplit_str <- function(info,X,ncat, call, terms, control, ylevels){

  var_lev <- var_ord <- attribute <- list()
  for (i in names(ncat)){
    var_lev[[i]] <- seq(ncat[i])
    names(var_lev[[i]]) <- unique(X[,i])
    attribute[[i]] <- as.character(unique(X[,i]))
  }

  # ordVar <- get_classes(X)
  # ordVar <- names(ordVar[ordVar=="ordered"])
  # for (i in names(ordVar)){
  #   var_ord[[i]] <- seq(length(levels(X[[i]])))
  #   names(var_ord[[i]]) <- unique(levels(X[[i]]))
  #   attribute[[i]] <- as.character(levels(X[[i]]))
  # }

  #qualitative splits
  splits <- info %>%
    dplyr::filter(!is.na(variable)) %>%
    dplyr::select(n,ncat,variable,decImp, splitLabel)

  #splits <- info[!is.na(info$variable), c("n","ncat","variable","decImp", "splitLabel")]
  # index for numerical predictors
  if(nrow(info)>1){
    varnumerics <- strsplit(splits$splitLabel[splits$ncat==-1],"<=")
    splits$index[splits$ncat==-1] <- unlist(lapply(varnumerics, function(x) x[2]))
  }

  #splits$index <- suppressWarnings(as.numeric(as.numeric(gsub("[^[:digit:]., ]", "",splits$splitLabel))))
  splits$index[splits$ncat!=-1] <- seq(sum(splits$ncat!=-1))
  splits$index <- as.numeric(splits$index)



  catsplits <- splits %>%
    dplyr::filter(ncat!=-1) %>%
    dplyr::mutate(ID = row_number()) %>%
    dplyr::group_by(ID) %>%
    dplyr::mutate(modal = gsub(paste0(variable," %in% "),"",splitLabel))

  splits <- splits %>%
    #data.frame() %>%
    dplyr::select(n,ncat,decImp,index) %>%
    dplyr::rename(improve = decImp,
                  count = n) %>%
    dplyr::mutate(adj=0) %>%
    as.matrix()

  attr(splits,"dimnames")[[1]]  <- info$variable[!is.na(info$variable)]

  ### creation of csplit
  if (nrow(catsplits)>0){
    csplit <- matrix(2,nrow(catsplits),max(catsplits$ncat))

    for (i in 1:nrow(csplit)){
      modal <- eval(parse(text=catsplits$modal[i]))
      vec <- var_lev[[catsplits$variable[i]]]
      ind <- vec[modal]
      csplit[i,ind] <- 1
      ind <- vec[setdiff(names(vec),modal)]
      csplit[i,ind] <- 3
    }
    csplit <- as.matrix(csplit)
  } else {
    csplit = NULL
  }

  object <- list(tree=info, csplit=csplit,splits=splits, call=call, terms=terms, control=control)

  attr(object,"xlevels") <- attribute
  attr(object,"ylevels") <- ylevels

  return(object)
}





## Variance
variance <- function(x){
  sum((x-mean(x))^2)/length(x)
}





Wtest = function(Y, X, type, p.value=0.05){
  switch (type,
          "classification" = {resp=TRUE},
          "regression" = {resp <- wilcox.test(Y ~ X)$p.value <= 0.05}
  )
  return(resp)
}

# get_classes <- function(df) {
#   dfClasses <- data.frame(
#     column = names(df),
#     class = sapply(df, function(x) paste(class(x), collapse = ", "))
#   ) %>%
#     mutate(type = class,
#            type = ifelse(sapply(df, is.factor), "factor",
#                          ifelse(sapply(df, is.character), "character", type))) %>%
#     select(column, type)
#   classes <- dfClasses$type
#   names(classes) <- dfClasses$column
#   return(classes)
# }

get_classes <- function(df) {
  dfClasses <- data.frame(
    column = names(df),
    class = sapply(df, function(x) paste(class(x), collapse = ", "))
  ) %>%
    mutate(type = class,
           type = ifelse(regexpr("ordered",type)>-1, "ordered",
                         ifelse(sapply(df, is.character), "character", type))) %>%
    select(column, type)
  classes <- dfClasses$type
  names(classes) <- dfClasses$column
  return(classes)
}

ordered2factor <- function(X){
  varClass <- get_classes(X)
  varClass <- names(varClass[varClass=="ordered"])
  for (i in varClass){
    class(X[[i]]) <- "factor"
  }
  return(X)
}
