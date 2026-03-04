utils::globalVariables(c("node", "Y", "p", "variable", "decImp", "splitLabel", 
                         "ID", "index", "Wt", "prob", "type", "column")) # to avoid CRAN check errors for tidyverse programming

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
  
  if (!inherits(formula, "formula")) {
    stop("Error: 'formula' must be a valid formula object.")
  }
  
  if (!is.data.frame(data) || nrow(data) == 0) {
    stop("Error: 'data' must be a non-empty data frame.")
  }
  
  if (!is.matrix(D) || nrow(D) != ncol(D)) {
    stop("Error: 'D' must be a square dissimilarity matrix.")
  }
  
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
  
  if (!is.list(setting) || !all(c("impTotal", "maxDec", "n", "level") %in% names(setting))) {
    stop("Error: 'setting' must be a list with keys: 'impTotal', 'maxDec', 'n', and 'level'.")
  }
  
  if (!all(sapply(setting, is.numeric)) || any(unlist(setting) <= 0)) {
    stop("Error: All values in 'setting' must be positive numeric values.")
  }
  
  # === Proceed with the function ===
  
  row.names(data) <- NULL
  Call <- match.call()
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())
  Terms <- attributes(mf)$terms
  
  response <- mf[,1]
  X <- ordered2factor(mf[,-1])
  
  # Create type object
  if (inherits(ensemble, "randomForest")) {
    type <- ensemble$type
  } else if (inherits(ensemble, "ranger")) {
    type <- tolower(ensemble$treetype)
  }
  
  setting$tMax <- 1
  for (i in 1:setting$level) setting$tMax <- setting$tMax*2+1
  
  ## Identify qualitative variables and number of categories
  var_classes <- get_classes(X)
  ind <- which(var_classes %in% c("factor","character"))
  ncat <- NULL
  if (length(ind)>0){
    for (i in 1:length(ind)){
      ncat[i] <- length(unique(X[,ind[i]]))
    }
    names(ncat) <- names(X)[ind]
  }
  var_classes[names(ncat)] <- ncat
  var_classes[!names(var_classes) %in% names(ncat)] <- -1
  var_classes <- as.numeric(var_classes)
  names(var_classes) <- names(X)
  
  ## Generate the split matrix S
  res <- split(X)
  S <- res$S
  l <- res$lab
  rm(res)
  
  # Initialize node vector
  nodes <- rep(1, nrow(S))
  
  # ✅ OPTIMIZATION 1: Pre-allocate vectors to avoid dynamic growth
  # Instead of: N <- c(N, t) which copies everything each iteration
  N <- integer(setting$tMax)
  N_count <- 0
  
  # List of non-terminal nodes - also pre-allocated
  nterm <- integer(setting$tMax)
  nterm_count <- 1
  nterm[1] <- 1
  
  m <- unique(response)
  
  # Create info dataframe structure
  if (type == "classification") {
    m_label <- paste("V", seq(1, length(m)*2), sep="")
    labels <- c("node","n", "pred", "prob", "impTotal", "impChildren","decImp","decImpSur",
                "variable" ,"split", "splitLabel", "variableSur", "splitLabelSur",
                "parent","children","terminal", "obs", "path", "ncat", m_label)
    info <- data.frame(matrix(NA, setting$tMax, length(labels)))
    names(info) <- labels
    indv <- (ncol(info)-length(m_label)+1):ncol(info)
  } else {
    labels <- c("node","n", "pred", "prob", "impTotal", "impChildren","decImp","decImpSur",
                "variable" ,"split", "splitLabel", "variableSur", "splitLabelSur",
                "parent","children","terminal", "obs", "path", "ncat")
    info <- data.frame(matrix(NA, setting$tMax, length(labels)))
    names(info) <- labels
  }
  
  # ✅ OPTIMIZATION 2: Store obs as separate list structure
  # Avoids issues with list columns and eliminates need for eval(parse())
  obs_list <- vector("list", setting$tMax)
  
  # ✅ OPTIMIZATION 3: Pre-compute paths incrementally
  path_list <- character(setting$tMax)
  path_list[1] <- ""  # Root has empty path
  
  ### Variance in the root node
  vart1 <- ifelse(type=="regression", variance(response), NA)
  
  # === Main Loop ===
  while(nterm_count > 0){
    # Get last non-terminal node
    t <- nterm[nterm_count]
    
    # Add to processed nodes list
    N_count <- N_count + 1
    N[N_count] <- t
    
    # Get observations in this node
    index <- which(nodes == t)
    
    ### Check Stopping Rules
    results <- eStoppingRules(D, index, t, setting, response, ensemble, vart1)
    
    ### Compute the response in the node
    switch(type,
           classification={
             res <- moda(response[index])
           },
           regression={
             res <- c(mean(response[index]), sum((response[index] - ensemble$predicted[index])^2))
           })
    
    info$pred[t] <- res[1]
    info$prob[t] <- as.numeric(res[2])
    info$node[t] <- t
    info$parent[t] <- floor(t/2)
    info$n[t] <- length(index)
    info$impTotal[t] <- results$impTotal
    
    ##### Statistics for classification
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
      # Terminal node
      info$terminal[t] <- TRUE
      obs_list[[t]] <- index  # ✅ Store directly in list
      path_list[t] <- paths_optimized(info, path_list, t)  # ✅ Optimized version
      
    } else {
      # Find best split using optimized eImpurity
      imp <- eImpurity(D, index, S)
      s <- which.min(imp)
      v <- gsub(" <=.*| %in%.*","",names(s))
      s2 <- which.min(imp[!(l %in% v)])
      
      # Max decrease of impurity
      decImp <- results$impTotal - imp[s]
      decImpSur <- results$impTotal - imp[s2]
      
      # Check for max impurity stopping rule
      if (decImp <= setting$maxDec){
        info$terminal[t] <- TRUE
        obs_list[[t]] <- index
        path_list[t] <- paths_optimized(info, path_list, t)
        
      } else if (suppressWarnings(Wtest(Y=response[index], X=S[index,s], p.value=0.05, type = type))){
        # Split the node
        
        info$impChildren[t] <- as.numeric(imp[s])
        info$decImp[t] <- as.numeric(decImp)
        info$decImpSur[t] <- as.numeric(decImpSur)
        info$split[t] <- as.numeric(s)
        info$splitLabel[t] <- names(s)
        info$splitLabelSur[t] <- names(s2)
        info$terminal[t] <- FALSE
        info$parent[t] <- floor(t/2)
        info$children[t] <- list(c(t*2, t*2+1))
        info$impTotal[t] <- results$impTotal
        obs_list[[t]] <- index  # ✅ Store directly
        path_list[t] <- paths_optimized(info, path_list, t)  # ✅ Optimized
        info$variable[t] <- gsub(" <=.*| %in%.*","",info$splitLabel[t])
        info$ncat[t] <- var_classes[info$variable[t]]
        
        # Update nodes and add children to queue
        nodes[index] <- (nodes[index]*2+1) - S[index,s]
        
        # ✅ OPTIMIZATION 4: Efficient queue management
        # Add children to non-terminal list
        new_children <- sort(unique(nodes[index]), decreasing=TRUE)
        for (child in new_children) {
          nterm_count <- nterm_count + 1
          nterm[nterm_count] <- child
        }
        
      } else {
        info$terminal[t] <- TRUE
        obs_list[[t]] <- index
        path_list[t] <- paths_optimized(info, path_list, t)
      }
    }
    
    # Remove current node from queue
    nterm_count <- nterm_count - 1
  }
  
  # Trim pre-allocated vectors to actual size
  N <- N[1:N_count]
  
  # Clean up info dataframe
  info <- info %>% drop_na(node)
  
  # ✅ Convert obs_list back to dataframe column for compatibility
  for (node_id in info$node) {
    info$obs[info$node == node_id] <- list(obs_list[[node_id]])
  }
  
  # ✅ Copy paths from path_list
  info$path <- path_list[info$node]
  
  info$pred_val <- as.numeric(factor(info$pred))
  info$variableSur <- gsub(" <=.*| %in%.*","",info$splitLabelSur)
  
  if (type == "classification") {
    yval2 <- as.matrix(info[,indv])
    ypred <- info$pred_val
    nodeprob <- info$n/first(info$n)
    info <- info[,-indv]
    
    yval2 <- cbind(ypred,yval2)
    attr(yval2,"dimnames")[[2]] <- paste("V",seq(ncol(yval2)),sep="")
    info$yval2 <- cbind(yval2, nodeprob)
  }
  
  ylevels <- levels(mf[[1]])
  row.names(info) <- info$node
  info <- info[as.character(N),]
  
  ## Normalized variance in nodes for regression
  if (type == "regression"){
    info$Wt <- NULL
    maxvar <- diff(range(response))^2L / 9L
    size <- length(response)
    
    # ✅ OPTIMIZATION 5: Direct access to obs_list instead of eval(parse())
    for (i in info$node){
      indice <- obs_list[[i]]
      v <- 1-(variance(response[indice])/(maxvar*length(indice)/size))
      info$Wt[info$node==i] <- ifelse(v<0, 0, v)
    }
    info <- info %>% relocate(Wt, .after=prob)
  }
  
  object <- csplit_str(info, X, ncat, call=Call, terms=Terms, control=setting, ylevels=ylevels)
  object$N <- N
  
  class(object) <- c("list", "e2tree")
  
  return(object)
}


paths_optimized <- function(info, path_list, t){
  # Calculate path incrementally instead of traversing entire tree
  # This is O(1) instead of O(depth)
  #
  # Args:
  #   info: info dataframe
  #   path_list: pre-computed paths for parent nodes
  #   t: current node
  #
  # Returns:
  #   Path string for current node
  
  if (t == 1) {
    return("")
  }
  
  # Get parent node
  parent_t <- floor(t / 2)
  
  # Get parent's path
  parent_path <- path_list[parent_t]
  
  # Determine symbol (left or right child)
  symb <- ifelse((t %% 2) == 0, "", "!")
  
  # Get split label from parent
  parent_split <- info$splitLabel[parent_t]
  
  # Build new path
  if (parent_path == "" || is.na(parent_path)) {
    # First split (children of root)
    new_path <- paste0(symb, parent_split)
  } else {
    # Append to parent's path
    new_path <- paste0(parent_path, " & ", symb, parent_split)
  }
  
  return(new_path)
}


### creation objects for rpart.plot ----
csplit_str <- function(info, X, ncat, call, terms, control, ylevels){
  # Create rpart-compatible object structure
  
  var_lev <- var_ord <- attribute <- list()
  for (i in names(ncat)){
    var_lev[[i]] <- seq(ncat[i])
    names(var_lev[[i]]) <- unique(X[,i])
    attribute[[i]] <- as.character(unique(X[,i]))
  }
  
  # Qualitative splits
  splits <- info %>%
    dplyr::filter(!is.na(variable)) %>%
    dplyr::select(n, ncat, variable, decImp, splitLabel)
  
  # Index for numerical predictors
  if(nrow(info) > 1){
    varnumerics <- strsplit(splits$splitLabel[splits$ncat==-1], "<=")
    splits$index[splits$ncat==-1] <- unlist(lapply(varnumerics, function(x) x[2]))
  }
  
  splits$index[splits$ncat != -1] <- seq(sum(splits$ncat != -1))
  splits$index <- as.numeric(splits$index)
  
  # Categorical splits
  catsplits <- splits %>%
    dplyr::filter(ncat != -1) %>%
    dplyr::mutate(ID = row_number()) %>%
    dplyr::group_by(ID) %>%
    dplyr::mutate(modal = gsub(paste0(variable," %in% "),"",splitLabel))
  
  splits <- splits %>%
    dplyr::select(n, ncat, decImp, index) %>%
    dplyr::rename(improve = decImp,
                  count = n) %>%
    dplyr::mutate(adj=0) %>%
    as.matrix()
  
  attr(splits,"dimnames")[[1]] <- info$variable[!is.na(info$variable)]
  
  # Creation of csplit
  if (nrow(catsplits) > 0){
    csplit <- matrix(2, nrow(catsplits), max(catsplits$ncat))
    
    for (i in 1:nrow(csplit)){
      modal <- eval(parse(text=catsplits$modal[i]))
      vec <- var_lev[[catsplits$variable[i]]]
      ind <- vec[modal]
      csplit[i, ind] <- 1
      ind <- vec[setdiff(names(vec), modal)]
      csplit[i, ind] <- 3
    }
    csplit <- as.matrix(csplit)
  } else {
    csplit <- NULL
  }
  
  object <- list(tree=info, csplit=csplit, splits=splits, 
                 call=call, terms=terms, control=control)
  
  attr(object,"xlevels") <- attribute
  attr(object,"ylevels") <- ylevels
  
  return(object)
}


# ============================================================================
# Helper Functions
# ============================================================================

variance <- function(x){
  # Calculate population variance
  sum((x - mean(x))^2) / length(x)
}


Wtest <- function(Y, X, type, p.value=0.05){
  # Wilcoxon test for regression stopping rule
  switch(type,
         "classification" = {resp <- TRUE},
         "regression" = {resp <- wilcox.test(Y ~ X)$p.value <= 0.05}
  )
  return(resp)
}


get_classes <- function(df) {
  # Get classes of dataframe columns
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
  # Convert ordered factors to regular factors
  varClass <- get_classes(X)
  varClass <- names(varClass[varClass=="ordered"])
  for (i in varClass){
    class(X[[i]]) <- "factor"
  }
  return(X)
}
