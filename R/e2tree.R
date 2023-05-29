utils::globalVariables(c("node", "Y", "p", "variable", "decImp", "splitLabel", "ID", "index")) # to avoid CRAN check errors for tidyverse programming

#' Explainable Ensemble Tree
#'
#' It creates an explainable tree for Random Forest. Explainable Ensemble Trees (E2Tree) aimed to generate a “new tree” that can explain and represent the relational structure between the response variable and the predictors. This lead to providing a tree structure similar to those obtained for a decision tree exploiting the advantages of a dendrogram-like output.
#'
#' @param formula is a formula describing the model to be fitted, with a response but no interaction terms.
#' @param data a data frame containing the variables in the model. It is a data frame in which to interpret the variables named in the formula.
#' @param D is the dissimilarity matrix. This is a dissimilarity matrix measuring the discordance between two observations concerning a given classifier of a random forest model. The dissimilarity matrix is obtained with the \link{createDisMatrix} function.
#' @param setting is a list containing the set of stopping rules for the tree building procedure.
#' \tabular{lll}{
#' \code{impTotal}\tab   \tab The threshold for the impurity in the node\cr
#' \code{maxDec}\tab   \tab The threshold for the maximum impurity decrease of the node\cr
#' \code{n}\tab   \tab The minimum number of the observations in the node \cr
#' \code{level}\tab   \tab The maximum depth of the tree (levels) \cr
#' \code{tMax}\tab   \tab The maximum number of terminal nodes\cr}
#' @param method The 'method' parameter specifies the method to be used for the analysis. There are two supported methods:
#' \tabular{lll}{
#' \code{classification}\tab   \tab Select this method if you want to perform a classification analysis. Classification is a supervised learning task where the goal is to assign input data to predefined categories or classes. The function will use algorithms tailored for classification tasks to build a model and make predictions based on the input data.\cr
#' \code{regression}\tab   \tab Choose this method if you want to perform a regression analysis. Regression is a supervised learning task that aims to predict continuous numerical values based on input data. The function will use regression algorithms to build a model that can estimate the relationship between variables and predict numeric outcomes.\cr}
#' Default is \code{setting=list(impTotal=0.1, maxDec=0.01, n=5, level=5, tMax=5)}.
#'
#' @return A e2tree object, which is a list with the following components:
#' \tabular{lll}{
#' \code{tree}\tab   \tab  A data frame representing the main structure of the tree aimed at explaining and graphically representing the relationships and interactions between the variables used to perform an ensemble method. \cr
#' \code{call}\tab   \tab The matched call\cr
#' \code{terms}\tab   \tab A list of terms and attributes \cr
#' \code{control}\tab   \tab A list containing the set of stopping rules for the tree building procedure  \cr
#' \code{varimp}\tab   \tab A list containing a table and a plot for the variable importance. Variable importance refers to a quantitative measure that assesses the contribution of individual variables within a predictive model towards accurate predictions. It quantifies the influence or impact that each variable has on the model's overall performance. Variable importance provides insights into the relative significance of different variables in explaining the observed outcomes and aids in understanding the underlying relationships and dynamics within the model \cr}
#'
#' #examples
#'
#' @export


e2tree <- function(formula, data, D, setting=list(impTotal=0.1, maxDec=0.01, n=5, level=5, tMax=5), method="classification"){

  Call <- match.call()
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())
  Terms <- attributes(mf)$terms

  response <- mf[,1]
  X <- mf[,-1]

  for (i in 1:setting$level) setting$tMax=setting$tMax*2+1

  # identify qualitative variable and the number of categories
  var_classes <- unlist(lapply(X,class))
  ind <- which(var_classes %in% c("factor","character"))
  ncat <- (apply(X[,ind],2, function(x) length(unique(x))))
  var_classes[names(ncat)] = ncat
  var_classes[!names(var_classes) %in% names(ncat)] = -1
  var_classes <- as.numeric(var_classes)
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
  m_label <- paste("V", seq(1,length(m)*2), sep="")
  labels <- c("node","n", "pred", "prob", "impTotal", "impChildren","decImp","decImpSur","variable" ,"split", "splitLabel", "variableSur", "splitLabelSur","parent","children","terminal", "obs", "path", "ncat",m_label)
  info <- data.frame(matrix(NA,setting$tMax,length(labels)))
  names(info) <- labels
  indv <- (ncol(info)-length(m_label)+1):ncol(info)

  N <- NULL

  while(length(nterm)>0){

    t <- tail(nterm,1)
    N <- c(N,t)
    print(t)
    index <- which(nodes == t)

        ### verifica regole di arresto
    results <- eStoppingRules(D,index, t, setting, response)

    ### Compute the response in the node
    res <- moda(response[index])
    ###

    info$pred[t] <- res[1]
    info$prob[t] <- res[2]
    info$node[t] <- t
    info$parent[t] <- floor(t/2)
    info$n[t] <- length(index)
    info$impTotal[t] <- results$impTotal

    ##### statistics
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

    ###

    if (isTRUE(results$sRule)){
      #list(imp=NA,decImp=NA, split=NA, splitLabel=NA, terminal=TRUE, impTotal=results$impTotal, obs=index, path=NA)
      info$terminal[t] <- TRUE
      #info$impTotal[t] <- results$impTotal
      info$obs[t] <- list(index)
      info$path[t] <- paths(info,t)

      #### aggiungere la predizione
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
      } else{
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
      }
    }
    #print(info$path[t])
    nterm <- setdiff(nterm,t)

  }
  info <- info %>% drop_na(node)
  info$pred_val <- as.numeric(factor(info$pred))
  info$variableSur <- gsub(" <=.*| %in%.*","",info$splitLabelSur)

  yval2 <- as.matrix(info[,indv])
  ypred <- info$pred_val
  nodeprob <- info$n/first(info$n)
  info <- info[,-indv]

  #info$yval2 <- cbind(ypred, yval2, nodeprob)
  yval2 <- cbind(ypred,yval2)
  #names(yval2) <- paste("V",seq(ncol(yval2)),sep="")
  attr(yval2,"dimnames")[[2]] <- paste("V",seq(ncol(yval2)),sep="")
  info$yval2 <- cbind(yval2, nodeprob)
  ylevels <- as.character(unique(response))
  row.names(info) <- info$node
  info <- info[as.character(N),]

  object <- csplit_str(info,X,ncat, call=Call, terms=Terms, control=setting, ylevels=ylevels)

  object$varimp <- vimp(object, data = data)
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

  var_lev <- attribute <- list()
  for (i in names(ncat)){
    var_lev[[i]] <- seq(ncat[i])
    names(var_lev[[i]]) <- unique(X[,i])
    attribute[[i]] <- as.character(unique(X[,i]))
  }

  #qualitative splits
  splits <- info %>%
    dplyr::filter(!is.na(variable)) %>%
    dplyr::select(n,ncat,variable,decImp, splitLabel)

  #splits <- info[!is.na(info$variable), c("n","ncat","variable","decImp", "splitLabel")]
  # index for numerical predictors
  varnumerics <- strsplit(splits$splitLabel[splits$ncat==-1],"<=")
  splits$index[splits$ncat==-1] <- unlist(lapply(varnumerics, function(x) x[2]))

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
