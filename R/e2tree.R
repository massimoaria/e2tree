#' Explainable Ensemble Tree
#'
#' It creates an explainable tree for Random Forest
#'
#' @param formula is a formula, with a response but no interaction terms.
#' @param data is a data frame in which to interpret the variables named in the formula.
#' @param D is the dissimilarity matrix.
#' @param setting is a list containing the setting parameters for tree building procedure.
#' Default is \code{setting=list(impTotal=0.1, maxDec=0.01, n=5, level=5, tMax=5)}.
#'
#' @return a e2tree object.
#'
#' #examples
#'
#' @export

e2tree <- function(formula, data, D, setting=list(impTotal=0.1, maxDec=0.01, n=5, level=5, tMax=5)){

  Call <- match.call()
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())
  #Terms <- attr(mf, "terms")
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
  info <- info %>% drop_na(.data$node)
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

  object$varimp <- vimp(object$tree, response, X)
  object$N <- N

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
