#' Explainable Ensemble Tree
#'
#' It creates an explainable tree for Random Forest
#'
#' @param D is the dissimilarity matrix
#' @param X is the training data frame with only the predictors
#' @param response is the vector of responses of the training dataset
#' @param setting is a list containing the setting parameters for tree building procedure.
#' Default is \code{setting=list(impTotal=0.1, maxDec=0.01, n=5, level=5, tMax=5)}.
#'
#' @return a e2tree object.
#'
#' #examples
#'
#' @export

e2tree <- function(D, X, response, setting=list(impTotal=0.1, maxDec=0.01, n=5, level=5, tMax=5)){

  for (i in 1:setting$level) setting$tMax=setting$tMax*2+1


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
  labels <- c("node","n", "pred", "prob", "impTotal", "impChildren","decImp","decImpSur","variable" ,"split", "splitLabel", "variableSur", "splitLabelSur","parent","children","terminal", "obs", "path", m_label)
  info <- data.frame(matrix(NA,setting$tMax,length(labels)))
  names(info) <- labels
  indv <- (ncol(info)-length(m_label)+1):ncol(info)

  while(length(nterm)>0){

    t <- tail(nterm,1)
    print(t)
    index <- which(nodes == t)

        ### verifica regole di arresto
    results <- eStoppingRules(D,index, t, setting, response)

    ### Calcolo risposta nel nodo (per ora solo classificazione)
    res <- moda(response[index])
    ###

    info$pred[t] <- res[1]
    info$prob[t] <- res[2]
    info$node[t] <- t
    info$parent[t] <- floor(t/2)
    info$n[t] <- length(index)
    info$impTotal[t] <- results$impTotal

    ##### MODIFICO per ottenere le statistiche
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

        nodes[index] <- (nodes[index]*2+1)-S[index,s]
        nterm <- c(nterm, unique(nodes[index]))
      }
    }
    #print(info$path[t])
    nterm <- setdiff(nterm,t)

  }
  info <- info %>% drop_na(.data$node)
  info$pred_val <- as.numeric(factor(info$pred))
  info$variable <- gsub(" <=.*| %in%.*","",info$splitLabel)
  info$variableSur <- gsub(" <=.*| %in%.*","",info$splitLabelSur)

  #info$yval2.V1 <- info$pred

  yval2 <- as.matrix(info[,indv])
  ypred <- info$pred_val
  nodeprob <- info$n/first(info$n)
  info <- info[,-indv]

  info$yval2 <- cbind(ypred, yval2, nodeprob)

  return(info)
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





#do.call("rbind", lapply(info, "[[", 1))

### extract data using path
#attach(ToothGrowth)
#ToothGrowth[eval(parse(text=info[[9]]$path)),]



