utils::globalVariables(c("decImp", "n", "pred", "variable", "Nimp", "vimp", "decProb", "pimp",
                         "Pimp", "Nimp", "vimp_resp", "vimp_prob", "Variable", "MeanImpurityDecrease", "MeanAccuracyDecrease")) # to avoid CRAN check errors for tidyverse programming

#' Variable Importance
#'
#' It calculate variable importance of an explainable tree
#'
#' @param fit is a e2tree object
#' @param data is a data frame in which to interpret the variables named in the formula.
#'
#' @return a data frame containing variable importance metrics.
#'
#'
#'
#' #examples
#'
#' @export
#'
vimp <- function(fit, data){

  row.names(data) <- NULL
  tree <- fit$tree
  variables <- names(attr(fit$terms, "dataClasses"))[1]
  response <- data[,variables]
  ## Calcolo decrease of Accuracy ##
  row.names(tree) <- tree$node
  tree$prob <- as.numeric(tree$prob)
  t <- row.names(tree)[tree$terminal==FALSE]
  tL <- as.character(tree[t,"node"]*2)
  tR <- as.character(tree[t,"node"]*2+1)
  tree[t,"probChildren"] <- (tree[tL,"prob"]*tree[tL,"n"]/tree[t,"n"]) + (tree[tR,"prob"]*tree[tR,"n"]/tree[t,"n"])
  #attach(X)
  names(response) <- row.names(data)
  for (i in t){
    obs <- row.names(data)[unlist(tree[i,"obs"])]
    path <- tree[i,"splitLabelSur"]
    x <- data[obs,]
    indL <- obs[eval(parse(text=paste("x$",path)))]
    indR <- setdiff(obs,indL)
    probL <- as.numeric(moda(response[indL])[2])
    probR <- as.numeric(moda(response[indR])[2])
    tree[i,"probChildrenSur"] <- (probL*length(indL)/tree[i,"n"]) + (probR*length(indR)/tree[i,"n"])
  }


  tree[t,"decProb"] <- tree[t,"probChildren"]-tree[t,"probChildrenSur"]


  vimp_resp <- tree %>%
    mutate(Nimp = decImp*n/n[1]) %>%
    group_by(variable,pred) %>%
    summarize(vimp = sum(Nimp)) %>%
    drop_na(variable) %>%
    pivot_wider(names_from = pred, values_from = vimp)

  names(vimp_resp)[-1] <- paste("ImpDec_",names(vimp_resp)[-1])

  vimp_prob <- tree %>%
    mutate(Pimp = decProb*n/n[1]) %>%
    group_by(variable,pred) %>%
    summarize(pimp = sum(Pimp)) %>%
    drop_na(variable) %>%
    pivot_wider(names_from = pred, values_from = pimp)

  names(vimp_prob)[-1] <- paste("AccDec_",names(vimp_prob)[-1])

  vimp <- tree %>%
    mutate(Nimp = decImp*n/n[1],
           Pimp = decProb*n/n[1]) %>%
    group_by(variable) %>%
    summarize(vimp = sum(Nimp, na.rm=TRUE),
              pimp = sum(Pimp, na.rm=TRUE)) %>%
    drop_na(variable) %>%
    # mutate(vimpNorm = .data$vimp/sum(.data$vimp)*100,
    #        pimpNorm = .data$pimp/sum(.data$pimp)*100) %>%
    arrange(desc(vimp), by_group=FALSE) %>%
    left_join(vimp_resp, by = "variable") %>%
    left_join(vimp_prob, by = "variable")

  names(vimp)[1:3] <- c("Variable","MeanImpurityDecrease","MeanAccuracyDecrease")

  # Minimal theme + blue fill color
    pImp <-ggplot(vimp, aes(y=Variable, x=MeanImpurityDecrease)) +
      geom_bar(stat="identity", fill="steelblue") +
      scale_y_discrete(limits = rev(vimp$Variable))+
      labs(title="Variable Importance Plot", x = "Mean Impurity Decrease", y = "Variance") +
      theme_minimal()

    pAcc <-vimp %>%
      arrange(desc(MeanAccuracyDecrease), by_group=FALSE)
    pAcc <- pAcc %>%
      ggplot(aes(y=Variable, x=.data$MeanAccuracyDecrease)) +
      geom_bar(stat="identity", fill="steelblue") +
      scale_y_discrete(limits = rev(pAcc$Variable))+
      labs(title="Variable Importance Plot", x = "Mean Accuracy Decrease", y = "Variance") +
      theme_minimal()

  res <- list(vimp=vimp, g_imp=pImp, g_acc=pAcc)
  return(res)
}
