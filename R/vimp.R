utils::globalVariables(c("decImp", "n", "pred", "variable", "Nimp", "vimp", "decProb", "pimp",
                         "Pimp", "Nimp", "vimp_resp", "vimp_prob", "Variable", "MeanImpurityDecrease", "MeanAccuracyDecrease")) # to avoid CRAN check errors for tidyverse programming

#' Variable Importance
#'
#' It calculate variable importance of an explainable tree
#'
#' @param fit is a e2tree object
#' @param data is a data frame in which to interpret the variables named in the formula.
#' @param type Specify the type. The default is ‘classification’, otherwise ‘regression’.
#'
#' @return a data frame containing variable importance metrics.
#'
#
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
#' ensemble <- randomForest::randomForest(Species ~ ., data=training, 
#' importance=TRUE, proximity=TRUE)
#' 
#' D <- createDisMatrix(ensemble, data=training, label = "Species", 
#'                              parallel = list(active=FALSE, no_cores = 1))
#' 
#' setting=list(impTotal=0.1, maxDec=0.01, n=2, level=5)
#' tree <- e2tree(Species ~ ., training, D, ensemble, setting)
#'
#' vimp(tree, training, type = "classification")
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
#' ensemble = randomForest::randomForest(mpg ~ ., data=training, ntree=1000, 
#' importance=TRUE, proximity=TRUE)
#' 
#' D = createDisMatrix(ensemble, data=training, label = "mpg", 
#'                          parallel = list(active=FALSE, no_cores = 1))  
#' 
#' setting=list(impTotal=0.1, maxDec=(1*10^-6), n=2, level=5)
#' tree <- e2tree(mpg ~ ., training, D, ensemble, setting)
#' 
#' vimp(tree, training, type = "regression")
#' 
#' }
#'
#' @export
#'
vimp <- function(fit, data, type = "classification") {

     # === Input Validation ===
  
  # Validate 'fit' (must be an 'e2tree' object)
  if (!inherits(fit, "e2tree")) {
    stop("Error: 'fit' must be an 'e2tree' object.")
  }
  
  # Validate 'data' (must be a non-empty data frame)
  if (!is.data.frame(data) || nrow(data) == 0) {
    stop("Error: 'data' must be a non-empty data frame.")
  }
  
  # Validate 'type' (must be either 'classification' or 'regression')
  if (!type %in% c("classification", "regression")) {
    stop("Error: 'type' must be either 'classification' or 'regression'.")
  }
  
  # Validate 'fit$tree' (must be a data frame)
  if (!is.data.frame(fit$tree)) {
    stop("Error: 'fit$tree' must be a data frame.")
  }
  
  # Validate that 'fit$terms' exists
  if (is.null(fit$terms)) {
    stop("Error: 'fit$terms' is missing or NULL in the e2tree object.")
  }
  
  data <- as.data.frame(data)
  row.names(data) <- NULL
  tree <- fit$tree
  response_var <- names(attr(fit$terms, "dataClasses"))[1]
  
  # Validate that response_var exists in data
  if (!response_var %in% colnames(data)) {
    stop("Error: The response variable from 'fit' is not found in 'data'.")
  }

  # === Proceed with the function  ===

  data <- as.data.frame(data)
  row.names(data) <- NULL
  tree <- fit$tree
  response_var <- names(attr(fit$terms, "dataClasses"))[1]
  response <- data[[response_var]]
  
  # Initialize vimp table
  vimp_table <- data.frame(variable = character(), 
                           MeanImpurityDecrease = numeric(), 
                           stringsAsFactors = FALSE)
  
  # Check if classification or regression
  if (type == "classification") {
    # Classification logic
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
      summarize(vimp = sum(Nimp), .groups = "drop") %>%
      drop_na(variable) %>%
      pivot_wider(names_from = pred, values_from = vimp)
    
    names(vimp_resp)[-1] <- paste("ImpDec_",names(vimp_resp)[-1])
    
    vimp_prob <- tree %>%
      mutate(Pimp = decProb*n/n[1]) %>%
      group_by(variable,pred) %>%
      summarize(pimp = sum(Pimp), .groups = "drop") %>%
      drop_na(variable) %>%
      pivot_wider(names_from = pred, values_from = pimp)
    
    names(vimp_prob)[-1] <- paste("AccDec_",names(vimp_prob)[-1])
    
    vimp <- tree %>%
      mutate(Nimp = decImp*n/n[1],
             Pimp = decProb*n/n[1]) %>%
      group_by(variable) %>%
      summarize(vimp = sum(Nimp, na.rm=TRUE),
                pimp = sum(Pimp, na.rm=TRUE), .groups = "drop") %>%
      drop_na(variable) %>%
      # mutate(vimpNorm = .data$vimp/sum(.data$vimp)*100,
      #        pimpNorm = .data$pimp/sum(.data$pimp)*100) %>%
      arrange(desc(vimp), by_group=FALSE) %>%
      left_join(vimp_resp, by = "variable") %>%
      left_join(vimp_prob, by = "variable")
    
    names(vimp)[1:3] <- c("Variable","MeanImpurityDecrease","MeanAccuracyDecrease")
    
    vimp_table <- vimp_resp
  } else if (type == "regression") {
    # Regression logic
    tree$variance <- as.numeric(tree$prob) # Variance at nodes
    t <- row.names(tree)[tree$terminal == FALSE]
    tL <- as.character(tree[t, "node"] * 2)
    tR <- as.character(tree[t, "node"] * 2 + 1)
    tree[t, "varianceChildren"] <- (tree[tL, "variance"] * tree[tL, "n"] / tree[t, "n"]) +
      (tree[tR, "variance"] * tree[tR, "n"] / tree[t, "n"])
    
    # Calculate decrease in variance
    tree[t, "decVariance"] <- tree[t, "variance"] - tree[t, "varianceChildren"]
    
    vimp_table <- tree %>%
      mutate(Nimp = decImp * n / n[1]) %>%
      group_by(variable) %>%
      summarize(MeanImpurityDecrease = sum(Nimp, na.rm = TRUE), .groups = "drop") %>%
      drop_na(variable)
  }
  
  # Add ggplot visualizations
  if (type == "classification") {
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
  } else if (type == "regression"){
    pImp <- ggplot(vimp_table, aes(y = variable, x = MeanImpurityDecrease)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      scale_y_discrete(limits = rev(vimp_table$variable)) +
      labs(title = "Variable Importance Plot", 
           x = "Mean Decrease in Impurity", 
           # Mean Decrease in Impurity is a measure of the importance of a variable in a model based on decision trees (e.g. Random Forest, Decision Trees, Gradient Boosting). 
           # This value is based on the reduction in impurity that a variable produces when it is used to partition the data into a node of a decision tree. 
           # For a regression problem, impurity is typically measured through the mean square error (MSE).
           y = "Variable") +
      theme_minimal()
    
    return(list(vimp = vimp_table, g_imp = pImp))
  }
}