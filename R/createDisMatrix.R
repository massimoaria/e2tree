utils::globalVariables(c("resp", "W")) # to avoid CRAN check errors for tidyverse programming
#' Dissimilarity matrix
#'
#' The function createDisMatrix creates a dissimilarity matrix among observations from an ensemble tree.
#'
#' @param ensemble is an ensemble tree object (for the moment ensemble works only with random forest objects)
#' @param data a data frame containing the variables in the model. It is the data frame used for ensemble learning.
#' @param label is a character. It indicates the response label.
#' @param parallel to speed up your code. Run all the process in parallel on your laptop using your cores-1
#'
#' @return A dissimilarity matrix. This is a dissimilarity matrix measuring the discordance between two observations concerning a given classifier of a random forest model.
#'
#' @examples
#' \donttest{
#' ## Classification
#' data("iris")
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
#' D <- createDisMatrix(ensemble, data=training, label = "Species", parallel = FALSE)
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
#' D = createDisMatrix(ensemble, data=training, label = "mpg", parallel = FALSE)  
#' 
#' }
#' @export

createDisMatrix <- function(ensemble, data, label, parallel = FALSE) {
  row.names(data) <- NULL
  
  # Determine the type of the ensemble (e.g, regression or classification)
  type <- ensemble$type
  
  
  # Predict nodes or leaf indices based on the ensemble type
  switch(class(ensemble)[length(class(ensemble))],
         randomForest = {
           # If the ensemble is a random forest, get the terminal nodes for each tree
           obs <- as.data.frame(attr(predict(ensemble, newdata = data, nodes = TRUE), "nodes"))
           n_tree <- ensemble$ntree
         },
         xgb.Booster = 
           {
           # If the ensemble is an xgboost model, get the leaf indices for each tree
           obs <- as.data.frame(predict(ensemble, newdata = data_XGB, predleaf = TRUE))
           # Remove columns with all zeros
           obs <- obs[, colSums(obs) != 0L] #PROBABILMENTE NON SERVE, PERCHE NON HO PIU IL NUMERO MAX DI ALBERI PRODOTTI
           n_tree <- ensemble$niter
         })
  
  # Ensure data is a data.frame and the response is a factor
  class(data) <- "data.frame"
  if (!inherits(data[[label]], "factor")) {
    # Convert the response variable to a factor if it is not already
    data[[label]] <- factor(data[[label]])
  }
  
  
  
  # Add observation IDs and tree indices to the observations
  obs <- cbind(row.names(obs), obs)
  names(obs) <- c("OBS", paste("Tree", seq(1, (ncol(obs) - 1L)), sep = ""))
  row.names(obs) <- NULL
  
  nodes <- sort(unique(as.numeric(as.matrix(((obs %>%
                                                select(starts_with("Tree"))))))))
  
  
  
  # Add the response variable to the observations
  if (type == "classification") {
    # For classification, retain the factor response
    obs$resp <- data[as.numeric(obs$OBS), label]
  } else {
    # For regression, convert the response to numeric
    obs$resp <- as.numeric(as.character(data[obs$OBS, label]))
  }
  
  # Number of trees in the ensemble
  ntree <- ncol(obs) - 2L
  
  # Initialize matrices
  w <- matrix(NA, nrow(obs), ntree)  # Matrix to store weights
  a <- Matrix(0L, nrow(obs), nrow(obs), sparse = TRUE)  # Sparse matrix for co-occurrences
  
  # Register parallel backend
  if (parallel == TRUE) {
    # If parallel processing is enabled, use all minus one core
    no_cores <- detectCores() - 1L
    registerDoParallel(cores = no_cores)
  } else {
    # If not using parallel processing, use one core
    no_cores <- detectCores() - (max(detectCores()) - 1)  
    registerDoParallel(cores = no_cores)
  }
  
  
  ## Start the computation
  if (type == "classification") {
    cat("Classification Framework\n")
    
    # Progress bar setup
    # pb <- txtProgressBar(min = 0L, max = ensemble$ntree, style = 3L)
    pb <- txtProgressBar(min = 0L, max = n_tree, style = 3L)
    
    # Parallel computation
    #results <- foreach(i = seq_len(ensemble$ntree), .packages = c('dplyr', 'Matrix')) %dopar% {
    results <- foreach(i = seq_len(ntree), .packages = c('dplyr', 'Matrix')) %dopar% {
      cooccurrences(type, obs, w, i)
    }
    
    # Update progress bar and combine results
    for (i in seq_along(results)) {
      setTxtProgressBar(pb, i)
      a <- as.matrix(a + results[[i]])
    }
    close(pb)
    stopImplicitCluster()
    
  } else {
    cat("Regression Framework\n")
    #maxvar <- variance(obs$resp)
    maxvar <- diff(range(obs$resp))^2L / 9L
    #maxvar <- var(obs$resp)*(nrow(obs)-1)/nrow(obs) # population variance 
    
      
    # Progress bar setup
    # pb <- txtProgressBar(min = 0L, max = ensemble$ntree, style = 3L)
    pb <- txtProgressBar(min = 0L, max = n_tree, style = 3L)
    
    # Parallel computation
    #results <- foreach(i = seq_len(ensemble$ntree), .packages = c('dplyr', 'Matrix')) %dopar% {
    results <- foreach(i = seq_len(n_tree), .packages = c('dplyr', 'Matrix')) %dopar% {
      cooccurrences(type, obs, w, i, maxvar)
    }
    
    # Update progress bar and combine results
    for (i in seq_along(results)) {
      setTxtProgressBar(pb, i)
      a <- as.matrix(a + results[[i]])
    }
    close(pb)
    stopImplicitCluster()
  }
  
  # Similarity matrix
  ## a is the similarity matrix
  ## aa is the maximum similarity matrix
  aa <- diag(a)
  aa <- outer(aa, aa, "maxValue")
  a <- a / aa  # now a is scaled between 0 and 1
  
  # Dissimilarity matrix among observations (respect to the co-occurrences in the same node)
  dis <- 1L - a
  row.names(dis) <- colnames(dis) <- obs$OBS
  #dis <- as.matrix(dis)
  
  return(dis)
}


## Variance
variance <- function(x){
  sum((x-mean(x))^2)/length(x)
}



## Main function
cooccurrences <- function(type, obs, w, i, maxvar=NA) {
  # Group data based on the tree node corresponding to column (i + 1L)
  N <- nrow(obs)
  if (type == "classification") {
    R <- obs %>%
      group_by(pick(i + 1L)) %>%
      select(i + 1L, resp) %>%  # Select the node column and the response (resp)
      mutate(n=n(),
             freq = as.numeric(moda(resp)[2L])) %>%  # Find the mode of the responses and its frequency
      select(-resp, -n) %>%  # Remove the 'resp' and 'n' columns no longer needed
      distinct() %>%  
      as.data.frame()  # Convert the result to a data frame
  } else {
    R <- obs %>%
      group_by(pick(i + 1L)) %>%
      select(i + 1L, resp) %>%  # Select the node column and the response (resp)
      mutate(W = 1L - variance(resp) / (maxvar*length(resp)/N),  # Calculate weight based on variance
             W = if_else(W < 0L, 0L, W)
             ) %>%  # Ensure no negative weights
      select(-resp) %>%  # Remove the 'resp' and 'n' columns no longer needed
      distinct() %>%  
      replace_na(list(W=0)) %>%
      as.data.frame()  # Convert the result to a data frame
  }
  
  # Map the calculated weights to the corresponding column of the matrix w
  # w[, i] <- R[match(obs[, i + 1L], R[, 1L]), 2L]
  w[,i] <- R[as.numeric(factor(obs[,i+1L])),2L]
  
  # Perform garbage collection to free unused memory
  gc()  
  
  # Initialize a sparse matrix for co-occurrences
  co_occurrences <- Matrix(0L, nrow(obs), nrow(obs), sparse = TRUE)
  
  # Identify the unique node IDs
  node_ids <- unique(obs[, i + 1L])
  
  # For each unique node ID
  for (node in node_ids) {
    # Find the indices of the observations that belong to the current node
    indices <- which(obs[, i + 1L] == node)
    # Update the co-occurrence matrix with the corresponding weights
    co_occurrences[indices, indices] <- w[indices, i]
  }
  
  # Return the co-occurrence matrix
  return(co_occurrences)
}





maxValue <- function(x,y){
  apply(cbind(x,y),1L,max)
}
