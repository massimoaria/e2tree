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
#' \dontrun{
#' ## Classification
#' data(iris)
#'
#' # Create training and validation set:
#' data_set_size <- floor(nrow(iris)/2)
#' indexes <- sample(1:nrow(iris), size = data_set_size)
#' training <- iris[indexes,]
#' validation <- iris[-indexes,]
#' response_training <- training[,5]
#' response_validation <- validation[,5]
#'
#' # Perform training:
#'
#' require(randomForest)
#' ensemble <- randomForest(Species ~ ., data=training, importance=TRUE, proximity=TRUE)
#' D <- createDisMatrix(ensemble, data=data, label = "Species", parallel = TRUE)
#'
#'
#' # Regression
#' data("mtcars")
#'
#' require(randomForest)
#' ensemble = randomForest(mpg ~ ., data=mtcars, ntree=1000, importance=TRUE, proximity=TRUE)
#' D = createDisMatrix(ensemble, data=data, label = "mpg", parallel = TRUE)
#' }
#' @export

createDisMatrix <- function(ensemble, data, label, parallel = FALSE){


  switch(class(ensemble)[length(class(ensemble))],
         randomForest={
           obs <- as.data.frame(attr(predict(ensemble, newdata = data, nodes = TRUE),"nodes"))
         },
         xgb.Booster={
           obs <- as.data.frame(predict(ensemble, newdata = data, predleaf = TRUE))
           ind <- which(colSums(obs)==0)
           obs <- obs[,-ind]
         })


  # check if data is a data.frame
  class(data) <- "data.frame"

  #if (!inherits(data,"data.frame")){
  #  data <- data.frame(data)
  #}
  # check if response has a factor class
  if (!inherits(data[[label]],"factor")){
    data[[label]] <- factor(data[[label]])
  }

  # save the type of the framework
  type = ensemble$type


  # Start
  obs <- as.data.frame(attr(predict(ensemble, newdata = data, nodes = TRUE),"nodes"))
  obs <- cbind(row.names(obs), obs)
  names(obs) <- c("OBS",paste("Tree",seq(1,(ncol(obs)-1L)), sep=""))
  row.names(obs)=NULL

  nodes <- sort(unique(as.numeric(as.matrix(((obs %>%
                                                select(starts_with("Tree"))))))))

  # response
  if (type=="regression"){
    obs$resp <- as.numeric(as.character(data[obs$OBS,label]))
  } else {
    obs$resp <- data[as.numeric(obs$OBS),label]
  }

  # n. of trees
  ntree <- ncol(obs)-2L

  ## Correct-classification rate matrix (Obs x Trees)
  w <- matrix(NA,nrow(obs),ntree)
  a <- Matrix(0, nrow(obs),nrow(obs), sparse = TRUE)

  ####### Register the parallel backend to use
  if (parallel == TRUE) {
  no_cores <- detectCores() - 1L  # Leave one core free for system processes
  registerDoParallel(cores = no_cores)
  } else {
    no_cores <- detectCores() - (max(detectCores()) - 1)  # Leave one core free for system processes
    registerDoParallel(cores = no_cores)
  }


    # if loop for Classification or Regression
    if (type=="classification"){
      cat(noquote("Classification Framework"), '\n')

        # progress bar
        pb <- txtProgressBar(min = 0L, max = ensemble$ntree, initial = 0L, style = 3)

        results <- foreach(i=seq_len(ensemble$ntree),.packages='dplyr') %dopar% {
        R <- obs %>% group_by(pick(i+1L)) %>%
          select(i+1L, resp) %>%
          mutate(n = n(),
                 freq = as.numeric(moda(resp)[2L])) %>%
          select(-resp, -n) %>%
          distinct() %>%
          as.data.frame()
        w[,i] <- R[as.numeric(factor(obs[,i+1L])),2L]
        # Calculate weighted co-occurrences among the observations
        (outer(obs[,i+1L],obs[,i+1L],"==")+0L)*w[,i]
        }

        for(i in seq_along(results)) {
          setTxtProgressBar(pb, i)
        }
        close(pb)
        stopImplicitCluster()
        a = Reduce("+", results)

    } else {
      cat(noquote("Regression Framework"), '\n')

      maxvar <- diff(range(obs$resp))^2/9

      # progress bar
        pb <- txtProgressBar(min = 0L, max = ensemble$ntree, initial = 0L, style = 3)

        results <- foreach(i=seq_len(ensemble$ntree),.packages='dplyr') %dopar% {
        R <- obs %>% group_by(pick(i+1L)) %>%
          select(i+1L, resp) %>%
          mutate(W = 1L-var(resp)/maxvar,
                 W = if_else(W < 0L, 0L, W)) %>% # if there are some negative value, type 0
          select(-resp) %>%
          distinct() %>%
          replace_na(list(W=0)) %>%
          as.data.frame()
        w[,i] <- R[as.numeric(factor(obs[,i+1L])),2L]
        # Calculate weighted co-occurrences among the observations
        (outer(obs[,i+1L],obs[,i+1L],"==")+0L)*w[,i]
      }

    for(i in seq_along(results)) {
      setTxtProgressBar(pb, i)
    }
    close(pb)
    stopImplicitCluster()
    a = Reduce("+", results)
    }

  ## a is the similarity matrix

  ## aa is the maximum similarity matrix
  aa=diag(a)

  aa=outer(aa,aa,"maxValue")

  a <- a/aa # now a is scaled between 0 and 1

  ## Dissimilarity matrix among observations (respect to the co-occurrences in the same node)
  dis <- 1L-a
  row.names(dis)=colnames(dis)=obs$OBS

  return(dis)
}

 maxValue <- function(x,y){
  apply(cbind(x,y),1L,max)
}
