utils::globalVariables(c("resp")) # to avoid CRAN check errors for tidyverse programming

#' Dissimilarity matrix
#'
#' The function createDisMatrix creates a dissimilarity matrix among observations from an ensemble tree.
#'
#' @param ensemble is an ensemble tree object (for the moment ensemble works only with random forest objects)
#' @param data a data frame containing the variables in the model. It is the data frame used for ensemble learning.
#'
#' @return A dissimilarity matrix. This is a dissimilarity matrix measuring the discordance between two observations concerning a given classifier of a random forest model.
#'
#' @examples
#'
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
#' require(randomForest)
#' rf = randomForest(Species ~ ., data=training, ntree=1000, mtry=2, importance=TRUE, keep.inbag=TRUE, proximity=TRUE)
#' D <- createDisMatrix(rf, data=training)
#'
#' @export

createDisMatrix <- function(ensemble, data){

  obs <- as.data.frame(attr(predict(ensemble, newdata = data, nodes = TRUE),"nodes"))
  obs <- cbind(row.names(obs), obs)
  names(obs) <- c("OBS",paste("Tree",seq(1,(ncol(obs)-1)), sep=""))
  row.names(obs)=NULL

  nodes <- sort(unique(as.numeric(as.matrix(((obs %>%
                                                select(starts_with("Tree"))))))))

  # response
  obs$resp <- ensemble$y

  ## Correct-classification rate matrix (Obs x Trees)
  w <- matrix(NA,nrow(obs),ensemble$ntree)
  a <- Matrix(0, nrow(obs),nrow(obs), sparse = TRUE)
  for (i in seq_len(ensemble$ntree)){
    if ((i %% 100)==0) cat("\nAnalized ", (i), " trees")
    # Correct classification rate for all terminal nodes of the i-th tree
    # R <- obs %>% group_by_at(i+1) %>%
    #   mutate(n = n()) %>%
    #   summarize(freq = sort(table(resp), decreasing=T)[1]/n) %>%
    #   slice(1) %>% as.data.frame()
    R <- obs %>% group_by(pick(i+1)) %>%
      select(i+1, resp) %>%
      mutate(n = n(),
             freq = as.numeric(moda(resp)[2])) %>%
      select(-resp, -n) %>%
      distinct() %>%
      as.data.frame()
    w[,i] <- R[as.numeric(factor(obs[,i+1])),2]
    # Calculate weighted co-occurrences among the observations
    a <- a+((outer(obs[,i+1],obs[,i+1],"==")+0)*w[,i])
  }


  ## a is the similarity matrix

  ## aa is the maximum similarity matrix
  aa=diag(a)

  aa=outer(aa,aa,"maxValue")

  a <- a/aa # now a is scaled between 0 and 1

  ## Dissimilarity matrix among observations (respect to the co-occurrences in the same node)
  dis <- 1-a
  row.names(dis)=colnames(dis)=obs$OBS

  return(dis)
}

maxValue <- function(x,y){
  apply(cbind(x,y),1,max)
}

