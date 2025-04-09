#' Predict responses through an explainable RF
#'
#' It predicts classification and regression tree responses
#'
#' @param fit is a e2tree object
#' @param data is a data frame
#' @param target is the target value of response in the classification case
#'
#' @return an object.
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
#' ePredTree(tree, validation, target="1")
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
#'                               parallel = list(active=FALSE, no_cores = 1))  
#' 
#' setting=list(impTotal=0.1, maxDec=(1*10^-6), n=2, level=5)
#' tree <- e2tree(mpg ~ ., training, D, ensemble, setting)
#' 
#' ePredTree(tree, validation)
#' 
#' }
#'
#' @export
#'
ePredTree <- function(fit, data, target="1") {
  tree <- fit$tree
  
  row.names(data) <- NULL
  data$nodeIndex <- 1:nrow(data)
  
  # Initialize the prediction dataframe
  pred <- data.frame(fit = rep(NA, nrow(data)), accuracy = NA, score = NA, row.names = 1:nrow(data))
  
  # Determine if the model is classification or regression
  is_classification <- !is.numeric(tree$pred)  # If 'pred' contains factors, it's classification
  
  # Extract paths of terminal nodes
  paths_list <- tree[tree$terminal == TRUE, c("node", "path", "pred", "prob")]
  
  if (is_classification) {
    # Convert probability columns to numeric
    paths_list$prob <- paths_list$score <- as.numeric(paths_list$prob)
    
    # Adjust scores for classification (probability of the target class)
    ind <- which(paths_list$pred != target)
    paths_list$score[ind] <- 1 - paths_list$score[ind]
  } else {
    # No probability scores in regression
    paths_list$score <- NA  
  }
  
  # Identify observations belonging to terminal nodes and assign predictions
  for (i in 1:nrow(paths_list)) {
    path <- paths_list[i, "path"]
    
    # Filter data based on the path condition
    Xn <- data %>% dplyr::filter(eval(parse(text = path)))
    index <- Xn$nodeIndex
    
    # Assign predictions
    pred[index, "fit"] <- paths_list[i, "pred"]
    
    if (is_classification) {
      pred[index, "accuracy"] <- paths_list[i, "prob"]
      pred[index, "score"] <- paths_list[i, "score"]
    } 
  }
  
  return(pred)
}



