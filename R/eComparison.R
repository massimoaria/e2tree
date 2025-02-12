utils::globalVariables("tree") # to avoid CRAN check errors for tidyverse programming

#' Comparison of Heatmaps and Mantel Test
#'
#' This function processes heatmaps for visual comparison and performs the Mantel testbetween a dissimilarity matrix derived from Random Forest outputs and a matrix estimated 
#' by E2Tree. Heatmaps are generated for both matrices. The Mantel test quantifies the correlation between the matrices, offering a statistical measure of similarity.
#'
#' @param data a data frame containing the variables in the model. It is the data frame used for ensemble learning.
#' @param fit is e2tree object.
#' @param D is the dissimilarity matrix. This is a dissimilarity matrix measuring the discordance between two observations concerning a given classifier of a random forest model. The dissimilarity matrix is obtained with the \link{createDisMatrix} function.
#'
#' @return A list containing three elements:
#'   \itemize{
#'     \item \code{RF HeatMap}: A heatmap plot of the Random Forest-derived dissimilarity matrix.
#'     \item \code{E2Tree HeatMap}: A heatmap plot of the E2Tree-estimated matrix.
#'     \item \code{Mantel Test}: Results of the Mantel test, including the correlation coefficient and significance level.
#'   }
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
#' D <- createDisMatrix(ensemble, data=training, label = "Species", parallel = FALSE)
#' 
#' setting=list(impTotal=0.1, maxDec=0.01, n=2, level=5)
#' tree <- e2tree(Species ~ ., training, D, ensemble, setting)
#'
#' eComparison(training, tree, D)
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
#' setting=list(impTotal=0.1, maxDec=(1*10^-6), n=2, level=5)
#' tree <- e2tree(mpg ~ ., training, D, ensemble, setting)
#' 
#' 
#' }
#'
#' @export

# Define a function to process heatmaps and perform Mantel test
# The comparison is between the heatmap of the matrix O obtained from the RF output and the heatmap of the matrix O estimated by E2Tree
eComparison <- function(data, fit, D, graph = TRUE) {
  # === Input Validation ===
  
  # Validate 'data'
  if (!is.data.frame(data) || nrow(data) == 0) {
    stop("Error: 'data' must be a non-empty data frame.")
  }
  
  # Validate 'fit' (must be an 'e2tree' object)
  if (!inherits(fit, "e2tree")) {
    stop("Error: 'fit' must be an 'e2tree' object.")
  }
  
  # Validate 'D' (dissimilarity matrix)
  if (!is.matrix(D) || nrow(D) != ncol(D)) {
    stop("Error: 'D' must be a square dissimilarity matrix.")
  }
  
  # Ensure number of rows in 'data' matches the dimension of 'D'
  if (nrow(data) != nrow(D)) {
    stop("Error: The number of rows in 'data' must match the dimensions of 'D'.")
  }
  
  # === Proceed with the function ===
  
  # Get the number of observations in the data
  n <- nrow(data)
  
  # Extract the tree structure from the tree object
  df <- fit$tree
  
  # Identify terminal nodes in the tree
  terminal_nodes <- df$node[df$terminal]
  
  # Initialize a matrix to store probabilities
  Ps <- matrix(0, n, n)
  
  # Populate the probability matrix Ps based on classification or regression
  for (i in terminal_nodes) {
    # Extract observations corresponding to the current terminal node
    obs <- eval(parse(text = df$obs[df$node == i]))
    
    # Populate Ps using the appropriate column based on the task type
    if (!is.null(df$prob)) {
      # Assign the probability of the current terminal node to the respective cells
      Ps[obs, obs] <- df$prob[df$node == i]
    } else {
      Ps[obs, obs] <- df$Wt[df$node == i]
    }
  }
  
  # Set diagonal elements of Ps to 1
  diag(Ps) <- 1
  
  # Assign row and column names to the Ps matrix
  rownames(Ps) <- 1:nrow(Ps)
  colnames(Ps) <- 1:ncol(Ps)
  
  # Use the provided O matrix
  D_exp <- D
  
  # Perform hierarchical clustering on the O matrix
  clusD <- hclust(as.dist(D_exp))
  
  # Extract the order of observations based on clustering
  order <- clusD$order
  
  # Reorder the Ps matrix based on the clustering order
  Ps_ord <- Ps[order, order]
  
  # Update row and column names to reflect the new order
  rownames(Ps_ord) <- order
  colnames(Ps_ord) <- order
  
  # Create a black-and-white color palette for the heatmaps
  bw_palette <- colorRampPalette(c("white", "black"))(100)
  
  
  # Perform Mantel test between the two matrices
  mantel_test <- ape::mantel.test(
    Ps_ord, 
    1 - D_exp[order, order], 
    graph = graph, 
    main = "Mantel test"
  )
  
  if (graph){
    # Save the E2Tree heatmap as an object
    heatmap(
      sqrt(Ps_ord), 
      Rowv = NA, 
      Colv = NA, 
      scale = "none", 
      col = bw_palette,
      main = "E2Tree Heatmap"
    )
    
    # Save the Random Forest heatmap as an object
    heatmap(
      sqrt(1 - D_exp[order, order]), 
      Rowv = NA, 
      Colv = NA, 
      scale = "none", 
      col = bw_palette,
      main = "Ensemble Heatmap"
    )
  }
  
  
  
  # Return only the Mantel test result and heatmaps
  return(list(mantel_test = mantel_test))
}
