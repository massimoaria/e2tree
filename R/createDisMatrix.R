utils::globalVariables(c("resp", "W", "data_XGB")) # to avoid CRAN check errors for tidyverse programming

#' Dissimilarity matrix
#'
#' The function createDisMatrix creates a dissimilarity matrix among
#' observations from an ensemble tree.
#'
#' @param ensemble is an ensemble tree object
#' @param data is a data frame containing the variables in the model. It is the
#'   data frame used for ensemble learning.
#' @param label is a character. It indicates the response label.
#' @param parallel A list with two elements: \code{active} (logical) and
#'   \code{no_cores} (integer). If \code{active = TRUE}, the function performs
#'   parallel computation using the number of cores specified in
#'   \code{no_cores}. If \code{no_cores} is NULL or equal to 0, it defaults to
#'   using all available cores minus one. If \code{active = FALSE}, the function
#'   runs on a single core. Default: \code{list(active = FALSE, no_cores = 1)}.
#' @param verbose Logical. If TRUE, the function prints progress messages and
#'   other information during execution. If FALSE (the default), messages are
#'   suppressed.
#'
#' @return A dissimilarity matrix. This is a dissimilarity matrix measuring the
#'   discordance between two observations concerning a given random forest
#'   model.
#' @details An `ensemble` is a trained object of one of these classes trained
#'   for *classification* or *regression* task:
#'
#' \itemize{
#'   \item `randomForest`
#'   \item `ranger`
#' }
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
#' ## "randomForest" package
#' ensemble <- randomForest::randomForest(Species ~ ., data=training,
#' importance=TRUE, proximity=TRUE)
#'
#' ## "ranger" package
#' ensemble <- ranger::ranger(Species ~ ., data = iris,
#' num.trees = 1000, importance = 'impurity')
#'
#' D <- createDisMatrix(ensemble, data=training,
#'                      label = "Species",
#'                      parallel = list(active=FALSE, no_cores = 1))
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
#' ## "randomForest" package
#' ensemble = randomForest::randomForest(mpg ~ ., data=training, ntree=1000,
#' importance=TRUE, proximity=TRUE)
#'
#' ## "ranger" package
#' ensemble <- ranger::ranger(formula = mpg ~ ., data = training,
#' num.trees = 1000, importance = "permutation")
#'
#' D = createDisMatrix(ensemble, data=training,
#'                         label = "mpg",
#'                        parallel = list(active=FALSE, no_cores = 1))
#'
#' }
#' @export

createDisMatrix <- function(
  ensemble,
  data,
  label,
  parallel = list(active = FALSE, no_cores = 1),
  verbose = FALSE
) {
  # === Input Validation ===

  # Check if 'ensemble' is NULL or not a supported model type
  if (is.null(ensemble)) {
    stop(
      "Error: 'ensemble' cannot be NULL. Please provide a trained randomForest or xgboost or ranger model."
    )
  }

  if (!inherits(ensemble, c("randomForest", "xgb.Booster", "ranger"))) {
    stop(
      "Error: 'ensemble' must be of class 'randomForest' or 'xgb.Booster' or 'ranger'"
    )
  }

  # Check if 'data' is a valid data frame
  if (!is.data.frame(data)) {
    stop("Error: 'data' must be a valid data frame.")
  }

  # Check if 'label' is a valid column in 'data'
  if (
    !is.character(label) || length(label) != 1 || !(label %in% colnames(data))
  ) {
    stop("Error: 'label' must be a valid column name in 'data'.")
  }

  # # Check if 'parallel' is a logical value
  # if (!is.logical(parallel) || length(parallel) != 1) {
  #   stop("Error: 'parallel' must be a logical (TRUE/FALSE) value.")
  # }

  row.names(data) <- NULL

  # Determine the type of the ensemble (e.g, regression or classification)
  if (inherits(ensemble, "ranger")) {
    type <- tolower(ensemble[["treetype"]])

    # additional check for ranger to ensure regression or classification
    allowed_ranger_model_types <- c("classification", "regression")
    if (!(type %in% allowed_ranger_model_types)) {
      stop("ranger model should be either a classification or regression type")
    }

    # additional check to ensure that ranger object is predictable
    if (is.null(ensemble$forest)) {
      stop("ranger model should be trained with `write.forest = TRUE`")
    }
  } else {
    type <- ensemble$type
  }

  # Predict nodes or leaf indices based on the ensemble type
  switch(
    class(ensemble)[length(class(ensemble))],
    randomForest = {
      # If the ensemble is a random forest, get the terminal nodes for each tree
      obs <- as.data.frame(attr(
        predict(ensemble, newdata = data, nodes = TRUE),
        "nodes"
      ))
      n_tree <- ensemble$ntree
    },
    xgb.Booster = {
      # If the ensemble is an xgboost model, get the leaf indices for each tree
      obs <- as.data.frame(predict(
        ensemble,
        newdata = data_XGB,
        predleaf = TRUE
      ))
      # Remove columns with all zeros
      obs <- obs[, colSums(obs) != 0L] #PROBABILMENTE NON SERVE, PERCHE NON HO PIU IL NUMERO MAX DI ALBERI PRODOTTI
      n_tree <- ensemble$niter
    },

    # get terminal nodes from ranger object
    ranger = {
      # obs is a data.frame with one column per tree
      # Each row corresponds to an observation
      # Creates an identical structure as 'randomForest'
      obs <-
        predict(
          ensemble,
          data,
          type = "terminalNodes",
          num.threads = 1 # not to interfere with parallelization
        ) %>%
        {
          .$predictions
        } %>%
        as.data.frame()

      n_tree <- ensemble$num.trees
    }
  )

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

  nodes <- sort(unique(as.numeric(as.matrix(
    ((obs %>%
      select(starts_with("Tree"))))
  ))))

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
  w <- matrix(NA, nrow(obs), ntree) # Matrix to store weights
  a <- Matrix(0L, nrow(obs), nrow(obs), sparse = TRUE) # Sparse matrix for co-occurrences

  # === Parallel Backend Registration ===
  if (isTRUE(parallel$active)) {
    # if not specified, no_cores -1
    if (is.null(parallel$no_cores) || parallel$no_cores < 1L) {
      no_cores <- max(1L, detectCores() - 1L)
    } else {
      no_cores <- parallel$no_cores
    }
    if (verbose) {
      message(paste0("Parallel mode ON (", no_cores, " cores)\n"))
    }
    registerDoParallel(cores = no_cores)
  } else {
    if (verbose) {
      message("Parallel mode OFF (1 core)\n")
    }
    registerDoParallel(cores = 1L)
  }

  if (verbose) {
    pb <- txtProgressBar(min = 0L, max = n_tree, style = 3L)
  }

  ## Start the computation
  if (type == "classification") {
    if (verbose) {
      message("Classification Framework\n")
    }

    # Parallel computation
    #results <- foreach(i = seq_len(ensemble$ntree), .packages = c('dplyr', 'Matrix')) %dopar% {
    results <- foreach(i = seq_len(ntree), .packages = c("Rcpp")) %dopar%
      {
        compute_cooccurrences_cpp(type, obs, w, i)
      }

    # Update progress bar and combine results
    for (i in seq_along(results)) {
      if (verbose) {
        setTxtProgressBar(pb, i)
      }
      a <- as.matrix(a + results[[i]])
    }
    stopImplicitCluster()
  } else {
    if (verbose) {
      message("Regression Framework\n")
    }

    # Parallel computation
    #results <- foreach(i = seq_len(ensemble$ntree), .packages = c('dplyr', 'Matrix')) %dopar% {
    results <- foreach(i = seq_len(ntree), .packages = c("Rcpp")) %dopar%
      {
        compute_cooccurrences_cpp(
          type,
          obs,
          w,
          i,
          maxvar = diff(range(obs$resp))^2 / 9L
        )
      }

    # Update progress bar and combine results
    for (i in seq_along(results)) {
      if (verbose) {
        setTxtProgressBar(pb, i)
      }
      a <- as.matrix(a + results[[i]])
    }
    stopImplicitCluster()
  }

  if (verbose) {
    close(pb)
  }

  # Similarity matrix
  ## a is the similarity matrix
  ## aa is the maximum similarity matrix
  aa <- diag(a)
  aa <- outer(aa, aa, "maxValue")
  a <- a / aa # now a is scaled between 0 and 1

  # Dissimilarity matrix among observations (respect to the co-occurrences in the same node)
  dis <- 1L - a
  row.names(dis) <- colnames(dis) <- obs$OBS
  #dis <- as.matrix(dis)

  return(dis)
}


## Variance
variance <- function(x) {
  sum((x - mean(x))^2) / length(x)
}


maxValue <- function(x, y) {
  apply(cbind(x, y), 1L, max)
}
