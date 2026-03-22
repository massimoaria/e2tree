# ===========================================================================
# S3 methods for the "e2tree" class
# ===========================================================================

#' Print an E2Tree model
#'
#' Displays a compact summary of the fitted E2Tree model including
#' task type, tree size, terminal nodes, and splitting variables.
#'
#' @param x An e2tree object
#' @param ... Additional arguments (ignored)
#'
#' @method print e2tree
#' @export
print.e2tree <- function(x, ...) {

  tree_info <- x$tree
  is_class <- !is.null(attr(x, "ylevels"))
  resp <- all.vars(x$terms)[1]
  preds <- all.vars(x$terms)[-1]

  n_nodes <- nrow(tree_info)
  n_terminal <- sum(tree_info$terminal)
  n_obs <- tree_info$n[1]  # root node contains total observations

  # Max depth
  max_depth <- max(vapply(tree_info$node, function(nd) {
    d <- 0L; while (nd > 1L) { nd <- nd %/% 2L; d <- d + 1L }; d
  }, integer(1)))

  # Splitting variables used
  split_vars <- unique(tree_info$variable[!is.na(tree_info$variable)])

  cat("\n")
  cat("  Explainable Ensemble Tree (E2Tree)\n")
  cat("  -----------------------------------\n")
  cat(sprintf("  Task:            %s\n", if (is_class) "Classification" else "Regression"))
  cat(sprintf("  Response:        %s\n", resp))
  cat(sprintf("  Predictors:      %d (%s)\n", length(preds),
              paste(head(preds, 5), collapse = ", ")))
  if (length(preds) > 5) cat(sprintf("                   ... and %d more\n", length(preds) - 5))
  cat(sprintf("  Observations:    %d\n", n_obs))
  cat(sprintf("  Nodes:           %d (total), %d (terminal)\n", n_nodes, n_terminal))
  cat(sprintf("  Max depth:       %d\n", max_depth))
  cat(sprintf("  Split variables: %s\n", paste(split_vars, collapse = ", ")))

  if (is_class) {
    cat(sprintf("  Classes:         %s\n", paste(attr(x, "ylevels"), collapse = ", ")))
  }

  cat("\n")
  invisible(x)
}


#' Summary of an E2Tree model
#'
#' Displays a comprehensive summary including tree structure, decision rules,
#' terminal node statistics, and variable importance.
#'
#' @param object An e2tree object
#' @param ... Additional arguments (ignored)
#'
#' @method summary e2tree
#' @export
summary.e2tree <- function(object, ...) {

  tree_info <- object$tree
  is_class <- !is.null(attr(object, "ylevels"))
  resp <- all.vars(object$terms)[1]

  n_nodes <- nrow(tree_info)
  n_terminal <- sum(tree_info$terminal)
  n_obs <- tree_info$n[1]  # root node

  max_depth <- max(vapply(tree_info$node, function(nd) {
    d <- 0L; while (nd > 1L) { nd <- nd %/% 2L; d <- d + 1L }; d
  }, integer(1)))

  split_vars <- unique(tree_info$variable[!is.na(tree_info$variable)])

  cat("\n")
  cat(paste(rep("=", 70), collapse = ""), "\n")
  cat("                    E2TREE MODEL SUMMARY\n")
  cat(paste(rep("=", 70), collapse = ""), "\n\n")

  # --- Model info ---
  cat("MODEL INFORMATION\n")
  cat(paste(rep("-", 40), collapse = ""), "\n")
  cat(sprintf("  Task:              %s\n", if (is_class) "Classification" else "Regression"))
  cat(sprintf("  Response:          %s\n", resp))
  cat(sprintf("  Observations:      %d\n", n_obs))
  cat(sprintf("  Total Nodes:       %d\n", n_nodes))
  cat(sprintf("  Terminal Nodes:    %d\n", n_terminal))
  cat(sprintf("  Max Depth:         %d\n", max_depth))
  cat(sprintf("  Split Variables:   %s\n", paste(split_vars, collapse = ", ")))

  if (is_class) {
    cat(sprintf("  Classes:           %s\n", paste(attr(object, "ylevels"), collapse = ", ")))
  }

  # --- Variable importance ---
  if (!is.null(object$varimp) && !is.null(object$varimp$vimp)) {
    cat("\n\nVARIABLE IMPORTANCE\n")
    cat(paste(rep("-", 40), collapse = ""), "\n")
    vimp_df <- object$varimp$vimp
    if (is.data.frame(vimp_df) && ncol(vimp_df) >= 2) {
      vimp_df <- vimp_df[order(vimp_df[[2]], decreasing = TRUE), ]
      max_imp <- max(vimp_df[[2]])
      for (i in seq_len(nrow(vimp_df))) {
        bar_width <- round(vimp_df[[2]][i] / max_imp * 20)
        bar <- paste(rep("#", bar_width), collapse = "")
        cat(sprintf("  %-20s %8.4f  %s\n", vimp_df[[1]][i], vimp_df[[2]][i], bar))
      }
    }
  }

  # --- Terminal node summary ---
  cat("\n\nTERMINAL NODES\n")
  cat(paste(rep("-", 70), collapse = ""), "\n")

  terminal <- tree_info[tree_info$terminal, ]

  if (is_class) {
    cat(sprintf("  %-8s  %-15s  %6s  %6s  %6s\n",
                "Node", "Prediction", "n", "Purity", "Wt"))
    cat(paste(rep("-", 55), collapse = ""), "\n")
    for (i in seq_len(nrow(terminal))) {
      nd <- terminal[i, ]
      purity <- if (!is.null(nd$prob)) sprintf("%.1f%%", nd$prob * 100) else "--"
      wt <- if (!is.null(nd$Wt)) sprintf("%.3f", nd$Wt) else "--"
      cat(sprintf("  %-8d  %-15s  %6d  %6s  %6s\n",
                  nd$node, nd$pred, nd$n, purity, wt))
    }
  } else {
    cat(sprintf("  %-8s  %-12s  %6s  %8s\n",
                "Node", "Prediction", "n", "Wt"))
    cat(paste(rep("-", 45), collapse = ""), "\n")
    for (i in seq_len(nrow(terminal))) {
      nd <- terminal[i, ]
      wt <- if (!is.null(nd$Wt)) sprintf("%.3f", nd$Wt) else "--"
      cat(sprintf("  %-8d  %-12s  %6d  %8s\n",
                  nd$node, format(nd$pred, digits = 4), nd$n, wt))
    }
  }

  # --- Decision rules ---
  cat("\n\nDECISION RULES\n")
  cat(paste(rep("-", 70), collapse = ""), "\n")

  for (i in seq_len(nrow(terminal))) {
    nd <- terminal[i, ]
    cat(sprintf("\nRule %d (Node %d, n=%d):\n", i, nd$node, nd$n))

    if (!is.na(nd$path) && nchar(nd$path) > 0) {
      conditions <- strsplit(nd$path, " & ")[[1]]
      for (j in seq_along(conditions)) {
        cond <- trimws(conditions[j])
        # Clean up negation
        if (startsWith(cond, "!")) {
          cond <- substring(cond, 2)
          if (grepl("<=", cond)) cond <- gsub("<=", ">", cond)
          else if (grepl("%in%", cond)) cond <- gsub("%in%", "NOT IN", cond)
        } else {
          if (grepl("%in%", cond)) cond <- gsub("%in%", "IN", cond)
        }
        cond <- gsub("c\\(", "{", cond)
        cond <- gsub("\\)", "}", cond)
        prefix <- if (j == 1) "  IF  " else "  AND "
        cat(sprintf("  %s %s\n", prefix, cond))
      }
    } else {
      cat("  IF (root -- all observations)\n")
    }
    cat(sprintf("  THEN: %s\n", nd$pred))
  }

  cat("\n")
  cat(paste(rep("=", 70), collapse = ""), "\n\n")

  invisible(object)
}


#' Plot an E2Tree model
#'
#' Displays the tree structure using rpart.plot.
#' This is a convenience wrapper around \code{\link{plot_e2tree}}.
#'
#' @param x An e2tree object
#' @param ensemble The ensemble model (randomForest or ranger).
#'   Required for converting the tree to rpart format.
#' @param main Plot title. Default is "E2Tree".
#' @param ... Additional arguments passed to \code{rpart.plot::rpart.plot}
#'
#' @method plot e2tree
#' @export
plot.e2tree <- function(x, ensemble = NULL, main = "E2Tree", ...) {

  if (is.null(ensemble)) {
    stop("'ensemble' argument is required for plotting. ",
         "Pass the randomForest/ranger model used to build the E2Tree.")
  }

  if (!requireNamespace("rpart.plot", quietly = TRUE)) {
    stop("Package 'rpart.plot' is required. Install with: install.packages('rpart.plot')")
  }

  rpart_obj <- rpart2Tree(x, ensemble)
  is_class <- !is.null(attr(x, "ylevels"))

  rpart.plot::rpart.plot(
    rpart_obj,
    main = main,
    type = 4,
    extra = if (is_class) 104 else 101,
    under = TRUE,
    faclen = 0,
    cex = 0.9,
    box.palette = if (is_class) "auto" else "Blues",
    shadow.col = "gray",
    nn = TRUE,
    ...
  )

  invisible(rpart_obj)
}


#' Predict Responses from an E2Tree Model
#'
#' Predicts classification or regression responses for new data using the
#' fitted E2Tree model.
#'
#' @param object An e2tree object.
#' @param newdata A data frame containing the new observations. If missing,
#'   the fitted values for the training data are returned.
#' @param target Character string specifying the target class for computing
#'   classification scores. Only used for classification trees. Default is
#'   \code{NULL}, which uses the first level.
#' @param ... Additional arguments (ignored).
#'
#' @return For regression: a data frame with columns \code{fit} (predicted
#'   value) and \code{sd} (standard deviation of the response within the
#'   terminal node, computed from the training data).
#'   For classification: a data frame with columns \code{fit} (predicted class),
#'   \code{accuracy} (probability of the predicted class), and \code{score}
#'   (probability of the target class).
#'
#' @examples
#' \donttest{
#' data(iris)
#' smp_size <- floor(0.75 * nrow(iris))
#' train_ind <- sample(seq_len(nrow(iris)), size = smp_size)
#' training <- iris[train_ind, ]
#' validation <- iris[-train_ind, ]
#'
#' ensemble <- randomForest::randomForest(Species ~ ., data = training,
#'   importance = TRUE, proximity = TRUE)
#' D <- createDisMatrix(ensemble, data = training, label = "Species",
#'   parallel = list(active = FALSE, no_cores = 1))
#' setting <- list(impTotal = 0.1, maxDec = 0.01, n = 2, level = 5)
#' tree <- e2tree(Species ~ ., training, D, ensemble, setting)
#'
#' ## Predict on new data
#' pred <- predict(tree, newdata = validation)
#' }
#'
#' @method predict e2tree
#' @export
predict.e2tree <- function(object, newdata, target = NULL, ...) {
  is_class <- !is.null(attr(object, "ylevels"))

  if (missing(newdata) || is.null(newdata)) {
    return(object$fitted.values)
  }

  if (is.null(target)) target <- "1"

  result <- ePredTree(fit = object, data = newdata, target = target)

  if (!is_class) {
    # For regression: compute node-level sd from training data
    tree_df <- object$tree
    terminal <- tree_df[tree_df$terminal, , drop = FALSE]
    y <- object$y

    # Compute sd for each terminal node, keyed by node number
    node_sd <- vapply(seq_len(nrow(terminal)), function(i) {
      obs_idx <- unlist(terminal$obs[[i]])
      if (length(obs_idx) < 2) return(0)
      sd(y[obs_idx])
    }, numeric(1))
    names(node_sd) <- as.character(terminal$node)

    # Build lookup: prediction value -> node number (handles duplicate preds)
    pred_to_node <- setNames(as.character(terminal$node), as.character(terminal$pred))

    # Map each prediction to its node's sd via pred value
    # Use match on prediction values to find the correct terminal node
    fit_vals <- as.numeric(result$fit)
    sd_vec <- vapply(fit_vals, function(v) {
      idx <- which(abs(as.numeric(terminal$pred) - v) < 1e-10)
      if (length(idx) == 0) return(NA_real_)
      node_sd[idx[1]]
    }, numeric(1))

    return(data.frame(
      fit = fit_vals,
      sd  = as.numeric(sd_vec),
      row.names = NULL
    ))
  }

  result
}


#' Extract Fitted Values from an E2Tree Model
#'
#' Returns the fitted values (predictions) for the training data used to
#' build the E2Tree model.
#'
#' @param object An e2tree object.
#' @param ... Additional arguments (ignored).
#'
#' @return A vector of fitted values.
#'
#' @examples
#' \donttest{
#' data(iris)
#' smp_size <- floor(0.75 * nrow(iris))
#' train_ind <- sample(seq_len(nrow(iris)), size = smp_size)
#' training <- iris[train_ind, ]
#'
#' ensemble <- randomForest::randomForest(Species ~ ., data = training,
#'   importance = TRUE, proximity = TRUE)
#' D <- createDisMatrix(ensemble, data = training, label = "Species",
#'   parallel = list(active = FALSE, no_cores = 1))
#' setting <- list(impTotal = 0.1, maxDec = 0.01, n = 2, level = 5)
#' tree <- e2tree(Species ~ ., training, D, ensemble, setting)
#'
#' fitted(tree)
#' }
#'
#' @method fitted e2tree
#' @export
fitted.e2tree <- function(object, ...) {
  object$fitted.values
}


#' Extract Residuals from an E2Tree Model
#'
#' Returns the residuals (observed minus fitted) for regression E2Tree models.
#' Not available for classification models.
#'
#' @param object An e2tree object.
#' @param ... Additional arguments (ignored).
#'
#' @return A numeric vector of residuals.
#'
#' @examples
#' \donttest{
#' data("mtcars")
#' smp_size <- floor(0.75 * nrow(mtcars))
#' train_ind <- sample(seq_len(nrow(mtcars)), size = smp_size)
#' training <- mtcars[train_ind, ]
#'
#' ensemble <- randomForest::randomForest(mpg ~ ., data = training, ntree = 500,
#'   importance = TRUE, proximity = TRUE)
#' D <- createDisMatrix(ensemble, data = training, label = "mpg",
#'   parallel = list(active = FALSE, no_cores = 1))
#' setting <- list(impTotal = 0.1, maxDec = 1e-6, n = 2, level = 5)
#' tree <- e2tree(mpg ~ ., training, D, ensemble, setting)
#'
#' residuals(tree)
#' }
#'
#' @method residuals e2tree
#' @export
residuals.e2tree <- function(object, ...) {
  if (!is.null(attr(object, "ylevels"))) {
    stop("Residuals are not available for classification E2Tree models.")
  }
  as.numeric(object$y) - as.numeric(object$fitted.values)
}
