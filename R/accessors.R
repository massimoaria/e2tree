# ===========================================================================
# Accessor (extractor) functions for e2tree package classes
# ===========================================================================

# --- nodes() generic and methods ---

#' Extract Tree Node Information
#'
#' Extracts the data frame describing the nodes of an E2Tree model,
#' including split rules, predictions, and node statistics.
#'
#' @param x An e2tree object.
#' @param terminal Logical. If \code{TRUE}, return only terminal (leaf) nodes.
#'   Default is \code{FALSE}.
#' @param ... Additional arguments (ignored).
#'
#' @return A data frame with one row per node.
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
#' nodes(tree)
#' nodes(tree, terminal = TRUE)
#' }
#'
#' @export
nodes <- function(x, ...) {
  UseMethod("nodes")
}

#' @rdname nodes
#' @method nodes e2tree
#' @export
nodes.e2tree <- function(x, terminal = FALSE, ...) {
  tree_df <- x$tree
  if (terminal) {
    tree_df[tree_df$terminal == TRUE, , drop = FALSE]
  } else {
    tree_df
  }
}


# --- e2splits() generic and methods ---

#' Extract Split Information from an E2Tree Model
#'
#' Returns the split matrix and categorical split encoding from a fitted
#' E2Tree model.
#'
#' @param x An e2tree object.
#' @param ... Additional arguments (ignored).
#'
#' @return A list with components:
#' \describe{
#'   \item{splits}{The split information matrix.}
#'   \item{csplit}{The categorical split encoding matrix.}
#' }
#'
#' @export
e2splits <- function(x, ...) {
  UseMethod("e2splits")
}

#' @rdname e2splits
#' @method e2splits e2tree
#' @export
e2splits.e2tree <- function(x, ...) {
  list(splits = x$splits, csplit = x$csplit)
}


# --- measures() generic and methods ---

#' Extract Validation Measures
#'
#' Extracts the data frame of validation measures from an eValidation object,
#' including divergence and similarity metrics between the ensemble and
#' E2Tree proximity matrices.
#'
#' @param x An eValidation object.
#' @param ... Additional arguments (ignored).
#'
#' @return A data frame with columns for method name, type, observed value,
#'   and (if permutation tests were performed) null distribution statistics
#'   and p-values.
#'
#' @export
measures <- function(x, ...) {
  UseMethod("measures")
}

#' @rdname measures
#' @method measures eValidation
#' @export
measures.eValidation <- function(x, ...) {
  x$measures
}


# --- proximity() generic and methods ---

#' Extract Proximity Matrices
#'
#' Extracts proximity matrices from an eValidation object. The ensemble
#' proximity matrix is derived from the original ensemble model, while the
#' E2Tree proximity matrix is estimated from the fitted E2Tree.
#'
#' @param x An eValidation object.
#' @param type Character string specifying which proximity matrix to extract.
#'   One of \code{"ensemble"}, \code{"e2tree"}, or \code{"both"} (default).
#' @param ... Additional arguments (ignored).
#'
#' @return A matrix (if \code{type} is \code{"ensemble"} or \code{"e2tree"})
#'   or a list of two matrices (if \code{type} is \code{"both"}).
#'
#' @export
proximity <- function(x, ...) {
  UseMethod("proximity")
}

#' @rdname proximity
#' @method proximity eValidation
#' @export
proximity.eValidation <- function(x, type = c("both", "ensemble", "e2tree"), ...) {
  type <- match.arg(type)
  switch(type,
    ensemble = x$Proximity_matrix_ensemble,
    e2tree   = x$Proximity_matrix_e2tree,
    both     = list(
      ensemble = x$Proximity_matrix_ensemble,
      e2tree   = x$Proximity_matrix_e2tree
    )
  )
}
