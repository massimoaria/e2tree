# ===========================================================================
# Coercion methods for the "e2tree" class
# ===========================================================================

#' Convert an E2Tree Object to rpart Format
#'
#' Coerces an \code{e2tree} object into an \code{rpart} object, which can
#' then be used with standard rpart methods for printing, plotting
#' (e.g., via \code{rpart.plot}), and prediction.
#'
#' @param x An e2tree object.
#' @param ensemble The ensemble model (randomForest or ranger) used to
#'   build the E2Tree.
#' @param ... Additional arguments (ignored).
#'
#' @return An \code{rpart} object.
#'
#' @seealso \code{\link{as.party.e2tree}} for conversion to partykit format.
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
#' rpart_obj <- as.rpart(tree, ensemble)
#' }
#'
#' @export
as.rpart <- function(x, ...) {
  UseMethod("as.rpart")
}

#' @rdname as.rpart
#' @method as.rpart e2tree
#' @export
as.rpart.e2tree <- function(x, ensemble, ...) {
  rpart2Tree(fit = x, ensemble = ensemble)
}


#' Convert an E2Tree Object to partykit Format
#'
#' Coerces an \code{e2tree} object into a \code{party} object from the
#' \pkg{partykit} package. This enables the use of partykit's infrastructure
#' for printing, plotting, and predicting with the E2Tree model.
#'
#' @param x An e2tree object.
#' @param ... Additional arguments (ignored).
#'
#' @return A \code{party} object (from \pkg{partykit}).
#'
#' @seealso \code{\link{as.rpart.e2tree}} for conversion to rpart format.
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
#' if (requireNamespace("partykit", quietly = TRUE)) {
#'   party_obj <- partykit::as.party(tree)
#'   plot(party_obj)
#' }
#' }
#'
#' @keywords internal
as.party.e2tree <- function(x, ...) {
  check_package("partykit")

  tree_df <- x$tree
  data <- x$data
  is_class <- !is.null(attr(x, "ylevels"))

  # Build partynode recursively from the e2tree structure
  node_map <- base::split(tree_df, seq_len(nrow(tree_df)))
  names(node_map) <- tree_df$node

  # Counter for sequential partykit node IDs
  id_counter <- 0L

  build_partynode <- function(node_id) {
    nd <- node_map[[as.character(node_id)]]
    id_counter <<- id_counter + 1L
    current_id <- id_counter

    # Terminal node
    if (nd$terminal) {
      return(partykit::partynode(id = current_id))
    }

    # Internal node: determine split
    split_label <- nd$splitLabel
    var_name <- nd$variable
    var_idx <- which(names(data) == var_name)

    if (length(var_idx) == 0) {
      return(partykit::partynode(id = current_id))
    }

    # Create the split
    if (grepl("<=", split_label)) {
      threshold <- as.numeric(sub(".*<=\\s*", "", split_label))
      sp <- partykit::partysplit(
        varid = as.integer(var_idx),
        breaks = threshold,
        right = FALSE
      )
    } else if (grepl("%in%", split_label)) {
      var_levels <- levels(factor(data[[var_name]]))
      cats_str <- sub(".*%in%\\s*", "", split_label)
      left_cats <- eval(parse(text = cats_str))
      index_vec <- ifelse(var_levels %in% left_cats, 1L, 2L)
      sp <- partykit::partysplit(
        varid = as.integer(var_idx),
        index = index_vec
      )
    } else {
      return(partykit::partynode(id = current_id))
    }

    # Recurse on children (left then right)
    left_id <- node_id * 2L
    right_id <- node_id * 2L + 1L

    kids <- list(
      build_partynode(left_id),
      build_partynode(right_id)
    )

    partykit::partynode(id = current_id, split = sp, kids = kids)
  }

  # Build the tree starting from root (node 1)
  pnode <- build_partynode(1L)

  # Build the fitted data frame with terminal node IDs
  # We need a mapping from e2tree node numbers to partykit sequential IDs
  # Traverse the tree again to build the mapping
  id_counter2 <- 0L
  node_id_map <- integer(0)

  map_node_ids <- function(node_id) {
    id_counter2 <<- id_counter2 + 1L
    nd <- node_map[[as.character(node_id)]]
    node_id_map[as.character(node_id)] <<- id_counter2
    if (!nd$terminal) {
      map_node_ids(node_id * 2L)
      map_node_ids(node_id * 2L + 1L)
    }
  }
  map_node_ids(1L)

  # Assign each observation to its terminal node's partykit ID
  terminal_nodes <- tree_df[tree_df$terminal, ]
  n_obs <- nrow(data)
  node_ids <- rep(NA_integer_, n_obs)
  for (i in seq_len(nrow(terminal_nodes))) {
    obs_idx <- unlist(terminal_nodes$obs[[i]])
    pk_id <- node_id_map[as.character(terminal_nodes$node[i])]
    node_ids[obs_idx] <- pk_id
  }

  if (is_class) {
    fitted_df <- data.frame(
      `(fitted)` = node_ids,
      `(response)` = factor(x$y, levels = attr(x, "ylevels")),
      check.names = FALSE
    )
  } else {
    fitted_df <- data.frame(
      `(fitted)` = node_ids,
      `(response)` = as.numeric(x$y),
      check.names = FALSE
    )
  }

  party_obj <- partykit::party(
    node = pnode,
    data = data,
    fitted = fitted_df,
    terms = x$terms
  )

  # Set constparty class for proper terminal node rendering
  class(party_obj) <- c("constparty", class(party_obj))

  party_obj
}
