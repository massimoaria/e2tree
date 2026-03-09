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
  n <- nrow(data)

  # Determine if the model is classification or regression
  is_classification <- !is.numeric(tree$pred)

  # Initialize the prediction dataframe (use correct type for fit column)
  if (is_classification) {
    pred <- data.frame(fit = rep(NA_character_, n), accuracy = NA_real_, score = NA_real_, row.names = 1:n)
  } else {
    pred <- data.frame(fit = rep(NA_real_, n), accuracy = NA_real_, score = NA_real_, row.names = 1:n)
  }

  # ---- Tree-traversal prediction (replaces eval/parse) ----
  # Start all observations at root node (node = 1)
  node_assignment <- rep(1L, n)

  # Internal (non-terminal) nodes, indexed by node number for fast lookup
  internal <- tree[tree$terminal == FALSE, , drop = FALSE]
  terminal <- tree[tree$terminal == TRUE, , drop = FALSE]

  # --- Pre-parse all split rules ONCE (avoid regex per prediction) ---
  split_cache <- parse_all_splits(internal$splitLabel)
  names(split_cache) <- internal$node

  # Traverse tree: assign each observation to a terminal node
  # Use named lookup for O(1) access to internal node rows
  internal_lookup <- setNames(seq_len(nrow(internal)), internal$node)

  max_iter <- nrow(tree) * 2  # safety limit
  iter <- 0
  while (iter < max_iter) {
    iter <- iter + 1
    in_internal <- node_assignment %in% internal$node
    if (!any(in_internal)) break

    active_nodes <- unique(node_assignment[in_internal])

    for (nd in active_nodes) {
      obs_idx <- which(node_assignment == nd)
      rule <- split_cache[[as.character(nd)]]
      if (is.null(rule)) next

      goes_left <- apply_split_rule(data, obs_idx, rule)

      node_assignment[obs_idx[goes_left]]  <- nd * 2L
      node_assignment[obs_idx[!goes_left]] <- nd * 2L + 1L
    }
  }

  # Now assign predictions from terminal nodes
  for (i in seq_len(nrow(terminal))) {
    nd <- terminal$node[i]
    obs_idx <- which(node_assignment == nd)
    if (length(obs_idx) == 0) next

    pred$fit[obs_idx] <- terminal$pred[i]

    if (is_classification) {
      prob_val <- as.numeric(terminal$prob[i])
      pred$accuracy[obs_idx] <- prob_val
      score_val <- prob_val
      if (!is.na(terminal$pred[i]) && terminal$pred[i] != target) {
        score_val <- 1 - prob_val
      }
      pred$score[obs_idx] <- score_val
    }
  }

  return(pred)
}


# Pre-parse all split labels into structured rules (called once)
parse_all_splits <- function(split_labels) {
  lapply(split_labels, function(lbl) {
    if (grepl(" <= ?", lbl)) {
      parts <- regmatches(lbl, regexec("^(.+?) <=(.+)$", lbl))[[1]]
      list(type = "numeric",
           var  = trimws(parts[2]),
           thr  = as.numeric(trimws(parts[3])))
    } else if (grepl(" %in% ", lbl)) {
      parts <- regmatches(lbl, regexec("^(.+?) %in% (.+)$", lbl))[[1]]
      cats_str <- parts[3]
      ## Parse c('a','b') once; safe because split labels are generated internally
      cats <- eval(parse(text = cats_str))
      list(type = "categorical",
           var  = trimws(parts[2]),
           cats = cats)
    } else {
      list(type = "unknown")
    }
  })
}

# Apply a pre-parsed split rule to data (no regex, no eval/parse)
apply_split_rule <- function(data, obs_idx, rule) {
  if (rule$type == "numeric") {
    return(data[[rule$var]][obs_idx] <= rule$thr)
  } else if (rule$type == "categorical") {
    return(as.character(data[[rule$var]][obs_idx]) %in% rule$cats)
  } else {
    warning("Unknown split format in cached rule")
    return(rep(TRUE, length(obs_idx)))
  }
}
