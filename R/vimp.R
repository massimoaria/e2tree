utils::globalVariables(c("decImp", "n", "pred", "variable", "Nimp", "vimp", "decProb", "pimp",
                         "Pimp", "Nimp", "vimp_resp", "vimp_prob", "Variable", "MeanImpurityDecrease", "MeanAccuracyDecrease")) # to avoid CRAN check errors for tidyverse programming

#' Variable Importance
#'
#' Computes variable importance for an E2Tree model based on mean
#' impurity decrease and (for classification) mean accuracy decrease.
#'
#' @param fit An e2tree object.
#' @param data A data frame containing the variables in the model.
#' @param type Character string: \code{"classification"} or \code{"regression"}.
#'   If \code{NULL} (default), the type is automatically detected from the
#'   e2tree object.
#'
#' @return A list containing:
#' \describe{
#'   \item{vimp}{A data frame with variable importance metrics.}
#'   \item{g_imp}{A ggplot bar chart of Mean Impurity Decrease.}
#'   \item{g_acc}{(Classification only) A ggplot bar chart of Mean Accuracy Decrease.}
#' }
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
#' vi <- vimp(tree, training)
#' vi$vimp
#' vi$g_imp
#'
#'
#' ## Regression
#' data("mtcars")
#'
#' # Create training and validation set:
#' smp_size <- floor(0.75 * nrow(mtcars))
#' train_ind <- sample(seq_len(nrow(mtcars)), size = smp_size)
#' training <- mtcars[train_ind, ]
#'
#' # Perform training
#' ensemble = randomForest::randomForest(mpg ~ ., data=training, ntree=1000,
#' importance=TRUE, proximity=TRUE)
#'
#' D = createDisMatrix(ensemble, data=training, label = "mpg",
#'                          parallel = list(active=FALSE, no_cores = 1))
#'
#' setting=list(impTotal=0.1, maxDec=(1*10^-6), n=2, level=5)
#' tree <- e2tree(mpg ~ ., training, D, ensemble, setting)
#'
#' vi <- vimp(tree, training)
#' vi$vimp
#' vi$g_imp
#'
#' }
#'
#' @export
#'
vimp <- function(fit, data, type = NULL) {

  # === Input Validation ===

  if (!inherits(fit, "e2tree")) {
    stop("Error: 'fit' must be an 'e2tree' object.")
  }
  if (!is.data.frame(data) || nrow(data) == 0) {
    stop("Error: 'data' must be a non-empty data frame.")
  }
  if (!is.data.frame(fit$tree)) {
    stop("Error: 'fit$tree' must be a data frame.")
  }
  if (is.null(fit$terms)) {
    stop("Error: 'fit$terms' is missing or NULL in the e2tree object.")
  }

  # Auto-detect type from the e2tree object
  if (is.null(type)) {
    type <- if (!is.null(attr(fit, "ylevels"))) "classification" else "regression"
  }
  type <- match.arg(type, c("classification", "regression"))

  # === Prepare data ===

  data <- as.data.frame(data)
  row.names(data) <- NULL
  tree <- fit$tree
  response_var <- names(attr(fit$terms, "dataClasses"))[1]

  if (!response_var %in% colnames(data)) {
    stop("Error: The response variable from 'fit' is not found in 'data'.")
  }

  response <- data[[response_var]]

  # === Compute variable importance ===

  if (type == "classification") {
    res <- .vimp_classification(tree, data, response)
  } else {
    res <- .vimp_regression(tree)
  }

  res
}


# ===========================================================================
# Classification variable importance
# ===========================================================================
.vimp_classification <- function(tree, data, response) {

  row.names(tree) <- tree$node
  tree$prob <- as.numeric(tree$prob)

  # Internal nodes
  t <- row.names(tree)[tree$terminal == FALSE]
  tL <- as.character(tree[t, "node"] * 2)
  tR <- as.character(tree[t, "node"] * 2 + 1)

  # Weighted probability of children (primary split)
  tree[t, "probChildren"] <- (tree[tL, "prob"] * tree[tL, "n"] / tree[t, "n"]) +
    (tree[tR, "prob"] * tree[tR, "n"] / tree[t, "n"])

  # Weighted probability of children (surrogate split)
  names(response) <- row.names(data)
  for (i in t) {
    obs <- row.names(data)[unlist(tree[i, "obs"])]
    split_label <- tree[i, "splitLabelSur"]
    x <- data[obs, , drop = FALSE]
    goes_left <- evaluate_vimp_split(x, split_label)
    indL <- obs[goes_left]
    indR <- obs[!goes_left]
    probL <- as.numeric(moda(response[indL])[2])
    probR <- as.numeric(moda(response[indR])[2])
    tree[i, "probChildrenSur"] <- (probL * length(indL) / tree[i, "n"]) +
      (probR * length(indR) / tree[i, "n"])
  }

  tree[t, "decProb"] <- tree[t, "probChildren"] - tree[t, "probChildrenSur"]

  # Compute weighted importance
  tree_aug <- tree %>%
    mutate(Nimp = decImp * n / n[1],
           Pimp = decProb * n / n[1])

  # Grouped summarize by (variable, pred)
  by_var_pred <- tree_aug %>%
    group_by(variable, pred) %>%
    summarize(vimp = sum(Nimp, na.rm = TRUE),
              pimp = sum(Pimp, na.rm = TRUE), .groups = "drop") %>%
    drop_na(variable)

  vimp_resp <- by_var_pred %>%
    select(variable, pred, vimp) %>%
    pivot_wider(names_from = pred, values_from = vimp)
  names(vimp_resp)[-1] <- paste("ImpDec_", names(vimp_resp)[-1])

  vimp_prob <- by_var_pred %>%
    select(variable, pred, pimp) %>%
    pivot_wider(names_from = pred, values_from = pimp)
  names(vimp_prob)[-1] <- paste("AccDec_", names(vimp_prob)[-1])

  # Overall vimp
  vimp_df <- by_var_pred %>%
    group_by(variable) %>%
    summarize(vimp = sum(vimp, na.rm = TRUE),
              pimp = sum(pimp, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(vimp)) %>%
    left_join(vimp_resp, by = "variable") %>%
    left_join(vimp_prob, by = "variable")

  names(vimp_df)[1:3] <- c("Variable", "MeanImpurityDecrease", "MeanAccuracyDecrease")

  # --- Plots ---
  # Sort by MeanImpurityDecrease
  vimp_sorted_imp <- vimp_df %>% arrange(desc(MeanImpurityDecrease))
  pImp <- ggplot(vimp_sorted_imp, aes(y = Variable, x = MeanImpurityDecrease)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    scale_y_discrete(limits = rev(vimp_sorted_imp$Variable)) +
    labs(title = "Mean Impurity Decrease",
         x = "Mean Impurity Decrease", y = "Variable") +
    theme_minimal()

  # Sort by MeanAccuracyDecrease
  vimp_sorted_acc <- vimp_df %>% arrange(desc(MeanAccuracyDecrease))
  pAcc <- ggplot(vimp_sorted_acc, aes(y = Variable, x = MeanAccuracyDecrease)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    scale_y_discrete(limits = rev(vimp_sorted_acc$Variable)) +
    labs(title = "Mean Accuracy Decrease",
         x = "Mean Accuracy Decrease", y = "Variable") +
    theme_minimal()

  list(vimp = vimp_df, g_imp = pImp, g_acc = pAcc)
}


# ===========================================================================
# Regression variable importance
# ===========================================================================
.vimp_regression <- function(tree) {

  row.names(tree) <- tree$node

  vimp_df <- tree %>%
    mutate(Nimp = decImp * n / n[1]) %>%
    group_by(variable) %>%
    summarize(MeanImpurityDecrease = sum(Nimp, na.rm = TRUE), .groups = "drop") %>%
    drop_na(variable) %>%
    arrange(desc(MeanImpurityDecrease))

  # Rename for consistency
  names(vimp_df)[1] <- "Variable"

  # --- Plot ---
  pImp <- ggplot(vimp_df, aes(y = Variable, x = MeanImpurityDecrease)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    scale_y_discrete(limits = rev(vimp_df$Variable)) +
    labs(title = "Mean Impurity Decrease",
         x = "Mean Impurity Decrease",
         y = "Variable") +
    theme_minimal()

  list(vimp = vimp_df, g_imp = pImp)
}


## Helper: evaluate surrogate split without eval/parse
evaluate_vimp_split <- function(x, split_label) {
  if (grepl(" <= ?", split_label)) {
    parts <- regmatches(split_label, regexec("^(.+?) <=(.+)$", split_label))[[1]]
    var_name <- trimws(parts[2])
    threshold <- as.numeric(trimws(parts[3]))
    return(x[[var_name]] <= threshold)
  } else if (grepl(" %in% ", split_label)) {
    parts <- regmatches(split_label, regexec("^(.+?) %in% (.+)$", split_label))[[1]]
    var_name <- trimws(parts[2])
    cats <- eval(parse(text = parts[3]))
    return(as.character(x[[var_name]]) %in% cats)
  } else {
    warning(paste("Unknown split format:", split_label))
    return(rep(TRUE, nrow(x)))
  }
}
