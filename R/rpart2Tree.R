#' Convert e2tree into an rpart object
#'
#' It converts an e2tree output into an rpart object.
#'
#' @param fit is e2tree object.
#'
#' @return An rpart object. It contains the following components:
#' \tabular{lll}{
#' \code{frame}\tab   \tab  The data frame includes a singular row for each node present in the tree. The row.names within the frame are assigned as unique node numbers, following a binary ordering system indexed by the depth of the nodes. The columns of the frame consist of the following components: (var) this variable denotes the names of the variables employed in the split at each node. In the case of leaf nodes, the level "<leaf>" is used to indicate their status as terminal nodes; (n) the variable 'n' represents the number of observations that reach a particular node; (wt) 'wt' signifies the sum of case weights associated with the observations reaching a given node; (dev) the deviance of the node, which serves as a measure of the node's impurity or lack of fit; (yval) the fitted value of the response variable at the node; (splits) this two-column matrix presents the labels for the left and right splits associated with each node; (complexity) the complexity parameter indicates the threshold value at which the split is likely to collapse; (ncompete) 'ncompete' denotes the number of competitor splits recorded for a node; (nsurrogate) the variable 'nsurrogate' represents the number of surrogate splits recorded for a node \cr
#' \code{where}\tab   \tab An integer vector that matches the length of observations in the root node. The vector contains the row numbers in the frame that correspond to the leaf nodes where each observation is assigned \cr
#' \code{call}\tab   \tab The matched call \cr
#' \code{terms}\tab   \tab A list of terms and attributes  \cr
#' \code{control}\tab   \tab A list containing the set of stopping rules for the tree building procedure \cr
#' \code{functions}\tab   \tab The summary, print, and text functions are utilized for the specific method required \cr
#' \code{variable.importance}\tab   \tab Variable importance refers to a quantitative measure that assesses the contribution of individual variables within a predictive model towards accurate predictions. It quantifies the influence or impact that each variable has on the model's overall performance. Variable importance provides insights into the relative significance of different variables in explaining the observed outcomes and aids in understanding the underlying relationships and dynamics within the model \cr}
#'
#'
#' #examples
#'
#' @export


rpart2Tree <- function(fit){

  frame <- fit$tree %>%
    select(.data$node,.data$variable, .data$n, .data$pred_val,.data$pred,.data$prob,.data$decImp, starts_with("yval2")) %>%
    rename(var=.data$variable,
           yval=.data$pred_val) %>%
    mutate(wt=.data$n,
           ncompete=0,
           nsurrogate=0,
           complexity=1-as.numeric(.data$prob),
           dev=round(.data$n*(1-as.numeric(.data$prob)))) %>%
    as.data.frame()

  rownames(frame) <- frame$node
  frame$var[is.na(frame$var)] <- "<leaf>"
  frame$complexity[is.na(frame$complexity)] <- 0.01

  frame <- frame %>%
    select("var","n","wt","dev","yval","complexity","ncompete","nsurrogate",starts_with("yval2"))#, nodeprob)
  #frame <- frame[,c("var","n","wt","dev","yval","complexity","ncompete","nsurrogate","yval2")]

  obs <- fit$tree %>%
    dplyr::filter(.data$terminal==TRUE) %>%
    select(.data$node,.data$n,.data$obs)
  where <- rep(obs$node,obs$n)
  names(where) <- do.call(c,obs$obs)
  where <- where[order(as.numeric(names(where)))]

  variable.importance <- fit$varimp$vimp[[2]]
  names(variable.importance) <- fit$varimp$vimp[[1]]

  obj <- list(frame=frame, where=where, call=fit$call, terms=fit$terms, method="class", control=fit$control, functions=rpartfunctions(),
              splits=fit$splits, csplit=fit$csplit, variable.importance=variable.importance)
  attr(obj, "xlevels") <- attr(fit, "xlevels")
  attr(obj, "ylevels") <- attr(fit, "ylevels")
  #obj$frame <- obj$frame[as.character(fit$N),]
  class(obj) <- "rpart"
  return(obj)
}


rpartfunctions <- function(){
  summary <- function (yval, dev, wt, ylevel, digits)
  {
    nclass <- (ncol(yval) - 2L)/2L
    group <- yval[, 1L]
    counts <- yval[, 1L + (1L:nclass)]
    yprob <- yval[, 1L + nclass + 1L:nclass]
    nodeprob <- yval[, 2L * nclass + 2L]
    if (!is.null(ylevel))
      group <- ylevel[group]
    temp1 <- formatg(counts, format = "%5g")
    temp2 <- formatg(yprob, format = "%5.3f")
    if (nclass > 1) {
      temp1 <- apply(matrix(temp1, ncol = nclass), 1L, paste,
                     collapse = " ")
      temp2 <- apply(matrix(temp2, ncol = nclass), 1L, paste,
                     collapse = " ")
    }
    dev <- dev/(wt[1L] * nodeprob)
    paste0("  predicted class=", format(group, justify = "left"),
           "  expected loss=", formatg(dev, digits), "  P(node) =",
           formatg(nodeprob, digits), "\n", "    class counts: ",
           temp1, "\n", "   probabilities: ", temp2)
  }

  print <- function (yval, ylevel, digits, nsmall)
  {
    temp <- if (is.null(ylevel))
      as.character(yval[, 1L])
    else ylevel[yval[, 1L]]
    nclass <- (ncol(yval) - 2L)/2L
    yprob <- if (nclass < 5L)
      format(yval[, 1L + nclass + 1L:nclass], digits = digits,
             nsmall = nsmall)
    else formatg(yval[, 1L + nclass + 1L:nclass], digits = 2L)
    if (!is.matrix(yprob))
      yprob <- matrix(yprob, nrow = 1L)
    temp <- paste0(temp, " (", yprob[, 1L])
    for (i in 2L:ncol(yprob)) temp <- paste(temp, yprob[, i],
                                            sep = " ")
    temp <- paste0(temp, ")")
    temp
  }
  text <- function (yval, dev, wt, ylevel, digits, n, use.n)
  {
    nclass <- (ncol(yval) - 2L)/2L
    group <- yval[, 1L]
    counts <- yval[, 1L + (1L:nclass)]
    if (!is.null(ylevel))
      group <- ylevel[group]
    temp1 <- formatg(counts, digits)
    if (nclass > 1L)
      temp1 <- apply(matrix(temp1, ncol = nclass), 1L, paste,
                     collapse = "/")
    if (use.n)
      paste0(format(group, justify = "left"), "\n", temp1)
    else format(group, justify = "left")
  }

  functions <- list(summary=summary, print=print, text=text)
  return(functions)
}

formatg <- function(x, digits = getOption("digits"),
                    format = paste0("%.", digits, "g"))
{
  if (!is.numeric(x)) stop("'x' must be a numeric vector")

  temp <- sprintf(format, x)
  if (is.matrix(x)) matrix(temp, nrow = nrow(x)) else temp
}
