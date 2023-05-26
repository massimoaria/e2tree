#' Convert a e2tree into an rpart object
#'
#' It converts a e2tree into an rpart object
#'
#' @param fit is e2tree object
#'
#' @return an rpart object.
#'
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
