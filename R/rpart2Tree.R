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
  obj <- list(frame=frame, where=where, splits=fit$splits, csplit=fit$csplit, method="class")
  attr(obj, "xlevels") <- attr(fit, "xlevels")
  class(obj) <- "rpart"
  return(obj)
}
