#' Convert a e2tree into an rpart object
#'
#' It converts a e2tree into an rpart object
#'
#' @param tree is e2tree object
#'
#' @return an rpart object.
#'
#'
#'
#' #examples
#'
#' @export


rpart2Tree <- function(tree){
  frame <- tree %>%
    select(.data$node,.data$variable, .data$n, .data$pred_val,.data$pred,.data$prob,.data$decImp, starts_with("yval2")) %>%
    rename(var=.data$variable,
           yval=.data$pred_val) %>%
    mutate(wt=.data$n,
           ncompete=0,
           nsurrogate=0,
           #yval2=.data$yval,
           complexity=1-as.numeric(.data$prob),
           dev=round(.data$n*(1-as.numeric(.data$prob)))) %>% 
           #nodeprob=frame$n/first(frame$n)) %>% ### AGGIUNTO
    as.data.frame()

  rownames(frame) <- frame$node
  frame$var[is.na(frame$var)] <- "<leaf>"
  frame$complexity[is.na(frame$complexity)] <- 0.01
  
  frame <- frame %>% 
    select("var","n","wt","dev","yval","complexity","ncompete","nsurrogate",starts_with("yval2"))#, nodeprob)
  #frame <- frame[,c("var","n","wt","dev","yval","complexity","ncompete","nsurrogate","yval2")]
  
  obs <- tree %>%
    dplyr::filter(.data$terminal==TRUE) %>%
    select(.data$node,.data$n,.data$obs)
  where <- rep(obs$node,obs$n)
  names(where) <- do.call(c,obs$obs)
  where <- where[order(as.numeric(names(where)))]
  
  #### da controllare e aggiustare
  splits <- tree %>%
    dplyr::filter(.data$terminal==FALSE) %>%
    mutate(ncat = -1,
           improve = 0,
           adj=0,
           splitLabel=as.numeric(unlist(lapply(strsplit(.data$splitLabel,"<="), function(l){l[-1]})))) %>%
    select(.data$variable, .data$n, .data$ncat, .data$improve, .data$splitLabel, .data$adj) %>%
    rename(count = .data$n,
           index = .data$splitLabel)
  variable <- splits$variable
  splits <- as.matrix(splits[,-1])
  row.names(splits) <- variable
  ######################################
  obj <- list(frame=frame, where=where, splits=splits, method="class")
  class(obj) <- "rpart"
  return(obj)
}
