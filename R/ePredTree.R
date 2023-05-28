#' Predict responses through an explainable RF
#'
#' It predicts classification tree responses
#'
#' @param fit is a e2tree object
#' @param data is a data frame
#' @param target is the target value of response
#'
#' @return an object.
#'
#'
#'
#' #examples
#'
#' @export
#'
ePredTree <- function(fit, data, target="1"){
  tree <- fit$tree
  #type can be type=c("value","prob")

  row.names(data)=NULL
  data$nodeIndex <- 1:nrow(data)
  pred <- data.frame(fit=rep(NA,nrow(data)), accuracy=NA, score=NA, row.names = 1:nrow(data))

  # extracting paths of terminal nodes
  paths_list <- tree[tree$terminal==TRUE, c("node","path","pred","prob")]
  paths_list$prob = paths_list$score = as.numeric(paths_list$prob)

  ind <- which(paths_list$pred!=target)
  paths_list$score[ind] <- 1-paths_list$score[ind]

  # identifying obs by paths of terminal nodes and add predictions

  for (i in 1:nrow(paths_list)){
    print(i)
    path <- paths_list[i,"path"]
    Xn <- data %>% dplyr::filter(eval(parse(text=path)))
    index <- Xn$nodeIndex
    pred[index,"fit"] <- paths_list[i,"pred"]
    pred[index,"accuracy"] <- paths_list[i,"prob"]
    pred[index,"score"] <- paths_list[i,"score"]

  }

return(pred)

}



