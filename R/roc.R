#' Roc curve
#'
#' It calculates roc curve
#'
#' @param response is the response variable vector
#' @param scores is the probability vector of the prediction
#' @param target is the target response class
#'
#' @return an object.
#'
#'
#'
#' #examples
#'
#' @export
#'
roc <- function(response, scores, target="1"){
  labels <- as.numeric(response==target)
  AUC <- auc_probability(labels, scores)
  labels <- labels[order(scores, decreasing=TRUE)]
  R <- data.frame(TPR=cumsum(labels)/sum(labels), FPR=cumsum(!labels)/sum(!labels), labels)

  p <- R %>% ggplot(aes(x=.data$FPR,y=.data$TPR)) +
    geom_rect(aes(xmin=0, xmax=1, ymin=0, ymax=1), alpha=0, color="grey")+
    geom_line(size=0.5, color="firebrick")+
    geom_abline(intercept=0,slope=1, linetype="dashed")+
    annotate("text",x = 0.75, y = 0.25, label = paste("AUC = ",round(AUC,3)))+
    scale_x_continuous(breaks = c(0,0.25,0.5,0.75,1),labels = c("1.00","0.75","0.50","0.25","0.00"))+
    labs(title="Roc Curve", x = "Specificity", y = "Sensitivity") +
    theme_minimal()
  plot(p)

results <- list(roc_data=R, auc=AUC)
return(results)
}


auc_probability <- function(labels, scores, N=1e7){
  labels <- as.logical(labels)
  pos <- sample(scores[labels], N, replace=TRUE)
  neg <- sample(scores[!labels], N, replace=TRUE)
  # sum( (1 + sign(pos - neg))/2)/N # does the same thing
  (sum(pos > neg) + sum(pos == neg)/2) / N # give partial credit for ties
}
