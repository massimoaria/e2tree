#' Roc curve
#'
#' Computes and plots the Receiver Operating Characteristic (ROC) curve for a binary classification model, 
#' along with the Area Under the Curve (AUC). The ROC curve is a graphical representation of a classifier’s 
#' performance across all classification thresholds.
#'
#' @param response is the response variable vector
#' @param scores is the probability vector of the prediction
#' @param target is the target response class
#'
#' @return an object.
#'
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
#'                             parallel = list(active=FALSE, no_cores = 1))
#' 
#' setting=list(impTotal=0.1, maxDec=0.01, n=2, level=5)
#' tree <- e2tree(Species ~ ., training, D, ensemble, setting)
#'
#' pr <- ePredTree(tree, validation, target="setosa")
#' 
#' roc(response_training, scores = pr$score, target = "setosa")
#'  
#' }
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
    geom_line(linewidth=0.5, color="firebrick")+
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
