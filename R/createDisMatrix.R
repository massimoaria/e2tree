#' Dissimilarity matrix
#'
#' The function crateDisMatrix creates a dissimilarity matrix among observations from an ensemble tree
#'
#' @param ensemble is an ensemble tree object (for the moment ensemble works only with rf objects)
#' @param data is the training dataset used for ensemble learning
#'
#' @return a dissimilarity matrix.
#'
#' @export

createDisMatrix <- function(ensemble, data){

  obs <- as.data.frame(attr(predict(ensemble, newdata = data, nodes = TRUE),"nodes"))
  obs <- cbind(row.names(obs), obs)
  names(obs) <- c("OBS",paste("Tree",seq(1,(ncol(obs)-1)), sep=""))
  row.names(obs)=NULL

  nodes <- sort(unique(as.numeric(as.matrix(((obs %>%
                                                select(starts_with("Tree"))))))))

  # response
  obs$resp <- ensemble$y

  ## Correct-classification rate matrix (Obs x Trees)
  w <- matrix(NA,nrow(obs),ensemble$ntree)
  a <- Matrix(0, nrow(obs),nrow(obs), sparse = TRUE)
  for (i in seq_len(ensemble$ntree)){
    if ((i %% 100)==0) cat("\nAnalized ", (i), " trees")
    # Correct classification rate for all terminal nodes of the i-th tree
    R <- obs %>% group_by_at(i+1) %>%
      mutate(n = n()) %>%
      summarize(freq = sort(table(.data$resp), decreasing=T)[1]/n) %>%
      slice(1) %>% as.data.frame()
    w[,i] <- R[as.numeric(factor(obs[,i+1])),2]
    # Calculate weighted co-occurrences among the observations
    a <- a+((outer(obs[,i+1],obs[,i+1],"==")+0)*w[,i])
  }


  ## a is the similarity matrix

  ## aa is the maximum similarity matrix
  aa=diag(a)

  aa=outer(aa,aa,"maxValue")

  a <- a/aa # now a is scaled between 0 and 1

  ## Dissimilarity matrix among observations (respect to the co-occurrences in the same node)
  dis <- 1-a
  row.names(dis)=colnames(dis)=obs$OBS

  return(dis)
}

maxValue <- function(x,y){
  apply(cbind(x,y),1,max)
}

