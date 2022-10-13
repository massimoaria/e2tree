meanDis <- function(dis){
  n <- nrow(dis)
  sum(dis)/(n*(n-1))
}
