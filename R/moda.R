moda <- function(x) {
  if ( anyNA(x) ) x = x[!is.na(x)]
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  res <- c(ux[which.max(tab)[1]], max(tab)[1]/sum(tab))
  res[1] <- as.character(levels(x)[res[1]])
  return(res)

}
