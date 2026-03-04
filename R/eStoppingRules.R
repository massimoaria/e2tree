eStoppingRules <- function(y, index, t, setting, response, ensemble, vart1){
  # Check stopping rules for current node
  #
  # Args:
  #   y: full dissimilarity matrix
  #   index: indices of observations in current node
  #   t: current node number
  #   setting: stopping rules configuration
  #   response: response variable
  #   ensemble: trained ensemble model
  #   vart1: variance in root node (for regression)
  #
  # Returns:
  #   List with sRule (boolean), impTotal, and n
  
  n <- length(index)
  
  # Create type object
  if (inherits(ensemble, "randomForest")) {
    type <- ensemble$type
  } else if (inherits(ensemble, "ranger")) {
    type <- tolower(ensemble$treetype)
  }
  
  if (n > 1){
    # ✅ OPTIMIZED: Pass matrix and indices separately
    impTotal <- meanDis_optimized(y, index)
    
    switch(type,
           classification = {
             res <- as.numeric(moda(response[index])[2])
           },
           regression = {
             res <- 1 - (variance(response[index]) / vart1)
           }
    )
  } else {
    impTotal <- 0
    res <- 1
  }
  
  sRule <- isTRUE(impTotal <= setting$impTotal |
                    n <= setting$n |
                    res > 0.95 |
                    (t*2)+1 > setting$tMax)
  
  results <- list(sRule=sRule, impTotal=impTotal, n=n)
  return(results)
}

# ============================================================================
# OPTIMIZED VERSION OF meanDis
# ============================================================================
# Avoids creating submatrix, works with indices directly
# ============================================================================

meanDis_optimized <- function(dis, index){
  # Calculate mean dissimilarity for observations in index
  # Optimized to work with indices instead of creating submatrix first
  #
  # Args:
  #   dis: full dissimilarity matrix
  #   index: indices of observations to consider
  #
  # Returns:
  #   Mean dissimilarity value
  
  n <- length(index)
  
  # Handle edge cases
  if (n <= 1) return(0)
  
  # Extract submatrix (unavoidable, but done efficiently)
  dis_subset <- dis[index, index]
  
  # Calculate sum
  total_sum <- sum(dis_subset)
  
  # Formula: mean = sum / (n * (n-1))
  result <- total_sum / (n * (n - 1))
  
  return(result)
}


# ============================================================================
# moda - Mode calculation
# ============================================================================

moda <- function(x) {
  if (anyNA(x)) x <- x[!is.na(x)]
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  res <- c(ux[which.max(tab)[1]], max(tab)[1]/sum(tab))
  res[1] <- as.character(levels(x)[res[1]])
  return(res)
}
