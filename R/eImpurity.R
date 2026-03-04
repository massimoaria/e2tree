# ============================================================================
# eImpurity 
# ============================================================================

eImpurity <- function(y, index, S){
  # Calculate impurity for all possible splits at a given node
  # Optimized version eliminates future_apply to avoid memory transfer issues
  #
  # Args:
  #   y: Full dissimilarity matrix (n_obs x n_obs)
  #   index: Vector of indices for observations in current node
  #   S: Split matrix (n_obs x n_splits), binary indicators for each split
  #
  # Returns:
  #   Named vector of impurity values for each split
  
  n <- length(index)
  n_splits <- ncol(S)
  
  # Pre-calculate column sums for filtering
  tab <- colSums(S[index, , drop = FALSE])
  
  # Identify invalid splits (don't divide the node properly)
  ind_invalid <- !(tab > 1 & tab < (n - 1))
  
  # Initialize impurity vector
  imp <- numeric(n_splits)
  
  # ✅ CRITICAL: Preserve column names from S
  names(imp) <- colnames(S)
  
  # Calculate impurity for each split
  # Sequential loop is faster than parallel for this operation
  # because it avoids massive data transfer overhead
  for (j in 1:n_splits) {
    if (ind_invalid[j]) {
      imp[j] <- Inf
    } else {
      s <- S[index, j]
      imp[j] <- dissimilarity(y, index, s)
    }
  }
  
  return(imp)
}


# ============================================================================
# OPTIMIZED dissimilarity
# ============================================================================

dissimilarity <- function(y, index, s){
  # Calculate weighted dissimilarity for a given split
  # Optimized to use direct indexing instead of creating submatrices
  #
  # Args:
  #   y: Full dissimilarity matrix (not subset)
  #   index: Vector of indices for observations in current node
  #   s: Split vector (0=right, 1=left) for observations in index
  #
  # Returns:
  #   Weighted dissimilarity value for this split
  
  n <- length(index)
  
  # Identify observations going right (0) and left (1)
  idx_R_local <- which(s == 0)
  idx_L_local <- which(s == 1)
  
  nR <- length(idx_R_local)
  nL <- length(idx_L_local)
  
  # Convert to global indices
  idx_R <- index[idx_R_local]
  idx_L <- index[idx_L_local]
  
  # Calculate weighted dissimilarity for right child
  if (nR > 1) {
    sum_R <- sum(y[idx_R, idx_R])
    sR <- sum_R / (n * (nR - 1))
  } else {
    sR <- 0
  }
  
  # Calculate weighted dissimilarity for left child
  if (nL > 1) {
    sum_L <- sum(y[idx_L, idx_L])
    sL <- sum_L / (n * (nL - 1))
  } else {
    sL <- 0
  }
  
  # Return pooled impurity
  imp <- sR + sL
  
  return(imp)
}


