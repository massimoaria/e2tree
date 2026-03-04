eImpurity <- function(y, index, S) {
  S_sub <- S[index, , drop = FALSE]
  n <- length(index)

  # Pre-filter: remove splits where one group has < 2 obs
  tab <- colSums(S_sub)
  valid <- which(tab >= 2 & tab <= (n - 2))

  if (length(valid) == 0) {
    imp <- rep(Inf, ncol(S_sub))
    names(imp) <- colnames(S_sub)
    return(imp)
  }

  # Extract the dissimilarity sub-matrix for this node
  y_sub <- y[index, index]

  # Use C++ for fast impurity computation on valid splits only
  S_valid <- matrix(as.integer(as.matrix(S_sub[, valid, drop = FALSE])),
                    nrow = n, ncol = length(valid))
  imp_valid <- compute_impurity_cpp(y_sub, S_valid)

  # Build full result vector
  imp <- rep(Inf, ncol(S_sub))
  names(imp) <- colnames(S_sub)
  imp[valid] <- imp_valid

  return(imp)
}


############
# Kept as fallback for debugging; no longer used in main loop
dissimilarity <- function(y, s) {
  n <- dim(y)[1]
  nR <- sum(s == 0)
  nL <- n - nR
  dR <- y[s == 0, s == 0]
  dL <- y[s == 1, s == 1]
  sR <- sum(dR) / (n * (nR - 1))
  sL <- sum(dL) / (n * (nL - 1))
  return(sR + sL)
}
