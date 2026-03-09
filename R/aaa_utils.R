# ============================================================================
# Utility functions shared across the e2tree package
# ============================================================================

#' Population variance (divides by n, not n-1)
#' @keywords internal
e2_variance <- function(x) {
  sum((x - mean(x))^2) / length(x)
}

#' Determine the ensemble type from a trained model
#' @param ensemble A trained randomForest or ranger model
#' @return Character string: "classification" or "regression"
#' @keywords internal
get_ensemble_type <- function(ensemble) {
  if (inherits(ensemble, "randomForest")) {
    return(ensemble$type)
  } else if (inherits(ensemble, "ranger")) {
    return(tolower(ensemble$treetype))
  } else {
    stop("'ensemble' must be a trained 'randomForest' or 'ranger' model.")
  }
}

#' Check that required packages are available (for Suggests dependencies)
#' @keywords internal
check_package <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(
      sprintf("Package '%s' is required but not installed. Please install it with: install.packages('%s')", pkg, pkg),
      call. = FALSE
    )
  }
}
