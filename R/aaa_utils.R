# ============================================================================
# Utility functions shared across the e2tree package
# ============================================================================

#' Population Variance
#' @keywords internal
e2_variance <- function(x) {
  sum((x - mean(x))^2) / length(x)
}

#' Determine Ensemble Type from a Trained Model
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

#' Check Availability of Suggested Packages
#' @keywords internal
check_package <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(
      sprintf("Package '%s' is required but not installed. Please install it with: install.packages('%s')", pkg, pkg),
      call. = FALSE
    )
  }
}
