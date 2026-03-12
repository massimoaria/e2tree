#' Goodness of Interpretability (GoI) Index
#'
#' Computes the GoI index measuring how well the E2Tree-estimated proximity 
#' matrix reconstructs the original ensemble proximity matrix.
#' 
#' The statistic is defined as:
#' 
#' \deqn{GoI(O, \hat{O}) = \sum_{i < j} \frac{(o_{ij} - \hat{o}_{ij})^2}{\max(o_{ij}, \hat{o}_{ij})}}
#' 
#' where:
#' \itemize{
#'   \item \eqn{o_{ij}} are the ensemble proximities
#'   \item \eqn{\hat{o}_{ij}} are the E2Tree-estimated proximities
#'   \item The sum is computed over all unique pairs \eqn{i < j}
#' }
#'
#' @param O Proximity matrix from the ensemble model (n x n), values in the interval 0 to 1
#' @param O_hat Proximity matrix estimated by E2Tree (n x n), values in the interval 0 to 1
#' @param sample Logical. If TRUE, randomly shuffles O_hat values for permutation testing.
#'   Default is FALSE.
#' @param seed Random seed for reproducibility when sample is TRUE. Default is NULL.
#'
#' @return A numeric value where:
#'   \itemize{
#'     \item 0 indicates perfect reconstruction (identical matrices)
#'     \item Higher values indicate greater discrepancy between matrices
#'   }
#'
#' @details
#' The statistic uses a normalized squared difference, where each cell's 
#' contribution is weighted by the maximum of the two proximity values.
#' This gives more weight to discrepancies in high-proximity regions.
#'
#' The metric uses only the lower triangle of the matrix (excluding the
#' diagonal) since proximity matrices are symmetric.
#'
#' Zero values in the ensemble matrix O are treated as missing (NA) and
#' excluded from the computation.
#'
#' @examples
#' \donttest{
#'# Example
#'
#' data(iris)
#' smp_size <- floor(0.75 * nrow(iris))
#' set.seed(42)
#' train_ind <- sample(seq_len(nrow(iris)), size = smp_size)
#' training <- iris[train_ind, ]
#'
#' ensemble <- randomForest::randomForest(Species ~ ., data = training,
#'   importance = TRUE, proximity = TRUE)
#'
#' D <- createDisMatrix(ensemble, data = training, label = "Species",
#'   parallel = list(active = FALSE, no_cores = 1))
#'
#' setting <- list(impTotal = 0.1, maxDec = 0.01, n = 2, level = 5)
#' tree <- e2tree(Species ~ ., training, D, ensemble, setting)
#'
#' vs <- eValidation(training, tree, D)
#' O  <- vs$Proximity_matrix_ensemble
#' O_hat <- vs$Proximity_matrix_e2tree
#'
#' goi(O, O_hat)
#' goi_perm(O, O_hat, alternative = "less", seed = 42)
#' goi_analysis(O, O_hat, n_perm = 9999, seed = 42)
#'
#' plot(goi_perm(O, O_hat, n_perm = 9999, seed = 42))
#'
#'
#' # Example with simulated data
#' n <- 50
#' O <- matrix(runif(n^2, 0.3, 1), n, n)
#' O <- (O + t(O)) / 2
#' diag(O) <- 1
#' O_hat <- O + matrix(rnorm(n^2, 0, 0.1), n, n)
#' O_hat <- pmax(pmin(O_hat, 1), 0)
#' diag(O_hat) <- 1
#'
#' goi(O, O_hat)
#'
#'
#' }
#'
#' @export
goi <- function(O, O_hat, sample = FALSE, seed = NULL) {
  
  # ---------------------------------------------------------------------------
  # INPUT VALIDATION
  # ---------------------------------------------------------------------------
  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  if (!is.matrix(O) || !is.matrix(O_hat)) {
    stop("Both O and O_hat must be matrices")
  }
  if (nrow(O) != ncol(O) || nrow(O_hat) != ncol(O_hat)) {
    stop("Matrices must be square")
  }
  if (!all(dim(O) == dim(O_hat))) {
    stop("O and O_hat must have the same dimensions")
  }
  
  # ---------------------------------------------------------------------------
  # EXTRACT LOWER TRIANGLE
  # ---------------------------------------------------------------------------
  
  # Since matrices are symmetric (proximities), use only lower triangle
  # to avoid counting pairs twice
  idx <- lower.tri(O, diag = FALSE)
  
  O_vec <- O[idx]
  O_hat_vec <- O_hat[idx]
  
  # ---------------------------------------------------------------------------
  # COMPUTE STATISTIC
  # ---------------------------------------------------------------------------
  
  if (sample) {
    # For permutation test: shuffle O_hat values among valid positions
    ind <- which(!is.na(O_vec))
    O_hat_vec <- sample(O_hat_vec[ind], size = length(ind), replace = FALSE)
    
    numerator <- (O_vec[ind] - O_hat_vec)^2
    denominator <- pmax(O_vec[ind], O_hat_vec)
  } else {
    # Standard computation
    numerator <- (O_vec - O_hat_vec)^2
    denominator <- pmax(O_vec, O_hat_vec)
  }
  
  # Compute weighted squared differences
  terms <- numerator / denominator
  
  # Sum all terms
  value <- sum(terms, na.rm = TRUE)
  
  return(value)
}


#' Permutation Test for GoI
#'
#' Performs a permutation test to assess whether the association between the
#' ensemble proximity matrix and the E2Tree reconstruction is significantly
#' greater than expected by chance. Includes computation of confidence 
#' intervals based on the null distribution.
#'
#' @param O Proximity matrix from the ensemble model (n x n)
#' @param O_hat Proximity matrix estimated by E2Tree (n x n)
#' @param n_perm Number of permutations (default: 999)
#' @param alternative Type of alternative hypothesis: "greater", "less", "two.sided"
#' @param conf.level Confidence level for intervals (default: 0.95)
#' @param graph Logical. If TRUE, displays the null distribution plot. Default is FALSE.
#' @param seed Random seed for reproducibility. Default is NULL.
#' @param .silent Logical. If TRUE, suppresses automatic printing. Default is FALSE.
#'
#' @return An object of class "goi_perm" containing:
#'   \item{statistic}{Observed GoI value}
#'   \item{p.value}{Test p-value}
#'   \item{ci}{Permutation-based confidence interval}
#'   \item{alternative}{Type of test performed}
#'   \item{n_perm}{Number of permutations}
#'   \item{null_dist}{Null distribution (vector of permuted GoI values)}
#'   \item{null_mean}{Mean of the null distribution}
#'   \item{null_sd}{Standard deviation of the null distribution}
#'   \item{z_stat}{Standardized Z statistic}
#'
#' @details
#' ## Test Logic
#' 
#' The test evaluates H0: there is no association between the structure of 
#' the ensemble proximity matrix and the E2Tree matrix.
#'
#' Under H0, randomly shuffling the values in O_hat breaks any structural 
#' association with O, generating the null distribution.
#'
#' ## P-value Calculation
#' 
#' The +1 correction in numerator and denominator includes the observed 
#' statistic in the count (Phipson & Smyth, 2010), ensuring valid p-values 
#' in the interval from 1/(B+1) to 1.
#'
#' ## Permutation-based Confidence Intervals
#'
#' CIs are computed by shifting the null distribution variability onto
#' the observed statistic.
#'
#' @examples
#' \donttest{
#' n <- 50
#' O <- matrix(runif(n^2, 0.3, 1), n, n)
#' O <- (O + t(O)) / 2
#' diag(O) <- 1
#' O_hat <- O + matrix(rnorm(n^2, 0, 0.1), n, n)
#' O_hat <- pmax(pmin(O_hat, 1), 0)
#' diag(O_hat) <- 1
#'
#' result <- goi_perm(O, O_hat, n_perm = 199)
#' }
#'
#' @export
goi_perm <- function(O, O_hat, n_perm = 999, 
                     alternative = c("greater", "less", "two.sided"),
                     conf.level = 0.95,
                     graph = FALSE,
                     seed = NULL,
                     .silent = FALSE) {
  
  # ---------------------------------------------------------------------------
  # INITIAL SETUP
  # ---------------------------------------------------------------------------
  
  alternative <- match.arg(alternative)
  
  # ---------------------------------------------------------------------------
  # INPUT VALIDATION
  # ---------------------------------------------------------------------------
  
  if (!is.matrix(O) || !is.matrix(O_hat)) {
    stop("Both O and O_hat must be matrices")
  }
  
  if (!all(dim(O) == dim(O_hat))) {
    stop("O and O_hat must have the same dimensions")
  }
  
  if (conf.level <= 0 || conf.level >= 1) {
    stop("conf.level must be between 0 and 1")
  }
  
  if (!is.null(seed)) set.seed(seed)
  
  n <- nrow(O)
  
  # ---------------------------------------------------------------------------
  # HANDLE ZERO VALUES
  # ---------------------------------------------------------------------------
  
  # Treat zeros in O as missing values
  idx_zero <- O == 0
  O_idx <- O
  O_idx[idx_zero] <- NA
  
  # ---------------------------------------------------------------------------
  # COMPUTE OBSERVED STATISTIC
  # ---------------------------------------------------------------------------
  
  G_obs <- goi(O_idx, O_hat, sample = FALSE)

  # ---------------------------------------------------------------------------
  # GENERATE NULL DISTRIBUTION VIA PERMUTATION (vectorized)
  # ---------------------------------------------------------------------------

  # Pre-extract vectors once to avoid repeated lower.tri extraction
  idx <- lower.tri(O_idx, diag = FALSE)
  O_vec <- O_idx[idx]
  O_hat_vec <- O_hat[idx]
  valid <- which(!is.na(O_vec))
  O_vec_valid <- O_vec[valid]
  O_hat_vec_valid <- O_hat_vec[valid]
  n_valid <- length(valid)

  # Vectorized permutation function (avoids full goi() dispatch)
  perm_one <- function(dummy) {
    shuffled <- O_hat_vec_valid[sample.int(n_valid)]
    num <- (O_vec_valid - shuffled)^2
    denom <- pmax(O_vec_valid, shuffled)
    sum(num / denom, na.rm = TRUE)
  }

  perm_G <- future.apply::future_sapply(seq_len(n_perm), perm_one, future.seed = TRUE)
  
  # ---------------------------------------------------------------------------
  # P-VALUE CALCULATION
  # ---------------------------------------------------------------------------
  
  # P-value includes +1 correction (Phipson & Smyth, 2010)
  
  if (alternative == "greater") {
    # H1: GoI is greater than expected by chance
    p_value <- (1 + sum(perm_G >= G_obs, na.rm = TRUE)) / (1 + n_perm)
    
  } else if (alternative == "less") {
    # H1: GoI is less than expected by chance
    p_value <- (1 + sum(perm_G <= G_obs, na.rm = TRUE)) / (1 + n_perm)
    
  } else {
    # Two-sided test
    null_mean_ts <- mean(perm_G, na.rm = TRUE)
    obs_dev <- abs(G_obs - null_mean_ts)
    perm_dev <- abs(perm_G - null_mean_ts)
    p_value <- (1 + sum(perm_dev >= obs_dev, na.rm = TRUE)) / (1 + n_perm)
  }
  
  # ---------------------------------------------------------------------------
  # PERMUTATION-BASED CONFIDENCE INTERVALS
  # ---------------------------------------------------------------------------
  
  alpha <- 1 - conf.level
  null_mean <- mean(perm_G, na.rm = TRUE)
  null_quantiles <- quantile(perm_G, probs = c(alpha/2, 1 - alpha/2), na.rm = TRUE)
  
  # Compute shift from null mean to quantiles
  p_shift <- null_mean - null_quantiles
  
  # Apply shift to observed statistic
  ci_raw <- G_obs + p_shift
  
  # Order correctly: [lower, upper]
  ci <- c(ci_raw[2], ci_raw[1])
  
  # GoI is non-negative, bound CI at 0
  ci <- pmax(ci, 0)
  
  names(ci) <- c(paste0((alpha/2) * 100, "%"), paste0((1 - alpha/2) * 100, "%"))
  
  # ---------------------------------------------------------------------------
  # COMPUTE STANDARDIZED Z STATISTIC
  # ---------------------------------------------------------------------------
  
  null_sd <- sd(perm_G, na.rm = TRUE)
  z_stat <- (G_obs - null_mean) / null_sd
  
  n_na <- sum(is.na(perm_G))
  
  # ---------------------------------------------------------------------------
  # PLOT (if requested)
  # ---------------------------------------------------------------------------
  
  if (graph) {
    z <- (perm_G - null_mean) / null_sd
    z <- z[!is.na(z)]
    
    plot(density(z), type = "l", 
         main = "Standardized Null Distribution of GoI",
         xlab = "Z-score", 
         ylab = "Density",
         lwd = 2)
    
    abline(v = z_stat, col = "red", lwd = 2, lty = 2)
    
    legend("topright", 
           legend = c("Null Distribution", paste0("Z obs = ", round(z_stat, 2))),
           col = c("black", "red"),
           lty = c(1, 2),
           lwd = 2,
           bty = "n")
  }
  
  # ---------------------------------------------------------------------------
  # BUILD RESULT OBJECT
  # ---------------------------------------------------------------------------
  
  result <- list(
    statistic = G_obs,
    p.value = p_value,
    ci = ci,
    conf.level = conf.level,
    alternative = alternative,
    n_perm = n_perm,
    null_dist = perm_G,
    null_mean = null_mean,
    null_sd = null_sd,
    z_stat = z_stat,
    n_na = n_na
  )
  
  class(result) <- "goi_perm"
  
  if (!.silent) {
    print(result)
  }
  
  invisible(result)
}


#' @method print goi_perm
#' @export
print.goi_perm <- function(x, digits = 4, ...) {
  
  cat("\n")
  cat("==============================================================================\n")
  cat("   Permutation Test for Goodness of Interpretability (GoI)\n")
  cat("==============================================================================\n\n")
  
  cat(sprintf("  Observed GoI:        %.*f\n", digits, x$statistic))
  cat(sprintf("  Null mean:           %.*f\n", digits, x$null_mean))
  cat(sprintf("  Null SD:             %.*f\n", digits, x$null_sd))
  cat(sprintf("  Z-statistic:         %.*f\n", digits, x$z_stat))
  
  cat("\n------------------------------------------------------------------------------\n")
  cat("  Hypothesis Test\n")
  cat("------------------------------------------------------------------------------\n")
  
  alt_text <- switch(x$alternative,
                     "greater" = "GoI > expected by chance",
                     "less" = "GoI < expected by chance",
                     "two.sided" = "GoI != expected by chance")
  cat(sprintf("  H1:                  %s\n", alt_text))
  
  if (x$p.value < 0.0001) {
    cat("  p-value:             < 0.0001 ***\n")
  } else {
    stars <- ifelse(x$p.value < 0.001, "***",
                    ifelse(x$p.value < 0.01, "**",
                           ifelse(x$p.value < 0.05, "*",
                                  ifelse(x$p.value < 0.1, ".", ""))))
    cat(sprintf("  p-value:             %.*f %s\n", digits, x$p.value, stars))
  }
  
  cat("\n------------------------------------------------------------------------------\n")
  cat("  Confidence Interval (Permutation-based)\n")
  cat("------------------------------------------------------------------------------\n")
  
  cat(sprintf("  %d%% CI:             [%.*f, %.*f]\n", 
              round(x$conf.level * 100),
              digits, x$ci[1], digits, x$ci[2]))
  
  cat(sprintf("\n  Permutations:        %d\n", x$n_perm))
  
  if (!is.null(x$n_na) && x$n_na > 0) {
    cat(sprintf("\n  WARNING: %d permutations produced NA (%.1f%%)\n", 
                x$n_na, 100 * x$n_na / x$n_perm))
  }
  
  cat("\n==============================================================================\n")
  cat("  Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1\n\n")
  
  invisible(x)
}


#' Plot method for Permutation Test results
#'
#' Displays the null distribution with the observed statistic and
#' confidence intervals.
#'
#' @param x A goi_perm object
#' @param ... Additional arguments passed to hist()
#'
#' @method plot goi_perm
#' @export
plot.goi_perm <- function(x, ...) {
  
  # Calculate x-axis limits
  xlim_range <- range(c(x$null_dist, x$statistic, x$ci), na.rm = TRUE)
  
  # Pre-calculate for proper ylim
  h <- hist(x$null_dist, plot = FALSE)
  d <- density(x$null_dist, na.rm = TRUE)
  ylim_max <- max(max(h$density), max(d$y)) * 1.1
  
  hist(x$null_dist, 
       main = "Null Distribution of GoI (Permutation Test)",
       xlab = "GoI",
       col = "lightgray",
       border = "white",
       xlim = xlim_range,
       ylim = c(0, ylim_max),
       freq = FALSE,
       ...)
  
  # Add density curve
  lines(d, col = "darkgray", lwd = 2)
  
  # Observed statistic (red)
  abline(v = x$statistic, col = "red", lwd = 2, lty = 1)
  
  # Null mean (gray dashed)
  abline(v = x$null_mean, col = "darkgray", lwd = 2, lty = 2)
  
  # Confidence intervals (blue dotted)
  abline(v = x$ci[1], col = "blue", lwd = 2, lty = 3)
  abline(v = x$ci[2], col = "blue", lwd = 2, lty = 3)
  
  legend("topleft", 
         legend = c(sprintf("Observed (%.2f)", x$statistic), 
                    sprintf("Null mean (%.2f)", x$null_mean), 
                    paste0(round(x$conf.level * 100), "% CI")),
         col = c("red", "darkgray", "blue"),
         lty = c(1, 2, 3),
         lwd = 2,
         bty = "n")
  
  if (x$p.value < 0.0001) {
    mtext("p < 0.0001", side = 3, line = -2, adj = 0.95)
  } else {
    mtext(sprintf("p = %.4f", x$p.value), side = 3, line = -2, adj = 0.95)
  }
}


#' Complete GoI Analysis
#'
#' Performs complete GoI analysis including point estimate and 
#' permutation test with confidence intervals.
#'
#' @param O Proximity matrix from the ensemble model (n x n)
#' @param O_hat Proximity matrix estimated by E2Tree (n x n)
#' @param n_perm Number of permutations (default: 999)
#' @param conf.level Confidence level (default: 0.95)
#' @param graph Logical. If TRUE, displays the null distribution plot. Default is FALSE.
#' @param seed Random seed for reproducibility. Default is NULL.
#'
#' @return An object of class "goi_analysis" containing:
#'   \item{estimate}{Observed GoI value}
#'   \item{ci}{Permutation-based confidence interval}
#'   \item{p.value}{Test p-value}
#'   \item{z_stat}{Standardized Z statistic}
#'   \item{perm}{Complete goi_perm object for detailed analysis}
#'
#' @examples
#' \donttest{
#' n <- 50
#' O <- matrix(runif(n^2, 0.3, 1), n, n)
#' O <- (O + t(O)) / 2
#' diag(O) <- 1
#' O_hat <- O + matrix(rnorm(n^2, 0, 0.1), n, n)
#' O_hat <- pmax(pmin(O_hat, 1), 0)
#' diag(O_hat) <- 1
#'
#' result <- goi_analysis(O, O_hat, n_perm = 199)
#' }
#'
#' @export
goi_analysis <- function(O, O_hat, n_perm = 999,
                         conf.level = 0.95, graph = FALSE, seed = NULL) {
  
  if (!is.null(seed)) set.seed(seed)
  
  # Permutation test (silent to avoid double printing)
  perm_result <- goi_perm(O, O_hat, n_perm = n_perm, 
                          alternative = "less",
                          conf.level = conf.level,
                          graph = FALSE,
                          .silent = TRUE)
  
  # Build result object
  result <- list(
    estimate = perm_result$statistic,
    ci = perm_result$ci,
    p.value = perm_result$p.value,
    z_stat = perm_result$z_stat,
    perm = perm_result
  )
  
  class(result) <- "goi_analysis"
  
  # Plot if requested
  if (graph) {
    plot(perm_result)
  }
  
  print(result)
  
  invisible(result)
}


#' @method print goi_analysis
#' @export
print.goi_analysis <- function(x, digits = 4, ...) {
  
  cat("\n")
  cat("##############################################################################\n")
  cat("      GOODNESS OF INTERPRETABILITY (GoI)\n")
  cat("##############################################################################\n\n")
  
  cat(sprintf("  GoI:                 %.*f\n", digits, x$estimate))
  cat(sprintf("  %d%% CI:             [%.*f, %.*f]\n", 
              round(x$perm$conf.level * 100),
              digits, x$ci[1], digits, x$ci[2]))
  
  cat("\n------------------------------------------------------------------------------\n")
  cat("  Permutation Test (H1: GoI > chance)\n")
  cat("------------------------------------------------------------------------------\n")
  
  if (x$p.value < 0.0001) {
    cat("  p-value:             < 0.0001 ***\n")
  } else {
    stars <- ifelse(x$p.value < 0.001, "***",
                    ifelse(x$p.value < 0.01, "**",
                           ifelse(x$p.value < 0.05, "*",
                                  ifelse(x$p.value < 0.1, ".", ""))))
    cat(sprintf("  p-value:             %.*f %s\n", digits, x$p.value, stars))
  }
  cat(sprintf("  Z-statistic:         %.*f\n", digits, x$z_stat))
  cat(sprintf("  Permutations:        %d\n", x$perm$n_perm))
  
  cat("\n##############################################################################\n\n")
  
  invisible(x)
}



