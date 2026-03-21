#' Loss of Interpretability (LoI) Index
#'
#' Computes the LoI index and its decomposition, measuring how well
#' the E2Tree-estimated proximity matrix reconstructs the original
#' ensemble proximity matrix.
#'
#' The statistic is defined as:
#'
#' \deqn{\mathrm{LoI}(O, \hat{O}) = \sum_{i < j}
#'   \frac{(o_{ij} - \hat{o}_{ij})^2}{\max(o_{ij}, \hat{o}_{ij})}}
#'
#' The Normalized LoI divides by the number of pairs \eqn{M = n(n-1)/2}:
#'
#' \deqn{\mathrm{nLoI}(O, \hat{O}) = \frac{1}{M} \mathrm{LoI}(O, \hat{O})}
#'
#' The LoI decomposes into two components:
#' \itemize{
#'   \item \strong{LoI_in}: within-node loss (pairs grouped together by E2Tree)
#'   \item \strong{LoI_out}: between-node loss (pairs separated by E2Tree)
#' }
#' The ratio \eqn{\pi_{\mathrm{out}} = \mathrm{LoI}_{\mathrm{out}} / \mathrm{LoI}}
#' indicates what fraction of the total loss is due to the partition itself.
#'
#' @param O Proximity matrix from the ensemble model (n x n), values in [0,1]
#' @param O_hat Proximity matrix estimated by E2Tree (n x n), values in [0,1]
#' @param normalize Logical. If TRUE (default), returns nLoI (divided by M).
#'   If FALSE, returns raw LoI.
#'
#' @return An object of class \code{"loi"} containing:
#'   \item{loi}{Raw LoI value (unnormalized)}
#'   \item{nloi}{Normalized LoI (LoI / M)}
#'   \item{loi_in}{Within-node component}
#'   \item{loi_out}{Between-node component}
#'   \item{pi_in}{Proportion of loss from within-node discrepancy}
#'   \item{pi_out}{Proportion of loss from between-node separation}
#'   \item{n}{Matrix dimension}
#'   \item{m}{Number of unique pairs}
#'   \item{n_within}{Number of within-node pairs}
#'   \item{n_between}{Number of between-node pairs}
#'
#' @details
#' The statistic uses a normalized squared difference, where each cell's
#' contribution is weighted by the maximum of the two proximity values.
#' This gives more weight to discrepancies in high-proximity regions.
#'
#' \strong{Decomposition interpretation:}
#' \itemize{
#'   \item High \eqn{\pi_{\mathrm{out}}} (> 0.7): the tree needs more terminal
#'     nodes or different split criteria --- it is separating observations that
#'     the ensemble considers similar.
#'   \item High \eqn{\pi_{\mathrm{in}}} (> 0.7): the partition boundaries are
#'     well-placed, but within-node proximity calibration needs improvement.
#'   \item Balanced: the reconstruction is limited by the inherent tension
#'     between the fuzzy ensemble and the crisp E2Tree structure.
#' }
#'
#' @examples
#' \donttest{
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
#' O <- vs$Proximity_matrix_ensemble
#' O_hat <- vs$Proximity_matrix_e2tree
#'
#' # Compute LoI with decomposition
#' result <- loi(O, O_hat)
#' print(result)
#' summary(result)
#' plot(result)
#'
#' # Permutation test
#' perm <- loi_perm(O, O_hat, n_perm = 999, seed = 42)
#' print(perm)
#' plot(perm)
#' }
#'
#' @export
loi <- function(O, O_hat, normalize = TRUE) {

  # -------------------------------------------------------------------------
  # INPUT VALIDATION
  # -------------------------------------------------------------------------

  if (!is.matrix(O) || !is.matrix(O_hat)) {
    stop("Both O and O_hat must be matrices")
  }
  if (nrow(O) != ncol(O) || nrow(O_hat) != ncol(O_hat)) {
    stop("Matrices must be square")
  }
  if (!all(dim(O) == dim(O_hat))) {
    stop("O and O_hat must have the same dimensions")
  }

  n <- nrow(O)
  m <- n * (n - 1) / 2

  # -------------------------------------------------------------------------
  # EXTRACT LOWER TRIANGLE
  # -------------------------------------------------------------------------

  idx <- lower.tri(O, diag = FALSE)
  o_vec <- O[idx]
  ohat_vec <- O_hat[idx]

  # -------------------------------------------------------------------------
  # DECOMPOSITION: within-node vs between-node
  # -------------------------------------------------------------------------

  # Between-node pairs: O_hat == 0 (E2Tree separated them)
  is_between <- ohat_vec == 0
  is_within <- !is_between

  # Within-node component: standard LoI formula
  if (sum(is_within) > 0) {
    o_in <- o_vec[is_within]
    ohat_in <- ohat_vec[is_within]
    denom_in <- pmax(o_in, ohat_in, .Machine$double.eps)
    loi_in <- sum((o_in - ohat_in)^2 / denom_in, na.rm = TRUE)
  } else {
    loi_in <- 0
  }

  # Between-node component: reduces to sum(o_ij) for pairs where O_hat == 0
  # Because (o - 0)^2 / max(o, 0) = o when o > 0, and 0 when o == 0
  loi_out <- sum(o_vec[is_between], na.rm = TRUE)

  # Total
  loi_total <- loi_in + loi_out

  # Proportions
  if (loi_total > 0) {
    pi_in <- loi_in / loi_total
    pi_out <- loi_out / loi_total
  } else {
    pi_in <- 0
    pi_out <- 0
  }

  # Normalized
  nloi <- loi_total / m

  # -------------------------------------------------------------------------
  # BUILD RESULT OBJECT
  # -------------------------------------------------------------------------

  result <- list(
    loi      = loi_total,
    nloi     = nloi,
    loi_in   = loi_in,
    loi_out  = loi_out,
    pi_in    = pi_in,
    pi_out   = pi_out,
    n        = n,
    m        = m,
    n_within  = sum(is_within),
    n_between = sum(is_between)
  )
  class(result) <- "loi"
  result
}


# ===========================================================================
# PRINT METHOD
# ===========================================================================

#' @method print loi
#' @export
print.loi <- function(x, digits = 4, ...) {

  cat("\n")
  cat("  Loss of Interpretability (LoI)\n")
  cat("  -------------------------------------------\n")
  cat(sprintf("  nLoI (normalized):   %.*f\n", digits, x$nloi))
  cat(sprintf("  LoI (raw):           %.*f\n", digits, x$loi))
  cat(sprintf("  n = %d, pairs = %d\n", x$n, x$m))
  cat("\n")

  invisible(x)
}


# ===========================================================================
# SUMMARY METHOD
# ===========================================================================

#' @method summary loi
#' @export
summary.loi <- function(object, digits = 4, ...) {

  x <- object

  cat("\n")
  cat("##############################################################################\n")
  cat("   Loss of Interpretability (LoI) — Decomposition\n")
  cat("##############################################################################\n\n")

  cat(sprintf("  nLoI (normalized):   %.*f\n", digits, x$nloi))
  cat(sprintf("  LoI (raw):           %.*f\n", digits, x$loi))
  cat(sprintf("  n = %d, pairs = %d\n\n", x$n, x$m))

  cat("------------------------------------------------------------------------------\n")
  cat("  Component                Value       Pairs      Proportion   Diagnosis\n")
  cat("------------------------------------------------------------------------------\n")

  # Determine diagnosis
  diag_in <- ""
  diag_out <- ""
  if (x$pi_out > 0.7) {
    diag_out <- "<- dominant"
  } else if (x$pi_in > 0.7) {
    diag_in <- "<- dominant"
  }

  cat(sprintf("  Within-node (LoI_in)   %8.*f   %8d   %8.1f%%     %s\n",
              digits, x$loi_in, x$n_within, x$pi_in * 100, diag_in))
  cat(sprintf("  Between-node (LoI_out) %8.*f   %8d   %8.1f%%     %s\n",
              digits, x$loi_out, x$n_between, x$pi_out * 100, diag_out))

  cat("------------------------------------------------------------------------------\n\n")

  if (x$pi_out > 0.7) {
    cat("  Interpretation: Partition loss dominates. The E2Tree is separating\n")
    cat("  observations that the ensemble considers similar. Consider increasing\n")
    cat("  the number of terminal nodes or relaxing pruning constraints.\n\n")
  } else if (x$pi_in > 0.7) {
    cat("  Interpretation: Calibration loss dominates. The E2Tree's partition\n")
    cat("  boundaries are well-placed, but within-node proximity values differ\n")
    cat("  from the ensemble's. This is typical of the fuzzy-to-crisp transition.\n\n")
  } else {
    cat("  Interpretation: Loss is balanced between partition and calibration\n")
    cat("  components. The reconstruction is limited by the inherent structural\n")
    cat("  difference between the fuzzy ensemble and the crisp E2Tree.\n\n")
  }

  cat("##############################################################################\n\n")

  invisible(x)
}


# ===========================================================================
# PLOT METHOD
# ===========================================================================

#' @method plot loi
#' @export
plot.loi <- function(x, ...) {

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  par(mfrow = c(1, 2), mar = c(5, 4, 4, 2) + 0.1)

  # Panel 1: Decomposition bar chart
  vals <- c(x$loi_in, x$loi_out)
  pcts <- c(x$pi_in, x$pi_out) * 100
  nms <- c("Within-node\n(LoI_in)", "Between-node\n(LoI_out)")
  cols <- c("#3498db", "#e74c3c")

  bp <- barplot(vals, names.arg = nms, col = cols, border = NA,
                main = "LoI Decomposition",
                ylab = "LoI value", ylim = c(0, max(vals) * 1.25))
  text(bp, vals, labels = sprintf("%.1f%%", pcts),
       pos = 3, cex = 0.9, font = 2)

  # Panel 2: Proportion pie chart
  if (x$loi > 0) {
    pie(c(x$pi_in, x$pi_out),
        labels = sprintf("%s\n%.1f%%", c("Within (LoI_in)", "Between (LoI_out)"),
                         c(x$pi_in, x$pi_out) * 100),
        col = cols, border = NA,
        main = sprintf("Partition Loss Ratio\nnLoI = %.4f", x$nloi))
  } else {
    plot.new()
    text(0.5, 0.5, "Perfect reconstruction\nnLoI = 0", cex = 1.5)
  }
}


# ===========================================================================
# PERMUTATION TEST
# ===========================================================================

#' Permutation Test for LoI
#'
#' Performs a permutation test using row/column permutation to assess
#' whether the E2Tree reconstruction is significantly better than expected
#' by chance.
#'
#' @param O Proximity matrix from the ensemble model (n x n)
#' @param O_hat Proximity matrix estimated by E2Tree (n x n)
#' @param n_perm Number of permutations (default: 999)
#' @param conf.level Confidence level for intervals (default: 0.95)
#' @param seed Random seed for reproducibility. Default is NULL.
#'
#' @return An object of class \code{"loi_perm"} containing:
#'   \item{observed}{Observed nLoI value and decomposition (loi object)}
#'   \item{statistic}{Observed nLoI value (scalar)}
#'   \item{p.value}{Test p-value (one-sided, less)}
#'   \item{ci}{Permutation-based confidence interval for nLoI}
#'   \item{null_dist}{Null distribution of nLoI values}
#'   \item{null_mean}{Mean of the null distribution}
#'   \item{null_sd}{Standard deviation of the null distribution}
#'   \item{z_stat}{Standardized Z statistic}
#'   \item{n_perm}{Number of permutations}
#'   \item{conf.level}{Confidence level}
#'
#' @details
#' The test uses \strong{simultaneous row/column permutation} of
#' \eqn{\hat{O}}: for each replicate, a random permutation \eqn{\pi}
#' of \eqn{\{1, \ldots, n\}} is drawn and \eqn{\hat{O}^\pi =
#' \hat{O}[\pi, \pi]} is computed. This preserves the block-diagonal
#' structure of \eqn{\hat{O}} while breaking the correspondence with
#' \eqn{O}.
#'
#' The null hypothesis is: the E2Tree labeling is unrelated to the
#' ensemble structure. Under H1 (good reconstruction), the observed
#' nLoI should be significantly \emph{lower} than the null distribution.
#'
#' P-values include the +1 correction of Phipson & Smyth (2010).
#'
#' @examples
#' \donttest{
#' n <- 50
#' O <- matrix(runif(n^2, 0.3, 1), n, n)
#' O <- (O + t(O)) / 2; diag(O) <- 1
#' O_hat <- O + matrix(rnorm(n^2, 0, 0.05), n, n)
#' O_hat <- pmin(pmax((O_hat + t(O_hat)) / 2, 0), 1); diag(O_hat) <- 1
#'
#' result <- loi_perm(O, O_hat, n_perm = 199, seed = 42)
#' print(result)
#' summary(result)
#' plot(result)
#' }
#'
#' @export
loi_perm <- function(O, O_hat, n_perm = 999,
                     conf.level = 0.95,
                     seed = NULL) {

  if (!is.null(seed)) set.seed(seed)

  if (!is.matrix(O) || !is.matrix(O_hat)) {
    stop("Both O and O_hat must be matrices")
  }
  if (!all(dim(O) == dim(O_hat))) {
    stop("O and O_hat must have the same dimensions")
  }
  if (conf.level <= 0 || conf.level >= 1) {
    stop("conf.level must be between 0 and 1")
  }

  n <- nrow(O)

  # -------------------------------------------------------------------------
  # OBSERVED STATISTIC (with full decomposition)
  # -------------------------------------------------------------------------

  obs <- loi(O, O_hat)

  # -------------------------------------------------------------------------
  # NULL DISTRIBUTION via row/column permutation
  # -------------------------------------------------------------------------

  perm_one <- function(dummy) {
    perm_idx <- sample.int(n)
    O_hat_perm <- O_hat[perm_idx, perm_idx]
    loi(O, O_hat_perm)$nloi
  }

  if (requireNamespace("future.apply", quietly = TRUE)) {
    null_dist <- future.apply::future_sapply(seq_len(n_perm), perm_one,
                                              future.seed = TRUE)
  } else {
    null_dist <- vapply(seq_len(n_perm), perm_one, numeric(1))
  }

  # -------------------------------------------------------------------------
  # P-VALUE (one-sided, less: good reconstruction = low nLoI)
  # -------------------------------------------------------------------------

  p_value <- (1 + sum(null_dist <= obs$nloi, na.rm = TRUE)) / (1 + n_perm)

  # -------------------------------------------------------------------------
  # NULL DISTRIBUTION STATISTICS
  # -------------------------------------------------------------------------

  null_mean <- mean(null_dist, na.rm = TRUE)
  null_sd <- sd(null_dist, na.rm = TRUE)
  z_stat <- (obs$nloi - null_mean) / null_sd

  # -------------------------------------------------------------------------
  # CONFIDENCE INTERVAL
  # -------------------------------------------------------------------------

  alpha <- 1 - conf.level
  null_quantiles <- quantile(null_dist, probs = c(alpha / 2, 1 - alpha / 2),
                              na.rm = TRUE)
  p_shift <- null_mean - null_quantiles
  ci_raw <- obs$nloi + p_shift
  ci <- c(ci_raw[2], ci_raw[1])
  ci <- pmax(ci, 0)
  names(ci) <- c(paste0((alpha / 2) * 100, "%"),
                  paste0((1 - alpha / 2) * 100, "%"))

  # -------------------------------------------------------------------------
  # BUILD RESULT
  # -------------------------------------------------------------------------

  result <- list(
    observed   = obs,
    statistic  = obs$nloi,
    p.value    = p_value,
    ci         = ci,
    null_dist  = null_dist,
    null_mean  = null_mean,
    null_sd    = null_sd,
    z_stat     = z_stat,
    n_perm     = n_perm,
    conf.level = conf.level
  )
  class(result) <- "loi_perm"
  result
}


# ===========================================================================
# PRINT METHOD for loi_perm
# ===========================================================================

#' @method print loi_perm
#' @export
print.loi_perm <- function(x, digits = 4, ...) {

  cat("\n")
  cat("==============================================================================\n")
  cat("   Permutation Test for Loss of Interpretability (LoI)\n")
  cat("==============================================================================\n\n")

  cat(sprintf("  Observed nLoI:       %.*f\n", digits, x$statistic))
  cat(sprintf("  Null mean:           %.*f\n", digits, x$null_mean))
  cat(sprintf("  Null SD:             %.*f\n", digits, x$null_sd))
  cat(sprintf("  Z-statistic:         %.*f\n", digits, x$z_stat))

  cat("\n------------------------------------------------------------------------------\n")
  cat("  Hypothesis Test (H1: nLoI < expected by chance)\n")
  cat("------------------------------------------------------------------------------\n")

  if (x$p.value < 0.0001) {
    cat("  p-value:             < 0.0001 ***\n")
  } else {
    stars <- if (x$p.value < 0.001) "***"
             else if (x$p.value < 0.01) "**"
             else if (x$p.value < 0.05) "*"
             else if (x$p.value < 0.1) "."
             else ""
    cat(sprintf("  p-value:             %.*f %s\n", digits, x$p.value, stars))
  }

  cat(sprintf("  %d%% CI:             [%.*f, %.*f]\n",
              round(x$conf.level * 100),
              digits, x$ci[1], digits, x$ci[2]))
  cat(sprintf("  Permutations:        %d (row/column)\n", x$n_perm))

  cat("\n------------------------------------------------------------------------------\n")
  cat("  Decomposition\n")
  cat("------------------------------------------------------------------------------\n")

  obs <- x$observed
  cat(sprintf("  LoI_in  (within):    %.*f  (%5.1f%% of total)\n",
              digits, obs$loi_in, obs$pi_in * 100))
  cat(sprintf("  LoI_out (between):   %.*f  (%5.1f%% of total)\n",
              digits, obs$loi_out, obs$pi_out * 100))

  cat("\n==============================================================================\n")
  cat("  Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1\n\n")

  invisible(x)
}


# ===========================================================================
# SUMMARY METHOD for loi_perm
# ===========================================================================

#' @method summary loi_perm
#' @export
summary.loi_perm <- function(object, digits = 4, ...) {

  print(object, digits = digits, ...)
  cat("\n")
  summary(object$observed, digits = digits, ...)

  invisible(object)
}


# ===========================================================================
# PLOT METHOD for loi_perm
# ===========================================================================

#' @method plot loi_perm
#' @export
plot.loi_perm <- function(x, ...) {

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  par(mfrow = c(1, 2), mar = c(5, 4, 4, 2) + 0.1)

  # --- Panel 1: Null distribution with observed ---

  xlim_range <- range(c(x$null_dist, x$statistic, x$ci), na.rm = TRUE)

  h <- hist(x$null_dist, plot = FALSE)
  d <- density(x$null_dist, na.rm = TRUE)
  ylim_max <- max(max(h$density), max(d$y)) * 1.15

  hist(x$null_dist,
       main = "Null Distribution of nLoI",
       xlab = "nLoI",
       col = "lightgray",
       border = "white",
       xlim = xlim_range,
       ylim = c(0, ylim_max),
       freq = FALSE)

  lines(d, col = "darkgray", lwd = 2)
  abline(v = x$statistic, col = "red", lwd = 2, lty = 1)
  abline(v = x$null_mean, col = "darkgray", lwd = 2, lty = 2)
  abline(v = x$ci[1], col = "blue", lwd = 1.5, lty = 3)
  abline(v = x$ci[2], col = "blue", lwd = 1.5, lty = 3)

  p_str <- if (x$p.value < 0.0001) "p < 0.0001"
           else sprintf("p = %.4f", x$p.value)

  legend("topright",
         legend = c(sprintf("Observed (%.4f)", x$statistic),
                    sprintf("Null mean (%.4f)", x$null_mean),
                    paste0(round(x$conf.level * 100), "% CI"),
                    p_str),
         col = c("red", "darkgray", "blue", NA),
         lty = c(1, 2, 3, NA),
         lwd = c(2, 2, 1.5, NA),
         bty = "n", cex = 0.85)

  # --- Panel 2: Decomposition ---

  obs <- x$observed
  vals <- c(obs$loi_in, obs$loi_out)
  pcts <- c(obs$pi_in, obs$pi_out) * 100
  nms <- c("Within-node\n(LoI_in)", "Between-node\n(LoI_out)")
  cols <- c("#3498db", "#e74c3c")

  bp <- barplot(vals, names.arg = nms, col = cols, border = NA,
                main = sprintf("LoI Decomposition\nnLoI = %.4f", obs$nloi),
                ylab = "LoI value", ylim = c(0, max(vals) * 1.3))
  text(bp, vals, labels = sprintf("%.1f%%", pcts),
       pos = 3, cex = 0.9, font = 2)
}
