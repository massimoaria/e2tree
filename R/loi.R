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
#' The per-pair averages \code{mean_in} and \code{mean_out} enable direct
#' comparison between the two components despite their different pair counts.
#'
#' @param O Proximity matrix from the ensemble model (n x n), values in the interval 0 to 1
#' @param O_hat Proximity matrix estimated by E2Tree (n x n), values in the interval 0 to 1
#' @param normalize Logical. If TRUE (default), returns nLoI (divided by M).
#'   If FALSE, returns raw LoI.
#'
#' @return An object of class \code{"loi"} containing:
#'   \item{loi}{Raw LoI value (unnormalized)}
#'   \item{nloi}{Normalized LoI (LoI / M)}
#'   \item{loi_in}{Within-node component (total)}
#'   \item{loi_out}{Between-node component (total)}
#'   \item{mean_in}{Per-pair average within-node loss (comparable with mean_out)}
#'   \item{mean_out}{Per-pair average between-node loss (comparable with mean_in)}
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
#' \strong{Decomposition interpretation (per-pair averages):}
#' \itemize{
#'   \item \code{mean_out}: average ensemble proximity lost by the partition.
#'     Low values (< 0.1) indicate the tree correctly separates low-proximity
#'     pairs. High values (> 0.3) suggest the tree splits apart pairs that
#'     the ensemble considers similar --more terminal nodes may help.
#'   \item \code{mean_in}: average calibration error within nodes. Low values
#'     (< 0.01) indicate excellent within-node reconstruction. Higher values
#'     reflect the inherent fuzzy-to-crisp transition.
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
#' prox <- proximity(vs)
#' O <- prox$ensemble
#' O_hat <- prox$e2tree
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
#' @importFrom graphics barplot layout text
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

  # Normalized
  nloi <- loi_total / m

  # Counts
  n_within <- sum(is_within)
  n_between <- sum(is_between)

  # Per-pair averages (comparable across components)
  mean_in  <- if (n_within > 0)  loi_in  / n_within  else 0
  mean_out <- if (n_between > 0) loi_out / n_between else 0

  # -------------------------------------------------------------------------
  # BUILD RESULT OBJECT
  # -------------------------------------------------------------------------

  result <- list(
    loi       = loi_total,
    nloi      = nloi,
    loi_in    = loi_in,
    loi_out   = loi_out,
    mean_in   = mean_in,
    mean_out  = mean_out,
    n         = n,
    m         = m,
    n_within  = n_within,
    n_between = n_between
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
  cat("   Loss of Interpretability (LoI) --Decomposition\n")
  cat("##############################################################################\n\n")

  cat(sprintf("  nLoI (normalized):   %.*f\n", digits, x$nloi))
  cat(sprintf("  LoI (raw):           %.*f\n", digits, x$loi))
  cat(sprintf("  n = %d, pairs = %d (within: %d, between: %d)\n\n",
              x$n, x$m, x$n_within, x$n_between))

  cat("------------------------------------------------------------------------------\n")
  cat("  Component              Total       Pairs     Mean/pair\n")
  cat("------------------------------------------------------------------------------\n")

  cat(sprintf("  Within-node (LoI_in)  %8.*f   %8d     %.*f\n",
              digits, x$loi_in, x$n_within, 6, x$mean_in))
  cat(sprintf("  Between-node (LoI_out)%8.*f   %8d     %.*f\n",
              digits, x$loi_out, x$n_between, 6, x$mean_out))

  cat("------------------------------------------------------------------------------\n\n")

  cat("  Per-pair interpretation (comparable across components):\n\n")

  cat(sprintf("    mean_in  = %.6f  (avg calibration error within nodes)\n", x$mean_in))
  cat(sprintf("    mean_out = %.6f  (avg ensemble proximity lost by separation)\n\n", x$mean_out))

  if (x$mean_out > 0.3) {
    cat("  Diagnostic: HIGH mean_out (> 0.3). The E2Tree is separating pairs\n")
    cat("  with substantial ensemble proximity. Consider more terminal nodes.\n\n")
  } else if (x$mean_out > 0.1) {
    cat("  Diagnostic: MODERATE mean_out. Some ensemble proximity is lost by\n")
    cat("  the partition, but most separated pairs have low ensemble proximity.\n\n")
  } else {
    cat("  Diagnostic: LOW mean_out. The partition correctly separates pairs\n")
    cat("  that have low ensemble proximity --the tree structure is well-placed.\n\n")
  }

  if (x$mean_in > 0.1) {
    cat("  Diagnostic: HIGH mean_in (> 0.1). Within-node proximity values\n")
    cat("  differ substantially from the ensemble. Check within-node calibration.\n\n")
  } else if (x$mean_in > 0.01) {
    cat("  Diagnostic: MODERATE mean_in. Some within-node calibration error,\n")
    cat("  typical of the fuzzy-to-crisp structural transition.\n\n")
  } else {
    cat("  Diagnostic: LOW mean_in. Within-node proximity values closely\n")
    cat("  match the ensemble --excellent calibration.\n\n")
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

  cols <- c("#3498db", "#e74c3c")

  # Panel 1: Per-pair average loss (comparable)
  means <- c(x$mean_in, x$mean_out)
  nms <- c("Within-node\n(mean_in)", "Between-node\n(mean_out)")

  bp <- barplot(means, names.arg = nms, col = cols, border = NA,
                main = sprintf("LoI Per-Pair Average Loss\nnLoI = %.4f", x$nloi),
                ylab = "Mean loss per pair",
                ylim = c(0, max(means) * 1.35))
  text(bp, means, labels = sprintf("%.4f", means),
       pos = 3, cex = 0.9, font = 2)

  # Panel 2: Pair counts
  counts <- c(x$n_within, x$n_between)
  nms2 <- c("Within-node", "Between-node")

  bp2 <- barplot(counts, names.arg = nms2, col = cols, border = NA,
                 main = "Number of Pairs",
                 ylab = "Count",
                 ylim = c(0, max(counts) * 1.25))
  text(bp2, counts, labels = counts,
       pos = 3, cex = 0.9, font = 2)
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
  cat("  Decomposition (per-pair averages)\n")
  cat("------------------------------------------------------------------------------\n")

  obs <- x$observed
  cat(sprintf("  mean_in  (within):   %.*f  (n_pairs = %d)\n",
              6, obs$mean_in, obs$n_within))
  cat(sprintf("  mean_out (between):  %.*f  (n_pairs = %d)\n",
              6, obs$mean_out, obs$n_between))

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

  # --- Panel 2: Per-pair decomposition ---

  obs <- x$observed
  means <- c(obs$mean_in, obs$mean_out)
  nms <- c("Within-node\n(mean_in)", "Between-node\n(mean_out)")
  cols <- c("#3498db", "#e74c3c")

  bp <- barplot(means, names.arg = nms, col = cols, border = NA,
                main = sprintf("Per-Pair Average Loss\nnLoI = %.4f", obs$nloi),
                ylab = "Mean loss per pair",
                ylim = c(0, max(means) * 1.35))
  text(bp, means, labels = sprintf("%.4f", means),
       pos = 3, cex = 0.9, font = 2)
}
