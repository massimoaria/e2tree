utils::globalVariables("tree") # to avoid CRAN check errors for tidyverse programming

#' Validate an E2Tree Model via Proximity Matrix Comparison
#'
#' Compares the ensemble proximity matrix with the E2Tree-estimated proximity
#' matrix using multiple divergence and similarity measures. Can perform the
#' Mantel test, permutation tests on divergence/similarity measures (nLoI,
#' Hellinger, wRMSE, RV, SSIM), or both.
#'
#' @param data A data frame containing the variables in the model.
#' @param fit An e2tree object.
#' @param D The dissimilarity matrix obtained with \code{\link{createDisMatrix}}.
#' @param test Character string specifying which tests to perform. One of
#'   \code{"both"} (default), \code{"mantel"} (Mantel test only), or
#'   \code{"measures"} (divergence/similarity measures with permutation tests only).
#' @param graph Logical (default TRUE). If TRUE, heatmaps are displayed.
#' @param n_perm Integer. Number of permutations for the permutation
#'   test on measures. Default is 999. Set to 0 to skip permutation testing.
#'   Ignored when \code{test = "mantel"}.
#' @param conf.level Numeric. Confidence level for intervals. Default is 0.95.
#' @param seed Integer or NULL. Random seed for reproducibility.
#'
#' @return An object of class \code{"eValidation"} containing:
#'   \describe{
#'     \item{Proximity_matrix_ensemble}{Ensemble proximity matrix (reordered)}
#'     \item{Proximity_matrix_e2tree}{E2Tree proximity matrix (reordered)}
#'     \item{mantel_test}{Mantel test result (NULL if \code{test = "measures"})}
#'     \item{loi}{LoI object with decomposition (NULL if \code{test = "mantel"})}
#'     \item{measures}{Data frame with all measures (NULL if \code{test = "mantel"})}
#'     \item{permutation}{Permutation test results for measures (if applicable)}
#'   }
#'
#' @examples
#' \donttest{
#' ## Classification:
#' data(iris)
#' smp_size <- floor(0.75 * nrow(iris))
#' train_ind <- sample(seq_len(nrow(iris)), size = smp_size)
#' training <- iris[train_ind, ]
#'
#' ensemble <- randomForest::randomForest(Species ~ ., data=training,
#'   importance=TRUE, proximity=TRUE)
#'
#' D <- createDisMatrix(ensemble, data=training, label = "Species",
#'   parallel = list(active=FALSE, no_cores = 1))
#'
#' setting <- list(impTotal=0.1, maxDec=0.01, n=2, level=5)
#' tree <- e2tree(Species ~ ., training, D, ensemble, setting)
#'
#' val <- eValidation(training, tree, D, n_perm = 199)
#' print(val)
#' summary(val)
#' plot(val)
#' }
#'
#' @export
eValidation <- function(data, fit, D, test = c("both", "mantel", "measures"),
                        graph = TRUE,
                        n_perm = 999, conf.level = 0.95, seed = NULL) {

  if (!is.null(seed)) set.seed(seed)
  test <- match.arg(test)

  # === Input Validation ===
  if (!is.data.frame(data) || nrow(data) == 0) {
    stop("Error: 'data' must be a non-empty data frame.")
  }
  if (!inherits(fit, "e2tree")) {
    stop("Error: 'fit' must be an 'e2tree' object.")
  }
  if (!is.matrix(D) || nrow(D) != ncol(D)) {
    stop("Error: 'D' must be a square dissimilarity matrix.")
  }
  if (nrow(data) != nrow(D)) {
    stop("Error: The number of rows in 'data' must match the dimensions of 'D'.")
  }

  n <- nrow(data)
  df <- fit$tree
  terminal_nodes <- df$node[df$terminal]

  # === Build E2Tree proximity matrix Ps ===
  Ps <- matrix(0, n, n)
  for (i in terminal_nodes) {
    obs <- unlist(df$obs[df$node == i])
    if (!is.null(df$prob)) {
      Ps[obs, obs] <- df$prob[df$node == i]
    } else {
      Ps[obs, obs] <- df$Wt[df$node == i]
    }
  }
  diag(Ps) <- 1
  rownames(Ps) <- 1:n
  colnames(Ps) <- 1:n

  # === Reorder by hierarchical clustering ===
  D_exp <- D
  clusD <- hclust(as.dist(D_exp))
  ord <- clusD$order

  Ps_ord <- Ps[ord, ord]
  rownames(Ps_ord) <- ord
  colnames(Ps_ord) <- ord

  # === Mantel test ===
  mantel_test <- NULL
  if (test %in% c("both", "mantel")) {
    mantel_test <- ape::mantel.test(
      Ps_ord,
      1 - D_exp[ord, ord],
      graph = graph,
      main = "Mantel test",
      xlab = "z-statistic", ylab = "Density"
    )
  }

  # === Clamp to [0,1] and compute proximity matrices ===
  Ps_ord <- pmin(pmax(Ps_ord, 0), 1)
  prox_e2tree <- sqrt(Ps_ord)
  prox_ens <- sqrt(pmax(1 - D_exp[ord, ord], 0))

  # === Heatmaps ===
  if (graph) {
    e2heatmap(prox_e2tree)
    e2heatmap(prox_ens)
  }

  # === Compute divergence/similarity measures ===
  loi_result <- NULL
  measures_df <- NULL
  perm_result <- NULL

  if (test %in% c("both", "measures")) {
    O <- prox_ens
    O_hat <- prox_e2tree

    loi_result <- loi(O, O_hat)

    hellinger_val <- .e2val_hellinger(O, O_hat)
    wrmse_val     <- .e2val_wrmse(O, O_hat)
    rv_val        <- .e2val_rv(O, O_hat)
    ssim_val      <- .e2val_ssim(O, O_hat)

    measures_df <- data.frame(
      method   = c("nLoI", "Hellinger", "wRMSE", "RV", "SSIM"),
      type     = c("divergence", "divergence", "divergence", "similarity", "similarity"),
      observed = c(loi_result$nloi, hellinger_val, wrmse_val, rv_val, ssim_val),
      stringsAsFactors = FALSE
    )

    # === Permutation test (unified, row/column) ===
    if (n_perm > 0) {
      perm_result <- .e2val_unified_perm(O, O_hat, n_perm, conf.level)
      measures_df$null_mean <- perm_result$null_means
      measures_df$null_sd   <- perm_result$null_sds
      measures_df$z_stat    <- perm_result$z_stats
      measures_df$p_value   <- perm_result$p_values
      measures_df$ci_lower  <- perm_result$ci_lower
      measures_df$ci_upper  <- perm_result$ci_upper
    }
  }

  # === Build result ===
  results <- list(
    Proximity_matrix_ensemble = prox_ens,
    Proximity_matrix_e2tree   = prox_e2tree,
    mantel_test               = mantel_test,
    loi                       = loi_result,
    measures                  = measures_df,
    permutation               = perm_result,
    n                         = n,
    n_perm                    = n_perm,
    conf.level                = conf.level,
    test                      = test
  )
  class(results) <- "eValidation"
  results
}


# ===========================================================================
# INTERNAL: Hellinger distance
# ===========================================================================
.e2val_hellinger <- function(O, O_hat) {
  idx <- lower.tri(O, diag = FALSE)
  m <- sum(idx)
  sq_diff <- (sqrt(O[idx]) - sqrt(O_hat[idx]))^2
  sqrt(sum(sq_diff, na.rm = TRUE) / m)
}

# ===========================================================================
# INTERNAL: Weighted RMSE
# ===========================================================================
.e2val_wrmse <- function(O, O_hat, epsilon = .Machine$double.eps) {
  idx <- lower.tri(O, diag = FALSE)
  o <- O[idx]
  oh <- O_hat[idx]
  w <- pmax(o, oh, epsilon)
  sqrt(sum(w * (o - oh)^2, na.rm = TRUE) / sum(w, na.rm = TRUE))
}

# ===========================================================================
# INTERNAL: RV coefficient
# ===========================================================================
.e2val_rv <- function(O, O_hat) {
  O_c <- O; O_hat_c <- O_hat
  diag(O_c) <- 0; diag(O_hat_c) <- 0
  num <- sum(O_c * O_hat_c)
  denom <- sqrt(sum(O_c * O_c) * sum(O_hat_c * O_hat_c))
  if (denom < .Machine$double.eps) return(0)
  num / denom
}

# ===========================================================================
# INTERNAL: SSIM
# ===========================================================================
.e2val_ssim <- function(O, O_hat, window_size = NULL) {
  n <- nrow(O)
  if (is.null(window_size)) {
    window_size <- min(11L, max(3L, floor(n / 3)))
  }
  window_size <- as.integer(window_size)
  if (window_size %% 2 == 0) window_size <- window_size + 1L

  L <- 1
  C1 <- (0.01 * L)^2
  C2 <- (0.03 * L)^2
  w <- window_size
  k <- w * w

  .box_filter <- function(M, w) {
    n <- nrow(M)
    half_w <- (w - 1L) %/% 2L
    pad <- matrix(0, n + w - 1, n + w - 1)
    pad[(half_w + 1):(half_w + n), (half_w + 1):(half_w + n)] <- M
    cs <- apply(pad, 2, cumsum)
    row_sum <- cs[w:nrow(cs), , drop = FALSE] -
      rbind(rep(0, ncol(cs)), cs[1:(nrow(cs) - w), , drop = FALSE])
    cs2 <- t(apply(row_sum, 1, cumsum))
    out <- cs2[, w:ncol(cs2), drop = FALSE] -
      cbind(rep(0, nrow(cs2)), cs2[, 1:(ncol(cs2) - w), drop = FALSE])
    out[1:n, 1:n, drop = FALSE] / k
  }

  mu_O <- .box_filter(O, w)
  mu_Oh <- .box_filter(O_hat, w)
  mu_O2 <- .box_filter(O * O, w)
  mu_Oh2 <- .box_filter(O_hat * O_hat, w)
  mu_OOh <- .box_filter(O * O_hat, w)

  sigma_O2 <- pmax(mu_O2 - mu_O^2, 0)
  sigma_Oh2 <- pmax(mu_Oh2 - mu_Oh^2, 0)
  sigma_OOh <- mu_OOh - mu_O * mu_Oh

  numerator <- (2 * mu_O * mu_Oh + C1) * (2 * sigma_OOh + C2)
  denominator <- (mu_O^2 + mu_Oh^2 + C1) * (sigma_O2 + sigma_Oh2 + C2)
  ssim_map <- numerator / denominator

  half_w <- (w - 1L) %/% 2L
  valid_rows <- (half_w + 1):(n - half_w)
  valid_cols <- (half_w + 1):(n - half_w)

  if (length(valid_rows) < 1 || length(valid_cols) < 1) {
    o_vec <- O[lower.tri(O)]
    oh_vec <- O_hat[lower.tri(O_hat)]
    mu_o <- mean(o_vec); mu_oh <- mean(oh_vec)
    s_o2 <- var(o_vec); s_oh2 <- var(oh_vec)
    s_cross <- cov(o_vec, oh_vec)
    return(((2*mu_o*mu_oh + C1) * (2*s_cross + C2)) /
             ((mu_o^2 + mu_oh^2 + C1) * (s_o2 + s_oh2 + C2)))
  }

  mean(ssim_map[valid_rows, valid_cols], na.rm = TRUE)
}


# ===========================================================================
# INTERNAL: Unified permutation test for all measures
# ===========================================================================
.e2val_unified_perm <- function(O, O_hat, n_perm, conf.level) {

  n <- nrow(O)
  method_names <- c("nLoI", "Hellinger", "wRMSE", "RV", "SSIM")
  n_methods <- length(method_names)

  # Observed values
  obs_vals <- c(
    loi(O, O_hat)$nloi,
    .e2val_hellinger(O, O_hat),
    .e2val_wrmse(O, O_hat),
    .e2val_rv(O, O_hat),
    .e2val_ssim(O, O_hat)
  )

  is_div <- c(TRUE, TRUE, TRUE, FALSE, FALSE)

  # Permutation loop
  compute_perm <- function(dummy) {
    perm_idx <- sample.int(n)
    O_hat_p <- O_hat[perm_idx, perm_idx]
    c(
      loi(O, O_hat_p)$nloi,
      .e2val_hellinger(O, O_hat_p),
      .e2val_wrmse(O, O_hat_p),
      .e2val_rv(O, O_hat_p),
      .e2val_ssim(O, O_hat_p)
    )
  }

  if (requireNamespace("future.apply", quietly = TRUE)) {
    perm_mat <- future.apply::future_sapply(seq_len(n_perm), compute_perm,
                                             future.seed = TRUE)
  } else {
    perm_mat <- vapply(seq_len(n_perm), compute_perm, numeric(n_methods))
  }
  # perm_mat: n_methods x n_perm

  # Statistics
  null_means <- rowMeans(perm_mat, na.rm = TRUE)
  null_sds <- apply(perm_mat, 1, sd, na.rm = TRUE)
  z_stats <- (obs_vals - null_means) / null_sds

  # P-values
  p_values <- numeric(n_methods)
  for (i in seq_len(n_methods)) {
    if (is_div[i]) {
      p_values[i] <- (1 + sum(perm_mat[i, ] <= obs_vals[i], na.rm = TRUE)) / (1 + n_perm)
    } else {
      p_values[i] <- (1 + sum(perm_mat[i, ] >= obs_vals[i], na.rm = TRUE)) / (1 + n_perm)
    }
  }

  # Confidence intervals
  alpha <- 1 - conf.level
  ci_lower <- ci_upper <- numeric(n_methods)
  for (i in seq_len(n_methods)) {
    null_q <- quantile(perm_mat[i, ], probs = c(alpha/2, 1-alpha/2), na.rm = TRUE)
    p_shift <- null_means[i] - null_q
    ci_raw <- obs_vals[i] + p_shift
    ci <- c(ci_raw[2], ci_raw[1])
    if (is_div[i]) ci <- pmax(ci, 0)
    if (!is_div[i]) {
      lb <- if (method_names[i] == "SSIM") -1 else 0
      ci <- pmin(pmax(ci, lb), 1)
    }
    ci_lower[i] <- ci[1]
    ci_upper[i] <- ci[2]
  }

  list(
    null_means = null_means,
    null_sds   = null_sds,
    z_stats    = z_stats,
    p_values   = p_values,
    ci_lower   = ci_lower,
    ci_upper   = ci_upper,
    null_dists = perm_mat,
    method_names = method_names
  )
}


# ===========================================================================
# PRINT METHOD
# ===========================================================================

#' @method print eValidation
#' @export
print.eValidation <- function(x, digits = 4, ...) {

  cat("\n")
  cat("##############################################################################\n")
  cat("   E2Tree Validation\n")
  cat("##############################################################################\n\n")

  cat(sprintf("  Matrix dimension:    %d x %d\n", x$n, x$n))
  cat(sprintf("  Pairs:               %d\n", x$n * (x$n - 1) / 2))

  # Mantel test
  if (!is.null(x$mantel_test)) {
    cat(sprintf("\n  Mantel test:         z = %.2f, p = %.4f\n",
                x$mantel_test$z.stat, x$mantel_test$p))
  }

  # Measures table
  if (!is.null(x$measures)) {
    tbl <- x$measures

    cat("\n------------------------------------------------------------------------------\n")
    cat("  Measure          Type        Observed")
    if (!is.null(x$permutation)) cat("    Null mean    Z-stat     p-value")
    cat("\n")
    cat("------------------------------------------------------------------------------\n")

    for (i in seq_len(nrow(tbl))) {
      dir <- if (tbl$type[i] == "divergence") "[div]" else "[sim]"

      line <- sprintf("  %-14s %s   %8.*f",
                      tbl$method[i], dir, digits, tbl$observed[i])

      if (!is.null(x$permutation)) {
        stars <- if (tbl$p_value[i] < 0.001) "***"
                 else if (tbl$p_value[i] < 0.01) "**"
                 else if (tbl$p_value[i] < 0.05) "*"
                 else if (tbl$p_value[i] < 0.1) "."
                 else ""

        p_str <- if (tbl$p_value[i] < 0.0001) "< 0.0001"
                 else sprintf("%.*f", digits, tbl$p_value[i])

        line <- paste0(line, sprintf("   %8.*f   %+8.2f   %s %s",
                                      digits, tbl$null_mean[i],
                                      tbl$z_stat[i], p_str, stars))
      }
      cat(line, "\n")
    }
    cat("------------------------------------------------------------------------------\n")

    if (!is.null(x$permutation)) {
      cat(sprintf("  Permutations: %d (row/column), conf.level: %d%%\n",
                  x$n_perm, round(x$conf.level * 100)))
    }

    # LoI decomposition summary
    if (!is.null(x$loi)) {
      lo <- x$loi
      cat(sprintf("\n  LoI Decomposition (per-pair avg):  mean_in = %.6f,  mean_out = %.6f\n",
                  lo$mean_in, lo$mean_out))
    }

    cat("\n")
    cat("  [div] = divergence (lower=better), [sim] = similarity (higher=better)\n")
    cat("  Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1\n")
  }

  cat("\n##############################################################################\n\n")

  invisible(x)
}


# ===========================================================================
# SUMMARY METHOD
# ===========================================================================

#' @method summary eValidation
#' @export
summary.eValidation <- function(object, digits = 4, ...) {

  print(object, digits = digits, ...)
  if (!is.null(object$loi)) {
    cat("\n")
    summary(object$loi, digits = digits)
  }

  invisible(object)
}


# ===========================================================================
# PLOT METHOD
# ===========================================================================

#' @method plot eValidation
#' @export
plot.eValidation <- function(x, ...) {

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  has_perm <- !is.null(x$permutation)
  has_measures <- !is.null(x$measures)

  # Determine layout based on available components
  n_panels <- 2L  # always show heatmaps
  if (has_measures && has_perm) n_panels <- 4L
  else if (has_measures) n_panels <- 3L

  if (n_panels == 4L) {
    layout(matrix(c(1, 2, 3, 4), 2, 2, byrow = TRUE))
  } else {
    par(mfrow = c(1, n_panels))
  }

  par(mar = c(4, 4, 3, 1))

  # --- Panel 1: Ensemble heatmap ---
  image(x$Proximity_matrix_ensemble,
        main = "Ensemble Proximity (O)",
        col = colorRampPalette(c("white", "black"))(100),
        axes = FALSE, xlab = "", ylab = "")

  # --- Panel 2: E2Tree heatmap ---
  image(x$Proximity_matrix_e2tree,
        main = expression("E2Tree Proximity (" * hat(O) * ")"),
        col = colorRampPalette(c("white", "black"))(100),
        axes = FALSE, xlab = "", ylab = "")

  if (has_measures && has_perm) {
    # --- Panel 3: Null distribution of nLoI ---
    null_nloi <- x$permutation$null_dists[1, ]
    obs_nloi <- x$measures$observed[1]

    xlim_range <- range(c(null_nloi, obs_nloi), na.rm = TRUE)
    h <- hist(null_nloi, plot = FALSE)
    d <- density(null_nloi, na.rm = TRUE)
    ylim_max <- max(max(h$density), max(d$y)) * 1.15

    hist(null_nloi,
         main = "Null Distribution of nLoI",
         xlab = "nLoI", col = "lightgray", border = "white",
         xlim = xlim_range, ylim = c(0, ylim_max), freq = FALSE)
    lines(d, col = "darkgray", lwd = 2)
    abline(v = obs_nloi, col = "red", lwd = 2)
    abline(v = mean(null_nloi), col = "darkgray", lwd = 1.5, lty = 2)

    p_val <- x$measures$p_value[1]
    p_str <- if (p_val < 0.0001) "p < 0.0001" else sprintf("p = %.4f", p_val)
    legend("topright",
           legend = c(sprintf("Observed (%.4f)", obs_nloi), p_str),
           col = c("red", NA), lty = c(1, NA), lwd = c(2, NA),
           bty = "n", cex = 0.85)
  }

  # --- LoI Decomposition (per-pair) ---
  if (has_measures && !is.null(x$loi)) {
    lo <- x$loi
    means <- c(lo$mean_in, lo$mean_out)
    nms <- c("Within\n(mean_in)", "Between\n(mean_out)")
    cols <- c("#3498db", "#e74c3c")

    bp <- barplot(means, names.arg = nms, col = cols, border = NA,
                  main = sprintf("Per-Pair Average Loss\nnLoI = %.4f", lo$nloi),
                  ylab = "Mean loss per pair",
                  ylim = c(0, max(means) * 1.35))
    text(bp, means, labels = sprintf("%.4f", means),
         pos = 3, cex = 0.9, font = 2)
  }
}


# ===========================================================================
# HEATMAP HELPER (internal)
# ===========================================================================

e2heatmap <- function(data_matrix) {
  heatmap(
    data_matrix,
    Rowv = NA,
    Colv = NA,
    scale = "none",
    col = colorRampPalette(c("white", "black"))(100)
  )
}
