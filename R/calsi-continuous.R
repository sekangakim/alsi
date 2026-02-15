## =======================================================================
## cALSI: Continuous Aggregated Latent Space Index
## Adaptation of ALSI for continuous multivariate data via ipsatized SVD
## Designed to complement SEPA (Segmented Profile Analysis)
## =======================================================================

## ---- Utility Functions ----

#' Ipsatize (row-center) a matrix
#' @param X Numeric matrix or data frame (persons x domains)
#' @return Row-centered matrix where each person's mean is zero
#' @keywords internal
ipsatize <- function(X) {
  X <- as.matrix(X)
  sweep(X, 1, rowMeans(X, na.rm = TRUE), "-")
}

## ---- Core SVD Engine for Continuous Data ----

#' Perform ipsatized SVD on continuous data (SEPA-style)
#' 
#' @param X Data frame or matrix with continuous variables (persons x domains)
#' @param K Number of dimensions to retain (default: all)
#' @return List containing SVD results with eigenvalues, coordinates
#' @keywords internal
svd_ipsatized <- function(X, K = NULL) {
  X <- as.matrix(X)
  X <- X[complete.cases(X), , drop = FALSE]
  
  N <- nrow(X)
  p <- ncol(X)
  
  if (N < 2) stop("Need at least 2 complete rows.")
  if (p < 2) stop("Need at least 2 variables.")
  
  # Store domain names
  domains <- colnames(X)
  if (is.null(domains)) domains <- paste0("V", 1:p)
  
  # Ipsatize (row-center) to isolate pattern effects
  Xstar <- ipsatize(X)
  
  # Note: rank of ipsatized matrix is at most min(N-1, p-1)
  max_rank <- min(N - 1, p - 1)
  
  # SVD
  sv <- svd(Xstar)
  
  # Eigenvalues (squared singular values)
  eig <- sv$d^2
  
  # Determine K
  if (is.null(K)) K <- length(sv$d)
  K <- min(K, length(sv$d), max_rank)
  
  # Row-isometric biplot coordinates
  # F = U * D (person coordinates)
  # B = V (domain loadings)
  if (K == 1) {
    F_coords <- matrix(sv$u[, 1] * sv$d[1], ncol = 1)
    B_loadings <- matrix(sv$v[, 1], ncol = 1)
  } else {
    F_coords <- sv$u[, 1:K, drop = FALSE] %*% diag(sv$d[1:K])
    B_loadings <- sv$v[, 1:K, drop = FALSE]
  }
  
  rownames(B_loadings) <- domains
  colnames(B_loadings) <- paste0("Dim", 1:K)
  colnames(F_coords) <- paste0("Dim", 1:K)
  
  structure(
    list(
      X = X,
      Xstar = Xstar,
      N = N,
      p = p,
      K = K,
      eig = eig,
      var_explained = eig / sum(eig),
      svd = sv,
      F = F_coords,
      B = B_loadings,
      domains = domains
    ),
    class = "svd_fit"
  )
}

#' @export
print.svd_fit <- function(x, ...) {
  cat("Ipsatized SVD fit (cALSI/SEPA-compatible):\n")
  cat("  N =", x$N, "individuals\n")
  cat("  p =", x$p, "domains\n")
  cat("  K =", x$K, "dimensions retained\n")
  cat("  Total pattern variance:", round(sum(x$eig), 4), "\n")
  cat("  Top eigenvalues:", round(x$eig[1:min(5, length(x$eig))], 4), "\n")
  cat("  Variance explained:", round(x$var_explained[1:min(5, length(x$var_explained))] * 100, 2), "%\n")
  invisible(x)
}

## =====================================================================
## Parallel Analysis for Continuous Ipsatized Data
## =====================================================================

#' Parallel Analysis for Ipsatized SVD Dimensionality Assessment
#'
#' Uses the paran package (Horn's parallel analysis with Longman-Allen-Chabassol
#' bias adjustment) for dimensionality assessment, ensuring compatibility with
#' SEPA methodology. Falls back to a built-in method if paran is unavailable.
#'
#' @param data Data frame or matrix of continuous variables
#' @param B Integer, number of iterations for paran (default: 2000)
#' @param q Numeric, centile for retention threshold (default: 0.95)
#' @param seed Integer, random seed for reproducibility
#' @param graph Logical, whether to display the scree plot (default: TRUE)
#' @param verbose Logical, print progress messages
#'
#' @return S3 object of class \code{svd_pa} containing:
#'   \item{eig_obs}{Observed eigenvalues}
#'   \item{eig_adj}{Adjusted eigenvalues (from paran)}
#'   \item{eig_rand}{Random eigenvalues (threshold)}
#'   \item{K_star}{Number of dimensions to retain}
#'   \item{fit}{SVD fit object for downstream cALSI computation}
#'   \item{method}{Method used ("paran" or "fallback")}
#'
#' @details
#' This function primarily uses the paran package, which implements Horn's

#' parallel analysis with the bias adjustment described in Longman, Cota,
#' Holden, & Fekken (1989). This is the same method used in SEPA.
#'
#' The paran package should be installed: install.packages("paran")
#'
#' @export
svd_pa <- function(data, 
                   B = 2000, 
                   q = 0.95, 
                   seed = 20260206,
                   graph = TRUE,
                   verbose = TRUE) {
  
  # Load and prepare data
  X <- as.matrix(data)
  X <- X[complete.cases(X), , drop = FALSE]
  
  N <- nrow(X)
  p <- ncol(X)
  
  # Ipsatize the data (row-center)
  X_ipsatized <- ipsatize(X)
  
  # Get SVD fit for downstream cALSI computation (on original scale)
  fit0 <- svd_ipsatized(X)
  
  # =====================================================================
  # Primary method: Use paran package (SEPA-compatible)
  # =====================================================================
  
  if (requireNamespace("paran", quietly = TRUE)) {
    if (verbose) cat("Using paran package for parallel analysis (SEPA-compatible)...\n")
    
    # Set seed for reproducibility
    set.seed(seed)
    
    # Run paran on ipsatized data
    # cfa = FALSE for PCA (not common factor analysis)
    pa_result <- paran::paran(X_ipsatized, 
                              iterations = B, 
                              centile = q * 100,
                              quietly = !verbose, 
                              graph = graph, 
                              cfa = FALSE,
                              color = TRUE,
                              col = c("black", "red", "blue"))
    
    # Extract results
    K_star <- pa_result$Retained
    
    out <- structure(
      list(
        eig_obs = pa_result$Ev,
        eig_adj = pa_result$AdjEv,
        eig_rand = pa_result$RndEv,
        K_star = K_star,
        fit = fit0,
        method = "paran",
        q = q,
        B = B,
        paran_result = pa_result
      ),
      class = "svd_pa"
    )
    
    if (verbose) {
      cat("\nParallel Analysis (paran) complete.\n")
      cat("Retained dimensions K* =", K_star, "\n\n")
    }
    
    return(invisible(out))
    
  } else {
    # =====================================================================
    # Fallback: Built-in PA (if paran not available)
    # =====================================================================
    
    warning("paran package not installed. Using built-in PA (may differ from SEPA).\n",
            "Install paran for exact SEPA compatibility: install.packages('paran')")
    
    if (verbose) cat("Running built-in parallel analysis with B =", B, "permutations...\n")
    
    # Column-standardize then ipsatize
    X_scaled <- scale(X, center = TRUE, scale = TRUE)
    X_std_ips <- ipsatize(X_scaled)
    
    # Observed eigenvalues
    sv_obs <- svd(X_std_ips)
    eig_obs <- sv_obs$d^2
    eig_obs_scaled <- eig_obs / sum(eig_obs) * p
    
    Kmax <- length(eig_obs)
    
    # Permutation distribution
    eig_perm <- matrix(NA_real_, nrow = B, ncol = Kmax)
    set.seed(seed)
    
    if (verbose) pb <- txtProgressBar(0, B, style = 3)
    
    for (b in seq_len(B)) {
      Xp <- apply(X_scaled, 2, sample, replace = FALSE)
      Xp_star <- ipsatize(Xp)
      eig_b <- svd(Xp_star)$d^2
      eig_perm[b, ] <- eig_b / sum(eig_b) * p
      if (verbose) setTxtProgressBar(pb, b)
    }
    
    if (verbose) close(pb)
    
    # Threshold
    eig_q <- apply(eig_perm, 2, quantile, probs = q, na.rm = TRUE)
    K_star <- max(1, sum(eig_obs_scaled > eig_q))
    
    out <- structure(
      list(
        eig_obs = eig_obs_scaled,
        eig_adj = NULL,
        eig_rand = eig_q,
        K_star = K_star,
        fit = fit0,
        method = "fallback (install paran for SEPA compatibility)",
        q = q,
        B = B
      ),
      class = "svd_pa"
    )
    
    if (verbose) {
      cat("\nParallel Analysis (fallback) complete.\n")
      cat("Suggested K* =", K_star, "\n")
      cat("NOTE: Install 'paran' package for exact SEPA compatibility.\n\n")
    }
    
    # Plot
    if (graph) {
      Kplot <- min(20, Kmax)
      plot(1:Kplot, eig_obs_scaled[1:Kplot], 
           type = "b", pch = 19, col = "black",
           xlab = "Dimension k", ylab = "Eigenvalue",
           main = sprintf("Parallel Analysis (fallback, B=%d)", B),
           ylim = range(c(eig_obs_scaled[1:Kplot], eig_q[1:Kplot])))
      lines(1:Kplot, eig_q[1:Kplot], type = "b", pch = 1, col = "red")
      legend("topright", legend = c("Observed", "Random"), 
             pch = c(19, 1), col = c("black", "red"), bty = "n")
    }
    
    return(invisible(out))
  }
}

#' @export
print.svd_pa <- function(x, ...) {
  cat("Parallel Analysis for Ipsatized SVD\n")
  cat("  Method:", x$method, "\n")
  cat("  Retained dimensions K* =", x$K_star, "\n\n")
  
  if (x$method == "paran") {
    cat("Eigenvalue comparison:\n")
    top_k <- min(10, length(x$eig_obs))
    comp <- data.frame(
      k = 1:top_k,
      Observed = round(x$eig_obs[1:top_k], 4),
      Adjusted = round(x$eig_adj[1:top_k], 4),
      Random = round(x$eig_rand[1:top_k], 4),
      Retain = ifelse(1:top_k <= x$K_star, "Yes", "No")
    )
    print(comp, row.names = FALSE)
  } else {
    cat("Eigenvalue comparison:\n")
    top_k <- min(10, length(x$eig_obs))
    comp <- data.frame(
      k = 1:top_k,
      Observed = round(x$eig_obs[1:top_k], 4),
      Random = round(x$eig_rand[1:top_k], 4),
      Retain = ifelse(1:top_k <= x$K_star, "Yes", "No")
    )
    print(comp, row.names = FALSE)
  }
  invisible(x)
}

## ========================================================================
## Procrustes Alignment for Continuous Data
## ========================================================================

#' Align SVD solution via Procrustes rotation with sign anchoring
#'
#' @param B Matrix of domain loadings to align
#' @param Bref Reference matrix of domain loadings
#'
#' @return List with aligned coordinates and rotation matrix
#' @export
svd_align <- function(B, Bref) {
  B <- as.matrix(B)
  Bref <- as.matrix(Bref)
  
  if (nrow(B) != nrow(Bref) || ncol(B) != ncol(Bref)) {
    stop("B and Bref must have identical dimensions.")
  }
  
  # Orthogonal Procrustes: minimize ||B R - Bref||_F
  svp <- svd(crossprod(B, Bref))
  R <- tcrossprod(svp$u, svp$v)
  Bal <- B %*% R
  
  # Sign anchoring: maximize agreement with reference
  for (k in seq_len(ncol(Bal))) {
    if (sum(Bal[, k] * Bref[, k]) < 0) {
      Bal[, k] <- -Bal[, k]
      R[, k] <- -R[, k]
    }
  }
  
  list(B_aligned = Bal, R = R)
}

## =====================================================================
## Bootstrap Stability Assessment for Continuous Data
## =====================================================================

#' Bootstrap-Based Subspace Stability Assessment for Ipsatized SVD
#'
#' Evaluates reproducibility of retained dimensions via bootstrap resampling.
#' Uses Procrustes principal angles (subspace-level) and Tucker's congruence
#' coefficients (dimension-level).
#'
#' @param data Data frame or matrix of continuous variables
#' @param K Integer, number of dimensions to assess
#' @param B Integer, number of bootstrap resamples (default: 2000)
#' @param seed Integer, random seed for reproducibility
#' @param verbose Logical, print progress messages
#'
#' @return S3 object of class \code{svd_bootstrap}
#' @export
svd_bootstrap <- function(data, 
                          K, 
                          B = 2000, 
                          seed = 20260206,
                          verbose = TRUE) {
  
  # Prepare data
  X <- as.matrix(data)
  X <- X[complete.cases(X), , drop = FALSE]
  N <- nrow(X)
  
  # Reference fit
  fit_ref <- svd_ipsatized(X, K = K)
  
  if (K < 1 || K > ncol(fit_ref$svd$v)) {
    stop("K must be between 1 and ", ncol(fit_ref$svd$v))
  }
  
  Vref <- fit_ref$svd$v[, 1:K, drop = FALSE]
  Bref <- fit_ref$B[, 1:K, drop = FALSE]
  
  # Pre-allocate results
  angles <- matrix(NA_real_, nrow = B, ncol = K)
  tucker <- matrix(NA_real_, nrow = B, ncol = K)
  
  set.seed(seed)
  
  if (verbose) {
    cat("Running bootstrap stability assessment with B =", B, "resamples...\n")
    pb <- txtProgressBar(0, B, style = 3)
  }
  
  for (b in seq_len(B)) {
    # Bootstrap resample
    idx <- sample.int(N, size = N, replace = TRUE)
    Xb <- X[idx, , drop = FALSE]
    
    # Ipsatize and SVD
    Xb_star <- ipsatize(Xb)
    sv_b <- svd(Xb_star)
    
    Vb <- sv_b$v[, 1:K, drop = FALSE]
    Bb <- sv_b$v[, 1:K, drop = FALSE]
    
    # Principal angles: measure subspace similarity
    s <- svd(crossprod(Vref, Vb), nu = 0, nv = 0)$d
    s <- pmax(pmin(s, 1), -1)  # numerical stability
    angles[b, ] <- sort(acos(s) * 180 / pi)
    
    # Tucker congruence: measure dimension-level similarity (after alignment)
    Bal <- svd_align(Bb, Bref)$B_aligned
    tucker[b, ] <- colSums(Bal * Bref) / sqrt(colSums(Bal^2) * colSums(Bref^2))
    
    if (verbose) setTxtProgressBar(pb, b)
  }
  
  if (verbose) close(pb)
  
  # Label columns
  colnames(angles) <- paste0("theta", 1:K)
  colnames(tucker) <- paste0("phi", 1:K)
  
  # Summary statistics
  angles_summary <- summarise_matrix(angles, probs = c(0.05, 0.95))
  tucker_summary <- summarise_matrix(tucker, probs = c(0.05, 0.95))
  
  out <- structure(
    list(
      ref = fit_ref,
      K = K,
      B = B,
      angles = angles,
      tucker = tucker,
      angles_summary = angles_summary,
      tucker_summary = tucker_summary
    ),
    class = "svd_bootstrap"
  )
  
  if (verbose) {
    cat("\n\n--- Principal Angles (degrees) ---\n")
    print(round(angles_summary, 2))
    cat("\n--- Tucker Congruence Coefficients ---\n")
    print(round(tucker_summary, 3))
    cat("\nInterpretation: phi >= 0.95 excellent, >= 0.85 good, >= 0.65 fair\n")
  }
  
  invisible(out)
}

#' @export
print.svd_bootstrap <- function(x, ...) {
  cat("Ipsatized SVD Bootstrap Stability Assessment\n")
  cat("  K =", x$K, "dimensions\n")
  cat("  B =", x$B, "bootstrap resamples\n\n")
  
  cat("Principal Angles (degrees):\n")
  print(round(x$angles_summary, 2))
  
  cat("\nTucker Congruence:\n")
  print(round(x$tucker_summary, 3))
  
  invisible(x)
}

#' @export
plot.svd_bootstrap <- function(x, ...) {
  plot_subspace_stability_cont(x)
}

## ===============================================================
## cALSI Computation
## ===============================================================

#' Compute Continuous Aggregated Latent Space Index (cALSI)
#'
#' Calculates cALSI as a variance-weighted Euclidean norm of row coordinates
#' within a retained K-dimensional ipsatized SVD subspace.
#'
#' @param F Matrix of row coordinates (N x K or larger)
#' @param eig Vector of eigenvalues
#' @param K Integer, number of dimensions to aggregate
#'
#' @return S3 object of class \code{calsi} containing:
#'   \item{alpha}{Numeric vector of cALSI values (length N)}
#'   \item{w}{Variance weights (length K)}
#'   \item{alpha_vec}{Aggregated direction vector (sqrt of weights)}
#'   \item{K}{Number of dimensions used}
#'
#' @export
calsi <- function(F, eig, K) {
  F <- as.matrix(F)
  
  if (K < 1) stop("K must be >= 1")
  if (ncol(F) < K) stop("F has fewer than K columns")
  if (length(eig) < K) stop("eig has fewer than K elements")
  
  # Variance weights: proportion of retained inertia
  w <- eig[1:K] / sum(eig[1:K])
  alpha_vec <- sqrt(w)
  
  # cALSI: weighted L2 norm
  # alpha_i = sqrt(sum_k w_k * f_ik^2)
  Fk <- F[, 1:K, drop = FALSE]
  alpha_vals <- sqrt(rowSums(sweep(Fk^2, 2, w, "*")))
  
  structure(
    list(
      alpha = alpha_vals,
      w = w,
      alpha_vec = alpha_vec,
      K = K
    ),
    class = "calsi"
  )
}

#' @export
print.calsi <- function(x, ...) {
  cat("cALSI (Continuous Aggregated Latent Space Index)\n")
  cat("  K =", x$K, "dimensions\n")
  cat("  N =", length(x$alpha), "individuals\n")
  cat("  Variance weights:", round(x$w, 4), "\n")
  cat("  cALSI range: [", 
      round(min(x$alpha), 4), ", ", 
      round(max(x$alpha), 4), "]\n", sep = "")
  cat("  cALSI mean:", round(mean(x$alpha), 4), "\n")
  cat("  cALSI median:", round(median(x$alpha), 4), "\n")
  invisible(x)
}

#' @export
summary.calsi <- function(object, ...) {
  cat("cALSI Summary Statistics\n")
  print(summary(object$alpha))
  invisible(object)
}

## ==========================================================
## Visualization Functions
## ==========================================================

#' Plot Subspace Stability Diagnostics for Continuous Data
#'
#' @param boot_obj Object of class \code{svd_bootstrap}
#' @export
plot_subspace_stability_cont <- function(boot_obj) {
  if (!inherits(boot_obj, "svd_bootstrap")) {
    stop("boot_obj must be of class 'svd_bootstrap'")
  }
  
  angles <- boot_obj$angles
  tucker <- boot_obj$tucker
  
  op <- par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3, 1))
  on.exit(par(op), add = TRUE)
  
  # Principal angles boxplot
  boxplot(angles, 
          main = "Principal Angles (degrees)",
          xlab = "Dimension k", 
          ylab = expression(theta[k]),
          col = "lightblue",
          border = "darkblue")
  abline(h = 0, lty = 2, col = "gray50")
  
  # Tucker congruence boxplot
  boxplot(tucker, 
          main = "Tucker Congruence",
          xlab = "Dimension k", 
          ylab = expression(phi[k]),
          ylim = c(max(0.5, min(tucker, na.rm = TRUE) - 0.05), 1),
          col = "lightgreen",
          border = "darkgreen")
  abline(h = c(0.85, 0.95), lty = 2, col = c("orange", "red"))
  legend("bottomright", 
         legend = c("phi = 0.85 (good)", "phi = 0.95 (excellent)"),
         lty = 2, col = c("orange", "red"), bty = "n", cex = 0.8)
  
  invisible(NULL)
}

#' Plot Domain Loadings in SVD Space
#'
#' Visualizes domain loadings in a 2D subspace (biplot-style).
#'
#' @param fit SVD fit object (class \code{svd_fit})
#' @param dim_pair Integer vector of length 2, dimensions to plot
#' @param cex Character expansion for labels
#' @export
plot_domain_loadings <- function(fit, 
                                 dim_pair = c(1, 2),
                                 cex = 1.0) {
  
  if (!inherits(fit, "svd_fit")) {
    stop("fit must be of class 'svd_fit'")
  }
  
  B <- fit$B
  K <- ncol(B)
  
  d1 <- dim_pair[1]
  d2 <- dim_pair[2]
  if (d1 > K || d2 > K || d1 < 1 || d2 < 1) {
    stop("dim_pair must be within 1..K")
  }
  
  var_pct <- round(fit$var_explained * 100, 1)
  
  # Plot
  xlim <- range(c(0, B[, d1])) * 1.3
  ylim <- range(c(0, B[, d2])) * 1.3
  
  plot(0, 0, type = "n",
       xlim = xlim, ylim = ylim,
       xlab = sprintf("Dimension %d (%.1f%%)", d1, var_pct[d1]),
       ylab = sprintf("Dimension %d (%.1f%%)", d2, var_pct[d2]),
       main = sprintf("Domain Loadings (Dims %d-%d)", d1, d2),
       asp = 1)
  
  abline(h = 0, v = 0, lty = 2, col = "gray60")
  
  # Draw arrows from origin to domain coordinates
  arrows(0, 0, B[, d1], B[, d2], 
         length = 0.12, angle = 25, 
         col = "steelblue", lwd = 2)
  
  # Labels
  text(B[, d1] * 1.1, B[, d2] * 1.1, 
       labels = rownames(B), 
       cex = cex, col = "black", font = 2)
  
  invisible(NULL)
}

## ===================
## Complete Workflow
## ===================

#' Complete cALSI Workflow for Continuous Data
#'
#' Integrates parallel analysis, bootstrap stability, and cALSI computation.
#'
#' @param data Data frame or matrix of continuous variables
#' @param B_pa Number of permutations for parallel analysis
#' @param B_boot Number of bootstrap resamples
#' @param q Quantile for parallel analysis
#' @param seed Random seed
#' @param K_override Optional: override parallel analysis K* with specified value
#'
#' @return List containing all analysis objects
#' @export
calsi_workflow <- function(data,
                           B_pa = 2000,
                           B_boot = 2000,
                           q = 0.95,
                           seed = 20260206,
                           K_override = NULL) {
  
  cat("========================================\n")
  cat("cALSI Workflow (Continuous Data)\n")
  cat("========================================\n\n")
  
  # Step 1: Parallel analysis
  cat("Step 1: Parallel Analysis\n")
  cat("----------------------------------------\n")
  pa <- svd_pa(data, B = B_pa, q = q, seed = seed, verbose = TRUE)
  K <- if (!is.null(K_override)) K_override else pa$K_star
  cat("Using K =", K, "dimensions\n")
  
  cat("\n\n")
  
  # Step 2: Bootstrap stability
  cat("Step 2: Bootstrap Stability Assessment\n")
  cat("----------------------------------------\n")
  boot <- svd_bootstrap(data, K = K, B = B_boot, seed = seed, verbose = TRUE)
  plot_subspace_stability_cont(boot)
  
  cat("\n\n")
  
  # Step 3: cALSI computation
  cat("Step 3: cALSI Computation\n")
  cat("----------------------------------------\n")
  fit <- boot$ref
  calsi_obj <- calsi(fit$F, fit$eig, K = K)
  print(calsi_obj)
  
  cat("\n\n")
  
  # Step 4: Domain loadings plot
  cat("Step 4: Domain Loadings\n")
  cat("----------------------------------------\n")
  if (K >= 2) {
    plot_domain_loadings(fit, dim_pair = c(1, 2))
  }
  
  # Domain contributions (variance explained per domain)
  B <- fit$B[, 1:K, drop = FALSE]
  domain_var <- rowSums(B^2)
  domain_contrib <- domain_var / sum(domain_var) * 100
  cat("\nDomain contributions to retained subspace:\n")
  print(round(sort(domain_contrib, decreasing = TRUE), 2))
  
  cat("\n========================================\n")
  cat("Workflow complete!\n")
  cat("========================================\n")
  
  invisible(list(
    pa = pa,
    boot = boot,
    fit = fit,
    calsi = calsi_obj,
    K = K,
    domain_contrib = domain_contrib
  ))
}

## ===============================================================
## SEPA-cALSI Comparison Functions
## ===============================================================

#' Compare SEPA plane-wise summaries with cALSI
#'
#' @param fit SVD fit object
#' @param K Number of dimensions
#' @param target_ids Optional vector of person IDs to highlight
#'
#' @return Data frame comparing SEPA and cALSI person-level indices
#' @export
compare_sepa_calsi <- function(fit, K, target_ids = NULL) {
  
  F_mat <- fit$F[, 1:K, drop = FALSE]
  B_mat <- fit$B[, 1:K, drop = FALSE]
  eig <- fit$eig[1:K]
  N <- nrow(F_mat)
  p <- nrow(B_mat)
  
  # cALSI: variance-weighted norm across ALL K dimensions
  w <- eig / sum(eig)
  calsi_vals <- sqrt(rowSums(sweep(F_mat^2, 2, w, "*")))
  
  # SEPA-style: plane-wise norms
  # Plane 1: dims 1-2
  if (K >= 2) {
    norm_plane1 <- sqrt(rowSums(F_mat[, 1:2, drop = FALSE]^2))
  } else {
    norm_plane1 <- abs(F_mat[, 1])
  }
  
  # Plane 2: dims 3-4 (if available)
  if (K >= 4) {
    norm_plane2 <- sqrt(rowSums(F_mat[, 3:4, drop = FALSE]^2))
  } else if (K >= 3) {
    norm_plane2 <- abs(F_mat[, 3])
  } else {
    norm_plane2 <- rep(NA, N)
  }
  
  # SEPA SV-weighted combination (for comparison)
  if (K >= 4) {
    alpha1 <- (eig[1] + eig[2]) / sum(eig[1:K])
    alpha2 <- (eig[3] + eig[4]) / sum(eig[1:K])
    sepa_combined <- sqrt(alpha1 * norm_plane1^2 + alpha2 * norm_plane2^2)
  } else if (K >= 2) {
    sepa_combined <- norm_plane1
  } else {
    sepa_combined <- calsi_vals
  }
  
  # Build comparison data frame
  result <- data.frame(
    ID = 1:N,
    cALSI = round(calsi_vals, 4),
    SEPA_Plane1_Norm = round(norm_plane1, 4),
    SEPA_Plane2_Norm = round(norm_plane2, 4),
    SEPA_Combined = round(sepa_combined, 4)
  )
  
  # Correlation between methods
  cat("\n--- Correlation between cALSI and SEPA measures ---\n")
  cat("cALSI vs SEPA Plane1 Norm: r =", round(cor(calsi_vals, norm_plane1), 4), "\n")
  if (!all(is.na(norm_plane2))) {
    cat("cALSI vs SEPA Plane2 Norm: r =", round(cor(calsi_vals, norm_plane2, use = "complete"), 4), "\n")
  }
  cat("cALSI vs SEPA Combined:    r =", round(cor(calsi_vals, sepa_combined, use = "complete"), 4), "\n")
  
  # If target IDs provided, show their values
  if (!is.null(target_ids)) {
    cat("\n--- Selected individuals ---\n")
    print(result[target_ids, ], row.names = FALSE)
  }
  
  invisible(result)
}

## ===============================================================
## What cALSI Adds Beyond SEPA
## ===============================================================

#' Demonstrate what cALSI adds beyond SEPA
#'
#' @param data Data matrix
#' @param K Number of dimensions
#' @param B_boot Bootstrap samples for stability
#' @param seed Random seed
#'
#' @return List with comparison results
#' @export
calsi_vs_sepa_demo <- function(data, K = 4, B_boot = 2000, seed = 20260206) {
  
  cat("\n=====================================================\n")
  cat("What cALSI Adds Beyond SEPA: A Demonstration\n")
  cat("=====================================================\n\n")
  
  # Fit model
  X <- as.matrix(data)
  X <- X[complete.cases(X), , drop = FALSE]
  
  fit <- svd_ipsatized(X, K = K)
  
  # 1. Stability-validated dimension selection
  cat("1. STABILITY-VALIDATED DIMENSION SELECTION\n")
  cat("-------------------------------------------\n")
  cat("SEPA uses parallel analysis only.\n")
  cat("cALSI adds: Bootstrap stability assessment with Tucker's phi.\n\n")
  
  boot <- svd_bootstrap(X, K = K, B = B_boot, seed = seed, verbose = FALSE)
  cat("Tucker congruence by dimension:\n")
  print(round(boot$tucker_summary, 3))
  
  stable_dims <- which(boot$tucker_summary[, "median"] >= 0.85)
  cat("\nStable dimensions (phi >= 0.85):", paste(stable_dims, collapse = ", "), "\n")
  cat("This ensures aggregation only over replicable dimensions.\n\n")
  
  # 2. Principled cross-plane aggregation
  cat("2. PRINCIPLED CROSS-PLANE AGGREGATION\n")
  cat("--------------------------------------\n")
  cat("SEPA provides plane-wise norms and SV-weighted combination.\n")
  cat("cALSI provides: Single stability-validated index with clear variance interpretation.\n\n")
  
  calsi_obj <- calsi(fit$F, fit$eig, K = K)
  cat("cALSI weights (proportion of retained variance):\n")
  cat("  ", paste(paste0("Dim", 1:K, "=", round(calsi_obj$w, 3)), collapse = ", "), "\n\n")
  
  # 3. Direct comparison
  cat("3. NUMERICAL COMPARISON\n")
  cat("-----------------------\n")
  comp <- compare_sepa_calsi(fit, K)
  
  # 4. Interpretation advantage
  cat("\n4. INTERPRETIVE ADVANTAGE\n")
  cat("-------------------------\n")
  cat("cALSI = single number per person representing weighted distance\n")
  cat("       from the centroid in the stable pattern subspace.\n")
  cat("Larger cALSI = more differentiated (non-flat) profile.\n\n")
  
  cat("SEPA provides richer plane-specific information (cosines, rho).\n")
  cat("cALSI provides a SINGLE validated summary for downstream analysis.\n\n")
  
  cat("RECOMMENDATION: Use both!\n")
  cat("- SEPA for detailed plane-wise interpretation\n")
  cat("- cALSI for a single person-level index in regression/prediction\n")
  
  invisible(list(
    fit = fit,
    boot = boot,
    calsi = calsi_obj,
    comparison = comp
  ))
}
