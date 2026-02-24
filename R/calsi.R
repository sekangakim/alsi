## =======================================================================
## cALSI: Continuous Aggregated Latent Space Index
## Adaptation of ALSI for continuous multivariate data via ipsatized SVD
## Designed to complement SEPA (Segmented Profile Analysis)
## =======================================================================

#' @importFrom stats sd cor quantile median complete.cases
#' @importFrom graphics boxplot abline legend arrows text plot par
#' @importFrom utils txtProgressBar setTxtProgressBar
NULL

## ---- Utility Functions ----

#' Ipsatize (row-center) a matrix
#' 
#' Row-centers a data matrix by subtracting each person's mean from their scores.
#' This removes individual differences in overall level, isolating pattern effects.
#'
#' @param X Numeric matrix or data frame (persons x domains)
#' @return Row-centered matrix where each person's mean is zero
#' @keywords internal
#' @examples
#' \dontrun{
#' X <- matrix(c(1,2,3, 4,5,6), nrow=2, byrow=TRUE)
#' ipsatize(X)
#' }
ipsatize <- function(X) {
  X <- as.matrix(X)
  sweep(X, 1, rowMeans(X, na.rm = TRUE), "-")
}

#' Summarize matrix columns (median and quantiles)
#' @keywords internal
summarise_matrix <- function(X, probs = c(0.05, 0.95)) {
  t(apply(X, 2, function(x) {
    c(median = median(x, na.rm = TRUE),
      setNames(quantile(x, probs = probs, na.rm = TRUE), 
               paste0("p", probs * 100)))
  }))
}

## ---- Core SVD Engine for Continuous Data ----

#' Perform ipsatized SVD on continuous data (SEPA-style)
#' 
#' Conducts singular value decomposition on row-centered (ipsatized) continuous data.
#' This isolates within-person pattern variation from between-person level differences.
#'
#' @param X Data frame or matrix with continuous variables (persons x domains)
#' @param K Number of dimensions to retain (default: all available)
#' @return List of class \code{svd_fit} containing:
#'   \item{X}{Original data matrix}
#'   \item{Xstar}{Ipsatized (row-centered) data matrix}
#'   \item{N}{Number of individuals}
#'   \item{p}{Number of domains/variables}
#'   \item{K}{Number of dimensions retained}
#'   \item{eig}{Eigenvalues (squared singular values)}
#'   \item{var_explained}{Proportion of variance explained by each dimension}
#'   \item{svd}{Full SVD object}
#'   \item{F}{Person coordinates (N x K matrix)}
#'   \item{B}{Domain loadings (p x K matrix)}
#'   \item{domains}{Variable names}
#' @keywords internal
#' @examples
#' \dontrun{
#' data <- matrix(rnorm(100*5), ncol=5)
#' fit <- svd_ipsatized(data, K=3)
#' print(fit)
#' }
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
#' @param verbose Logical, print progress messages (default: TRUE)
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
#' The paran package should be installed: \code{install.packages("paran")}
#'
#' @export
#' @examples
#' \dontrun{
#' data <- matrix(rnorm(500*10), ncol=10)
#' pa_result <- svd_pa(data, B=1000, seed=123)
#' print(pa_result$K_star)
#' }
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
        eig_adj = eig_obs_scaled,
        eig_rand = eig_q,
        K_star = K_star,
        fit = fit0,
        method = "fallback",
        q = q,
        B = B
      ),
      class = "svd_pa"
    )
    
    if (verbose) {
      cat("\n\nRetained dimensions K* =", K_star, "\n\n")
    }
    
    return(invisible(out))
  }
}

#' @export
print.svd_pa <- function(x, ...) {
  cat("Parallel Analysis for Ipsatized SVD\n")
  cat("  Method:", x$method, "\n")
  cat("  Iterations:", x$B, "\n")
  cat("  Centile:", x$q, "\n")
  cat("  Retained dimensions: K* =", x$K_star, "\n")
  invisible(x)
}

## =====================================================================
## Bootstrap Stability Assessment
## =====================================================================

#' Procrustes rotation to align two matrices
#' @keywords internal
procrustes_rotation <- function(X, Y) {
  # Find R such that Y %*% R ~= X
  # Using SVD: X'Y = UDV', then R = VU'
  s <- svd(t(X) %*% Y)
  R <- s$v %*% t(s$u)
  return(R)
}

#' Principal angles between column spaces of two matrices
#' @keywords internal
principal_angles_rad <- function(A, B) {
  # QR decomposition for orthonormal bases
  QA <- qr.Q(qr(A))
  QB <- qr.Q(qr(B))
  
  # SVD of Q_A' * Q_B
  sv <- svd(t(QA) %*% QB, nu = 0, nv = 0)$d
  
  # Clamp to [0,1] for numerical stability
  sv <- pmin(1.0, pmax(0.0, sv))
  
  # Return angles in radians
  acos(sv)
}

#' Tucker's congruence coefficient
#' @keywords internal
tucker_congruence <- function(x, y) {
  sum(x * y) / sqrt(sum(x^2) * sum(y^2))
}

#' Bootstrap Stability Assessment for SVD
#'
#' Assesses the stability of SVD dimensions through bootstrap resampling,
#' computing principal angles and Tucker's congruence coefficients.
#'
#' @param data Data frame or matrix of continuous variables
#' @param K Number of dimensions to assess
#' @param B Number of bootstrap samples (default: 2000)
#' @param seed Random seed for reproducibility
#' @param verbose Logical, print progress (default: TRUE)
#'
#' @return S3 object of class \code{svd_bootstrap} containing:
#'   \item{ref}{Reference SVD fit object}
#'   \item{angles}{Bootstrap distribution of principal angles (B x K matrix, in degrees)}
#'   \item{tucker}{Bootstrap distribution of Tucker's phi (B x K matrix)}
#'   \item{angles_summary}{Summary statistics for angles}
#'   \item{tucker_summary}{Summary statistics for Tucker's phi}
#'   \item{K}{Number of dimensions assessed}
#'   \item{B}{Number of bootstrap samples}
#'
#' @export
#' @examples
#' \dontrun{
#' data <- matrix(rnorm(500*10), ncol=10)
#' boot_result <- svd_bootstrap(data, K=3, B=1000, seed=123)
#' plot_subspace_stability_cont(boot_result)
#' }
svd_bootstrap <- function(data, 
                          K, 
                          B = 2000, 
                          seed = 20260206,
                          verbose = TRUE) {
  
  X <- as.matrix(data)
  X <- X[complete.cases(X), , drop = FALSE]
  N <- nrow(X)
  
  # Reference fit
  ref <- svd_ipsatized(X, K = K)
  V_ref <- ref$svd$v[, 1:K, drop = FALSE]
  
  # Storage
  angles_mat <- matrix(NA_real_, nrow = B, ncol = K)
  tucker_mat <- matrix(NA_real_, nrow = B, ncol = K)
  
  set.seed(seed)
  
  if (verbose) {
    cat("Bootstrap validation (", B, " iterations)...\n", sep = "")
    pb <- txtProgressBar(0, B, style = 3)
  }
  
  for (b in seq_len(B)) {
    # Resample
    idx <- sample(N, replace = TRUE)
    Xb <- X[idx, , drop = FALSE]
    
    # Fit bootstrap SVD
    fit_b <- svd_ipsatized(Xb, K = K)
    V_b <- fit_b$svd$v[, 1:K, drop = FALSE]
    
    # Procrustes align bootstrap to reference
    R <- procrustes_rotation(V_ref, V_b)
    V_b_aligned <- V_b %*% R
    
    # Principal angles (in radians, then convert to degrees)
    angles_rad <- principal_angles_rad(V_ref, V_b_aligned)
    angles_mat[b, ] <- angles_rad * (180 / pi)
    
    # Tucker's congruence
    for (k in seq_len(K)) {
      tucker_mat[b, k] <- tucker_congruence(V_ref[, k], V_b_aligned[, k])
    }
    
    if (verbose) setTxtProgressBar(pb, b)
  }
  
  if (verbose) close(pb)
  
  # Summaries
  angles_summary <- summarise_matrix(angles_mat)
  tucker_summary <- summarise_matrix(tucker_mat)
  
  rownames(angles_summary) <- paste0("Dim", 1:K)
  rownames(tucker_summary) <- paste0("Dim", 1:K)
  
  structure(
    list(
      ref = ref,
      angles = angles_mat,
      tucker = tucker_mat,
      angles_summary = angles_summary,
      tucker_summary = tucker_summary,
      K = K,
      B = B
    ),
    class = "svd_bootstrap"
  )
}

#' @export
print.svd_bootstrap <- function(x, ...) {
  cat("Bootstrap Stability Assessment (Continuous Data)\n")
  cat("  K =", x$K, "dimensions\n")
  cat("  B =", x$B, "bootstrap samples\n\n")
  
  cat("Principal Angles (degrees):\n")
  print(round(x$angles_summary, 2))
  cat("\n")
  
  cat("Tucker's Congruence:\n")
  print(round(x$tucker_summary, 3))
  
  invisible(x)
}

## =====================================================================
## cALSI Index Computation
## =====================================================================

#' Compute cALSI (Continuous Aggregated Latent Space Index)
#'
#' Computes individual-level profile differentiation indices as variance-weighted
#' Euclidean norms in the validated latent space.
#'
#' @param F Matrix of person coordinates (N x K)
#' @param eig Vector of eigenvalues
#' @param K Number of dimensions to use
#'
#' @return S3 object of class \code{calsi} containing:
#'   \item{alpha}{Vector of cALSI values (length N)}
#'   \item{w}{Variance weights for each dimension}
#'   \item{alpha_vec}{Square root of variance weights}
#'   \item{K}{Number of dimensions used}
#'   \item{eig}{Eigenvalues used}
#'
#' @details
#' cALSI values are computed as:
#' \deqn{\alpha_i = \sqrt{\sum_{k=1}^{K} w_k f_{ik}^2}}
#' where \eqn{w_k = \lambda_k / \sum_{k=1}^{K} \lambda_k} are variance weights.
#'
#' Higher values indicate more differentiated (non-flat) profiles.
#'
#' @export
#' @examples
#' \dontrun{
#' data <- matrix(rnorm(500*10), ncol=10)
#' fit <- svd_ipsatized(data, K=3)
#' calsi_result <- calsi(fit$F, fit$eig, K=3)
#' summary(calsi_result$alpha)
#' }
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
      K = K,
      eig = eig[1:K]
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
  cat("========================\n\n")
  
  cat("Sample size: N =", length(object$alpha), "\n")
  cat("Dimensions: K =", object$K, "\n\n")
  
  cat("Distribution:\n")
  print(summary(object$alpha))
  cat("\nSD:", round(sd(object$alpha), 4), "\n")
  
  if (requireNamespace("moments", quietly = TRUE)) {
    cat("Skewness:", round(moments::skewness(object$alpha), 3), "\n")
    cat("Kurtosis:", round(moments::kurtosis(object$alpha), 3), "\n")
  }
  
  cat("\nVariance Weights:\n")
  for (k in 1:object$K) {
    cat(sprintf("  Dim %d: w = %.4f (%.1f%%)\n", 
                k, object$w[k], object$w[k] * 100))
  }
  
  invisible(object)
}

## ==========================================================
## Visualization Functions
## ==========================================================

#' Plot Subspace Stability Diagnostics for Continuous Data
#'
#' Creates side-by-side boxplots of principal angles and Tucker's congruence
#' from bootstrap stability assessment.
#'
#' @param boot_obj Object of class \code{svd_bootstrap}
#' @export
#' @examples
#' \dontrun{
#' data <- matrix(rnorm(500*10), ncol=10)
#' boot_result <- svd_bootstrap(data, K=3, B=1000)
#' plot_subspace_stability_cont(boot_result)
#' }
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
  abline(h = 10, lty = 2, col = "orange")
  legend("topright", 
         legend = "θ = 10° threshold",
         lty = 2, col = "orange", bty = "n", cex = 0.8)
  
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
         legend = c("φ = 0.85 (good)", "φ = 0.95 (excellent)"),
         lty = 2, col = c("orange", "red"), bty = "n", cex = 0.8)
  
  invisible(NULL)
}

#' Plot Domain Loadings in SVD Space
#'
#' Visualizes domain loadings in a 2D subspace (biplot-style).
#'
#' @param fit SVD fit object (class \code{svd_fit})
#' @param dim_pair Integer vector of length 2, dimensions to plot (default: c(1,2))
#' @param cex Character expansion for labels (default: 1.0)
#' @export
#' @examples
#' \dontrun{
#' data <- matrix(rnorm(500*10), ncol=10)
#' fit <- svd_ipsatized(data, K=3)
#' plot_domain_loadings(fit, dim_pair=c(1,2))
#' }
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
#' Integrates parallel analysis, bootstrap stability assessment, and cALSI
#' computation into a single streamlined workflow.
#'
#' @param data Data frame or matrix of continuous variables (persons x domains)
#' @param B_pa Number of permutations for parallel analysis (default: 2000)
#' @param B_boot Number of bootstrap resamples for stability (default: 2000)
#' @param phi_threshold Tucker's congruence threshold for stability (default: 0.85)
#' @param angle_threshold Principal angle threshold in degrees (default: 10)
#' @param ipsatize Logical, whether to ipsatize data (default: TRUE; always recommended)
#' @param q Quantile for parallel analysis (default: 0.95)
#' @param seed Random seed for reproducibility (default: 20260206)
#' @param K_override Optional integer to override parallel analysis K* with specified value
#' @param verbose Logical, print progress messages (default: TRUE)
#'
#' @return List containing all analysis objects:
#'   \item{pa}{Parallel analysis results (class \code{svd_pa})}
#'   \item{boot}{Bootstrap stability results (class \code{svd_bootstrap})}
#'   \item{ref}{Reference SVD fit (class \code{svd_fit})}
#'   \item{calsi}{cALSI results (class \code{calsi})}
#'   \item{K}{Final number of dimensions used}
#'   \item{domain_contrib}{Domain contributions to retained subspace (percentages)}
#'
#' @details
#' This function executes the complete cALSI pipeline:
#' 
#' 1. **Parallel Analysis**: Determines initial dimensionality K* using permutation-based
#'    parallel analysis (paran package if available, fallback otherwise)
#' 
#' 2. **Bootstrap Stability**: Assesses stability of K dimensions via B bootstrap resamples,
#'    computing principal angles (subspace-level stability) and Tucker's congruence
#'    (dimension-level stability)
#' 
#' 3. **cALSI Computation**: Calculates individual profile differentiation indices as
#'    variance-weighted Euclidean norms across the K validated dimensions
#' 
#' 4. **Diagnostics**: Displays stability plots and domain contribution summaries
#'
#' @export
#' @examples
#' \dontrun{
#' # Generate simulated data
#' set.seed(123)
#' data <- matrix(rnorm(500*10, mean=50, sd=10), ncol=10)
#' colnames(data) <- paste0("Domain", 1:10)
#' 
#' # Run complete workflow
#' result <- calsi_workflow(data, B_pa=1000, B_boot=1000, seed=123)
#' 
#' # Extract cALSI values
#' alpha_i <- result$calsi$alpha
#' summary(alpha_i)
#' }
calsi_workflow <- function(data,
                           B_pa = 2000,
                           B_boot = 2000,
                           phi_threshold = 0.85,
                           angle_threshold = 10,
                           ipsatize = TRUE,
                           q = 0.95,
                           seed = 20260206,
                           K_override = NULL,
                           verbose = TRUE) {
  
  if (!ipsatize) {
    warning("ipsatize=FALSE not recommended. cALSI is designed for ipsatized data.")
  }
  
  if (verbose) {
    cat("========================================\n")
    cat("cALSI Workflow (Continuous Data)\n")
    cat("========================================\n\n")
  }
  
  # Step 1: Parallel analysis
  if (verbose) {
    cat("Step 1: Parallel Analysis\n")
    cat("----------------------------------------\n")
  }
  
  pa <- svd_pa(data, B = B_pa, q = q, seed = seed, 
               graph = TRUE, verbose = verbose)
  
  K <- if (!is.null(K_override)) {
    if (verbose) cat("Using user-specified K =", K_override, "\n")
    K_override
  } else {
    if (verbose) cat("Retained dimensions K* =", pa$K_star, "\n")
    pa$K_star
  }
  
  if (verbose) cat("\n")
  
  # Step 2: Bootstrap stability
  if (verbose) {
    cat("Step 2: Bootstrap Stability Assessment\n")
    cat("----------------------------------------\n")
  }
  
  boot <- svd_bootstrap(data, K = K, B = B_boot, seed = seed, verbose = verbose)
  
  if (verbose) {
    cat("\n")
    plot_subspace_stability_cont(boot)
    cat("\n")
  }
  
  # Check stability criteria
  if (verbose) {
    cat("Stability Criteria:\n")
    cat("  Angle threshold: θ < ", angle_threshold, "°\n", sep = "")
    cat("  Tucker threshold: φ ≥ ", phi_threshold, "\n\n", sep = "")
    
    angles_med <- apply(boot$angles, 2, median)
    tucker_med <- apply(boot$tucker, 2, median)
    
    cat("Median Stability Diagnostics:\n")
    for (k in 1:K) {
      meets_angle <- angles_med[k] < angle_threshold
      meets_tucker <- tucker_med[k] >= phi_threshold
      status <- if (meets_angle && meets_tucker) "✓" else "✗"
      cat(sprintf("  Dim %d: θ = %5.2f°, φ = %.3f %s\n", 
                  k, angles_med[k], tucker_med[k], status))
    }
    cat("\n")
  }
  
  # Step 3: cALSI computation
  if (verbose) {
    cat("Step 3: cALSI Computation\n")
    cat("----------------------------------------\n")
  }
  
  ref <- boot$ref
  calsi_obj <- calsi(ref$F, ref$eig, K = K)
  
  if (verbose) {
    print(calsi_obj)
    cat("\n")
  }
  
  # Step 4: Domain loadings
  if (verbose) {
    cat("Step 4: Domain Loadings\n")
    cat("----------------------------------------\n")
  }
  
  if (K >= 2 && verbose) {
    plot_domain_loadings(ref, dim_pair = c(1, 2))
  }
  
  # Domain contributions (variance explained per domain)
  B_mat <- ref$B[, 1:K, drop = FALSE]
  domain_var <- rowSums(B_mat^2)
  domain_contrib <- domain_var / sum(domain_var) * 100
  
  if (verbose) {
    cat("\nDomain contributions to retained subspace:\n")
    contrib_sorted <- sort(domain_contrib, decreasing = TRUE)
    for (i in 1:length(contrib_sorted)) {
      cat(sprintf("  %s: %.1f%%\n", names(contrib_sorted)[i], contrib_sorted[i]))
    }
    cat("\n========================================\n")
    cat("Workflow complete!\n")
    cat("========================================\n")
  }
  
  invisible(list(
    pa = pa,
    boot = boot,
    ref = ref,
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
#' Compares SEPA's plane-wise indices with cALSI's unified index.
#'
#' @param fit SVD fit object (class \code{svd_fit})
#' @param K Number of dimensions
#' @param target_ids Optional vector of person IDs to highlight
#'
#' @return Data frame comparing SEPA and cALSI person-level indices
#' @export
#' @examples
#' \dontrun{
#' data <- matrix(rnorm(500*10), ncol=10)
#' fit <- svd_ipsatized(data, K=4)
#' comparison <- compare_sepa_calsi(fit, K=4, target_ids=c(1,50,100))
#' }
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
#' Provides a comprehensive comparison showing the advantages of cALSI's
#' stability-validated approach over traditional SEPA analysis.
#'
#' @param data Data matrix (persons x domains)
#' @param K Number of dimensions (default: 4)
#' @param B_boot Bootstrap samples for stability (default: 2000)
#' @param seed Random seed (default: 20260206)
#'
#' @return List with comparison results
#' @export
#' @examples
#' \dontrun{
#' data <- matrix(rnorm(500*10), ncol=10)
#' demo_results <- calsi_vs_sepa_demo(data, K=4, B_boot=1000)
#' }
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
