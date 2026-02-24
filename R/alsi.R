## =======================================================================
## ALSI: Aggregated Latent Space Index for MCA
## Core functions for stability-validated aggregation
## =======================================================================

## ---- Utility Functions ----
#' @importFrom stats complete.cases quantile median model.matrix setNames
#' @importFrom graphics plot lines legend abline boxplot text par
#' @importFrom utils txtProgressBar setTxtProgressBar
NULL

#' Read Excel file with fallback options
#' @keywords internal
.read_xlsx <- function(path) {
  if (requireNamespace("readxl", quietly = TRUE)) {
    return(as.data.frame(readxl::read_xlsx(path)))
  }
  if (requireNamespace("openxlsx", quietly = TRUE)) {
    return(as.data.frame(openxlsx::read.xlsx(path)))
  }
  stop("Please install 'readxl' or 'openxlsx' to read .xlsx files.")
}

#' Convert various formats to binary 0/1
#' @keywords internal
to01 <- function(x) {
  if (is.logical(x)) return(as.integer(x))
  if (is.factor(x)) x <- as.character(x)
  
  if (is.character(x)) {
    x <- tolower(trimws(x))
    x <- ifelse(x %in% c("true", "t", "yes", "y", "1"), 1L,
                ifelse(x %in% c("false", "f", "no", "n", "0"), 0L, NA_integer_))
  } else {
    x <- as.integer(x)
  }
  
  if (any(!is.na(x) & !x %in% c(0L, 1L))) {
    stop("Non-binary values detected; recode to 0/1.")
  }
  x
}

#' Create disjunctive (indicator) matrix from binary data
#' @keywords internal
make_disjunctive <- function(X01) {
  X01 <- as.data.frame(X01)
  
  out <- lapply(names(X01), function(v) {
    f <- factor(X01[[v]], levels = c(0, 1))
    mm <- model.matrix(~ f - 1, data = data.frame(f = f))
    colnames(mm) <- paste0(v, "_", c("0", "1"))
    mm
  })
  
  Xd <- do.call(cbind, out)
  storage.mode(Xd) <- "numeric"
  Xd
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

## ---- Core MCA Engine ----

#' Perform Multiple Correspondence Analysis on binary indicator matrix
#' 
#' @param Xbin01 Data frame or matrix with binary (0/1) variables
#' @return List containing MCA results with eigenvalues, coordinates, and masses
#' @keywords internal
mca_indicator <- function(Xbin01) {
  # Validate and convert to binary
  Xbin01 <- as.data.frame(Xbin01)
  Xbin01[] <- lapply(Xbin01, to01)
  Xbin01 <- Xbin01[complete.cases(Xbin01), , drop = FALSE]
  
  N <- nrow(Xbin01)
  J <- ncol(Xbin01)
  
  if (N < 2) stop("Need at least 2 complete rows for MCA.")
  if (J < 2) stop("Need at least 2 variables for MCA.")
  
  # Create disjunctive coding
  Xd <- make_disjunctive(Xbin01)
  M <- ncol(Xd)
  
  # Correspondence matrix
  P <- Xd / (N * J)
  r <- rep(1 / N, N)  # row masses (constant for indicator matrix)
  c <- colSums(P)      # column masses
  
  if (any(c <= 0)) stop("Zero column mass; check input coding.")
  
  # Standardized residual matrix: S = Dr^{-1/2} (P - rc') Dc^{-1/2}
  # Since r_i = 1/N constant, Dr^{-1/2} = sqrt(N) * I
  Pc <- P - tcrossprod(r, c)
  Dc_inv_sqrt <- diag(1 / sqrt(c), nrow = M)
  S <- sqrt(N) * Pc %*% Dc_inv_sqrt
  
  # SVD
  sv <- svd(S)
  eig <- sv$d^2
  
  # Principal coordinates
  # Row: Fmat = Dr^{-1/2} U Sigma = sqrt(N) U Sigma
  if (length(sv$d) == 1) {
    Fmat <- sqrt(N) * sv$u * sv$d
  } else {
    Fmat <- sqrt(N) * sv$u %*% diag(sv$d)
  }
  
  # Column: G = Dc^{-1/2} V Sigma
  if (length(sv$d) == 1) {
    G <- Dc_inv_sqrt %*% sv$v * sv$d
  } else {
    G <- Dc_inv_sqrt %*% sv$v %*% diag(sv$d)
  }
  rownames(G) <- colnames(Xd)
  
  structure(
    list(
      Xbin = Xbin01,
      Xd = Xd,
      N = N,
      J = J,
      M = M,
      eig = eig,
      svd = sv,
      masses = list(r = r, c = c),
      F = Fmat,
      G = G
    ),
    class = "mca_fit"
  )
}

## ---- S3 Methods for mca_fit ----

#' @export
print.mca_fit <- function(x, ...) {
  cat("MCA fit:\n")
  cat("  N =", x$N, "individuals\n")
  cat("  J =", x$J, "binary variables\n")
  cat("  M =", x$M, "categories (disjunctive coding)\n")
  cat("  Total inertia:", round(sum(x$eig), 4), "\n")
  cat("  Top 5 eigenvalues:", round(x$eig[1:min(5, length(x$eig))], 4), "\n")
  invisible(x)
}

## =====================================================================
## Main Functions (Exported API)
## =====================================================================

#' Parallel Analysis for MCA Dimensionality Assessment
#'
#' Compares observed MCA eigenvalues against reference distributions from
#' permuted data to identify statistically meaningful dimensions.
#'
#' @param data Data frame or path to .xlsx file
#' @param vars Character vector of binary variable names
#' @param B Integer, number of permutations (default: 2000)
#' @param q Numeric, reference quantile for retention (default: 0.95)
#' @param seed Integer, random seed for reproducibility
#' @param max_dims Integer, maximum dimensions to display in plot
#' @param verbose Logical, print progress messages
#'
#' @return S3 object of class \code{mca_pa} containing:
#'   \item{eig_obs}{Observed eigenvalues from the MCA of the original data}
#'   \item{eig_q}{Reference quantiles from permutation distribution}
#'   \item{eig_perm}{Matrix of permutation eigenvalues (B x dimensions)}
#'   \item{K_star}{Suggested number of dimensions to retain (where observed > reference)}
#'   \item{fit}{MCA fit object (class \code{mca_fit}) from original data}
#'   \item{q}{Quantile threshold used for comparison}
#'   \item{B}{Number of permutations performed}
#'
#' @export
#' @examples
#' \donttest{
#' # Using included ANR2 dataset
#' data(ANR2)
#' vars <- c("MDD", "DYS", "DEP", "PTSD", "OCD", "GAD", "ANX", "SOPH", "ADHD")
#' pa <- mca_pa(ANR2, vars = vars, B = 100)
#' print(pa$K_star)
#' }
mca_pa <- function(data, 
                   vars, 
                   B = 2000, 
                   q = 0.95, 
                   seed = 20260123,
                   max_dims = 20, 
                   verbose = TRUE) {
  
  # Load and prepare data
  dat <- if (is.character(data)) .read_xlsx(data) else as.data.frame(data)
  
  if (!all(vars %in% names(dat))) {
    missing <- setdiff(vars, names(dat))
    stop("Missing variables: ", paste(missing, collapse = ", "))
  }
  
  # Clean data once
  Xbin <- dat[, vars, drop = FALSE]
  Xbin[] <- lapply(Xbin, to01)
  Xbin <- Xbin[complete.cases(Xbin), , drop = FALSE]
  
  # Reference MCA
  fit0 <- mca_indicator(Xbin)
  eig_obs <- fit0$eig
  Kmax <- length(eig_obs)
  
  # Permutation loop
  eig_perm <- matrix(NA_real_, nrow = B, ncol = Kmax)
  set.seed(seed)
  
  if (verbose) {
    cat("Running parallel analysis with B =", B, "permutations...\n")
    pb <- txtProgressBar(0, B, style = 3)
  }
  
  for (b in seq_len(B)) {
    # Permute within each variable
    Xp <- as.data.frame(lapply(Xbin, sample, replace = FALSE))
    eig_perm[b, ] <- mca_indicator(Xp)$eig
    
    if (verbose) setTxtProgressBar(pb, b)
  }
  
  if (verbose) close(pb)
  
  # Determine retention
  eig_q <- apply(eig_perm, 2, quantile, probs = q, na.rm = TRUE)
  K_star <- sum(round(eig_obs, 8) > round(eig_q, 8))
  
  # Create output object
  out <- structure(
    list(
      eig_obs = eig_obs,
      eig_q = eig_q,
      eig_perm = eig_perm,
      q = q,
      B = B,
      K_star = K_star,
      fit = fit0
    ),
    class = "mca_pa"
  )
  
  if (verbose) {
    cat("\nParallel Analysis complete.\n")
    cat("Suggested K* (dims where eig_obs > q =", q, ") =", K_star, "\n\n")
  }
  
  # Generate diagnostic plot
  Kplot <- min(max_dims, Kmax)
  plot(1:Kplot, eig_obs[1:Kplot], 
       type = "b", pch = 19, col = "black",
       xlab = "Dimension k", 
       ylab = "Eigenvalue (inertia)",
       main = sprintf("MCA Parallel Analysis (B=%d, q=%.2f)", B, q),
       ylim = range(c(eig_obs[1:Kplot], eig_q[1:Kplot])))
  lines(1:Kplot, eig_q[1:Kplot], type = "b", pch = 1, col = "blue")
  legend("topright", 
         legend = c("Observed", sprintf("PA q=%.2f", q)),
         lty = 1, pch = c(19, 1), col = c("black", "blue"),
         bty = "n")
  
  invisible(out)
}

#' @export
print.mca_pa <- function(x, ...) {
  cat("MCA Parallel Analysis\n")
  cat("  B =", x$B, "permutations\n")
  cat("  Quantile threshold: q =", x$q, "\n")
  cat("  Suggested K* =", x$K_star, "\n")
  cat("  Top eigenvalues:\n")
  top_k <- min(10, length(x$eig_obs))
  comp <- data.frame(
    k = 1:top_k,
    Observed = round(x$eig_obs[1:top_k], 4),
    Reference = round(x$eig_q[1:top_k], 4),
    Retain = ifelse(1:top_k <= x$K_star, "Yes", "No")
  )
  print(comp, row.names = FALSE)
  invisible(x)
}

## ========================================================================
## Procrustes Alignment
## ========================================================================

#' Align MCA solution via Procrustes rotation with sign anchoring
#'
#' Performs orthogonal Procrustes rotation to align a set of category 
#' coordinates to a reference solution, then applies sign correction to
#' maximize agreement with the reference on each dimension.
#'
#' @param G Matrix of category coordinates to align (M x K)
#' @param Gref Reference matrix of category coordinates (M x K)
#'
#' @return List containing:
#'   \item{G_aligned}{Matrix of aligned category coordinates (M x K), 
#'     rotated and sign-corrected to match the reference}
#'   \item{R}{Orthogonal rotation matrix (K x K) used for alignment}
#'
#' @export
#' @examples
#' # Create example matrices
#' set.seed(123)
#' Gref <- matrix(rnorm(20), nrow = 10, ncol = 2)
#' G <- Gref %*% matrix(c(0.8, 0.6, -0.6, 0.8), 2, 2)
#' 
#' # Align G to Gref
#' aligned <- mca_align(G, Gref)
#' print(aligned$G_aligned)
mca_align <- function(G, Gref) {
  G <- as.matrix(G)
  Gref <- as.matrix(Gref)
  
  if (nrow(G) != nrow(Gref) || ncol(G) != ncol(Gref)) {
    stop("G and Gref must have identical dimensions.")
  }
  
  # Orthogonal Procrustes: minimize ||G R - Gref||_F
  # Solution: R = U V' from SVD(G' Gref) = U Sigma V'
  svp <- svd(crossprod(G, Gref))
  R <- tcrossprod(svp$u, svp$v)
  Gal <- G %*% R
  
  # Sign anchoring: maximize agreement with reference
  for (k in seq_len(ncol(Gal))) {
    if (sum(Gal[, k] * Gref[, k]) < 0) {
      Gal[, k] <- -Gal[, k]
    }
  }
  
  list(G_aligned = Gal, R = R)
}

## =====================================================================
## Bootstrap Stability Assessment
## =====================================================================

#' Bootstrap-Based Subspace Stability Assessment
#'
#' Evaluates reproducibility of retained MCA dimensions via bootstrap resampling.
#' Quantifies stability using Procrustes principal angles (subspace-level) and
#' Tucker's congruence coefficients (dimension-level).
#'
#' @param data Data frame or path to .xlsx file
#' @param vars Character vector of binary variable names
#' @param K Integer, number of dimensions to retain and assess
#' @param B Integer, number of bootstrap resamples (default: 2000)
#' @param seed Integer, random seed for reproducibility
#' @param verbose Logical, print progress messages
#'
#' @return S3 object of class \code{mca_bootstrap} containing:
#'   \item{ref}{Reference MCA fit object (class \code{mca_fit})}
#'   \item{K}{Number of dimensions assessed}
#'   \item{B}{Number of bootstrap resamples performed}
#'   \item{angles}{Matrix of principal angles in degrees (B x K), measuring
#'     subspace similarity between bootstrap and reference solutions}
#'   \item{tucker}{Matrix of Tucker congruence coefficients (B x K), measuring
#'     dimension-level similarity after Procrustes alignment}
#'   \item{angles_summary}{Summary statistics (median, 5th, 95th percentiles) 
#'     for principal angles}
#'   \item{tucker_summary}{Summary statistics (median, 5th, 95th percentiles) 
#'     for Tucker congruence coefficients}
#'
#' @export
#' @examples
#' \donttest{
#' # Using included ANR2 dataset
#' data(ANR2)
#' vars <- c("MDD", "DYS", "DEP", "PTSD", "OCD", "GAD", "ANX", "SOPH", "ADHD")
#' boot <- mca_bootstrap(ANR2, vars = vars, K = 3, B = 100)
#' print(boot)
#' }
mca_bootstrap <- function(data, 
                          vars, 
                          K, 
                          B = 2000, 
                          seed = 20260123,
                          verbose = TRUE) {
  
  # Load and prepare data
  dat <- if (is.character(data)) .read_xlsx(data) else as.data.frame(data)
  
  if (!all(vars %in% names(dat))) {
    missing <- setdiff(vars, names(dat))
    stop("Missing variables: ", paste(missing, collapse = ", "))
  }
  
  # Clean data once
  Xbin <- dat[, vars, drop = FALSE]
  Xbin[] <- lapply(Xbin, to01)
  Xbin <- Xbin[complete.cases(Xbin), , drop = FALSE]
  
  # Reference fit
  fit_ref <- mca_indicator(Xbin)
  
  if (K < 1 || K > ncol(fit_ref$svd$v)) {
    stop("K must be between 1 and ", ncol(fit_ref$svd$v))
  }
  
  N <- fit_ref$N
  Vref <- fit_ref$svd$v[, 1:K, drop = FALSE]
  Gref <- fit_ref$G[, 1:K, drop = FALSE]
  
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
    Xb <- fit_ref$Xbin[idx, , drop = FALSE]
    fit_b <- mca_indicator(Xb)
    
    # Extract K-dimensional solutions
    Vb <- fit_b$svd$v[, 1:K, drop = FALSE]
    Gb <- fit_b$G[, 1:K, drop = FALSE]
    
    # Principal angles: measure subspace similarity
    # Computed from singular values of V_ref' V_b
    s <- svd(crossprod(Vref, Vb), nu = 0, nv = 0)$d
    s <- pmax(pmin(s, 1), -1)  # numerical stability
    angles[b, ] <- sort(acos(s) * 180 / pi)
    
    # Tucker congruence: measure dimension-level similarity (after alignment)
    Gal <- mca_align(Gb, Gref)$G_aligned
    tucker[b, ] <- colSums(Gal * Gref) / sqrt(colSums(Gal^2) * colSums(Gref^2))
    
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
    class = "mca_bootstrap"
  )
  
  if (verbose) {
    cat("\n\n--- Principal Angles (degrees) ---\n")
    print(round(angles_summary, 2))
    cat("\n--- Tucker Congruence Coefficients ---\n")
    print(round(tucker_summary, 3))
    cat("\n")
  }
  
  invisible(out)
}

#' @export
print.mca_bootstrap <- function(x, ...) {
  cat("MCA Bootstrap Stability Assessment\n")
  cat("  K =", x$K, "dimensions\n")
  cat("  B =", x$B, "bootstrap resamples\n\n")
  
  cat("Principal Angles (degrees):\n")
  print(round(x$angles_summary, 2))
  
  cat("\nTucker Congruence:\n")
  print(round(x$tucker_summary, 3))
  
  invisible(x)
}

#' @export
plot.mca_bootstrap <- function(x, ...) {
  plot_subspace_stability(x)
}

## ===============================================================
## ALSI Computation
## ===============================================================

#' Compute Aggregated Latent Space Index (ALSI)
#'
#' Calculates ALSI as a variance-weighted Euclidean norm of row principal
#' coordinates within a retained K-dimensional MCA subspace.
#'
#' @param Fmat Matrix of row principal coordinates (N x K or larger)
#' @param eig Vector of eigenvalues (inertias)
#' @param K Integer, number of dimensions to aggregate
#'
#' @return S3 object of class \code{alsi} containing:
#'   \item{alpha}{Numeric vector of ALSI values (length N), representing
#'     each individual's variance-weighted distance from the centroid 
#'     in the retained MCA subspace}
#'   \item{w}{Variance weights (length K), computed as the proportion of 
#'     retained inertia for each dimension}
#'   \item{alpha_vec}{Aggregated direction vector (length K), equal to 
#'     sqrt(w), used for projecting category coordinates}
#'   \item{K}{Number of dimensions used in aggregation}
#'
#' @export
#' @examples
#' # Create example data
#' set.seed(123)
#' Fmat <- matrix(rnorm(100 * 4), nrow = 100, ncol = 4)
#' eig <- c(0.5, 0.3, 0.15, 0.05)
#' 
#' # Compute ALSI
#' a <- alsi(Fmat, eig, K = 3)
#' print(a)
#' hist(a$alpha, main = "Distribution of ALSI")
alsi <- function(Fmat, eig, K) {
  Fmat <- as.matrix(Fmat)
  
  if (K < 1) stop("K must be >= 1")
  if (ncol(Fmat) < K) stop("Fmat has fewer than K columns")
  if (length(eig) < K) stop("eig has fewer than K elements")
  
  # Variance weights: proportion of retained inertia
  w <- eig[1:K] / sum(eig[1:K])
  alpha_vec <- sqrt(w)
  
  # ALSI: weighted L2 norm
  # alpha_i = sqrt(sum_k w_k * f_ik^2)
  Fk <- Fmat[, 1:K, drop = FALSE]
  alpha_vals <- sqrt(rowSums(sweep(Fk^2, 2, w, "*")))
  
  structure(
    list(
      alpha = alpha_vals,
      w = w,
      alpha_vec = alpha_vec,
      K = K
    ),
    class = "alsi"
  )
}

#' @export
print.alsi <- function(x, ...) {
  cat("ALSI (Aggregated Latent Space Index)\n")
  cat("  K =", x$K, "dimensions\n")
  cat("  N =", length(x$alpha), "individuals\n")
  cat("  Variance weights:", round(x$w, 4), "\n")
  cat("  ALSI range: [", 
      round(min(x$alpha), 4), ", ", 
      round(max(x$alpha), 4), "]\n", sep = "")
  cat("  ALSI mean:", round(mean(x$alpha), 4), "\n")
  cat("  ALSI median:", round(median(x$alpha), 4), "\n")
  invisible(x)
}

#' @export
summary.alsi <- function(object, ...) {
  cat("ALSI Summary Statistics\n")
  print(summary(object$alpha))
  invisible(object)
}

## ==========================================================
## Visualization Functions
## ==========================================================

#' Plot Subspace Stability Diagnostics
#'
#' Creates diagnostic plots showing distributions of principal angles
#' and Tucker congruence coefficients across bootstrap resamples.
#'
#' @param boot_obj Object of class \code{mca_bootstrap}
#'
#' @return No return value, called for side effects. The function creates 
#'   a two-panel figure with: (1) boxplots of principal angles (left panel), 
#'   showing the distribution of subspace similarity across bootstrap resamples 
#'   for each dimension; and (2) boxplots of Tucker congruence coefficients 
#'   (right panel), showing dimension-level replicability with reference lines 
#'   at phi = 0.85 (good) and phi = 0.95 (excellent).
#'
#' @export
#' @examples
#' \donttest{
#' data(ANR2)
#' vars <- c("MDD", "DYS", "DEP", "PTSD", "OCD", "GAD", "ANX", "SOPH", "ADHD")
#' boot <- mca_bootstrap(ANR2, vars = vars, K = 3, B = 100)
#' plot_subspace_stability(boot)
#' }
plot_subspace_stability <- function(boot_obj) {
  if (!inherits(boot_obj, "mca_bootstrap")) {
    stop("boot_obj must be of class 'mca_bootstrap'")
  }
  
  angles <- boot_obj$angles
  tucker <- boot_obj$tucker
  
  # Save original par settings
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
         legend = c("phi = 0.85 (fair)", "phi = 0.95 (excellent)"),
         lty = 2, col = c("orange", "red"), bty = "n", cex = 0.8)
  
  invisible(NULL)
}

#' Plot Category Projections in MCA Space
#'
#' Visualizes category coordinates in a 2D MCA subspace and optionally
#' displays projections onto the aggregated ALSI direction.
#'
#' @param fit MCA fit object (class \code{mca_fit})
#' @param K Number of dimensions in retained subspace
#' @param alpha_vec Optional aggregated direction vector (from \code{alsi()})
#' @param dim_pair Integer vector of length 2, dimensions to plot (default: c(1,2))
#' @param cex Character expansion for labels
#' @param top_n Number of top categories to display by projection (default: 15)
#'
#' @return No return value, called for side effects. The function creates 
#'   a scatter plot of category coordinates in the specified 2D subspace, 
#'   with category labels displayed. If \code{alpha_vec} is provided, it also 
#'   prints the top categories ranked by their absolute projection onto the 
#'   ALSI direction to the console.
#'
#' @export
#' @examples
#' \donttest{
#' data(ANR2)
#' vars <- c("MDD", "DYS", "DEP", "PTSD", "OCD", "GAD", "ANX", "SOPH", "ADHD")
#' pa <- mca_pa(ANR2, vars = vars, B = 100, verbose = FALSE)
#' fit <- pa$fit
#' plot_category_projections(fit, K = pa$K_star)
#' }
plot_category_projections <- function(fit, 
                                      K, 
                                      alpha_vec = NULL, 
                                      dim_pair = c(1, 2),
                                      cex = 0.8,
                                      top_n = 15) {
  
  if (!inherits(fit, "mca_fit")) {
    fit <- structure(fit, class = "mca_fit")  # try to coerce
  }
  
  Gk <- fit$G[, 1:K, drop = FALSE]
  
  # Validate dim_pair
  d1 <- dim_pair[1]
  d2 <- dim_pair[2]
  if (d1 > K || d2 > K || d1 < 1 || d2 < 1) {
    stop("dim_pair must be within 1..K")
  }
  
  # Category biplot
  plot(Gk[, d1], Gk[, d2], 
       pch = 19, col = "steelblue",
       xlab = paste0("Dimension ", d1),
       ylab = paste0("Dimension ", d2),
       main = sprintf("MCA Category Map (Dims %d-%d)", d1, d2))
  text(Gk[, d1], Gk[, d2], 
       labels = rownames(Gk), 
       pos = 3, cex = cex, col = "black")
  abline(h = 0, v = 0, lty = 2, col = "gray60")
  
  # Aggregated direction projections
  if (!is.null(alpha_vec)) {
    if (length(alpha_vec) != K) {
      stop("alpha_vec must have length K")
    }
    
    proj <- drop(Gk %*% alpha_vec)
    ord <- order(abs(proj), decreasing = TRUE)
    
    cat("\nTop", min(top_n, length(proj)), "categories by |projection onto ALSI direction|:\n")
    top_idx <- ord[1:min(top_n, length(proj))]
    top_proj <- data.frame(
      Category = rownames(Gk)[top_idx],
      Projection = round(proj[top_idx], 4)
    )
    print(top_proj, row.names = FALSE)
  }
  
  invisible(NULL)
}

## ===================
## Example Workflow
## ===================

#' Complete ALSI Analysis Workflow
#'
#' Runs a complete ALSI analysis including parallel analysis for dimensionality
#' assessment, bootstrap stability evaluation, ALSI computation, and visualization.
#'
#' @param data Data frame or path to .xlsx file
#' @param vars Character vector of binary variable names
#' @param B_pa Number of permutations for parallel analysis (default: 2000)
#' @param B_boot Number of bootstrap resamples (default: 2000)
#' @param q Quantile for parallel analysis (default: 0.95)
#' @param seed Random seed for reproducibility
#'
#' @return List (returned invisibly) containing all analysis objects:
#'   \item{pa}{Parallel analysis results (class \code{mca_pa})}
#'   \item{boot}{Bootstrap stability results (class \code{mca_bootstrap})}
#'   \item{fit}{MCA fit object (class \code{mca_fit})}
#'   \item{alsi}{ALSI values (class \code{alsi})}
#'   \item{K}{Number of dimensions retained based on parallel analysis}
#'
#' @export
#' @examples
#' \donttest{
#' data(ANR2)
#' vars <- c("MDD", "DYS", "DEP", "PTSD", "OCD", "GAD", "ANX", "SOPH", "ADHD")
#' results <- alsi_workflow(
#'   data   = ANR2,
#'   vars   = vars,
#'   B_pa   = 100,
#'   B_boot = 100
#' )
#' results$pa
#' results$boot
#' results$alsi
#' }
alsi_workflow <- function(data,
                          vars,
                          B_pa = 2000,
                          B_boot = 2000,
                          q = 0.95,
                          seed = 20260123) {
  
  cat("========================================\n")
  cat("ALSI Workflow\n")
  cat("========================================\n\n")
  
  # Step 1: Parallel analysis
  cat("Step 1: Parallel Analysis\n")
  cat("----------------------------------------\n")
  pa <- mca_pa(data, vars, B = B_pa, q = q, seed = seed, verbose = TRUE)
  K <- pa$K_star
  
  cat("\n\n")
  
  # Step 2: Bootstrap stability
  cat("Step 2: Bootstrap Stability Assessment\n")
  cat("----------------------------------------\n")
  boot <- mca_bootstrap(data, vars, K = K, B = B_boot, seed = seed, verbose = TRUE)
  plot_subspace_stability(boot)
  
  cat("\n\n")
  
  # Step 3: ALSI computation
  cat("Step 3: ALSI Computation\n")
  cat("----------------------------------------\n")
  fit <- boot$ref
  alsi_obj <- alsi(fit$F, fit$eig, K = K)
  print(alsi_obj)
  
  cat("\n\n")
  
  # Step 4: Category projections
  cat("Step 4: Category Projections\n")
  cat("----------------------------------------\n")
  plot_category_projections(fit, K = K, alpha_vec = alsi_obj$alpha_vec)
  
  cat("\n========================================\n")
  cat("Workflow complete!\n")
  cat("========================================\n")
  
  invisible(list(
    pa = pa,
    boot = boot,
    fit = fit,
    alsi = alsi_obj,
    K = K
  ))
}