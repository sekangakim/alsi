## =======================================================================
## ALSI: Aggregated Latent Space Index for MCA
## Refactored version with improved efficiency and JSS standards
## =======================================================================
setwd("/Users/sekangkim/Documents/J Sub/MCA/")
## ---- Package Dependencies ----
#' @importFrom stats complete.cases quantile median svd
#' @importFrom graphics plot lines legend abline boxplot text par
#' @importFrom utils txtProgressBar setTxtProgressBar

## ---- Utility Functions ----

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
  # Row: F = Dr^{-1/2} U Sigma = sqrt(N) U Sigma
  if (length(sv$d) == 1) {
    F <- sqrt(N) * sv$u * sv$d
  } else {
    F <- sqrt(N) * sv$u %*% diag(sv$d)
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
      F = F,
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
#'   \item{eig_obs}{Observed eigenvalues}
#'   \item{eig_q}{Reference quantiles from permutation}
#'   \item{K_star}{Suggested number of dimensions to retain}
#'   \item{fit}{MCA fit object (class \code{mca_fit})}
#'   \item{q}{Quantile used}
#'   \item{B}{Number of permutations}
#'
#' @export
#' @examples
#' \dontrun{
#' # Using data frame
#' pa <- mca_pa(mydata, vars = c("var1", "var2", "var3"))
#' 
#' # Using Excel file
#' pa <- mca_pa("data.xlsx", vars = c("var1", "var2", "var3"), B = 1000)
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
  K_star <- sum(eig_obs > eig_q)
  
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
#' @param G Matrix of category coordinates to align
#' @param Gref Reference matrix of category coordinates
#'
#' @return List with aligned coordinates and rotation matrix
#' @export
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
#'   \item{ref}{Reference MCA fit}
#'   \item{K}{Number of dimensions assessed}
#'   \item{B}{Number of bootstrap resamples}
#'   \item{angles}{Matrix of principal angles (B × K)}
#'   \item{tucker}{Matrix of Tucker congruence coefficients (B × K)}
#'   \item{angles_summary}{Summary statistics for angles}
#'   \item{tucker_summary}{Summary statistics for congruence}
#'
#' @export
#' @examples
#' \dontrun{
#' boot <- mca_bootstrap(mydata, vars = c("v1", "v2", "v3"), K = 3, B = 1000)
#' print(boot)
#' plot(boot)
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
#' @param F Matrix of row principal coordinates (N × K or larger)
#' @param eig Vector of eigenvalues (inertias)
#' @param K Integer, number of dimensions to aggregate
#'
#' @return S3 object of class \code{alsi} containing:
#'   \item{alpha}{Numeric vector of ALSI values (length N)}
#'   \item{w}{Variance weights (length K)}
#'   \item{alpha_vec}{Aggregated direction vector (sqrt of weights)}
#'   \item{K}{Number of dimensions used}
#'
#' @export
#' @examples
#' \dontrun{
#' fit <- mca_indicator(mydata)
#' a <- alsi(fit$F, fit$eig, K = 3)
#' hist(a$alpha, main = "Distribution of ALSI")
#' }
alsi <- function(F, eig, K) {
  F <- as.matrix(F)
  
  if (K < 1) stop("K must be >= 1")
  if (ncol(F) < K) stop("F has fewer than K columns")
  if (length(eig) < K) stop("eig has fewer than K elements")
  
  # Variance weights: proportion of retained inertia
  w <- eig[1:K] / sum(eig[1:K])
  alpha_vec <- sqrt(w)
  
  # ALSI: weighted L2 norm
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
#' @export
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
         legend = c("φ = 0.85 (fair)", "φ = 0.95 (excellent)"),
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
#' @export
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
       main = sprintf("MCA Category Map (Dims %d–%d)", d1, d2))
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

#' Example workflow using the ALSI package
#'
#' @param path Path to data file (.xlsx) or data frame
#' @param vars Character vector of binary variable names
#' @param B_pa Number of permutations for parallel analysis
#' @param B_boot Number of bootstrap resamples
#' @param q Quantile for parallel analysis
#' @param seed Random seed
#'
#' @return List containing all analysis objects
#' @export
#' @examples
#' \dontrun{
#' # Complete workflow
#' results <- alsi_workflow(
#'   path = "ANR2.xlsx",
#'   vars = c("MDD", "DYS", "DEP", "PTSD", "OCD", "GAD", "ANX", "SOPH", "ADHD"),
#'   B_pa = 2000,
#'   B_boot = 2000
#' )
#' 
#' # Access components
#' results$pa       # Parallel analysis
#' results$boot     # Bootstrap stability
#' results$alsi     # ALSI values
#' }
alsi_workflow <- function(path,
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
  pa <- mca_pa(path, vars, B = B_pa, q = q, seed = seed, verbose = TRUE)
  K <- pa$K_star
  
  cat("\n\n")
  
  # Step 2: Bootstrap stability
  cat("Step 2: Bootstrap Stability Assessment\n")
  cat("----------------------------------------\n")
  boot <- mca_bootstrap(path, vars, K = K, B = B_boot, seed = seed, verbose = TRUE)
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

## ==============================
## Complete ALSI Workflow Example
## Using ANR2.xlsx data
## ==============================

# Source the refactored ALSI functions
# source("alsi_refactored.R")

# Define variables for MCA
vars <- c("MDD", "DYS", "DEP", "PTSD", "OCD", "GAD", "ANX", "SOPH", "ADHD")

# Run complete workflow
results <- alsi_workflow(
  path = "ANR2.xlsx",
  vars = vars,
  B_pa = 2000,
  B_boot = 2000
)

# Extract ALSI values
alpha_values <- results$alsi$alpha

# Load data for downstream analysis
dat <- readxl::read_xlsx("ANR2.xlsx")

# Add ALSI to dataset
stopifnot(nrow(dat) == length(alpha_values))
dat$alpha <- alpha_values

## ========================================
## Downstream Analysis Examples
## ========================================

# Note: Column names are lowercase: pre_bmi, post_bmi, pre_EDI, post_EDI

## Example 1: BMI model
cat("\n========================================\n")
cat("Example 1: BMI Model\n")
cat("========================================\n")

# Complete cases only for BMI
dat_bmi <- dat[complete.cases(dat[, c("pre_bmi", "post_bmi", "alpha")]), ]
cat("N =", nrow(dat_bmi), "with complete BMI data\n\n")

fit_bmi <- lm(post_bmi ~ pre_bmi + alpha, data = dat_bmi)
summary(fit_bmi)

## Example 2: EDI model  
cat("\n========================================\n")
cat("Example 2: EDI Model\n")
cat("========================================\n")

# Complete cases only for EDI
dat_edi <- dat[complete.cases(dat[, c("pre_EDI", "post_EDI", "alpha")]), ]
cat("N =", nrow(dat_edi), "with complete EDI data\n\n")

fit_edi <- lm(post_EDI ~ pre_EDI + alpha, data = dat_edi)
summary(fit_edi)

## Example 3: ALSI descriptives by diagnosis groups
cat("\n========================================\n")
cat("Example 3: ALSI by MDD Status\n")
cat("========================================\n")

cat("\nALSI Summary Statistics:\n")
cat("Overall:\n")
print(summary(dat$alpha))

cat("\nBy MDD status:\n")
cat("MDD = 0 (No MDD):\n")
print(summary(dat$alpha[dat$MDD == 0]))

cat("\nMDD = 1 (MDD present):\n")
print(summary(dat$alpha[dat$MDD == 1]))

# t-test
t_result <- t.test(alpha ~ MDD, data = dat)
cat("\nt-test (MDD vs no MDD):\n")
print(t_result)

## Example 4: Visualize ALSI distribution
cat("\n========================================\n")
cat("Example 4: ALSI Distribution\n")
cat("========================================\n")

# Histogram
hist(dat$alpha, 
     breaks = 30,
     main = "Distribution of ALSI",
     xlab = "ALSI (Aggregated Latent Space Index)",
     col = "lightblue",
     border = "darkblue")
abline(v = median(dat$alpha), col = "red", lwd = 2, lty = 2)
legend("topright", 
       legend = paste("Median =", round(median(dat$alpha), 3)),
       col = "red", lty = 2, lwd = 2, bty = "n")

# Boxplot by MDD
boxplot(alpha ~ MDD, 
        data = dat,
        main = "ALSI by MDD Status",
        xlab = "MDD (0 = No, 1 = Yes)",
        ylab = "ALSI",
        col = c("lightgreen", "lightcoral"),
        border = c("darkgreen", "darkred"))

## Example 5: Correlation with pre-treatment severity
cat("\n========================================\n")
cat("Example 5: ALSI Correlations\n")
cat("========================================\n")

cat("\nCorrelation with pre-treatment EDI:\n")
cor_edi <- cor.test(dat$alpha, dat$pre_EDI)
print(cor_edi)

cat("\nCorrelation with pre-treatment BMI:\n")
cor_bmi <- cor.test(dat$alpha, dat$pre_bmi)
print(cor_bmi)

## Example 6: Multiple regression with diagnostic variables
cat("\n========================================\n")
cat("Example 6: Multiple Regression\n")
cat("========================================\n")

# Predict post-EDI from pre-EDI, ALSI, and other diagnoses
fit_full <- lm(post_EDI ~ pre_EDI + alpha + MDD + DYS + PTSD + OCD + GAD + ANX, 
               data = dat)
summary(fit_full)

fit_alpha <- lm(post_EDI ~ pre_EDI + alpha, 
               data = dat)
summary(fit_alpha)
## Save results
cat("\n========================================\n")
cat("Saving Results\n")
cat("========================================\n")

# Save dataset with ALSI
write.csv(dat, "ANR2_with_ALSI.csv", row.names = FALSE)
cat("Dataset with ALSI saved to: ANR2_with_ALSI.csv\n")

# Save ALSI object
saveRDS(results, "alsi_results.rds")
cat("Full results object saved to: alsi_results.rds\n")

cat("\n========================================\n")
cat("Analysis Complete!\n")
cat("========================================\n")
