## =============================================================================
## R/ordinal.R  -  alsi package v0.2.0
##
## alsi_workflow_ordinal(): four-stage ordinal ALSI via homals ALS.
## Ported from BFI_ordinal_analysis_homals_v8.R (validated pipeline).
##
## Pipeline
##   Stage 1  Permutation parallel analysis            -> K_PA
##   Stage 2  Reference homals + varimax on cat scores -> C_ref, Z_ref
##   Stage 3  Bootstrap dual-criterion stability
##            (angle < threshold AND Tucker >= threshold,
##             BOTH on same post-Procrustes cat-score matrix) -> K*
##   Stage 4  Eigenvalue-weighted linear ALSI index
##
## WHY homals OVER MCAvariants
##   MCAvariants OMCA imposes equal-interval polynomial contrasts.
##   homals level="ordinal" uses ALS, estimating category quantifications
##   subject only to the monotonicity constraint (empirically unequal spacing).
##   Takane, Young & de Leeuw (1977, Psychometrika, 42, 7-67).
##
## Verified homals 1.0.11 slot names
##   fit$objscores  [n x ndim]         person scores          USE THIS
##   fit$catscores  list p x [c x dim] cat quantifications    USE THIS (rbind)
##   fit$eigenvalues [ndim]            real eigenvalues       USE THIS
##   fit$discrim    [p x ndim]         near-zero values       DO NOT USE
##   fit$scoremat   [n x p x ndim]     3-D array              DO NOT USE
##
## Shared helpers: all .alsi_* functions are in R/utils.R.
##
## @importFrom homals homals
## @importFrom stats varimax complete.cases quantile median sd
## =============================================================================


## ---------------------------------------------------------------------------
## Internal: fit homals, extract Z / C / lambda
## ---------------------------------------------------------------------------

#' Fit homals and return person scores, stacked category scores, eigenvalues
#'
#' Converts ordered-factor columns to plain integers before calling homals.
#' Passing ordered factors directly causes ALS to collapse to a trivial
#' zero-discrimination solution (verified in homals 1.0.11).
#'
#' @param X       Ordered-factor data frame.
#' @param ndim    Integer. Number of dimensions to extract.
#' @param suppress_warnings Logical. Muffle "Loss function increases" warnings
#'   (expected on permuted data; meaningful on real data).
#' @param itermax Integer. Maximum ALS iterations (default 1000).
#' @return List: Z [n x ndim], C [P x ndim], lambda [ndim], fit (raw object).
#' @keywords internal
.alsi_fit_homals <- function(X, ndim, suppress_warnings = FALSE,
                             itermax = 1000L) {
  X_int <- as.data.frame(
    lapply(X, function(x) as.integer(as.character(x))),
    check.names = FALSE)
  fn <- function()
    homals::homals(X_int,
                   ndim    = ndim,
                   level   = rep("ordinal", ncol(X_int)),
                   itermax = itermax)
  fit <- if (suppress_warnings) suppressWarnings(fn()) else fn()
  for (sl in c("objscores", "catscores", "eigenvalues"))
    if (!sl %in% names(fit))
      stop("homals: slot '", sl, "' not found. names(fit): ",
           paste(names(fit), collapse = ", "))
  Z <- as.matrix(fit$objscores)[, seq_len(ndim), drop = FALSE]
  C <- do.call(rbind, lapply(fit$catscores, function(m)
    matrix(as.numeric(m), nrow(m), ndim)[, seq_len(ndim), drop = FALSE]))
  lambda <- as.numeric(fit$eigenvalues)[seq_len(ndim)]
  if (anyNA(Z) || anyNA(C) || anyNA(lambda))
    stop("homals returned NA in objscores, catscores, or eigenvalues.")
  list(Z = Z, C = C, lambda = lambda, fit = fit)
}


## ---------------------------------------------------------------------------
## Exported entry point
## ---------------------------------------------------------------------------

#' Ordinal ALSI pipeline via homals ALS optimal scaling
#'
#' Runs the four-stage ordinal ALSI pipeline:
#' \enumerate{
#'   \item Permutation parallel analysis (column-wise shuffle preserves
#'         marginals, destroys inter-item structure) determines K_PA.
#'   \item Reference homals fit followed by varimax rotation on the stacked
#'         category score matrix (the loading analogue in homogeneity
#'         analysis). The same rotation matrix is applied to person scores.
#'   \item Bootstrap dual-criterion stability. For each resample, homals is
#'         refitted and the category score matrix is Procrustes-aligned to
#'         the reference. Principal angle and Tucker congruence phi are
#'         computed on the \emph{same} post-Procrustes matrix. K* is the
#'         largest k where ALL dimensions 1..k satisfy BOTH criteria
#'         simultaneously.
#'   \item Eigenvalue-weighted linear ALSI index from K* retained rotated
#'         person scores (result can be negative; z-standardized version
#'         also returned).
#' }
#'
#' @param data          A \code{data.frame} containing item columns.
#' @param items         Character vector of item column names.
#' @param reversed_items Character vector of items to reverse-score
#'   (\eqn{x' = \text{scale\_min} + \text{scale\_max} - x}) before analysis.
#' @param scale_min     Integer. Lowest valid response value (default 1).
#' @param scale_max     Integer. Highest valid response value (default 5).
#' @param n_permutations Integer. Permutation replicates for Stage 1 (100).
#' @param pa_percentile Numeric. Null-distribution percentile cutoff (95).
#' @param B_boot        Integer. Bootstrap replicates for Stage 3 (1000).
#' @param angle_threshold_deg Numeric. Max principal angle in degrees for
#'   a dimension to pass the stability criterion (default 20).
#' @param tucker_threshold Numeric. Min Tucker congruence phi for a dimension
#'   to pass the replicability criterion (default 0.85).
#' @param seed          Integer. Random seed (default 12345).
#' @param itermax       Integer. Max ALS iterations passed to homals (1000).
#' @param verbose       Logical. Print progress messages (default TRUE).
#'
#' @return An S3 object of class \code{"alsi_ordinal"} with components:
#' \describe{
#'   \item{ALSI_index}{Numeric vector (n). Raw eigenvalue-weighted linear
#'     composite. Can be negative.}
#'   \item{ALSI_z}{Numeric vector (n). Z-standardized ALSI.}
#'   \item{K_PA}{Integer. Dimensions retained by parallel analysis.}
#'   \item{K_star}{Integer. Final model order after dual-criterion selection.}
#'   \item{Z_ref}{Matrix n x K_PA. Varimax-rotated person scores.}
#'   \item{C_ref}{Matrix P x K_PA. Varimax-rotated stacked category scores.}
#'   \item{lambda_rot}{Numeric vector (K_PA). Eigenvalues (invariant to
#'     varimax rotation).}
#'   \item{stability_table}{Data frame. Per-dimension stability metrics
#'     (eigenvalue, angle, Tucker phi, pass/fail, grade).}
#'   \item{pa_table}{Data frame. Parallel analysis results per dimension.}
#'   \item{n_skipped}{Integer. Bootstrap replicates discarded due to
#'     non-convergence or degenerate resamples.}
#'   \item{call}{The matched call.}
#' }
#'
#' @references
#'   de Leeuw, J., & Mair, P. (2009). Gifi methods for optimal scaling in R:
#'   The package homals. \emph{Journal of Statistical Software}, 31(4), 1-21.
#'
#'   Gifi, A. (1990). \emph{Nonlinear multivariate analysis}. Wiley.
#'
#'   Lorenzo-Seva, U., & ten Berge, J. M. F. (2006). Tucker's congruence
#'   coefficient as a meaningful index of factor similarity.
#'   \emph{Methodology}, 2, 57-64.
#'
#'   Takane, Y., Young, F. W., & de Leeuw, J. (1977). Nonmetric individual
#'   differences multidimensional scaling: An alternating least squares
#'   method with optimal scaling features. \emph{Psychometrika}, 42, 7-67.
#'
#' @importFrom homals homals
#' @importFrom stats varimax complete.cases quantile median sd
#' @export
alsi_workflow_ordinal <- function(
    data,
    items,
    reversed_items      = character(0L),
    scale_min           = 1L,
    scale_max           = 5L,
    n_permutations      = 100L,
    pa_percentile       = 95,
    B_boot              = 1000L,
    angle_threshold_deg = 20.0,
    tucker_threshold    = 0.85,
    seed                = 12345L,
    itermax             = 1000L,
    verbose             = TRUE) {

  mc <- match.call()

  ## ---- 0. Validate --------------------------------------------------------
  if (!is.data.frame(data)) stop("`data` must be a data.frame.")
  miss <- setdiff(items, names(data))
  if (length(miss))
    stop("Items not found in `data`: ", paste(miss, collapse = ", "))

  ## ---- 0a. Reverse-score --------------------------------------------------
  X <- data[, items, drop = FALSE]
  if (length(reversed_items))
    X <- .alsi_reverse_score(X, reversed_items, scale_min, scale_max)

  ## ---- 0b. Ordered factors + listwise deletion ----------------------------
  X_ord <- as.data.frame(lapply(X, .alsi_make_ordered), check.names = FALSE)
  cc    <- complete.cases(X_ord)
  if (any(!cc) && verbose)
    message(sprintf("Listwise deletion: %d row(s) removed.", sum(!cc)))
  X_ord      <- X_ord[cc, , drop = FALSE]
  X_levels   <- lapply(X_ord, levels)
  n          <- nrow(X_ord)
  p          <- ncol(X_ord)
  total_cats <- sum(sapply(X_ord, nlevels))

  if (verbose)
    message(sprintf("N = %d  |  items = %d  |  total categories = %d  |  ratio = %.1f:1",
                    n, p, total_cats, n / total_cats))
  if (n <= 2L) stop("Sample size too small for analysis (N = ", n, ").")

  .alsi_check_rare(X_ord, items, n, verbose = verbose)

  ## ---- 0c. Slot diagnostic (quick ndim=2 test) ----------------------------
  if (verbose) message("Slot diagnostic (ndim = 2 test fit)...")
  tst <- suppressWarnings(.alsi_fit_homals(X_ord, 2L, itermax = itermax))
  if (verbose)
    message(sprintf("  objscores %s  |  catscores %s  |  eigenvalues: %s",
                    paste(dim(tst$Z), collapse = "x"),
                    paste(dim(tst$C), collapse = "x"),
                    paste(round(tst$lambda, 4L), collapse = ", ")))
  rm(tst)

  ## =========================================================================
  ## STAGE 1: Permutation parallel analysis -> K_PA
  ## =========================================================================
  if (verbose)
    message(sprintf("\n=== Stage 1: Parallel analysis (%d permutations) ===",
                    n_permutations))

  ndim_max   <- min(n - 1L, total_cats - p)
  if (verbose) message("Max extractable dimensions: ", ndim_max)

  ref_full   <- .alsi_fit_homals(X_ord, ndim_max, itermax = itermax)
  lambda_obs <- ref_full$lambda

  set.seed(seed)
  null_lam <- matrix(NA_real_, n_permutations, ndim_max)
  for (pm in seq_len(n_permutations)) {
    Xp <- X_ord
    for (j in seq_len(p)) Xp[[j]] <- sample(Xp[[j]])
    fp <- tryCatch(
      .alsi_fit_homals(Xp, ndim_max, suppress_warnings = TRUE,
                       itermax = itermax),
      error = function(e) NULL)
    if (!is.null(fp))
      null_lam[pm, seq_along(fp$lambda)] <- fp$lambda
    if (verbose && pm %% 50L == 0L)
      message(sprintf("  permutation %d / %d", pm, n_permutations))
  }

  null_p95 <- apply(null_lam, 2L, quantile, pa_percentile / 100,
                    na.rm = TRUE)
  pa_pass  <- lambda_obs > null_p95
  K_PA     <- sum(cumprod(as.integer(pa_pass)) == 1L)
  if (K_PA == 0L) {
    warning("Parallel analysis retained 0 dimensions; setting K_PA = 1.")
    K_PA <- 1L
  }

  pa_table <- data.frame(
    Dimension    = seq_len(ndim_max),
    Observed     = round(lambda_obs, 5L),
    Null_p95     = round(null_p95,   5L),
    Exceeds_null = pa_pass,
    stringsAsFactors = FALSE)

  if (verbose)
    message(sprintf("K_PA = %d (consecutive dims where observed > null p%d)",
                    K_PA, pa_percentile))
  K_max <- K_PA

  ## =========================================================================
  ## STAGE 2: Reference homals + varimax on category scores
  ## =========================================================================
  if (verbose)
    message(sprintf("\n=== Stage 2: Reference homals (K = %d) ===", K_max))

  ref <- .alsi_fit_homals(X_ord, K_max, itermax = itermax)

  ## Varimax applied to CATEGORY scores (loading analogue).
  ## Same rotation matrix R_ref applied to person scores.
  ## Bootstrap solutions: Procrustes alignment only (no per-bootstrap varimax).
  if (K_max == 1L) {
    R_ref      <- matrix(1, 1L, 1L)
    C_ref      <- ref$C
    Z_ref      <- ref$Z
    lambda_rot <- ref$lambda
    if (verbose) message("K_max = 1: varimax not applicable (identity).")
  } else {
    vx         <- varimax(ref$C, normalize = FALSE)
    R_ref      <- vx$rotmat
    C_ref      <- ref$C %*% R_ref   # P x K_max
    Z_ref      <- ref$Z %*% R_ref   # n x K_max
    lambda_rot <- ref$lambda        # eigenvalues invariant to rotation
    if (verbose) message("Varimax applied to stacked category score matrix.")
  }
  M_ref <- C_ref   # reference for Procrustes + Tucker phi

  ## =========================================================================
  ## STAGE 3: Bootstrap dual-criterion stability -> K*
  ##
  ## Per replicate:
  ##   1. Fit homals -> C_b_raw
  ##   2. Procrustes-align C_b_raw to M_ref -> M_b_al
  ##   3. Principal angle: M_ref vs M_b_al (post-Procrustes residual)
  ##   4. Tucker phi:      M_ref vs M_b_al (same matrix)
  ## "Loss function increases" -> non-convergent ALS -> skip replicate.
  ## 50 re-draw attempts per replicate to ensure all levels present.
  ## =========================================================================
  if (verbose)
    message(sprintf(
      "\n=== Stage 3: Bootstrap stability (B = %d | angle < %.0f deg | Tucker >= %.2f) ===",
      B_boot, angle_threshold_deg, tucker_threshold))

  set.seed(seed + 1L)
  ang_mat <- matrix(NA_real_, B_boot, K_max)
  tuc_mat <- matrix(NA_real_, B_boot, K_max)
  n_skip  <- 0L

  for (b in seq_len(B_boot)) {
    ok <- FALSE;  fb <- NULL
    for (tri in seq_len(50L)) {
      idx <- sample.int(n, n, replace = TRUE)
      Xb  <- X_ord[idx, , drop = FALSE]
      if (!.alsi_all_levels_present(Xb, X_levels)) next
      loss_up <- FALSE
      fb <- tryCatch(
        withCallingHandlers(
          .alsi_fit_homals(Xb, K_max, itermax = itermax),
          warning = function(w) {
            if (grepl("Loss function increases", conditionMessage(w))) {
              loss_up <<- TRUE
              invokeRestart("muffleWarning")
            }
          }),
        error = function(e) NULL)
      if (!is.null(fb) && !loss_up && !anyNA(fb$C)) {
        ok <- TRUE
        break
      }
      fb <- NULL
    }
    if (!ok) { n_skip <- n_skip + 1L; next }

    M_b_al <- .alsi_procrustes(M_ref, fb$C)$B_aligned

    for (k in seq_len(K_max)) {
      ang_mat[b, k] <- .alsi_principal_angle_max_rad(
        M_ref[,  seq_len(k), drop = FALSE],
        M_b_al[, seq_len(k), drop = FALSE]) * (180 / pi)
      tuc_mat[b, k] <- .alsi_tucker_phi(M_ref[, k], M_b_al[, k])
    }
    if (verbose && b %% 200L == 0L)
      message(sprintf("  bootstrap %d / %d", b, B_boot))
  }

  if (verbose)
    message(sprintf("Bootstrap done. Skipped: %d / %d", n_skip, B_boot))

  ## Summarise
  ang_med  <- apply(ang_mat, 2L, median,   na.rm = TRUE)
  ang_q25  <- apply(ang_mat, 2L, quantile, 0.25, na.rm = TRUE)
  ang_q75  <- apply(ang_mat, 2L, quantile, 0.75, na.rm = TRUE)
  tuc_med  <- apply(tuc_mat, 2L, median,   na.rm = TRUE)
  tuc_q25  <- apply(tuc_mat, 2L, quantile, 0.25, na.rm = TRUE)
  tuc_q75  <- apply(tuc_mat, 2L, quantile, 0.75, na.rm = TRUE)
  vfrac    <- colMeans(!is.na(ang_mat))

  ## Flag dimensions with < 50% valid replicates as unreliable
  ang_med_g <- ifelse(vfrac < 0.5, NA_real_, ang_med)
  tuc_med_g <- ifelse(vfrac < 0.5, NA_real_, tuc_med)

  stab_tbl <- .alsi_stability_table(
    K_max, lambda_rot,
    ang_med_g, ang_q25, ang_q75,
    tuc_med_g, tuc_q25, tuc_q75,
    vfrac, angle_threshold_deg, tucker_threshold)

  ## =========================================================================
  ## STAGE 4: K* selection + ALSI index
  ## =========================================================================
  K_star <- .alsi_select_kstar(ang_med_g, tuc_med_g,
                               angle_threshold_deg, tucker_threshold)
  if (verbose)
    message(sprintf("\n=== Stage 4: K* = %d ===", K_star))

  lam_K    <- lambda_rot[seq_len(K_star)]
  ALSI_raw <- .alsi_index(Z_ref[, seq_len(K_star), drop = FALSE], lam_K)
  ALSI_z   <- as.vector(scale(ALSI_raw))

  if (verbose) {
    message(sprintf("Weights w_k = lambda_k / sum(lambda): %s",
                    paste(round(lam_K / sum(lam_K), 4L), collapse = ", ")))
    message(sprintf("ALSI_z: mean = %.3f  SD = %.3f  range [%.3f, %.3f]",
                    mean(ALSI_z), sd(ALSI_z), min(ALSI_z), max(ALSI_z)))
  }

  ## ---- Return -------------------------------------------------------------
  structure(list(
    ALSI_index      = ALSI_raw,
    ALSI_z          = ALSI_z,
    K_PA            = K_PA,
    K_star          = K_star,
    Z_ref           = Z_ref,
    C_ref           = C_ref,
    lambda_rot      = lambda_rot,
    stability_table = stab_tbl,
    pa_table        = pa_table,
    n_skipped       = n_skip,
    call            = mc),
    class = "alsi_ordinal")
}


## ---------------------------------------------------------------------------
## S3 methods
## ---------------------------------------------------------------------------

#' @export
print.alsi_ordinal <- function(x, ...) {
  cat("alsi_ordinal (homals ALS optimal scaling)\n")
  cat(sprintf("  K_PA = %d  |  K* = %d  |  skipped = %d\n",
              x$K_PA, x$K_star, x$n_skipped))
  cat("\nStability table:\n")
  cols <- c("Dimension", "Eigenvalue", "Angle_deg_median",
            "Tucker_phi_median", "Pass_BOTH", "Grade_Tucker")
  print(x$stability_table[, cols], row.names = FALSE)
  cat(sprintf("\nALSI index (n = %d):\n", length(x$ALSI_index)))
  print(round(summary(x$ALSI_z), 3L))
  invisible(x)
}

#' @export
summary.alsi_ordinal <- function(object, ...) {
  cat("alsi_ordinal summary\n")
  cat(sprintf("  K_PA = %d  |  K* = %d\n", object$K_PA, object$K_star))
  cat("\nFull stability table:\n")
  print(object$stability_table, row.names = FALSE)
  cat("\nALSI_z:\n")
  print(summary(object$ALSI_z))
  invisible(object)
}
