## =============================================================================
## R/utils.R  -  alsi package v0.2.0
##
## Shared internal helpers (.alsi_* prefix, NOT exported) for all pipelines:
##   alsi_workflow()          binary MCA            R/binary.R
##   alsi_workflow_ordinal()  homals ALS ordinal    R/ordinal.R
##   calsi_workflow()         continuous ipsatized  R/continuous.R
## =============================================================================

## 1. Ordered-factor coercion [ordinal] ----------------------------------------
#' @noRd
.alsi_make_ordered <- function(x) {
  if (is.ordered(x)) return(x)
  if (is.factor(x)) {
    lev <- levels(x)
    suppressWarnings({ num_lev <- as.numeric(lev) })
    if (!anyNA(num_lev)) lev <- lev[order(num_lev)]
    return(ordered(x, levels = lev))
  }
  ux <- sort(unique(x[!is.na(x)]))
  ordered(factor(x, levels = ux), levels = ux)
}

## 2. Bootstrap level guard [ordinal] ------------------------------------------
#' @noRd
.alsi_all_levels_present <- function(Xb, levels_list) {
  for (j in seq_len(ncol(Xb))) {
    if (any(table(factor(Xb[[j]], levels = levels_list[[j]])) == 0L))
      return(FALSE)
  }
  TRUE
}

## 3. Core geometry [all pipelines] --------------------------------------------

#' Thin-QR orthonormal basis for col(M)
#' @noRd
.alsi_orth_basis <- function(M) qr.Q(qr(M))

#' Maximum principal angle (radians) between col(A) and col(B).
#' Called on post-Procrustes matrices; measures residual subspace instability.
#' @noRd
.alsi_principal_angle_max_rad <- function(A, B) {
  QA <- .alsi_orth_basis(A)
  QB <- .alsi_orth_basis(B)
  sv <- svd(t(QA) %*% QB, nu = 0L, nv = 0L)$d
  sv <- pmin(1.0, pmax(0.0, sv))
  max(acos(sv))
}

#' Orthogonal Procrustes: minimise ||A - B R||_F (no sign anchoring).
#' Returns list(B_aligned, R). Used by the ordinal pipeline.
#' @noRd
.alsi_procrustes <- function(A, B) {
  sv <- svd(t(B) %*% A)
  R  <- sv$u %*% t(sv$v)
  list(B_aligned = B %*% R, R = R)
}

#' Orthogonal Procrustes with column-wise sign anchoring.
#' Flips each aligned column to maximise dot-product with reference.
#' Returns list(B_aligned, R). Used by binary and continuous pipelines.
#' @noRd
.alsi_procrustes_signed <- function(A, B) {
  sv  <- svd(t(B) %*% A)
  R   <- sv$u %*% t(sv$v)
  Bal <- B %*% R
  for (k in seq_len(ncol(Bal))) {
    if (sum(Bal[, k] * A[, k]) < 0) {
      Bal[, k] <- -Bal[, k]
      R[, k]   <- -R[, k]
    }
  }
  list(B_aligned = Bal, R = R)
}

#' Absolute Tucker congruence coefficient (sign-invariant).
#' Returns NA_real_ when either vector has zero norm.
#' Ref: Lorenzo-Seva & ten Berge (2006, Methodology, 2, 57-64).
#' @noRd
.alsi_tucker_phi <- function(a, b) {
  den <- sqrt(sum(a^2, na.rm = TRUE) * sum(b^2, na.rm = TRUE))
  if (den < .Machine$double.eps) return(NA_real_)
  abs(sum(a * b, na.rm = TRUE)) / den
}

## 4. Index scoring ------------------------------------------------------------

#' Eigenvalue-weighted linear ALSI (ordinal pipeline).
#' Formula: Z %*% (lambda / sum(lambda)). Can be negative.
#' @noRd
.alsi_index <- function(Z, lambda) {
  as.vector(Z %*% (lambda / sum(lambda)))
}

#' Variance-weighted L2-norm ALSI (binary and continuous pipelines).
#' Formula: sqrt(sum_k w_k * f_ik^2), w_k = eig_k / sum(eig_1:K). Always >= 0.
#' @noRd
.alsi_norm_index <- function(Fmat, eig, K) {
  Fmat <- as.matrix(Fmat)
  w    <- eig[seq_len(K)] / sum(eig[seq_len(K)])
  Fk   <- Fmat[, seq_len(K), drop = FALSE]
  sqrt(rowSums(sweep(Fk^2, 2L, w, "*")))
}

## 5. Reverse-scoring [ordinal] ------------------------------------------------
#' @noRd
.alsi_reverse_score <- function(X, reversed_items, scale_min, scale_max) {
  miss <- setdiff(reversed_items, names(X))
  if (length(miss)) warning("reversed_items not in data: ", paste(miss, collapse=", "))
  for (itm in intersect(reversed_items, names(X)))
    X[[itm]] <- (scale_min + scale_max) - X[[itm]]
  X
}

## 6. Tucker grade lookup [all pipelines] --------------------------------------
#' Lorenzo-Seva & ten Berge (2006) descriptive grade for Tucker phi.
#' @noRd
.alsi_tucker_grade <- function(phi) {
  ifelse(is.na(phi),  "Insufficient data",
  ifelse(phi >= 0.95, "Excellent",
  ifelse(phi >= 0.85, "Good",
  ifelse(phi >= 0.70, "Adequate", "Poor"))))
}

## 7. K* dual-criterion selection [ordinal] ------------------------------------
#' K* = largest k where ALL dims 1..k pass angle AND Tucker simultaneously.
#' Falls back to 1 (with warning) when no dimension passes.
#' @noRd
.alsi_select_kstar <- function(angle_med_g, tucker_med_g,
                               angle_threshold_deg, tucker_threshold) {
  K_max     <- length(angle_med_g)
  pass_both <- (!is.na(angle_med_g)  & angle_med_g  <  angle_threshold_deg) &
               (!is.na(tucker_med_g) & tucker_med_g >= tucker_threshold)
  K_star <- 0L
  for (k in seq_len(K_max))
    if (all(pass_both[seq_len(k)])) K_star <- k
  if (K_star == 0L) {
    warning("No dimension passed both stability criteria. Returning K* = 1.")
    K_star <- 1L
  }
  K_star
}

## 8. Detailed stability table [ordinal] ---------------------------------------
#' @noRd
.alsi_stability_table <- function(K_max, lambda_rot,
                                  angle_med_g,  angle_q25,  angle_q75,
                                  tucker_med_g, tucker_q25, tucker_q75,
                                  valid_frac,
                                  angle_threshold_deg, tucker_threshold) {
  pass_ang <- !is.na(angle_med_g)  & angle_med_g  <  angle_threshold_deg
  pass_tuc <- !is.na(tucker_med_g) & tucker_med_g >= tucker_threshold
  data.frame(
    Dimension         = seq_len(K_max),
    Eigenvalue        = round(lambda_rot,   6L),
    Cum_eigen_pct     = round(100 * cumsum(lambda_rot) / sum(lambda_rot), 1L),
    Angle_deg_median  = round(angle_med_g,  2L),
    Angle_deg_Q25     = round(angle_q25,    2L),
    Angle_deg_Q75     = round(angle_q75,    2L),
    Tucker_phi_median = round(tucker_med_g, 3L),
    Tucker_phi_Q25    = round(tucker_q25,   3L),
    Tucker_phi_Q75    = round(tucker_q75,   3L),
    Valid_pct         = round(100 * valid_frac, 1L),
    Pass_angle        = pass_ang,
    Pass_Tucker       = pass_tuc,
    Pass_BOTH         = pass_ang & pass_tuc,
    Grade_Tucker      = .alsi_tucker_grade(tucker_med_g),
    stringsAsFactors  = FALSE
  )
}

## 9. Rare-category check [ordinal] --------------------------------------------
#' @noRd
.alsi_check_rare <- function(X_ord, items, n, threshold = 0.02, verbose = TRUE) {
  rare_any <- FALSE
  for (vn in items) {
    freq <- table(X_ord[[vn]]) / n
    if (any(freq < threshold)) {
      rare_any <- TRUE
      warning(sprintf("Item %s: rare category (%.1f%%) below %.0f%% threshold: %s",
        vn, 100 * min(freq[freq < threshold]), 100 * threshold,
        paste(names(freq)[freq < threshold], collapse = ", ")))
    }
  }
  if (!rare_any && verbose) message("Rare-category check: none detected.")
  invisible(rare_any)
}

## 10. Ipsatisation [continuous] -----------------------------------------------
#' Row-wise ipsatisation.
#' scale_sd = FALSE (default): subtract row mean only. Matches calsi.R ipsatize().
#' scale_sd = TRUE: also divide by row SD (constant rows protected, SD -> 1.0).
#' @noRd
.alsi_ipsatise <- function(X, scale_sd = FALSE) {
  X  <- as.matrix(X)
  Xi <- sweep(X, 1L, rowMeans(X, na.rm = TRUE), "-")
  if (scale_sd) {
    rs <- apply(Xi, 1L, sd, na.rm = TRUE)
    rs[rs < .Machine$double.eps] <- 1.0
    Xi <- Xi / rs
  }
  Xi
}

## 11. Bootstrap column summary [binary + continuous] --------------------------
#' Column-wise median and quantile summary for bootstrap result matrices.
#' @param X     Numeric matrix (B replicates x K dimensions).
#' @param probs Quantile probabilities (default: 5th and 95th percentiles).
#' @return Named matrix; rows = dimensions, columns = median + quantiles.
#' @noRd
.alsi_summarise_matrix <- function(X, probs = c(0.05, 0.95)) {
  t(apply(X, 2L, function(x) {
    c(median = median(x, na.rm = TRUE),
      stats::setNames(quantile(x, probs = probs, na.rm = TRUE),
                      paste0("p", probs * 100)))
  }))
}
