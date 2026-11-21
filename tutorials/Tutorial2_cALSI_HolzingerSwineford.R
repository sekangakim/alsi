# Set the working directory in RStudio before running this script.
## ============================================================
## TUTORIAL 2: cALSI for Continuous Data
## Last updated: 21 November 2026 (R3 final: zero-dimension stop rule)
## Holzinger & Swineford (1939) cognitive test battery
##
## Data source (public; no download or data request needed):
##   Dataset : `HolzingerSwineford1939` in the R package lavaan (CRAN)
##   Content : 301 seventh- and eighth-grade students from two schools
##             (Pasteur, Grant-White), nine cognitive test scores:
##     Visual  : x1 visual perception, x2 cubes, x3 lozenges
##     Textual : x4 paragraph comprehension, x5 sentence completion,
##               x6 word meaning
##     Speed   : x7 speeded addition, x8 speeded counting of dots,
##               x9 speeded discrimination of straight and curved capitals
##   Covariates: sex, ageyr, agemo, school, grade
##   Cite: Holzinger, K. J., & Swineford, F. (1939). A study in factor
##         analysis: The stability of a bi-factor solution. Supplementary
##         Educational Monographs, 48. University of Chicago.
##         Rosseel, Y. (2012). lavaan: An R package for structural equation
##         modeling. Journal of Statistical Software, 48(2), 1-36.
##
## Preprocessing: each test is standardized to z-scores before ipsatization.
## The tests have different means and spreads; without standardization the
## average test profile becomes the first dimension and cALSI largely
## reflects that shared profile rather than individual differences.
##
## Figures produced (numbered as in the manuscript):
##   Figure 2 (Section 2.1) : from continuous profiles to latent coordinates
##   Figure 5 (Section 4)   : distribution of alpha_i
##   Figure 6 (Section 4)   : most and least differentiated students
##   (Figures 1, 3, and 4 are produced by the Tutorial 1 script.)
##
## To reproduce: install the packages listed below, then run this script
## top to bottom in a fresh R session. Outputs go to the working directory.
## ============================================================

## ---- Setup --------------------------------------------------
required <- c("alsi", "lavaan", "moments", "paran")   # paran: parallel analysis used by calsi_workflow()
missing_pkgs <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop("Please install the following package(s) first: ",
       paste(missing_pkgs, collapse = ", "),
       "\n  e.g. install.packages(c(\"", paste(missing_pkgs, collapse = "\", \""), "\"))")
}
library(alsi)
library(moments)

SEED <- 20260223

cat("============================================================\n")
cat("TUTORIAL 2: cALSI for Continuous Data (Holzinger & Swineford, 1939)\n")
cat("============================================================\n\n")

## ---- Step 1: Load the data ----------------------------------
cat("Step 1: Loading Holzinger & Swineford (1939) data from lavaan...\n")

data("HolzingerSwineford1939", package = "lavaan")
hs <- HolzingerSwineford1939
test_cols <- paste0("x", 1:9)
test_labels <- c("Visual perception", "Cubes", "Lozenges",
                 "Paragraph comprehension", "Sentence completion", "Word meaning",
                 "Speeded addition", "Speeded counting", "Straight-curved capitals")
domain_of <- rep(c("Visual", "Textual", "Speed"), each = 3)

cat(sprintf("  lavaan version : %s\n", as.character(packageVersion("lavaan"))))
cat(sprintf("  Raw data       : N = %d students, %d tests\n", nrow(hs), length(test_cols)))
cat(sprintf("  School         : %s\n", paste(names(table(hs$school)), table(hs$school), sep = " = ", collapse = "; ")))
cat(sprintf("  Sex            : boys (1) = %d; girls (2) = %d\n", sum(hs$sex == 1), sum(hs$sex == 2)))
cat(sprintf("  Age in years   : M = %.2f, SD = %.2f\n\n",
            mean(hs$ageyr + hs$agemo / 12), sd(hs$ageyr + hs$agemo / 12)))

cat("  Test means and SDs (raw scores):\n")
for (j in seq_along(test_cols)) {
  cat(sprintf("    %-3s %-26s M = %5.2f  SD = %4.2f\n", test_cols[j], test_labels[j],
              mean(hs[[test_cols[j]]], na.rm = TRUE), sd(hs[[test_cols[j]]], na.rm = TRUE)))
}
cat("\n")

## ---- Step 2: Missing data and standardization ---------------
cat("Step 2: Missing data and standardization...\n")

keep <- complete.cases(hs[, test_cols])
hs   <- hs[keep, ]
cat(sprintf("  Complete cases on the nine tests: %d of %d\n", sum(keep), length(keep)))

X  <- as.matrix(hs[, test_cols])
Xz <- scale(X)                       # z-scores: mean 0, SD 1 for every test
colnames(Xz) <- test_cols
cat("  Each test standardized to z-scores (sample mean 0, SD 1).\n")

## Why standardize? Without it, the first dimension of the ipsatized raw
## scores mainly reproduces the differences among the test means.
sv_raw <- svd(sweep(X, 1, rowMeans(X), "-"))
cat(sprintf("  Without standardization, Dimension 1 would account for %.1f%% of the\n",
            100 * sv_raw$d[1]^2 / sum(sv_raw$d^2)))
cat(sprintf("  ipsatized variance, and |r| between its loadings and the test means = %.2f.\n\n",
            abs(cor(sv_raw$v[, 1], colMeans(X)))))

p <- ncol(Xz)
cat("cALSI parameters:\n")
cat(sprintf("  p (tests) = %d\n", p))
cat(sprintf("  K_max     = p - 1 = %d (rank of the ipsatized matrix when N > p)\n", p - 1))
cat(sprintf("  N / p     = %.1f\n\n", nrow(Xz) / p))

## ---- Step 3: cALSI workflow ---------------------------------
cat("Step 3: Running cALSI stability-validation workflow...\n")
cat("  B_pa = 2000, B_boot = 2000 (this may take a few minutes)\n\n")

tmp_sink <- tempfile()
sink(tmp_sink)
calsi_result <- tryCatch(
  calsi_workflow(
    data   = as.data.frame(Xz),
    B_pa   = 2000,
    B_boot = 2000,
    seed   = SEED
  ),
  error = function(e) { sink(NULL); stop(e) }
)
sink(NULL)
cat("Workflow complete.\n\n")

## ---- Step 4: Stability assessment ---------------------------
cat("Step 4: Evaluating dimensional stability...\n\n")

first_found <- function(...) {
  for (x in list(...)) if (!is.null(x) && length(x) > 0) return(x)
  NULL
}
K_pa       <- first_found(calsi_result$K, calsi_result$K_star, calsi_result$K_PA,
                          calsi_result$pa$K_star)
angles_mat <- first_found(calsi_result$boot$angles, calsi_result$angles)
tucker_mat <- first_found(calsi_result$boot$tucker, calsi_result$boot$phi, calsi_result$tucker)
if (is.null(K_pa) || is.null(angles_mat) || is.null(tucker_mat)) {
  cat("Components found in the workflow output:\n"); print(names(calsi_result))
  stop("Could not locate K, principal angles, or Tucker coefficients; see names above.")
}
cat(sprintf("  Parallel analysis retained K = %d dimension(s)\n\n", K_pa))

## angles_mat: k-th principal angle between the reference and bootstrap K-dim
##             subspaces (sorted, smallest first)
## tucker_mat: Tucker's congruence per dimension after Procrustes alignment
angle_threshold  <- 20      # same criteria as Tutorial 1
tucker_threshold <- 0.85
cat("  Sequential stop rule (identical to Tutorial 1):\n")
cat(sprintf("    median k-th principal angle < %d degrees\n", angle_threshold))
cat(sprintf("    median Tucker's phi for dimension k >= %.2f\n\n", tucker_threshold))
cat(sprintf("  %-6s %16s %14s %10s\n", "k", "Median angle", "Median phi", "Decision"))
cat(strrep("-", 52), "\n")

K_final <- K_pa
for (k in seq_len(min(K_pa, ncol(angles_mat), ncol(tucker_mat)))) {
  angle_k  <- median(angles_mat[, k], na.rm = TRUE)
  tucker_k <- median(tucker_mat[, k], na.rm = TRUE)
  ok <- is.finite(angle_k)  && angle_k  < angle_threshold &&
        is.finite(tucker_k) && tucker_k >= tucker_threshold
  cat(sprintf("  %-6d %16.2f %14.3f %10s\n", k, angle_k, tucker_k,
              if (ok) "Retain" else "Stop"))
  if (!ok) { K_final <- k - 1; break }
}
if (K_final < 1) {
  stop("No dimension satisfies both stability criteria; index not computed.")
}
cat(sprintf("\n  FINAL DIMENSIONALITY: K* = %d\n\n", K_final))

## Values reported in Table 5: median, 5th, and 95th percentiles of the
## principal angles and Tucker's phi over bootstrap samples.
cat("  Bootstrap stability (Table 5): median [5th, 95th percentile]\n")
for (k in seq_len(K_pa)) {
  qa <- quantile(angles_mat[, k], c(0.5, 0.05, 0.95), na.rm = TRUE)
  qt <- quantile(tucker_mat[, k], c(0.5, 0.05, 0.95), na.rm = TRUE)
  cat(sprintf("    Dimension %d: angle %.2f [%.2f, %.2f]   phi %.3f [%.3f, %.3f]\n",
              k, qa[1], qa[2], qa[3], qt[1], qt[2], qt[3]))
}
cat("\n")

## Parallel analysis in detail. calsi_workflow() runs paran::paran() on the
## ipsatized scores: eigenvalues of their correlation matrix are compared
## with the 95th percentile of eigenvalues from random normal data. (The
## "Reference" column of calsi_result$pa_table holds paran's ADJUSTED
## eigenvalues, retained when > 1; the two rules are equivalent.)
ips    <- function(M) sweep(M, 1, rowMeans(M), "-")
pa_obs <- eigen(cor(ips(Xz)), symmetric = TRUE, only.values = TRUE)$values
pa_ref <- calsi_result$pa$eig_rand
cat("  Parallel analysis (Table 5):\n")
cat(sprintf("  %-4s %10s %22s\n", "k", "Observed", "Reference (paran, 95th)"))
for (k in 1:4) cat(sprintf("  %-4d %10.3f %22s\n", k, pa_obs[k],
                           if (is.null(pa_ref)) "n/a" else sprintf("%.3f", pa_ref[k])))

## Robustness check: paran's random data are not ipsatized. Repeat the
## comparison with random normal data that are standardized and ipsatized
## exactly as the observed data were.
set.seed(SEED)
pa_ips <- apply(replicate(2000, eigen(cor(ips(scale(matrix(rnorm(nrow(Xz) * p), nrow(Xz), p)))),
                                      symmetric = TRUE, only.values = TRUE)$values),
                1, quantile, probs = 0.95)
cat("\n  With ipsatized reference data (95th percentile):\n")
for (k in 1:4) cat(sprintf("  %-4d %10.3f %22.3f  %s\n", k, pa_obs[k], pa_ips[k],
                           if (pa_obs[k] > pa_ips[k]) "exceeds" else "does not exceed"))
cat(sprintf("  Dimensions retained with ipsatized reference data: %d\n\n",
            which(pa_obs <= pa_ips)[1] - 1))

## ---- Step 5: Compute alpha_i with K* ------------------------
cat("Step 5: Computing cALSI values with K* dimensions...\n\n")

## Ipsatized SVD, computed here with the same formulas as the alsi package
## (row-centre, SVD, F = U D, eigenvalues = D^2), so the script does not
## depend on output-object names that differ across package versions.
Xs  <- sweep(Xz, 1, rowMeans(Xz), "-")
sv  <- svd(Xs)
fit <- list(F = sv$u %*% diag(sv$d), eig = sv$d^2)
w   <- fit$eig[1:K_final] / sum(fit$eig[1:K_final])

alpha_i <- tryCatch(calsi(fit$F, fit$eig, K_final)$alpha,
                    error = function(e) sqrt(rowSums(sweep(fit$F[, 1:K_final, drop = FALSE]^2, 2, w, "*"))))

wf_alpha <- first_found(calsi_result$calsi$alpha, calsi_result$alpha)
if (!is.null(wf_alpha) && length(wf_alpha) == length(alpha_i)) {
  if (K_final == K_pa) {
    cat(sprintf("  Check: max |difference| from workflow alpha_i = %.2e (should be ~0)\n",
                max(abs(wf_alpha - alpha_i))))
  } else {
    cat(sprintf("  Note: the workflow's alpha_i used K = %d; the values below use K* = %d.\n",
                K_pa, K_final))
  }
}

cat("  Variance weights:\n")
for (k in seq_len(K_final)) cat(sprintf("    w_%d = %.4f\n", k, w[k]))
cat(sprintf("  Variance of the ipsatized scores accounted for: %s\n",
            paste(sprintf("%.1f%%", 100 * fit$eig[1:K_final] / sum(fit$eig)), collapse = ", ")))

## What the dimensions represent: test loadings (right singular vectors) and
## correlations of the person coordinates with contrasts among domain means
load2 <- sv$v[, 1:2]; rownames(load2) <- test_cols; colnames(load2) <- c("Dim1", "Dim2")
cat("  Test loadings:\n"); print(round(load2, 2))
dom_means <- sapply(c("Visual", "Textual", "Speed"), function(d) rowMeans(Xz[, domain_of == d]))
cat(sprintf("  r(Dimension 1, speed minus textual) = %.3f\n",
            cor(fit$F[, 1], dom_means[, "Speed"] - dom_means[, "Textual"])))
cat(sprintf("  r(Dimension 2, visual minus mean of textual and speed) = %.3f\n",
            cor(fit$F[, 2], dom_means[, "Visual"] - rowMeans(dom_means[, c("Textual", "Speed")]))))
cat("\ncALSI DISTRIBUTION:\n")
cat(strrep("-", 46), "\n")
cat(sprintf("  N            = %d\n",           length(alpha_i)))
cat(sprintf("  K*           = %d\n",           K_final))
cat(sprintf("  Range        = %.4f - %.4f\n",  min(alpha_i), max(alpha_i)))
cat(sprintf("  M (SD)       = %.4f (%.4f)\n",  mean(alpha_i), sd(alpha_i)))
cat(sprintf("  Median       = %.4f\n",         median(alpha_i)))
cat(sprintf("  Skewness     = %.3f\n",         skewness(alpha_i)))
cat(sprintf("  Kurtosis     = %.3f\n\n",       kurtosis(alpha_i)))

## ---- Step 6: Figure 2 - from continuous profiles to latent coordinates
## Section 2.1 of the manuscript. Ten students spanning the range of alpha_i
## (lowest = 1, highest = 10) are shown as (A) standardized test profiles,
## (B) coordinates on the first two dimensions of the ipsatized data, and
## (C) weighted coordinates sqrt(w_k) * f_ik, so that arrow length from the
## centroid equals alpha_i when K* = 2. Same layout as Figure 1 (Tutorial 1).
cat("Step 6: Generating Figure 2 (cALSI: profiles, latent space, index)...\n")

sel  <- order(alpha_i)[round(seq(1, length(alpha_i), length.out = 10))]
pct  <- 100 * fit$eig / sum(fit$eig)                         # % of variance
F2   <- fit$F[, 1:2]
Fw   <- sweep(F2, 2, sqrt(w[1:2]), "*")                       # weighted coordinates
ramp <- colorRamp(c("#2C7BB6", "#F2C12E", "#C0392B"))
col_of <- function(a) rgb(ramp((a - min(alpha_i)) / diff(range(alpha_i))), maxColorValue = 255)
pcol <- col_of(alpha_i[sel])
if (K_final == 2) {
  cat(sprintf("  Check: max |arrow length - alpha_i| = %.1e (should be ~0)\n",
              max(abs(sqrt(rowSums(Fw[sel, ]^2)) - alpha_i[sel]))))
} else {
  cat("  Note: K* is not 2; Panel C shows Dimensions 1 and 2 only.\n")
}

pdf("Figure2_cALSI_Latent_Space_HS1939.pdf", width = 15, height = 5.4, family = "Helvetica", pointsize = 16)
par(mfrow = c(1, 3), mar = c(5, 5, 4, 2), cex.main = 1.3, cex.lab = 1.15)

## (A) standardized profiles: cell shading and value = z-score
Z10 <- Xz[sel, ]
zpal <- colorRampPalette(c("#8C510A", "#D8B365", "#F6E8C3", "#F5F5F5",
                           "#C7EAE5", "#5AB4AC", "#01665E"))(101)
zc   <- pmax(pmin(Z10, 2.5), -2.5)                         # shading capped at |z| = 2.5
plot(NA, xlim = c(0.5, p + 3.6), ylim = c(0.5, 10.5), axes = FALSE, xlab = "",
     ylab = "Student (ordered by cALSI)", main = "(A) Standardized Test Profiles")
for (r in 1:10) for (j in 1:p) {
  rect(j - 0.46, r - 0.42, j + 0.46, r + 0.42, border = "gray70",
       col = zpal[round((zc[r, j] + 2.5) / 5 * 100) + 1])
  text(j, r, sprintf("%.1f", Z10[r, j]), cex = 0.68,
       col = if (abs(Z10[r, j]) > 1.6) "white" else "black")
}
points(rep(0.0, 10), 1:10, pch = 21, bg = pcol, cex = 1.6, xpd = TRUE)
axis(2, at = 1:10, labels = 1:10, las = 1, tick = FALSE, line = 0.3)
abline(v = c(3.5, 6.5), col = "gray40", lwd = 1.5)
axis(1, at = 1:p, labels = test_cols, tick = FALSE, line = -0.8, cex.axis = 0.85)
mtext(c("Visual", "Textual", "Speed"), side = 1, at = c(2, 5, 8), line = 1.4, cex = 0.8)
mtext("Test (3 per domain)", side = 1, line = 2.6, cex = 0.8)
mtext("Brown = below sample mean; teal = above (z)", side = 1, line = 3.9, cex = 0.75, col = "gray30")
text(p + 1.3, 1:10, sprintf("%.2f", rowMeans(Z10)), cex = 0.95)
text(p + 2.9, 1:10, sprintf("%.2f", alpha_i[sel]), cex = 0.95)
text(p + 1.3, 10.95, "Mean", cex = 0.85, xpd = TRUE)
text(p + 2.9, 10.95, expression(alpha[i]), cex = 1.0, xpd = TRUE)

## (B) latent space: all students (gray) and the 10 shown in Panel A
lim <- range(F2) * 1.1
plot(F2, pch = 16, col = adjustcolor("gray60", 0.45), cex = 0.7, asp = 1, xlim = lim, ylim = lim, las = 1,
     xlab = sprintf("Dimension 1 (%.1f%%)", pct[1]), ylab = sprintf("Dimension 2 (%.1f%%)", pct[2]),
     main = "(B) Latent Space (Ipsatized SVD)")
abline(h = 0, v = 0, lty = 2, col = "gray50")
points(0, 0, pch = 3, cex = 2.5, lwd = 3)
points(F2[sel, ], pch = 21, bg = pcol, cex = 2)
text(F2[sel, ], labels = 1:10, pos = 3, offset = 0.7, cex = 0.95, font = 2)

## (C) weighted distance from the centroid = alpha_i
limw <- range(Fw[sel, ], 0) + c(-0.15, 0.15) * diff(range(Fw[sel, ], 0))
plot(NA, asp = 1, xlim = limw, ylim = limw, las = 1,
     xlab = expression(paste("Dimension 1 ", phantom(0) %*% phantom(0), sqrt(w[1]))),
     ylab = expression(paste("Dimension 2 ", phantom(0) %*% phantom(0), sqrt(w[2]))),
     main = expression(bold(paste("(C) Profile Differentiation (", bolditalic(alpha[i]), ")"))))
abline(h = 0, v = 0, lty = 2, col = "gray50")
arrows(0, 0, Fw[sel, 1], Fw[sel, 2], col = pcol, lwd = 2.5, length = 0.1)
points(Fw[sel, ], pch = 21, bg = pcol, cex = 2)
points(0, 0, pch = 3, cex = 2.5, lwd = 3)
text(Fw[sel, ], labels = 1:10, pos = 3, offset = 0.7, cex = 0.95, font = 2)   # alpha_i values: Panel A
dev.off()
cat("  Saved: Figure2_cALSI_Latent_Space_HS1939.pdf\n")
cat("  Students shown (1 = lowest alpha_i, 10 = highest):\n")
for (r in 1:10) cat(sprintf("    %2d  id %3s  alpha_i = %.3f  mean z = %5.2f  SD of z = %.2f\n",
                            r, hs$id[sel[r]], alpha_i[sel[r]], mean(Z10[r, ]), sd(Z10[r, ])))
cat("\n")

## ---- Step 7: Figure 5 - distribution of alpha_i ------------
## Figures 3 and 4 (ALSI) are produced by the Tutorial 1 script.
cat("Step 7: Generating Figure 5 (distribution of alpha_i)...\n")

alpha_mean   <- mean(alpha_i)
alpha_median <- median(alpha_i)
alpha_sd     <- sd(alpha_i)
alpha_skew   <- skewness(alpha_i)

pdf("Figure5_cALSI_Distribution_HS1939.pdf", width = 8, height = 6, family = "Helvetica")
par(mar = c(5, 5, 4, 2))
hist_breaks <- seq(min(alpha_i) - 0.001, max(alpha_i) + 0.001, length.out = 25)
hist(alpha_i, breaks = hist_breaks, col = "steelblue", border = "white",
     main = "Distribution of cALSI Values (Holzinger & Swineford, 1939)",
     xlab = expression(paste("cALSI (", alpha[i], ")")), ylab = "Frequency",
     cex.main = 1.2, cex.lab = 1.1, las = 1)
dens <- density(alpha_i, adjust = 1.2, from = 0)   # alpha_i cannot be negative
lines(dens$x, dens$y * length(alpha_i) * diff(hist_breaks)[1], col = "darkred", lwd = 2.5)
abline(v = alpha_mean,   col = "red",    lwd = 2.5, lty = 2)
abline(v = alpha_median, col = "orange", lwd = 2.5, lty = 3)
legend("topright",
       legend = c("Histogram (frequency)", "Density curve",
                  sprintf("Mean = %.3f", alpha_mean), sprintf("Median = %.3f", alpha_median),
                  sprintf("SD = %.3f", alpha_sd),     sprintf("Skewness = %.3f", alpha_skew)),
       col = c("steelblue", "darkred", "red", "orange", "black", "black"),
       lty = c(NA, 1, 2, 3, NA, NA), pch = c(15, NA, NA, NA, NA, NA),
       lwd = c(NA, 2.5, 2.5, 2.5, NA, NA), bty = "n", cex = 0.90)
dev.off()
cat("  Saved: Figure5_cALSI_Distribution_HS1939.pdf\n\n")

## ---- Step 8: Figure 6 - most and least differentiated profiles
cat("Step 8: Generating Figure 6 (highest and lowest alpha_i)...\n")

hi <- which.max(alpha_i); lo <- which.min(alpha_i)
cols <- c(Visual = "steelblue", Textual = "darkorange", Speed = "darkgreen")[domain_of]
ylim <- range(c(Xz[hi, ], Xz[lo, ], -1, 1)) * 1.15

pdf("Figure6_Contrasting_Profiles_HS1939.pdf", width = 10, height = 5, family = "Helvetica")
par(mfrow = c(1, 2), mar = c(7, 5, 4, 2))
for (case in list(list(i = hi, lab = "(A) Highest"), list(i = lo, lab = "(B) Lowest"))) {
  bp <- barplot(Xz[case$i, ], names.arg = test_cols, col = cols, border = "white", ylim = ylim,
                main = bquote(.(case$lab) ~ alpha[i] == .(sprintf("%.2f", alpha_i[case$i]))),
                ylab = "Test score (z)", las = 1)
  abline(h = 0, col = "gray40")
  abline(h = mean(Xz[case$i, ]), lty = 2)                 # the student's own mean level
}
legend("bottomright", legend = c("Visual", "Textual", "Speed", "Student mean"),
       fill = c("steelblue", "darkorange", "darkgreen", NA), border = NA,
       lty = c(NA, NA, NA, 2), bty = "n", cex = 0.85)
dev.off()
cat(sprintf("  Highest alpha_i = %.4f (id %s); lowest alpha_i = %.4f (id %s)\n",
            alpha_i[hi], hs$id[hi], alpha_i[lo], hs$id[lo]))
cat("  Saved: Figure6_Contrasting_Profiles_HS1939.pdf\n\n")

## ---- Step 9: Subsequent analysis ---------------------------
cat("Step 9: Predicting profile differentiation...\n\n")

level <- rowMeans(Xz)                                     # overall level (mean z-score)
cat(sprintf("  r(alpha_i, overall level)        = %.3f\n",   cor(alpha_i, level)))
cat(sprintf("  r(alpha_i, |overall level|)      = %.3f\n",   cor(alpha_i, abs(level))))
cat(sprintf("  R-squared, alpha_i ~ level + level^2 = %.3f\n", summary(lm(alpha_i ~ level + I(level^2)))$r.squared))
cat(sprintf("  r(alpha_i, SD of the nine z-scores)     = %.3f\n", cor(alpha_i, apply(Xz, 1, sd))))
cat(sprintf("  r(alpha_i, SD of the three domain means) = %.3f\n\n", cor(alpha_i, apply(dom_means, 1, sd))))

reg_data <- data.frame(
  alpha_i = alpha_i,
  sex     = factor(hs$sex, levels = 1:2, labels = c("Male", "Female")),   # lavaan codes 1 = boys, 2 = girls
  age     = hs$ageyr + hs$agemo / 12,
  school  = factor(hs$school),
  level   = level
)
## Overall level is included as a covariate, as recommended in Section 2.2.
model <- lm(alpha_i ~ sex + age + school + level, data = reg_data)
print(summary(model))
cat("\n95% confidence intervals:\n"); print(round(confint(model), 3))

## Robustness: alpha_i is non-negative and positively skewed, so the model is
## refitted with log(alpha_i) as the outcome.
model_log <- lm(log(alpha_i) ~ sex + age + school + level, data = reg_data)
fl <- summary(model_log)$fstatistic
cat(sprintf("\nRobustness, log(alpha_i) as outcome: R-squared = %.3f, F(%d, %d) = %.2f, p = %.3f\n",
            summary(model_log)$r.squared, fl[2], fl[3], fl[1], pf(fl[1], fl[2], fl[3], lower.tail = FALSE)))
print(round(coef(summary(model_log)), 3))
cat("\n")

## ---- Step 10: Save outputs ----------------------------------
cat("Step 10: Saving outputs...\n")

tutorial2_hs <- data.frame(id = hs$id, Xz, alpha_i = alpha_i, level = level,
                           sex = hs$sex, ageyr = hs$ageyr, agemo = hs$agemo,
                           school = hs$school, grade = hs$grade)
save(tutorial2_hs, calsi_result, K_final, w, model,
     file = "tutorial2_hs1939_calsi.rda", compress = "xz")
cat("  Saved: tutorial2_hs1939_calsi.rda (analysed data, alpha_i, workflow output, model)\n\n")

cat("============================================================\n")
cat("TUTORIAL 2 COMPLETE\n")
cat("============================================================\n\n")
sessionInfo()
