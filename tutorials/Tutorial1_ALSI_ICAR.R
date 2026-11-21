# Set the working directory in RStudio before running this script.
## ============================================================
## TUTORIAL 1: ALSI for Inherently Binary Data
## Last updated: 21 November 2026 (R3 final: zero-dimension stop rule)
## ICAR ability items (psychTools::ability)
##
## Data source (public; no download or data request needed):
##   Dataset : `ability` in the R package psychTools (CRAN)
##   Content : 16 multiple-choice ability items from the International
##             Cognitive Ability Resource (ICAR), scored correct (1) or
##             incorrect (0); 1,525 respondents from the Synthetic Aperture
##             Personality Assessment (SAPA) web-based project.
##   Item types (4 items each):
##     Verbal reasoning : reason.4,  reason.16, reason.17, reason.19
##     Letter series    : letter.7,  letter.33, letter.34, letter.58
##     Matrix reasoning : matrix.45, matrix.46, matrix.47, matrix.55
##     3-D rotation     : rotate.3,  rotate.4,  rotate.6,  rotate.8
##   Cite: Condon, D. M., & Revelle, W. (2014). The International Cognitive
##         Ability Resource: Development and initial validation of a
##         public-domain measure. Intelligence, 43, 52-64.
##         Revelle, W. (psychTools: Tools to accompany the 'psych' package).
##
## The items are scored correct/incorrect at source, so no dichotomization
## of any ordinal or continuous variable takes place anywhere in this script.
##
## Figures produced (numbered as in the manuscript):
##   Figure 1 (Section 2.1) : from binary responses to latent coordinates
##   Figure 3 (Section 3)   : distribution of alpha_i
##   Figure 4 (Section 3)   : most and least differentiated respondents
##   (Figures 2, 5, and 6 are produced by the Tutorial 2 script.)
##
## To reproduce: install the three packages listed below, then run this
## script top to bottom in a fresh R session. Outputs are written to the
## current working directory.
## ============================================================

## ---- Setup --------------------------------------------------
required <- c("alsi", "psychTools", "moments")
missing_pkgs <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop("Please install the following package(s) first: ",
       paste(missing_pkgs, collapse = ", "),
       "\n  e.g. install.packages(c(\"", paste(missing_pkgs, collapse = "\", \""), "\"))")
}
library(alsi)
library(psychTools)
library(moments)

SEED <- 20260218

cat("============================================================\n")
cat("TUTORIAL 1: ALSI for Inherently Binary Data (ICAR ability items)\n")
cat("============================================================\n\n")

## ---- Step 1: Load the ICAR data -----------------------------
cat("Step 1: Loading ICAR ability items from psychTools...\n")

data("ability", package = "psychTools")
item_cols <- colnames(ability)            # the 16 ICAR items
domains <- list(
  Verbal  = grep("^reason", item_cols, value = TRUE),
  Letter  = grep("^letter", item_cols, value = TRUE),
  Matrix  = grep("^matrix", item_cols, value = TRUE),
  Rotate  = grep("^rotate", item_cols, value = TRUE)
)

cat(sprintf("  psychTools version : %s\n", as.character(packageVersion("psychTools"))))
cat(sprintf("  Raw data           : N = %d, items = %d\n", nrow(ability), ncol(ability)))
stopifnot(all(unlist(ability) %in% c(0, 1, NA)))   # confirm 0/1 scoring
cat("  Coding             : 1 = correct, 0 = incorrect (as scored at source)\n\n")

## ---- Step 2: Missing responses ------------------------------
cat("Step 2: Missing responses...\n")

miss <- colSums(is.na(ability))
cat("  Missing per item:\n")
for (v in item_cols) {
  cat(sprintf("    %-10s : %4d (%4.1f%%)\n", v, miss[v], miss[v] / nrow(ability) * 100))
}

## Complete cases are used here. Report the retained N in the manuscript and
## describe the handling consistently with Section 5.2 (Missing Data).
keep      <- complete.cases(ability)
bin_items <- as.data.frame(ability[keep, item_cols])
cat(sprintf("\n  Complete cases     : %d of %d (%.1f%% retained)\n",
            nrow(bin_items), nrow(ability), mean(keep) * 100))

## Do excluded respondents differ? Compare proportion correct on the items
## each respondent answered.
pc_answered <- rowMeans(ability, na.rm = TRUE)          # NaN if no item answered
cmp <- t.test(pc_answered[keep], pc_answered[!keep])     # Welch t test
cat(sprintf("  Proportion correct on answered items: retained M = %.3f, excluded M = %.3f\n",
            mean(pc_answered[keep]), mean(pc_answered[!keep], na.rm = TRUE)))
cat(sprintf("  Welch t(%.1f) = %.2f, p = %.4f (excluded respondents with >= 1 answer: %d)\n\n",
            cmp$parameter, cmp$statistic, cmp$p.value, sum(!keep & is.finite(pc_answered))))

## Endorsement (proportion correct) per item
cat("  Proportion correct per item (complete cases):\n")
pc <- colMeans(bin_items)
for (v in item_cols) cat(sprintf("    %-10s : %.3f\n", v, pc[v]))
cat("\n")

p <- ncol(bin_items)
cat("ALSI parameters:\n")
cat(sprintf("  p (items) = %d binary items\n", p))
cat(sprintf("  K_max     = p = %d (non-trivial dimensions of the indicator-matrix MCA)\n", p))
cat(sprintf("  N / p     = %.1f\n\n", nrow(bin_items) / p))

## ---- Step 3: ALSI workflow ----------------------------------
cat("Step 3: Running ALSI stability-validation workflow...\n")
cat("  B_pa = 2000, B_boot = 2000 (this may take several minutes)\n\n")

tmp_sink <- tempfile()
sink(tmp_sink)
alsi_result <- tryCatch(
  alsi_workflow(
    data   = bin_items,
    vars   = item_cols,
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

angles_mat <- alsi_result$boot$angles   # B x K: k-th principal angle between the
                                        # reference and bootstrap K-dim subspaces
                                        # (sorted, smallest first)
tucker_mat <- alsi_result$boot$tucker   # B x K: Tucker's congruence per dimension,
                                        # after Procrustes alignment
K_pa <- alsi_result$K
if (is.null(K_pa) || length(K_pa) == 0) K_pa <- alsi_result$pa$K_star
if (is.null(K_pa) || length(K_pa) == 0) K_pa <- ncol(angles_mat)
cat(sprintf("  Parallel analysis retained K = %d dimension(s)\n\n", K_pa))

angle_threshold  <- 20
tucker_threshold <- 0.85
cat("  Sequential stop rule:\n")
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

## Values reported in Table 4: observed and reference eigenvalues (reference =
## 95th percentile over the permuted datasets), and the median, 5th, and 95th
## percentiles of the principal angles and Tucker's phi over bootstrap samples.
if (!is.null(alsi_result$pa_table)) {
  cat("  Parallel analysis (Table 4):\n"); print(head(alsi_result$pa_table, K_pa + 1), row.names = FALSE)
}
cat("\n  Bootstrap stability (Table 4): median [5th, 95th percentile]\n")
for (k in seq_len(K_pa)) {
  qa <- quantile(angles_mat[, k], c(0.5, 0.05, 0.95), na.rm = TRUE)
  qt <- quantile(tucker_mat[, k], c(0.5, 0.05, 0.95), na.rm = TRUE)
  cat(sprintf("    Dimension %d: angle %.2f [%.2f, %.2f]   phi %.3f [%.3f, %.3f]\n",
              k, qa[1], qa[2], qa[3], qt[1], qt[2], qt[3]))
}
cat("\n")

## ---- Step 5: Compute alpha_i with K* ------------------------
## alpha_i is recomputed explicitly with the stability-validated K*, so the
## reported index always uses the same dimensions as the reported K*.
cat("Step 5: Computing ALSI values with K* dimensions...\n\n")

fit <- if (!is.null(alsi_result$fit)) alsi_result$fit else alsi_result$boot$ref
if (is.null(fit$F) || is.null(fit$eig)) {
  stop("Could not find principal coordinates (F) and eigenvalues (eig) in the workflow output.")
}
alsi_final <- alsi(fit$F, fit$eig, K_final)   # positional: coordinates, eigenvalues, K
alpha_i    <- alsi_final$alpha
w          <- fit$eig[1:K_final] / sum(fit$eig[1:K_final])

cat("  Variance weights:\n")
for (k in seq_len(K_final)) cat(sprintf("    w_%d = %.4f\n", k, w[k]))
cat("\nALSI DISTRIBUTION:\n")
cat(strrep("-", 46), "\n")
cat(sprintf("  N            = %d\n",           length(alpha_i)))
cat(sprintf("  K*           = %d\n",           K_final))
cat(sprintf("  Range        = %.4f - %.4f\n",  min(alpha_i), max(alpha_i)))
cat(sprintf("  M (SD)       = %.4f (%.4f)\n",  mean(alpha_i), sd(alpha_i)))
cat(sprintf("  Median       = %.4f\n",         median(alpha_i)))
cat(sprintf("  Skewness     = %.3f\n",         skewness(alpha_i)))
cat(sprintf("  Kurtosis     = %.3f\n\n",       kurtosis(alpha_i)))

## ---- Step 6: Figure 1 - from binary responses to latent coordinates
## Section 2.1 of the manuscript. Ten respondents spanning the range of
## alpha_i (lowest = 1, highest = 10) are shown as (A) 0/1 response patterns,
## (B) coordinates on the first two MCA dimensions, and (C) weighted
## coordinates sqrt(w_k) * f_ik, so that arrow length from the centroid
## equals alpha_i when K* = 2.
cat("Step 6: Generating Figure 1 (ALSI: responses, latent space, index)...\n")

item_order <- unlist(domains)                                # items grouped by type
sel  <- order(alpha_i)[round(seq(1, length(alpha_i), length.out = 10))]
pct  <- 100 * fit$eig / sum(fit$eig)                         # % of total inertia
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

pdf("Figure1_ALSI_Latent_Space_ICAR.pdf", width = 15, height = 5.4, family = "Helvetica", pointsize = 16)
par(mfrow = c(1, 3), mar = c(5, 5, 4, 2), cex.main = 1.3, cex.lab = 1.15)

## (A) response patterns: filled cell = correct
X10 <- as.matrix(bin_items[sel, item_order])
plot(NA, xlim = c(0.5, p + 3.6), ylim = c(0.5, 10.5), axes = FALSE, xlab = "",
     ylab = "Respondent (ordered by ALSI)", main = "(A) Binary Response Patterns")
for (r in 1:10) for (j in 1:p)
  rect(j - 0.42, r - 0.42, j + 0.42, r + 0.42, border = "gray70",
       col = if (X10[r, j] == 1) "gray25" else "white")
points(rep(0.0, 10), 1:10, pch = 21, bg = pcol, cex = 1.6, xpd = TRUE)
axis(2, at = 1:10, labels = 1:10, las = 1, tick = FALSE, line = 0.3)
abline(v = c(4.5, 8.5, 12.5), col = "gray40", lwd = 1.5)
mtext(c("Verbal", "Letter", "Matrix", "Rotation"), side = 1, at = c(2.5, 6.5, 10.5, 14.5), line = 0.6, cex = 0.8)
mtext("Item type (4 items each)", side = 1, line = 2.6, cex = 0.8)
text(p + 1.3, 1:10, rowSums(X10), cex = 0.95)
text(p + 2.9, 1:10, sprintf("%.2f", alpha_i[sel]), cex = 0.95)
text(p + 1.3, 10.95, "Total", cex = 0.85, xpd = TRUE)
text(p + 2.9, 10.95, expression(alpha[i]), cex = 1.0, xpd = TRUE)
mtext("Filled cell = correct; open cell = incorrect", side = 1, line = 3.9, cex = 0.75, col = "gray30")

## (B) latent space: all respondents (gray) and the 10 shown in Panel A
lim <- range(F2) * 1.1
plot(F2, pch = 16, col = adjustcolor("gray60", 0.35), cex = 0.6, asp = 1, xlim = lim, ylim = lim, las = 1,
     xlab = sprintf("Dimension 1 (%.1f%%)", pct[1]), ylab = sprintf("Dimension 2 (%.1f%%)", pct[2]),
     main = "(B) Latent Space (MCA)")
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
cat("  Saved: Figure1_ALSI_Latent_Space_ICAR.pdf\n")
cat("  Respondents shown (1 = lowest alpha_i, 10 = highest):\n")
for (r in 1:10) cat(sprintf("    %2d  row %4d  alpha_i = %.3f  total = %2d\n",
                            r, sel[r], alpha_i[sel[r]], sum(X10[r, ])))
cat("\n")

## ---- Step 7: Figure 3 - distribution of alpha_i ------------
## Figure 2 (cALSI) is produced by the Tutorial 2 script.
cat("Step 7: Generating Figure 3 (distribution of alpha_i)...\n")

alpha_mean   <- mean(alpha_i)
alpha_median <- median(alpha_i)
alpha_sd     <- sd(alpha_i)
alpha_skew   <- skewness(alpha_i)

pdf("Figure3_ALSI_Distribution_ICAR.pdf", width = 8, height = 6, family = "Helvetica")
par(mar = c(5, 5, 4, 2))
hist_breaks <- seq(min(alpha_i) - 0.001, max(alpha_i) + 0.001, length.out = 25)
hist(alpha_i, breaks = hist_breaks, col = "steelblue", border = "white",
     main = "Distribution of ALSI Values (ICAR Ability Items)",
     xlab = expression(paste("ALSI (", alpha[i], ")")), ylab = "Frequency",
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
cat("  Saved: Figure3_ALSI_Distribution_ICAR.pdf\n\n")

## ---- Step 8: Figure 4 - most and least differentiated profiles
cat("Step 8: Generating Figure 4 (highest and lowest alpha_i)...\n")

domain_scores <- sapply(domains, function(cols) rowMeans(bin_items[, cols]))  # proportion correct
hi <- which.max(alpha_i); lo <- which.min(alpha_i)

pdf("Figure4_Contrasting_Profiles_ICAR.pdf", width = 10, height = 5, family = "Helvetica")
par(mfrow = c(1, 2), mar = c(5, 5, 4, 2))
for (case in list(list(i = hi, lab = "(A) Highest"), list(i = lo, lab = "(B) Lowest"))) {
  ## Outlined box = maximum possible (all four items correct); filled part =
  ## observed proportion. The outline keeps a zero score visible as an empty box.
  bp <- barplot(rep(1, length(domains)), names.arg = names(domains), ylim = c(0, 1.1),
                col = "gray96", border = "gray55",
                main = bquote(.(case$lab) ~ alpha[i] == .(sprintf("%.2f", alpha_i[case$i]))),
                ylab = "Proportion correct", xlab = "Item type", las = 1)
  barplot(domain_scores[case$i, ], add = TRUE, col = "steelblue", border = NA,
          axes = FALSE, names.arg = rep("", length(domains)))
  text(bp, domain_scores[case$i, ] + 0.05, sprintf("%.2f", domain_scores[case$i, ]), cex = 0.9)
  mtext(sprintf("Total score: %d of %d correct", sum(bin_items[case$i, ]), p), side = 3, line = 0.3, cex = 0.9)
}
dev.off()
cat(sprintf("  Highest alpha_i = %.4f (row %d); lowest alpha_i = %.4f (row %d)\n",
            alpha_i[hi], hi, alpha_i[lo], lo))
cat("  Saved: Figure4_Contrasting_Profiles_ICAR.pdf\n\n")

## ---- Step 9: Profile shape versus overall level ------------
## ALSI is meant to capture profile shape rather than overall level. With
## ability items, which share a strong general factor, this needs checking.
cat("Step 9: Profile shape versus overall level...\n\n")

total      <- rowSums(bin_items)                       # overall level (0-16 correct)
extremity  <- abs(total - mean(total))                 # distance of total from the mean
unevenness <- apply(domain_scores, 1, sd)              # spread across the 4 item types

cat(sprintf("  Total score: M = %.2f, SD = %.2f (0-%d possible)\n", mean(total), sd(total), p))
cat(sprintf("  r(alpha_i, total score)            = %.3f\n", cor(alpha_i, total)))
cat(sprintf("  r(alpha_i, |total - mean total|)   = %.3f\n", cor(alpha_i, extremity)))
cat(sprintf("  r(alpha_i, SD across item types)   = %.3f\n", cor(alpha_i, unevenness)))
cat(sprintf("  r(|total - mean|, SD across types) = %.3f\n\n", cor(extremity, unevenness)))

## How much of alpha_i does total score explain? Total is entered as a factor
## (17 levels, 0-16 correct), so any form of dependence on level is captured.
fit_level <- lm(alpha_i ~ factor(total))
alpha_adj <- resid(fit_level)                          # alpha_i adjusted for total score
cat(sprintf("  R-squared, alpha_i ~ total score (as factor) = %.3f\n", summary(fit_level)$r.squared))
cat(sprintf("  r(adjusted alpha_i, |Dimension 2 coordinate|) = %.3f\n", cor(alpha_adj, abs(fit$F[, 2]))))
cat(sprintf("  r(Dimension 1, total score) = %.3f\n", cor(fit$F[, 1], total)))
cat(sprintf("  r(Dimension 2, rotation minus other item types) = %.3f\n",
            cor(fit$F[, 2], domain_scores[, "Rotate"] - rowMeans(domain_scores[, c("Verbal", "Letter", "Matrix")]))))
## Category coordinates on Dimensions 1 and 2, by item type and response
## (cited in Sections 3.2 and 5.1). The sign of a dimension is arbitrary.
G12 <- fit$G[, 1:2, drop = FALSE]
if (!is.null(rownames(G12))) {
  cat("  Category coordinates (min to max across the four items of each type):\n")
  cat(sprintf("    %-8s %-10s %16s %16s\n", "Type", "Response", "Dimension 1", "Dimension 2"))
  for (d in names(domains)) for (resp in c(1, 0)) {
    g <- G12[rownames(G12) %in% paste0(domains[[d]], "_", resp), , drop = FALSE]
    cat(sprintf("    %-8s %-10s %7.2f to %5.2f %7.2f to %5.2f\n", d, if (resp == 1) "correct" else "incorrect",
                min(g[, 1]), max(g[, 1]), min(g[, 2]), max(g[, 2])))
  }
}
if (!is.null(rownames(G12))) {
  cat("  Category coordinates by item (Supplementary Table S3):\n")
  cat(sprintf("    %-10s %8s %8s %8s %8s\n", "Item", "D1 corr", "D1 inc", "D2 corr", "D2 inc"))
  for (v in unlist(domains)) cat(sprintf("    %-10s %8.2f %8.2f %8.2f %8.2f\n", v,
      G12[paste0(v, "_1"), 1], G12[paste0(v, "_0"), 1], G12[paste0(v, "_1"), 2], G12[paste0(v, "_0"), 2]))
}
cat(sprintf("  Respondents with 0 correct: %d (alpha_i = %.3f); with %d correct: %d (alpha_i = %.3f)\n\n",
            sum(total == 0), max(alpha_i[total == 0]), p, sum(total == p), max(alpha_i[total == p])))
cat("  A high correlation with |total - mean| and a high R-squared mean that\n")
cat("  alpha_i largely reflects how far the overall score lies from the mean.\n\n")

## ---- Step 10: Save outputs ----------------------------------
cat("Step 10: Saving outputs...\n")

tutorial1_icar <- data.frame(bin_items, total = total, alpha_i = alpha_i, alpha_adj = alpha_adj)
save(tutorial1_icar, alsi_result, alsi_final, K_final,
     file = "tutorial1_icar_alsi.rda", compress = "xz")
cat("  Saved: tutorial1_icar_alsi.rda (analysed data, alpha_i, workflow output)\n\n")

cat("============================================================\n")
cat("TUTORIAL 1 COMPLETE\n")
cat("============================================================\n\n")
sessionInfo()
