# alsi: Aggregated Latent Space Index

<!-- badges: start -->
[![R-CMD-check](https://github.com/sekangakim/alsi/workflows/R-CMD-check/badge.svg)](https://github.com/sekangakim/alsi/actions)
[![CRAN status](https://www.r-pkg.org/badges/version/alsi)](https://CRAN.R-project.org/package=alsi)
<!-- badges: end -->

## Overview

The **alsi** package provides stability-validated methods for quantifying individual differences in multivariate profile differentiation across both categorical and continuous data.

### Key Features

- **ALSI** for categorical/binary data (via Multiple Correspondence Analysis)
- **cALSI** for continuous data (via ipsatized Singular Value Decomposition)
- Three-stage validation pipeline:
  1. Parallel analysis (dimensionality assessment)
  2. Procrustes principal angles (space-level stability)
  3. Tucker's congruence coefficients (dimension-level stability)
- Variance-weighted aggregation across validated dimensions
- Complete workflows with diagnostic plots

## Installation

### Development version from GitHub

```r
# install.packages("devtools")
devtools::install_github("sekangakim/alsi")
```

### CRAN version (when available)

```r
install.packages("alsi")
```

## Quick Start

### For Categorical Data (ALSI)

```r
library(alsi)

# Example: Binary survey data
data("tutorial_personality")  # Built-in example data

# Complete workflow
results <- alsi_workflow(
  path = personality_items,
  vars = c("item1", "item2", "item3", ...),
  B_pa = 2000,      # Parallel analysis iterations
  B_boot = 2000     # Bootstrap samples
)

# Extract ALSI values (alpha_i)
alpha_i <- results$alsi$alpha

# Use in subsequent analysis
model <- lm(outcome ~ alpha_i + covariates, data = mydata)
```

### For Continuous Data (cALSI)

```r
library(alsi)

# Example: Cognitive test scores
data("tutorial_cognitive")  # Built-in example data

# Complete workflow
results <- calsi_workflow(
  data = test_scores,  # N x p matrix of continuous scores
  B_pa = 2000,
  B_boot = 2000
)

# Extract cALSI values (alpha_i)
alpha_i <- results$calsi$alpha

# Use in subsequent analysis
model <- lm(outcome ~ alpha_i + age + gender, data = mydata)
```

## What is Profile Differentiation?

Profile differentiation quantifies how much an individual's scores vary across multiple dimensions, independent of their overall elevation (mean level).

**Example:** Two individuals with the same mean IQ (M = 100) can have vastly different profiles:
- **Flat profile** (low α): All scores near 100 (VCI=100, PRI=100, WMI=100, PSI=100)
- **Differentiated profile** (high α): Large peaks and valleys (VCI=130, PRI=115, WMI=90, PSI=75)

cALSI/ALSI captures this differentiation as a single, interpretable index.

## When to Use ALSI vs. cALSI

| Data Type | Method | Function | Example |
|-----------|--------|----------|---------|
| **Binary** | ALSI | `alsi()` | Yes/No survey items |
| **Ordinal** | ALSI | `alsi()` | Likert scales (1-5, 1-7) |
| **Nominal** | ALSI | `alsi()` | Categorical responses |
| **Continuous** | cALSI | `calsi()` | Test scores, ratings, RT |
| **Mixed** | Both | Compare results | Use judgment based on majority type |

## The ALSI Framework

Both ALSI and cALSI follow the same logic:

1. **Extract latent dimensions** (MCA for categorical, SVD for continuous)
2. **Validate stability** via bootstrap (Tucker's φ, principal angles)
3. **Aggregate** person coordinates using variance weighting:

$$\alpha_i = \sqrt{\sum_{k=1}^{K} w_k f_{ik}^2}$$

where:
- *f_ik* = person *i*'s coordinate on dimension *k*
- *w_k* = variance weight (proportion of explained variance)
- *K* = number of validated dimensions

## Key Advantages

### Over Traditional Approaches

- **vs. Single dimensions**: Aggregates across all meaningful dimensions
- **vs. Biplots**: Principled aggregation when K > 2 or K is odd
- **vs. Cluster analysis**: Preserves continuous variation
- **vs. Mahalanobis D²**: No need to specify reference profile

### Unique Features

- ✅ **Stability validation**: Only stable dimensions are aggregated
- ✅ **Variance weighting**: Dimensions weighted by importance
- ✅ **Continuous output**: Single index per person for downstream analysis
- ✅ **Interpretable**: Geometric distance from centroid in pattern space

## Documentation

- Full tutorials: `vignette("alsi-tutorial")` and `vignette("calsi-tutorial")`
- Function reference: `help(package = "alsi")`
- Methodological papers:
  - ALSI (categorical): Kim (under review), *Psychological Methods*
  - cALSI (continuous): Kim (202X), *Psychological Methods*

## Citation

If you use this package in your research, please cite:

```
Kim, S.-K. (under review). The Aggregated Latent Space Index (ALSI): A 
stability-validated measure of multivariate profile differentiation for 
categorical data. Psychological Methods.

Kim, S.-K. (202X). The Continuous Aggregated Latent Space Index (cALSI): 
Extending stability-validated profile aggregation to continuous data. 
Psychological Methods.
```

## Getting Help

- Bug reports: <https://github.com/sekangakim/alsi/issues>
- Questions: Open a discussion on GitHub
- Email: sekang.kim@example.edu

## License

GPL (>= 3)

## Development

This package is under active development. Contributions are welcome!

```r
# Run tests
devtools::test()

# Build documentation
devtools::document()

# Check package
devtools::check()
```

---

**Maintained by Se-Kang Kim** | [Website](https://example.com) | [GitHub](https://github.com/sekangakim)
