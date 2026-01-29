# ALSI: Aggregated Latent Space Index for Multiple Correspondence Analysis

[![License: GPL-3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

## Overview

The `alsi` package provides tools for stability-validated aggregation in Multiple Correspondence Analysis (MCA). It addresses the challenge of dimensional multiplicity by computing the Aggregated Latent Space Index (ALSI), a person-level summary measure derived from validated MCA dimensions.

## Features

- **Parallel Analysis**: Automated dimensionality assessment for MCA
- **Bootstrap Stability Diagnostics**: Subspace and dimension-level reproducibility testing
- **ALSI Computation**: Variance-weighted aggregation across stable dimensions
- **Visualization Tools**: Diagnostic plots and category projections
- **Complete Workflow**: Automated pipeline from data to interpretation

## Installation

You can install the development version from GitHub:

```r
# Install devtools if needed
install.packages("devtools")

# Install alsi from GitHub
devtools::install_github("yourusername/alsi")
```

## Quick Start

```r
library(alsi)

# Load example data
data(ANR2)
vars <- c("MDD", "DYS", "DEP", "PTSD", "OCD", "GAD", "ANX", "SOPH", "ADHD")

# Run complete workflow
results <- alsi_workflow(
  data = ANR2,
  vars = vars,
  B_pa = 2000,      # Parallel analysis permutations
  B_boot = 2000,    # Bootstrap resamples
  seed = 20260123
)

# Extract ALSI values
alpha_values <- results$alsi$alpha

# View results
summary(alpha_values)
```

## Main Functions

| Function | Purpose |
|----------|---------|
| `mca_pa()` | Parallel analysis for dimensionality assessment |
| `mca_bootstrap()` | Bootstrap-based subspace stability diagnostics |
| `alsi()` | Compute Aggregated Latent Space Index |
| `mca_align()` | Procrustes alignment of MCA solutions |
| `plot_subspace_stability()` | Visualize stability diagnostics |
| `plot_category_projections()` | Visualize category coordinates |
| `alsi_workflow()` | Automated complete workflow |

## Example: Step-by-Step Analysis

```r
# Step 1: Parallel analysis
pa <- mca_pa(data = ANR2, vars = vars, B = 2000, q = 0.95)
K <- pa$K_star  # Number of dimensions to retain

# Step 2: Bootstrap stability assessment
boot <- mca_bootstrap(data = ANR2, vars = vars, K = K, B = 2000)
plot_subspace_stability(boot)

# Step 3: Compute ALSI
fit <- boot$ref
alsi_obj <- alsi(fit$F, fit$eig, K = K)

# Step 4: Examine category projections
plot_category_projections(fit, K = K, alpha_vec = alsi_obj$alpha_vec)

# Step 5: Use in downstream analyses
ANR2$alpha <- alsi_obj$alpha
model <- lm(outcome ~ predictor + alpha, data = ANR2)
summary(model)
```

## Citation

If you use this software, please cite:

> Kim, S. (2026). The Aggregated Latent Space Index: Software for Stability-Validated 
> Aggregation in Multiple Correspondence Analysis. Journal of Statistical Software. 
> (Submitted)

## Manuscript

The accompanying manuscript is available at:
- Preprint: [Link to be added]
- Journal: Journal of Statistical Software (under review)

## Requirements

- R (>= 4.0.0)
- Optional: `readxl` or `openxlsx` for reading Excel files

## License

This package is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 3 of the License, or (at your option) any later version.

## Author

**Se-Kang Kim**  
Department of Pediatrics  
Baylor College of Medicine  
Email: se-kang.kim@bcm.edu  

## Acknowledgments

Development of this software was supported by research at Baylor College of Medicine.

## Issues and Contributions

If you encounter any issues or have suggestions for improvements, please file an issue on the GitHub repository.
