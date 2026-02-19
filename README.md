# alsi: Aggregated Latent Space Index for Multiple Correspondence Analysis

## Overview

The `alsi` package provides tools for stability-validated aggregation in
Multiple Correspondence Analysis (MCA). It addresses the challenge of
dimensional multiplicity by computing the Aggregated Latent Space Index
(ALSI), a person-level summary measure derived from validated MCA dimensions.

## Installation

You can install the released version of `alsi` from CRAN with:

```r
install.packages("alsi")
```

Or install the development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("sekangakim/alsi")
```

## Features

* **Parallel Analysis:** Automated dimensionality assessment for MCA
* **Bootstrap Stability Diagnostics:** Subspace and dimension-level reproducibility testing
* **ALSI Computation:** Variance-weighted aggregation across stable dimensions
* **Visualization Tools:** Diagnostic plots and category projections
* **Complete Workflow:** Automated pipeline from data to interpretation

## Quick Example

```r
library(alsi)

# Load example data
data(ANR2)
vars <- c("MDD", "DYS", "DEP", "PTSD", "OCD", "GAD", "ANX", "SOPH", "ADHD")

# Run complete workflow
results <- alsi_workflow(
  data  = ANR2,
  vars  = vars,
  B_pa  = 2000,     # Parallel analysis permutations
  B_boot = 2000,    # Bootstrap resamples
  seed  = 20260123
)

# Extract ALSI values
alpha_values <- results$alsi$alpha
summary(alpha_values)
```

## Citation

If you use this software in published research, please cite:

Kim, S.-K. (2026). *alsi*: Aggregated Latent Space Index for Multiple
Correspondence Analysis (R package version 0.1.3).

## License

GPL-3

## Author

Se-Kang Kim  
Department of Pediatrics  
Baylor College of Medicine  
se-kang.kim@bcm.edu
