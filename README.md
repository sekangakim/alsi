# alsi: Aggregated Latent Space Index

An R package implementing stability-validated workflows for constructing
person-level scalar indices from multidimensional association structure
across binary, ordinal, and continuous multivariate data.

## Overview

Multivariate data in behavioral and psychological research commonly exhibit
**dimensional multiplicity** — the tendency for association structure to
distribute across several nontrivial dimensions rather than concentrate in
one or two leading axes. The **Aggregated Latent Space Index (ALSI)**
addresses this challenge by combining:

1. **Parallel analysis** for principled dimensionality assessment
2. **Bootstrap Procrustes stability diagnostics** using principal angles
   and Tucker's congruence coefficients
3. **Variance-weighted aggregation** of validated dimensions into a single
   interpretable person-level index

Three complete pipelines are implemented:

| Pipeline | Data type | Method | Index |
|---|---|---|---|
| Binary | Binary/categorical | Multiple Correspondence Analysis (MCA) | ALSI (non-negative) |
| Ordinal | Likert-type | homals ALS optimal scaling | Ordinal ALSI (signed) |
| Continuous | Continuous domain scores | Ipsatized SVD | cALSI (non-negative) |

## Installation
```r
# Install from CRAN (recommended)
install.packages("alsi")

# Install development version from GitHub
devtools::install_github("sekangakim/alsi")
```

## Quick Start
```r
library(alsi)

# Binary pipeline
data("ANR2")
vars <- c("MDD", "DYS", "DEP", "PTSD", "OCD", "GAD", "ANX", "SOPH", "ADHD")
results_binary <- alsi_workflow(data = ANR2, vars = vars,
                                B_pa = 2000, B_boot = 2000,
                                seed = 20260123)

# Ordinal pipeline
data("BFI_Extraversion")
items <- paste0("E", 1:10)
reversed_items <- c("E2", "E4", "E6", "E8", "E10")
results_ordinal <- alsi_workflow_ordinal(data = BFI_Extraversion,
                                         items = items,
                                         reversed_items = reversed_items,
                                         scale_min = 1L, scale_max = 5L,
                                         B_boot = 1000, seed = 12345)

# Continuous pipeline
data("wawm4")
domains <- c("VC", "PR", "WO", "PS", "IM", "DM", "VWM", "VM", "AM")
results_cont <- calsi_workflow(data = wawm4[, domains],
                               B_pa = 2000, B_boot = 2000,
                               seed = 20260206)
```

## Datasets

| Dataset | Pipeline | N | Description |
|---|---|---|---|
| `ANR2` | Binary | 1,261 | Binary psychiatric diagnostic indicators |
| `BFI_Extraversion` | Ordinal | 500 | Big Five Inventory Extraversion items |
| `wawm4` | Continuous | 900 | WAIS-IV/WMS-IV cognitive domain scores |

## Citation

Kim, S.-K. (2026). *The Aggregated Latent Space Index: A stability-validated
framework for person-level aggregation across binary, ordinal, and continuous
data*. Manuscript submitted for publication.

## Author

**Se-Kang Kim, Ph.D.**
Psychology Division, Department of Pediatrics
Texas Children's Hospital, Baylor College of Medicine
se-kang.kim@bcm.edu
ORCID: [0000-0003-0928-3396](https://orcid.org/0000-0003-0928-3396)

## License

GPL-3
