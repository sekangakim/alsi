# Reproducible ALSI and cALSI tutorials

These R scripts accompany the tutorial manuscript *Quantifying Multivariate Profile Differentiation: A Tutorial on the Aggregated Latent Space Index (ALSI) for Binary and Continuous Data* (R3 revision, November 2026).

The examples use freely available datasets supplied by R packages. **No artificial dichotomization of ordinal or continuous variables and no plausible values are used.**

## Tutorial 1: Inherently binary ICAR ability items

- Script: [`Tutorial1_ALSI_ICAR.R`](Tutorial1_ALSI_ICAR.R)
- Source: `psychTools::ability` (16 ICAR items coded 0/1 at source)
- Data: 1,525 records; 1,248 complete cases on all 16 items
- Method: binary ALSI / multiple correspondence analysis, parallel analysis, and bootstrap dimension-stability diagnostics
- Seed: `20260218`; 2,000 permutation datasets and 2,000 bootstrap resamples
- Main outputs include `tutorial1_icar_alsi.rda` and Figures 1, 3, and 4 of the manuscript.

## Tutorial 2: Continuous Holzinger–Swineford cognitive tests

- Script: [`Tutorial2_cALSI_HolzingerSwineford.R`](Tutorial2_cALSI_HolzingerSwineford.R)
- Source: `lavaan::HolzingerSwineford1939` (`x1`–`x9`, nine continuous cognitive tests)
- Data: 301 students, with no missing data on the nine tests
- Method: standardization, within-person centering (ipsatization), SVD-based cALSI, parallel analysis, and bootstrap dimension-stability diagnostics
- Seed: `20260223`; 2,000 reference datasets and 2,000 bootstrap resamples
- Main outputs include `tutorial2_hs1939_calsi.rda` and Figures 2, 5, and 6 of the manuscript.

## How to run

Install the R packages if needed:

```r
install.packages(c("alsi", "psychTools", "lavaan", "moments", "paran"))
```

Start a fresh R session, set the working directory to a folder where you want the output files saved, and run:

```r
source("Tutorial1_ALSI_ICAR.R")
source("Tutorial2_cALSI_HolzingerSwineford.R")
```

Both scripts print `sessionInfo()` after execution. Both R3 tutorials were rerun successfully with the final datasets and retained two stability-validated dimensions (K* = 2). Runtime and the final decimal places of some results can vary across hardware and software environments.

## Software and interpretation notes

The scripts apply a sequential dimension-retention rule: median principal angle < 20° and median Tucker congruence ≥ 0.85. If no dimension satisfies both criteria, the script stops without calculating an ALSI/cALSI value; it does not force K* = 1. A one-dimensional solution remains permissible when the first dimension meets both criteria.

ALSI and cALSI summarize a variance-weighted distance across *retained, stability-assessed* dimensions. For binary responses, ALSI can reflect overall-score extremity as well as item-pattern differences; for continuous data, cALSI removes additive person-level elevation through ipsatization. Interpret the index in relation to the data representation and retained dimensions.

The core R package is available at [CRAN](https://cran.r-project.org/package=alsi). The examples above are tutorial scripts, not replacements for the package implementation under the repository's `R/` directory.
