# alsi 2.0.0

## Major Changes

### New Features

* **Added cALSI for continuous data** 🎉
  * New `calsi()` function for computing ALSI on continuous data via ipsatized SVD
  * New `calsi_workflow()` for streamlined analysis of continuous profiles
  * New `svd_pa()` for parallel analysis on continuous data (compatible with SEPA)
  * New `svd_bootstrap()` for bootstrap stability assessment
  * New plotting functions: `plot_subspace_stability_cont()`, `plot_domain_loadings()`

* **Unified framework**
  * ALSI (categorical) and cALSI (continuous) now in single package
  * Consistent API across both variants
  * Shared utility functions and validation pipeline
  * Common S3 methods for printing and plotting

* **Enhanced stability validation**
  * Principal angles via Procrustes rotation (both variants)
  * Tucker's congruence coefficients (both variants)
  * Comprehensive bootstrap diagnostics
  * User-friendly interpretation guidelines

* **Comparison utilities**
  * New `compare_sepa_calsi()` for comparing with SEPA plane-wise norms
  * New `calsi_vs_sepa_demo()` demonstrating cALSI advantages

### Documentation

* Added comprehensive README with quick start guides
* Added tutorial vignettes for both ALSI and cALSI
* Improved function documentation with examples
* Added methodological references to peer-reviewed papers

### Infrastructure

* Package now requires R >= 4.0.0
* Suggests `paran` package for SEPA-compatible parallel analysis
* Added unit tests for core functions
* GitHub Actions for continuous integration

## Breaking Changes

* **None** - All original `alsi()` functionality is preserved and backward-compatible

## Bug Fixes

* Fixed edge case handling when K = 1 in both ALSI and cALSI
* Improved error messages throughout
* Fixed plot margins in stability diagnostic plots

---

# alsi 1.0.0 (Original Release)

## Initial Features

* `alsi()` function for categorical/binary data
* `mca_indicator()` for Multiple Correspondence Analysis
* `mca_pa()` for parallel analysis on categorical data
* `mca_bootstrap()` for bootstrap stability assessment
* `alsi_workflow()` for complete analysis pipeline
* Plotting functions for category projections and stability diagnostics
* Example datasets and workflows
