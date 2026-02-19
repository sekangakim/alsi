# alsi 0.1.3

## Patch Update

* Fixed floating-point precision issue in `mca_pa()` causing `K_star`
  to be overcounted by 1 in edge cases where observed and reference
  eigenvalues are very close in magnitude
* Corrected GitHub repository URL in README
* Renamed `path` argument to `data` in `alsi_workflow()` for
  consistency with `mca_pa()` and `mca_bootstrap()`
* Updated citation version number in README

# alsi 0.1.2

## Initial CRAN Release

* First submission to CRAN
* Implements parallel analysis for MCA dimensionality assessment
* Bootstrap-based subspace stability diagnostics with Procrustes alignment
* Computation of Aggregated Latent Space Index (ALSI)
* Tucker's congruence coefficients for dimension-level similarity
* Visualization tools for stability diagnostics and category projections
* Complete automated workflow function (`alsi_workflow()`)
* Example dataset (ANR2) with eating disorder diagnostic data
* Comprehensive documentation with usage examples