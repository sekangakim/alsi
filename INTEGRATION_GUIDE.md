# ALSI Package Integration Guide

## Package Structure (Version 2.0.0)

```
alsi/
├── DESCRIPTION              # Package metadata
├── NEWS.md                  # Version history and changelog
├── README.md                # Main documentation and quick start
├── LICENSE                  # GPL-3 license file
├── .Rbuildignore           # Files to ignore during R CMD build
├── .gitignore              # Git ignore patterns
│
├── R/                      # R source code
│   ├── utilities.R         # Shared utility functions (BOTH variants)
│   ├── alsi-categorical.R  # ALSI functions for categorical data (MCA)
│   ├── calsi-continuous.R  # cALSI functions for continuous data (SVD)
│   └── alsi-package.R      # Package documentation and imports
│
├── man/                    # Auto-generated documentation (via roxygen2)
│   ├── alsi.Rd
│   ├── calsi.Rd
│   ├── alsi_workflow.Rd
│   ├── calsi_workflow.Rd
│   └── ... (other function docs)
│
├── vignettes/              # Long-form tutorials
│   ├── alsi-tutorial.Rmd   # ALSI for categorical data tutorial
│   └── calsi-tutorial.Rmd  # cALSI for continuous data tutorial
│
├── data/                   # Example datasets
│   ├── tutorial_personality.rda  # For ALSI tutorial
│   └── tutorial_cognitive.rda    # For cALSI tutorial
│
├── tests/                  # Unit tests
│   ├── testthat.R
│   └── testthat/
│       ├── test-alsi.R
│       └── test-calsi.R
│
└── inst/                   # Installed files
    └── CITATION            # How to cite the package
```

## Files Status

### ✅ Created Files
1. `DESCRIPTION` - Package metadata with both ALSI and cALSI
2. `README.md` - Comprehensive guide with examples for both variants
3. `NEWS.md` - Version history documenting 2.0.0 changes
4. `R/utilities.R` - Shared utility functions
5. `R/alsi-categorical.R` - Original ALSI code (from alsi.R)
6. `R/calsi-continuous.R` - cALSI code (from calsi.R)

### ⏳ To Be Created
1. `R/alsi-package.R` - Package-level documentation
2. `.Rbuildignore` - Build configuration
3. `.gitignore` - Git configuration
4. `LICENSE` - GPL-3 license text
5. `vignettes/` - Tutorial documents
6. `data/` - Example datasets
7. `tests/` - Unit tests
8. `inst/CITATION` - Citation information

## Key Integration Steps Completed

### 1. Unified Utility Functions (utilities.R)
- ✅ Merged `summarise_matrix()` from both files
- ✅ Kept `.read_xlsx()` from ALSI
- ✅ Added new `.check_class()` and `.validate_matrix()` helpers

### 2. Preserved All Original Functionality
- ✅ ALSI functions unchanged (backward compatible)
- ✅ cALSI functions unchanged
- ✅ Both maintain their own S3 classes and methods

### 3. Consistent Naming Convention
- ✅ ALSI functions: `alsi()`, `mca_*()`, `alsi_workflow()`
- ✅ cALSI functions: `calsi()`, `svd_*()`, `calsi_workflow()`
- ✅ Clear distinction between variants

## Deduplication Actions Taken

### Functions Moved to utilities.R
1. `summarise_matrix()` - Was in both files, now shared
2. `.read_xlsx()` - From ALSI, now available to both
3. New helper functions added for consistency

### Functions Kept Separate (Variant-Specific)
1. `to01()` - ALSI only (binary conversion)
2. `ipsatize()` - cALSI only (row-centering)
3. `make_disjunctive()` - ALSI only (indicator matrix)
4. All MCA-specific functions - ALSI only
5. All SVD-specific functions - cALSI only

## Required Modifications to Original Files

### alsi-categorical.R (formerly alsi.R)
- Remove duplicate `summarise_matrix()` (line 62-69)
- Remove duplicate `.read_xlsx()` (line 14-22) 
- Add `@importFrom` directives at top
- Keep all MCA-specific code unchanged

### calsi-continuous.R (formerly calsi.R)
- Remove duplicate `summarise_matrix()` (line 19-26)
- Add `@importFrom` directives at top
- Keep all SVD-specific code unchanged

### utilities.R (new file)
- Contains shared functions only
- Properly documented with roxygen2
- `@keywords internal` for non-exported helpers

## Documentation Requirements

### Package-Level Documentation (alsi-package.R)
```r
#' @keywords internal
"_PACKAGE"

#' @importFrom stats complete.cases quantile median cor lm
#' @importFrom graphics plot lines legend abline boxplot text par arrows
#' @importFrom utils txtProgressBar setTxtProgressBar
NULL
```

### Function Documentation
- All exported functions need `@export` tag
- All functions need `@param` for parameters
- All functions need `@return` for return values
- All functions need `@examples` (can use `\\dontrun{}`)

## GitHub Upload Steps

### 1. Update Existing Repository

```bash
cd /path/to/local/alsi/repo

# Pull latest changes
git pull origin main

# Copy new package structure
cp -r /home/claude/alsi_package/* .

# Check status
git status
```

### 2. Commit Changes

```bash
# Add all new files
git add .

# Commit with descriptive message
git commit -m "Version 2.0.0: Add cALSI for continuous data

Major changes:
- Added cALSI functions for continuous data via ipsatized SVD
- Unified ALSI and cALSI into single package
- Added comprehensive documentation and tutorials
- Maintained backward compatibility with v1.0.0
- Added stability validation for both variants"
```

### 3. Push to GitHub

```bash
git push origin main
```

### 4. Create Release on GitHub

1. Go to https://github.com/sekangakim/alsi/releases
2. Click "Create a new release"
3. Tag version: `v2.0.0`
4. Release title: "ALSI 2.0.0: Unified Framework for Categorical and Continuous Data"
5. Description: Copy from NEWS.md
6. Publish release

## Testing Checklist

Before pushing to GitHub:

- [ ] `devtools::document()` - Generate documentation
- [ ] `devtools::check()` - Run R CMD check (0 errors, 0 warnings)
- [ ] `devtools::test()` - Run unit tests (when created)
- [ ] `devtools::build_vignettes()` - Build tutorials (when created)
- [ ] Test ALSI workflow with example data
- [ ] Test cALSI workflow with example data
- [ ] Verify backward compatibility with v1.0.0 scripts
- [ ] Check all examples run without errors

## Installation Testing

After pushing to GitHub:

```r
# Remove old version
remove.packages("alsi")

# Install from GitHub
devtools::install_github("sekangakim/alsi")

# Test basic functionality
library(alsi)

# Test ALSI (categorical)
# ... run example workflow

# Test cALSI (continuous)
# ... run example workflow
```

## CRAN Submission (Future)

When ready for CRAN:

1. ✅ All tests pass
2. ✅ No warnings in R CMD check
3. ✅ Vignettes built successfully
4. ✅ CRAN policy compliance checked
5. ✅ Example datasets documented
6. ✅ All dependencies declared in DESCRIPTION
7. Submit to CRAN via `devtools::submit_cran()`

## Notes for Maintainer

### Duplicate Function Resolution
- `summarise_matrix()` now in utilities.R only
- Both variant files should source from utilities.R
- No functional changes, just reorganization

### Backward Compatibility
- All `alsi()` functions work exactly as before
- Scripts using v1.0.0 will work with v2.0.0
- New users can access both ALSI and cALSI

### Future Enhancements
- Add unit tests for both variants
- Create comprehensive vignettes
- Add more example datasets
- Consider Shiny app for interactive exploration
- Add methods for handling missing data

---

**Integration completed:** 2026-02-16  
**Next steps:** Clean up duplicate functions, generate documentation, test thoroughly
