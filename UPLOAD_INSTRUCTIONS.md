# Step-by-Step Instructions for Uploading ALSI 2.0.0 to GitHub

## Overview
You now have a unified ALSI package (version 2.0.0) that includes both:
- **ALSI** for categorical/binary data (original functionality)
- **cALSI** for continuous data (new functionality)

This guide walks you through uploading the integrated package to your existing GitHub repository.

---

## Prerequisites

### 1. Install Required Tools

```r
# Install/update devtools
install.packages("devtools")

# Install roxygen2 for documentation
install.packages("roxygen2")
```

### 2. Locate Your Local Repository

Find where you cloned https://github.com/sekangakim/alsi on your computer.

```bash
# Example locations:
# Mac/Linux: ~/Documents/GitHub/alsi
# Windows: C:\Users\YourName\Documents\GitHub\alsi
```

---

## Step 1: Backup Current Repository

**IMPORTANT**: Before making any changes, back up your current repository.

```bash
# Navigate to your repository
cd /path/to/your/alsi

# Create a backup
cd ..
cp -r alsi alsi_backup_v1.0.0
cd alsi
```

---

## Step 2: Download and Extract New Package Files

The new package structure is in `/home/claude/alsi_package/` on the server.

### Files to Download from This Session:

1. `DESCRIPTION` - Updated package metadata
2. `README.md` - New comprehensive README
3. `NEWS.md` - Version history
4. `INTEGRATION_GUIDE.md` - Integration notes (for reference)
5. `R/alsi-package.R` - Package documentation
6. `R/utilities.R` - Shared utility functions
7. `R/alsi-categorical.R` - ALSI functions (from your alsi.R)
8. `R/calsi-continuous.R` - cALSI functions (from your calsi.R)
9. `.Rbuildignore` - Build configuration
10. `.gitignore` - Git configuration

---

## Step 3: Update Your Local Repository

### Option A: Manual File Replacement (Recommended for First Time)

```bash
cd /path/to/your/alsi

# 1. Replace DESCRIPTION
# Download the new DESCRIPTION file and replace the old one

# 2. Replace/Add README.md
# Download and replace

# 3. Add NEWS.md (new file)
# Download and add

# 4. Update R/ directory
mkdir -p R

# Backup old alsi.R if you want to keep it
mv R/alsi.R R/alsi.R.v1.0.0.backup

# Add new R files:
# - Copy alsi-package.R to R/
# - Copy utilities.R to R/
# - Copy alsi-categorical.R to R/  (this is your updated alsi.R)
# - Copy calsi-continuous.R to R/  (this is your calsi.R)

# 5. Add configuration files
# Copy .Rbuildignore (if not present)
# Update .gitignore
```

### Option B: Complete Replacement (For Advanced Users)

```bash
# Remove old files (CAUTION!)
rm -rf R/
rm DESCRIPTION
rm README.md

# Copy entire new structure
cp -r /path/to/downloaded/alsi_package/* .
```

---

## Step 4: Remove Duplicate Functions

The integration moved some duplicate functions to `utilities.R`. You need to edit the files:

### Edit R/alsi-categorical.R

Remove these lines (they're now in utilities.R):
- Lines 62-69: `summarise_matrix()` function
- Lines 14-22: `.read_xlsx()` function

Add at the top (after the header comments):
```r
## Uses shared utilities from utilities.R
## - summarise_matrix()
## - .read_xlsx()
```

### Edit R/calsi-continuous.R

Remove these lines:
- Lines 19-26: `summarise_matrix()` function  

Add at the top:
```r
## Uses shared utilities from utilities.R
## - summarise_matrix()
```

---

## Step 5: Generate Documentation

```r
# In R, navigate to your package directory
setwd("/path/to/your/alsi")

# Load devtools
library(devtools)

# Generate documentation from roxygen comments
document()

# This creates .Rd files in the man/ directory
```

---

## Step 6: Check Package Integrity

```r
# Run R CMD check
check()

# Expected result:
# ── R CMD check results ─── alsi 2.0.0 ────
# Duration: X seconds
# 
# 0 errors ✔ | 0 warnings ✔ | 0 notes ✔
```

**If you get errors/warnings:**
- Read the error messages carefully
- Most common issue: Missing roxygen tags (@export, @param, etc.)
- Fix issues and run `check()` again

---

## Step 7: Test the Package Locally

```r
# Install your updated package locally
install()

# Load it
library(alsi)

# Test ALSI (categorical) - should work exactly as before
# ... run your existing v1.0.0 scripts

# Test cALSI (continuous) - new functionality
# ... run calsi_workflow() with test data
```

---

## Step 8: Commit Changes to Git

```bash
cd /path/to/your/alsi

# Check current status
git status

# Should show:
# - Modified: DESCRIPTION, README.md
# - New files: NEWS.md, R/utilities.R, R/calsi-continuous.R, etc.

# Stage all changes
git add .

# Commit with detailed message
git commit -m "Version 2.0.0: Add cALSI for continuous data

Major changes:
- Added cALSI functions for continuous data via ipsatized SVD
- Unified ALSI and cALSI into single package
- Created shared utilities.R for common functions
- Added comprehensive documentation and tutorials
- Maintained complete backward compatibility with v1.0.0

New features:
- calsi() function for continuous data
- calsi_workflow() for streamlined analysis
- svd_pa() and svd_bootstrap() for stability validation
- compare_sepa_calsi() for methodological comparison

Files added:
- NEWS.md (version history)
- R/alsi-package.R (package documentation)
- R/utilities.R (shared functions)
- R/calsi-continuous.R (cALSI implementation)

Files modified:
- DESCRIPTION (updated for v2.0.0)
- README.md (comprehensive guide for both variants)
- R/alsi-categorical.R (removed duplicated utilities)"
```

---

## Step 9: Push to GitHub

```bash
# Push to GitHub
git push origin main

# If you get an error about upstream, set it:
# git push --set-upstream origin main
```

---

## Step 10: Create GitHub Release (Recommended)

1. Go to https://github.com/sekangakim/alsi
2. Click on "Releases" (right sidebar)
3. Click "Create a new release"
4. Fill in:
   - **Tag version**: `v2.0.0`
   - **Release title**: `ALSI 2.0.0: Unified Framework for Categorical and Continuous Data`
   - **Description**: 
   ```
   Major update adding support for continuous data via cALSI.
   
   🎉 New Features:
   - cALSI for continuous data (ipsatized SVD)
   - Unified framework with ALSI (categorical data)
   - Complete stability validation pipeline
   - Comprehensive documentation and examples
   
   ✅ Backward Compatibility:
   - All ALSI (v1.0.0) functionality preserved
   - Existing scripts will work without modification
   
   📚 Documentation:
   - Updated README with examples for both variants
   - See NEWS.md for detailed changelog
   
   For more information, see the README at:
   https://github.com/sekangakim/alsi
   ```
5. Click "Publish release"

---

## Step 11: Verify Installation from GitHub

Test that others can install your package:

```r
# Remove local version
remove.packages("alsi")

# Install from GitHub
devtools::install_github("sekangakim/alsi")

# Test
library(alsi)

# Should see both alsi() and calsi() available
ls("package:alsi")
```

---

## Troubleshooting

### Problem: `check()` fails with "object not found" errors

**Solution**: Make sure all `@export` tags are present in roxygen comments

### Problem: Git push rejected

**Solution**: Pull first, then push
```bash
git pull origin main
git push origin main
```

### Problem: Functions from utilities.R not found

**Solution**: Make sure utilities.R is in R/ directory and functions are properly documented

### Problem: Package won't install from GitHub

**Solution**: Check that DESCRIPTION file is valid and all dependencies are listed

---

## Next Steps (Optional)

### 1. Add Vignettes

Create tutorial documents in `vignettes/`:
- `alsi-tutorial.Rmd` - For categorical data
- `calsi-tutorial.Rmd` - For continuous data

### 2. Add Example Datasets

Create `data/` directory with:
- `tutorial_personality.rda` - Example categorical data
- `tutorial_cognitive.rda` - Example continuous data

### 3. Add Unit Tests

Create `tests/testthat/` with:
- `test-alsi.R` - Tests for categorical functions
- `test-calsi.R` - Tests for continuous functions

### 4. Submit to CRAN (Future)

When ready, submit to CRAN using:
```r
devtools::submit_cran()
```

---

## Summary Checklist

- [ ] Backed up current repository
- [ ] Downloaded all new files
- [ ] Replaced old files with new versions
- [ ] Removed duplicate functions from R files
- [ ] Ran `document()` successfully
- [ ] Ran `check()` with 0 errors, 0 warnings
- [ ] Tested package locally (both ALSI and cALSI work)
- [ ] Committed changes to Git
- [ ] Pushed to GitHub
- [ ] Created GitHub release (optional but recommended)
- [ ] Verified installation from GitHub works

---

## Questions?

If you encounter any issues:
1. Check the error message carefully
2. Review this guide
3. Check INTEGRATION_GUIDE.md for technical details
4. Ask for help if needed!

**Congratulations on the unified ALSI 2.0.0 package!** 🎉
