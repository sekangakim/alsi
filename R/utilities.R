## =======================================================================
## Shared Utility Functions for ALSI Package
## Used by both categorical (ALSI) and continuous (cALSI) variants
## =======================================================================

#' Summarize matrix columns (median and quantiles)
#' 
#' @param X Matrix to summarize
#' @param probs Numeric vector of probabilities for quantiles
#' @return Matrix with median and quantile summaries for each column
#' @keywords internal
summarise_matrix <- function(X, probs = c(0.05, 0.95)) {
  t(apply(X, 2, function(x) {
    c(median = median(x, na.rm = TRUE),
      setNames(quantile(x, probs = probs, na.rm = TRUE), 
               paste0("p", probs * 100)))
  }))
}

#' Read Excel file with fallback options
#' 
#' @param path Path to .xlsx file
#' @return Data frame
#' @keywords internal
.read_xlsx <- function(path) {
  if (requireNamespace("readxl", quietly = TRUE)) {
    return(as.data.frame(readxl::read_xlsx(path)))
  }
  if (requireNamespace("openxlsx", quietly = TRUE)) {
    return(as.data.frame(openxlsx::read.xlsx(path)))
  }
  stop("Please install 'readxl' or 'openxlsx' to read .xlsx files.\n",
       "  install.packages('readxl')")
}

#' Check if object inherits from expected class
#' 
#' @param x Object to check
#' @param class_name Expected class name
#' @param arg_name Argument name for error message
#' @keywords internal
.check_class <- function(x, class_name, arg_name = "object") {
  if (!inherits(x, class_name)) {
    stop(sprintf("%s must be of class '%s'", arg_name, class_name), 
         call. = FALSE)
  }
  invisible(TRUE)
}

#' Validate numeric matrix dimensions
#' 
#' @param X Matrix to validate
#' @param min_rows Minimum number of rows required
#' @param min_cols Minimum number of columns required
#' @keywords internal
.validate_matrix <- function(X, min_rows = 2, min_cols = 2) {
  X <- as.matrix(X)
  
  if (nrow(X) < min_rows) {
    stop(sprintf("Matrix must have at least %d rows", min_rows), 
         call. = FALSE)
  }
  
  if (ncol(X) < min_cols) {
    stop(sprintf("Matrix must have at least %d columns", min_cols),
         call. = FALSE)
  }
  
  invisible(X)
}
