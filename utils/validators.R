# validators.R
# Input/data validators and sanitizers

sanitize_filename <- function(name) {
  nm <- basename(name)
  nm <- gsub("[^A-Za-z0-9._-]", "_", nm)
  nm
}

sanitize_text <- function(x) {
  if (is.null(x)) return("")
  x <- as.character(x)
  x <- gsub("[\r\n\t]+", " ", x)
  x <- gsub("\\s{2,}", " ", x)
  trimws(x)
}

validate_file_input <- function(file_info, allowed_ext = c("csv", "tsv", "xlsx", "xls"), max_size_bytes = 50 * 1024^2) {
  if (is.null(file_info)) return(list(ok = FALSE, code = "upload_format", message = "No file uploaded"))

  ext <- tolower(tools::file_ext(file_info$name %||% ""))
  if (!(ext %in% allowed_ext)) {
    return(list(ok = FALSE, code = "upload_format", message = "Invalid file extension"))
  }

  if (isTRUE(file_info$size > max_size_bytes)) {
    return(list(ok = FALSE, code = "upload_size", message = "File exceeds maximum size"))
  }

  list(ok = TRUE, code = "ok", message = "valid")
}

validate_data_structure <- function(df, min_rows = 1, min_cols = 1, required_cols = NULL) {
  if (is.null(df) || !is.data.frame(df)) return(list(ok = FALSE, code = "parse_error", message = "Not a tabular dataset"))
  if (nrow(df) < min_rows) return(list(ok = FALSE, code = "insufficient_data", message = "Not enough rows"))
  if (ncol(df) < min_cols) return(list(ok = FALSE, code = "insufficient_data", message = "Not enough columns"))

  if (!is.null(required_cols)) {
    missing <- setdiff(required_cols, names(df))
    if (length(missing)) return(list(ok = FALSE, code = "insufficient_data", message = paste("Missing required columns:", paste(missing, collapse = ", "))))
  }

  list(ok = TRUE, code = "ok", message = "valid")
}

validate_numeric_range <- function(value, min_val = -Inf, max_val = Inf, required = FALSE) {
  if (required && (is.null(value) || length(value) == 0 || is.na(value))) return(FALSE)
  if (is.null(value) || is.na(value)) return(TRUE)
  is.numeric(value) && value >= min_val && value <= max_val
}

check_memory_guard <- function(df, threshold_cells = 5e7) {
  if (is.null(df) || !is.data.frame(df)) return(FALSE)
  (nrow(df) * ncol(df)) > threshold_cells
}

sample_large_data <- function(df, max_rows = 100000, seed = 42) {
  if (is.null(df) || !is.data.frame(df)) return(df)
  if (nrow(df) <= max_rows) return(df)
  set.seed(seed)
  df[sample(seq_len(nrow(df)), max_rows), , drop = FALSE]
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
