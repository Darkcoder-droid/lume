# error_handler.R
# Centralized error handling and application logging

LOG_DIR <- Sys.getenv("LUME_LOG_DIR", unset = "logs")
LOG_FILE <- Sys.getenv("LUME_LOG_FILE", unset = file.path(LOG_DIR, "app.log"))
ENABLE_AUTO_ERROR_REPORT <- identical(tolower(Sys.getenv("LUME_ENABLE_AUTO_ERROR_REPORT", "false")), "true")

ensure_log_dir <- function() {
  if (!dir.exists(LOG_DIR)) dir.create(LOG_DIR, recursive = TRUE, showWarnings = FALSE)
}

log_event <- function(level = "INFO", context = "app", message = "", details = NULL) {
  ensure_log_dir()
  line <- paste0(
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    " [", toupper(level), "] ",
    "[", context, "] ",
    message,
    if (!is.null(details)) paste0(" | details=", as.character(details)) else ""
  )
  cat(line, "\n", file = LOG_FILE, append = TRUE)
}

friendly_error_message <- function(type = "generic", fallback = NULL) {
  msg <- switch(
    type,
    upload_size = "This file is too large. Please upload a file up to 50 MB.",
    upload_format = "Unsupported file format. Use CSV, TSV, XLSX, or XLS.",
    upload_corrupt = "The uploaded file appears corrupted or unreadable.",
    parse_error = "We could not parse this dataset. Please verify delimiters/encoding and try again.",
    encoding_error = "Could not decode text encoding cleanly. Try saving the file as UTF-8.",
    insufficient_data = "Not enough valid data to render this visualization.",
    memory_limit = "This dataset is too large for current memory settings. Try sampling or filtering first.",
    network_error = "Network/deployment issue detected. Please retry in a few moments.",
    generic = "Something went wrong. Please try again."
  )

  if (!is.null(fallback) && nzchar(fallback)) fallback else msg
}

notify_error <- function(type = "generic", context = "app", technical = NULL, session = NULL) {
  user_msg <- friendly_error_message(type)
  log_event("ERROR", context, user_msg, details = technical)

  if (!is.null(session)) {
    shiny::showNotification(user_msg, type = "error", duration = 8)
  }

  if (ENABLE_AUTO_ERROR_REPORT) {
    log_event("INFO", context, "Auto error reporting enabled", details = technical)
  }

  invisible(user_msg)
}

safe_execute <- function(expr, context = "app", session = NULL, error_type = "generic") {
  tryCatch(
    expr,
    error = function(e) {
      notify_error(error_type, context = context, technical = e$message, session = session)
      NULL
    }
  )
}
