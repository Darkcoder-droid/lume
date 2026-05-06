# config.R
# Global configuration settings and theming for the Shiny application

# Load local .env without extra dependencies.
load_local_env <- function(path = ".env") {
  if (!file.exists(path)) return(invisible(FALSE))
  lines <- readLines(path, warn = FALSE)
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]
  lines <- lines[!startsWith(lines, "#")]
  for (ln in lines) {
    if (!grepl("=", ln, fixed = TRUE)) next
    kv <- strsplit(ln, "=", fixed = TRUE)[[1]]
    if (length(kv) < 2) next
    key <- trimws(kv[1])
    val <- paste(kv[-1], collapse = "=")
    if (!nzchar(key)) next
    if (!nzchar(Sys.getenv(key, ""))) {
      do.call(Sys.setenv, stats::setNames(list(val), key))
    }
  }
  invisible(TRUE)
}
load_local_env(".env")

# --- Application Settings ---

# Set maximum upload size to 50 MB
UPLOAD_MAX_MB <- as.numeric(Sys.getenv("LUME_MAX_UPLOAD_MB", "50"))
options(shiny.maxRequestSize = UPLOAD_MAX_MB * 1024^2)
options(shiny.trace = identical(tolower(Sys.getenv("LUME_SHINY_TRACE", "false")), "true"))

# Allowed file types for upload
ALLOWED_EXTENSIONS <- c(".csv", ".tsv", ".xlsx", ".xls")
ALLOWED_MIME_TYPES <- c(
  "text/csv", 
  "text/tab-separated-values",
  "text/comma-separated-values", 
  "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
  "application/vnd.ms-excel"
)

# --- UI Theme (bslib) ---
# Professional "Tableau/Mode Analytics" aesthetic
# Focused on clarity, high-quality typography, and purposeful whitespace

app_theme <- bslib::bs_theme(
  version = 5,
  bootswatch = "flatly", # Clean, professional base
  primary = "#2C3E50",   # Deep Slate (Tableau-esque)
  secondary = "#95A5A6", # Neutral Gray
  success = "#18BC9C",   # Modern Green
  base_font = bslib::font_google("Inter"), # High-legibility modern sans-serif
  heading_font = bslib::font_google("Outfit"), # Distinctive heading font
  "navbar-bg" = "#FFFFFF",
  "navbar-light-color" = "#2C3E50",
  "body-bg" = "#F8F9FA" # Very light gray for that premium dashboard feel
)

# Custom constants
APP_TITLE <- "Lume Data Analytics"
APP_VERSION <- "1.0.0"

# Performance controls
LUME_SAMPLE_THRESHOLD <- as.numeric(Sys.getenv("LUME_SAMPLE_THRESHOLD", "100000"))
