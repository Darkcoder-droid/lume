# helpers.R
# General utility functions for the application

#' Clean Column Names
#' @param df A data frame
#' @return A data frame with cleaned column names using janitor
clean_data_names <- function(df) {
  if (is.null(df)) return(NULL)
  df %>% janitor::clean_names()
}

#' Format Currency
#' @param x Numeric value
#' @return Formatted string
format_currency <- function(x) {
  scales::dollar(x)
}

#' Get File Extension
#' @param path File path
#' @return Extension string
get_file_ext <- function(path) {
  tools::file_ext(path)
}
