# packages.R
# Load and manage all required packages for the Shiny application

# Function to check and install missing packages silently
install_if_missing <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new_packages)) {
    message("Installing missing packages: ", paste(new_packages, collapse = ", "))
    install.packages(new_packages, repos = "https://cloud.r-project.org")
  }
}

# List of required packages as specified
required_packages <- c(
  "shiny",
  "bslib",
  "shinydashboard",
  "tidyverse", # Includes dplyr, ggplot2, readr
  "plotly",
  "DT",
  "readxl",
  "janitor",
  "shinyjqui",
  "shinyjs",
  "htmlwidgets",
  "webshot2",
  "jsonlite",
  "markdown"
  ,
  "memoise",
  "testthat",
  "profvis",
  "httr"
)

# Install missing packages
install_if_missing(required_packages)

# Load all packages
invisible(lapply(required_packages, library, character.only = TRUE))

# Confirm packages loaded
message("All packages loaded successfully.")
