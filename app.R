# app.R
# Main entry point for the Lume Data Analytics application

# 1. Load Dependencies
source("packages.R")
source("config.R")

# 2. Source Modules & Utils
# Automatically source all files in modules/ and utils/
invisible(lapply(list.files("modules", full.names = TRUE), source))
invisible(lapply(list.files("utils", full.names = TRUE), source))

# 3. User Interface
ui <- page_navbar(
  title = APP_TITLE,
  theme = app_theme,
  fillable = FALSE,
  shinyjs::useShinyjs(),

  # Inject Custom CSS
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),

  nav_panel(
    title = "Overview",
    div(
      insightsUI("insights"),
      accordion(
        accordion_panel("Data Source", uploadUI("uploader"))
      )
    )
  ),

  nav_panel(
    title = "Workspace",
    layout_column_wrap(
      width = 1/1,
      accordion(
        accordion_panel("Advanced Transformations", transformUI("transformer")),
        accordion_panel("Custom Visualization Builder", viz_ui("visualizer"))
      )
    )
  ),

  nav_panel(
    title = "Dashboard Builder",
    dashboardUI("dashboard_builder")
  ),

  nav_panel(
    title = "About",
    layout_column_wrap(
      width = 1/2,
      card(
        card_header("Project Info"),
        p("Lume is a modern data visualization framework built with R and Shiny."),
        p("Version: ", APP_VERSION),
        hr(),
        h6("Keyboard Shortcuts"),
        tags$ul(
          tags$li(tags$code("Alt + Shift + E"), " Open export dialog (Dashboard Builder)"),
          tags$li(tags$code("Alt + Shift + S"), " Save dashboard config"),
          tags$li(tags$code("Alt + Shift + A"), " Add chart card")
        )
      )
    )
  ),

  nav_panel(
    title = "Help / FAQ",
    layout_column_wrap(
      width = 1/1,
      card(
        card_header("Frequently Asked Questions"),
        tags$dl(
          tags$dt("What files can I upload?"),
          tags$dd("CSV, TSV, XLSX, and XLS up to 50 MB."),
          tags$dt("Why does my data show as sampled?"),
          tags$dd("Datasets over 100,000 rows use a performance sample for interactive visualizations."),
          tags$dt("How can I export my work?"),
          tags$dd("Use Dashboard Builder > Export for HTML, PDF, CSV, PNG, and reproducible R script."),
          tags$dt("How do I troubleshoot errors?"),
          tags$dd("User-facing messages are simplified; technical details are logged in logs/app.log.")
        )
      )
    )
  ),

  nav_spacer(),

  nav_item(
    tags$a(href = "https://github.com", icon("github"), "Source")
  )
)

# 4. Server Logic
server <- function(input, output, session) {
  log_event("INFO", "app", "Session started")

  observeEvent(TRUE, {
    showModal(modalDialog(
      title = "Welcome to Lume",
      p("Start by reviewing the sample data already loaded in the Upload panel, then add transformations and charts."),
      tags$ul(
        tags$li("Upload your own dataset or use the built-in sample."),
        tags$li("Build transformations step by step."),
        tags$li("Create visuals, then assemble them in Dashboard Builder.")
      ),
      footer = modalButton("Get started"),
      easyClose = TRUE
    ))
  }, once = TRUE, ignoreInit = FALSE)

  # Initialize Modules
  raw_data <- uploadServer("uploader")
  transformed_data <- transformServer("transformer", raw_data)
  insightsServer("insights", transformed_data)
  viz_state <- viz_server("visualizer", transformed_data)
  dashboardServer("dashboard_builder", transformed_data, viz_state = viz_state)
}

# 5. Launch Application
shinyApp(ui = ui, server = server)
