# viz_module.R
# Interactive Plotly visualization builder module

vizUI <- function(id) {
  ns <- NS(id)

  tagList(
    tags$style(HTML(sprintf("\n      .viz-shell-%1$s .viz-code {\n        font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, 'Liberation Mono', 'Courier New', monospace;\n      }\n      .viz-shell-%1$s .viz-type .btn {\n        text-align: left;\n      }\n      .viz-shell-%1$s .chart-card {\n        border: 1px solid #dbe3ec;\n        border-radius: 12px;\n        box-shadow: 0 4px 14px rgba(26, 44, 67, 0.05);\n      }\n      .viz-shell-%1$s .chart-toolbar {\n        display: flex;\n        gap: 0.4rem;\n        flex-wrap: wrap;\n      }\n    ", id))),

    div(
      class = sprintf("viz-shell-%s", id),
      card(
        card_header("Visualization Builder"),
        card_body(
          layout_columns(
            col_widths = c(5, 7),

            card(
              class = "chart-card",
              card_header("Builder"),
              card_body(
                fluidRow(
                  column(
                    6,
                    selectInput(
                      ns("template"), "Chart Template",
                      choices = c(
                        "Custom" = "custom",
                        "Distribution analysis" = "distribution",
                        "Trend over time" = "trend",
                        "Comparison" = "comparison",
                        "Correlation" = "correlation"
                      )
                    )
                  ),
                  column(
                    6,
                    selectInput(ns("grid_cols"), "Dashboard Grid Columns", choices = c("1" = 1, "2" = 2, "3" = 3), selected = 2)
                  )
                ),

                selectInput(
                  ns("chart_type"),
                  "Chart Type",
                  choices = c(
                    "Bar" = "bar",
                    "Line" = "line",
                    "Scatter" = "scatter",
                    "Box" = "box",
                    "Histogram" = "hist",
                    "Heatmap" = "heatmap",
                    "Pie/Donut" = "pie"
                  ),
                  selected = "bar"
                ),

                accordion(
                  accordion_panel(
                    "Data",
                    uiOutput(ns("map_x_ui")),
                    uiOutput(ns("map_y_ui")),
                    uiOutput(ns("map_color_ui")),
                    uiOutput(ns("map_size_ui")),
                    uiOutput(ns("map_facet_ui")),
                    uiOutput(ns("chart_specific_data_ui"))
                  ),
                  accordion_panel(
                    "Appearance",
                    selectInput(ns("palette"), "Color Palette", choices = c("Okabe-Ito" = "okabe", "Viridis" = "viridis", "Cividis" = "cividis", "Blue-Gray" = "bluegray")),
                    selectInput(ns("legend_pos"), "Legend Position", choices = c("Right" = "right", "Left" = "left", "Top" = "top", "Bottom" = "bottom", "Hidden" = "none")),
                    checkboxInput(ns("show_grid"), "Show Grid Lines", value = TRUE),
                    selectInput(ns("x_scale"), "X Axis Scale", choices = c("Linear" = "linear", "Log" = "log")),
                    selectInput(ns("y_scale"), "Y Axis Scale", choices = c("Linear" = "linear", "Log" = "log")),
                    sliderInput(ns("point_size"), "Point Size", min = 3, max = 18, value = 8),
                    sliderInput(ns("line_width"), "Line Width", min = 1, max = 8, value = 2)
                  ),
                  accordion_panel(
                    "Labels",
                    textInput(ns("chart_title"), "Chart Title", value = ""),
                    textInput(ns("x_label"), "X Label", value = ""),
                    textInput(ns("y_label"), "Y Label", value = "")
                  )
                ),

                br(),
                div(
                  class = "chart-toolbar",
                  actionButton(ns("add_chart"), "Apply", icon = icon("check"), class = "btn btn-primary btn-sm"),
                  actionButton(ns("reset_builder"), "Reset", icon = icon("rotate-left"), class = "btn btn-outline-secondary btn-sm")
                )
              )
            ),

            card(
              class = "chart-card",
              card_header("Live Preview"),
              card_body(plotly::plotlyOutput(ns("preview_plot"), height = "460px"))
            )
          )
        )
      ),

      card(
        card_header("Dashboard Charts"),
        card_body(uiOutput(ns("charts_grid_ui")))
      )
    )
  )
}

vizServer <- function(id, data_r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    chart_store <- reactiveValues(items = list(), next_id = 1)

    `%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && nzchar(as.character(a)[1])) a else b

    base_palette <- function(name, n = 8) {
      name <- name %||% "okabe"
      if (name == "okabe") {
        cols <- c("#0072B2", "#E69F00", "#009E73", "#D55E00", "#CC79A7", "#56B4E9", "#F0E442", "#999999")
      } else if (name == "viridis") {
        cols <- viridisLite::viridis(max(3, n))
      } else if (name == "cividis") {
        cols <- viridisLite::cividis(max(3, n))
      } else {
        cols <- c("#4C78A8", "#6B8BA4", "#8EA9BF", "#A8BED0", "#C2D3E0", "#3A5F87", "#567DA4", "#7C9BBC")
      }
      rep(cols, length.out = n)
    }

    valid_cols <- reactive({
      df <- data_r()
      if (is.null(df)) character(0) else names(df)
    })

    numeric_cols <- reactive({
      df <- data_r(); req(df)
      names(df)[vapply(df, is.numeric, logical(1))]
    })

    cat_cols <- reactive({
      df <- data_r(); req(df)
      names(df)[vapply(df, function(x) is.character(x) || is.factor(x), logical(1))]
    })

    date_cols <- reactive({
      df <- data_r(); req(df)
      names(df)[vapply(df, function(x) inherits(x, "Date") || inherits(x, "POSIXt"), logical(1))]
    })

    output$map_x_ui <- renderUI({
      req(valid_cols())
      selectInput(ns("map_x"), "X Axis", choices = c("(none)" = "", valid_cols()))
    })

    output$map_y_ui <- renderUI({
      req(valid_cols())
      choices <- if (input$chart_type %in% c("hist", "pie")) c("(none)" = "") else c("(none)" = "", valid_cols())
      selectInput(ns("map_y"), "Y Axis", choices = choices)
    })

    output$map_color_ui <- renderUI({
      req(valid_cols())
      selectInput(ns("map_color"), "Color", choices = c("(none)" = "", valid_cols()))
    })

    output$map_size_ui <- renderUI({
      req(numeric_cols())
      selectInput(ns("map_size"), "Size", choices = c("(none)" = "", numeric_cols()))
    })

    output$map_facet_ui <- renderUI({
      req(cat_cols())
      selectInput(ns("map_facet"), "Facet", choices = c("(none)" = "", cat_cols()))
    })

    output$chart_specific_data_ui <- renderUI({
      req(input$chart_type)

      if (input$chart_type == "bar") {
        tagList(
          selectInput(ns("bar_orientation"), "Orientation", choices = c("Vertical" = "v", "Horizontal" = "h")),
          selectInput(ns("bar_mode"), "Bar Mode", choices = c("Grouped" = "group", "Stacked" = "stack"))
        )
      } else if (input$chart_type == "scatter") {
        checkboxInput(ns("trend_line"), "Show Trend Line", value = FALSE)
      } else if (input$chart_type == "pie") {
        checkboxInput(ns("is_donut"), "Donut", value = TRUE)
      } else {
        NULL
      }
    })

    observeEvent(input$template, {
      req(data_r())
      df <- data_r()
      num <- names(df)[vapply(df, is.numeric, logical(1))]
      catc <- names(df)[vapply(df, function(x) is.character(x) || is.factor(x), logical(1))]
      dates <- names(df)[vapply(df, function(x) inherits(x, "Date") || inherits(x, "POSIXt"), logical(1))]

      if (input$template == "distribution") {
        if (length(num)) {
          updateSelectInput(session, "chart_type", selected = "hist")
          updateSelectInput(session, "map_x", selected = num[1])
          updateTextInput(session, "chart_title", value = paste("Distribution of", num[1]))
        }
      }
      if (input$template == "trend") {
        if (length(num) && length(dates)) {
          updateSelectInput(session, "chart_type", selected = "line")
          updateSelectInput(session, "map_x", selected = dates[1])
          updateSelectInput(session, "map_y", selected = num[1])
          updateTextInput(session, "chart_title", value = paste(num[1], "Over Time"))
        }
      }
      if (input$template == "comparison") {
        if (length(catc) && length(num)) {
          updateSelectInput(session, "chart_type", selected = "bar")
          updateSelectInput(session, "map_x", selected = catc[1])
          updateSelectInput(session, "map_y", selected = num[1])
          updateTextInput(session, "chart_title", value = paste("Comparison of", num[1], "by", catc[1]))
        }
      }
      if (input$template == "correlation") {
        if (length(num) >= 2) {
          updateSelectInput(session, "chart_type", selected = "scatter")
          updateSelectInput(session, "map_x", selected = num[1])
          updateSelectInput(session, "map_y", selected = num[2])
          updateCheckboxInput(session, "trend_line", value = TRUE)
          updateTextInput(session, "chart_title", value = paste("Correlation:", num[1], "vs", num[2]))
        }
      }
    })

    build_plot <- function(cfg, df) {
      req(df)

      x_col <- cfg$map_x %||% ""
      y_col <- cfg$map_y %||% ""
      color_col <- cfg$map_color %||% ""
      size_col <- cfg$map_size %||% ""
      facet_col <- cfg$map_facet %||% ""

      marker_sz <- cfg$point_size %||% 8
      line_w <- cfg$line_width %||% 2
      pal <- base_palette(cfg$palette, n = 12)

      make_single_plot <- function(dsub) {
        p <- plotly::plot_ly(dsub)

        chart_type <- tolower(as.character(cfg$chart_type %||% "bar"))

        if (chart_type == "bar") {
          if (!nzchar(x_col)) return(plotly::plot_ly() %>% plotly::layout(title = "Select X axis for bar chart"))

          if (!nzchar(y_col)) {
            ct <- dsub %>% dplyr::count(.data[[x_col]], name = "count")
            p <- plotly::plot_ly(ct, x = ~.data[[x_col]], y = ~count, type = "bar")
          } else {
            if (nzchar(color_col)) {
              p <- plotly::plot_ly(dsub, x = as.formula(paste0("~", x_col)), y = as.formula(paste0("~", y_col)), color = as.formula(paste0("~", color_col)), type = "bar", colors = pal)
            } else {
              p <- plotly::plot_ly(dsub, x = as.formula(paste0("~", x_col)), y = as.formula(paste0("~", y_col)), type = "bar", marker = list(color = pal[1]))
            }
          }

          if (cfg$bar_orientation == "h") p <- p %>% plotly::layout(barmode = cfg$bar_mode, xaxis = list(title = cfg$y_label %||% y_col), yaxis = list(title = cfg$x_label %||% x_col)) %>% plotly::style(orientation = "h")
          p <- p %>% plotly::layout(barmode = cfg$bar_mode)
        }

        if (chart_type == "line") {
          if (!(nzchar(x_col) && nzchar(y_col))) return(plotly::plot_ly() %>% plotly::layout(title = "Select X and Y for line chart"))
          if (nzchar(color_col)) {
            p <- plotly::plot_ly(dsub, x = as.formula(paste0("~", x_col)), y = as.formula(paste0("~", y_col)), color = as.formula(paste0("~", color_col)), type = "scatter", mode = "lines+markers", line = list(width = line_w), marker = list(size = marker_sz), colors = pal)
          } else {
            p <- plotly::plot_ly(dsub, x = as.formula(paste0("~", x_col)), y = as.formula(paste0("~", y_col)), type = "scatter", mode = "lines+markers", line = list(width = line_w, color = pal[1]), marker = list(size = marker_sz, color = pal[1]))
          }
        }

        if (chart_type == "scatter") {
          if (!(nzchar(x_col) && nzchar(y_col))) return(plotly::plot_ly() %>% plotly::layout(title = "Select X and Y for scatter plot"))

          marker_args <- list(size = marker_sz, opacity = 0.8)
          if (nzchar(size_col) && size_col %in% names(dsub)) marker_args$size <- scales::rescale(dsub[[size_col]], to = c(6, 24))

          if (nzchar(color_col)) {
            p <- plotly::plot_ly(dsub, x = as.formula(paste0("~", x_col)), y = as.formula(paste0("~", y_col)), color = as.formula(paste0("~", color_col)), type = "scatter", mode = "markers", marker = marker_args, colors = pal)
          } else {
            marker_args$color <- pal[1]
            p <- plotly::plot_ly(dsub, x = as.formula(paste0("~", x_col)), y = as.formula(paste0("~", y_col)), type = "scatter", mode = "markers", marker = marker_args)
          }

          if (isTRUE(cfg$trend_line) && is.numeric(dsub[[x_col]]) && is.numeric(dsub[[y_col]])) {
            fit <- stats::lm(stats::as.formula(paste0(y_col, " ~ ", x_col)), data = dsub)
            pred <- dsub %>% dplyr::arrange(.data[[x_col]])
            pred$.trend <- stats::predict(fit, newdata = pred)
            p <- p %>% plotly::add_lines(data = pred, x = as.formula(paste0("~", x_col)), y = ~.trend, name = "Trend", line = list(color = "#444", width = line_w, dash = "dot"), inherit = FALSE)
          }
        }

        if (chart_type == "box") {
          if (!nzchar(y_col)) return(plotly::plot_ly() %>% plotly::layout(title = "Select Y for box plot"))
          if (nzchar(x_col)) {
            p <- plotly::plot_ly(dsub, x = as.formula(paste0("~", x_col)), y = as.formula(paste0("~", y_col)), type = "box", color = if (nzchar(color_col)) as.formula(paste0("~", color_col)) else NULL, colors = pal)
          } else {
            p <- plotly::plot_ly(dsub, y = as.formula(paste0("~", y_col)), type = "box", marker = list(color = pal[1]))
          }
        }

        if (chart_type == "hist") {
          if (!nzchar(x_col)) return(plotly::plot_ly() %>% plotly::layout(title = "Select X for histogram"))
          if (nzchar(color_col)) {
            p <- plotly::plot_ly(dsub, x = as.formula(paste0("~", x_col)), color = as.formula(paste0("~", color_col)), type = "histogram", colors = pal)
          } else {
            p <- plotly::plot_ly(dsub, x = as.formula(paste0("~", x_col)), type = "histogram", marker = list(color = pal[1]))
          }
        }

        if (chart_type == "heatmap") {
          if (!(nzchar(x_col) && nzchar(y_col))) return(plotly::plot_ly() %>% plotly::layout(title = "Select X and Y for heatmap"))
          agg <- dsub %>% dplyr::count(.data[[x_col]], .data[[y_col]], name = "n")
          p <- plotly::plot_ly(agg, x = as.formula(paste0("~", x_col)), y = as.formula(paste0("~", y_col)), z = ~n, type = "heatmap", colors = colorRamp(c("#edf3fb", "#5a88b5", "#20476b")))
        }

        if (chart_type == "pie") {
          if (!nzchar(x_col)) return(plotly::plot_ly() %>% plotly::layout(title = "Select category column for pie"))

          pie_df <- if (nzchar(y_col) && y_col %in% names(dsub) && is.numeric(dsub[[y_col]])) {
            dsub %>% dplyr::group_by(.data[[x_col]]) %>% dplyr::summarise(value = sum(.data[[y_col]], na.rm = TRUE), .groups = "drop")
          } else {
            dsub %>% dplyr::count(.data[[x_col]], name = "value")
          }

          p <- plotly::plot_ly(
            pie_df,
            labels = as.formula(paste0("~", x_col)),
            values = ~value,
            type = "pie",
            hole = if (isTRUE(cfg$is_donut)) 0.45 else 0,
            marker = list(colors = base_palette(cfg$palette, nrow(pie_df)))
          )
        }

        p
      }

      if (nzchar(facet_col) && facet_col %in% names(df) && tolower(as.character(cfg$chart_type %||% "")) != "pie") {
        levs <- unique(df[[facet_col]])
        levs <- levs[!is.na(levs)]
        levs <- head(levs, 8)
        subplots <- lapply(levs, function(lv) {
          dsub <- df[df[[facet_col]] == lv, , drop = FALSE]
          make_single_plot(dsub) %>% plotly::layout(title = as.character(lv), showlegend = FALSE)
        })
        p <- plotly::subplot(subplots, nrows = ceiling(length(subplots) / 2), shareX = FALSE, shareY = FALSE, titleX = TRUE, titleY = TRUE)
      } else {
        p <- make_single_plot(df)
      }

      x_title <- cfg$x_label %||% x_col
      y_title <- cfg$y_label %||% y_col

      p <- p %>% plotly::layout(
        title = cfg$chart_title %||% "",
        legend = list(orientation = if (cfg$legend_pos %in% c("top", "bottom")) "h" else "v"),
        showlegend = cfg$legend_pos != "none",
        xaxis = list(
          title = x_title,
          type = if (cfg$x_scale == "log") "log" else "linear",
          showgrid = isTRUE(cfg$show_grid),
          gridcolor = "#eef2f6"
        ),
        yaxis = list(
          title = y_title,
          type = if (cfg$y_scale == "log") "log" else "linear",
          showgrid = isTRUE(cfg$show_grid),
          gridcolor = "#eef2f6"
        ),
        plot_bgcolor = "#ffffff",
        paper_bgcolor = "#ffffff",
        margin = list(l = 60, r = 24, t = 58, b = 60)
      )

      if (cfg$legend_pos == "left") p <- p %>% plotly::layout(legend = list(x = -0.18, y = 1))
      if (cfg$legend_pos == "right") p <- p %>% plotly::layout(legend = list(x = 1.02, y = 1))
      if (cfg$legend_pos == "top") p <- p %>% plotly::layout(legend = list(x = 0, y = 1.15))
      if (cfg$legend_pos == "bottom") p <- p %>% plotly::layout(legend = list(x = 0, y = -0.2))

      p %>% plotly::config(
        responsive = TRUE,
        displaylogo = FALSE,
        toImageButtonOptions = list(format = "png", filename = "lume_chart", scale = 2)
      )
    }

    current_config <- reactive({
      list(
        chart_type = input$chart_type,
        map_x = input$map_x,
        map_y = input$map_y,
        map_color = input$map_color,
        map_size = input$map_size,
        map_facet = input$map_facet,
        bar_orientation = input$bar_orientation %||% "v",
        bar_mode = input$bar_mode %||% "group",
        trend_line = isTRUE(input$trend_line),
        is_donut = isTRUE(input$is_donut),
        palette = input$palette,
        legend_pos = input$legend_pos,
        show_grid = isTRUE(input$show_grid),
        x_scale = input$x_scale,
        y_scale = input$y_scale,
        point_size = input$point_size,
        line_width = input$line_width,
        chart_title = input$chart_title,
        x_label = input$x_label,
        y_label = input$y_label
      )
    })

    preview_config <- reactive(current_config()) %>% bindEvent(
      input$chart_type, input$map_x, input$map_y, input$map_color, input$map_size, input$map_facet,
      input$bar_orientation, input$bar_mode, input$trend_line, input$is_donut,
      input$palette, input$legend_pos, input$show_grid, input$x_scale, input$y_scale,
      input$point_size, input$line_width, input$chart_title, input$x_label, input$y_label,
      ignoreInit = FALSE
    )

    output$preview_plot <- plotly::renderPlotly({
      req(data_r())
      df <- data_r()
      shiny::validate(shiny::need(nrow(df) > 1 && ncol(df) > 0, friendly_error_message("insufficient_data")))
      build_plot(preview_config(), df)
    })

    observeEvent(input$add_chart, {
      req(data_r())
      cid <- chart_store$next_id
      chart_store$next_id <- cid + 1
      cfg <- current_config()
      title <- cfg$chart_title %||% paste0("Chart ", cid)
      chart_store$items[[as.character(cid)]] <- list(id = cid, title = title, cfg = cfg)
    })

    observeEvent(input$reset_builder, {
      updateSelectInput(session, "template", selected = "custom")
      updateSelectInput(session, "chart_type", selected = "bar")
      updateTextInput(session, "chart_title", value = "")
      updateTextInput(session, "x_label", value = "")
      updateTextInput(session, "y_label", value = "")
      updateSelectInput(session, "legend_pos", selected = "right")
      updateSelectInput(session, "palette", selected = "okabe")
      updateCheckboxInput(session, "show_grid", value = TRUE)
      updateSelectInput(session, "x_scale", selected = "linear")
      updateSelectInput(session, "y_scale", selected = "linear")
      updateSliderInput(session, "point_size", value = 8)
      updateSliderInput(session, "line_width", value = 2)
    })

    output$charts_grid_ui <- renderUI({
      items <- reactiveValuesToList(chart_store)$items
      if (!length(items)) return(div(class = "text-muted", "No charts added yet. Build a chart and click Apply."))

      cols <- as.integer(input$grid_cols %||% 2)
      width <- if (cols == 1) 12 else if (cols == 2) 6 else 4

      tagList(
        fluidRow(
          lapply(items, function(ch) {
            column(
              width = width,
              card(
                class = "chart-card mb-3",
                card_header(
                  div(class = "d-flex justify-content-between align-items-center",
                    tags$span(class = "viz-code", ch$title),
                    div(
                      actionButton(ns(paste0("dup_", ch$id)), NULL, icon = icon("clone"), class = "btn btn-outline-secondary btn-sm", title = "Duplicate chart", `aria-label` = "Duplicate chart"),
                      actionButton(ns(paste0("del_", ch$id)), NULL, icon = icon("trash"), class = "btn btn-outline-danger btn-sm ms-1", title = "Delete chart", `aria-label` = "Delete chart", onclick = "return confirm('Delete this chart card?');")
                    )
                  )
                ),
                card_body(plotly::plotlyOutput(ns(paste0("chart_", ch$id)), height = "320px"))
              )
            )
          })
        )
      )
    })

    observe({
      items <- reactiveValuesToList(chart_store)$items
      lapply(items, function(ch) {
        local({
          chart <- ch

          output[[paste0("chart_", chart$id)]] <- plotly::renderPlotly({
            req(data_r())
            build_plot(chart$cfg, data_r())
          })

          observeEvent(input[[paste0("del_", chart$id)]], {
            chart_store$items[[as.character(chart$id)]] <- NULL
          }, ignoreInit = TRUE)

          observeEvent(input[[paste0("dup_", chart$id)]], {
            nid <- chart_store$next_id
            chart_store$next_id <- nid + 1
            dup <- chart
            dup$id <- nid
            dup$title <- paste0(chart$title, " (copy)")
            chart_store$items[[as.character(nid)]] <- dup
          }, ignoreInit = TRUE)
        })
      })
    })

    return(list(
      charts = reactive(reactiveValuesToList(chart_store)$items),
      build_plot = build_plot,
      current_config = current_config
    ))
  })
}

# Backward-compatible aliases
viz_ui <- vizUI
viz_server <- vizServer
