# dashboard_module.R
# Dashboard layout builder, KPI cards, save/load, and export workflows

dashboardUI <- function(id) {
  ns <- NS(id)

  tagList(
    tags$script(HTML("
      document.addEventListener('keydown', function(e) {
        if (!(e.altKey && e.shiftKey)) return;
        var k = (e.key || '').toLowerCase();
        var clickIf = function(id) {
          var el = document.getElementById(id);
          if (el) el.click();
        };
        if (k === 'e') clickIf(document.querySelector('[id$=\"btn_export\"]')?.id || '');
        if (k === 's') clickIf(document.querySelector('[id$=\"btn_save\"]')?.id || '');
        if (k === 'a') clickIf(document.querySelector('[id$=\"add_chart\"]')?.id || '');
      });
    ")),
    tags$style(HTML(sprintf("\n      .dash-shell-%1$s .dash-topbar {\n        display: flex;\n        gap: 0.5rem;\n        flex-wrap: wrap;\n        margin-bottom: 0.8rem;\n      }\n      .dash-shell-%1$s .dash-canvas-card {\n        border: 1px solid #dbe4ec;\n        border-radius: 12px;\n        box-shadow: 0 4px 14px rgba(26, 44, 67, 0.05);\n      }\n      .dash-shell-%1$s .metric-value {\n        font-size: 1.8rem;\n        font-weight: 700;\n        line-height: 1.1;\n      }\n      .dash-shell-%1$s .mono {\n        font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, 'Liberation Mono', 'Courier New', monospace;\n      }\n      .dash-shell-%1$s .placeholder-zone {\n        border: 2px dashed #c8d6e5;\n        border-radius: 12px;\n        padding: 1.2rem;\n        text-align: center;\n        color: #6c7f92;\n        background: #f7fafd;\n      }\n    ", id))),

    div(
      class = sprintf("dash-shell-%s", id),

      div(
        class = "dash-topbar",
        selectInput(ns("layout_template"), "Layout", choices = c("Executive", "Detailed Analysis", "Comparison"), selected = "Executive", width = "220px"),
        selectInput(ns("grid_cols"), "Columns", choices = c("1" = 1, "2" = 2, "3" = 3), selected = 2, width = "120px"),
        actionButton(ns("add_chart"), "Add Chart", icon = icon("chart-line"), class = "btn btn-primary btn-sm"),
        actionButton(ns("add_metric"), "Add Metric", icon = icon("gauge-high"), class = "btn btn-primary btn-sm"),
        actionButton(ns("add_text"), "Add Text", icon = icon("align-left"), class = "btn btn-outline-primary btn-sm"),
        actionButton(ns("btn_save"), "Save", icon = icon("floppy-disk"), class = "btn btn-outline-secondary btn-sm"),
        actionButton(ns("btn_load"), "Load", icon = icon("folder-open"), class = "btn btn-outline-secondary btn-sm"),
        actionButton(ns("btn_ai_note"), "AI Note", icon = icon("wand-magic-sparkles"), class = "btn btn-outline-secondary btn-sm"),
        actionButton(ns("btn_export"), "Export", icon = icon("download"), class = "btn btn-outline-secondary btn-sm"),
        actionButton(ns("btn_share"), "Share", icon = icon("share-nodes"), class = "btn btn-outline-secondary btn-sm")
      ),

      card(
        class = "dash-canvas-card",
        card_header("Dashboard Canvas"),
        card_body(
          uiOutput(ns("order_ui")),
          uiOutput(ns("canvas_ui"))
        )
      ),

      downloadButton(ns("download_csv"), label = NULL, style = "display:none;"),
      downloadButton(ns("download_html"), label = NULL, style = "display:none;"),
      downloadButton(ns("download_pdf"), label = NULL, style = "display:none;"),
      downloadButton(ns("download_png"), label = NULL, style = "display:none;"),
      downloadButton(ns("download_r"), label = NULL, style = "display:none;"),
      downloadButton(ns("download_config"), label = NULL, style = "display:none;")
    )
  )
}

dashboardServer <- function(id, data_r, viz_state = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(
      cards = list(),
      next_id = 1,
      metric_defs = list(),
      text_defs = list(),
      chart_refs = list(),
      pending_png_chart = NULL
    )

    `%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

    add_card <- function(type, title, payload = list()) {
      cid <- paste0(type, "_", rv$next_id)
      rv$next_id <- rv$next_id + 1
      rv$cards[[cid]] <- list(id = cid, type = type, title = title, payload = payload)
      cid
    }

    layout_preset <- reactive({
      if (identical(input$layout_template, "Executive")) return(1)
      if (identical(input$layout_template, "Detailed Analysis")) return(2)
      3
    })

    observeEvent(layout_preset(), {
      updateSelectInput(session, "grid_cols", selected = as.character(layout_preset()))
    }, ignoreInit = FALSE)

    chart_choices <- reactive({
      if (is.null(viz_state) || is.null(viz_state$charts)) return(character(0))
      ch <- viz_state$charts()
      if (!length(ch)) return(character(0))
      labels <- vapply(ch, function(x) paste0("#", x$id, " ", x$title), character(1))
      ids <- vapply(ch, function(x) as.character(x$id), character(1))
      stats::setNames(ids, labels)
    })

    observeEvent(input$add_chart, {
      choices <- chart_choices()
      if (!length(choices)) {
        showNotification("No charts found in visualization builder. Create charts first.", type = "warning")
        return()
      }
      showModal(modalDialog(
        title = "Add Chart Card",
        selectInput(ns("new_chart_ref"), "Select Chart", choices = choices),
        textInput(ns("new_chart_title"), "Card Title", value = "Chart"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_add_chart"), "Add", class = "btn btn-primary")
        ),
        easyClose = TRUE
      ))
    })

    observeEvent(input$confirm_add_chart, {
      req(input$new_chart_ref)
      ref_id <- as.character(input$new_chart_ref)
      cid <- add_card("chart", input$new_chart_title %||% "Chart", list(ref = ref_id))
      rv$chart_refs[[cid]] <- ref_id
      removeModal()
    })

    observeEvent(input$add_text, {
      showModal(modalDialog(
        title = "Add Annotation Card",
        textInput(ns("text_title"), "Title", value = "Notes"),
        textAreaInput(ns("text_markdown"), "Markdown", value = "### Insight\nAdd notes here", rows = 7),
        footer = tagList(modalButton("Cancel"), actionButton(ns("confirm_add_text"), "Add", class = "btn btn-primary")),
        easyClose = TRUE,
        size = "l"
      ))
    })

    observeEvent(input$confirm_add_text, {
      req(input$text_markdown)
      cid <- add_card("text", input$text_title %||% "Notes", list(markdown = input$text_markdown))
      rv$text_defs[[cid]] <- list(markdown = input$text_markdown)
      removeModal()
    })

    observeEvent(input$add_metric, {
      req(data_r())
      df <- data_r()
      num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
      showModal(modalDialog(
        title = "Add KPI Card",
        textInput(ns("metric_title"), "Title", value = "KPI"),
        selectInput(ns("metric_col"), "Column", choices = num_cols),
        selectInput(ns("metric_fn"), "Aggregation", choices = c("sum", "average", "count", "min", "max"), selected = "sum"),
        numericInput(ns("metric_target"), "Target (optional)", value = NA),
        selectInput(ns("metric_cmp"), "Comparison", choices = c("none", "vs_target", "vs_previous"), selected = "none"),
        numericInput(ns("metric_green"), "Green Threshold", value = NA),
        numericInput(ns("metric_red"), "Red Threshold", value = NA),
        checkboxInput(ns("metric_spark"), "Show Sparkline", value = TRUE),
        footer = tagList(modalButton("Cancel"), actionButton(ns("confirm_add_metric"), "Add", class = "btn btn-primary")),
        easyClose = TRUE
      ))
    })

    observeEvent(input$confirm_add_metric, {
      req(input$metric_col, input$metric_fn)
      def <- list(
        col = input$metric_col,
        fn = input$metric_fn,
        target = input$metric_target,
        cmp = input$metric_cmp,
        green = input$metric_green,
        red = input$metric_red,
        spark = isTRUE(input$metric_spark)
      )
      cid <- add_card("metric", input$metric_title %||% "KPI", def)
      rv$metric_defs[[cid]] <- def
      removeModal()
    })

    output$order_ui <- renderUI({
      items <- names(rv$cards)
      if (!length(items)) return(div(class = "placeholder-zone empty-state", icon("grip-vertical"), br(), tags$strong("No cards yet"), br(), "Use Add Chart, Add Metric, or Add Text to get started."))
      shinyjqui::orderInput(ns("card_order"), "Rearrange Cards (Drag)", items = stats::setNames(items, items), width = "100%")
    })

    ordered_cards <- reactive({
      cards <- rv$cards
      ord <- input$card_order
      if (is.null(ord) || !length(ord)) return(cards)
      out <- cards[ord]
      out[!vapply(out, is.null, logical(1))]
    })

    metric_value <- function(df, def) {
      x <- df[[def$col]]
      if (def$fn == "sum") return(sum(x, na.rm = TRUE))
      if (def$fn == "average") return(mean(x, na.rm = TRUE))
      if (def$fn == "count") return(sum(!is.na(x)))
      if (def$fn == "min") return(min(x, na.rm = TRUE))
      if (def$fn == "max") return(max(x, na.rm = TRUE))
      NA_real_
    }

    metric_color <- function(value, def) {
      g <- suppressWarnings(as.numeric(def$green))
      r <- suppressWarnings(as.numeric(def$red))
      if (is.finite(g) && value >= g) return("#1e7e34")
      if (is.finite(r) && value <= r) return("#b02a37")
      "#29435c"
    }

    render_metric_plot <- function(vec) {
      d <- data.frame(i = seq_along(vec), v = as.numeric(vec))
      plotly::plot_ly(d, x = ~i, y = ~v, type = "scatter", mode = "lines", line = list(color = "#6b8fb3", width = 1.5)) %>%
        plotly::layout(margin = list(l = 5, r = 5, t = 5, b = 5), xaxis = list(visible = FALSE), yaxis = list(visible = FALSE), paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)") %>%
        plotly::config(displayModeBar = FALSE, staticPlot = TRUE)
    }

    output$canvas_ui <- renderUI({
      cards <- ordered_cards()
      if (!length(cards)) return(NULL)

      cols <- as.integer(input$grid_cols %||% 2)
      width <- if (cols == 1) 12 else if (cols == 2) 6 else 4

      fluidRow(
        lapply(cards, function(cd) {
          column(
            width = width,
            card(
              class = "dash-canvas-card mb-3",
              card_header(
                div(class = "d-flex justify-content-between align-items-center",
                  tags$span(cd$title),
                  div(
                    actionButton(ns(paste0("edit_", cd$id)), NULL, icon = icon("pen"), class = "btn btn-outline-secondary btn-sm", title = "Edit card", `aria-label` = "Edit card"),
                    actionButton(ns(paste0("remove_", cd$id)), NULL, icon = icon("trash"), class = "btn btn-outline-danger btn-sm ms-1", title = "Remove card", `aria-label` = "Remove card", onclick = "return confirm('Remove this dashboard card?');"),
                    if (cd$type == "chart") actionButton(ns(paste0("png_", cd$id)), NULL, icon = icon("image"), class = "btn btn-outline-primary btn-sm ms-1", title = "Export chart as PNG", `aria-label` = "Export chart as PNG")
                  )
                )
              ),
              card_body(
                if (cd$type == "chart") plotly::plotlyOutput(ns(paste0("plot_", cd$id)), height = "300px"),
                if (cd$type == "text") uiOutput(ns(paste0("text_", cd$id))),
                if (cd$type == "metric") uiOutput(ns(paste0("metric_", cd$id))),
                if (cd$type == "metric" && isTRUE(cd$payload$spark)) plotly::plotlyOutput(ns(paste0("spark_", cd$id)), height = "70px")
              )
            )
          )
        })
      )
    })

    observe({
      cards <- ordered_cards()
      if (!length(cards)) return()

      lapply(cards, function(cd) {
        local({
          card <- cd

          if (card$type == "chart") {
            output[[paste0("plot_", card$id)]] <- plotly::renderPlotly({
              req(viz_state, data_r())
              charts <- viz_state$charts()
              ref <- card$payload$ref
              idx <- which(vapply(charts, function(x) as.character(x$id) == as.character(ref), logical(1)))
              shiny::validate(shiny::need(length(idx) == 1, "Referenced chart not found."))
              viz_state$build_plot(charts[[idx]]$cfg, data_r())
            })

            observeEvent(input[[paste0("png_", card$id)]], {
              rv$pending_png_chart <- card$id
              shinyjs::click(ns("download_png"))
            }, ignoreInit = TRUE)
          }

          if (card$type == "text") {
            output[[paste0("text_", card$id)]] <- renderUI({
              md <- card$payload$markdown %||% ""
              ai_markdown_html(md)
            })
          }

          if (card$type == "metric") {
            output[[paste0("metric_", card$id)]] <- renderUI({
              req(data_r())
              df <- data_r()
              def <- card$payload
              val <- metric_value(df, def)

              cmp_txt <- ""
              if (identical(def$cmp, "vs_target") && is.finite(as.numeric(def$target))) {
                delta <- val - as.numeric(def$target)
                cmp_txt <- paste0("vs target: ", scales::number(delta, accuracy = 0.01))
              }
              if (identical(def$cmp, "vs_previous")) {
                prev <- if (nrow(df) >= 2) metric_value(df[-nrow(df), , drop = FALSE], def) else NA_real_
                if (is.finite(prev)) cmp_txt <- paste0("vs previous: ", scales::percent((val - prev) / ifelse(prev == 0, NA, prev), accuracy = 0.1))
              }

              div(
                div(class = "metric-value", style = paste0("color:", metric_color(val, def), ";"), scales::number(val, accuracy = 0.01)),
                if (nzchar(cmp_txt)) div(class = "text-muted small", cmp_txt),
                div(class = "mono small text-muted", paste(def$fn, "of", def$col))
              )
            })

            output[[paste0("spark_", card$id)]] <- plotly::renderPlotly({
              req(data_r())
              vec <- data_r()[[card$payload$col]]
              vec <- vec[is.finite(as.numeric(vec))]
              shiny::validate(shiny::need(length(vec) > 1, ""))
              render_metric_plot(vec)
            })
          }

          observeEvent(input[[paste0("remove_", card$id)]], {
            rv$cards[[card$id]] <- NULL
            rv$metric_defs[[card$id]] <- NULL
            rv$text_defs[[card$id]] <- NULL
            rv$chart_refs[[card$id]] <- NULL
          }, ignoreInit = TRUE)

          observeEvent(input[[paste0("edit_", card$id)]], {
            if (card$type == "text") {
              showModal(modalDialog(
                title = paste("Edit", card$title),
                textAreaInput(ns("edit_text_md"), "Markdown", value = card$payload$markdown %||% "", rows = 8),
                footer = tagList(modalButton("Cancel"), actionButton(ns(paste0("save_edit_", card$id)), "Save", class = "btn btn-primary")),
                easyClose = TRUE,
                size = "l"
              ))

              observeEvent(input[[paste0("save_edit_", card$id)]], {
                rv$cards[[card$id]]$payload$markdown <- input$edit_text_md
                removeModal()
              }, ignoreInit = TRUE, once = TRUE)
            }
          }, ignoreInit = TRUE)
        })
      })
    })

    current_config <- reactive({
      list(
        version = 1,
        grid_cols = as.integer(input$grid_cols %||% 2),
        layout_template = input$layout_template,
        card_order = input$card_order %||% names(rv$cards),
        cards = rv$cards
      )
    })

    output$download_config <- downloadHandler(
      filename = function() paste0("dashboard_config_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".json"),
      content = function(file) {
        jsonlite::write_json(current_config(), file, pretty = TRUE, auto_unbox = TRUE)
      }
    )

    observeEvent(input$btn_save, {
      shinyjs::click(ns("download_config"))
    })

    observeEvent(input$btn_load, {
      showModal(modalDialog(
        title = "Load Dashboard Config",
        fileInput(ns("config_file"), "Upload JSON", accept = c("application/json", ".json")),
        footer = tagList(modalButton("Cancel"), actionButton(ns("confirm_load"), "Load", class = "btn btn-primary")),
        easyClose = TRUE
      ))
    })

    observeEvent(input$btn_ai_note, {
      req(data_r())
      note <- ai_dashboard_note(data_r(), ordered_cards())
      showModal(modalDialog(
        title = "AI Dashboard Note",
        textAreaInput(ns("ai_note_text"), "Generated Markdown", value = note, rows = 8),
        tags$hr(),
        tags$div(class = "text-muted small", "Rendered Preview"),
        uiOutput(ns("ai_note_preview")),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("ai_note_add"), "Add as Text Card", class = "btn btn-primary")
        ),
        easyClose = TRUE
      ))
    })

    output$ai_note_preview <- renderUI({
      req(input$ai_note_text)
      ai_markdown_html(input$ai_note_text)
    })

    observeEvent(input$ai_note_add, {
      req(input$ai_note_text)
      cid <- add_card("text", "AI Note", list(markdown = input$ai_note_text))
      rv$text_defs[[cid]] <- list(markdown = input$ai_note_text)
      removeModal()
      showNotification("AI note added to dashboard.", type = "message")
    })

    observeEvent(input$confirm_load, {
      req(input$config_file)
      cfg <- jsonlite::fromJSON(input$config_file$datapath, simplifyVector = FALSE)
      req(cfg$cards)

      rv$cards <- cfg$cards
      ids <- as.integer(gsub("[^0-9]", "", names(cfg$cards)))
      rv$next_id <- if (length(ids)) max(ids, na.rm = TRUE) + 1 else 1

      updateSelectInput(session, "grid_cols", selected = as.character(cfg$grid_cols %||% 2))
      updateSelectInput(session, "layout_template", selected = cfg$layout_template %||% "Executive")

      output$order_ui <- renderUI({
        items <- names(rv$cards)
        if (!length(items)) return(div(class = "placeholder-zone empty-state", icon("circle-info"), br(), "No cards loaded in this configuration."))
        shinyjqui::orderInput(ns("card_order"), "Rearrange Cards (Drag)", items = stats::setNames(items, items), width = "100%")
      })

      removeModal()
      showNotification("Dashboard configuration loaded.", type = "message")
    })

    output$download_csv <- downloadHandler(
      filename = function() paste0("dashboard_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
      content = function(file) {
        req(data_r())
        utils::write.csv(data_r(), file, row.names = FALSE)
      }
    )

    html_preview_tag <- reactive({
      cards <- ordered_cards()
      req(length(cards) > 0)
      req(data_r())
      df <- data_r()

      tags <- lapply(cards, function(cd) {
        if (cd$type == "chart") {
          charts <- viz_state$charts()
          ref <- cd$payload$ref
          idx <- which(vapply(charts, function(x) as.character(x$id) == as.character(ref), logical(1)))
          if (!length(idx)) return(htmltools::div())
          p <- viz_state$build_plot(charts[[idx]]$cfg, df)
          htmltools::div(style = "margin-bottom:16px;border:1px solid #dbe4ec;padding:12px;border-radius:8px;", htmltools::h4(cd$title), htmlwidgets::as.tags(p))
        } else if (cd$type == "text") {
          htmltools::div(style = "margin-bottom:16px;border:1px solid #dbe4ec;padding:12px;border-radius:8px;", htmltools::h4(cd$title), ai_markdown_html(cd$payload$markdown %||% ""))
        } else {
          val <- metric_value(df, cd$payload)
          htmltools::div(style = "margin-bottom:16px;border:1px solid #dbe4ec;padding:12px;border-radius:8px;", htmltools::h4(cd$title), htmltools::div(style = paste0("font-size:28px;font-weight:700;color:", metric_color(val, cd$payload), ";"), scales::number(val, accuracy = 0.01)))
        }
      })

      htmltools::browsable(htmltools::tagList(tags))
    })

    output$download_html <- downloadHandler(
      filename = function() paste0("dashboard_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html"),
      content = function(file) {
        htmltools::save_html(html_preview_tag(), file = file)
      }
    )

    output$download_pdf <- downloadHandler(
      filename = function() paste0("dashboard_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf"),
      content = function(file) {
        tmp <- tempfile(fileext = ".html")
        htmltools::save_html(html_preview_tag(), file = tmp)
        webshot2::webshot(tmp, file = file, vwidth = 1600, vheight = 2200)
      }
    )

    output$download_png <- downloadHandler(
      filename = function() paste0("chart_", rv$pending_png_chart %||% "export", ".png"),
      content = function(file) {
        req(rv$pending_png_chart)
        cid <- rv$pending_png_chart
        card <- rv$cards[[cid]]
        req(card, card$type == "chart", viz_state)

        charts <- viz_state$charts()
        ref <- card$payload$ref
        idx <- which(vapply(charts, function(x) as.character(x$id) == as.character(ref), logical(1)))
        req(length(idx) == 1)

        p <- viz_state$build_plot(charts[[idx]]$cfg, data_r())
        tmp <- tempfile(fileext = ".html")
        htmlwidgets::saveWidget(p, file = tmp, selfcontained = TRUE)
        webshot2::webshot(tmp, file = file, vwidth = 1400, vheight = 900)
      }
    )

    output$download_r <- downloadHandler(
      filename = function() paste0("repro_analysis_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".R"),
      content = function(file) {
        cards <- ordered_cards()
        lines <- c(
          "# Reproducible analysis scaffold generated from dashboard configuration",
          "library(dplyr)",
          "library(plotly)",
          "library(jsonlite)",
          "",
          "# Load transformed data",
          "df <- read.csv('your_transformed_data.csv')",
          "",
          "# Dashboard card definitions"
        )

        for (cd in cards) {
          lines <- c(lines, paste0("# Card: ", cd$id, " (", cd$type, ") - ", cd$title))
          if (cd$type == "metric") {
            lines <- c(lines, paste0("# KPI: ", cd$payload$fn, " on ", cd$payload$col))
          }
          if (cd$type == "chart") {
            lines <- c(lines, paste0("# Chart ref: ", cd$payload$ref))
          }
          if (cd$type == "text") {
            lines <- c(lines, "# Annotation markdown included in dashboard")
          }
          lines <- c(lines, "")
        }

        writeLines(lines, con = file)
      }
    )

    observeEvent(input$btn_export, {
      showModal(modalDialog(
        title = "Export Dashboard",
        p("Choose an export format:"),
        div(class = "d-grid gap-2",
          actionButton(ns("exp_html"), "Export HTML (interactive)", class = "btn btn-primary"),
          actionButton(ns("exp_pdf"), "Export PDF (snapshot)", class = "btn btn-outline-primary"),
          actionButton(ns("exp_csv"), "Export Data CSV", class = "btn btn-outline-primary"),
          actionButton(ns("exp_r"), "Generate R Code", class = "btn btn-outline-primary")
        ),
        footer = modalButton("Close"),
        easyClose = TRUE
      ))
    })

    observeEvent(input$exp_html, shinyjs::click(ns("download_html")), ignoreInit = TRUE)
    observeEvent(input$exp_pdf, shinyjs::click(ns("download_pdf")), ignoreInit = TRUE)
    observeEvent(input$exp_csv, shinyjs::click(ns("download_csv")), ignoreInit = TRUE)
    observeEvent(input$exp_r, shinyjs::click(ns("download_r")), ignoreInit = TRUE)

    observeEvent(input$btn_share, {
      base_url <- paste0(
        session$clientData$url_protocol,
        "//",
        session$clientData$url_hostname,
        if (!is.null(session$clientData$url_port) && nzchar(session$clientData$url_port)) paste0(":", session$clientData$url_port) else "",
        session$clientData$url_pathname
      )

      embed <- "<iframe src=\"YOUR_DEPLOYED_CHART_URL\" width=\"100%\" height=\"420\" frameborder=\"0\"></iframe>"

      showModal(modalDialog(
        title = "Share Dashboard",
        p("Shareable link (when deployed):"),
        textInput(ns("share_link"), "URL", value = base_url),
        p("Embed code (individual chart):"),
        textAreaInput(ns("embed_code"), "HTML", value = embed, rows = 3),
        footer = modalButton("Close"),
        easyClose = TRUE
      ))
    })

    return(list(cards = reactive(rv$cards), config = current_config))
  })
}

# Backward-compatible aliases
dashboard_ui <- dashboardUI
dashboard_server <- dashboardServer
