# upload_module.R
# Robust Shiny module for file upload, validation, and preview

uploadUI <- function(id) {
  ns <- NS(id)

  tagList(
    tags$style(HTML(sprintf("\n      .upload-card-%1$s {\n        border: 1px solid #dbe4ee;\n        border-radius: 12px;\n        box-shadow: 0 3px 12px rgba(22, 45, 77, 0.06);\n      }\n      .upload-dropzone-%1$s {\n        border: 2px dashed #9db6cf;\n        border-radius: 12px;\n        background: linear-gradient(180deg, #f8fbff 0%%, #f4f7fb 100%%);\n        padding: 1.2rem;\n        text-align: center;\n        transition: all 0.2s ease;\n        cursor: pointer;\n      }\n      .upload-dropzone-%1$s:hover, .upload-dropzone-%1$s.active {\n        border-color: #4c78a8;\n        background: #edf4fb;\n        box-shadow: inset 0 0 0 1px #84a7cb;\n      }\n      .upload-format-badges-%1$s .badge {\n        background: #e8eef6;\n        color: #35506b;\n        border: 1px solid #d3dfeb;\n        font-weight: 500;\n        margin-right: 0.4rem;\n        margin-bottom: 0.35rem;\n      }\n      .upload-meta-grid-%1$s {\n        display: grid;\n        grid-template-columns: repeat(2, minmax(0, 1fr));\n        gap: 0.6rem;\n      }\n      .upload-meta-item-%1$s {\n        background: #f6f9fc;\n        border: 1px solid #e2e9f1;\n        border-radius: 10px;\n        padding: 0.65rem 0.8rem;\n      }\n      .upload-meta-item-%1$s .k {\n        color: #6b7b8e;\n        font-size: 0.78rem;\n        line-height: 1.2;\n      }\n      .upload-meta-item-%1$s .v {\n        color: #253746;\n        font-weight: 600;\n        font-size: 0.95rem;\n      }\n      @media (max-width: 768px) {\n        .upload-meta-grid-%1$s { grid-template-columns: 1fr; }\n      }\n    ", id))),
    tags$script(HTML(sprintf("\n      (function() {\n        const dropId = '%1$s';\n        const inputId = '%2$s';\n        const bind = function() {\n          const dropzone = document.getElementById(dropId);\n          const fileInput = document.getElementById(inputId);\n          if (!dropzone || !fileInput || dropzone.dataset.bound === '1') return;\n          dropzone.dataset.bound = '1';\n\n          const openPicker = function() {\n            if (dropzone.dataset.pickerOpen === '1') return;\n            dropzone.dataset.pickerOpen = '1';\n            try { fileInput.click(); } catch (err) {}\n            setTimeout(function() { dropzone.dataset.pickerOpen = '0'; }, 500);\n          };\n\n          fileInput.addEventListener('change', function() {\n            dropzone.dataset.pickerOpen = '0';\n          });\n\n          ['dragenter','dragover'].forEach(function(evt) {\n            dropzone.addEventListener(evt, function(e) {\n              e.preventDefault();\n              e.stopPropagation();\n              dropzone.classList.add('active');\n            });\n          });\n\n          ['dragleave','drop'].forEach(function(evt) {\n            dropzone.addEventListener(evt, function(e) {\n              e.preventDefault();\n              e.stopPropagation();\n              if (evt === 'drop') {\n                try {\n                  const dt = new DataTransfer();\n                  if (e.dataTransfer && e.dataTransfer.files && e.dataTransfer.files.length > 0) {\n                    dt.items.add(e.dataTransfer.files[0]);\n                    fileInput.files = dt.files;\n                    fileInput.dispatchEvent(new Event('change', { bubbles: true }));\n                  }\n                } catch (err) {}\n              }\n              dropzone.classList.remove('active');\n            });\n          });\n\n          dropzone.addEventListener('click', function(e) {\n            if (e.target && e.target.closest('.upload-choose-btn')) {\n              e.preventDefault();\n              e.stopPropagation();\n              openPicker();\n              return;\n            }\n            openPicker();\n          });\n        };\n        document.addEventListener('DOMContentLoaded', bind);\n        document.addEventListener('shiny:connected', bind);\n        setTimeout(bind, 100);\n      })();\n    ", ns("dropzone"), ns("file_input")))),

    card(
      class = sprintf("upload-card-%s", id),
      card_header("Data Upload"),
      card_body(
        div(
          id = ns("dropzone"),
          class = sprintf("upload-dropzone-%s", id),
          tags$input(
            id = ns("file_input"),
            type = "file",
            accept = ".csv,.tsv,.xlsx,.xls,text/csv,text/tab-separated-values,application/vnd.ms-excel,application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
            style = "display:none;"
          ),
          tags$div(icon("cloud-upload-alt", class = "fa-2x"), style = "color:#4c78a8;margin-bottom:0.4rem;"),
          tags$div(tags$strong("Drag and drop a file"), " or click to browse"),
          tags$small(class = "text-muted", "CSV, TSV, XLSX, XLS up to 50 MB"),
          tags$button(
            type = "button",
            class = "btn btn-outline-primary btn-sm mt-2 upload-choose-btn",
            "Choose File"
          )
        ),
        br(),
        fluidRow(
          column(
            width = 6,
            selectInput(
              ns("csv_delimiter"),
              "CSV Delimiter",
              choices = c("Comma (,)" = ",", "Semicolon (;)" = ";", "Tab (\\t)" = "\t", "Pipe (|)" = "|"),
              selected = ","
            )
          ),
          column(
            width = 6,
            div(
              class = sprintf("upload-format-badges-%s", id),
              tags$label("Supported Types", class = "form-label"),
              tags$div(
                tags$span(class = "badge rounded-pill", icon("file-csv"), " CSV"),
                tags$span(class = "badge rounded-pill", icon("table"), " TSV"),
                tags$span(class = "badge rounded-pill", icon("file-excel"), " XLSX/XLS")
              )
            )
          )
        ),
        checkboxInput(ns("use_sample"), "Use sampled data for performance when dataset is large", value = TRUE),
        actionButton(ns("load_demo_data"), "Load Sample Dataset", icon = icon("database"), class = "btn btn-outline-primary btn-sm"),
        uiOutput(ns("upload_status")),
        uiOutput(ns("ai_upload_summary")),
        uiOutput(ns("error_message")),
        hr(),
        tags$h6("File Metadata"),
        uiOutput(ns("file_metadata")),
        br(),
        tags$h6("Column Types"),
        DT::DTOutput(ns("type_table")),
        br(),
        tags$h6("Missing Values by Column"),
        DT::DTOutput(ns("missing_table")),
        br(),
        tags$h6("Preview (First 100 Rows)"),
        DT::DTOutput(ns("preview_table"))
      )
    )
  )
}

uploadServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    max_size <- 50 * 1024^2
    allowed_ext <- c("csv", "tsv", "xlsx", "xls")

    # Enforce upload size in case global option was not configured.
    options(shiny.maxRequestSize = max_size)

    error_msg <- reactiveVal(NULL)
    upload_info <- reactiveVal(NULL)
    parsed_cache <- reactiveVal(NULL)
    cache_key <- reactiveVal(NULL)
    demo_data <- reactiveVal(NULL)

    observe({
      demo_path <- "data/sample_sales.csv"
      if (file.exists(demo_path)) {
        demo_data(readr::read_csv(demo_path, show_col_types = FALSE))
      }
    })

    read_delim_with_encoding <- function(path, delim) {
      try_utf8 <- try(
        readr::read_delim(
          file = path,
          delim = delim,
          locale = readr::locale(encoding = "UTF-8"),
          show_col_types = FALSE,
          progress = FALSE
        ),
        silent = TRUE
      )

      if (!inherits(try_utf8, "try-error")) {
        return(try_utf8)
      }

      readr::read_delim(
        file = path,
        delim = delim,
        locale = readr::locale(encoding = "Latin1"),
        show_col_types = FALSE,
        progress = FALSE
      )
    }

    parse_date_columns <- function(df) {
      if (!nrow(df)) return(df)
      tryCatch({

      date_name_pattern <- "date|time|timestamp|created|updated|dob"
      char_cols <- names(df)[vapply(df, function(x) is.character(x) || inherits(x, "POSIXct"), logical(1))]
      safe_parse <- function(v) {
        tryCatch(
          suppressWarnings(as.POSIXct(v, tz = "UTC", tryFormats = c(
            "%Y-%m-%d", "%d-%m-%Y", "%m/%d/%Y", "%d/%m/%Y",
            "%Y/%m/%d", "%Y-%m-%d %H:%M:%S", "%d-%b-%Y"
          ))),
          error = function(e) rep(as.POSIXct(NA), length(v))
        )
      }

      for (col in char_cols) {
        if (!is.character(df[[col]])) next

        sample_vals <- df[[col]][!is.na(df[[col]])]
        sample_vals <- sample_vals[nzchar(sample_vals)]
        sample_vals <- head(sample_vals, 200)

        if (!length(sample_vals)) next

        parsed <- safe_parse(sample_vals)
        parse_rate <- mean(!is.na(parsed))

        if (grepl(date_name_pattern, col, ignore.case = TRUE) || parse_rate >= 0.8) {
          full_parse <- safe_parse(df[[col]])

          if (sum(!is.na(full_parse)) > 0) {
            df[[col]] <- as.Date(full_parse)
          }
        }
      }

      df
      }, error = function(e) {
        # Never block upload on date coercion issues; preserve original values.
        df
      })
    }

    processed_data <- reactive({
      error_msg(NULL)
      f <- input$file_input

      if (is.null(f)) {
        req(demo_data())
        df0 <- janitor::clean_names(demo_data())
        upload_info(list(
          name = "sample_sales.csv",
          ext = "csv",
          size_bytes = as.numeric(object.size(df0)),
          rows = nrow(df0),
          cols = ncol(df0),
          upload_time = Sys.time(),
          sampled = FALSE
        ))
        return(df0)
      }

      withProgress(message = "Uploading and processing file...", value = 0, {
        incProgress(0.15, detail = "Validating file")

        valid <- validate_file_input(f, allowed_ext = allowed_ext, max_size_bytes = max_size)
        if (!isTRUE(valid$ok)) stop(friendly_error_message(valid$code))

        ext <- tolower(tools::file_ext(f$name))
        key <- paste0(f$datapath, "_", f$size, "_", ext, "_", input$csv_delimiter, "_", input$use_sample)
        if (identical(cache_key(), key) && !is.null(parsed_cache())) return(parsed_cache())

        incProgress(0.35, detail = "Reading file")

        df <- safe_execute(
          switch(
            ext,
            csv = read_delim_with_encoding(f$datapath, delim = input$csv_delimiter),
            tsv = read_delim_with_encoding(f$datapath, delim = "\t"),
            xlsx = readxl::read_excel(f$datapath),
            xls = readxl::read_excel(f$datapath)
          ),
          context = "upload:read",
          session = session,
          error_type = "upload_corrupt"
        )
        req(df)

        incProgress(0.65, detail = "Cleaning and validating structure")

        struct_ok <- validate_data_structure(df, min_rows = 1, min_cols = 1)
        if (!isTRUE(struct_ok$ok)) stop(friendly_error_message(struct_ok$code))

        df <- janitor::clean_names(df)
        df <- parse_date_columns(df)
        sampled <- FALSE
        if (isTRUE(input$use_sample) && nrow(df) > LUME_SAMPLE_THRESHOLD) {
          df <- sample_large_data(df, max_rows = LUME_SAMPLE_THRESHOLD)
          sampled <- TRUE
          shiny::showNotification(paste0("Large dataset detected: using ", format(LUME_SAMPLE_THRESHOLD, big.mark = ","), "-row sample for performance."), type = "message")
        }

        if (check_memory_guard(df)) stop(friendly_error_message("memory_limit"))

        upload_info(list(
          name = sanitize_filename(f$name),
          ext = ext,
          size_bytes = f$size,
          rows = nrow(df),
          cols = ncol(df),
          upload_time = Sys.time(),
          sampled = sampled
        ))

        parsed_cache(df)
        cache_key(key)
        incProgress(1, detail = "Done")
        df
      })
    }) %>% bindEvent(input$file_input, input$csv_delimiter, input$use_sample, ignoreInit = FALSE)

    observeEvent(input$load_demo_data, {
      updateSelectInput(session, "csv_delimiter", selected = ",")
      showNotification("Sample dataset loaded.", type = "message")
    })

    safe_data <- reactive({
      tryCatch(
        processed_data(),
        error = function(e) {
          error_msg(e$message)
          NULL
        }
      )
    })

    output$upload_status <- renderUI({
      f <- input$file_input
      if (is.null(f)) {
        div(class = "text-muted", "No file uploaded yet.")
      } else if (!is.null(error_msg())) {
        div(class = "text-muted", paste("Selected:", f$name))
      } else if (!is.null(safe_data())) {
        meta <- upload_info()
        suffix <- if (!is.null(meta$sampled) && isTRUE(meta$sampled)) " (sampled)" else ""
        div(class = "text-success fw-semibold", icon("check-circle"), paste("Loaded:", f$name, suffix))
      }
    })

    output$error_message <- renderUI({
      req(error_msg())
      div(
        class = "alert alert-danger",
        icon("exclamation-triangle"),
        error_msg()
      )
    })

    output$ai_upload_summary <- renderUI({
      req(safe_data())
      txt <- ai_upload_summary(safe_data(), upload_info())
      cls <- if (ai_is_enabled()) "alert alert-info" else "alert alert-light"
      div(class = cls, tags$strong("AI Upload Read"), tags$br(), ai_markdown_html(txt))
    })

    output$file_metadata <- renderUI({
      req(upload_info())
      meta <- upload_info()

      size_mb <- round(meta$size_bytes / 1024^2, 2)
      uploaded_at <- format(meta$upload_time, "%Y-%m-%d %H:%M:%S")

      div(
        class = sprintf("upload-meta-grid-%s", id),
        div(class = sprintf("upload-meta-item-%s", id), div(class = "k", "File"), div(class = "v", meta$name)),
        div(class = sprintf("upload-meta-item-%s", id), div(class = "k", "Format"), div(class = "v", toupper(meta$ext))),
        div(class = sprintf("upload-meta-item-%s", id), div(class = "k", "Rows"), div(class = "v", format(meta$rows, big.mark = ","))),
        div(class = sprintf("upload-meta-item-%s", id), div(class = "k", "Columns"), div(class = "v", meta$cols)),
        div(class = sprintf("upload-meta-item-%s", id), div(class = "k", "File Size"), div(class = "v", paste0(size_mb, " MB"))),
        div(class = sprintf("upload-meta-item-%s", id), div(class = "k", "Upload Time"), div(class = "v", uploaded_at))
      )
    })

    output$preview_table <- DT::renderDT({
      req(safe_data())
      DT::datatable(
        head(safe_data(), 100),
        options = list(pageLength = 10, scrollX = TRUE, dom = "tip"),
        class = "stripe hover",
        rownames = FALSE
      )
    })

    output$type_table <- DT::renderDT({
      req(safe_data())
      df <- safe_data()

      type_df <- data.frame(
        column = names(df),
        detected_type = vapply(df, function(x) {
          if (inherits(x, "Date") || inherits(x, "POSIXct") || inherits(x, "POSIXt")) return("date")
          if (is.numeric(x)) return("numeric")
          if (is.factor(x)) return("factor")
          if (is.character(x)) return("character")
          class(x)[1]
        }, character(1)),
        stringsAsFactors = FALSE
      )

      DT::datatable(
        type_df,
        options = list(pageLength = 8, dom = "tip", scrollX = TRUE),
        rownames = FALSE
      )
    })

    output$missing_table <- DT::renderDT({
      req(safe_data())
      df <- safe_data()

      miss_df <- data.frame(
        column = names(df),
        missing_count = vapply(df, function(x) sum(is.na(x)), numeric(1)),
        missing_pct = round(vapply(df, function(x) mean(is.na(x)) * 100, numeric(1)), 2),
        stringsAsFactors = FALSE
      )

      DT::datatable(
        miss_df,
        options = list(pageLength = 8, dom = "tip", scrollX = TRUE),
        rownames = FALSE
      )
    })

    return(safe_data)
  })
}

# Backward-compatible aliases for existing app code
upload_ui <- uploadUI
upload_server <- uploadServer
