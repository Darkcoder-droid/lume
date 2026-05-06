# transform_module.R
# Stateful transformation builder for Shiny dashboard data workflows

transformUI <- function(id) {
  ns <- NS(id)

  tagList(
    tags$style(HTML(sprintf("\n      .transform-shell-%1$s .card {\n        border: 1px solid #d8e1ea;\n        box-shadow: 0 4px 14px rgba(35, 55, 80, 0.05);\n      }\n      .transform-shell-%1$s .transform-code {\n        font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, 'Liberation Mono', 'Courier New', monospace;\n        font-size: 0.9rem;\n      }\n      .transform-shell-%1$s .transform-active-dot {\n        width: 9px;\n        height: 9px;\n        display: inline-block;\n        border-radius: 50%%;\n        background: #2f78b7;\n        margin-right: 0.4rem;\n      }\n      .transform-shell-%1$s .accordion-button {\n        font-weight: 600;\n      }\n    ", id))),

    div(
      class = sprintf("transform-shell-%s", id),
      layout_sidebar(
        sidebar = sidebar(
          width = 380,
          open = "desktop",

          h5("Transformation Builder"),
          p(class = "text-muted small", "Apply step-by-step transformations. Each step is tracked and can be removed."),
          actionButton(ns("reset_all"), "Reset All", icon = icon("rotate-left"), class = "btn btn-outline-secondary btn-sm"),
          hr(),

          accordion(
            accordion_panel(
              "Filtering",
              selectInput(ns("filter_logic"), "Condition Logic", choices = c("AND", "OR"), selected = "AND"),
              numericInput(ns("filter_n"), "Number of Conditions", value = 1, min = 1, max = 5),
              uiOutput(ns("filter_builder_ui")),
              fluidRow(
                column(8, textInput(ns("preset_name"), "Preset Name", placeholder = "e.g. High Value Segment")),
                column(4, br(), actionButton(ns("save_preset"), "Save", class = "btn btn-outline-primary btn-sm"))
              ),
              uiOutput(ns("preset_ui")),
              br(),
              actionButton(ns("apply_filter"), "Apply Filter", class = "btn btn-primary btn-sm")
            ),

            accordion_panel(
              "Column Operations",
              uiOutput(ns("column_select_ui")),
              actionButton(ns("apply_select_cols"), "Apply Column Selection", class = "btn btn-primary btn-sm"),
              hr(),
              uiOutput(ns("rename_ui")),
              textInput(ns("rename_to"), "New Name"),
              actionButton(ns("apply_rename"), "Apply Rename", class = "btn btn-primary btn-sm"),
              hr(),
              uiOutput(ns("type_change_ui")),
              selectInput(ns("new_type"), "New Type", choices = c("numeric", "character", "date", "factor")),
              actionButton(ns("apply_type"), "Apply Type Change", class = "btn btn-primary btn-sm"),
              hr(),
              selectInput(ns("calc_mode"), "Calculated Column Type", choices = c("Arithmetic", "Text Concatenation")),
              textInput(ns("calc_name"), "New Calculated Column Name", placeholder = "e.g. total_sales"),
              uiOutput(ns("calc_ui")),
              actionButton(ns("apply_calc"), "Apply Calculated Column", class = "btn btn-primary btn-sm")
            ),

            accordion_panel(
              "Aggregation & Pivot",
              uiOutput(ns("group_by_ui")),
              uiOutput(ns("agg_target_ui")),
              selectInput(ns("agg_fn"), "Aggregation Function", choices = c("sum", "mean", "median", "count", "min", "max")),
              textInput(ns("agg_name"), "Output Column Name", value = "aggregated_value"),
              actionButton(ns("apply_aggregate"), "Apply Aggregation", class = "btn btn-primary btn-sm"),
              hr(),
              selectInput(ns("pivot_mode"), "Pivot Mode", choices = c("Wide to Long", "Long to Wide")),
              uiOutput(ns("pivot_ui")),
              actionButton(ns("apply_pivot"), "Apply Pivot", class = "btn btn-primary btn-sm")
            ),

            accordion_panel(
              "Data Cleaning",
              selectInput(ns("na_mode"), "Missing Value Strategy", choices = c("remove_rows", "fill_value", "forward_fill", "backward_fill")),
              textInput(ns("na_fill_value"), "Fill Value (when using fill_value)", placeholder = "0 or Unknown"),
              uiOutput(ns("na_cols_ui")),
              actionButton(ns("apply_na"), "Apply Missing Value Rule", class = "btn btn-primary btn-sm"),
              hr(),
              checkboxInput(ns("remove_dupes"), "Remove Duplicate Rows", value = TRUE),
              actionButton(ns("apply_dupes"), "Apply Duplicate Rule", class = "btn btn-primary btn-sm"),
              hr(),
              checkboxInput(ns("trim_ws"), "Trim Whitespace", value = TRUE),
              selectInput(ns("case_mode"), "Case Conversion", choices = c("none", "upper", "lower", "title")),
              uiOutput(ns("text_cols_ui")),
              actionButton(ns("apply_text_clean"), "Apply Text Cleaning", class = "btn btn-primary btn-sm")
            ),

            accordion_panel(
              "AI Assistant (Safe)",
              p(class = "text-muted small", "AI suggests a sequence of transformations only. Nothing is applied automatically."),
              actionButton(ns("ai_suggest"), "Generate Suggestions", icon = icon("wand-magic-sparkles"), class = "btn btn-outline-primary btn-sm"),
              br(), br(),
              uiOutput(ns("ai_suggest_md"))
            )
          )
        ),

        card(
          card_header("Current Transformed Data"),
          card_body(
            uiOutput(ns("active_steps_badges")),
            DT::DTOutput(ns("data_preview"))
          )
        ),

        card(
          card_header("Transformation History"),
          card_body(uiOutput(ns("history_ui")))
        ),

        card(
          card_header("Summary Statistics"),
          card_body(DT::DTOutput(ns("summary_table")))
        )
      )
    )
  )
}

transformServer <- function(id, data_r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    safe_to_date <- function(x) {
      if (inherits(x, "Date")) return(x)
      if (inherits(x, "POSIXt")) return(as.Date(x))
      x <- as.character(x)
      out <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
      idx <- is.na(out)
      if (any(idx)) out[idx] <- suppressWarnings(as.Date(x[idx], format = "%d-%m-%Y"))
      idx <- is.na(out)
      if (any(idx)) out[idx] <- suppressWarnings(as.Date(x[idx], format = "%m/%d/%Y"))
      idx <- is.na(out)
      if (any(idx)) out[idx] <- suppressWarnings(as.Date(x[idx], format = "%d/%m/%Y"))
      idx <- is.na(out)
      if (any(idx)) out[idx] <- suppressWarnings(as.Date(x[idx], format = "%Y/%m/%d"))
      out
    }

    steps <- reactiveVal(list())
    presets <- reactiveVal(list())
    step_counter <- reactiveVal(0)
    ai_suggest_text <- reactiveVal(NULL)

    get_df <- reactive({
      req(data_r())
      df <- data_r()
      current_steps <- steps()

      if (!length(current_steps)) return(df)

      for (s in current_steps) {
        df <- s$fn(df)
      }
      df
    })

    base_cols <- reactive({
      df <- data_r()
      if (is.null(df)) character(0) else names(df)
    })

    current_cols <- reactive({
      df <- get_df()
      if (is.null(df)) character(0) else names(df)
    })

    add_step <- function(type, description, fn, params = list()) {
      next_id <- step_counter() + 1
      step_counter(next_id)
      new_step <- list(id = next_id, type = type, description = description, fn = fn, params = params, at = Sys.time())
      steps(append(steps(), list(new_step)))
    }

    output$filter_builder_ui <- renderUI({
      req(base_cols())
      n <- input$filter_n %||% 1
      cols <- base_cols()

      tagList(lapply(seq_len(n), function(i) {
        wellPanel(
          class = "mb-2",
          selectInput(ns(paste0("f_col_", i)), paste("Column", i), choices = cols, selected = cols[1]),
          uiOutput(ns(paste0("f_op_ui_", i))),
          uiOutput(ns(paste0("f_val_ui_", i)))
        )
      }))
    })

    observe({
      n <- input$filter_n %||% 1
      cols <- base_cols()
      if (!length(cols)) return()

      for (i in seq_len(n)) {
        local({
          ii <- i
          output[[paste0("f_op_ui_", ii)]] <- renderUI({
            req(input[[paste0("f_col_", ii)]])
            col_name <- input[[paste0("f_col_", ii)]]
            df <- data_r()
            req(df, col_name %in% names(df))

            x <- df[[col_name]]
            ops <- if (is.numeric(x)) {
              c("between", ">=", "<=", "==")
            } else if (inherits(x, "Date") || inherits(x, "POSIXt")) {
              c("between", ">=", "<=", "==")
            } else {
              c("contains", "equals", "starts_with", "ends_with")
            }

            selectInput(ns(paste0("f_op_", ii)), "Operator", choices = ops)
          })

          output[[paste0("f_val_ui_", ii)]] <- renderUI({
            req(input[[paste0("f_col_", ii)]], input[[paste0("f_op_", ii)]])
            col_name <- input[[paste0("f_col_", ii)]]
            op <- input[[paste0("f_op_", ii)]]
            df <- data_r()
            req(df, col_name %in% names(df))
            x <- df[[col_name]]

            if (op == "between") {
              if (is.numeric(x)) {
                rng <- range(x, na.rm = TRUE)
                fluidRow(
                  column(6, numericInput(ns(paste0("f_v1_", ii)), "From", value = if (is.finite(rng[1])) rng[1] else 0)),
                  column(6, numericInput(ns(paste0("f_v2_", ii)), "To", value = if (is.finite(rng[2])) rng[2] else 0))
                )
              } else {
                d <- safe_to_date(x)
                d <- d[!is.na(d)]
                min_d <- if (length(d)) min(d) else Sys.Date() - 30
                max_d <- if (length(d)) max(d) else Sys.Date()
                fluidRow(
                  column(6, dateInput(ns(paste0("f_v1_", ii)), "From", value = min_d)),
                  column(6, dateInput(ns(paste0("f_v2_", ii)), "To", value = max_d))
                )
              }
            } else {
              if (is.numeric(x)) {
                numericInput(ns(paste0("f_v1_", ii)), "Value", value = 0)
              } else if (inherits(x, "Date") || inherits(x, "POSIXt")) {
                dateInput(ns(paste0("f_v1_", ii)), "Date", value = Sys.Date())
              } else {
                textInput(ns(paste0("f_v1_", ii)), "Text", value = "")
              }
            }
          })
        })
      }
    })

    observeEvent(input$save_preset, {
      req(nzchar(input$preset_name))
      n <- input$filter_n %||% 1
      cfg <- list(logic = input$filter_logic, n = n, conditions = list())

      for (i in seq_len(n)) {
        cfg$conditions[[i]] <- list(
          col = input[[paste0("f_col_", i)]],
          op = input[[paste0("f_op_", i)]],
          v1 = input[[paste0("f_v1_", i)]],
          v2 = input[[paste0("f_v2_", i)]]
        )
      }

      p <- presets()
      p[[input$preset_name]] <- cfg
      presets(p)
      showNotification("Filter preset saved.", type = "message")
    })

    output$preset_ui <- renderUI({
      p <- presets()
      if (!length(p)) return(NULL)
      selectInput(ns("preset_pick"), "Saved Presets", choices = names(p))
    })

    observeEvent(input$preset_pick, {
      req(input$preset_pick)
      cfg <- presets()[[input$preset_pick]]
      req(cfg)
      updateSelectInput(session, "filter_logic", selected = cfg$logic)
      updateNumericInput(session, "filter_n", value = cfg$n)

      session$onFlushed(function() {
        for (i in seq_len(cfg$n)) {
          cnd <- cfg$conditions[[i]]
          updateSelectInput(session, paste0("f_col_", i), selected = cnd$col)
          updateSelectInput(session, paste0("f_op_", i), selected = cnd$op)
          if (!is.null(cnd$v1)) {
            try(updateTextInput(session, paste0("f_v1_", i), value = as.character(cnd$v1)), silent = TRUE)
            try(updateNumericInput(session, paste0("f_v1_", i), value = suppressWarnings(as.numeric(cnd$v1))), silent = TRUE)
            try(updateDateInput(session, paste0("f_v1_", i), value = safe_to_date(cnd$v1)[1]), silent = TRUE)
          }
          if (!is.null(cnd$v2)) {
            try(updateTextInput(session, paste0("f_v2_", i), value = as.character(cnd$v2)), silent = TRUE)
            try(updateNumericInput(session, paste0("f_v2_", i), value = suppressWarnings(as.numeric(cnd$v2))), silent = TRUE)
            try(updateDateInput(session, paste0("f_v2_", i), value = safe_to_date(cnd$v2)[1]), silent = TRUE)
          }
        }
      }, once = TRUE)
    })

    observeEvent(input$apply_filter, {
      req(data_r())
      n <- input$filter_n %||% 1
      logic <- input$filter_logic

      conds <- lapply(seq_len(n), function(i) {
        list(
          col = input[[paste0("f_col_", i)]],
          op = input[[paste0("f_op_", i)]],
          v1 = input[[paste0("f_v1_", i)]],
          v2 = input[[paste0("f_v2_", i)]]
        )
      })

      ffun <- function(df) {
        keep <- rep(if (logic == "AND") TRUE else FALSE, nrow(df))
        for (cnd in conds) {
          col <- cnd$col
          op <- cnd$op
          x <- df[[col]]
          m <- rep(TRUE, nrow(df))

          if (is.numeric(x)) {
            if (op == "between") m <- x >= as.numeric(cnd$v1) & x <= as.numeric(cnd$v2)
            if (op == ">=") m <- x >= as.numeric(cnd$v1)
            if (op == "<=") m <- x <= as.numeric(cnd$v1)
            if (op == "==") m <- x == as.numeric(cnd$v1)
          } else if (inherits(x, "Date") || inherits(x, "POSIXt")) {
            xd <- safe_to_date(x)
            v1 <- safe_to_date(cnd$v1)
            v2 <- safe_to_date(cnd$v2)
            if (op == "between") m <- xd >= v1 & xd <= v2
            if (op == ">=") m <- xd >= v1
            if (op == "<=") m <- xd <= v1
            if (op == "==") m <- xd == v1
          } else {
            xs <- as.character(x)
            v <- as.character(cnd$v1)
            if (op == "contains") m <- stringr::str_detect(tolower(xs), stringr::fixed(tolower(v)))
            if (op == "equals") m <- tolower(xs) == tolower(v)
            if (op == "starts_with") m <- startsWith(tolower(xs), tolower(v))
            if (op == "ends_with") m <- endsWith(tolower(xs), tolower(v))
          }

          m[is.na(m)] <- FALSE
          if (logic == "AND") keep <- keep & m else keep <- keep | m
        }
        df[keep, , drop = FALSE]
      }

      add_step("filter", sprintf("Filter rows (%s across %s conditions)", logic, n), ffun, list(logic = logic, conds = conds))
    })

    output$column_select_ui <- renderUI({
      req(current_cols())
      checkboxGroupInput(ns("selected_cols"), "Columns to Keep", choices = current_cols(), selected = current_cols())
    })

    observeEvent(input$apply_select_cols, {
      req(input$selected_cols)
      sel <- input$selected_cols
      add_step("select_columns", sprintf("Keep %s selected columns", length(sel)), function(df) df[, sel, drop = FALSE], list(cols = sel))
    })

    output$rename_ui <- renderUI({
      req(current_cols())
      selectInput(ns("rename_col"), "Column to Rename", choices = current_cols())
    })

    observeEvent(input$apply_rename, {
      req(input$rename_col, input$rename_to)
      clean_new_name <- sanitize_text(input$rename_to)
      shiny::validate(shiny::need(nzchar(clean_new_name), "Please provide a new column name."))
      old <- input$rename_col
      new <- janitor::make_clean_names(clean_new_name)
      add_step("rename", sprintf("Rename %s to %s", old, new), function(df) {
        if (old %in% names(df)) names(df)[names(df) == old] <- new
        df
      }, list(old = old, new = new))
    })

    output$type_change_ui <- renderUI({
      req(current_cols())
      selectInput(ns("type_col"), "Column to Convert", choices = current_cols())
    })

    observeEvent(input$apply_type, {
      req(input$type_col, input$new_type)
      col <- input$type_col
      t <- input$new_type
      add_step("type_change", sprintf("Convert %s to %s", col, t), function(df) {
        if (!(col %in% names(df))) return(df)
        if (t == "numeric") df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
        if (t == "character") df[[col]] <- as.character(df[[col]])
        if (t == "date") df[[col]] <- safe_to_date(df[[col]])
        if (t == "factor") df[[col]] <- as.factor(df[[col]])
        df
      }, list(column = col, type = t))
    })

    output$calc_ui <- renderUI({
      req(current_cols())
      if (input$calc_mode == "Arithmetic") {
        tagList(
          selectInput(ns("calc_col_a"), "Column A", choices = current_cols()),
          selectInput(ns("calc_op"), "Operator", choices = c("+", "-", "*", "/")),
          selectInput(ns("calc_col_b"), "Column B", choices = current_cols())
        )
      } else {
        tagList(
          selectizeInput(ns("calc_text_cols"), "Columns to Concatenate", choices = current_cols(), multiple = TRUE),
          textInput(ns("calc_sep"), "Separator", value = "_")
        )
      }
    })

    observeEvent(input$apply_calc, {
      req(nzchar(input$calc_name))
      new_col <- janitor::make_clean_names(sanitize_text(input$calc_name))

      if (input$calc_mode == "Arithmetic") {
        req(input$calc_col_a, input$calc_col_b, input$calc_op)
        a <- input$calc_col_a
        b <- input$calc_col_b
        op <- input$calc_op
        add_step("calc", sprintf("Create %s = %s %s %s", new_col, a, op, b), function(df) {
          av <- suppressWarnings(as.numeric(df[[a]]))
          bv <- suppressWarnings(as.numeric(df[[b]]))
          df[[new_col]] <- switch(op,
            "+" = av + bv,
            "-" = av - bv,
            "*" = av * bv,
            "/" = ifelse(bv == 0, NA_real_, av / bv)
          )
          df
        }, list(mode = "arith", name = new_col, a = a, b = b, op = op))
      } else {
        req(input$calc_text_cols)
        cols <- input$calc_text_cols
        sep <- input$calc_sep %||% ""
        add_step("calc", sprintf("Create %s by concatenating %s columns", new_col, length(cols)), function(df) {
          df[[new_col]] <- apply(df[, cols, drop = FALSE], 1, function(r) paste(as.character(r), collapse = sep))
          df
        }, list(mode = "text", name = new_col, cols = cols, sep = sep))
      }
    })

    output$group_by_ui <- renderUI({
      req(current_cols())
      selectizeInput(ns("group_cols"), "Group By Columns", choices = current_cols(), multiple = TRUE)
    })

    output$agg_target_ui <- renderUI({
      df <- get_df()
      req(df)
      num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
      selectInput(ns("agg_target"), "Aggregate Target", choices = c("(none for count)" = "", num_cols))
    })

    observeEvent(input$apply_aggregate, {
      req(input$group_cols, input$agg_fn, nzchar(input$agg_name))
      gcols <- input$group_cols
      fn <- input$agg_fn
      target <- input$agg_target
      out_name <- janitor::make_clean_names(input$agg_name)

      add_step("aggregate", sprintf("Aggregate by %s using %s", paste(gcols, collapse = ", "), fn), function(df) {
        g <- dplyr::group_by(df, dplyr::across(dplyr::all_of(gcols)))
        if (fn == "count") {
          dplyr::summarise(g, !!out_name := dplyr::n(), .groups = "drop")
        } else {
          if (!nzchar(target) || !(target %in% names(df))) return(df)
          dplyr::summarise(
            g,
            !!out_name := switch(fn,
              sum = sum(.data[[target]], na.rm = TRUE),
              mean = mean(.data[[target]], na.rm = TRUE),
              median = median(.data[[target]], na.rm = TRUE),
              min = min(.data[[target]], na.rm = TRUE),
              max = max(.data[[target]], na.rm = TRUE)
            ),
            .groups = "drop"
          )
        }
      }, list(group = gcols, fn = fn, target = target, out = out_name))
    })

    output$pivot_ui <- renderUI({
      req(current_cols())
      if (input$pivot_mode == "Wide to Long") {
        tagList(
          selectizeInput(ns("pivot_long_cols"), "Columns to Pivot Longer", choices = current_cols(), multiple = TRUE),
          textInput(ns("pivot_names_to"), "Names To", value = "variable"),
          textInput(ns("pivot_values_to"), "Values To", value = "value")
        )
      } else {
        tagList(
          selectInput(ns("pivot_names_from"), "Names From Column", choices = current_cols()),
          selectInput(ns("pivot_values_from"), "Values From Column", choices = current_cols())
        )
      }
    })

    observeEvent(input$apply_pivot, {
      if (input$pivot_mode == "Wide to Long") {
        req(input$pivot_long_cols, nzchar(input$pivot_names_to), nzchar(input$pivot_values_to))
        cols <- input$pivot_long_cols
        n_to <- janitor::make_clean_names(input$pivot_names_to)
        v_to <- janitor::make_clean_names(input$pivot_values_to)
        add_step("pivot", sprintf("Pivot longer (%s columns)", length(cols)), function(df) {
          tidyr::pivot_longer(df, cols = dplyr::all_of(cols), names_to = n_to, values_to = v_to)
        }, list(mode = "long", cols = cols, names_to = n_to, values_to = v_to))
      } else {
        req(input$pivot_names_from, input$pivot_values_from)
        n_from <- input$pivot_names_from
        v_from <- input$pivot_values_from
        add_step("pivot", sprintf("Pivot wider using %s", n_from), function(df) {
          tidyr::pivot_wider(df, names_from = dplyr::all_of(n_from), values_from = dplyr::all_of(v_from))
        }, list(mode = "wide", names_from = n_from, values_from = v_from))
      }
    })

    output$na_cols_ui <- renderUI({
      req(current_cols())
      selectizeInput(ns("na_cols"), "Columns for Missing Value Handling", choices = current_cols(), multiple = TRUE, selected = current_cols())
    })

    observeEvent(input$apply_na, {
      req(input$na_mode, input$na_cols)
      mode <- input$na_mode
      cols <- input$na_cols
      fv <- input$na_fill_value

      add_step("missing", sprintf("Missing values: %s on %s columns", mode, length(cols)), function(df) {
        if (mode == "remove_rows") {
          df <- tidyr::drop_na(df, dplyr::all_of(cols))
        } else if (mode == "fill_value") {
          for (cc in cols) {
            if (is.numeric(df[[cc]])) {
              fill_num <- suppressWarnings(as.numeric(fv))
              if (is.na(fill_num)) fill_num <- 0
              df[[cc]][is.na(df[[cc]])] <- fill_num
            } else {
              df[[cc]][is.na(df[[cc]])] <- as.character(fv)
            }
          }
        } else if (mode == "forward_fill") {
          df <- tidyr::fill(df, dplyr::all_of(cols), .direction = "down")
        } else if (mode == "backward_fill") {
          df <- tidyr::fill(df, dplyr::all_of(cols), .direction = "up")
        }
        df
      }, list(mode = mode, cols = cols, fill_value = fv))
    })

    observeEvent(input$apply_dupes, {
      if (!isTRUE(input$remove_dupes)) return()
      add_step("dedupe", "Remove duplicate rows", function(df) dplyr::distinct(df), list())
    })

    output$text_cols_ui <- renderUI({
      df <- get_df()
      req(df)
      ch <- names(df)[vapply(df, is.character, logical(1))]
      selectizeInput(ns("text_cols"), "Text Columns", choices = ch, multiple = TRUE, selected = ch)
    })

    observeEvent(input$apply_text_clean, {
      req(input$text_cols)
      cols <- input$text_cols
      trim <- isTRUE(input$trim_ws)
      cmode <- input$case_mode

      add_step("text_clean", sprintf("Text cleanup on %s columns", length(cols)), function(df) {
        for (cc in cols) {
          x <- as.character(df[[cc]])
          if (trim) x <- stringr::str_trim(x)
          if (cmode == "upper") x <- toupper(x)
          if (cmode == "lower") x <- tolower(x)
          if (cmode == "title") x <- tools::toTitleCase(tolower(x))
          df[[cc]] <- x
        }
        df
      }, list(cols = cols, trim = trim, case_mode = cmode))
    })

    observeEvent(input$reset_all, {
      steps(list())
      showNotification("Transformation pipeline reset.", type = "warning")
    })

    observeEvent(input$ai_suggest, {
      req(get_df())
      showNotification("Generating AI suggestions...", type = "message")
      txt <- ai_transform_suggestions(get_df(), steps())
      ai_suggest_text(txt)
    })

    output$ai_suggest_md <- renderUI({
      req(ai_suggest_text())
      ai_markdown_html(ai_suggest_text())
    })

    output$active_steps_badges <- renderUI({
      s <- steps()
      if (!length(s)) return(div(class = "text-muted small", "No active transformations."))
      div(
        lapply(s, function(x) {
          tags$span(class = "badge rounded-pill text-bg-light me-1 mb-1 transform-code",
            tags$span(class = "transform-active-dot"),
            paste0("#", x$id, " ", x$type)
          )
        })
      )
    })

    output$history_ui <- renderUI({
      s <- steps()
      if (!length(s)) return(div(class = "text-muted", "No transformations applied yet."))

      tagList(lapply(s, function(st) {
        div(
          class = "border rounded p-2 mb-2",
          div(class = "d-flex justify-content-between align-items-start",
            div(
              tags$div(class = "fw-semibold", paste0("#", st$id, " ", st$type)),
              tags$div(class = "small text-muted", format(st$at, "%Y-%m-%d %H:%M:%S")),
              tags$div(class = "transform-code", st$description)
            ),
            actionButton(ns(paste0("remove_step_", st$id)), "Remove", class = "btn btn-outline-danger btn-sm")
          )
        )
      }))
    })

    observe({
      s <- steps()
      lapply(s, function(st) {
        observeEvent(input[[paste0("remove_step_", st$id)]], {
          remaining <- Filter(function(x) x$id != st$id, steps())
          steps(remaining)
        }, ignoreInit = TRUE)
      })
    })

    output$data_preview <- DT::renderDT({
      req(get_df())
      DT::datatable(
        head(get_df(), 100),
        options = list(pageLength = 10, scrollX = TRUE),
        class = "stripe hover compact transform-code",
        rownames = FALSE
      )
    })

    output$summary_table <- DT::renderDT({
      req(get_df())
      df <- get_df()

      summary_df <- data.frame(
        metric = c(
          "Rows",
          "Columns",
          "Numeric Columns",
          "Character Columns",
          "Date/Time Columns",
          "Factor Columns",
          "Total Missing Values"
        ),
        value = c(
          nrow(df),
          ncol(df),
          sum(vapply(df, is.numeric, logical(1))),
          sum(vapply(df, is.character, logical(1))),
          sum(vapply(df, function(x) inherits(x, "Date") || inherits(x, "POSIXt"), logical(1))),
          sum(vapply(df, is.factor, logical(1))),
          sum(vapply(df, function(x) sum(is.na(x)), numeric(1)))
        ),
        stringsAsFactors = FALSE
      )

      DT::datatable(summary_df, options = list(dom = "t", paging = FALSE), rownames = FALSE)
    })

    return(get_df)
  })
}

# Backward-compatible aliases
transform_ui <- transformUI
transform_server <- transformServer
