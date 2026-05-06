# insights_module.R
# Friendly auto-insights overview for first-time users

insightsUI <- function(id) {
  ns <- NS(id)

  tagList(
    card(
      fill = FALSE,
      card_header("Auto Insights"),
      card_body(
        p(class = "text-muted", "Instant overview of what your dataset represents."),
        uiOutput(ns("narrative")),
        uiOutput(ns("ai_summary")),
        div(
          class = "mb-3",
          fluidRow(
            column(3, uiOutput(ns("kpi_rows"))),
            column(3, uiOutput(ns("kpi_cols"))),
            column(3, uiOutput(ns("kpi_missing"))),
            column(3, uiOutput(ns("kpi_types")))
          )
        ),
        div(class = "mb-3", uiOutput(ns("sales_kpis"))),
        uiOutput(ns("insight_cards_ui"))
      )
    )
  )
}

insightsServer <- function(id, data_r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    safe_date_vec <- function(x) {
      if (inherits(x, "Date")) return(x)
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

    safe_plot <- function(expr) {
      tryCatch(expr, error = function(e) {
        graphics::plot.new()
        graphics::text(0.5, 0.5, labels = paste("Insight unavailable:", as.character(e$message)), cex = 0.9, col = "#666666")
      })
    }

    summary_info <- reactive({
      req(data_r())
      df <- data_r()
      req(is.data.frame(df), nrow(df) > 0, ncol(df) > 0)

      num_n <- sum(vapply(df, is.numeric, logical(1)))
      chr_n <- sum(vapply(df, function(x) is.character(x) || is.factor(x), logical(1)))
      date_n <- sum(vapply(df, function(x) inherits(x, "Date") || inherits(x, "POSIXt"), logical(1)))
      miss <- sum(vapply(df, function(x) sum(is.na(x)), numeric(1)))
      miss_pct <- round(miss / (nrow(df) * ncol(df)) * 100, 2)

      list(df = df, num_n = num_n, chr_n = chr_n, date_n = date_n, miss = miss, miss_pct = miss_pct)
    })

    detect_sales_schema <- reactive({
      s <- summary_info()
      df <- s$df
      nms <- names(df)
      norm_col <- function(x) {
        if (length(x) == 0 || is.null(x) || is.na(x) || !nzchar(as.character(x))) return("")
        as.character(x)
      }
      find_col <- function(patterns, numeric_only = FALSE) {
        cand <- nms[Reduce(`|`, lapply(patterns, function(p) grepl(p, nms, ignore.case = TRUE)))]
        if (numeric_only) cand <- cand[vapply(df[cand], is.numeric, logical(1))]
        norm_col(cand[1])
      }
      list(
        item = find_col(c("item", "product", "sku", "name")),
        qty = find_col(c("qty", "quantity", "units", "sold"), numeric_only = TRUE),
        price = find_col(c("price", "unit_price", "rate"), numeric_only = TRUE),
        sales = find_col(c("sales", "revenue", "amount", "total"), numeric_only = TRUE),
        date = find_col(c("date", "month", "period", "time"))
      )
    })

    kpi_card <- function(title, value, subtitle = NULL) {
      card(
        fill = FALSE,
        card_body(
          div(class = "text-muted small", title),
          div(style = "font-size:1.4rem;font-weight:700;", value),
          if (!is.null(subtitle)) div(class = "text-muted small", subtitle)
        )
      )
    }

    output$kpi_rows <- renderUI({
      s <- summary_info(); kpi_card("Rows", format(nrow(s$df), big.mark = ","))
    })
    output$kpi_cols <- renderUI({
      s <- summary_info(); kpi_card("Columns", ncol(s$df))
    })
    output$kpi_missing <- renderUI({
      s <- summary_info(); kpi_card("Missing Values", format(s$miss, big.mark = ","), paste0(s$miss_pct, "% of all cells"))
    })
    output$kpi_types <- renderUI({
      s <- summary_info(); kpi_card("Column Types", paste0("N:", s$num_n, "  C:", s$chr_n, "  D:", s$date_n))
    })

    output$narrative <- renderUI({
      s <- summary_info()
      sc <- detect_sales_schema()
      tips <- c()
      if (s$miss_pct > 15) tips <- c(tips, "Missing values are relatively high; consider cleaning before deep analysis.")
      if (s$num_n >= 2) tips <- c(tips, "You have enough numeric columns for correlation and trend analysis.")
      if (s$date_n >= 1) tips <- c(tips, "Date/time fields detected, so time-based trends are available.")
      if (nzchar(sc$item) && nzchar(sc$qty)) tips <- c(tips, paste0("Sales-like structure detected: '", sc$item, "' and '", sc$qty, "' support item performance tracking."))
      if (nzchar(sc$price) && nzchar(sc$qty)) tips <- c(tips, "Price and quantity are available, so unit economics can be explored quickly.")
      if (!length(tips)) tips <- "Dataset looks analysis-ready. You can continue with custom visuals or dashboard builder."

      tagList(
        div(class = "alert alert-info", tags$strong("Quick read:"),
            p(sprintf("This dataset has %s rows and %s columns.", format(nrow(s$df), big.mark = ","), ncol(s$df))),
            tags$ul(lapply(tips, tags$li)))
      )
    })

    output$ai_summary <- renderUI({
      s <- summary_info()
      txt <- ai_dataset_summary(s$df)
      cls <- if (ai_is_enabled()) "alert alert-secondary" else "alert alert-light"
      div(class = cls, tags$strong("AI Summary"), tags$br(), ai_markdown_html(txt))
    })

    output$narrated_insights <- renderUI({
      tryCatch({
        s <- summary_info(); df <- s$df; sc <- detect_sales_schema()
        bullets <- list()

      # What happened
      happened <- c()
      happened <- c(happened, sprintf("Dataset snapshot includes %s rows across %s columns.", format(nrow(df), big.mark = ","), ncol(df)))
      if (nzchar(sc$sales)) happened <- c(happened, sprintf("Total sales observed: %s.", scales::dollar(sum(df[[sc$sales]], na.rm = TRUE))))
      if (nzchar(sc$qty)) happened <- c(happened, sprintf("Total units/items sold: %s.", format(sum(df[[sc$qty]], na.rm = TRUE), big.mark = ",")))
      bullets[[1]] <- list(title = "What Happened", lines = happened)

      # Why it matters
      matters <- c()
      if (s$miss_pct > 10) matters <- c(matters, sprintf("Missing data is %.2f%% of all cells, which can bias summaries and trend signals.", s$miss_pct))
      if (nzchar(sc$item) && nzchar(sc$qty)) {
        top_item <- df %>% dplyr::group_by(.data[[sc$item]]) %>% dplyr::summarise(q = sum(.data[[sc$qty]], na.rm = TRUE), .groups = "drop") %>% dplyr::arrange(dplyr::desc(q)) %>% head(1)
        if (nrow(top_item) == 1) matters <- c(matters, sprintf("Top item concentration suggests '%s' is a major driver.", as.character(top_item[[sc$item]][1])))
      }
      if (nzchar(sc$price) && nzchar(sc$qty)) matters <- c(matters, "Price–quantity relationship can indicate discount sensitivity or premium segmentation.")
      if (!length(matters)) matters <- "Current profile is stable and suitable for deeper slicing by region/time/category."
      bullets[[2]] <- list(title = "Why It Matters", lines = matters)

      # What to check next
      next_steps <- c()
      if (nzchar(sc$date)) next_steps <- c(next_steps, "Review monthly/weekly trend for seasonality, outliers, and momentum shifts.")
      if (nzchar(sc$item) && nzchar(sc$qty)) next_steps <- c(next_steps, "Compare top items by both quantity and revenue to spot margin opportunities.")
      if (s$num_n >= 2) next_steps <- c(next_steps, "Use correlation heatmap to identify metrics that move together before modeling.")
      if (s$miss_pct > 0) next_steps <- c(next_steps, "Apply missing-value handling in Advanced Transformations before final dashboarding.")
      bullets[[3]] <- list(title = "What To Check Next", lines = next_steps)

        card(
          class = "mb-3",
          card_header("Narrated Insights"),
          card_body(
            tags$div(class = "small text-muted mb-2", "AI-assisted executive summary from current dataset profile."),
            lapply(bullets, function(b) {
              tags$div(
                class = "mb-3",
                tags$div(style = "font-weight:700;", b$title),
                tags$ul(lapply(b$lines, tags$li))
              )
            })
          )
        )
      }, error = function(e) {
        div(class = "alert alert-warning", "Narrated insights are temporarily unavailable for this dataset.")
      })
    })

    output$sales_kpis <- renderUI({
      s <- summary_info(); df <- s$df; sc <- detect_sales_schema()
      if (!(nzchar(sc$qty) || nzchar(sc$sales) || nzchar(sc$price))) return(NULL)

      cards <- list()
      if (nzchar(sc$sales)) cards <- append(cards, list(kpi_card("Total Sales", scales::dollar(sum(df[[sc$sales]], na.rm = TRUE)))))
      if (nzchar(sc$qty)) cards <- append(cards, list(kpi_card("Items Sold", format(sum(df[[sc$qty]], na.rm = TRUE), big.mark = ","))))
      if (nzchar(sc$price)) cards <- append(cards, list(kpi_card("Average Price", scales::dollar(mean(df[[sc$price]], na.rm = TRUE)))))
      if (nzchar(sc$item) && nzchar(sc$qty)) {
        top_item <- df %>% dplyr::group_by(.data[[sc$item]]) %>% dplyr::summarise(q = sum(.data[[sc$qty]], na.rm = TRUE), .groups = "drop") %>% dplyr::arrange(dplyr::desc(q)) %>% head(1)
        if (nrow(top_item) == 1) cards <- append(cards, list(kpi_card("Top Item", as.character(top_item[[sc$item]][1]), paste("Qty:", format(top_item$q[1], big.mark = ",")))))
      }
      widths <- c(4, 4, 4, 3)
      per_row <- if (length(cards) >= 3) 3 else 2
      chunks <- split(cards, ceiling(seq_along(cards) / per_row))
      tagList(lapply(chunks, function(ch) {
        fluidRow(lapply(seq_along(ch), function(i) {
          column(widths[per_row], ch[[i]])
        }))
      }))
    })

    output$cat_plot <- renderPlot({
      safe_plot({
        s <- summary_info(); df <- s$df
        cat_cols <- names(df)[vapply(df, function(x) is.character(x) || is.factor(x), logical(1))]
        shiny::validate(shiny::need(length(cat_cols) > 0, "No categorical columns available."))

        col <- cat_cols[1]
        agg <- df %>%
          dplyr::mutate(.cat = as.character(.data[[col]])) %>%
          dplyr::count(.cat, name = "n") %>%
          dplyr::arrange(dplyr::desc(n)) %>%
          head(12)
        x_vals <- as.character(agg$.cat)
        y_vals <- as.numeric(agg$n)
        graphics::barplot(
          height = y_vals,
          names.arg = x_vals,
          col = "#2C5282",
          border = NA,
          las = 2,
          cex.names = 0.8,
          ylab = "Count",
          xlab = as.character(col)
        )
      })
    })

    output$hist_plot <- renderPlot({
      safe_plot({
        s <- summary_info(); df <- s$df
        num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
        shiny::validate(shiny::need(length(num_cols) > 0, "No numeric columns available."))

        col <- num_cols[1]
        x <- suppressWarnings(as.numeric(df[[col]]))
        x <- x[is.finite(x)]
        shiny::validate(shiny::need(length(x) > 0, "No valid numeric values available."))
        graphics::hist(
          x,
          breaks = 20,
          col = "#0F766E",
          border = "white",
          main = "",
          xlab = as.character(col),
          ylab = "Frequency"
        )
      })
    })

    output$trend_plot <- renderPlot({
      safe_plot({
        s <- summary_info(); df <- s$df
        dcols <- names(df)[vapply(df, function(x) inherits(x, "Date") || inherits(x, "POSIXt"), logical(1))]
        ncols <- names(df)[vapply(df, is.numeric, logical(1))]
        shiny::validate(shiny::need(length(dcols) > 0 && length(ncols) > 0, "Need at least one date and one numeric column."))

        dcol <- dcols[1]; ncol <- ncols[1]
        agg <- df %>% dplyr::mutate(.d = safe_date_vec(.data[[dcol]])) %>% dplyr::filter(!is.na(.d)) %>% dplyr::group_by(.d) %>% dplyr::summarise(v = mean(.data[[ncol]], na.rm = TRUE), .groups = "drop")
        shiny::validate(shiny::need(nrow(agg) > 1, "Insufficient date coverage for trend."))

        graphics::plot(
          x = agg$.d,
          y = agg$v,
          type = "b",
          pch = 16,
          col = "#2D3748",
          xlab = as.character(dcol),
          ylab = paste("Avg", as.character(ncol))
        )
      })
    })

    output$corr_plot <- renderPlot({
      safe_plot({
        s <- summary_info(); df <- s$df
        num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
        shiny::validate(shiny::need(length(num_cols) >= 2, "Need at least two numeric columns for correlation."))

        cmat <- stats::cor(df[, num_cols, drop = FALSE], use = "pairwise.complete.obs")
        graphics::image(
          1:ncol(cmat),
          1:nrow(cmat),
          t(cmat[nrow(cmat):1, , drop = FALSE]),
          col = colorRampPalette(c("#f7fafc", "#90aec0", "#2c5282"))(100),
          xaxt = "n",
          yaxt = "n",
          xlab = "",
          ylab = ""
        )
        graphics::axis(1, at = 1:ncol(cmat), labels = colnames(cmat), las = 2, cex.axis = 0.8)
        graphics::axis(2, at = 1:nrow(cmat), labels = rev(rownames(cmat)), las = 2, cex.axis = 0.8)
      })
    })

    output$items_plot <- renderPlot({
      safe_plot({
        s <- summary_info(); df <- s$df; sc <- detect_sales_schema()
        shiny::validate(shiny::need(!is.na(sc$item) && !is.na(sc$qty) && nzchar(sc$item) && nzchar(sc$qty), "Need item and quantity-like columns for this view."))
        agg <- df %>%
          dplyr::mutate(.item = as.character(.data[[sc$item]])) %>%
          dplyr::group_by(.item) %>%
          dplyr::summarise(qty = sum(.data[[sc$qty]], na.rm = TRUE), .groups = "drop") %>%
          dplyr::arrange(dplyr::desc(qty)) %>%
          head(12)
        graphics::barplot(
          height = agg$qty,
          names.arg = agg$.item,
          horiz = TRUE,
          col = "#2D3748",
          border = NA,
          las = 1,
          xlab = "Quantity Sold"
        )
      })
    })

    output$price_qty_plot <- renderPlot({
      safe_plot({
        s <- summary_info(); df <- s$df; sc <- detect_sales_schema()
        shiny::validate(shiny::need(!is.na(sc$price) && !is.na(sc$qty) && nzchar(sc$price) && nzchar(sc$qty), "Need price and quantity-like columns for this view."))
        x <- suppressWarnings(as.numeric(df[[sc$price]]))
        y <- suppressWarnings(as.numeric(df[[sc$qty]]))
        graphics::plot(
          x, y,
          pch = 16,
          col = "#0F766E",
          xlab = "Price",
          ylab = "Quantity"
        )
      })
    })

    output$insight_cards_ui <- renderUI({
      s <- summary_info()
      df <- s$df
      sc <- detect_sales_schema()
      cat_cols <- names(df)[vapply(df, function(x) is.character(x) || is.factor(x), logical(1))]
      num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
      date_cols <- names(df)[vapply(df, function(x) inherits(x, "Date") || inherits(x, "POSIXt"), logical(1))]

      can_trend <- FALSE
      if (length(date_cols) > 0 && length(num_cols) > 0) {
        dcol <- date_cols[1]
        ncol <- num_cols[1]
        agg_try <- try(
          df %>%
            dplyr::mutate(.d = safe_date_vec(.data[[dcol]])) %>%
            dplyr::filter(!is.na(.d)) %>%
            dplyr::group_by(.d) %>%
            dplyr::summarise(v = mean(.data[[ncol]], na.rm = TRUE), .groups = "drop"),
          silent = TRUE
        )
        can_trend <- !inherits(agg_try, "try-error") && nrow(agg_try) > 1
      }

      cards <- list()
      if (length(cat_cols) > 0) cards <- append(cards, list(card(card_header("Top Category Distribution"), card_body(plotOutput(ns("cat_plot"), height = "300px")))))
      if (length(num_cols) > 0) cards <- append(cards, list(card(card_header("Numeric Distribution"), card_body(plotOutput(ns("hist_plot"), height = "300px")))))
      if (can_trend) cards <- append(cards, list(card(card_header("Trend Over Time"), card_body(plotOutput(ns("trend_plot"), height = "300px")))))
      if (length(num_cols) >= 2) cards <- append(cards, list(card(card_header("Correlation Heatmap"), card_body(plotOutput(ns("corr_plot"), height = "300px")))))
      if (nzchar(sc$item) && nzchar(sc$qty)) cards <- append(cards, list(card(card_header("Top Items by Quantity"), card_body(plotOutput(ns("items_plot"), height = "300px")))))
      if (nzchar(sc$price) && nzchar(sc$qty)) cards <- append(cards, list(card(card_header("Price vs Quantity"), card_body(plotOutput(ns("price_qty_plot"), height = "300px")))))

      if (!length(cards)) return(div(class = "placeholder-zone", "No compatible columns found for auto insight charts yet."))

      rows <- split(cards, ceiling(seq_along(cards) / 2))
      tagList(lapply(rows, function(rw) {
        fluidRow(lapply(rw, function(cd) column(6, cd)))
      }))
    })
  })
}

# Backward-compatible aliases
insights_ui <- insightsUI
insights_server <- insightsServer
