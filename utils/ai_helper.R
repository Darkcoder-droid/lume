# ai_helper.R
# Optional AI helpers for lightweight summaries and suggestions.
options(lume_ai_last_error = "")
options(lume_ai_cache = list())
options(lume_ai_cooldown_until = as.POSIXct(NA))

ai_set_last_error <- function(msg = "") {
  options(lume_ai_last_error = as.character(msg))
  invisible(NULL)
}

ai_get_last_error <- function() {
  as.character(getOption("lume_ai_last_error", ""))
}

ai_markdown_html <- function(text) {
  txt <- paste(as.character(text %||% ""), collapse = "\n")
  # Escape raw HTML first, then render markdown for safer display in UI.
  safe_txt <- htmltools::htmlEscape(txt)
  htmltools::HTML(markdown::markdownToHTML(text = safe_txt, fragment.only = TRUE))
}

ai_in_cooldown <- function() {
  until <- getOption("lume_ai_cooldown_until", as.POSIXct(NA))
  isTRUE(!is.na(until) && Sys.time() < until)
}

ai_set_cooldown <- function(seconds = 45) {
  options(lume_ai_cooldown_until = Sys.time() + as.numeric(seconds))
  invisible(NULL)
}

ai_cache_get <- function(key) {
  cache <- getOption("lume_ai_cache", list())
  val <- cache[[key]]
  if (is.null(val)) return(NULL)
  if (is.null(val$expires) || Sys.time() > val$expires) return(NULL)
  val$text
}

ai_cache_set <- function(key, text, ttl_sec = 120) {
  cache <- getOption("lume_ai_cache", list())
  cache[[key]] <- list(text = as.character(text), expires = Sys.time() + as.numeric(ttl_sec))
  options(lume_ai_cache = cache)
  invisible(NULL)
}

ai_is_enabled <- function() {
  provider <- tolower(Sys.getenv("LUME_AI_PROVIDER", "openai"))
  if (provider == "gemini") return(nzchar(Sys.getenv("LUME_GEMINI_API_KEY", "")))
  if (provider == "groq") return(nzchar(Sys.getenv("LUME_GROQ_API_KEY", "")))
  nzchar(Sys.getenv("LUME_OPENAI_API_KEY", ""))
}

ai_get_model <- function() {
  provider <- tolower(Sys.getenv("LUME_AI_PROVIDER", "openai"))
  default_model <- if (provider == "gemini") {
    "gemini-2.0-flash"
  } else if (provider == "groq") {
    "llama-3.1-8b-instant"
  } else {
    "gpt-4o-mini"
  }
  env_name <- if (provider == "gemini") {
    "LUME_GEMINI_MODEL"
  } else if (provider == "groq") {
    "LUME_GROQ_MODEL"
  } else {
    "LUME_OPENAI_MODEL"
  }
  m <- Sys.getenv(env_name, default_model)
  if (!nzchar(m)) default_model else m
}

ai_extract_text <- function(resp) {
  if (is.list(resp) && !is.null(resp$output_text) && nzchar(as.character(resp$output_text))) {
    return(as.character(resp$output_text))
  }

  if (is.list(resp) && !is.null(resp$output)) {
    out <- unlist(lapply(resp$output, function(item) {
      if (is.null(item$content)) return(NA_character_)
      unlist(lapply(item$content, function(cn) {
        if (!is.null(cn$text)) as.character(cn$text) else NA_character_
      }))
    }), use.names = FALSE)

    out <- out[is.finite(nchar(out))]
    if (length(out)) return(paste(out, collapse = "\n"))
  }

  ""
}

ai_call <- function(system_prompt, user_prompt, max_output_tokens = 280) {
  ai_set_last_error("")
  if (!ai_is_enabled()) return(NULL)
  if (ai_in_cooldown()) {
    wait_s <- ceiling(as.numeric(difftime(getOption("lume_ai_cooldown_until"), Sys.time(), units = "secs")))
    ai_set_last_error(paste0("AI temporarily paused due to quota throttling. Retry in ~", max(1, wait_s), "s."))
    return(NULL)
  }

  provider <- tolower(Sys.getenv("LUME_AI_PROVIDER", "openai"))
  model <- ai_get_model()
  cache_key <- paste(provider, model, system_prompt, user_prompt, as.integer(max_output_tokens), sep = "||")
  cached <- ai_cache_get(cache_key)
  if (!is.null(cached)) return(cached)

  if (provider == "gemini") {
    key <- Sys.getenv("LUME_GEMINI_API_KEY", "")
    if (!nzchar(key)) {
      ai_set_last_error("Gemini API key is missing.")
      return(NULL)
    }

    body <- list(
      system_instruction = list(parts = list(list(text = system_prompt))),
      contents = list(
        list(role = "user", parts = list(list(text = user_prompt)))
      ),
      generationConfig = list(maxOutputTokens = as.integer(max_output_tokens))
    )

    resp <- tryCatch(
      httr::POST(
        url = paste0("https://generativelanguage.googleapis.com/v1beta/models/", model, ":generateContent?key=", key),
        httr::add_headers(`Content-Type` = "application/json"),
        body = jsonlite::toJSON(body, auto_unbox = TRUE, null = "null"),
        encode = "raw",
        httr::timeout(20)
      ),
      error = function(e) {
        ai_set_last_error(paste("Gemini request error:", e$message))
        NULL
      }
    )

    if (is.null(resp)) return(NULL)
    if (httr::status_code(resp) >= 300) {
      body_txt <- tryCatch(httr::content(resp, as = "text", encoding = "UTF-8"), error = function(e) "")
      if (httr::status_code(resp) == 429) {
        retry_secs <- suppressWarnings(as.numeric(sub(".*retry in ([0-9]+(\\.[0-9]+)?)s.*", "\\1", tolower(body_txt))))
        if (!is.finite(retry_secs)) retry_secs <- 45
        ai_set_cooldown(retry_secs)
      }
      ai_set_last_error(paste0("Gemini API HTTP ", httr::status_code(resp), if (nzchar(body_txt)) paste0(": ", body_txt) else ""))
      return(NULL)
    }
    parsed <- tryCatch(httr::content(resp, as = "parsed", type = "application/json"), error = function(e) NULL)
    if (is.null(parsed)) {
      ai_set_last_error("Gemini response parse failure.")
      return(NULL)
    }

    txt <- tryCatch({
      cand <- parsed$candidates[[1]]
      parts <- cand$content$parts
      out <- unlist(lapply(parts, function(x) if (!is.null(x$text)) as.character(x$text) else NA_character_))
      out <- out[!is.na(out)]
      paste(out, collapse = "\n")
    }, error = function(e) "")

    if (!nzchar(txt)) {
      ai_set_last_error("Gemini returned empty text.")
      return(NULL)
    }
    ai_cache_set(cache_key, txt, ttl_sec = 120)
    return(txt)
  }

  if (provider == "groq") {
    key <- Sys.getenv("LUME_GROQ_API_KEY", "")
    if (!nzchar(key)) {
      ai_set_last_error("Groq API key is missing.")
      return(NULL)
    }

    body <- list(
      model = model,
      messages = list(
        list(role = "system", content = system_prompt),
        list(role = "user", content = user_prompt)
      ),
      max_tokens = as.integer(max_output_tokens),
      temperature = 0.2
    )

    resp <- tryCatch(
      httr::POST(
        url = "https://api.groq.com/openai/v1/chat/completions",
        httr::add_headers(
          Authorization = paste("Bearer", key),
          `Content-Type` = "application/json"
        ),
        body = jsonlite::toJSON(body, auto_unbox = TRUE, null = "null"),
        encode = "raw",
        httr::timeout(20)
      ),
      error = function(e) {
        ai_set_last_error(paste("Groq request error:", e$message))
        NULL
      }
    )

    if (is.null(resp)) return(NULL)
    if (httr::status_code(resp) >= 300) {
      body_txt <- tryCatch(httr::content(resp, as = "text", encoding = "UTF-8"), error = function(e) "")
      if (httr::status_code(resp) == 429) ai_set_cooldown(45)
      ai_set_last_error(paste0("Groq API HTTP ", httr::status_code(resp), if (nzchar(body_txt)) paste0(": ", body_txt) else ""))
      return(NULL)
    }

    parsed <- tryCatch(httr::content(resp, as = "parsed", type = "application/json"), error = function(e) NULL)
    if (is.null(parsed)) {
      ai_set_last_error("Groq response parse failure.")
      return(NULL)
    }

    txt <- tryCatch(as.character(parsed$choices[[1]]$message$content), error = function(e) "")
    if (!nzchar(txt)) {
      ai_set_last_error("Groq returned empty text.")
      return(NULL)
    }
    ai_cache_set(cache_key, txt, ttl_sec = 120)
    return(txt)
  }

  key <- Sys.getenv("LUME_OPENAI_API_KEY", "")
  if (!nzchar(key)) {
    ai_set_last_error("OpenAI API key is missing.")
    return(NULL)
  }

  body <- list(
    model = model,
    input = list(
      list(role = "system", content = system_prompt),
      list(role = "user", content = user_prompt)
    ),
    max_output_tokens = as.integer(max_output_tokens)
  )

  resp <- tryCatch(
    httr::POST(
      url = "https://api.openai.com/v1/responses",
      httr::add_headers(
        Authorization = paste("Bearer", key),
        `Content-Type` = "application/json"
      ),
      body = jsonlite::toJSON(body, auto_unbox = TRUE, null = "null"),
      encode = "raw",
      httr::timeout(20)
    ),
    error = function(e) {
      ai_set_last_error(paste("OpenAI request error:", e$message))
      NULL
    }
  )

  if (is.null(resp)) return(NULL)
  if (httr::status_code(resp) >= 300) {
    body_txt <- tryCatch(httr::content(resp, as = "text", encoding = "UTF-8"), error = function(e) "")
    if (httr::status_code(resp) == 429) ai_set_cooldown(45)
    ai_set_last_error(paste0("OpenAI API HTTP ", httr::status_code(resp), if (nzchar(body_txt)) paste0(": ", body_txt) else ""))
    return(NULL)
  }

  parsed <- tryCatch(httr::content(resp, as = "parsed", type = "application/json"), error = function(e) NULL)
  if (is.null(parsed)) {
    ai_set_last_error("OpenAI response parse failure.")
    return(NULL)
  }

  txt <- ai_extract_text(parsed)
  if (!nzchar(txt)) {
    ai_set_last_error("OpenAI returned empty text.")
    return(NULL)
  }
  ai_cache_set(cache_key, txt, ttl_sec = 120)
  txt
}

ai_upload_summary <- function(df, meta = NULL) {
  req(is.data.frame(df))

  n <- nrow(df); p <- ncol(df)
  num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
  cat_cols <- names(df)[vapply(df, function(x) is.character(x) || is.factor(x), logical(1))]
  date_cols <- names(df)[vapply(df, function(x) inherits(x, "Date") || inherits(x, "POSIXt"), logical(1))]

  prof <- list(
    rows = n,
    cols = p,
    numeric = num_cols,
    categorical = cat_cols,
    date = date_cols,
    missing_pct = round(sum(is.na(df)) / max(1, n * p) * 100, 2)
  )

  if (!ai_is_enabled()) {
    return(sprintf("AI disabled. Dataset profile: %s rows, %s columns, %.2f%% missing.", format(n, big.mark = ","), p, prof$missing_pct))
  }

  prompt <- paste0(
    "Dataset profile JSON:\n", jsonlite::toJSON(prof, auto_unbox = TRUE),
    "\nReturn 3 concise bullets: what dataset appears to be, likely key columns, first chart suggestion."
  )

  txt <- ai_call(
    "You are a concise data analyst assistant. Never invent column names not present.",
    prompt,
    max_output_tokens = 220
  )

  if (is.null(txt) || !nzchar(txt)) {
    err <- ai_get_last_error()
    base <- sprintf("AI summary unavailable right now. Dataset profile: %s rows, %s columns.", format(n, big.mark = ","), p)
    return(if (nzchar(err)) paste(base, "Reason:", err) else base)
  }

  txt
}

ai_dataset_summary <- function(df) {
  req(is.data.frame(df))

  n <- nrow(df); p <- ncol(df)
  num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
  miss <- round(sum(is.na(df)) / max(1, n * p) * 100, 2)

  if (!ai_is_enabled()) {
    return(sprintf("AI disabled. Snapshot: %s rows, %s columns, %s numeric columns, %.2f%% missing.", format(n, big.mark = ","), p, length(num_cols), miss))
  }

  summary_payload <- list(
    rows = n,
    cols = p,
    numeric_columns = head(num_cols, 12),
    missing_pct = miss
  )

  prompt <- paste0(
    "Dataset summary JSON:\n", jsonlite::toJSON(summary_payload, auto_unbox = TRUE),
    "\nWrite a short executive summary (4-6 lines) and end with 'Next best step: ...'."
  )

  txt <- ai_call(
    "You write precise, business-friendly dataset summaries grounded only in the input.",
    prompt,
    max_output_tokens = 260
  )

  if (is.null(txt) || !nzchar(txt)) {
    err <- ai_get_last_error()
    return(if (nzchar(err)) paste("AI summary unavailable right now.", "Reason:", err) else "AI summary unavailable right now.")
  }
  txt
}

ai_dashboard_note <- function(df, cards = list()) {
  req(is.data.frame(df))

  card_types <- if (length(cards)) vapply(cards, function(x) {
    if (!is.null(x$type)) as.character(x$type) else ""
  }, character(1)) else character(0)
  payload <- list(
    rows = nrow(df),
    cols = ncol(df),
    card_counts = as.list(table(card_types))
  )

  if (!ai_is_enabled()) {
    return("AI disabled. You can still add manual notes from the observed KPI and chart patterns.")
  }

  prompt <- paste0(
    "Dashboard context JSON:\n", jsonlite::toJSON(payload, auto_unbox = TRUE),
    "\nGenerate markdown with sections: Summary, Risks, Recommended Actions. Keep under 120 words."
  )

  txt <- ai_call(
    "You generate concise dashboard annotation markdown for analysts.",
    prompt,
    max_output_tokens = 220
  )

  if (is.null(txt) || !nzchar(txt)) {
    err <- ai_get_last_error()
    return(if (nzchar(err)) paste("AI note unavailable right now.", "Reason:", err) else "AI note unavailable right now.")
  }
  txt
}

ai_transform_suggestions <- function(df, steps = list()) {
  req(is.data.frame(df))

  n <- nrow(df); p <- ncol(df)
  num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
  cat_cols <- names(df)[vapply(df, function(x) is.character(x) || is.factor(x), logical(1))]
  miss <- round(sum(is.na(df)) / max(1, n * p) * 100, 2)
  step_types <- if (length(steps)) vapply(steps, function(s) {
    if (!is.null(s$type)) as.character(s$type) else ""
  }, character(1)) else character(0)

  payload <- list(
    rows = n,
    cols = p,
    numeric_cols = head(num_cols, 15),
    categorical_cols = head(cat_cols, 15),
    missing_pct = miss,
    existing_steps = as.list(table(step_types))
  )

  if (!ai_is_enabled()) {
    return(
      paste(
        "### AI Suggestions (Disabled)",
        "",
        "- Use **Filtering** to isolate the segment you care about first.",
        "- If missing values are present, apply **Data Cleaning** before aggregation.",
        "- Create one calculated metric (for example ratio or margin) before dashboarding.",
        sep = "\n"
      )
    )
  }

  prompt <- paste0(
    "Transformation context JSON:\n", jsonlite::toJSON(payload, auto_unbox = TRUE),
    "\nReturn markdown with sections:",
    "\n## Suggested Steps",
    "\n- 3 to 5 suggested transformations in order",
    "\n## Why",
    "\n- short rationale",
    "\n## Safety Checks",
    "\n- checks user should run before apply",
    "\nDo not output code."
  )

  txt <- ai_call(
    "You are a careful data transformation assistant. Suggest safe, incremental, user-approved steps only.",
    prompt,
    max_output_tokens = 320
  )

  if (is.null(txt) || !nzchar(txt)) {
    err <- ai_get_last_error()
    return(if (nzchar(err)) paste("AI transform suggestions unavailable.\n\nReason:", err) else "AI transform suggestions unavailable.")
  }
  txt
}
