# Official BNR observations and ING end-of-quarter forecasts.
BNR_URL <- "https://curs.bnr.ro/nbrfxrates.xml"
BNR_ANNUAL_URL <- "https://curs.bnr.ro/files/xml/years/nbrfxrates%d.xml"
ING_URL <- "https://think.ing.com/forecasts/"

bucharest_today <- function(now = Sys.time()) {
  as.Date(format(now, tz = "Europe/Bucharest", format = "%Y-%m-%d"))
}

fetch_document <- function(url) {
  path <- tempfile()
  on.exit(unlink(path), add = TRUE)
  old <- options(timeout = 20, HTTPUserAgent = "CalculeChirii/1.0 (R; public exchange-rate data)")
  on.exit(options(old), add = TRUE)
  status <- suppressWarnings(utils::download.file(url, path, method = "libcurl", quiet = TRUE, mode = "wb"))
  if (status != 0 || !file.exists(path) || file.info(path)$size == 0)
    stop("Sursa nu a returnat date.", call. = FALSE)
  readBin(path, "raw", n = file.info(path)$size)
}

parse_bnr <- function(document) {
  doc <- xml2::read_xml(document, options = "NONET")
  currency <- xml2::xml_text(xml2::xml_find_first(doc, "//*[local-name()='OrigCurrency']"))
  if (!identical(currency, "RON")) stop("Moneda de bază BNR nu este RON.", call. = FALSE)
  nodes <- xml2::xml_find_all(doc, "//*[local-name()='Cube']/*[local-name()='Rate' and @currency='EUR']")
  dates <- as.Date(xml2::xml_attr(xml2::xml_parent(nodes), "date"), format = "%Y-%m-%d")
  rates <- suppressWarnings(as.numeric(xml2::xml_text(nodes)))
  multiplier <- xml2::xml_attr(nodes, "multiplier")
  multiplier[is.na(multiplier)] <- "1"
  multiplier <- suppressWarnings(as.numeric(multiplier))
  if (!length(rates) || anyNA(dates) || anyDuplicated(dates) ||
      any(!is.finite(rates) | rates <= 0) || any(!is.finite(multiplier) | multiplier <= 0))
    stop("Răspunsul BNR nu conține cursuri EUR valide.", call. = FALSE)
  i <- which.max(dates)
  list(date = dates[i], eur_ron = rates[i] / multiplier[i])
}

parse_ing <- function(document) {
  doc <- xml2::read_html(document, options = c("RECOVER", "NONET"))
  fx <- xml2::xml_find_first(doc, "//*[@id='fx']")
  row <- xml2::xml_find_all(fx, ".//tr[td[normalize-space(.)='EUR/RON']]")
  if (length(row) != 1) stop("Tabelul ING EUR/RON nu a fost găsit sau este ambiguu.", call. = FALSE)
  # Each region has its own thead inside the FX table. Never use GDP/rate headers.
  header <- xml2::xml_find_first(row, "ancestor::tbody/preceding-sibling::thead[1]/tr")
  headers <- trimws(xml2::xml_text(xml2::xml_find_all(header, "./th|./td")))
  cells <- trimws(xml2::xml_text(xml2::xml_find_all(row, "./td")))
  idx <- which(grepl("^[1-4]Q[0-9]{2}F$", headers))
  if (!length(idx) || length(headers) != length(cells) || !any(grepl("eop", headers, fixed = TRUE)))
    stop("Formatul prognozelor trimestriale ING s-a schimbat.", call. = FALSE)
  quarter <- as.integer(substr(headers[idx], 1, 1))
  year <- 2000L + as.integer(substr(headers[idx], 3, 4))
  end_month <- quarter * 3L
  next_month <- as.Date(sprintf("%04d-%02d-01", year + (end_month == 12), end_month %% 12 + 1))
  rates <- suppressWarnings(as.numeric(cells[idx]))
  if (any(!is.finite(rates) | rates <= 0) || anyDuplicated(next_month))
    stop("ING a publicat cursuri lipsă sau invalide.", call. = FALSE)
  points <- data.frame(date = next_month - 1, eur_ron = rates)
  points <- points[order(points$date), , drop = FALSE]
  label <- trimws(xml2::xml_text(xml2::xml_find_first(fx, "./p[contains(., 'Last updated')]")))
  if (is.na(label) || !nzchar(label)) label <- "Data actualizării nu este publicată"
  list(points = points, published_label = label)
}

# Parse a BNR annual XML file (one year of daily rates).
# Returns one EUR/RON rate per month (first available trading day).
parse_bnr_annual <- function(document) {
  doc <- xml2::read_xml(document, options = "NONET")
  nodes <- xml2::xml_find_all(doc, "//*[local-name()='Cube']/*[local-name()='Rate' and @currency='EUR']")
  if (!length(nodes)) stop("XML-ul anual BNR nu conține cursuri EUR.", call. = FALSE)
  dates <- as.Date(xml2::xml_attr(xml2::xml_parent(nodes), "date"), format = "%Y-%m-%d")
  rates <- suppressWarnings(as.numeric(xml2::xml_text(nodes)))
  multiplier <- xml2::xml_attr(nodes, "multiplier")
  multiplier[is.na(multiplier)] <- "1"
  multiplier <- suppressWarnings(as.numeric(multiplier))
  if (anyNA(dates) || any(!is.finite(rates) | rates <= 0) ||
      any(!is.finite(multiplier) | multiplier <= 0))
    stop("XML-ul anual BNR conține cursuri EUR invalide.", call. = FALSE)
  df <- data.frame(date = dates, eur_ron = rates / multiplier, stringsAsFactors = FALSE)
  df <- df[order(df$date), ]
  df$month <- format(df$date, "%Y-%m")
  df <- df[!duplicated(df$month), c("month", "eur_ron", "date"), drop = FALSE]
  rownames(df) <- NULL
  df
}

# Store only successfully parsed responses. Atomic replacement preserves the last
# good response across failures; the directory can be shared by Shiny sessions.
cached_source <- function(key, url, parser, ttl, max_stale, force = FALSE,
                          cache_dir = tools::R_user_dir("calcule-chirii", "cache"),
                          now = Sys.time(), fetch = fetch_document) {
  path <- file.path(cache_dir, paste0(key, "-v1.rds"))
  cached <- tryCatch(suppressWarnings(readRDS(path)), error = function(e) NULL)
  valid <- is.list(cached) && identical(cached$url, url) &&
    inherits(cached$fetched_at, "POSIXct") && length(cached$fetched_at) == 1 &&
    is.finite(as.numeric(cached$fetched_at)) && !is.null(cached$data)
  age <- if (valid) as.numeric(difftime(now, cached$fetched_at, units = "secs")) else Inf
  if (!is.finite(age) || age < 0) age <- Inf
  result <- function(entry, stale = FALSE, message = NULL) {
    c(entry, list(stale = stale, notice = message))
  }
  if (!force && age < ttl) return(result(cached))
  attempt <- tryCatch(list(data = parser(fetch(url)), url = url, fetched_at = now), error = identity)
  if (inherits(attempt, "error")) {
    if (age <= max_stale) return(result(cached, TRUE, paste0("Actualizarea a eșuat; folosim copia salvată: ", conditionMessage(attempt))))
    stop(paste0("Nu putem actualiza ", key, ": ", conditionMessage(attempt),
                ". Nu există un cache suficient de recent. Poți reîncerca sau selecta CSV."), call. = FALSE)
  }
  write_error <- tryCatch({
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    tmp <- tempfile(pattern = paste0(key, "-"), tmpdir = cache_dir)
    on.exit(unlink(tmp), add = TRUE)
    saveRDS(attempt, tmp)
    if (!file.rename(tmp, path)) stop("Nu se poate salva cache-ul.")
    NULL
  }, error = function(e) "Datele au fost preluate, dar cache-ul nu a putut fi salvat.")
  result(attempt, message = write_error)
}

# Load BNR monthly history. Tries the cached (auto-updated) version first, then
# the shipped bootstrap CSV. Validates that all rates are finite and positive.
load_bnr_history <- function(cache_dir = tools::R_user_dir("calcule-chirii", "cache"),
                              shipped_path = "R/bnr_history.csv") {
  cached_path <- file.path(cache_dir, "bnr_history.csv")
  path <- if (file.exists(cached_path)) {
    cached_path
  } else {
    candidates <- c(
      shipped_path,
      "bnr_history.csv",
      file.path("..", shipped_path),
      file.path("..", "bnr_history.csv"),
      file.path(tools::R_user_dir("calcule-chirii", "data"), "bnr_history.csv")
    )
    found <- candidates[nzchar(candidates) & file.exists(candidates)]
    if (length(found) > 0) found[1] else NULL
  }
  if (is.null(path) || !file.exists(path)) {
    stop("Fișierul cu date istorice BNR nu a fost găsit.", call. = FALSE)
  }
  df <- utils::read.csv(path, stringsAsFactors = FALSE)
  if (!all(c("month", "eur_ron", "date") %in% names(df)) || nrow(df) == 0)
    stop("Fișierul cu date istorice BNR este invalid sau gol.", call. = FALSE)
  df$eur_ron <- suppressWarnings(as.numeric(df$eur_ron))
  df$date <- as.Date(df$date)
  if (any(!is.finite(df$eur_ron) | df$eur_ron <= 0) || anyNA(df$date))
    stop("Fișierul cu date istorice BNR conține cursuri lipsă sau negative.", call. = FALSE)
  df[order(df$month), c("month", "eur_ron", "date"), drop = FALSE]
}

# Fetch BNR annual XMLs for any months missing from history (up to last month)
# and save the merged result to the cache directory.  Failures are non-fatal;
# the best available history is always returned.
update_bnr_history <- function(history, today, cache_dir, fetch) {
  prev_month <- format(seq(as.Date(format(today, "%Y-%m-01")),
                           by = "-1 month", length.out = 2)[2], "%Y-%m")
  if (!is.null(history) && nrow(history) > 0 && max(history$month) >= prev_month)
    return(history)
  last_year <- if (!is.null(history) && nrow(history) > 0) {
    as.integer(substr(max(history$month), 1, 4))
  } else 2018L
  current_year <- as.integer(format(today, "%Y"))
  new_rows <- list()
  for (y in seq(last_year, current_year)) {
    url <- sprintf(BNR_ANNUAL_URL, y)
    annual <- tryCatch(parse_bnr_annual(fetch(url)), error = function(e) NULL)
    if (!is.null(annual)) new_rows[[length(new_rows) + 1L]] <- annual
  }
  if (!length(new_rows)) return(history)
  fresh <- do.call(rbind, new_rows)
  fresh <- fresh[!duplicated(fresh$month), , drop = FALSE]
  if (!is.null(history) && nrow(history) > 0) {
    keep <- history[!(history$month %in% fresh$month), , drop = FALSE]
    fresh <- rbind(keep, fresh)
  }
  fresh <- fresh[!duplicated(fresh$month), , drop = FALSE]
  fresh <- fresh[order(fresh$month), c("month", "eur_ron", "date"), drop = FALSE]
  rownames(fresh) <- NULL
  tryCatch({
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    path <- file.path(cache_dir, "bnr_history.csv")
    tmp <- tempfile(pattern = "bnr_history-", tmpdir = cache_dir, fileext = ".csv")
    on.exit(unlink(tmp), add = TRUE)
    utils::write.csv(fresh, tmp, row.names = FALSE)
    if (!file.rename(tmp, path)) stop("rename failed")
  }, error = function(e) NULL)
  fresh
}

load_live_sources <- function(force = FALSE, now = Sys.time(),
                              cache_dir = tools::R_user_dir("calcule-chirii", "cache"),
                              fetch = fetch_document) {
  if (!requireNamespace("xml2", quietly = TRUE))
    stop("Instalează pachetul R xml2 pentru date live: install.packages('xml2').", call. = FALSE)
  history <- tryCatch(load_bnr_history(cache_dir), error = function(e) NULL)
  today <- bucharest_today(now)
  history <- tryCatch(update_bnr_history(history, today, cache_dir, fetch),
                      error = function(e) history)
  list(
    bnr = cached_source("bnr", BNR_URL, parse_bnr, 3600, 7 * 86400, force, cache_dir, now, fetch),
    ing = cached_source("ing", ING_URL, parse_ing, 86400, 30 * 86400, force, cache_dir, now, fetch),
    history = history
  )
}

# Preserve the rent anniversary day; January 31 becomes February 28/29,
# then March 31. Current month means starting today, future months on day 1.
monthly_payment_dates <- function(start, count) {
  firsts <- seq(as.Date(format(start, "%Y-%m-01")), by = "month", length.out = count + 1)
  firsts[seq_len(count)] + pmin(as.integer(format(start, "%d")), as.integer(diff(firsts))) - 1
}

build_live_rates <- function(sources, start_month, today = bucharest_today()) {
  history <- if (!is.null(sources$history)) sources$history else load_bnr_history()
  current_month <- format(today, "%Y-%m")
  start_month <- format(as.Date(start_month), "%Y-%m")
  if (is.na(start_month))
    stop("Luna de început este invalidă.", call. = FALSE)

  min_hist <- if (!is.null(history) && nrow(history) > 0) min(history$month) else current_month
  if (start_month < min_hist)
    stop(sprintf("Datele istorice BNR sunt disponibile începând cu %s. Alege altă lună sau folosește CSV.", min_hist), call. = FALSE)

  bnr <- sources$bnr$data
  bnr_age <- as.integer(today - bnr$date)
  if (bnr_age < 0 || bnr_age > 7)
    stop("Data cursului BNR nu este actuală (mai veche de 7 zile sau în viitor). Reîncearcă actualizarea.", call. = FALSE)

  points <- sources$ing$data$points
  points <- points[points$date > today, , drop = FALSE]
  horizon <- if (nrow(points)) max(points$date) else today

  if (!nrow(points) && as.Date(paste0(start_month, "-01")) > today)
    stop("ING nu acoperă luna selectată. Alege altă lună sau folosește CSV.", call. = FALSE)

  # Orizont maxim de selecție: până la 3 ani în viitor
  max_horizon <- seq(as.Date(format(today, "%Y-%m-01")), by = "36 months", length.out = 2)[2] - 1
  if (as.Date(paste0(start_month, "-01")) > max_horizon)
    stop("Orizontul maxim de prognoză este de 3 ani (36 luni). Alege altă lună sau folosește CSV.", call. = FALSE)

  start <- if (start_month == current_month) today else as.Date(paste0(start_month, "-01"))

  # Generăm cel puțin 36 de luni de la start_month (sau până la today + 36 luni)
  months_to_today <- max(0L, as.integer(ceiling(as.numeric(difftime(today, start, units = "days")) / 28)))
  n_months <- max(36L, months_to_today + 36L)
  dates <- monthly_payment_dates(start, n_months)
  if (!nrow(points)) dates <- dates[dates <= today]

  months <- format(dates, "%Y-%m")
  values <- numeric(length(dates))
  rate_type <- character(length(dates))
  is_forecast <- logical(length(dates))

  # Viitor (prognoză ING + extrapolare trend până la 3 ani)
  future_mask <- months > current_month
  if (any(future_mask)) {
    fut_dates <- dates[future_mask]
    fut_vals <- numeric(length(fut_dates))
    fut_types <- character(length(fut_dates))

    # Segment 1: În cadrul orizontului ING (interpolare exactă)
    ing_mask <- fut_dates <= horizon
    if (any(ing_mask)) {
      ing_dates <- fut_dates[ing_mask]
      ing_vals <- if (!nrow(points)) rep(bnr$eur_ron, length(ing_dates)) else {
        approx(x = as.numeric(c(today, points$date)), y = c(bnr$eur_ron, points$eur_ron),
               xout = as.numeric(ing_dates), rule = 1)$y
      }
      fut_vals[ing_mask] <- ing_vals
      fut_types[ing_mask] <- ifelse(ing_dates %in% points$date,
                                    "Prognoză ING (sfârșit trimestru)",
                                    "Estimare între repere ING")
    }

    # Segment 2: După orizontul ING (extrapolare trend 3 ani)
    ext_mask <- fut_dates > horizon
    if (any(ext_mask)) {
      ext_dates <- fut_dates[ext_mask]
      all_pts <- rbind(data.frame(date = today, eur_ron = bnr$eur_ron), points)
      slope <- if (nrow(all_pts) >= 2) {
        fit <- stats::lm(eur_ron ~ as.numeric(date), data = all_pts)
        max(0, stats::coef(fit)[2])
      } else {
        0.00015 # ~0.05 RON / an (~1% anual, media istorică BNR)
      }
      base_rate <- if (nrow(points)) points$eur_ron[nrow(points)] else bnr$eur_ron
      fut_vals[ext_mask] <- base_rate + slope * as.numeric(ext_dates - horizon)
      fut_types[ext_mask] <- "Prognoză extinsă (trend 3 ani)"
    }

    values[future_mask] <- fut_vals
    rate_type[future_mask] <- fut_types
    is_forecast[future_mask] <- TRUE
  }

  # Luna curentă
  curr_mask <- months == current_month
  if (any(curr_mask)) {
    values[curr_mask] <- bnr$eur_ron
    rate_type[curr_mask] <- ifelse(dates[curr_mask] == today, "BNR observat", "BNR observat (luna curentă)")
    is_forecast[curr_mask] <- FALSE
  }

  # Trecut (istoric oficial BNR)
  past_mask <- months < current_month
  if (any(past_mask)) {
    past_months <- months[past_mask]
    hist_idx <- match(past_months, history$month)
    if (anyNA(hist_idx)) {
      missing_m <- past_months[is.na(hist_idx)]
      stop(sprintf("Lipsesc date istorice BNR pentru lunile: %s.", paste(missing_m, collapse = ", ")), call. = FALSE)
    }
    values[past_mask] <- history$eur_ron[hist_idx]
    rate_type[past_mask] <- "BNR istoric (observat)"
    is_forecast[past_mask] <- FALSE
  }

  data.frame(month = months, eur_ron = values,
             payment_date = dates,
             rate_type = rate_type,
             is_forecast = is_forecast,
             stringsAsFactors = FALSE)
}

# Build the full available timeline from the earliest historical month to the
# last ING forecast.  Used by date_curs() to populate the date picker range;
# the actual calculation uses build_live_rates() with the user-selected start.
build_live_timeline <- function(sources, today = bucharest_today()) {
  history <- if (!is.null(sources$history)) sources$history else load_bnr_history()
  first_month <- if (!is.null(history) && nrow(history) > 0) min(history$month) else format(today, "%Y-%m")
  build_live_rates(sources, as.Date(paste0(first_month, "-01")), today)
}
