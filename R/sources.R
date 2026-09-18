# Official BNR observations and ING end-of-quarter forecasts.
BNR_URL <- "https://curs.bnr.ro/nbrfxrates.xml"
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

load_live_sources <- function(force = FALSE, now = Sys.time(),
                              cache_dir = tools::R_user_dir("calcule-chirii", "cache"),
                              fetch = fetch_document) {
  if (!requireNamespace("xml2", quietly = TRUE))
    stop("Instalează pachetul R xml2 pentru date live: install.packages('xml2').", call. = FALSE)
  list(
    bnr = cached_source("bnr", BNR_URL, parse_bnr, 3600, 7 * 86400, force, cache_dir, now, fetch),
    ing = cached_source("ing", ING_URL, parse_ing, 86400, 30 * 86400, force, cache_dir, now, fetch)
  )
}

# Preserve the rent anniversary day; January 31 becomes February 28/29,
# then March 31. Current month means starting today, future months on day 1.
monthly_payment_dates <- function(start, count) {
  firsts <- seq(as.Date(format(start, "%Y-%m-01")), by = "month", length.out = count + 1)
  firsts[seq_len(count)] + pmin(as.integer(format(start, "%d")), as.integer(diff(firsts))) - 1
}

build_live_rates <- function(sources, start_month, today = bucharest_today()) {
  current_month <- format(today, "%Y-%m")
  start_month <- format(as.Date(start_month), "%Y-%m")
  if (is.na(start_month) || start_month < current_month)
    stop("Modul live începe în luna curentă sau în viitor. Pentru trecut, folosește CSV.", call. = FALSE)
  bnr <- sources$bnr$data
  bnr_age <- as.integer(today - bnr$date)
  if (bnr_age < 0 || bnr_age > 7)
    stop("Data cursului BNR nu este actuală (mai veche de 7 zile sau în viitor). Reîncearcă actualizarea.", call. = FALSE)
  start <- if (start_month == current_month) today else as.Date(paste0(start_month, "-01"))
  points <- sources$ing$data$points
  points <- points[points$date > today, , drop = FALSE]
  # The BNR rate is observed; intermediate future rates are estimates, not BNR.
  dates <- monthly_payment_dates(start, 120)
  horizon <- if (nrow(points)) max(points$date) else today
  dates <- dates[dates <= horizon]
  if (!length(dates)) stop("ING nu acoperă luna selectată. Alege altă lună sau folosește CSV.", call. = FALSE)
  values <- if (!nrow(points)) rep(bnr$eur_ron, length(dates)) else {
    approx(x = as.numeric(c(today, points$date)), y = c(bnr$eur_ron, points$eur_ron),
           xout = as.numeric(dates), rule = 1)$y
  }
  data.frame(month = format(dates, "%Y-%m"), eur_ron = values,
             payment_date = dates,
             rate_type = ifelse(dates == today, "BNR observat",
                        ifelse(dates %in% points$date, "Prognoză ING (sfârșit trimestru)",
                               "Estimare între repere ING")))
}
