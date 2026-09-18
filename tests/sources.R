# Offline regression checks for the source adapters, caching and live UI.
expect_error <- function(expr, pattern = NULL) {
  error <- tryCatch(force(expr), error = identity)
  check(inherits(error, "error"), TRUE)
  if (!is.null(pattern)) check(grepl(pattern, conditionMessage(error)), TRUE)
}
xml_fixture <- '<DataSet xmlns="https://www.bnr.ro/xsd"><Body><OrigCurrency>RON</OrigCurrency><Cube date="2026-09-16"><Rate currency="EUR">5.26</Rate></Cube><Cube date="2026-09-17"><Rate currency="USD">4.5</Rate><Rate currency="EUR">5.2601</Rate></Cube></Body></DataSet>'
html_fixture <- '<html><div id="growth"><table><tr><td>EUR/RON</td><td>999</td></tr></table></div><div id="fx"><p>Last updated: 7 September</p><table><thead><tr><th>G10 (eop)</th><th></th><th>1Q25F</th></tr></thead><tbody><tr><td>Euro</td><td>EUR/USD</td><td>1.10</td></tr></tbody><thead><tr><th>EMEA (eop)</th><th></th><th>3Q26F</th><th>4Q26F</th><th>1Q27F</th></tr></thead><tbody><tr><td>Romania</td><td>EUR/RON</td><td>5.3</td><td>5.6</td><td>5.9</td></tr></tbody></table></div></html>'
bnr <- parse_bnr(xml_fixture)
ing <- parse_ing(html_fixture)
check(bnr$date, as.Date("2026-09-17"))
check(bnr$eur_ron, 5.2601)
check(ing$points$date, as.Date(c("2026-09-30", "2026-12-31", "2027-03-31")))
check(ing$points$eur_ron, c(5.3, 5.6, 5.9))
check(ing$published_label, "Last updated: 7 September")
check(parse_bnr(gsub('currency="EUR">5.2601', 'currency="EUR" multiplier="100">526.01', xml_fixture))$eur_ron, 5.2601)
expect_error(parse_bnr(sub("<OrigCurrency>RON", "<OrigCurrency>EUR", xml_fixture)))
expect_error(parse_bnr(gsub('currency="EUR"', 'currency="GBP"', xml_fixture)))
expect_error(parse_bnr(sub('5.2601', 'NaN', xml_fixture)))
expect_error(parse_ing(sub("EUR/RON</td><td>5.3", "EUR/XXX</td><td>5.3", html_fixture)))
expect_error(parse_ing(sub("<td>5.6</td>", "<td>-</td>", html_fixture, fixed = TRUE)))
expect_error(parse_ing(sub("4Q26F", "3Q26F", html_fixture)))
expect_error(parse_ing("<html><body>Service unavailable</body></html>"))

stamp <- as.POSIXct("2026-09-17 14:00:00", tz = "UTC")
cache <- tempfile()
requests <- 0L
fetch_fake <- function(url) {
  requests <<- requests + 1L
  if (url == BNR_URL) charToRaw(xml_fixture) else charToRaw(html_fixture)
}
sources <- load_live_sources(cache_dir = cache, now = stamp, fetch = fetch_fake)
check(requests, 2L)
load_live_sources(cache_dir = cache, now = stamp + 60, fetch = fetch_fake)
check(requests, 2L)
load_live_sources(cache_dir = cache, now = stamp + 3601, fetch = fetch_fake)
check(requests, 3L) # Only BNR refreshes hourly.
load_live_sources(cache_dir = cache, now = stamp + 86401, fetch = fetch_fake)
check(requests, 5L)
load_live_sources(force = TRUE, cache_dir = cache, now = stamp + 86402, fetch = fetch_fake)
check(requests, 7L)
failure <- function(url) stop("offline")
stale <- load_live_sources(force = TRUE, cache_dir = cache, now = stamp + 86403, fetch = failure)
check(stale$bnr$stale, TRUE)
check(stale$ing$stale, TRUE)
check(stale$bnr$fetched_at, stamp + 86402) # A failed fetch must not renew the timestamp.
check(grepl("offline", stale$bnr$notice), TRUE)
expect_error(load_live_sources(cache_dir = cache, now = stamp + 10 * 86400, fetch = failure), "cache")
expect_error(load_live_sources(cache_dir = tempfile(), now = stamp, fetch = failure), "cache")
# A successful HTTP response with broken schema must preserve the good cache.
stale <- load_live_sources(force = TRUE, cache_dir = cache, now = stamp + 86403,
                           fetch = function(url) charToRaw("<html>error</html>"))
check(stale$ing$stale, TRUE)
check(readRDS(file.path(cache, "ing-v1.rds"))$data$points$eur_ron, c(5.3, 5.6, 5.9))
writeLines("corrupt", file.path(cache, "ing-v1.rds"))
load_live_sources(cache_dir = cache, now = stamp + 86404, fetch = fetch_fake)
check(requests, 8L)
unlink(cache, recursive = TRUE)

# Dates, interpolation, horizon and source distinctions.
today <- as.Date("2026-09-17")
live <- build_live_rates(sources, today, today)
check(live$eur_ron[1], 5.2601)
check(live$payment_date[1], today)
check(live$payment_date[2], as.Date("2026-10-17"))
# 17 days after September's quarter end out of 92 days to December's target.
check(live$eur_ron[2], 5.3 + (5.6 - 5.3) * 17 / 92)
check(live$rate_type[1], "BNR observat")
check(tail(live$month, 1), "2027-03")
check(build_live_rates(sources, as.Date("2026-11-01"), today)$payment_date[1], as.Date("2026-11-01"))
expect_error(build_live_rates(sources, as.Date("2026-08-01"), today), "trecut")
expect_error(build_live_rates(sources, as.Date("2027-04-01"), today), "acoperă")
check(monthly_payment_dates(as.Date("2027-01-31"), 3), as.Date(c("2027-01-31", "2027-02-28", "2027-03-31")))
check(monthly_payment_dates(as.Date("2028-01-31"), 3), as.Date(c("2028-01-31", "2028-02-29", "2028-03-31")))
check(monthly_payment_dates(as.Date("2026-12-30"), 2), as.Date(c("2026-12-30", "2027-01-30")))
check(bucharest_today(as.POSIXct("2026-09-17 22:30:00", tz = "UTC")), as.Date("2026-09-18"))
weekend <- sources
weekend$bnr$data$date <- as.Date("2026-09-18")
check(build_live_rates(weekend, as.Date("2026-09-20"), as.Date("2026-09-20"))$eur_ron[1], 5.2601)
expect_error(build_live_rates(sources, as.Date("2026-09-30"), as.Date("2026-09-30")), "actuală")
expect_error(build_live_rates(sources, as.Date("2026-09-16"), as.Date("2026-09-16")), "actuală")
# A past forecast horizon supports only today's observed rate, no extrapolation.
expired <- sources
expired$ing$data$points$date <- expired$ing$data$points$date - 730
check(nrow(build_live_rates(expired, today, today)), 1L)

# Use an isolated server environment to ensure no live network requests in tests.
server_live <- server
env <- new.env(parent = environment(server))
calls <- 0L
forces <- logical()
env$load_live_sources <- function(force = FALSE) {
  calls <<- calls + 1L
  forces <<- c(forces, force)
  sources
}
env$bucharest_today <- function() today
# build_live_rates' default is evaluated in its own environment.
env$build_live_rates <- function(s, month) build_live_rates(s, month, today)
environment(server_live) <- env
shiny::testServer(server_live, {
  session$setInputs(sursa_date = "live", luna_start = today, durata = 3, chirie_eur = 500,
                   fara_plafon = FALSE, plafon = 5.3, c2_n = 2, c2_avans_luni = 1,
                   c2_restituire_pct = 100, mod_vedere_tab1 = "pierdere", refresh_data = 0)
  check(calc_tab1()$eur_ron[1], 5.2601)
  check(length(calc_tab2()$months), 3L)
  check(grepl("BNR observat", output$curs_selectat), TRUE)
  check(grepl("7 September", output$source_status$html), TRUE)
  check(grepl("Tip curs", output$tabel_simplu), TRUE)
  initial_calls <- calls
  session$setInputs(durata = 5, chirie_eur = 600)
  check(length(calc_tab2()$months), 5L)
  check(calls, initial_calls)
  session$setInputs(refresh_data = 1)
  check(tail(forces, 1), TRUE)
  check(calls, initial_calls + 1L)
  session$setInputs(luna_start = as.Date("2026-11-01"))
  check(calc_tab1()$payment_date[1], as.Date("2026-11-01"))
  check(calls, initial_calls + 1L)
  session$setInputs(sursa_date = "local")
  check(calc_tab1()$eur_ron[1], 5.29)
  check(calls, initial_calls + 1L)
})
