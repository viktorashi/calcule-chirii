source("app.R")
checks <- 0L
check <- function(actual, expected) {
  stopifnot(isTRUE(all.equal(actual, expected, check.attributes = FALSE)))
  checks <<- checks + 1L
}
fixture <- function(rates) data.frame(
  month = format(seq(as.Date("2026-09-01"), by = "month", length.out = length(rates)), "%Y-%m"),
  eur_ron = rates
)
# Hand-calculated rent totals, including a partial last advance block.
df <- fixture(c(5, 6, 4, 7, 3))
r <- calculate_strategies(df, 100, 5.5, 2, 1, 0.5)
check(c(tail(r$s1_cum, 1), tail(r$s2_cum, 1), tail(r$s3_cum, 1), tail(r$s4_cum, 1)),
      c(2500, 2300, 2100, 2100))
check(r$ec_s3, c(0, 100, 100, 400, 400))
check(r$p1_cum, c(0, 100, 0, 200, 0))
check(r$avans_net, rep(250, 4))
# Falling rates must report a loss from prepayment, never clip it to zero.
r <- calculate_strategies(fixture(c(6, 5, 4)), 100, Inf, 3, 0, 1)
check(tail(r$ec_s3, 1), -300)
check(tail(r$p1_cum, 1), -300)
# Gains and losses within the same block must offset.
r <- calculate_strategies(fixture(c(5, 6, 4)), 100, Inf, 3)
check(tail(r$ec_s3, 1), 0)
r <- calculate_strategies(fixture(c(5, 6, 4)), 100, 5.5, 3)
check(r$loveste_plafon, rep(FALSE, 3))
# Deposits follow the rate of each strategy, even when capped from month one.
r <- calculate_strategies(fixture(c(6, 7)), 100, 5, 2, 2, 0.25)
check(r$avans_ron, c(1200, 1000, 1200, 1000))
check(r$avans_restituit, c(300, 250, 300, 250))
check(r$avans_net, c(900, 750, 900, 750))
# Invariants over rising, falling and oscillating paths, single months,
# all block sizes, no cap, a binding cap, and an initially binding cap.
for (rates in list(5, c(5, 6, 7, 8), c(8, 7, 6, 5), c(5, 7, 4, 6, 3))) {
  for (cap in c(Inf, 5.5, 4.8)) {
    for (n in 1:12) {
      df <- fixture(rates)
      r <- calculate_strategies(df, 500, cap, n, 1, 1)
      t1 <- calculate_cap(df, 500, cap)
      check(r$s1_cum - r$s2_cum, r$ec_s2)
      check(r$s1_cum - r$s3_cum, r$ec_s3)
      check(r$s1_cum - r$s4_cum, r$ec_s4)
      check(t1$economie_cum, r$ec_s2)
      check(t1$plus_depreciere_fara_cum, r$p1_cum)
      check(t1$plus_depreciere_cu_cum, r$p2_cum)
      check(r$s4_luna, 500 * pmin(rates[((seq_along(rates) - 1) %/% n) * n + 1], cap))
      check(r$avans_net, rep(0, 4))
      if (n == 1) check(r$s3_cum, r$s1_cum)
      if (is.infinite(cap)) check(r$s4_cum, r$s3_cum)
    }
  }
}
# Strict CSV validation: invalid data must not be dropped or replaced silently.
csv <- tempfile(fileext = ".csv")
for (contents in c("month,eur_ron", "wrong,eur_ron\n2026-09,5", "month,eur_ron\n2026-13,5",
                   "month,eur_ron\n2026-09,5\n2026-09,6", "month,eur_ron\n2026-09,5\n2026-11,6",
                   "month,eur_ron\n2026-09,NA", "month,eur_ron\n2026-09,Inf",
                   "month,eur_ron\n2026-09,-1", "month,eur_ron\n2026-09,nope")) {
  writeLines(contents, csv)
  check(inherits(tryCatch(read_rates(csv), error = identity), "error"), TRUE)
}
writeLines("month,eur_ron\n2026-10,6\n2026-09,5", csv)
check(read_rates(csv)$month, c("2026-09", "2026-10"))
unlink(csv)
# UI and reactive integration: start month, upload errors, rendered summaries.
check(month_input()$children[[2]]$attribs[["data-date-min-view-mode"]], "months")
check(month_input()$children[[2]]$attribs[["data-initial-date"]], format(bucharest_today(), "%Y-%m-01"))
shiny::testServer(server, {
  session$setInputs(sursa_date = "local", luna_start = as.Date("2026-10-01"), durata = 3, chirie_eur = 500,
                   fara_plafon = FALSE, plafon = 5.3, c2_n = 2, c2_avans_luni = 1,
                   c2_restituire_pct = 100, mod_vedere_tab1 = "pierdere")
  check(calc_tab1()$month, c("2026-10", "2026-11", "2026-12"))
  check(calc_tab1()$eur_ron[1], 5.28)
  check(calc_tab2()$months, calc_tab1()$month)
  check(is.character(output$tabel_comparare), TRUE)
  check(is.list(output$big_number_ui), TRUE)
  check(is.list(output$c2_big_number_ui), TRUE)
  session$setInputs(luna_start = as.Date("2029-08-01"), durata = 36)
  check(nrow(calc_tab1()), 1L)
  check(length(calc_tab2()$months), 1L)
  session$setInputs(luna_start = as.Date("2030-01-01"))
  check(inherits(tryCatch(date_curs_ajustat(), error = identity), "shiny.silent.error"), TRUE)
  invalid_csv <- tempfile(fileext = ".csv")
  writeLines("bad,headers\nx,y", invalid_csv)
  session$setInputs(sursa_date = "csv", csv_upload = list(datapath = invalid_csv))
  check(inherits(tryCatch(date_curs(), error = identity), "shiny.silent.error"), TRUE)
  unlink(invalid_csv)
  uploaded_csv <- tempfile(fileext = ".csv")
  writeLines("month,eur_ron\n2026-09,6\n2026-10,5\n2026-11,4", uploaded_csv)
  session$setInputs(csv_upload = list(datapath = uploaded_csv),
                   luna_start = as.Date("2026-09-01"), durata = 3,
                   fara_plafon = TRUE, c2_n = 3)
  check(calc_tab1()$eur_ron, c(6, 5, 4))
  check(tail(calc_tab2()$ec_s3, 1), -1500)
  check(grepl("-1.500", output$c2_big_number_ui$html, fixed = TRUE), TRUE)
  check(is.character(output$tabel_comparare), TRUE)
  session$setInputs(mod_vedere_tab1 = "economie")
  check(grepl("dezactivat", output$big_number_ui$html), TRUE)
  unlink(uploaded_csv)
  session$setInputs(sursa_date = "local", csv_upload = NULL, luna_start = as.Date("2026-09-01"), chirie_eur = -500)
  check(inherits(tryCatch(calc_tab1(), error = identity), "shiny.silent.error"), TRUE)
})
source("tests/sources.R")
cat(sprintf("Passed %d calculation, CSV and Shiny integration checks.\n", checks))
