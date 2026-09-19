# ============================================================
#  app.R  — Simulator negociere chirie EUR→RON
#  Chirie: 500 EUR/lună, plătită în RON la cursul BNR
#  Date live: BNR + repere trimestriale ING; alternativ CSV local
# ============================================================
# Excluderi explicite din model:
#   - Dobânzi / randament pe sumele deținute
#   - Indexarea chiriei
#   - Alte costuri comune tuturor variantelor (utilități etc.)
#   - Fluctuații intra-lunare ale cursului
# ============================================================

library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(bslib)

# ---- date implicite ----------------------------------------
DEFAULT_CSV <- "date_curs.csv"
source("R/model.R")
source("R/sources.R")

month_input <- function() {
  picker <- dateInput("luna_start", "Prima lună de chirie",
                      value = format(bucharest_today(), "%Y-%m-01"),
                      format = "mm/yyyy", startview = "year", language = "ro")
  picker$children[[2]]$attribs[["data-date-min-view-mode"]] <- "months"
  picker
}

# ---- Helper functions for forecast visual indicators -------
render_period_pill <- function(is_forecast) {
  if (is.null(is_forecast) || length(is_forecast) == 0) return(NULL)
  luni <- length(is_forecast)
  n_past <- sum(!is_forecast)
  n_fut  <- sum(is_forecast)
  
  fmt_past <- if (n_past == 1) "1 lună istorică reală (BNR)" else paste0(n_past, " luni istorice reale (BNR)")
  fmt_fut  <- if (n_fut == 1) "1 lună prognoză viitoare (ING)" else paste0(n_fut, " luni prognoză viitoare (ING)")
  
  if (n_fut == 0) {
    span(style = "background-color: #d4edda; color: #155724; border: 1px solid #c3e6cb; font-size: 0.88em; font-weight: 600; padding: 7px 18px; border-radius: 20px; display: inline-block;",
         if (luni == 1) "🏛️ Singura lună este dată istorică reală BNR"
         else paste0("🏛️ Toate cele ", luni, " luni sunt date istorice reale BNR"))
  } else if (n_past == 0) {
    span(style = "background-color: #fff3cd; color: #856404; border: 1px solid #ffeeba; font-size: 0.88em; font-weight: 600; padding: 7px 18px; border-radius: 20px; display: inline-block;",
         if (luni == 1) "🔮 Singura lună este prognoză / estimare viitoare"
         else paste0("🔮 Toate cele ", luni, " luni sunt prognoze / estimări viitoare"))
  } else {
    span(style = "background-color: #d1ecf1; color: #0c5460; border: 1px solid #bee5eb; font-size: 0.88em; font-weight: 600; padding: 7px 18px; border-radius: 20px; display: inline-block;",
         paste0("⚖️ Perioadă mixtă: ", fmt_past, " + ", fmt_fut))
  }
}

add_forecast_shading <- function(p, is_forecast, alpha = 0.5) {
  if (!is.null(is_forecast) && any(is_forecast)) {
    fc_idx   <- which(is_forecast)
    first_fc <- min(fc_idx)
    last_fc  <- length(is_forecast)
    p <- p + annotate("rect", xmin = first_fc - 0.5, xmax = last_fc + 0.5,
                      ymin = -Inf, ymax = Inf, fill = "#fef9e7", alpha = alpha)
    if (any(!is_forecast) && first_fc > 1) {
      p <- p + geom_vline(xintercept = first_fc - 0.5, linetype = "dashed",
                          color = "#d35400", linewidth = 1)
    }
  }
  p
}

forecast_subtitle_suffix <- function(is_forecast, months) {
  if (is.null(is_forecast) || length(is_forecast) == 0) return("")
  has_fc   <- any(is_forecast)
  has_hist <- any(!is_forecast)
  if (has_fc && has_hist) {
    paste0(" | Zonă galbenă & inele: 🔮 Prognoză din ", months[min(which(is_forecast))])
  } else if (has_fc) {
    " | 🔮 Toate lunile sunt prognoze/estimări viitoare"
  } else {
    " | 🏛️ Toate lunile sunt date istorice reale BNR"
  }
}

# # ---- UI ----------------------------------------------------
ui <- page_navbar(
  title = "🏠 Simulator Chirie EUR→RON",
  theme = bs_theme(bootswatch = "flatly"),
  sidebar = sidebar(
    width = 330,
    open = TRUE,
    h4("⚙️ Parametri Generali"),
    numericInput("chirie_eur", "Chirie lunară (EUR)",
                 value = 500, min = 50, max = 5000, step = 25),
    uiOutput("month_picker"),
    textOutput("curs_selectat"),
    sliderInput("durata", "Durata șederii (luni)",
                min = 1, max = 36, value = 36, step = 1),
    hr(),
    h5("Plafon negociat curs"),
    checkboxInput("fara_plafon", "Fără plafon (Plafon = ∞)", value = FALSE),
    conditionalPanel(
      condition = "!input.fara_plafon",
      sliderInput("plafon", "Plafon curs (RON/EUR)",
                  min = 4.80, max = 6.00, value = 5.30, step = 0.01)
    ),
    hr(),
    h5("Date curs EUR/RON"),
    selectInput("sursa_date", "Sursa datelor", choices = c(
      "BNR live + prognoze ING" = "live", "CSV propriu" = "csv",
      "CSV existent (offline)" = "local"), selected = "live"),
    conditionalPanel("input.sursa_date === 'live'",
      actionButton("refresh_data", "Actualizează acum"),
      uiOutput("source_status"),
      helpText("Mod live: poți alege orice lună de pornire (istoric oficial BNR din 2018 până în prezent, sau pornire în viitor cu prognoze ING). Cursurile viitoare sunt estimate între reperele trimestriale ING.")
    ),
    conditionalPanel("input.sursa_date === 'csv'",
      fileInput("csv_upload", "Încarcă CSV propriu curs", accept = ".csv")
    ),
    conditionalPanel("input.sursa_date !== 'live'",
      helpText("CSV: month (AAAA-LL), eur_ron. CSV-ul existent este un scenariu static, cu proveniență neverificată.")
    ),
    helpText("Cursurile viitoare și economiile sunt estimări, nu garanții.")
  ),
  nav_spacer(),

  # ======================== TAB 1: PLAFON ========================
  nav_panel(
    "📊 Plafon (vedere simplă)",
    layout_sidebar(
      sidebar = sidebar(
        width = 280,
        open = "closed",
        h5("Opțiuni afișare Tab 1"),
        radioButtons("mod_vedere_tab1", "Perspectivă afișare:",
                     choices = c("Diferență netă față de cursul primei luni" = "pierdere",
                                 "Economie generată de plafon" = "economie"),
                     selected = "pierdere"),
        helpText("Parametrii generali de curs, chirie și plafon sunt sincronizați din bara laterală principală.")
      ),
      fluidRow(
        column(12,
          uiOutput("big_number_ui")
        )
      ),
      fluidRow(
        column(12,
          plotOutput("plot_economie", height = "400px")
        )
      ),
      fluidRow(
        column(12,
          div(style = "overflow-x: auto;", tableOutput("tabel_simplu"))
        )
      )
    )
  ),

  # ======================== TAB 2: COMPARARE STRATEGII ========================
  nav_panel(
    "⚖️ Comparare strategii",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        h5("Opțiuni Strategii"),
        helpText("Primești salariul în RON și achiți direct proprietarului în RON la cursul oficial BNR, fără comisioane bancare/valutare."),
        sliderInput("c2_n", "n = luni achitate odată (în avans)", 1, 24, 3, 1),
        hr(),
        h5("Garanție (RON)"),
        numericInput("c2_avans_luni", "Garanție inițială (luni chirii)",
                     value = 1, min = 0, max = 6, step = 1),
        sliderInput("c2_restituire_pct", "% din garanție recuperat la plecare",
                     0, 100, 100, 5)
      ),
      # main
      fluidRow(
        column(12,
          uiOutput("c2_big_number_ui")
        )
      ),
      fluidRow(
        column(12,
          plotOutput("plot_comparare", height = "480px")
        )
      ),
      fluidRow(
        column(12,
          h5("Sinteză comparativă strategii (pentru durata selectată)"),
          div(style = "overflow-x: auto;", tableOutput("tabel_comparare"))
        )
      ),
      fluidRow(
        column(12,
          wellPanel(
            h6("Mecanismul de plată în RON la curs BNR"),
            tags$ul(
              tags$li("Fără comisioane bancare sau de schimb valutar (nu cumperi valută prin bănci/Revolut cu spread)."),
              tags$li("Plata anticipată pe ", tags$b("n luni"), " blochează cursul BNR din prima lună a blocului pentru toată perioada de n luni."),
              tags$li("Dacă leul se depreciază în lunile 2, 3, etc., plata în avans te protejează, economisind diferența de curs."),
              tags$li("Ultimul bloc acoperă doar lunile rămase din ședere. Graficul repartizează chiria pe lunile acoperite, chiar dacă plata se face anticipat."),
              tags$li("Garanția se calculează la cursul inițial al fiecărei strategii și se restituie ca procent din aceeași sumă în RON."),
              tags$li("Dacă plafonul este activ, cursul aplicat este limitat la valoarea plafonului."),
              tags$li("Diferențele față de luna 1 includ și scăderile cursului. Economie negativă = cost suplimentar. Graficele și cardurile exclud garanția; tabelul include și costul ei net.")
            )
          )
        )
      )
    )
  )
)

# ---- SERVER ------------------------------------------------
server <- function(input, output, session) {

  output$month_picker <- renderUI(month_input())
  last_refresh <- 0L
  live_sources <- reactive({
    req(identical(input$sursa_date, "live"))
    invalidateLater(5 * 60 * 1000, session)
    refresh <- input$refresh_data %||% 0L
    force <- refresh != last_refresh
    last_refresh <<- refresh
    tryCatch(load_live_sources(force = force), error = function(e) {
      validate(need(FALSE, conditionMessage(e)))
    })
  })

  date_curs <- reactive({
    req(input$sursa_date)
    if (identical(input$sursa_date, "live")) {
      return(tryCatch(build_live_timeline(live_sources(), bucharest_today()),
                      error = function(e) validate(need(FALSE, conditionMessage(e)))))
    }
    if (identical(input$sursa_date, "csv")) {
      validate(need(!is.null(input$csv_upload), "Încarcă un CSV pentru această sursă."))
    }
    path <- if (identical(input$sursa_date, "csv")) input$csv_upload$datapath else DEFAULT_CSV
    tryCatch({
      df <- read_rates(path)
      cur_m <- format(bucharest_today(), "%Y-%m")
      if (!("payment_date" %in% names(df))) df$payment_date <- as.Date(paste0(df$month, "-01"))
      if (!("is_forecast" %in% names(df))) df$is_forecast <- df$month > cur_m
      if (!("rate_type" %in% names(df))) {
        df$rate_type <- ifelse(df$is_forecast, "CSV (scenariu viitor)", "CSV (istoric)")
      }
      df
    }, error = function(e) {
      validate(need(FALSE, conditionMessage(e)))
    })
  })

  observe({
    df <- date_curs()
    updateDateInput(session, "luna_start",
                    min = as.Date(paste0(df$month[1], "-01")),
                    max = as.Date(paste0(tail(df$month, 1), "-01")))
  })

  date_curs_ajustat <- reactive({
    validate(need(length(input$luna_start) == 1 && !is.na(input$luna_start),
                  "Alege o lună disponibilă din calendar."))
    if (identical(input$sursa_date, "live")) {
      return(tryCatch(build_live_rates(live_sources(), input$luna_start),
                      error = function(e) validate(need(FALSE, conditionMessage(e)))))
    }
    df <- date_curs()
    month <- format(as.Date(input$luna_start), "%Y-%m")
    validate(need(month %in% df$month,
                  "Luna aleasă nu există în CSV. Alege o lună disponibilă sau încarcă alte date."))
    df[df$month >= month, , drop = FALSE]
  })

  observe({
    total <- nrow(date_curs_ajustat())
    updateSliderInput(session, "durata", max = total,
                      value = min(isolate(input$durata) %||% 36, total))
    updateSliderInput(session, "c2_n", max = min(total, 12),
                      value = min(isolate(input$c2_n) %||% 3, total, 12))
  })

  output$curs_selectat <- renderText({
    df <- date_curs_ajustat()
    label <- if (identical(input$sursa_date, "live")) {
      paste0(df$rate_type[1], " pentru ", format(df$payment_date[1], "%d.%m.%Y"), ": ")
    } else "Curs din CSV: "
    paste0(label, sprintf("%.4f", df$eur_ron[1]),
           " RON/EUR · ", nrow(df), " luni disponibile")
  })

  output$source_status <- renderUI({
    s <- live_sources()
    stamp <- function(x) format(x$fetched_at, "%d.%m.%Y %H:%M", tz = "Europe/Bucharest")
    hist_min <- if (!is.null(s$history) && nrow(s$history) > 0) min(s$history$month) else "2018-01"
    tagList(
      p(tags$a("BNR", href = BNR_URL, target = "_blank"), ": ",
        sprintf("%.4f", s$bnr$data$eur_ron), " RON/EUR, publicat ",
        format(s$bnr$data$date, "%d.%m.%Y"), ". Verificat: ", stamp(s$bnr)),
      p(tags$a("ING", href = ING_URL, target = "_blank"), ": ", s$ing$data$published_label,
        ". Verificat: ", stamp(s$ing), ". Repere până la ",
        format(max(s$ing$data$points$date), "%d.%m.%Y"), "."),
      p("🏛️ ", tags$b("Date istorice:"), " Cursuri lunare oficiale BNR disponibile din ", hist_min, " până în prezent."),
      if (!is.null(s$bnr$notice)) div(class = "alert alert-warning", s$bnr$notice),
      if (!is.null(s$ing$notice)) div(class = "alert alert-warning", s$ing$notice)
    )
  })

  parametri <- reactive({
    valid_number <- function(x, low, high, integer = FALSE) {
      is.numeric(x) && length(x) == 1 && is.finite(x) &&
        x >= low && x <= high && (!integer || x == floor(x))
    }
    validate(need(valid_number(input$chirie_eur, 50, 5000), "Chiria trebuie să fie între 50 și 5000 EUR."),
             need(valid_number(input$durata, 1, 10000, TRUE), "Durata trebuie să fie un număr întreg pozitiv."),
             need(isTRUE(input$fara_plafon) || valid_number(input$plafon, 4.8, 6), "Plafon invalid."))
    list(chirie = input$chirie_eur,
         pl = if (isTRUE(input$fara_plafon)) Inf else input$plafon)
  })

  # ============================================================
  #  TAB 1 — PLAFON & DEPRECIERE (BANI ÎN PLUS VS ECONOMIE)
  # ============================================================
  calc_tab1 <- reactive({
    p <- parametri()
    calculate_cap(trunca(date_curs_ajustat(), input$durata), p$chirie, p$pl)
  })

  output$big_number_ui <- renderUI({
    df      <- calc_tab1()
    luni    <- nrow(df)
    fara_pl <- isTRUE(input$fara_plafon)
    pl      <- if (fara_pl) Inf else input$plafon
    mod     <- input$mod_vedere_tab1
    
    pierdere_totala_fara <- round(tail(df$plus_depreciere_fara_cum, 1), 2)
    economie_totala      <- round(tail(df$economie_cum, 1), 2)
    luni_plafonate       <- sum(df$loveste_plafon)
    
    period_pill <- render_period_pill(df$is_forecast)

    if (mod == "pierdere") {
      if (fara_pl) {
        div(
          style = "text-align:center; padding:18px;",
          div(style = "margin-bottom: 10px;", period_pill),
          h2(style = "color:#c0392b; font-size:2.3em;",
             paste0("Pierderi totale cumulative: ", format(pierdere_totala_fara, big.mark = ".", decimal.mark = ","), " lei")),
          h4(style = "color:#495057;",
             paste0("în ", luni, " luni față de cursul primei luni (fără niciun plafon de protecție)"))
        )
      } else {
        div(
          style = "text-align:center; padding:18px;",
          div(style = "margin-bottom: 10px;", period_pill),
          h2(style = "color:#b94a00; font-size:2.2em;",
             paste0("Fără plafon, pierderi totale cumulative: ", format(pierdere_totala_fara, big.mark = ".", decimal.mark = ","), " lei")),
          h4(style = "color:#1e824c; font-weight:600;",
             paste0("Plafonul de ", pl, " RON/EUR te protejează: economisești ", 
                    format(economie_totala, big.mark = ".", decimal.mark = ","), " lei (plafon atins în ", luni_plafonate, " din ", luni, " luni)"))
        )
      }
    } else {
      # mod economie
      if (fara_pl || economie_totala == 0) {
        div(
          style = "text-align:center; padding:18px;",
          div(style = "margin-bottom: 10px;", period_pill),
          h3(style = "color:#b94a00;",
             if (fara_pl) "Plafonul este dezactivat (∞). Nu există economie de plafonare."
             else paste0("Plafonul de ", pl, " RON/EUR nu generează economii în cele ", luni, " luni.")),
          h4(style = "color:#495057;", "→ Economia datorată plafonului este 0 lei.")
        )
      } else {
        div(
          style = "text-align:center; padding:18px;",
          div(style = "margin-bottom: 10px;", period_pill),
          h2(style = "color:#1e824c; font-size:2.4em;",
             paste0("Economisești în total ", format(economie_totala, big.mark = ".", decimal.mark = ","), " lei")),
          h4(style = "color:#2c3e50;",
             paste0("în ", luni, " luni, cu plafon ", pl, " RON/EUR (activ în ", luni_plafonate, " luni)"))
        )
      }
    }
  })

  output$plot_economie <- renderPlot({
    df      <- calc_tab1()
    fara_pl <- isTRUE(input$fara_plafon)
    pl      <- if (fara_pl) Inf else input$plafon
    mod     <- input$mod_vedere_tab1

    p <- ggplot(df, aes(x = luna_idx, group = 1))
    p <- add_forecast_shading(p, df$is_forecast, alpha = 0.6)
    
    if (mod == "pierdere") {
      # Grafic: Bani plătiți în plus din cauza deprecierii leului
      p <- p +
        geom_col(aes(y = plus_depreciere_fara_plafon, fill = "Diferență lunară față de luna 1 (fără plafon)"),
                 alpha = 0.45, width = 0.6)
      if (nrow(df) > 1) {
        p <- p + geom_line(aes(y = plus_depreciere_fara_cum, color = "Diferență netă cumulată (fără plafon)"),
                           linewidth = 1.3, linetype = "dashed")
      }
      p <- p + geom_point(aes(y = plus_depreciere_fara_cum, color = "Diferență netă cumulată (fără plafon)"), size = 2)
      
      if (!fara_pl) {
        if (nrow(df) > 1) {
          p <- p + geom_line(aes(y = plus_depreciere_cu_cum, color = "Diferență netă cumulată (cu plafon)"),
                             linewidth = 1.4)
        }
        p <- p + geom_point(aes(y = plus_depreciere_cu_cum, color = "Diferență netă cumulată (cu plafon)"), size = 2)
      }
      
      # Marcare puncte de lovire a plafonului
      puncte_lovite <- df[df$loveste_plafon, , drop = FALSE]
      if (nrow(puncte_lovite) > 0) {
        p <- p +
          geom_point(data = puncte_lovite,
                     aes(y = plus_depreciere_fara_cum, shape = "⚡ Plafon atins/depășit (curs >= plafon)"),
                     color = "#e74c3c", size = 4.5, stroke = 1.5) +
          scale_shape_manual(values = c("⚡ Plafon atins/depășit (curs >= plafon)" = 18))
      }

      # Evidențiere puncte din viitor (prognoză) cu inel portocaliu
      puncte_viitor <- df[df$is_forecast, , drop = FALSE]
      if (nrow(puncte_viitor) > 0) {
        p <- p +
          geom_point(data = puncte_viitor,
                     aes(x = luna_idx, y = plus_depreciere_fara_cum),
                     shape = 21, size = 4.5, stroke = 1.4, color = "#d35400", fill = "transparent",
                     inherit.aes = FALSE)
      }
      
      subtitlu <- paste0(
        "Chirie: ", input$chirie_eur, " EUR | Curs inițial: ", sprintf("%.4f", df$eur_ron[1]), " RON/EUR",
        forecast_subtitle_suffix(df$is_forecast, df$month)
      )

      p <- p +
        scale_fill_manual(values = c("Diferență lunară față de luna 1 (fără plafon)" = "#e74c3c")) +
        scale_color_manual(values = c("Diferență netă cumulată (fără plafon)" = "#c0392b",
                                     "Diferență netă cumulată (cu plafon)" = "#27ae60")) +
        labs(
          title = if (fara_pl) "Diferență față de cursul primei luni (fără plafon)"
                  else paste0("Diferență față de luna 1, fără și cu plafon de ", pl, " RON/EUR"),
          subtitle = subtitlu,
          x = "Luna", y = "RON", fill = NULL, color = NULL, shape = NULL
        )
    } else {
      # Mod clasic: Economie generată de plafon
      p <- p +
        geom_col(aes(y = economie_luna, fill = "Economie lunară"),
                 alpha = 0.6, width = 0.6)
      if (nrow(df) > 1) {
        p <- p + geom_line(aes(y = economie_cum, color = "Economie cumulată"),
                           linewidth = 1.4)
      }
      p <- p + geom_point(aes(y = economie_cum, color = "Economie cumulată"), size = 3)
      
      puncte_lovite <- df[df$loveste_plafon, , drop = FALSE]
      if (nrow(puncte_lovite) > 0) {
        p <- p +
          geom_point(data = puncte_lovite,
                     aes(y = economie_cum, shape = "⚡ Plafon atins/depășit"),
                     color = "#e74c3c", size = 4.5, stroke = 1.5) +
          scale_shape_manual(values = c("⚡ Plafon atins/depășit" = 18))
      }

      puncte_viitor <- df[df$is_forecast, , drop = FALSE]
      if (nrow(puncte_viitor) > 0) {
        p <- p +
          geom_point(data = puncte_viitor,
                     aes(x = luna_idx, y = economie_cum),
                     shape = 21, size = 5.2, stroke = 1.4, color = "#d35400", fill = "transparent",
                     inherit.aes = FALSE)
      }

      subtitlu <- paste0(
        "Chirie: ", input$chirie_eur, " EUR/lună",
        forecast_subtitle_suffix(df$is_forecast, df$month)
      )

      p <- p +
        scale_fill_manual(values = c("Economie lunară" = "#3498db")) +
        scale_color_manual(values = c("Economie cumulată" = "#27ae60")) +
        labs(
          title = if (fara_pl) "Plafon dezactivat (Economie = 0)"
                  else paste0("Economie datorată plafonului de ", pl, " RON/EUR"),
          subtitle = subtitlu,
          x = "Luna", y = "RON", fill = NULL, color = NULL, shape = NULL
        )
    }
    
    p +
      scale_x_continuous(breaks = df$luna_idx, labels = df$month) +
      scale_y_continuous(labels = label_comma(big.mark = ".", decimal.mark = ","),
                         expand = expansion(mult = c(0, 0.12))) +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "top",
            legend.box = "vertical",
            panel.grid.minor = element_blank())
  })

  output$tabel_simplu <- renderTable({
    df      <- calc_tab1()
    fara_pl <- isTRUE(input$fara_plafon)
    pl      <- if (fara_pl) Inf else input$plafon
    
    table <- data.frame(
      Luna                    = df$month,
      `Curs de referință`     = sprintf("%.4f", df$eur_ron),
      `Curs aplic.`           = sprintf("%.4f", df$curs_aplicat),
      `Plafon activ?`         = ifelse(df$loveste_plafon, "⚡ DA", "nu"),
      `Plată fără plafon`     = sprintf("%.2f lei", df$plata_fara),
      `Plată cu plafon`       = sprintf("%.2f lei", df$plata_cu),
      `Diferență netă cumulată` = sprintf("%.2f lei", df$plus_depreciere_fara_cum),
      `Economie plafon cum.`  = sprintf("%.2f lei", df$economie_cum),
      check.names = FALSE
    )
    if ("payment_date" %in% names(df) || "rate_type" %in% names(df)) {
      type_label <- if ("rate_type" %in% names(df)) {
        ifelse(df$is_forecast, paste0("🔮 ", df$rate_type), paste0("🏛️ ", df$rate_type))
      } else ifelse(df$is_forecast, "🔮 Prognoză", "🏛️ Istoric")
      p_date <- if ("payment_date" %in% names(df)) format(df$payment_date, "%d.%m.%Y") else paste0("01.", df$month)
      table <- cbind(data.frame(`Data plății` = p_date,
                                `Tip curs` = type_label, check.names = FALSE), table)
    }
    table
  }, striped = TRUE, hover = TRUE, spacing = "s", align = "r")

  # ============================================================
  #  TAB 2 — COMPARARE STRATEGII (PLĂȚI ÎN LEI LA CURS BNR)
  # ============================================================
  calc_tab2 <- reactive({
    p <- parametri()
    validate(need(length(input$c2_n) == 1 && is.finite(input$c2_n) &&
                    input$c2_n >= 1 && input$c2_n <= 12 && input$c2_n == floor(input$c2_n),
                  "Numărul de luni în avans trebuie să fie întreg, între 1 și 12."),
             need(length(input$c2_avans_luni) == 1 && is.finite(input$c2_avans_luni) &&
                    input$c2_avans_luni >= 0 && input$c2_avans_luni <= 6 &&
                    input$c2_avans_luni == floor(input$c2_avans_luni), "Garanția trebuie să fie între 0 și 6 luni întregi."),
             need(length(input$c2_restituire_pct) == 1 && is.finite(input$c2_restituire_pct) &&
                    input$c2_restituire_pct >= 0 && input$c2_restituire_pct <= 100,
                  "Restituirea trebuie să fie între 0 și 100%."))
    calculate_strategies(trunca(date_curs_ajustat(), input$durata), p$chirie,
                         p$pl, input$c2_n, input$c2_avans_luni,
                         input$c2_restituire_pct / 100)
  })

  output$c2_big_number_ui <- renderUI({
    r <- calc_tab2()

    pierdere_s1 <- round(tail(r$p1_cum, 1), 2)
    pierdere_s3 <- round(tail(r$p3_cum, 1), 2)
    pierdere_s4 <- round(tail(r$p4_cum, 1), 2)

    ec_avans   <- round(tail(r$ec_s3, 1), 2)
    ec_av_plaf <- round(tail(r$ec_s4, 1), 2)
    luni_plaf  <- sum(r$loveste_plafon)

    period_pill <- render_period_pill(r$is_forecast)

    tagList(
      div(style = "text-align: center; margin-bottom: 12px;", period_pill),
      div(
        style = "display: flex; gap: 16px; flex-wrap: wrap; margin-bottom: 12px;",
        div(
          style = "flex: 1; min-width: 240px; background: #fff3f3; border: 1px solid #f5c6cb; border-radius: 8px; padding: 14px; text-align: center;",
          div(style = "font-size: 13px; color: #721c24; font-weight: 600; text-transform: uppercase;",
              "Diferență față de luna 1 (lunar fără plafon)"),
          div(style = "font-size: 26px; font-weight: 800; color: #dc3545; margin: 4px 0;",
              paste0(format(pierdere_s1, big.mark = ".", decimal.mark = ","), " lei")),
          div(style = "font-size: 12px; color: #842029;", "Pozitiv = cost suplimentar; negativ = economie")
        ),
        div(
          style = "flex: 1; min-width: 240px; background: #f0f7ff; border: 1px solid #b6d4fe; border-radius: 8px; padding: 14px; text-align: center;",
          div(style = "font-size: 13px; color: #084298; font-weight: 600; text-transform: uppercase;",
              paste0("Economie: Plată în avans (blocuri ", r$n, " luni)")),
          div(style = "font-size: 26px; font-weight: 800; color: #0d6efd; margin: 4px 0;",
              paste0(format(ec_avans, big.mark = ".", decimal.mark = ","), " lei")),
          div(style = "font-size: 12px; color: #084298;",
              paste0("Diferență față de luna 1: ", pierdere_s3, " lei. Economie negativă = cost în plus."))
        ),
        if (!r$fara_pl) {
          div(
            style = "flex: 1; min-width: 240px; background: #e8f8f5; border: 1px solid #a3e4d7; border-radius: 8px; padding: 14px; text-align: center;",
            div(style = "font-size: 13px; color: #0e6251; font-weight: 600; text-transform: uppercase;",
                paste0("Economie: Avans ", r$n, " luni + Plafon ", r$pl)),
            div(style = "font-size: 26px; font-weight: 800; color: #117a65; margin: 4px 0;",
                paste0(format(ec_av_plaf, big.mark = ".", decimal.mark = ","), " lei")),
            div(style = "font-size: 12px; color: #117a65;",
                paste0("Diferență față de luna 1: ", pierdere_s4, " lei | Plafon aplicat blocurilor pentru ", luni_plaf, " luni"))
          )
        }
      )
    )
  })

  output$plot_comparare <- renderPlot({
    r <- calc_tab2()

    df_plot <- data.frame(
      idx   = rep(r$luni, 3),
      val   = c(r$p1_cum, r$p3_cum, r$ec_s3),
      tip   = rep(c("Diferență vs luna 1", "Diferență vs luna 1", "Bani economisiți"), each = length(r$luni)),
      strat = rep(c(
        "Diferență: Lunar (fără plafon)",
        paste0("⚠️ Diferență vs luna 1: Blocuri ", r$n, " luni (avans BNR)"),
        paste0("✅ Economie realizată prin plata în avans pe ", r$n, " luni")
      ), each = length(r$luni))
    )

    if (!r$fara_pl) {
      df_plafon <- data.frame(
        idx   = rep(r$luni, 2),
        val   = c(r$p4_cum, r$ec_s4),
        tip   = rep(c("Diferență vs luna 1", "Bani economisiți"), each = length(r$luni)),
        strat = rep(c(
          paste0("🛡️ Diferență vs luna 1: Blocuri ", r$n, " luni + Plafon ", r$pl),
          paste0("✨ Economie totală: Blocuri ", r$n, " luni + Plafon ", r$pl)
        ), each = length(r$luni))
      )
      df_plot <- rbind(df_plot, df_plafon)
    }

    p <- ggplot(df_plot, aes(x = idx, y = val, color = strat, linetype = tip))
    p <- add_forecast_shading(p, r$is_forecast, alpha = 0.5)

    if (length(r$luni) > 1) {
      p <- p + geom_line(aes(group = interaction(strat, tip)), linewidth = 1.3)
    }
    p <- p + geom_point(size = 2.4)

    # Inele portocalii pentru punctele din viitor (prognoză)
    n_series <- nrow(df_plot) / length(r$luni)
    puncte_viitor <- df_plot[rep(r$is_forecast, n_series), , drop = FALSE]
    if (nrow(puncte_viitor) > 0) {
      p <- p + geom_point(data = puncte_viitor, aes(x = idx, y = val),
                          shape = 21, size = 4.4, stroke = 1.3, color = "#d35400", fill = "transparent",
                          inherit.aes = FALSE)
    }

    subtitle_txt <- paste0(
      "Curs la pornire: ", sprintf("%.4f", r$curs_baza),
      " RON/EUR | Chirie: ", input$chirie_eur, " EUR/lună | Plată directă în RON la curs BNR",
      forecast_subtitle_suffix(r$is_forecast, r$months)
    )

    p <- p +
      scale_linetype_manual(
        name = "Tip indicator",
        values = c("Diferență vs luna 1" = "solid", "Bani economisiți" = "dashed")
      ) +
      scale_color_brewer(palette = "Set1", name = "Strategie & Impact") +
      labs(
        title = "Diferențe față de luna 1 și economii față de plata lunară (RON)",
        subtitle = subtitle_txt,
        x = "Luna", y = "RON (economie negativă = cost suplimentar)"
      )

    # Marcaje când lovește plafonul
    if (!r$fara_pl && any(r$loveste_plafon)) {
      luni_lovite <- r$luni[r$loveste_plafon]
      p <- p + geom_vline(xintercept = luni_lovite, linetype = "dotted", color = "#dc3545", alpha = 0.5)
    }

    p +
      scale_x_continuous(breaks = r$luni, labels = r$months) +
      scale_y_continuous(labels = label_comma(big.mark = ".", decimal.mark = ","),
                         expand = expansion(mult = c(0.02, 0.08))) +
      theme_minimal(base_size = 13) +
      theme(
        axis.text.x     = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.box      = "vertical",
        panel.grid.minor = element_blank()
      )
  })

  output$tabel_comparare <- renderTable({
    r   <- calc_tab2()

    tabel_df <- data.frame(
      Strategie = c(
        "1. Plată lunară (fără plafon)",
        if (r$fara_pl) "2. Plată lunară (plafon inactiv)" else paste0("2. Plată lunară (cu plafon ", r$pl, ")"),
        paste0("3. Blocuri ", r$n, " luni (fără plafon)"),
        if (r$fara_pl) paste0("4. Blocuri ", r$n, " luni (fără plafon)") else paste0("4. Blocuri ", r$n, " luni (cu plafon ", r$pl, ")")
      ),
      `Total RON chirie` = round(c(
        tail(r$s1_cum, 1), tail(r$s2_cum, 1),
        tail(r$s3_cum, 1), tail(r$s4_cum, 1)
      ), 2),
      `Diferență vs cursul lunii 1` = round(c(
        tail(r$p1_cum, 1), tail(r$p2_cum, 1),
        tail(r$p3_cum, 1), tail(r$p4_cum, 1)
      ), 2),
      `Economie chirie vs Lunar fără plafon` = round(c(
        0, tail(r$ec_s2, 1),
        tail(r$ec_s3, 1), tail(r$ec_s4, 1)
      ), 2),
      `Garanție inițială` = round(r$avans_ron, 2),
      `Garanție recuperată` = round(r$avans_restituit, 2),
      `Cost net garanție` = round(r$avans_net, 2),
      `Economie netă inclusiv garanție` = round(c(
        0, tail(r$ec_s2, 1), tail(r$ec_s3, 1), tail(r$ec_s4, 1)
      ) + r$avans_net[1] - r$avans_net, 2),
      `Cost net final (+ Garanție netă)` = round(c(
        tail(r$s1_cum, 1), tail(r$s2_cum, 1),
        tail(r$s3_cum, 1), tail(r$s4_cum, 1)
      ) + r$avans_net, 2),
      check.names = FALSE
    )

    if (r$fara_pl) {
      tabel_df <- tabel_df[c(1, 3), ]
    }

    tabel_df
  }, striped = TRUE, hover = TRUE, spacing = "s", align = "r")
}

# ---- RUN ---------------------------------------------------
shinyApp(ui, server)
