# ============================================================
#  app.R  — Simulator negociere chirie EUR→RON
#  Chirie: 500 EUR/lună, plătită în RON la cursul BNR
#  Sursa prognozelor: Gov Capital (Forecast close, 10 sept 2026)
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
CHIRIE_EUR  <- 500

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
    numericInput("curs_prima_luna", "Curs BNR prima lună (RON/EUR)",
                 value = 5.26, min = 4.00, max = 8.00, step = 0.01),
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
    h5("Date curs BNR"),
    fileInput("csv_upload", "Încarcă CSV propriu curs", accept = ".csv"),
    helpText("Opțional. Default conține prognoza 2026-2029.")
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
                     choices = c("Bani plătiți în plus (depreciere leu & cost lipsă plafon)" = "pierdere",
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
          tableOutput("tabel_simplu")
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
        sliderInput("c2_n", "n = luni achitate odată (în avans)", 1, 12, 3, 1),
        hr(),
        h5("Garanție / Avans (RON)"),
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
          tableOutput("tabel_comparare")
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
              tags$li("Dacă plafonul este activ, cursul aplicat este limitat la valoarea plafonului."),
              tags$li("Simularea calculează exact câți lei plătești în plus exclusiv din cauza creșterii cursului EUR/RON față de luna 1.")
            )
          )
        )
      )
    )
  )
)

# ---- SERVER ------------------------------------------------
server <- function(input, output, session) {

  # ---- date curs (reactiv) -----------------------------------
  date_curs <- reactive({
    df <- if (!is.null(input$csv_upload)) {
      tryCatch(read.csv(input$csv_upload$datapath, stringsAsFactors = FALSE),
               error = function(e) read.csv(DEFAULT_CSV, stringsAsFactors = FALSE))
    } else {
      read.csv(DEFAULT_CSV, stringsAsFactors = FALSE)
    }
    # validare coloane
    req("month" %in% names(df), "eur_ron" %in% names(df))
    df$month  <- as.character(df$month)
    df$eur_ron <- as.numeric(df$eur_ron)
    df <- df[!is.na(df$eur_ron), ]
    df <- df[order(df$month), ]
    
    # ajustare automată dinamică a duratei dacă CSV-ul are alt număr de luni
    total_luni <- nrow(df)
    updateSliderInput(session, "durata", max = total_luni)
    updateSliderInput(session, "c2_n", max = min(total_luni, 12))
    
    df
  })

  # suprascrie cursul primei luni cu valoarea introdusă manual
  date_curs_ajustat <- reactive({
    df <- date_curs()
    if (nrow(df) == 0) return(df)
    df$eur_ron[1] <- input$curs_prima_luna
    df
  })

  # ---- helper: trunchiază la durata selectată ----------------
  trunca <- function(df, durata) {
    n <- min(durata, nrow(df))
    df[seq_len(n), , drop = FALSE]
  }

  # ============================================================
  #  TAB 1 — PLAFON & DEPRECIERE (BANI ÎN PLUS VS ECONOMIE)
  # ============================================================
  calc_tab1 <- reactive({
    df  <- trunca(date_curs_ajustat(), input$durata)
    req(nrow(df) > 0)
    
    chirie  <- req(input$chirie_eur)
    fara_pl <- isTRUE(input$fara_plafon)
    pl      <- if (fara_pl) Inf else input$plafon
    
    curs_baza <- df$eur_ron[1]
    
    df$curs_aplicat       <- pmin(df$eur_ron, pl)
    df$plata_fara         <- chirie * df$eur_ron
    df$plata_cu           <- chirie * df$curs_aplicat
    
    # Economie datorată plafonului față de plata fără plafon
    df$economie_luna      <- pmax(chirie * (df$eur_ron - pl), 0)
    df$economie_cum       <- cumsum(df$economie_luna)
    
    # Bani plătiți în plus din deprecierea leului (față de cursul primei luni):
    # Fără plafon:
    df$plus_depreciere_fara_plafon <- pmax(chirie * (df$eur_ron - curs_baza), 0)
    df$plus_depreciere_fara_cum    <- cumsum(df$plus_depreciere_fara_plafon)
    
    # Cu plafon (cât plătești efectiv în plus față de luna 1):
    df$plus_depreciere_cu_plafon   <- pmax(chirie * (df$curs_aplicat - curs_baza), 0)
    df$plus_depreciere_cu_cum      <- cumsum(df$plus_depreciere_cu_plafon)
    
    # Indicator: atinge sau depășește plafonul?
    df$loveste_plafon     <- !fara_pl & (df$eur_ron >= pl)
    df$luna_idx           <- seq_len(nrow(df))
    df
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
    
    if (mod == "pierdere") {
      if (fara_pl) {
        div(
          style = "text-align:center; padding:18px;",
          h2(style = "color:#c0392b; font-size:2.3em;",
             paste0("Plătești în plus ", format(pierdere_totala_fara, big.mark = ".", decimal.mark = ","), " lei")),
          h4(style = "color:#7f8c8d;",
             paste0("în ", luni, " luni DOAR din cauza deprecierii leului (fără niciun plafon de protecție)"))
        )
      } else {
        div(
          style = "text-align:center; padding:18px;",
          h2(style = "color:#d35400; font-size:2.2em;",
             paste0("Fără plafon ai pierde ", format(pierdere_totala_fara, big.mark = ".", decimal.mark = ","), " lei în plus")),
          h4(style = "color:#27ae60; font-weight:600;",
             paste0("Plafonul de ", pl, " RON/EUR te protejează: economisești ", 
                    format(economie_totala, big.mark = ".", decimal.mark = ","), " lei (plafon atins în ", luni_plafonate, " din ", luni, " luni)"))
        )
      }
    } else {
      # mod economie
      if (fara_pl || economie_totala == 0) {
        div(
          style = "text-align:center; padding:18px;",
          h3(style = "color:#e67e22;",
             if (fara_pl) "Plafonul este dezactivat (∞). Nu există economie de plafonare."
             else paste0("Plafonul de ", pl, " RON/EUR nu este atins în niciuna dintre cele ", luni, " luni.")),
          h4(style = "color:#7f8c8d;", "→ Economia datorată plafonului este 0 lei.")
        )
      } else {
        div(
          style = "text-align:center; padding:18px;",
          h2(style = "color:#27ae60; font-size:2.4em;",
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
    
    p <- ggplot(df, aes(x = luna_idx))
    
    if (mod == "pierdere") {
      # Grafic: Bani plătiți în plus din cauza deprecierii leului
      p <- p +
        geom_col(aes(y = plus_depreciere_fara_plafon, fill = "Pierdere lunară (depreciere leu fără plafon)"),
                 alpha = 0.45, width = 0.6) +
        geom_line(aes(y = plus_depreciere_fara_cum, color = "Total pierdut cumulativ (fără plafon)"),
                  linewidth = 1.3, linetype = "dashed")
      
      if (!fara_pl) {
        p <- p +
          geom_line(aes(y = plus_depreciere_cu_cum, color = "Cost suplimentar efectiv suportat (cu plafon)"),
                    linewidth = 1.4)
      }
      
      # Marcare puncte de lovire a plafonului
      puncte_lovite <- df[df$loveste_plafon, , drop = FALSE]
      if (nrow(puncte_lovite) > 0) {
        p <- p +
          geom_point(data = puncte_lovite,
                     aes(y = plus_depreciere_fara_cum, shape = "⚡ Plafon atins/depășit (curs >= plafon)"),
                     color = "#e74c3c", size = 4.5, stroke = 1.5)
      }
      
      p <- p +
        scale_fill_manual(values = c("Pierdere lunară (depreciere leu fără plafon)" = "#e74c3c")) +
        scale_color_manual(values = c("Total pierdut cumulativ (fără plafon)" = "#c0392b",
                                     "Cost suplimentar efectiv suportat (cu plafon)" = "#27ae60")) +
        scale_shape_manual(values = c("⚡ Plafon atins/depășit (curs >= plafon)" = 18)) +
        labs(
          title = if (fara_pl) "Câți bani pierzi din deprecierea leului (Fără niciun plafon)"
                  else paste0("Câți bani ai pierde fără plafon vs. Protecția plafonului de ", pl, " RON/EUR"),
          subtitle = paste0("Chirie: ", input$chirie_eur, " EUR | Curs inițial: ", df$eur_ron[1], " RON/EUR | Punctele roșii indică lunile unde intervine plafonul"),
          x = "Luna", y = "RON", fill = NULL, color = NULL, shape = NULL
        )
    } else {
      # Mod clasic: Economie generată de plafon
      p <- p +
        geom_col(aes(y = economie_luna, fill = "Economie lunară"),
                 alpha = 0.6, width = 0.6) +
        geom_line(aes(y = economie_cum, color = "Economie cumulată"),
                  linewidth = 1.4) +
        geom_point(aes(y = economie_cum, color = "Economie cumulată"), size = 3)
      
      puncte_lovite <- df[df$loveste_plafon, , drop = FALSE]
      if (nrow(puncte_lovite) > 0) {
        p <- p +
          geom_point(data = puncte_lovite,
                     aes(y = economie_cum, shape = "⚡ Plafon atins/depășit"),
                     color = "#e74c3c", size = 4.5, stroke = 1.5)
      }
      
      p <- p +
        scale_fill_manual(values = c("Economie lunară" = "#3498db")) +
        scale_color_manual(values = c("Economie cumulată" = "#27ae60")) +
        scale_shape_manual(values = c("⚡ Plafon atins/depășit" = 18)) +
        labs(
          title = if (fara_pl) "Plafon dezactivat (Economie = 0)"
                  else paste0("Economie datorată plafonului de ", pl, " RON/EUR"),
          subtitle = paste0("Chirie: ", input$chirie_eur, " EUR/lună | Punctele roșii marchează când plafonul generează economii"),
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
    
    data.frame(
      Luna                    = df$month,
      `Curs prog.`            = sprintf("%.4f", df$eur_ron),
      `Curs aplic.`           = sprintf("%.4f", df$curs_aplicat),
      `Plafon activ?`         = ifelse(df$loveste_plafon, "⚡ DA", "nu"),
      `Plată fără plafon`     = sprintf("%.2f lei", df$plata_fara),
      `Plată cu plafon`       = sprintf("%.2f lei", df$plata_cu),
      `Plătit în plus (depr.)` = sprintf("%.2f lei", df$plus_depreciere_fara_cum),
      `Economie plafon cum.`  = sprintf("%.2f lei", df$economie_cum),
      check.names = FALSE
    )
  }, striped = TRUE, hover = TRUE, spacing = "s", align = "r")

  # ============================================================
  #  TAB 2 — COMPARARE STRATEGII (PLĂȚI ÎN LEI LA CURS BNR)
  # ============================================================
  calc_tab2 <- reactive({
    df_full <- date_curs_ajustat()
    durata  <- input$durata
    df      <- trunca(df_full, durata)
    req(nrow(df) > 0)

    chirie   <- req(input$chirie_eur)
    n        <- input$c2_n
    fara_pl  <- isTRUE(input$fara_plafon)
    pl       <- if (fara_pl) Inf else input$plafon
    avans_l  <- input$c2_avans_luni
    rest_pct <- input$c2_restituire_pct / 100

    luni      <- seq_len(nrow(df))
    curs      <- df$eur_ron
    curs_baza <- curs[1]

    # Curs plafonat
    curs_ap <- pmin(curs, pl)

    # Garanție / Avans inițial (plătit la cursul primei luni)
    avans_ron       <- avans_l * chirie * pmin(curs[1], pl)
    avans_restituit <- avans_ron * rest_pct
    avans_net       <- avans_ron - avans_restituit

    # ---- 1. Lunar fără plafon ----
    s1_plati <- chirie * curs

    # ---- 2. Lunar cu plafon ----
    s2_plati <- chirie * curs_ap

    # ---- 3. Blocuri n luni fără plafon ----
    # La începutul fiecărui bloc de n luni, plătești n luni chirie în RON la cursul BNR din acea lună
    s3_plati <- numeric(length(luni))
    s3_curs_bloc <- numeric(length(luni))
    bloc_start <- seq(1, length(luni), by = n)
    for (bs in bloc_start) {
      be  <- min(bs + n - 1, length(luni))
      # cursul blocat pentru lunile din acest interval este cursul lunii bs
      s3_curs_bloc[bs:be] <- curs[bs]
      s3_plati[bs:be]     <- chirie * curs[bs]
    }

    # ---- 4. Blocuri n luni cu plafon ----
    s4_plati <- numeric(length(luni))
    s4_curs_bloc <- numeric(length(luni))
    for (bs in bloc_start) {
      be  <- min(bs + n - 1, length(luni))
      s4_curs_bloc[bs:be] <- pmin(curs[bs], pl)
      s4_plati[bs:be]     <- chirie * pmin(curs[bs], pl)
    }

    # Cât s-ar fi plătit dacă leul rămânea neschimbat la cursul lunii 1 (bază)
    plata_baza_luna <- chirie * curs_baza

    # Pierderi / Bani în plus plătiți exclusiv din deprecierea leului:
    p1_luna <- pmax(s1_plati - plata_baza_luna, 0)
    p2_luna <- pmax(s2_plati - plata_baza_luna, 0)
    p3_luna <- pmax(s3_plati - plata_baza_luna, 0)
    p4_luna <- pmax(s4_plati - plata_baza_luna, 0)

    # Economii realizate față de plata de bază (Strategia 1: Lunar fără plafon)
    # Cât economisești dacă folosești fiecare dintre opțiuni:
    ec_s2 <- cumsum(pmax(s1_plati - s2_plati, 0)) # din plafon
    ec_s3 <- cumsum(pmax(s1_plati - s3_plati, 0)) # din blocuri n luni (plată anticipată)
    ec_s4 <- cumsum(pmax(s1_plati - s4_plati, 0)) # din blocuri + plafon

    # Indicatori plafon atins
    loveste_plafon <- !fara_pl & (curs >= pl)

    list(
      luni            = luni,
      months          = df$month,
      curs            = curs,
      curs_baza       = curs_baza,
      fara_pl         = fara_pl,
      pl              = pl,
      n               = n,
      loveste_plafon  = loveste_plafon,
      # Plăți lunare
      s1_luna         = s1_plati,
      s2_luna         = s2_plati,
      s3_luna         = s3_plati,
      s4_luna         = s4_plati,
      # Total cumulat efectiv plătit
      s1_cum          = cumsum(s1_plati),
      s2_cum          = cumsum(s2_plati),
      s3_cum          = cumsum(s3_plati),
      s4_cum          = cumsum(s4_plati),
      # Bani plătiți în plus din depreciere (cumulat)
      p1_cum          = cumsum(p1_luna),
      p2_cum          = cumsum(p2_luna),
      p3_cum          = cumsum(p3_luna),
      p4_cum          = cumsum(p4_luna),
      # Economii cumulate vs Strategia 1
      ec_s2           = ec_s2,
      ec_s3           = ec_s3,
      ec_s4           = ec_s4,
      # Garanție / Avans
      avans_ron       = avans_ron,
      avans_restituit = avans_restituit,
      avans_net       = avans_net
    )
  })

  output$c2_big_number_ui <- renderUI({
    r <- calc_tab2()

    total_s1 <- tail(r$s1_cum, 1)
    total_s3 <- tail(r$s3_cum, 1)
    total_s4 <- tail(r$s4_cum, 1)

    pierdere_s1 <- round(tail(r$p1_cum, 1), 2)
    pierdere_s3 <- round(tail(r$p3_cum, 1), 2)
    pierdere_s4 <- round(tail(r$p4_cum, 1), 2)

    ec_avans   <- round(tail(r$ec_s3, 1), 2)
    ec_av_plaf <- round(tail(r$ec_s4, 1), 2)
    luni_plaf  <- sum(r$loveste_plafon)

    tagList(
      div(
        style = "display: flex; gap: 16px; flex-wrap: wrap; margin-bottom: 12px;",
        div(
          style = "flex: 1; min-width: 240px; background: #fff3f3; border: 1px solid #f5c6cb; border-radius: 8px; padding: 14px; text-align: center;",
          div(style = "font-size: 13px; color: #721c24; font-weight: 600; text-transform: uppercase;",
              "Pierdere din depreciere (plată lunară fără plafon)"),
          div(style = "font-size: 26px; font-weight: 800; color: #dc3545; margin: 4px 0;",
              paste0("+", format(pierdere_s1, big.mark = ".", decimal.mark = ","), " lei")),
          div(style = "font-size: 12px; color: #842029;", "plătiți în plus doar din creșterea cursului EUR/RON")
        ),
        div(
          style = "flex: 1; min-width: 240px; background: #f0f7ff; border: 1px solid #b6d4fe; border-radius: 8px; padding: 14px; text-align: center;",
          div(style = "font-size: 13px; color: #084298; font-weight: 600; text-transform: uppercase;",
              paste0("Economie: Plată în avans (blocuri ", r$n, " luni)")),
          div(style = "font-size: 26px; font-weight: 800; color: #0d6efd; margin: 4px 0;",
              paste0(format(ec_avans, big.mark = ".", decimal.mark = ","), " lei")),
          div(style = "font-size: 12px; color: #084298;",
              paste0("Reduci pierderea la +", pierdere_s3, " lei trimițând la BNR în avans"))
        ),
        if (!r$fara_pl) {
          div(
            style = "flex: 1; min-width: 240px; background: #e8f8f5; border: 1px solid #a3e4d7; border-radius: 8px; padding: 14px; text-align: center;",
            div(style = "font-size: 13px; color: #0e6251; font-weight: 600; text-transform: uppercase;",
                paste0("Economie: Avans ", r$n, " luni + Plafon ", r$pl)),
            div(style = "font-size: 26px; font-weight: 800; color: #117a65; margin: 4px 0;",
                paste0(format(ec_av_plaf, big.mark = ".", decimal.mark = ","), " lei")),
            div(style = "font-size: 12px; color: #117a65;",
                paste0("Pierdere rămasă: doar +", pierdere_s4, " lei | Plafon atins în ", luni_plaf, " luni"))
          )
        }
      )
    )
  })

  output$plot_comparare <- renderPlot({
    r <- calc_tab2()

    # Grafic suprapus direct:
    # 1. Bani plătiți în plus (depreciere leului fără nicio protecție - lunar fără plafon)
    # 2. Pierdere rămasă dacă plătești în blocuri de n luni (avans)
    # 3. Pierdere rămasă dacă ai și plafon (când plafonul e activ)
    # 4. Economia salvată prin plata în avans (blocuri n luni)
    # 5. Economia totală dacă ai și plafon

    df_plot <- data.frame(
      idx   = rep(r$luni, 3),
      val   = c(r$p1_cum, r$p3_cum, r$ec_s3),
      tip   = rep(c("Pierdere (bani în plus)", "Pierdere (bani în plus)", "Bani economisiți"), each = length(r$luni)),
      strat = rep(c(
        "❌ Pierdere: Lunar (fără plafon)",
        paste0("⚠️ Pierdere rămasă: Blocuri ", r$n, " luni (avans BNR)"),
        paste0("✅ Economie realizată prin plata în avans pe ", r$n, " luni")
      ), each = length(r$luni))
    )

    if (!r$fara_pl) {
      df_plafon <- data.frame(
        idx   = rep(r$luni, 2),
        val   = c(r$p4_cum, r$ec_s4),
        tip   = rep(c("Pierdere (bani în plus)", "Bani economisiți"), each = length(r$luni)),
        strat = rep(c(
          paste0("🛡️ Pierdere rămasă: Blocuri ", r$n, " luni + Plafon ", r$pl),
          paste0("✨ Economie totală: Blocuri ", r$n, " luni + Plafon ", r$pl)
        ), each = length(r$luni))
      )
      df_plot <- rbind(df_plot, df_plafon)
    }

    p <- ggplot(df_plot, aes(x = idx, y = val, color = strat, linetype = tip)) +
      geom_line(linewidth = 1.3) +
      geom_point(size = 2.4) +
      scale_linetype_manual(
        name = "Tip indicator",
        values = c("Pierdere (bani în plus)" = "solid", "Bani economisiți" = "dashed")
      ) +
      scale_color_brewer(palette = "Set1", name = "Strategie & Impact") +
      labs(
        title = "Grafic Suprapus: Bani pierduți din depreciere vs. Economii realizate (RON)",
        subtitle = paste0(
          "Curs BNR pornire: ", sprintf("%.4f", r$curs_baza),
          " RON/EUR | Chirie: ", input$chirie_eur, " EUR/lună | Plată directă în RON la curs BNR"
        ),
        x = "Luna", y = "RON (față de cursul lunii 1)"
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
    nl  <- length(r$luni)

    nota_avans <- paste0(
      "Garanție inițială: ", round(r$avans_ron, 2), " lei | ",
      "Recuperat (", input$c2_restituire_pct, "%): ", round(r$avans_restituit, 2), " lei | ",
      "Cost net garanție: ", round(r$avans_net, 2), " lei"
    )

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
      `Din care bani pierduți (depreciere)` = round(c(
        tail(r$p1_cum, 1), tail(r$p2_cum, 1),
        tail(r$p3_cum, 1), tail(r$p4_cum, 1)
      ), 2),
      `Economie vs Lunar fără plafon` = round(c(
        0, tail(r$ec_s2, 1),
        tail(r$ec_s3, 1), tail(r$ec_s4, 1)
      ), 2),
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
