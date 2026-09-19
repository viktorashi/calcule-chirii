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

# # ---- date implicite ----------------------------------------
DEFAULT_CSV <- "date_curs.csv"
source("R/model.R")
source("R/sources.R")
source("R/config.R")

month_input <- function() {
  picker <- dateInput("luna_start", UI_STRINGS$sidebar$luna_start_label,
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
  
  p_cfg <- UI_STRINGS$period_pills
  fmt_past <- if (n_past == 1) p_cfg$past_singular else p_cfg$past_plural(n_past)
  fmt_fut  <- if (n_fut == 1) p_cfg$fut_singular else p_cfg$fut_plural(n_fut)
  
  if (n_fut == 0) {
    span(style = "background-color: #d4edda; color: #155724; border: 1px solid #c3e6cb; font-size: 0.88em; font-weight: 600; padding: 7px 18px; border-radius: 20px; display: inline-block;",
         p_cfg$all_past(luni))
  } else if (n_past == 0) {
    span(style = "background-color: #fff3cd; color: #856404; border: 1px solid #ffeeba; font-size: 0.88em; font-weight: 600; padding: 7px 18px; border-radius: 20px; display: inline-block;",
         p_cfg$all_fut(luni))
  } else {
    span(style = "background-color: #d1ecf1; color: #0c5460; border: 1px solid #bee5eb; font-size: 0.88em; font-weight: 600; padding: 7px 18px; border-radius: 20px; display: inline-block;",
         p_cfg$mixed(fmt_past, fmt_fut))
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
  s_cfg <- UI_STRINGS$subtitles
  if (has_fc && has_hist) {
    s_cfg$suffix_mixed(months[min(which(is_forecast))])
  } else if (has_fc) {
    s_cfg$suffix_all_fut
  } else {
    s_cfg$suffix_all_past
  }
}

# # ---- UI ----------------------------------------------------
ui <- page_navbar(
  title = UI_STRINGS$app_title,
  theme = bs_theme(bootswatch = "flatly"),
  sidebar = sidebar(
    width = 330,
    open = TRUE,
    h4(UI_STRINGS$sidebar$general_header),
    numericInput("chirie_eur", PARAM_CONFIG$chirie$label,
                 value = PARAM_CONFIG$chirie$default,
                 min = PARAM_CONFIG$chirie$min,
                 max = PARAM_CONFIG$chirie$max,
                 step = PARAM_CONFIG$chirie$step),
    uiOutput("month_picker"),
    textOutput("curs_selectat"),
    sliderInput("durata", PARAM_CONFIG$durata$label,
                min = PARAM_CONFIG$durata$min,
                max = PARAM_CONFIG$durata$max,
                value = PARAM_CONFIG$durata$default,
                step = PARAM_CONFIG$durata$step),
    hr(),
    h5(UI_STRINGS$sidebar$plafon_header),
    checkboxInput("fara_plafon", UI_STRINGS$sidebar$fara_plafon_label, value = FALSE),
    conditionalPanel(
      condition = "!input.fara_plafon",
      sliderInput("plafon", PARAM_CONFIG$plafon$label,
                  min = PARAM_CONFIG$plafon$min,
                  max = PARAM_CONFIG$plafon$max,
                  value = PARAM_CONFIG$plafon$default,
                  step = PARAM_CONFIG$plafon$step)
    ),
    hr(),
    h5(UI_STRINGS$sidebar$sursa_header),
    selectInput("sursa_date", UI_STRINGS$sidebar$sursa_label, choices = UI_STRINGS$sidebar$sursa_choices, selected = "live"),
    conditionalPanel("input.sursa_date === 'live'",
      actionButton("refresh_data", UI_STRINGS$sidebar$refresh_btn),
      uiOutput("source_status"),
      helpText(UI_STRINGS$sidebar$live_help)
    ),
    conditionalPanel("input.sursa_date === 'csv'",
      fileInput("csv_upload", UI_STRINGS$sidebar$csv_upload_label, accept = ".csv")
    ),
    conditionalPanel("input.sursa_date !== 'live'",
      helpText(UI_STRINGS$sidebar$csv_help)
    ),
    helpText(UI_STRINGS$sidebar$disclaimer)
  ),
  nav_spacer(),

  # ======================== TAB 1: PLAFON ========================
  nav_panel(
    UI_STRINGS$tabs$tab1,
    layout_sidebar(
      sidebar = sidebar(
        width = 280,
        open = "closed",
        h5(UI_STRINGS$tab1$options_header),
        radioButtons("mod_vedere_tab1", UI_STRINGS$tab1$mode_label,
                     choices = UI_STRINGS$tab1$mode_choices,
                     selected = "pierdere"),
        helpText(UI_STRINGS$tab1$mode_help)
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
    UI_STRINGS$tabs$tab2,
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        h5(UI_STRINGS$tab2$options_header),
        helpText(UI_STRINGS$tab2$options_help),
        sliderInput("c2_n", PARAM_CONFIG$avans_bloc$label,
                    min = PARAM_CONFIG$avans_bloc$min,
                    max = PARAM_CONFIG$avans_bloc$max,
                    value = PARAM_CONFIG$avans_bloc$default,
                    step = PARAM_CONFIG$avans_bloc$step),
        hr(),
        h5(UI_STRINGS$tab2$garantie_header),
        numericInput("c2_avans_luni", PARAM_CONFIG$garantie_luni$label,
                     value = PARAM_CONFIG$garantie_luni$default,
                     min = PARAM_CONFIG$garantie_luni$min,
                     max = PARAM_CONFIG$garantie_luni$max,
                     step = PARAM_CONFIG$garantie_luni$step),
        sliderInput("c2_restituire_pct", PARAM_CONFIG$restituire_pct$label,
                    min = PARAM_CONFIG$restituire_pct$min,
                    max = PARAM_CONFIG$restituire_pct$max,
                    value = PARAM_CONFIG$restituire_pct$default,
                    step = PARAM_CONFIG$restituire_pct$step)
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
          h5(UI_STRINGS$tab2$table_header),
          div(style = "overflow-x: auto;", tableOutput("tabel_comparare"))
        )
      ),
      fluidRow(
        column(12,
          wellPanel(
            h6(UI_STRINGS$info_bullets$title),
            tags$ul(
              tags$li(UI_STRINGS$info_bullets$b1),
              tags$li(HTML(UI_STRINGS$info_bullets$b2(PARAM_CONFIG$avans_bloc$default))),
              tags$li(UI_STRINGS$info_bullets$b3),
              tags$li(UI_STRINGS$info_bullets$b4),
              tags$li(UI_STRINGS$info_bullets$b5),
              tags$li(UI_STRINGS$info_bullets$b6),
              tags$li(UI_STRINGS$info_bullets$b7)
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
      validate(need(!is.null(input$csv_upload), UI_STRINGS$validation$csv_upload_needed))
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
                  UI_STRINGS$validation$luna_start_needed))
    if (identical(input$sursa_date, "live")) {
      return(tryCatch(build_live_rates(live_sources(), input$luna_start),
                      error = function(e) validate(need(FALSE, conditionMessage(e)))))
    }
    df <- date_curs()
    month <- format(as.Date(input$luna_start), "%Y-%m")
    validate(need(month %in% df$month,
                  UI_STRINGS$validation$luna_csv_not_found))
    df[df$month >= month, , drop = FALSE]
  })

  observe({
    total <- nrow(date_curs_ajustat())
    cfg_d <- PARAM_CONFIG$durata
    updateSliderInput(session, "durata", max = total,
                      value = min(isolate(input$durata) %||% cfg_d$default, total))
    cfg_n <- PARAM_CONFIG$avans_bloc
    updateSliderInput(session, "c2_n", max = min(total, cfg_n$max),
                      value = min(isolate(input$c2_n) %||% cfg_n$default, total, cfg_n$max))
  })

  output$curs_selectat <- renderText({
    df <- date_curs_ajustat()
    label <- if (identical(input$sursa_date, "live")) {
      paste0(df$rate_type[1], " pentru ", format(df$payment_date[1], "%d.%m.%Y"), ": ")
    } else UI_STRINGS$status$curs_csv_prefix
    paste0(label, sprintf("%.4f", df$eur_ron[1]),
           UI_STRINGS$status$curs_disponibil_suf(nrow(df)))
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
      p("🏛️ ", tags$b(UI_STRINGS$status$date_istorice_title), " ", UI_STRINGS$status$date_istorice_desc(hist_min)),
      if (!is.null(s$bnr$notice)) div(class = "alert alert-warning", s$bnr$notice),
      if (!is.null(s$ing$notice)) div(class = "alert alert-warning", s$ing$notice)
    )
  })

  parametri <- reactive({
    valid_number <- function(x, low, high, integer = FALSE) {
      is.numeric(x) && length(x) == 1 && is.finite(x) &&
        x >= low && x <= high && (!integer || x == floor(x))
    }
    cfg_c <- PARAM_CONFIG$chirie
    cfg_d <- PARAM_CONFIG$durata
    cfg_p <- PARAM_CONFIG$plafon
    validate(
      need(valid_number(input$chirie_eur, cfg_c$min, cfg_c$max),
           cfg_c$error_msg(cfg_c$min, cfg_c$max)),
      need(valid_number(input$durata, cfg_d$min, 10000, TRUE),
           cfg_d$error_msg),
      need(isTRUE(input$fara_plafon) || valid_number(input$plafon, cfg_p$min, cfg_p$max),
           cfg_p$error_msg(cfg_p$min, cfg_p$max))
    )
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
             UI_STRINGS$tab1$pierdere_totala_fara(format(pierdere_totala_fara, big.mark = ".", decimal.mark = ","))),
          h4(style = "color:#495057;",
             UI_STRINGS$tab1$diff_sub_fara(luni))
        )
      } else {
        div(
          style = "text-align:center; padding:18px;",
          div(style = "margin-bottom: 10px;", period_pill),
          h2(style = "color:#b94a00; font-size:2.2em;",
             UI_STRINGS$tab1$pierdere_fara_plafon(format(pierdere_totala_fara, big.mark = ".", decimal.mark = ","))),
          h4(style = "color:#1e824c; font-weight:600;",
             UI_STRINGS$tab1$protectie_plafon(pl, format(economie_totala, big.mark = ".", decimal.mark = ","), luni_plafonate, luni))
        )
      }
    } else {
      # mod economie
      if (fara_pl || economie_totala == 0) {
        div(
          style = "text-align:center; padding:18px;",
          div(style = "margin-bottom: 10px;", period_pill),
          h3(style = "color:#b94a00;",
             if (fara_pl) UI_STRINGS$tab1$plafon_dezactivat
             else UI_STRINGS$tab1$plafon_inactiv(pl, luni)),
          h4(style = "color:#495057;", UI_STRINGS$tab1$ec_zero)
        )
      } else {
        div(
          style = "text-align:center; padding:18px;",
          div(style = "margin-bottom: 10px;", period_pill),
          h2(style = "color:#1e824c; font-size:2.4em;",
             UI_STRINGS$tab1$ec_total(format(economie_totala, big.mark = ".", decimal.mark = ","))),
          h4(style = "color:#2c3e50;",
             UI_STRINGS$tab1$ec_sub(luni, pl, luni_plafonate))
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
        geom_col(aes(y = plus_depreciere_fara_plafon, fill = UI_STRINGS$tab1$col_diff_luna),
                 alpha = 0.45, width = 0.6)
      if (nrow(df) > 1) {
        p <- p + geom_line(aes(y = plus_depreciere_fara_cum, color = UI_STRINGS$tab1$line_diff_fara),
                           linewidth = 1.3, linetype = "dashed")
      }
      p <- p + geom_point(aes(y = plus_depreciere_fara_cum, color = UI_STRINGS$tab1$line_diff_fara), size = 2)
      
      if (!fara_pl) {
        if (nrow(df) > 1) {
          p <- p + geom_line(aes(y = plus_depreciere_cu_cum, color = UI_STRINGS$tab1$line_diff_cu),
                             linewidth = 1.4)
        }
        p <- p + geom_point(aes(y = plus_depreciere_cu_cum, color = UI_STRINGS$tab1$line_diff_cu), size = 2)
      }
      
      # Marcare puncte de lovire a plafonului
      puncte_lovite <- df[df$loveste_plafon, , drop = FALSE]
      if (nrow(puncte_lovite) > 0) {
        p <- p +
          geom_point(data = puncte_lovite,
                     aes(y = plus_depreciere_fara_cum, shape = UI_STRINGS$tab1$pt_plafon_hit),
                     color = "#e74c3c", size = 4.5, stroke = 1.5) +
          scale_shape_manual(values = setNames(18, UI_STRINGS$tab1$pt_plafon_hit))
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
        UI_STRINGS$subtitles$chirie_eur_prefix(input$chirie_eur), " | ",
        UI_STRINGS$subtitles$curs_init(df$eur_ron[1]),
        forecast_subtitle_suffix(df$is_forecast, df$month)
      )

      p <- p +
        scale_fill_manual(values = setNames("#e74c3c", UI_STRINGS$tab1$col_diff_luna)) +
        scale_color_manual(values = setNames(c("#c0392b", "#27ae60"),
                                             c(UI_STRINGS$tab1$line_diff_fara, UI_STRINGS$tab1$line_diff_cu))) +
        labs(
          title = if (fara_pl) UI_STRINGS$tab1$title_fara_pl
                  else UI_STRINGS$tab1$title_cu_pl(pl),
          subtitle = subtitlu,
          x = UI_STRINGS$tab1$axis_x, y = UI_STRINGS$tab1$axis_y, fill = NULL, color = NULL, shape = NULL
        )
    } else {
      # Mod clasic: Economie generată de plafon
      p <- p +
        geom_col(aes(y = economie_luna, fill = UI_STRINGS$tab1$col_ec_luna),
                 alpha = 0.6, width = 0.6)
      if (nrow(df) > 1) {
        p <- p + geom_line(aes(y = economie_cum, color = UI_STRINGS$tab1$line_ec_cum),
                           linewidth = 1.4)
      }
      p <- p + geom_point(aes(y = economie_cum, color = UI_STRINGS$tab1$line_ec_cum), size = 3)
      
      puncte_lovite <- df[df$loveste_plafon, , drop = FALSE]
      if (nrow(puncte_lovite) > 0) {
        p <- p +
          geom_point(data = puncte_lovite,
                     aes(y = economie_cum, shape = UI_STRINGS$tab1$pt_ec_hit),
                     color = "#e74c3c", size = 4.5, stroke = 1.5) +
          scale_shape_manual(values = setNames(18, UI_STRINGS$tab1$pt_ec_hit))
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
        UI_STRINGS$subtitles$chirie_eur_luna(input$chirie_eur),
        forecast_subtitle_suffix(df$is_forecast, df$month)
      )

      p <- p +
        scale_fill_manual(values = setNames("#3498db", UI_STRINGS$tab1$col_ec_luna)) +
        scale_color_manual(values = setNames("#27ae60", UI_STRINGS$tab1$line_ec_cum)) +
        labs(
          title = if (fara_pl) UI_STRINGS$tab1$title_ec_off
                  else UI_STRINGS$tab1$title_ec_on(pl),
          subtitle = subtitlu,
          x = UI_STRINGS$tab1$axis_x, y = UI_STRINGS$tab1$axis_y, fill = NULL, color = NULL, shape = NULL
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
      df$month,
      sprintf("%.4f", df$eur_ron),
      sprintf("%.4f", df$curs_aplicat),
      ifelse(df$loveste_plafon, UI_STRINGS$tables$plafon_da, UI_STRINGS$tables$plafon_nu),
      sprintf("%.2f lei", df$plata_fara),
      sprintf("%.2f lei", df$plata_cu),
      sprintf("%.2f lei", df$plus_depreciere_fara_cum),
      sprintf("%.2f lei", df$economie_cum),
      check.names = FALSE
    )
    names(table) <- c(
      UI_STRINGS$tables$luna,
      UI_STRINGS$tables$curs_ref,
      UI_STRINGS$tables$curs_aplic,
      UI_STRINGS$tables$plafon_act,
      UI_STRINGS$tables$plata_fara,
      UI_STRINGS$tables$plata_cu,
      UI_STRINGS$tables$diff_cum,
      UI_STRINGS$tables$ec_cum
    )
    if ("payment_date" %in% names(df) || "rate_type" %in% names(df)) {
      type_label <- if ("rate_type" %in% names(df)) {
        ifelse(df$is_forecast, paste0(UI_STRINGS$tables$prognoza_prefix, df$rate_type),
               paste0(UI_STRINGS$tables$istoric_prefix, df$rate_type))
      } else ifelse(df$is_forecast, UI_STRINGS$tables$prognoza_label, UI_STRINGS$tables$istoric_label)
      p_date <- if ("payment_date" %in% names(df)) format(df$payment_date, "%d.%m.%Y") else paste0("01.", df$month)
      prefix_df <- data.frame(p_date, type_label, check.names = FALSE)
      names(prefix_df) <- c(UI_STRINGS$tables$data_platii, UI_STRINGS$tables$tip_curs)
      table <- cbind(prefix_df, table)
    }
    table
  }, striped = TRUE, hover = TRUE, spacing = "s", align = "r")

  # ============================================================
  #  TAB 2 — COMPARARE STRATEGII (PLĂȚI ÎN LEI LA CURS BNR)
  # ============================================================
  calc_tab2 <- reactive({
    p <- parametri()
    cfg_n <- PARAM_CONFIG$avans_bloc
    cfg_g <- PARAM_CONFIG$garantie_luni
    cfg_r <- PARAM_CONFIG$restituire_pct
    valid_int <- function(x, low, high) {
      length(x) == 1 && is.finite(x) && x >= low && x <= high && x == floor(x)
    }
    validate(
      need(valid_int(input$c2_n, cfg_n$min, cfg_n$max),
           cfg_n$error_msg(cfg_n$min, cfg_n$max)),
      need(valid_int(input$c2_avans_luni, cfg_g$min, cfg_g$max),
           cfg_g$error_msg(cfg_g$min, cfg_g$max)),
      need(length(input$c2_restituire_pct) == 1 && is.finite(input$c2_restituire_pct) &&
             input$c2_restituire_pct >= cfg_r$min && input$c2_restituire_pct <= cfg_r$max,
           cfg_r$error_msg(cfg_r$min, cfg_r$max))
    )
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
              UI_STRINGS$tab2$card1_title),
          div(style = "font-size: 26px; font-weight: 800; color: #dc3545; margin: 4px 0;",
              paste0(format(pierdere_s1, big.mark = ".", decimal.mark = ","), " lei")),
          div(style = "font-size: 12px; color: #842029;", UI_STRINGS$tab2$card1_note)
        ),
        div(
          style = "flex: 1; min-width: 240px; background: #f0f7ff; border: 1px solid #b6d4fe; border-radius: 8px; padding: 14px; text-align: center;",
          div(style = "font-size: 13px; color: #084298; font-weight: 600; text-transform: uppercase;",
              UI_STRINGS$tab2$card2_title(r$n)),
          div(style = "font-size: 26px; font-weight: 800; color: #0d6efd; margin: 4px 0;",
              paste0(format(ec_avans, big.mark = ".", decimal.mark = ","), " lei")),
          div(style = "font-size: 12px; color: #084298;",
              UI_STRINGS$tab2$card2_note(pierdere_s3))
        ),
        if (!r$fara_pl) {
          div(
            style = "flex: 1; min-width: 240px; background: #e8f8f5; border: 1px solid #a3e4d7; border-radius: 8px; padding: 14px; text-align: center;",
            div(style = "font-size: 13px; color: #0e6251; font-weight: 600; text-transform: uppercase;",
                UI_STRINGS$tab2$card3_title(r$n, r$pl)),
            div(style = "font-size: 26px; font-weight: 800; color: #117a65; margin: 4px 0;",
                paste0(format(ec_av_plaf, big.mark = ".", decimal.mark = ","), " lei")),
            div(style = "font-size: 12px; color: #117a65;",
                UI_STRINGS$tab2$card3_note(pierdere_s4, luni_plaf))
          )
        }
      )
    )
  })

  output$plot_comparare <- renderPlot({
    r <- calc_tab2()

    tip_diff <- UI_STRINGS$tab2$tip_diff
    tip_ec   <- UI_STRINGS$tab2$tip_ec

    strat_s1 <- UI_STRINGS$tab2$strat_s1
    strat_s3_diff <- UI_STRINGS$tab2$strat_s3_diff(r$n)
    strat_s3_ec   <- UI_STRINGS$tab2$strat_s3_ec(r$n)

    df_plot <- data.frame(
      idx   = rep(r$luni, 3),
      val   = c(r$p1_cum, r$p3_cum, r$ec_s3),
      tip   = rep(c(tip_diff, tip_diff, tip_ec), each = length(r$luni)),
      strat = rep(c(
        strat_s1,
        strat_s3_diff,
        strat_s3_ec
      ), each = length(r$luni))
    )

    if (!r$fara_pl) {
      strat_s4_diff <- UI_STRINGS$tab2$strat_s4_diff(r$n, r$pl)
      strat_s4_ec   <- UI_STRINGS$tab2$strat_s4_ec(r$n, r$pl)
      df_plafon <- data.frame(
        idx   = rep(r$luni, 2),
        val   = c(r$p4_cum, r$ec_s4),
        tip   = rep(c(tip_diff, tip_ec), each = length(r$luni)),
        strat = rep(c(
          strat_s4_diff,
          strat_s4_ec
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
      UI_STRINGS$subtitles$curs_pornire(r$curs_baza), " | ",
      UI_STRINGS$subtitles$chirie_eur_luna(input$chirie_eur), " | ",
      UI_STRINGS$subtitles$plata_directa,
      forecast_subtitle_suffix(r$is_forecast, r$months)
    )

    p <- p +
      scale_linetype_manual(
        name = UI_STRINGS$tab2$tip_indicator,
        values = setNames(c("solid", "dashed"), c(tip_diff, tip_ec))
      ) +
      scale_color_brewer(palette = "Set1", name = UI_STRINGS$tab2$strat_legend) +
      labs(
        title = UI_STRINGS$tab2$plot_title,
        subtitle = subtitle_txt,
        x = UI_STRINGS$tab2$axis_x, y = UI_STRINGS$tab2$plot_y_lab
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
      c(
        UI_STRINGS$tab2$strat_name_1,
        UI_STRINGS$tab2$strat_name_2(r$fara_pl, r$pl),
        UI_STRINGS$tab2$strat_name_3(r$n),
        UI_STRINGS$tab2$strat_name_4(r$fara_pl, r$n, r$pl)
      ),
      round(c(
        tail(r$s1_cum, 1), tail(r$s2_cum, 1),
        tail(r$s3_cum, 1), tail(r$s4_cum, 1)
      ), 2),
      round(c(
        tail(r$p1_cum, 1), tail(r$p2_cum, 1),
        tail(r$p3_cum, 1), tail(r$p4_cum, 1)
      ), 2),
      round(c(
        0, tail(r$ec_s2, 1),
        tail(r$ec_s3, 1), tail(r$ec_s4, 1)
      ), 2),
      round(r$avans_ron, 2),
      round(r$avans_restituit, 2),
      round(r$avans_net, 2),
      round(c(
        0, tail(r$ec_s2, 1), tail(r$ec_s3, 1), tail(r$ec_s4, 1)
      ) + r$avans_net[1] - r$avans_net, 2),
      round(c(
        tail(r$s1_cum, 1), tail(r$s2_cum, 1),
        tail(r$s3_cum, 1), tail(r$s4_cum, 1)
      ) + r$avans_net, 2),
      check.names = FALSE
    )
    names(tabel_df) <- c(
      UI_STRINGS$tables$col_strat,
      UI_STRINGS$tables$col_total,
      UI_STRINGS$tables$col_diff_l1,
      UI_STRINGS$tables$col_ec_ch,
      UI_STRINGS$tables$col_gar_in,
      UI_STRINGS$tables$col_gar_rec,
      UI_STRINGS$tables$col_gar_net,
      UI_STRINGS$tables$col_ec_net,
      UI_STRINGS$tables$col_cost_fin
    )

    if (r$fara_pl) {
      tabel_df <- tabel_df[c(1, 3), ]
    }

    tabel_df
  }, striped = TRUE, hover = TRUE, spacing = "s", align = "r")
}

# ---- RUN ---------------------------------------------------
shinyApp(ui, server)
