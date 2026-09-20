# ============================================================
#  R/config.R — Configurare centralizată parametri și texte UI
# ============================================================

# Limite numerice, valori implicite, pași și validări pentru inputs
PARAM_CONFIG <- list(
  chirie = list(
    label = "Chirie lunară (EUR)",
    min = 50, max = 5000, default = 500, step = 25,
    error_msg = function(min, max) sprintf("Chiria trebuie să fie între %d și %d EUR.", min, max)
  ),
  durata = list(
    label = "Durata șederii (luni)",
    min = 1, max = 36, default = 36, step = 1,
    error_msg = "Durata trebuie să fie un număr întreg pozitiv."
  ),
  plafon = list(
    label = "Plafon curs (RON/EUR)",
    min = 4.80, max = 6.00, default = 5.30, step = 0.01,
    error_msg = function(min, max) sprintf("Plafonul trebuie să fie între %.2f și %.2f RON/EUR.", min, max)
  ),
  avans_bloc = list(
    label = "n = luni achitate odată (în avans). Echivalent cu renegociere o data la `n` luni",
    min = 1, max = 36, default = 3, step = 1,
    error_msg = function(min, max) sprintf("Numărul de luni în avans trebuie să fie întreg, între %d și %d.", min, max)
  ),
  garantie_luni = list(
    label = "Garanție inițială (luni chirii)",
    min = 0, max = 6, default = 1, step = 1,
    error_msg = function(min, max) sprintf("Garanția trebuie să fie între %d și %d luni întregi.", min, max)
  ),
  restituire_pct = list(
    label = "% din garanție recuperat la plecare",
    min = 0, max = 100, default = 100, step = 5,
    error_msg = function(min, max) sprintf("Restituirea trebuie să fie între %d și %d%%.", min, max)
  )
)

# Sintagme comune refolosite în textele UI (deduplicare)
difference_first_month <- "Diferență cumulată față de luna 1"
diff_luna_1            <- "Diferență cumulată față de luna 1"
diff_vs_l1             <- "Diferență cumulată vs luna 1"
fara_plafon_suffix     <- "(fără plafon)"

# Dicționar centralizat pentru toate textele și etichetele afișate în UI
UI_STRINGS <- list(
  app_title = "🏠 Simulator Chirie EUR→RON",
  
  tabs = list(
    tab1 = "📊 Plafon (vedere simplă)",
    tab2 = "⚖️ Comparare strategii"
  ),
  
  sidebar = list(
    general_header   = "⚙️ Parametri Generali",
    luna_start_label = "Prima lună de chirie",
    snap_today_btn   = "📅 Luna curentă (Azi)",
    plafon_header    = "Plafon negociat curs",
    fara_plafon_label = "Fără plafon (Plafon = ∞)",
    sursa_header     = "Date curs EUR/RON",
    sursa_label      = "Sursa datelor",
    sursa_choices    = c(
      "BNR live + prognoze ING" = "live",
      "CSV propriu"             = "csv",
      "CSV existent (offline)"  = "local"
    ),
    refresh_btn      = "Actualizează acum",
    live_help        = "Mod live: poți alege orice lună de pornire (istoric oficial BNR din 2018 până în prezent, sau pornire în viitor cu prognoze ING). Cursurile viitoare sunt estimate între reperele trimestriale ING.",
    csv_upload_label = "Încarcă CSV propriu curs",
    csv_help         = "CSV: month (AAAA-LL), eur_ron. CSV-ul existent este un scenariu static, cu proveniență neverificată.",
    disclaimer       = "Cursurile viitoare și economiile sunt estimări, nu garanții."
  ),
  
  validation = list(
    csv_upload_needed  = "Încarcă un CSV pentru această sursă.",
    luna_start_needed  = "Alege o lună disponibilă din calendar.",
    luna_csv_not_found = "Luna aleasă nu există în CSV. Alege o lună disponibilă sau încarcă alte date."
  ),

  status = list(
    date_istorice_title = "Date istorice:",
    date_istorice_desc  = function(hist_min) sprintf("Cursuri lunare oficiale BNR disponibile din %s până în prezent.", hist_min),
    curs_csv_prefix     = "Curs din CSV: ",
    curs_disponibil_suf = function(luni) sprintf(" RON/EUR · %d luni disponibile", luni)
  ),
  
  tab1 = list(
    options_header = "Opțiuni afișare Tab 1",
    mode_label     = "Perspectivă afișare:",
    mode_choices   = c(
      "Diferență netă față de cursul primei luni" = "pierdere",
      "Economie generată de plafon"              = "economie"
    ),
    mode_help      = "Parametrii generali de curs, chirie și plafon sunt sincronizați din bara laterală principală.",
    
    # Carduri / Big Number UI
    pierdere_totala_fara = function(amt) sprintf("Pierderi totale cumulative: %s lei", amt),
    pierdere_fara_plafon = function(amt) sprintf("Fără plafon, pierderi totale cumulative: %s lei", amt),
    diff_total_fara      = function(amt) sprintf("Pierderi totale cumulative: %s lei", amt),
    diff_total_cu        = function(amt) sprintf("Fără plafon, pierderi totale cumulative: %s lei", amt),
    diff_sub_fara        = function(luni) sprintf("în %d luni față de cursul primei luni (fără niciun plafon de protecție)", luni),
    protectie_plafon  = function(pl, ec, atins, total) {
      sprintf("Plafonul de %s RON/EUR te protejează: economisești %s lei (plafon atins în %d din %d luni)",
              pl, ec, atins, total)
    },
    plafon_dezactivat = "Plafonul este dezactivat (∞). Nu există economie de plafonare.",
    plafon_inactiv    = function(pl, luni) sprintf("Plafonul de %s RON/EUR nu generează economii în cele %d luni.", pl, luni),
    ec_zero           = "→ Economia datorată plafonului este 0 lei.",
    ec_total          = function(amt) sprintf("Economisești în total %s lei", amt),
    ec_sub            = function(luni, pl, atins) sprintf("în %d luni, cu plafon %s RON/EUR (activ în %d luni)", luni, pl, atins),
    
    # Grafic
    title_fara_pl  = sprintf("%s %s", difference_first_month, fara_plafon_suffix),
    title_cu_pl    = function(pl) sprintf("%s, fără și cu plafon de %s RON/EUR", difference_first_month, pl),
    title_ec_off   = "Plafon dezactivat (Economie = 0)",
    title_ec_on    = function(pl) sprintf("Economie datorată plafonului de %s RON/EUR", pl),
    
    col_diff_luna  = sprintf("Diferență lunară față de luna 1 %s", fara_plafon_suffix),
    line_diff_fara = sprintf("%s %s", difference_first_month, fara_plafon_suffix),
    line_diff_cu   = sprintf("%s (cu plafon)", difference_first_month),
    pt_plafon_hit  = "⚡ Plafon atins/depășit (curs >= plafon)",
    col_ec_luna    = "Economie lunară",
    line_ec_cum    = "Economie cumulată",
    pt_ec_hit      = "⚡ Plafon atins/depășit",
    
    axis_x = "Luna",
    axis_y = "RON"
  ),
  
  tab2 = list(
    options_header  = "Opțiuni Strategii",
    options_help    = "Primești salariul în RON și achiți direct proprietarului în RON la cursul oficial BNR, fără comisioane bancare/valutare.",
    garantie_header = "Garanție (RON)",
    table_header    = "Sinteză comparativă strategii (pentru durata selectată)",
    
    # Carduri KPI
    card1_title = sprintf("%s (lunar %s)", diff_luna_1, "fără plafon"),
    card1_note  = "Pozitiv = cost suplimentar; negativ = economie",
    card2_title = function(n) sprintf("Economie: Plată în avans (blocuri %d luni)", n),
    card2_note  = function(diff) sprintf("%s: %s lei. Economie negativă = cost în plus.", diff_luna_1, diff),
    card3_title = function(n, pl) sprintf("Economie: Avans %d luni + Plafon %s", n, pl),
    card3_note  = function(diff, luni) sprintf("%s: %s lei | Plafon aplicat blocurilor pentru %d luni", diff_luna_1, diff, luni),
    
    # Grafic
    plot_title    = "Diferențe față de luna 1 și economii față de plata lunară (RON)",
    plot_y_lab    = "RON (economie negativă = cost suplimentar)",
    axis_x        = "Luna",
    tip_indicator = "Tip indicator",
    tip_diff      = diff_vs_l1,
    tip_ec        = "Bani economisiți",
    strat_legend  = "Strategie & Impact",
    strat_s1      = sprintf("Diferență: Lunar %s", fara_plafon_suffix),
    strat_s3_diff = function(n) sprintf("⚠️ %s: Blocuri %d luni (avans BNR)", diff_vs_l1, n),
    strat_s3_ec   = function(n) sprintf("✅ Economie realizată prin plata în avans pe %d luni", n),
    strat_s4_diff = function(n, pl) sprintf("🛡️ %s: Blocuri %d luni + Plafon %s", diff_vs_l1, n, pl),
    strat_s4_ec   = function(n, pl) sprintf("✨ Economie totală: Blocuri %d luni + Plafon %s", n, pl),
    
    # Denumiri strategii în tabel
    strat_name_1 = sprintf("1. Plată lunară %s", fara_plafon_suffix),
    strat_name_2 = function(fara_pl, pl) if (fara_pl) "2. Plată lunară (plafon inactiv)" else sprintf("2. Plată lunară (cu plafon %s)", pl),
    strat_name_3 = function(n) sprintf("3. Blocuri %d luni %s", n, fara_plafon_suffix),
    strat_name_4 = function(fara_pl, n, pl) if (fara_pl) sprintf("4. Blocuri %d luni %s", n, fara_plafon_suffix) else sprintf("4. Blocuri %d luni (cu plafon %s)", n, pl)
  ),
  
  tables = list(
    data_platii = "Data plății",
    tip_curs    = "Tip curs",
    luna        = "Luna",
    curs_ref    = "Curs de referință",
    curs_aplic  = "Curs aplic.",
    plafon_act  = "Plafon activ?",
    plafon_da   = "⚡ DA",
    plafon_nu   = "nu",
    plata_fara  = "Plată fără plafon",
    plata_cu    = "Plată cu plafon",
    diff_cum    = difference_first_month,
    ec_cum      = "Economie plafon cum.",
    prognoza_prefix = "🔮 ",
    istoric_prefix  = "🏛️ ",
    prognoza_label  = "🔮 Prognoză",
    istoric_label   = "🏛️ Istoric",
    
    # Antete tabel Tab 2
    col_strat    = "Strategie",
    col_total    = "Total RON chirie",
    col_diff_l1  = "Diferență vs cursul lunii 1",
    col_ec_ch    = "Economie chirie vs Lunar fără plafon",
    col_gar_in   = "Garanție inițială",
    col_gar_rec  = "Garanție recuperată",
    col_gar_net  = "Cost net garanție",
    col_ec_net   = "Economie netă inclusiv garanție",
    col_cost_fin = "Cost net final (+ Garanție netă)"
  ),
  
  period_pills = list(
    past_singular = "1 lună istorică reală (BNR)",
    past_plural   = function(n) sprintf("%d luni istorice reale (BNR)", n),
    fut_singular  = "1 lună prognoză viitoare (ING)",
    fut_plural    = function(n) sprintf("%d luni prognoză viitoare (ING)", n),
    all_past      = function(n) if (n == 1) "🏛️ Singura lună este dată istorică reală BNR" else sprintf("🏛️ Toate cele %d luni sunt date istorice reale BNR", n),
    all_fut       = function(n) if (n == 1) "🔮 Singura lună este prognoză / estimare viitoare" else sprintf("🔮 Toate cele %d luni sunt prognoze / estimări viitoare", n),
    mixed         = function(p, f) sprintf("⚖️ Perioadă mixtă: %s + %s", p, f)
  ),
  
  subtitles = list(
    chirie_eur_prefix = function(c) sprintf("Chirie: %s EUR", c),
    chirie_eur_luna   = function(c) sprintf("Chirie: %s EUR/lună", c),
    curs_init         = function(r) sprintf("Curs inițial: %.4f RON/EUR", r),
    curs_pornire      = function(r) sprintf("Curs la pornire: %.4f RON/EUR", r),
    plata_directa     = "Plată directă în RON la curs BNR",
    suffix_mixed      = function(m) sprintf(" | Zonă galbenă & inele: 🔮 Prognoză din %s", m),
    suffix_all_fut    = " | 🔮 Toate lunile sunt prognoze/estimări viitoare",
    suffix_all_past   = " | 🏛️ Toate lunile sunt date istorice reale BNR"
  ),
  
  info_bullets = list(
    title = "Mecanismul de plată în RON la curs BNR",
    b1 = "Fără comisioane bancare sau de schimb valutar (nu cumperi valută prin bănci/Revolut cu spread).",
    b2 = function(n) sprintf("Plata anticipată pe <b>%d luni</b> blochează cursul BNR din prima lună a blocului pentru toată perioada de %d luni.", n, n),
    b3 = "Dacă leul se depreciază în lunile 2, 3, etc., plata în avans te protejează, economisind diferența de curs.",
    b4 = "Ultimul bloc acoperă doar lunile rămase din ședere. Graficul repartizează chiria pe lunile acoperite, chiar dacă plata se face anticipat.",
    b5 = "Garanția se calculează la cursul inițial al fiecărei strategii și se restituie ca procent din aceeași sumă în RON.",
    b6 = "Dacă plafonul este activ, cursul aplicat este limitat la valoarea plafonului.",
    b7 = "Diferențele față de luna 1 includ și scăderile cursului. Economie negativă = cost suplimentar. Graficele și cardurile exclud garanția; tabelul include și costul ei net."
  )
)
