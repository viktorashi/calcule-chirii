# Pure calculations, shared by the UI and regression checks.
read_rates <- function(path) {
  df <- tryCatch(read.csv(path, stringsAsFactors = FALSE),
                 error = function(e) stop("CSV-ul nu poate fi citit.", call. = FALSE))
  if (!all(c("month", "eur_ron") %in% names(df)) || nrow(df) == 0)
    stop("CSV-ul trebuie să conțină month și eur_ron și cel puțin o lună.", call. = FALSE)
  df$month <- trimws(as.character(df$month))
  if (anyNA(df$month) || any(!grepl("^[0-9]{4}-(0[1-9]|1[0-2])$", df$month)))
    stop("Lunile din CSV trebuie să aibă formatul AAAA-LL.", call. = FALSE)
  if (anyDuplicated(df$month)) stop("CSV-ul conține luni duplicate.", call. = FALSE)
  df$eur_ron <- suppressWarnings(as.numeric(df$eur_ron))
  if (any(!is.finite(df$eur_ron) | df$eur_ron <= 0))
    stop("Toate cursurile trebuie să fie numere finite pozitive.", call. = FALSE)
  df <- df[order(df$month), c("month", "eur_ron"), drop = FALSE]
  dates <- as.Date(paste0(df$month, "-01"))
  if (anyNA(dates) || !identical(format(seq(dates[1], tail(dates, 1), by = "month"), "%Y-%m"), df$month))
    stop("CSV-ul trebuie să conțină luni consecutive, fără goluri.", call. = FALSE)
  rownames(df) <- NULL
  df
}

trunca <- function(df, durata) {
  df[seq_len(min(durata, nrow(df))), , drop = FALSE]
}

calculate_cap <- function(df, chirie, pl = Inf) {
    fara_pl <- is.infinite(pl)
    curs_baza <- df$eur_ron[1]
    
    df$curs_aplicat       <- pmin(df$eur_ron, pl)
    df$plata_fara         <- chirie * df$eur_ron
    df$plata_cu           <- chirie * df$curs_aplicat
    
    # Economie datorată plafonului față de plata fără plafon
    df$economie_luna      <- pmax(chirie * (df$eur_ron - pl), 0)
    df$economie_cum       <- cumsum(df$economie_luna)
    
    # Diferențe semnate față de cursul primei luni:
    # Fără plafon:
    df$plus_depreciere_fara_plafon <- chirie * (df$eur_ron - curs_baza)
    df$plus_depreciere_fara_cum    <- cumsum(df$plus_depreciere_fara_plafon)
    
    # Cu plafon, față de același curs inițial neplafonat:
    df$plus_depreciere_cu_plafon   <- chirie * (df$curs_aplicat - curs_baza)
    df$plus_depreciere_cu_cum      <- cumsum(df$plus_depreciere_cu_plafon)
    
    # Indicator: atinge sau depășește plafonul?
    df$loveste_plafon     <- !fara_pl & (df$eur_ron >= pl)
    df$luna_idx           <- seq_len(nrow(df))
    df
}

calculate_strategies <- function(df, chirie, pl = Inf, n = 3, avans_l = 1, rest_pct = 1) {
    fara_pl <- is.infinite(pl)
    luni      <- seq_len(nrow(df))
    curs      <- df$eur_ron
    curs_baza <- curs[1]

    # Curs plafonat
    curs_ap <- pmin(curs, pl)

    # Garanție inițial (plătit la cursul primei luni)
    avans_ron       <- avans_l * chirie * c(curs[1], min(curs[1], pl), curs[1], min(curs[1], pl))
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
    for (bs in bloc_start) {
      be  <- min(bs + n - 1, length(luni))
      s4_plati[bs:be]     <- chirie * pmin(curs[bs], pl)
    }

    # Cât s-ar fi plătit dacă leul rămânea neschimbat la cursul lunii 1 (bază)
    plata_baza_luna <- chirie * curs_baza

    # Diferențe semnate față de chiria la cursul inițial neplafonat:
    p1_luna <- s1_plati - plata_baza_luna
    p2_luna <- s2_plati - plata_baza_luna
    p3_luna <- s3_plati - plata_baza_luna
    p4_luna <- s4_plati - plata_baza_luna

    # Economii realizate față de plata de bază (Strategia 1: Lunar fără plafon)
    # Cât economisești dacă folosești fiecare dintre opțiuni:
    ec_s2 <- cumsum(s1_plati - s2_plati) # din plafon
    ec_s3 <- cumsum(s1_plati - s3_plati) # din blocuri n luni (plată anticipată)
    ec_s4 <- cumsum(s1_plati - s4_plati) # din blocuri + plafon

    # Plafon aplicat cursului blocat, pentru lunile acoperite de bloc
    loveste_plafon <- !fara_pl & (s3_curs_bloc >= pl)

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
      # Chirie cumulată repartizată pe lunile acoperite
      s1_cum          = cumsum(s1_plati),
      s2_cum          = cumsum(s2_plati),
      s3_cum          = cumsum(s3_plati),
      s4_cum          = cumsum(s4_plati),
      # Diferențe semnate cumulate față de cursul inițial
      p1_cum          = cumsum(p1_luna),
      p2_cum          = cumsum(p2_luna),
      p3_cum          = cumsum(p3_luna),
      p4_cum          = cumsum(p4_luna),
      # Economii cumulate vs Strategia 1
      ec_s2           = ec_s2,
      ec_s3           = ec_s3,
      ec_s4           = ec_s4,
      # Garanție
      avans_ron       = avans_ron,
      avans_restituit = avans_restituit,
      avans_net       = avans_net
    )
}
