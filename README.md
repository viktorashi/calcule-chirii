# Simulator Chirie EUR→RON — Instrucțiuni

## Cerințe
- R (≥ 4.0)
- Pachete: `shiny`, `ggplot2`, `dplyr`, `scales`, `bslib`

## Instalare pachete (o singură dată)
```r
install.packages(c("shiny", "ggplot2", "dplyr", "scales", "bslib"),
                 repos = "https://cloud.r-project.org")
```

## Pornire aplicație
```bash
# din directorul proiectului:
Rscript -e "shiny::runApp('.', port=7474, launch.browser=FALSE)"
```

Sau din R / Radian:
```r
shiny::runApp(".", port = 7474, launch.browser = FALSE)
```

Deschide în browser: **http://127.0.0.1:7474**

## Structura fișierelor
| Fișier | Rol |
|---|---|
| `app.R` | Aplicația Shiny completă |
| `date_curs.csv` | Prognozele EUR/RON (Gov Capital, 10 sept 2026) |
| `README.md` | Acest fișier |

## Date curs
- Sursa: [Gov Capital EUR/RON](https://gov.capital/forex-forecast/eur-ron/), coloana „Forecast close"
- Consultate: 10 septembrie 2026
- Acoperire: sept 2026 – aug 2027 (12 luni)
- **Nu sunt prognozele BNR** și nu sunt demonstrate a fi cele mai precise.
- Valorile sunt de **sfârșit de lună** — folosite ca curs la scadență este doar o aproximație.
- Simularea se limitează la lunile din dataset; nu se extrapolează în tăcere.

## Excluderi explicite din model
- Dobânzi / randament pe sumele deținute
- Indexarea chiriei
- Fluctuații intra-lunare ale cursului
- Orice costuri comune tuturor variantelor (utilități, întreținere etc.)

## Note juridice
- Procentul de restituire a avansului este o **ipoteză economică configurabilă**, nu o concluzie juridică sau contractuală.
