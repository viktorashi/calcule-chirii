Usage:

Instalați [just](https://github.com/casey/just), și R cum vă place (eu îl am prin `mise`) căutați voi pe net lol
și doar

```bash
just
```

----------

Am văzut ca (cel puțin în Cluj) tot mai mulți prorietari cer chiria in EURO in contract (sau echivalent BNR, care trebuie recalculat în fiecare lună) și refuză sa aiba un pret fix in RON. Așa că m-am gândit la un tool să vă ajute să negociați mai bine niște clauze mai deștepte în contractele de închiere:
Posibilitatea plății în avans cu N luni (pentru a amortiza deprecierea)
și / sau cererea unui plafon (care mă gândesc că trebuie ales cât să nu dea prea tare la ochi)

Am pus-o pe Sonnetă să facă programu ăsta, care momentan se ruleaza doar local din R, dar poate îl hostez mai încolo pe undeva să vă fie mai ușor.

Folosește prognozele RON / EUR pentru următoarele 36 de luni de când postez asta (overwritable printr-un .csv dropdown, but use it while it's fresh) de la Consensul CNP (Comisia Națională de Strategie și Prognoză), CFA România, BCR și ING (whoever tf those are lol)

Screenshoturile ar fi economiile mele reale dupa 3 ani de chirie dacă chiar zice da proprietarul vai mâncalaș (negociabil)

Nu am explitat tot ce-i pe-acolo dar ar trebui să fie (sper) self-explanatory.

----------

### Slopuială scrisă de dânsu

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

Deschide în browser: **<http://127.0.0.1:7474**>

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
