![poza1](./ss-1.png)
![poza2](./ss-2.png)

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

Folosește seria EUR/RON din `date_curs.csv`, etichetată în proiect ca prognoză Gov Capital. Nu sunt cursuri BNR observate și proveniența valorilor nu este verificată de aplicație. Poți încărca propria serie CSV.

Screenshoturile arată economii simulate, condiționate de cursurile din CSV și de clauzele acceptate de proprietar.

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
| `app.R` | Interfața și serverul Shiny |
| `R/model.R` | Validarea CSV și formulele |
| `tests/run.R` | Verificări de calcul și integrare Shiny |
| `date_curs.csv` | Prognozele EUR/RON (Gov Capital, 10 sept 2026) |
| `README.md` | Acest fișier |

## Date curs

- Sursa: [Gov Capital EUR/RON](https://gov.capital/forex-forecast/eur-ron/), coloana „Forecast close"
- Consultate: 10 septembrie 2026
- Acoperire: sept 2026 – aug 2029 (36 luni)
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

## Luna de început și interpretarea rezultatelor

- Selectorul afișează luni și ani, cu luna curentă implicită. Cursul inițial vine din rândul corespunzător din CSV, iar simularea începe acolo.
- Durata se limitează la lunile rămase. CSV-ul trebuie să aibă luni consecutive și unice în format `AAAA-LL`, cu cursuri finite și strict pozitive; fișierele invalide afișează o eroare.
- Diferența față de luna 1 este **semnată**: costul chiriei minus costul la cursul inițial constant. Scăderile cursului compensează creșterile.
- Economia unei strategii este chiria lunară fără plafon minus chiria acelei strategii. O economie negativă înseamnă că strategia costă mai mult.
- Avansul blochează cursul la începutul fiecărui bloc; ultimul bloc acoperă doar lunile rămase. Graficele repartizează costul pe lunile acoperite, nu reprezintă calendarul transferurilor bancare.
- Garanția este calculată separat pentru fiecare strategie, cu sau fără plafon, și restituită în RON ca procent din suma inițială. Cardurile și graficele compară chiria; tabelul prezintă și garanția și economia netă finală.
- Calculele păstrează precizia internă și rotunjesc rezultatele afișate la bani; contractele care rotunjesc fiecare plată pot produce diferențe de câțiva bani.

## Verificare

Din directorul proiectului: `Rscript tests/run.R`.
Verificările acoperă cursuri crescătoare, descrescătoare și oscilante, plafon,
plată în avans, bloc final incomplet, garanții, CSV-uri invalide și selectarea lunii în Shiny.
