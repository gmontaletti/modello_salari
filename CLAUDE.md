# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Econometric model analyzing wage determinants in Italy (1995-2024) using Vector Error Correction Model (VECM) methodology. Based on Giampaolo Montaletti's research framework published on Il Sussidiario (2009-2025).

## Project Structure

```
modello_salari/
├── paper_unificato.Rmd     # OUTPUT PRINCIPALE
├── README.md
├── CLAUDE.md
├── references.bib
├── modello_salari.Rproj
├── renv.lock
├── .gitignore
├── .claude/
│
├── scripts/                # Pipeline di analisi
│   ├── 00_download_data.R
│   ├── vecm_prep.R
│   ├── racli_prep.R
│   ├── imprese_prep.R
│   └── preliminare_prep.R
│
├── reports/                # Report componenti (secondari)
│   ├── vecm.Rmd
│   ├── racli.Rmd
│   ├── imprese.Rmd
│   └── preliminare.Rmd
│
├── docs/                   # Documentazione di riferimento
│   ├── fonti_dati_istat.md
│   ├── documentazione_modello_salari.md
│   └── determinanti_salari_economie_occidentali.md
│
├── R/functions.R           # Funzioni condivise
├── data/                   # Dati input
├── meta/                   # Metadata e codelist
├── racli/                  # Dataflow RACLI grezzi
│
├── output/                 # Risultati (solo sottodirectory)
│   ├── vecm/               # VECM results + grafici/
│   ├── racli/              # RACLI results + grafici/
│   ├── imprese/            # Imprese results + grafici/
│   └── preliminare/        # Dati preliminari
│
├── wip/                    # WIP non tracciato (gitignored)
└── backup/                 # Script legacy (gitignored)
```

## Running the Analysis

```bash
# 1. Download all data (run once)
Rscript scripts/00_download_data.R

# 2. Run specific analysis pipelines
Rscript scripts/racli_prep.R           # RACLI analysis → output/racli/
Rscript scripts/vecm_prep.R            # VECM analysis → output/vecm/
Rscript scripts/imprese_prep.R         # Enterprise analysis → output/imprese/
Rscript scripts/preliminare_prep.R     # Preliminary analysis → output/preliminare/

# 3. Generate reports
Rscript -e "rmarkdown::render('paper_unificato.Rmd')"
Rscript -e "rmarkdown::render('reports/racli.Rmd')"
Rscript -e "rmarkdown::render('reports/vecm.Rmd')"
Rscript -e "rmarkdown::render('reports/imprese.Rmd')"
Rscript -e "rmarkdown::render('reports/preliminare.Rmd')"
```

## Main Scripts

| Script | Purpose |
|--------|---------|
| `scripts/00_download_data.R` | Downloads ISTAT, OECD, RACLI data, shapefiles, creates geographic mapping |
| `scripts/racli_prep.R` | Full RACLI pipeline: data prep, descriptives, inequality, clustering, regressions, visualizations, cartography |
| `scripts/vecm_prep.R` | Full VECM pipeline: data generation, stationarity, cointegration, estimation, diagnostics, IRF/FEVD, policy scenarios |
| `scripts/imprese_prep.R` | Enterprise analysis: data cleaning, statistics, visualizations |
| `scripts/preliminare_prep.R` | Preliminary data preparation |

## Key Functions

### VECM Pipeline (vecm_prep.R)

| Function | Purpose |
|----------|---------|
| `generate_istat_data()` | Creates calibrated quarterly data (120 obs, 1995-2024) |
| `run_stationarity_tests()` | ADF, Phillips-Perron, KPSS unit root tests |
| `run_cointegration_tests()` | Johansen trace and max eigenvalue tests |
| `estimate_vecm_model()` | VECM via `cajorls()`, extracts β and α |
| `run_model_diagnostics()` | Portmanteau, ARCH-LM, Jarque-Bera tests |
| `compute_impulse_responses()` | Bootstrap IRF (500 reps, 95% CI) |
| `compute_variance_decomposition()` | FEVD at 4, 8, 24 quarters |
| `simulate_scenarios()` | Baseline + policy shock simulations |

### RACLI Pipeline (racli_prep.R)

| Function | Purpose |
|----------|---------|
| `clean_racli_data()` | Data cleaning and ISTAT code matching |
| `calc_gini_from_deciles()` | Gini approximation from D1, D5, D9 |
| `validate_dataset()` | Data quality checks |
| `theme_salari()` | Custom ggplot2 theme |

## Data Variables

**VECM endogenous variables:**
- `log_w` - Log nominal wages (index, 2015=100)
- `log_p` - Log price index (IPCA)
- `log_prod` - Log labor productivity (€/hour)
- `u` - Unemployment rate (%)

**Extended model:** `cuneo` (tax wedge), `prec` (precarious contracts), `inv` (investments), `pil` (real GDP)

## Dependencies

Core packages: `vars`, `urca`, `tsDyn`, `lmtest`, `sandwich`, `dplyr`, `tidyr`, `zoo`, `ggplot2`, `sf`, `dagitty`, `ggdag`, `corrplot`, `stargazer`, `istatlab`, `situas`

Packages auto-install via `install_if_missing()` function at script start.

## Econometric Methodology

**Long-run equation:**
```
w_t - p_t = β₀ + β₁(y/l)_t + β₂u_t + ε_t
```

**Expected parameters:**
- β₁ (productivity elasticity): 0.7-0.9
- β₂ (unemployment semi-elasticity): -1.5%
- α_w (wage adjustment speed): -0.15

## Code Style

- Section headers: `# 1. Section Name -----`
- Primary language: Italian (comments, variable names, output)
- Graphics: ggplot2 with minimal theme, 300 dpi PNG output
- Fixed random seed (123) for reproducibility

## Cartografia e Codici Geografici

- **NON usare** mappature manuali o normalizzazione stringhe per join geografici
- **Usare** libreria `gmontaletti/situas` per shapefile ISTAT ufficiali
- **Usare** codici ITTER107 (da codelists) mappati a COD_UTS tramite COD_NUTS3_2024 di situas
- Match rate atteso: 100% (sotto 100% è inaccettabile)
- Join key: `COD_NUTS3_2024` (situas) ↔ `REF_AREA` (RACLI)

## Output Organization

Each analysis track has its own output directory:
- `output/racli/` - 50+ RDS files + `grafici/` with 20+ PNG
- `output/vecm/` - Model results RDS + `grafici/`
- `output/imprese/` - 15+ RDS files + `grafici/` with 13 PNG
- `output/preliminare/` - Prepared data for preliminary report

## Limitations

1. VECM data is simulated (calibrated to ISTAT values, not direct API)
2. National aggregation masks sectoral/regional heterogeneity
3. Some endogeneity in productivity variable
4. Parameter stability assumed across regimes
