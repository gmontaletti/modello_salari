# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Econometric model analyzing wage determinants in Italy (1995-2024) using Vector Error Correction Model (VECM) methodology. Based on Giampaolo Montaletti's research framework published on Il Sussidiario (2009-2025).

## Running the Analysis

```bash
# Run the main econometric model
Rscript modello_salari_montaletti.R

# Generate causal DAG visualizations
Rscript dag_salari_montaletti.R

# Generate all plots (requires data from main model)
Rscript visualizzazioni_salari.R
```

In R console, typical workflow:
```r
source("modello_salari_montaletti.R")
# Functions are executed sequentially from data generation through policy scenarios
```

## Architecture

**Main Scripts:**

- `modello_salari_montaletti.R` - Core econometric pipeline: data generation, stationarity tests, cointegration (Johansen), VECM estimation, diagnostics, IRF/FEVD, policy scenarios
- `dag_salari_montaletti.R` - Causal DAG visualization (16 variables, 30+ edges) using dagitty/ggdag
- `visualizzazioni_salari.R` - Plotting functions: time series, Phillips curve, IRF, FEVD, correlation heatmaps

**Documentation:**

- `documentazione_modello_salari.md` - Technical model specification
- `determinanti_salari_economie_occidentali.md` - Theoretical framework and literature review

## Key Functions in Main Model

| Section | Function | Purpose |
|---------|----------|---------|
| 1 | `generate_istat_data()` | Creates calibrated quarterly data (120 obs, 1995-2024) |
| 2 | `exploratory_analysis()` | Descriptive stats, period comparisons, correlations |
| 3 | `run_stationarity_tests()` | ADF, Phillips-Perron, KPSS unit root tests |
| 4 | `select_lag_order()` | Information criteria (AIC, BIC, HQ) |
| 5 | `run_cointegration_test()` | Johansen trace and max eigenvalue tests |
| 6 | `estimate_vecm()` | VECM via `cajorls()`, extracts β (cointegration) and α (adjustment) |
| 7 | `run_diagnostics()` | Portmanteau, ARCH-LM, Jarque-Bera tests |
| 8-9 | `compute_irf()`, `compute_fevd()` | Bootstrap IRF/FEVD (500 reps, 95% CI) |
| 11 | Policy scenarios | Baseline + alternative shock simulations |

## Data Variables

Core endogenous variables for VECM:
- `log_w` - Log nominal wages (index, 2015=100)
- `log_p` - Log price index (IPCA)
- `log_prod` - Log labor productivity (€/hour)
- `u` - Unemployment rate (%)

Extended model variables: `cuneo` (tax wedge), `prec` (precarious contracts), `inv` (investments), `pil` (real GDP)

## Dependencies

Core packages: `vars`, `urca`, `tsDyn`, `lmtest`, `sandwich`, `dplyr`, `tidyr`, `zoo`, `ggplot2`, `dagitty`, `ggdag`, `corrplot`, `stargazer`

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

## Limitations

1. Data is simulated (calibrated to ISTAT values, not direct API)
2. National aggregation masks sectoral/regional heterogeneity
3. Some endogeneity in productivity variable
4. Parameter stability assumed across regimes
