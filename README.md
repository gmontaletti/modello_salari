# Determinanti e Differenziali Salariali in Italia

**Un'Analisi Integrata 2000-2025**

Analisi delle determinanti salariali in Italia mediante metodologia Vector Error Correction Model (VECM), analisi microdati RACLI, e indicatori di domanda di lavoro. Il progetto integra tre prospettive complementari (macro, micro, domanda) per fornire un quadro completo del "puzzle salariale italiano".

## Autore

**Giampaolo Montaletti**
- Email: giampaolo.montaletti@gmail.com
- GitHub: [gmontaletti](https://github.com/gmontaletti)
- ORCID: [0009-0002-5327-1122](https://orcid.org/0009-0002-5327-1122)

## Obiettivi

Il progetto analizza le dinamiche salariali italiane attraverso tre linee di ricerca complementari:

1. **Analisi VECM** - Modello macroeconomico delle relazioni di lungo periodo tra salari, prezzi, produttività e disoccupazione
2. **Analisi RACLI** - Disaggregazione territoriale e settoriale dei salari mediante dati ISTAT sui redditi da lavoro
3. **Analisi Imprese** - Dinamiche salariali a livello di impresa per dimensione aziendale e settore

## Metodologia

### Modello VECM

Equazione di lungo periodo:

```
w_t - p_t = β₀ + β₁(y/l)_t + β₂u_t + ε_t
```

Dove:
- `w_t` = log salari nominali (indice 2015=100)
- `p_t` = log indice prezzi (IPCA)
- `(y/l)_t` = log produttività del lavoro (€/ora)
- `u_t` = tasso di disoccupazione (%)

Parametri attesi:
- β₁ (elasticità alla produttività): 0.7-0.9
- β₂ (semi-elasticità alla disoccupazione): -1.5%
- α_w (velocità aggiustamento salari): -0.15

### Test econometrici

- Stazionarietà: ADF, Phillips-Perron, KPSS
- Cointegrazione: Johansen (trace e max eigenvalue)
- Diagnostica: Portmanteau, ARCH-LM, Jarque-Bera
- Stabilità: CUSUM, rolling coefficients

## Paper Unificato

Il paper principale `paper_unificato.Rmd` consolida le analisi VECM, RACLI e imprese in un documento unico strutturato in 5 parti:

1. **Parte I**: Contesto teorico, fonti dati e metodologia
2. **Parte II**: Evidenza descrittiva (tendenze macro, differenziali, domanda lavoro)
3. **Parte III**: Analisi econometrica VECM e parametri tempo-varianti
4. **Parte IV**: Validazione macro-micro e scenari di policy
5. **Parte V**: Sintesi risultati e implicazioni di policy

Include executive summary bilingue (inglese/italiano) e appendici tecniche per la riproducibilità.

```bash
# Generazione paper unificato
Rscript -e "rmarkdown::render('paper_unificato.Rmd')"
```

## Struttura del progetto

```
modello_salari/
├── paper_unificato.Rmd     # Paper consolidato (output principale)
├── 00_download_data.R      # Download dati (ISTAT, RACLI, shapefile)
│
├── vecm_prep.R             # Pipeline VECM
├── vecm.Rmd                # Report VECM (componente)
├── output/vecm/            # Risultati VECM
│
├── racli_prep.R            # Pipeline RACLI
├── racli.Rmd               # Report RACLI (componente)
├── output/racli/           # Risultati RACLI
│
├── imprese_prep.R          # Pipeline imprese
├── imprese.Rmd             # Report imprese (componente)
├── output/imprese/         # Risultati imprese
│
├── data/                   # Dati input
├── meta/                   # Metadata e codelist
├── R/                      # Funzioni condivise
└── references.bib          # Bibliografia
```

## Esecuzione

```bash
# 1. Download dati (eseguire una volta)
Rscript 00_download_data.R

# 2. Pipeline di analisi
Rscript racli_prep.R           # → output/racli/
Rscript vecm_prep.R            # → output/vecm/
Rscript imprese_prep.R         # → output/imprese/

# 3. Generazione paper unificato (output principale)
Rscript -e "rmarkdown::render('paper_unificato.Rmd')"

# 4. Report individuali (opzionale)
Rscript -e "rmarkdown::render('racli.Rmd')"
Rscript -e "rmarkdown::render('vecm.Rmd')"
Rscript -e "rmarkdown::render('imprese.Rmd')"
```

## Output principali

### VECM (`output/vecm/`)

| File | Contenuto |
|------|-----------|
| `modello_salari_results.rds` | Risultati completi: stime VECM, test diagnostici, IRF, FEVD, scenari |
| `dati_istat.rds` | Serie storiche trimestrali 1995-2024 |
| `grafici/irf_*.png` | Impulse Response Functions con intervalli di confidenza |
| `grafici/fevd.png` | Forecast Error Variance Decomposition |
| `grafici/serie_storiche.png` | Evoluzione variabili endogene |

### RACLI (`output/racli/`)

| File | Contenuto |
|------|-----------|
| `indici_disuguaglianza.rds` | Gini, rapporti interdecilici per provincia |
| `gap_gender*.rds` | Differenziali retributivi di genere |
| `model_ols*.rds` | Modelli OLS con interazioni settore-territorio |
| `map_data_province.rds` | Dati geografici per cartografia |
| `grafici/17_mappa_salari_province_2022.png` | Mappa salari mediani provinciali |
| `grafici/14_beta_convergenza.png` | Analisi beta-convergenza territoriale |

### Imprese (`output/imprese/`)

| File | Contenuto |
|------|-----------|
| `imprese_clean.rds` | Dataset pulito imprese |
| `stat_descrittive_*.rds` | Statistiche per settore e dimensione |
| `volatilita_settori.rds` | Analisi volatilità settoriale |
| `grafici/21_evoluzione_indice_totale.png` | Evoluzione indice salariale nazionale |
| `grafici/24_confronto_dimensione_aziendale.png` | Gap salariale per dimensione impresa |

## Dipendenze

Pacchetti R principali:

- **Econometria**: `vars`, `urca`, `tsDyn`, `lmtest`, `sandwich`
- **Dati**: `dplyr`, `tidyr`, `data.table`, `zoo`
- **Visualizzazione**: `ggplot2`, `sf`, `corrplot`
- **ISTAT**: `istatlab`, `situas`
- **Report**: `rmarkdown`, `stargazer`

L'installazione automatica è gestita dalla funzione `install_if_missing()` all'avvio degli script.

## Limitazioni

1. I dati VECM sono calibrati su valori ISTAT, non estratti direttamente via API
2. L'aggregazione nazionale maschera eterogeneità settoriale e regionale
3. Possibile endogeneità nella variabile produttività
4. Stabilità parametri assunta costante tra regimi

## Citazione

```bibtex
@software{montaletti2025salari,
  author = {Montaletti, Giampaolo},
  title = {Determinanti e Differenziali Salariali in Italia: Un'Analisi Integrata 2000-2025},
  year = {2025},
  month = {1},
  url = {https://github.com/gmontaletti/modello_salari}
}
```

## Licenza

MIT License
