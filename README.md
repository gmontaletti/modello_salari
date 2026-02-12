# Determinanti e Differenziali Salariali in Italia

**Un'Analisi Integrata 2000-2025**

> **Progetto archiviato.** Questo repository non e' piu' mantenuto attivamente. L'analisi e' stata completata a gennaio 2025. Il codice e i risultati restano disponibili a scopo di consultazione e riproducibilita'.

## Autore

**Giampaolo Montaletti**
- Email: giampaolo.montaletti@gmail.com
- GitHub: [gmontaletti](https://github.com/gmontaletti)
- ORCID: [0009-0002-5327-1122](https://orcid.org/0009-0002-5327-1122)

## Output principale

Il documento principale del progetto e' `paper_unificato.Rmd`, che consolida le analisi VECM, RACLI e imprese in un unico paper strutturato in cinque parti:

1. **Parte I** - Contesto teorico, fonti dati e metodologia
2. **Parte II** - Evidenza descrittiva (tendenze macro, differenziali, domanda di lavoro)
3. **Parte III** - Analisi econometrica VECM e parametri tempo-varianti
4. **Parte IV** - Validazione macro-micro e scenari di policy
5. **Parte V** - Sintesi dei risultati e implicazioni di policy

Il paper include un executive summary bilingue (inglese/italiano) e appendici tecniche.

```bash
Rscript -e "rmarkdown::render('paper_unificato.Rmd')"
```

## Struttura del progetto

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
├── R/                      # Funzioni condivise
├── data/                   # Dati input
├── meta/                   # Metadata e codelist
├── racli/                  # Dataflow RACLI grezzi
│
├── output/                 # Risultati (solo sottodirectory)
│   ├── vecm/
│   ├── racli/
│   ├── imprese/
│   └── preliminare/
│
├── wip/                    # WIP non tracciato
└── backup/                 # Script legacy (non tracciato)
```

## Esecuzione

```bash
# 1. Download dati (eseguire una volta)
Rscript scripts/00_download_data.R

# 2. Pipeline di analisi
Rscript scripts/racli_prep.R
Rscript scripts/vecm_prep.R
Rscript scripts/imprese_prep.R

# 3. Paper unificato (output principale)
Rscript -e "rmarkdown::render('paper_unificato.Rmd')"

# 4. Report individuali (opzionale)
Rscript -e "rmarkdown::render('reports/racli.Rmd')"
Rscript -e "rmarkdown::render('reports/vecm.Rmd')"
Rscript -e "rmarkdown::render('reports/imprese.Rmd')"
```

## Metodologia

Il modello VECM stima la relazione di lungo periodo tra salario reale, produttivita' e disoccupazione:

```
w_t - p_t = β₀ + β₁(y/l)_t + β₂u_t + ε_t
```

Dove `w_t` indica il log dei salari nominali, `p_t` il log dell'indice prezzi IPCA, `(y/l)_t` il log della produttivita' del lavoro e `u_t` il tasso di disoccupazione.

Parametri attesi:
- β₁ (elasticita' alla produttivita'): 0.7-0.9
- β₂ (semi-elasticita' alla disoccupazione): -1.5%
- α_w (velocita' di aggiustamento dei salari): -0.15

## Dipendenze

Pacchetti R principali:

- **Econometria**: `vars`, `urca`, `tsDyn`, `lmtest`, `sandwich`
- **Dati**: `dplyr`, `tidyr`, `data.table`, `zoo`
- **Visualizzazione**: `ggplot2`, `sf`, `corrplot`
- **ISTAT**: `istatlab`, `situas`
- **Report**: `rmarkdown`, `stargazer`

L'installazione automatica e' gestita dalla funzione `install_if_missing()` all'avvio degli script.

## Limitazioni

1. I dati VECM sono calibrati su valori ISTAT, non estratti direttamente via API
2. L'aggregazione nazionale maschera l'eterogeneita' settoriale e regionale
3. Possibile endogeneita' nella variabile produttivita'
4. La stabilita' dei parametri e' assunta costante tra regimi

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
