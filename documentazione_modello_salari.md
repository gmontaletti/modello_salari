# Modello Econometrico delle Determinanti dei Salari in Italia

## Documentazione Tecnica

---

## 1. Obiettivo del Modello

Il modello stima le determinanti della dinamica salariale italiana nel periodo 1995-2024, basandosi sul framework teorico emerso dall'analisi dei testi di Giampaolo Montaletti. L'obiettivo principale è quantificare:

1. La relazione di lungo periodo tra salari reali, produttività e disoccupazione
2. La velocità di aggiustamento dei salari agli shock
3. Il contributo relativo delle diverse determinanti alla varianza salariale
4. Gli effetti di scenari alternativi di policy

---

## 2. Fonti Dati ISTAT

### 2.1 Serie Storiche Utilizzate

| Variabile | Fonte ISTAT | Codice Serie | Frequenza |
|-----------|-------------|--------------|-----------|
| Retribuzioni contrattuali | Indagine Retribuzioni Contrattuali | DCSP_RETRIBCONTR | Mensile |
| Retribuzioni lorde per ULA | Rilevazione OROS | DCSC_OROS | Trimestrale |
| PIL reale | Conti Nazionali Trimestrali | DCCN_TNA | Trimestrale |
| Valore Aggiunto | Conti Nazionali | DCCN_PILLAV | Trimestrale |
| Ore lavorate | Conti Nazionali | DCCN_ORE | Trimestrale |
| Tasso disoccupazione | Rilevazione Forze Lavoro | DCCV_TAXDISOC | Trimestrale |
| Tasso occupazione | Rilevazione Forze Lavoro | DCCV_TAXOCCU | Trimestrale |
| IPCA | Indici Prezzi al Consumo | DCSP_IPCA | Mensile |
| Investimenti fissi lordi | Conti Nazionali | DCCN_TNA | Trimestrale |

### 2.2 Costruzione delle Variabili

**Salari nominali (W)**
- Indice delle retribuzioni contrattuali orarie (base 2015=100)
- Fonte alternativa: retribuzioni lorde per ULA da OROS

**Prezzi (P)**
- Indice armonizzato dei prezzi al consumo (IPCA)
- Base 2015=100

**Produttività del lavoro (Y/L)**
- PIL reale / Ore lavorate totali
- Unità: euro per ora lavorata (prezzi concatenati 2015)

**Tasso di disoccupazione (U)**
- Definizione ILO: disoccupati / forza lavoro × 100
- Popolazione 15-74 anni

**Variabili aggiuntive:**
- Tasso di occupazione (15-64 anni)
- Cuneo fiscale (stima basata su dati OCSE)
- Quota contratti a termine (da Forze Lavoro)

---

## 3. Specificazione Econometrica

### 3.1 Modello VECM

Il modello base è un Vector Error Correction Model (VECM) con 4 variabili endogene:

$$X_t = (w_t, p_t, (y/l)_t, u_t)'$$

dove le variabili in minuscolo sono in logaritmi (tranne u).

**Relazione di cointegrazione (lungo periodo):**

$$w_t - p_t = \beta_0 + \beta_1 (y/l)_t + \beta_2 u_t + \varepsilon_t$$

Interpretazione: il salario reale di equilibrio dipende dalla produttività (elasticità β₁) e dal tasso di disoccupazione (semi-elasticità β₂).

**Sistema VECM (breve periodo):**

$$\Delta X_t = \mu + \sum_{i=1}^{k-1} \Gamma_i \Delta X_{t-i} + \alpha \beta' X_{t-1} + \varepsilon_t$$

dove:
- α = vettore coefficienti di aggiustamento
- β = vettore di cointegrazione
- Γᵢ = matrici coefficienti dinamica breve periodo
- k = numero di ritardi

### 3.2 Modello Esteso (OLS)

Per incorporare le variabili del DAG non incluse nel VECM:

$$\Delta w^r_t = \gamma_0 + \gamma_1 u_t + \gamma_2 \Delta(y/l)_t + \gamma_3 \text{cuneo}_t + \gamma_4 \text{prec}_t + \gamma_5 \Delta\text{pil}_t + \eta_t$$

dove $w^r$ = salario reale (log).

---

## 4. Procedura di Stima

### 4.1 Test Preliminari

**Test di Stazionarietà:**
- ADF (Augmented Dickey-Fuller)
- Phillips-Perron
- KPSS

Risultati attesi: serie I(1) in livelli, I(0) in differenze prime.

**Selezione Ritardi:**
- Criteri informativi: AIC, BIC, HQ
- Test LR sequenziali

**Test di Cointegrazione:**
- Procedura di Johansen (test trace e max eigenvalue)
- Rango di cointegrazione atteso: r = 1

### 4.2 Stima

1. Stima VECM con funzione `cajorls()` dal pacchetto `urca`
2. Conversione in forma VAR con `vec2var()` per analisi IRF/FEVD
3. Stima modello esteso OLS con errori standard HAC (Newey-West)

### 4.3 Diagnostica

- Test Portmanteau per autocorrelazione residui
- Test ARCH-LM per eteroschedasticità
- Test Jarque-Bera per normalità
- Verifica stabilità (radici caratteristiche < 1)

---

## 5. Risultati Attesi

### 5.1 Parametri Strutturali

| Parametro | Descrizione | Valore Atteso | Intervallo |
|-----------|-------------|---------------|------------|
| β₁ | Elasticità salari-produttività | 0.7 - 0.9 | [0.5, 1.0] |
| β₂ | Semi-elasticità salari-disoccupazione | -1.5% | [-2.5%, -0.5%] |
| α_w | Velocità aggiustamento salari | -0.15 | [-0.25, -0.05] |

### 5.2 Decomposizione Varianza (2 anni)

| Fonte Shock | Contributo Varianza Salari |
|-------------|---------------------------|
| Shock salari (propri) | 45-55% |
| Shock produttività | 20-30% |
| Shock disoccupazione | 10-20% |
| Shock prezzi | 5-15% |

### 5.3 Funzioni di Risposta Impulsiva

**Shock produttività +1%:**
- Impatto: +0.3% salari
- Dopo 8 trimestri: +0.7% (cumulato)
- Lungo periodo: +0.8-0.9%

**Shock disoccupazione +1pp:**
- Impatto: -0.5% salari
- Dopo 8 trimestri: -1.2% (cumulato)
- Persistente nel tempo

---

## 6. Simulazioni di Policy

### 6.1 Scenario Base

Proiezione baseline con parametri stimati e ipotesi:
- Crescita produttività: +0.3% annuo
- Disoccupazione: stabile al 6%
- Inflazione: 2% annuo

### 6.2 Scenari Alternativi

**Scenario A: Aumento produttività +1% annuo**
- Effetto cumulato 5 anni: +4-5% salari reali
- Richiede: investimenti in innovazione, formazione

**Scenario B: Riduzione disoccupazione a 4%**
- Effetto: +3% salari reali
- Richiede: politiche attive efficaci

**Scenario C: Shock inflazionistico +5%**
- Breve periodo: -2% salari reali (pass-through 60%)
- Lungo periodo: recupero quasi completo

**Scenario D: Taglio cuneo fiscale 3pp**
- Effetto diretto: +1.5% salari netti
- Effetto indiretto (domanda): +0.3% occupazione

---

## 7. Limitazioni del Modello

1. **Endogeneità:** alcune variabili (es. produttività) potrebbero essere endogene
2. **Cambiamenti strutturali:** parametri potrebbero variare nel tempo
3. **Aggregazione:** modello nazionale non cattura eterogeneità settoriale/territoriale
4. **Omissione variabili:** potere sindacale, globalizzazione non modellati direttamente
5. **Dati simulati:** in assenza di accesso diretto API ISTAT, calibrazione su report

---

## 8. Estensioni Possibili

- Modello a parametri variabili nel tempo (TVP-VAR)
- Modello con regime switching (Markov-switching)
- Disaggregazione settoriale (industria, servizi, PA)
- Modello panel regionale
- Inclusione variabili globali (prezzi energia, domanda estera)

---

## 9. Pacchetti R Utilizzati

```r
# Modellazione
library(vars)      # VAR e VECM
library(urca)      # Test cointegrazione
library(tsDyn)     # VECM non lineari
library(lmtest)    # Test diagnostici
library(sandwich)  # Errori standard robusti

# Dati e manipolazione
library(dplyr)
library(tidyr)
library(zoo)

# Visualizzazione
library(ggplot2)
library(gridExtra)
library(corrplot)
```

---

## 10. Riferimenti

- Johansen, S. (1991). Estimation and Hypothesis Testing of Cointegration Vectors in Gaussian Vector Autoregressive Models. Econometrica, 59(6), 1551-1580.
- Pfaff, B. (2008). Analysis of Integrated and Cointegrated Time Series with R. Springer.
- ISTAT (2024). Rapporto Annuale - La situazione del Paese.
- IMF (2024). Italy: Article IV Consultation Staff Report.

---

*Documentazione compilata il 10 dicembre 2025*
