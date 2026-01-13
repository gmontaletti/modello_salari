# Fonti Dati ISTAT - Modello Salari Montaletti

## Variabili del modello e fonti

| Variabile | Codice | Fonte ISTAT | Descrizione | Unità |
|-----------|--------|-------------|-------------|-------|
| Salari nominali | `w_nom` | Indici delle Retribuzioni Contrattuali Orarie | Retribuzioni contrattuali per dipendente | Indice 2015=100 |
| Prezzi al consumo | `p` | Indici dei Prezzi al Consumo - IPCA | Indice armonizzato prezzi al consumo (Eurostat) | Indice 2015=100 |
| Produttività | `prod` | Conti Nazionali - Contabilità Nazionale | PIL per ora lavorata (valore aggiunto/ore lavorate) | €/ora |
| Disoccupazione | `u` | Rilevazione sulle Forze di Lavoro (RFL) | Tasso di disoccupazione 15+ anni | % |
| Occupazione | `occ` | Rilevazione sulle Forze di Lavoro (RFL) | Tasso di occupazione 15-64 anni | % |
| Cuneo fiscale | `cuneo` | ISTAT + OCSE Tax Wedge Database | Oneri sociali + imposte su reddito lavoro | % costo lavoro |
| Precarietà | `prec` | Rilevazione sulle Forze di Lavoro (RFL) | Quota dipendenti con contratto a termine | % dipendenti |
| Investimenti | `inv` | Conti Nazionali Trimestrali | Investimenti fissi lordi (valori concatenati) | Mld € |
| PIL reale | `pil` | Conti Nazionali Trimestrali | Prodotto interno lordo (valori concatenati 2015) | Mld € |

## Banche dati ISTAT di riferimento

1. **I.Stat** (dati.istat.it) - Portale principale accesso dati
2. **Conti Nazionali Trimestrali** - Serie SEC 2010
3. **OROS** - Occupazione, Retribuzioni e Oneri Sociali
4. **RFL** - Rilevazione Forze Lavoro (trimestrale)
5. **Indici Retribuzioni Contrattuali** - Serie mensili CCNL
6. **Indici Prezzi Consumo** - NIC, FOI, IPCA

## Dettaglio variabili

### Salari nominali (w_nom)
- **Fonte:** ISTAT - Indici delle Retribuzioni Contrattuali Orarie
- **Codice I.Stat:** DCSC_RETam
- **Frequenza:** Mensile (aggregata a trimestrale)
- **Base:** 2015=100
- **Copertura:** Contratti collettivi nazionali di lavoro
- **Dinamica storica:**
  - 1995-2009: crescita ~1.5% annuo
  - 2010-2020: stagnazione ~0.8% annuo
  - 2021-2024: ripresa ~3% annuo (recupero inflazione)

### Prezzi al consumo (p)
- **Fonte:** ISTAT - Indici dei Prezzi al Consumo Armonizzato (IPCA)
- **Codice I.Stat:** DCSP_IPCA
- **Frequenza:** Mensile (aggregata a trimestrale)
- **Base:** 2015=100
- **Metodologia:** Conforme regolamento Eurostat
- **Dinamica storica:**
  - 1995-1999: inflazione ~2.5% annuo
  - 2000-2019: stabilità ~2% annuo
  - 2020-2021: deflazione COVID ~0.5%
  - 2022: shock energetico ~8%
  - 2023-2024: normalizzazione ~2-5%

### Produttività del lavoro (prod)
- **Fonte:** ISTAT - Conti Nazionali, Contabilità Nazionale
- **Codice I.Stat:** DCCN_TNA, DCCN_ORE
- **Calcolo:** Valore aggiunto / Ore lavorate totali
- **Unità:** Euro per ora lavorata
- **Dinamica storica:**
  - Italia: stagnazione cronica (~0.2% annuo vs 1.5% UE)
  - 1995-1999: +1% annuo
  - 2000-2019: +0.2-0.5% annuo
  - 2020: -2% (shock COVID)
  - 2021-2024: +1.5% (rimbalzo parziale)

### Tasso di disoccupazione (u)
- **Fonte:** ISTAT - Rilevazione sulle Forze di Lavoro
- **Codice I.Stat:** DCCV_TAXDISOC
- **Frequenza:** Trimestrale
- **Popolazione:** 15 anni e oltre
- **Dinamica storica:**
  - 1995: 11.5%
  - 2007: 6.1% (minimo pre-crisi)
  - 2014: 12.7% (picco post-austerity)
  - 2024: 5.9% (minimo storico)

### Tasso di occupazione (occ)
- **Fonte:** ISTAT - Rilevazione sulle Forze di Lavoro
- **Codice I.Stat:** DCCV_TAXOCCU
- **Frequenza:** Trimestrale
- **Popolazione:** 15-64 anni
- **Dinamica storica:**
  - 1995: 52%
  - 2007: 58.7%
  - 2014: 55.7%
  - 2024: 62.8% (massimo storico)

### Cuneo fiscale (cuneo)
- **Fonte:** ISTAT (oneri sociali) + OCSE Tax Wedge Database
- **Definizione:** (Costo lavoro - Retribuzione netta) / Costo lavoro
- **Componenti:** Contributi sociali datore + lavoratore + IRPEF
- **Dinamica storica:**
  - Italia tra i più alti OCSE (~47%)
  - 1995-2004: stabile ~48%
  - 2015-2019: riforme Jobs Act ~45%
  - 2020-2024: taglio cuneo ~43%

### Quota contratti a termine (prec)
- **Fonte:** ISTAT - Rilevazione sulle Forze di Lavoro
- **Codice I.Stat:** DCCV_OCCUPATIT
- **Definizione:** Dipendenti tempo determinato / Totale dipendenti
- **Dinamica storica:**
  - 1995: 7%
  - 2019: 17% (picco)
  - 2024: 13.5%

### Investimenti fissi lordi (inv)
- **Fonte:** ISTAT - Conti Nazionali Trimestrali
- **Codice I.Stat:** DCCN_TNA
- **Valutazione:** Valori concatenati (anno riferimento 2015)
- **Componenti:** Costruzioni, macchinari, mezzi trasporto, proprietà intellettuale
- **Dinamica storica:**
  - 2007: 280 mld (picco pre-crisi)
  - 2014: 220 mld (minimo)
  - 2024: 290 mld (effetto PNRR)

### PIL reale (pil)
- **Fonte:** ISTAT - Conti Nazionali Trimestrali
- **Codice I.Stat:** DCCN_TNA
- **Valutazione:** Valori concatenati (anno riferimento 2015)
- **Metodologia:** SEC 2010
- **Dinamica storica:**
  - 2008-2009: -5% (crisi finanziaria)
  - 2012-2013: -3% (crisi debito sovrano)
  - 2020: -9% (COVID)
  - 2021-2022: +7% (rimbalzo)

## Note metodologiche

I dati nel modello sono generati mediante simulazione calibrata sui valori storici reali pubblicati nei report ISTAT 2020-2025, in assenza di accesso diretto alle API ISTAT.

I parametri di calibrazione riflettono:
- Valori medi e range storici documentati
- Dinamiche cicliche (crisi 2008, 2012, COVID)
- Trend strutturali (stagnazione produttività, dualismo mercato lavoro)
- Shock esogeni (energia 2022, PNRR 2021-2024)

## Riferimenti

- ISTAT (2024). *Rapporto Annuale - La situazione del Paese*
- ISTAT (2024). *Conti Economici Trimestrali*
- ISTAT (2024). *Indicatori del Lavoro*
- OCSE (2024). *Taxing Wages*
- Eurostat (2024). *Harmonised Index of Consumer Prices*
