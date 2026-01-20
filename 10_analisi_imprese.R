# 10_analisi_imprese.R -----
# Analisi esplorativa imprese: statistiche descrittive, ranking, evoluzione temporale
# NOTA: MHOUR_JV è un INDICE (base 2021=100), non valori assoluti
# Autore: Giampaolo Montaletti (ORCID: 0009-0002-5327-1122)
# Data: 2026-01-15

# 1. Setup e caricamento dati -----
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(knitr)
  library(kableExtra)
  library(zoo)
})

cat("==== Analisi Esplorativa Imprese ====\n\n")

# Carica dati preparati
imprese_clean <- readRDS("output/imprese_clean.rds")

cat("Dati caricati:\n")
cat("- Imprese clean:", nrow(imprese_clean), "righe\n")
cat(
  "- Periodo:",
  min(imprese_clean$periodo),
  "to",
  max(imprese_clean$periodo),
  "\n"
)
cat("- Settori:", length(unique(imprese_clean$settore_macro)), "\n\n")

cat("NOTA IMPORTANTE:\n")
cat("MHOUR_JV è un INDICE (base 2021=100), NON valori assoluti.\n")
cat("- Codice '0015' = TOTALE INDUSTRIA E SERVIZI (aggregato ufficiale)\n")
cat("- I valori rappresentano ore lavorate relative al 2021\n")
cat("- Non vanno sommati tra settori diversi\n\n")

# 2. Estrazione totale economia (codice 0015) -----

cat("==== Estrazione Totale Economia (codice 0015) ====\n\n")

totale_economia <- imprese_clean %>%
  filter(ECON_ACTIVITY_NACE_2007 == "0015") %>%
  select(
    periodo,
    anno,
    trimestre,
    anno_trimestre,
    dim_aziendale,
    dim_aziendale_label,
    ore_migliaia,
    settore_macro_label
  ) %>%
  arrange(anno, trimestre, dim_aziendale)

cat("Totale economia estratto:", nrow(totale_economia), "righe\n")
cat(
  "- Dimensioni aziendali:",
  paste(unique(totale_economia$dim_aziendale), collapse = ", "),
  "\n"
)
cat(
  "- Periodo:",
  min(totale_economia$periodo),
  "to",
  max(totale_economia$periodo),
  "\n\n"
)

# Pivot per confronto W_GE1 vs W_GE10
totale_wide <- totale_economia %>%
  select(
    periodo,
    anno,
    trimestre,
    anno_trimestre,
    dim_aziendale,
    ore_migliaia
  ) %>%
  pivot_wider(
    names_from = dim_aziendale,
    values_from = ore_migliaia
  ) %>%
  mutate(
    diff_assoluta = W_GE10 - W_GE1,
    diff_percentuale = (W_GE10 - W_GE1) / W_GE1 * 100
  )

cat("Confronto W_GE1 vs W_GE10 (totale economia):\n")
print(head(totale_wide, 10))
cat("\n")

saveRDS(totale_economia, "output/totale_economia_0015.rds")
saveRDS(totale_wide, "output/totale_economia_0015_wide.rds")
cat("Salvato: output/totale_economia_0015.rds\n")
cat("Salvato: output/totale_economia_0015_wide.rds\n\n")

# 3. Statistiche descrittive per settore -----

cat("==== Statistiche Descrittive per Settore ====\n\n")

# Rimuovi aggregati (codici numerici) per analisi settoriale
settori_singoli <- imprese_clean %>%
  filter(
    grepl("^[A-Z]$", settore_macro), # Solo settori con lettera singola
    !is.na(settore_macro)
  )

cat(
  "Settori singoli (lettere NACE):",
  length(unique(settori_singoli$settore_macro)),
  "\n\n"
)

stat_settori <- settori_singoli %>%
  group_by(settore_macro, settore_macro_label, dim_aziendale) %>%
  summarise(
    indice_medio = mean(ore_migliaia, na.rm = TRUE),
    indice_mediano = median(ore_migliaia, na.rm = TRUE),
    indice_min = min(ore_migliaia, na.rm = TRUE),
    indice_max = max(ore_migliaia, na.rm = TRUE),
    sd_indice = sd(ore_migliaia, na.rm = TRUE),
    cv = sd_indice / indice_medio,
    n_obs = n(),
    .groups = "drop"
  ) %>%
  arrange(settore_macro, dim_aziendale)

cat("Statistiche per settore (W_GE1, top 15):\n")
print(
  stat_settori %>%
    filter(dim_aziendale == "W_GE1") %>%
    arrange(desc(indice_medio)) %>%
    head(15) %>%
    mutate(across(where(is.numeric), ~ round(., 2))) %>%
    select(
      settore_macro_label,
      indice_medio,
      indice_min,
      indice_max,
      cv,
      n_obs
    ) %>%
    kable(
      col.names = c("Settore", "Indice Medio", "Min", "Max", "CV", "N Obs"),
      align = c("l", rep("r", 5))
    )
)
cat("\n")

saveRDS(stat_settori, "output/stat_descrittive_settori.rds")
cat("Salvato: output/stat_descrittive_settori.rds\n\n")

# 4. Ranking settori per indice ore (ultimo trimestre) -----

cat("==== Ranking Settori per Indice Ore (2025-Q3) ====\n\n")

ranking_2025Q3 <- settori_singoli %>%
  filter(
    anno == 2025,
    trimestre == 3,
    dim_aziendale == "W_GE1"
  ) %>%
  select(settore_macro, settore_macro_label, ore_migliaia) %>%
  arrange(desc(ore_migliaia)) %>%
  mutate(
    rank = row_number(),
    indice_fmt = sprintf("%.1f", ore_migliaia)
  )

cat("Top 10 settori per indice ore lavorate (2025-Q3, W_GE1):\n")
print(
  ranking_2025Q3 %>%
    select(rank, settore_macro_label, indice_fmt) %>%
    head(10) %>%
    kable(
      col.names = c("Rank", "Settore", "Indice (2021=100)"),
      align = c("r", "l", "r")
    )
)
cat("\n")

saveRDS(ranking_2025Q3, "output/ranking_settori_indice_2025Q3.rds")
cat("Salvato: output/ranking_settori_indice_2025Q3.rds\n\n")

# 5. Evoluzione temporale totale economia -----

cat("==== Evoluzione Temporale Totale Economia ====\n\n")

evoluzione_totale <- totale_wide %>%
  arrange(anno, trimestre) %>%
  mutate(
    # Variazioni trimestrali W_GE1
    W_GE1_lag1 = lag(W_GE1, 1),
    var_trim_W_GE1 = (W_GE1 - W_GE1_lag1) / W_GE1_lag1 * 100,
    # Variazioni annuali W_GE1
    W_GE1_lag4 = lag(W_GE1, 4),
    var_anno_W_GE1 = (W_GE1 - W_GE1_lag4) / W_GE1_lag4 * 100,
    # Variazioni trimestrali W_GE10
    W_GE10_lag1 = lag(W_GE10, 1),
    var_trim_W_GE10 = (W_GE10 - W_GE10_lag1) / W_GE10_lag1 * 100,
    # Variazioni annuali W_GE10
    W_GE10_lag4 = lag(W_GE10, 4),
    var_anno_W_GE10 = (W_GE10 - W_GE10_lag4) / W_GE10_lag4 * 100
  )

cat("Evoluzione totale economia (ultime 12 osservazioni):\n")
print(
  evoluzione_totale %>%
    select(periodo, W_GE1, var_trim_W_GE1, var_anno_W_GE1, diff_percentuale) %>%
    tail(12) %>%
    mutate(across(where(is.numeric), ~ round(., 2))) %>%
    kable(
      col.names = c(
        "Periodo",
        "Indice W_GE1",
        "Var% Trim",
        "Var% Anno",
        "Gap% W_GE10/W_GE1"
      ),
      align = c("l", rep("r", 4))
    )
)
cat("\n")

saveRDS(evoluzione_totale, "output/evoluzione_totale_economia.rds")
cat("Salvato: output/evoluzione_totale_economia.rds\n\n")

# 6. Evoluzione settoriale (top 5 settori) -----

cat("==== Evoluzione Settoriale (Top 5 Settori da Ranking) ====\n\n")

top5_settori <- ranking_2025Q3 %>%
  head(5) %>%
  pull(settore_macro)

evoluzione_settori <- settori_singoli %>%
  filter(
    settore_macro %in% top5_settori,
    dim_aziendale == "W_GE1"
  ) %>%
  select(
    periodo,
    anno,
    trimestre,
    settore_macro,
    settore_macro_label,
    ore_migliaia
  ) %>%
  arrange(settore_macro, anno, trimestre) %>%
  group_by(settore_macro, settore_macro_label) %>%
  mutate(
    ore_lag1 = lag(ore_migliaia, 1),
    var_trim = (ore_migliaia - ore_lag1) / ore_lag1 * 100,
    ore_lag4 = lag(ore_migliaia, 4),
    var_anno = (ore_migliaia - ore_lag4) / ore_lag4 * 100
  ) %>%
  ungroup()

cat("Evoluzione top 5 settori (ultime 20 osservazioni):\n")
print(
  evoluzione_settori %>%
    select(periodo, settore_macro_label, ore_migliaia, var_trim, var_anno) %>%
    tail(20) %>%
    mutate(across(where(is.numeric), ~ round(., 2))) %>%
    kable(
      col.names = c("Periodo", "Settore", "Indice", "Var% Trim", "Var% Anno"),
      align = c("l", "l", rep("r", 3))
    )
)
cat("\n")

saveRDS(evoluzione_settori, "output/evoluzione_settori_top5.rds")
cat("Salvato: output/evoluzione_settori_top5.rds\n\n")

# 7. Confronto dimensione aziendale (totale economia) -----

cat("==== Confronto Dimensione Aziendale (Totale Economia) ====\n\n")

confronto_dimensione <- totale_wide %>%
  select(
    periodo,
    anno,
    trimestre,
    anno_trimestre,
    W_GE1,
    W_GE10,
    diff_assoluta,
    diff_percentuale
  )

cat("Statistiche differenziale W_GE10 vs W_GE1:\n")
cat(
  "  Gap medio (punti indice):",
  sprintf("%.2f", mean(confronto_dimensione$diff_assoluta, na.rm = TRUE)),
  "\n"
)
cat(
  "  Gap medio (%):",
  sprintf("%.2f%%", mean(confronto_dimensione$diff_percentuale, na.rm = TRUE)),
  "\n"
)
cat(
  "  Gap min (%):",
  sprintf("%.2f%%", min(confronto_dimensione$diff_percentuale, na.rm = TRUE)),
  "\n"
)
cat(
  "  Gap max (%):",
  sprintf("%.2f%%", max(confronto_dimensione$diff_percentuale, na.rm = TRUE)),
  "\n\n"
)

cat(
  "NOTA: W_GE10 (imprese ≥10 dipendenti) è un sottoinsieme di W_GE1 (imprese ≥1 dipendente).\n"
)
cat(
  "Il gap misura la quota delle imprese ≥10 dip sul totale ore lavorate.\n\n"
)

saveRDS(confronto_dimensione, "output/confronto_dimensione_aziendale.rds")
cat("Salvato: output/confronto_dimensione_aziendale.rds\n\n")

# 8. Analisi volatilità settoriale -----

cat("==== Analisi Volatilità Settoriale ====\n\n")

volatilita_settori <- evoluzione_settori %>%
  group_by(settore_macro, settore_macro_label) %>%
  summarise(
    sd_var_trim = sd(var_trim, na.rm = TRUE),
    mean_var_trim = mean(var_trim, na.rm = TRUE),
    sd_indice = sd(ore_migliaia, na.rm = TRUE),
    mean_indice = mean(ore_migliaia, na.rm = TRUE),
    cv_indice = sd_indice / mean_indice,
    n_obs = sum(!is.na(var_trim)),
    .groups = "drop"
  ) %>%
  arrange(desc(sd_var_trim))

cat("Volatilità settoriale (ordinata per SD variazioni trimestrali):\n")
print(
  volatilita_settori %>%
    mutate(across(where(is.numeric), ~ round(., 3))) %>%
    kable(
      col.names = c(
        "Settore",
        "Label",
        "SD Var% Trim",
        "Media Var% Trim",
        "SD Indice",
        "Media Indice",
        "CV",
        "N Obs"
      ),
      align = c("l", "l", rep("r", 6))
    )
)
cat("\n")

saveRDS(volatilita_settori, "output/volatilita_settori.rds")
cat("Salvato: output/volatilita_settori.rds\n\n")

# 9. Decomposizione crescita 2015-2025 per settore -----

cat("==== Decomposizione Crescita 2015-2025 per Settore ====\n\n")

# Funzione helper per formattare crescita
format_crescita <- function(value) {
  if (is.na(value)) {
    return("N/A")
  } else if (value >= 0) {
    sprintf("crescita del %.1f%%", value)
  } else {
    sprintf("contrazione del %.1f%%", abs(value))
  }
}

decomp_crescita_settori <- settori_singoli %>%
  filter(
    periodo %in% c("2015 Q1", "2025 Q3"),
    dim_aziendale == "W_GE1"
  ) %>%
  select(settore_macro, settore_macro_label, periodo, ore_migliaia) %>%
  pivot_wider(
    names_from = periodo,
    values_from = ore_migliaia
  ) %>%
  rename(
    indice_2015Q1 = `2015 Q1`,
    indice_2025Q3 = `2025 Q3`
  ) %>%
  mutate(
    var_assoluta = indice_2025Q3 - indice_2015Q1,
    var_percentuale = (indice_2025Q3 - indice_2015Q1) / indice_2015Q1 * 100,
    # CAGR (10.5 anni = 42 trimestri)
    cagr = ((indice_2025Q3 / indice_2015Q1)^(1 / 10.5) - 1) * 100
  ) %>%
  arrange(desc(var_percentuale))

cat("Crescita settoriale 2015-Q1 to 2025-Q3 (W_GE1):\n")
print(
  decomp_crescita_settori %>%
    mutate(across(where(is.numeric), ~ round(., 2))) %>%
    kable(
      col.names = c(
        "Settore",
        "Label",
        "Indice 2015-Q1",
        "Indice 2025-Q3",
        "Var Assoluta",
        "Var %",
        "CAGR %"
      ),
      align = c("l", "l", rep("r", 5))
    )
)
cat("\n")

# Totale economia
decomp_totale <- totale_economia %>%
  filter(
    periodo %in% c("2015 Q1", "2025 Q3"),
    dim_aziendale == "W_GE1"
  ) %>%
  select(periodo, ore_migliaia) %>%
  pivot_wider(
    names_from = periodo,
    values_from = ore_migliaia
  ) %>%
  rename(
    indice_2015Q1 = `2015 Q1`,
    indice_2025Q3 = `2025 Q3`
  ) %>%
  mutate(
    var_assoluta = indice_2025Q3 - indice_2015Q1,
    var_percentuale = (indice_2025Q3 - indice_2015Q1) / indice_2015Q1 * 100,
    cagr = ((indice_2025Q3 / indice_2015Q1)^(1 / 10.5) - 1) * 100
  )

cat("Crescita totale economia (codice 0015, W_GE1):\n")
cat("  Indice 2015-Q1:", sprintf("%.1f", decomp_totale$indice_2015Q1), "\n")
cat("  Indice 2025-Q3:", sprintf("%.1f", decomp_totale$indice_2025Q3), "\n")
cat("  Variazione:", format_crescita(decomp_totale$var_percentuale), "\n")
cat("  CAGR:", sprintf("%.2f%%", decomp_totale$cagr), "\n\n")

saveRDS(decomp_crescita_settori, "output/decomposizione_crescita_settori.rds")
saveRDS(decomp_totale, "output/decomposizione_crescita_totale.rds")
cat("Salvato: output/decomposizione_crescita_settori.rds\n")
cat("Salvato: output/decomposizione_crescita_totale.rds\n\n")

# 10. Sintesi generale -----

cat("==== Sintesi Statistica Generale ====\n\n")

sintesi <- list(
  # Overall
  n_settori_singoli = length(unique(settori_singoli$settore_macro)),
  n_settori_totali = length(unique(imprese_clean$settore_macro)),
  n_trimestri = length(unique(imprese_clean$periodo)),
  periodo_min = min(imprese_clean$periodo),
  periodo_max = max(imprese_clean$periodo),

  # Totale economia 2025-Q3 (W_GE1)
  indice_totale_2025Q3 = totale_economia %>%
    filter(periodo == "2025 Q3", dim_aziendale == "W_GE1") %>%
    pull(ore_migliaia),

  # Growth totale economia
  crescita_totale_2015_2025 = decomp_totale$var_percentuale,
  cagr_totale = decomp_totale$cagr,

  # Settori
  settore_max_crescita = decomp_crescita_settori$settore_macro_label[1],
  settore_min_crescita = decomp_crescita_settori$settore_macro_label[nrow(
    decomp_crescita_settori
  )],
  settore_max_indice_2025 = ranking_2025Q3$settore_macro_label[1],
  settore_min_indice_2025 = ranking_2025Q3$settore_macro_label[nrow(
    ranking_2025Q3
  )],

  # Dimensione aziendale
  gap_W_GE10_medio = mean(confronto_dimensione$diff_percentuale, na.rm = TRUE),
  gap_W_GE10_2025Q3 = confronto_dimensione %>%
    filter(periodo == "2025 Q3") %>%
    pull(diff_percentuale),

  # Volatilità
  settore_piu_volatile = volatilita_settori$settore_macro_label[1],
  settore_meno_volatile = volatilita_settori$settore_macro_label[nrow(
    volatilita_settori
  )]
)

cat("Numero settori analizzati:\n")
cat("  - Settori singoli (lettere NACE):", sintesi$n_settori_singoli, "\n")
cat("  - Totale (inclusi aggregati):", sintesi$n_settori_totali, "\n")
cat("Periodo temporale:", sintesi$n_trimestri, "trimestri\n")
cat("  da", sintesi$periodo_min, "a", sintesi$periodo_max, "\n\n")

cat("Totale economia (codice 0015, W_GE1):\n")
cat(
  "  Indice 2025-Q3:",
  sprintf("%.1f", sintesi$indice_totale_2025Q3),
  "(base 2021=100)\n"
)
cat(
  "  Crescita 2015-2025:",
  format_crescita(sintesi$crescita_totale_2015_2025),
  "\n"
)
cat("  CAGR:", sprintf("%.2f%%", sintesi$cagr_totale), "\n\n")

cat("Settori:\n")
cat("  Massima crescita 2015-2025:", sintesi$settore_max_crescita, "\n")
cat("  Minima crescita 2015-2025:", sintesi$settore_min_crescita, "\n")
cat("  Indice più alto 2025-Q3:", sintesi$settore_max_indice_2025, "\n")
cat("  Indice più basso 2025-Q3:", sintesi$settore_min_indice_2025, "\n\n")

cat("Dimensione aziendale (W_GE10 vs W_GE1):\n")
cat("  Gap medio periodo:", sprintf("%.2f%%", sintesi$gap_W_GE10_medio), "\n")
cat("  Gap 2025-Q3:", sprintf("%.2f%%", sintesi$gap_W_GE10_2025Q3), "\n")
cat("  (W_GE10 ≥10 dip è sottoinsieme di W_GE1 ≥1 dip)\n\n")

cat("Volatilità:\n")
cat("  Settore più volatile:", sintesi$settore_piu_volatile, "\n")
cat("  Settore meno volatile:", sintesi$settore_meno_volatile, "\n\n")

saveRDS(sintesi, "output/sintesi_imprese.rds")
cat("Salvato: output/sintesi_imprese.rds\n\n")

cat("==== Script completato con successo ====\n")
cat("File creati nella directory output/:\n")
cat("- totale_economia_0015.rds\n")
cat("- totale_economia_0015_wide.rds\n")
cat("- stat_descrittive_settori.rds\n")
cat("- ranking_settori_indice_2025Q3.rds\n")
cat("- evoluzione_totale_economia.rds\n")
cat("- evoluzione_settori_top5.rds\n")
cat("- confronto_dimensione_aziendale.rds\n")
cat("- volatilita_settori.rds\n")
cat("- decomposizione_crescita_settori.rds\n")
cat("- decomposizione_crescita_totale.rds\n")
cat("- sintesi_imprese.rds\n")
