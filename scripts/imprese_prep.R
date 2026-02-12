# imprese_prep.R -----
# Pipeline imprese: preparazione dati, analisi statistiche, visualizzazioni
# Autore: Giampaolo Montaletti (ORCID: 0009-0002-5327-1122)
# Data: 2026-01-20

# 1. Setup e librerie -----
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(data.table)
  library(ggplot2)
  library(zoo)
  library(patchwork)
  library(scales)
  library(viridis)
  library(knitr)
  library(kableExtra)
})

cat("==== Pipeline Imprese: Preparazione, Analisi e Visualizzazioni ====\n\n")

# Crea directory output
if (!dir.exists("output/imprese")) {
  dir.create("output/imprese", recursive = TRUE)
}
if (!dir.exists("output/imprese/grafici")) {
  dir.create("output/imprese/grafici", recursive = TRUE)
}

# Theme per grafici (fallback se theme_salari non disponibile)
if (file.exists("visualizzazioni_salari.R")) {
  # Carica solo la funzione theme_salari senza eseguire altro
  local({
    env <- new.env()
    tryCatch(
      {
        # Leggi e valuta solo la definizione di theme_salari
        source("visualizzazioni_salari.R", local = env)
        if (exists("theme_salari", envir = env)) {
          assign(
            "theme_salari",
            get("theme_salari", envir = env),
            envir = .GlobalEnv
          )
          cat("Theme 'theme_salari' caricato da visualizzazioni_salari.R\n")
        }
      },
      error = function(e) {
        cat("Nota: impossibile caricare theme_salari, uso theme_minimal\n")
      }
    )
  })
}

if (!exists("theme_salari")) {
  theme_salari <- function() {
    theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 11, color = "gray40"),
        axis.title = element_text(size = 10),
        legend.position = "bottom"
      )
  }
  cat("Usando theme_salari fallback (basato su theme_minimal)\n")
}

# 2. Funzioni utility (da 09_prepara_imprese.R) -----

# Parse quarter string "YYYY-QN" or "YYYY QN" to components
parse_quarter <- function(quarter_string) {
  quarter_string <- as.character(quarter_string)

  # Gestisci entrambi formati: "2017-Q3" e "2017 Q3"
  if (grepl("-Q", quarter_string)) {
    parts <- strsplit(quarter_string, "-Q")[[1]]
  } else if (grepl(" Q", quarter_string)) {
    parts <- strsplit(quarter_string, " Q")[[1]]
  } else {
    stop("Formato trimestre non riconosciuto: ", quarter_string)
  }

  anno <- as.integer(parts[1])
  trimestre <- as.integer(parts[2])
  anno_trimestre <- anno + (trimestre - 1) / 4

  list(
    anno = anno,
    trimestre = trimestre,
    anno_trimestre = anno_trimestre
  )
}

# Apply parse_quarter to vector
parse_quarters_vector <- function(quarters_vec) {
  parsed <- lapply(quarters_vec, parse_quarter)

  data.frame(
    anno = sapply(parsed, function(x) x$anno),
    trimestre = sapply(parsed, function(x) x$trimestre),
    anno_trimestre = sapply(parsed, function(x) x$anno_trimestre)
  )
}

# Get vintage type from DATA_TYPE
get_vintage <- function(data_type) {
  case_when(
    grepl("MHOUR_JV_1$", data_type) ~ "MHOUR_JV_1",
    grepl("MHOUR_JV_2$", data_type) ~ "MHOUR_JV_2",
    grepl("MHOUR_JV_2021$", data_type) ~ "MHOUR_JV_2021",
    TRUE ~ "Other"
  )
}

# Clean and prepare imprese dataset
clean_imprese_data <- function(data, period_start = "2015-Q1") {
  cat("  Filtraggio periodo >= ", period_start, "...\n", sep = "")

  # Filter to MHOUR_JV variants, seasonally adjusted (Y), and period
  data_filtered <- data %>%
    filter(
      grepl("MHOUR_JV", DATA_TYPE),
      ADJUSTMENT == "Y", # Seasonally adjusted data
      ObsDimension >= period_start
    )

  cat("  Righe dopo filtraggio:", nrow(data_filtered), "\n")

  # Add labels using istatlab (CORRECT METHOD)
  cat("  Aggiunta labels ISTAT ufficiali con istatlab...\n")
  data_with_labels <- data_filtered %>%
    istatlab::apply_labels()

  # Parse quarters from tempo_temp column (dopo apply_labels)
  cat("  Parsing periodi trimestrali da tempo_temp...\n")

  # Combine data
  data_clean <- data_with_labels %>%
    mutate(
      vintage = get_vintage(DATA_TYPE),
      # Parse periodo da tempo_temp
      periodo = tempo_temp,
      anno = as.integer(substr(tempo_temp, 1, 4)),
      trimestre = as.integer(substr(tempo_temp, 7, 7)),
      anno_trimestre = anno + (trimestre - 1) / 4,
      # Converti a character prima di elaborare (apply_labels puo creare fattori)
      nace_code = as.character(ECON_ACTIVITY_NACE_2007),
      # Estrai settore macro (primo carattere se lettera)
      settore_macro = if_else(
        grepl("^[A-Z]", nace_code),
        if_else(
          nchar(nace_code) == 1,
          nace_code,
          substr(nace_code, 1, 1)
        ),
        nace_code # Mantieni codice completo se non lettera
      ),
      # Settore dettaglio (4 cifre)
      settore_dettaglio = if_else(
        grepl("^[0-9]{4}$", nace_code),
        nace_code,
        NA_character_
      ),
      # Usa label ISTAT ufficiale
      settore_macro_label = as.character(ECON_ACTIVITY_NACE_2007_label),
      dim_aziendale = as.character(EMPLOYESS_CLASS),
      dim_aziendale_label = as.character(EMPLOYESS_CLASS_label),
      ore_migliaia = valore, # apply_labels rinomina ObsValue -> valore
      periodo = tempo_temp # apply_labels rinomina ObsDimension -> tempo_temp
    ) %>%
    select(
      periodo,
      anno,
      trimestre,
      anno_trimestre,
      settore_macro,
      settore_dettaglio,
      settore_macro_label,
      dim_aziendale,
      dim_aziendale_label,
      vintage,
      ore_migliaia,
      # Mantieni anche colonne originali per debug
      ECON_ACTIVITY_NACE_2007,
      DATA_TYPE
    )

  # Remove NA values in ore_migliaia
  data_clean <- data_clean %>%
    filter(!is.na(ore_migliaia))

  cat("  Righe dopo rimozione NA:", nrow(data_clean), "\n")

  # Apply vintage priority to keep only most recent vintage per observation
  # Priority: MHOUR_JV_2021 > MHOUR_JV_2 > MHOUR_JV_1 > Other
  cat("  Applicazione priorita vintage...\n")
  data_clean <- data_clean %>%
    mutate(
      vintage_priority = case_when(
        vintage == "MHOUR_JV_2021" ~ 1,
        vintage == "MHOUR_JV_2" ~ 2,
        vintage == "MHOUR_JV_1" ~ 3,
        TRUE ~ 4
      )
    ) %>%
    group_by(
      periodo,
      anno,
      trimestre,
      anno_trimestre,
      settore_macro,
      settore_dettaglio,
      settore_macro_label,
      dim_aziendale,
      dim_aziendale_label,
      ECON_ACTIVITY_NACE_2007
    ) %>%
    filter(vintage_priority == min(vintage_priority)) %>%
    slice(1) %>% # In case of ties, take first
    ungroup() %>%
    select(-vintage_priority)

  cat("  Righe finali (dopo selezione vintage):", nrow(data_clean), "\n")

  return(data_clean)
}

# Aggregate to national level
aggregate_national_level <- function(data) {
  cat("  Aggregazione livello nazionale...\n")

  # Prima aggregazione per settore
  data_aggregated <- data %>%
    group_by(
      periodo,
      anno,
      trimestre,
      anno_trimestre,
      dim_aziendale,
      vintage
    ) %>%
    summarise(
      ore_totali_migliaia = sum(ore_migliaia, na.rm = TRUE),
      n_settori = n_distinct(settore_macro[!is.na(settore_macro)]),
      .groups = "drop"
    )

  # Gestione vintage con priorita (piu recente vince)
  # Priorita: MHOUR_JV_2021 > MHOUR_JV_2 > MHOUR_JV_1 > Other
  data_nazionale <- data_aggregated %>%
    mutate(
      vintage_priority = case_when(
        vintage == "MHOUR_JV_2021" ~ 1,
        vintage == "MHOUR_JV_2" ~ 2,
        vintage == "MHOUR_JV_1" ~ 3,
        TRUE ~ 4
      )
    ) %>%
    group_by(periodo, anno, trimestre, anno_trimestre, dim_aziendale) %>%
    # Seleziona solo il vintage con priorita piu alta (numero piu basso)
    filter(vintage_priority == min(vintage_priority)) %>%
    # Se ancora duplicati (stesso vintage priority), prendi il primo
    slice(1) %>%
    ungroup() %>%
    select(-vintage_priority) %>%
    arrange(anno, trimestre, dim_aziendale)

  cat("  Righe aggregate:", nrow(data_nazionale), "\n")
  cat("  Vintage utilizzati per periodo:\n")
  vintage_usage <- data_nazionale %>%
    group_by(vintage) %>%
    summarise(
      n_periodi = n_distinct(periodo),
      periodo_min = min(periodo),
      periodo_max = max(periodo)
    )
  print(vintage_usage)

  return(data_nazionale)
}

# Document vintage metadata
document_vintages <- function(data) {
  cat("  Documentazione vintages MHOUR_JV...\n")

  vintage_summary <- data %>%
    filter(!is.na(vintage), vintage != "Other") %>%
    group_by(vintage) %>%
    summarise(
      n_obs = n(),
      periodo_min = min(periodo, na.rm = TRUE),
      periodo_max = max(periodo, na.rm = TRUE),
      anno_min = min(anno, na.rm = TRUE),
      anno_max = max(anno, na.rm = TRUE),
      ore_totali = sum(ore_migliaia, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(anno_min)

  return(vintage_summary)
}

# Validation function
validate_dataset <- function(data, name) {
  cat("\nDataset:", name, "\n")
  cat("  Righe:", nrow(data), "\n")
  cat("  Colonne:", ncol(data), "\n")

  if ("anno" %in% names(data)) {
    cat("  Anni:", paste(sort(unique(data$anno)), collapse = ", "), "\n")
    cat("  Range temporale:", min(data$anno), "-", max(data$anno), "\n")
  }

  if ("periodo" %in% names(data)) {
    cat("  Trimestri:", length(unique(data$periodo)), "\n")
    cat(
      "  Periodo:",
      min(data$periodo, na.rm = TRUE),
      "to",
      max(data$periodo, na.rm = TRUE),
      "\n"
    )
  }

  if ("settore_macro" %in% names(data)) {
    n_macro <- length(unique(data$settore_macro[!is.na(data$settore_macro)]))
    cat("  Settori macro:", n_macro, "\n")
  }

  if (
    "ore_migliaia" %in% names(data) || "ore_totali_migliaia" %in% names(data)
  ) {
    ore_col <- if ("ore_migliaia" %in% names(data)) {
      "ore_migliaia"
    } else {
      "ore_totali_migliaia"
    }
    cat(
      "  Ore (000s) range:",
      sprintf("%.1f", min(data[[ore_col]], na.rm = TRUE)),
      "-",
      sprintf("%.1f", max(data[[ore_col]], na.rm = TRUE)),
      "\n"
    )
    cat(
      "  Ore totali:",
      sprintf("%.0f", sum(data[[ore_col]], na.rm = TRUE)),
      "migliaia\n"
    )
  }

  cat("\n")
}

# 3. Funzioni analisi (da 10_analisi_imprese.R) -----

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

# 4. Caricamento e pulizia dati (09_prepara_imprese.R) -----

cat("==== Sezione 4: Caricamento e Pulizia Dati ====\n\n")

cat("Caricamento file imprese con dipendenti...\n")
imprese_raw <- readRDS("data/imprese_con_dipendenti.rds")

cat("  Righe totali:", nrow(imprese_raw), "\n")
cat(
  "  Periodo complessivo:",
  min(imprese_raw$ObsDimension),
  "to",
  max(imprese_raw$ObsDimension),
  "\n\n"
)

cat("Pulizia e trasformazione dati...\n")
imprese_clean <- clean_imprese_data(imprese_raw, period_start = "2015-Q1")

cat("\nCreazione aggregati nazionali...\n")
imprese_nazionale <- aggregate_national_level(imprese_clean)

cat("\nDocumentazione vintages...\n")
vintage_doc <- document_vintages(imprese_clean)

cat("\n")
print(vintage_doc)
cat("\n")

cat("==== Validazione Dati ====\n")
validate_dataset(imprese_clean, "Imprese Pulito")
validate_dataset(imprese_nazionale, "Aggregato Nazionale")

# Check vintage distribution
cat("Distribuzione vintage per anno:\n")
vintage_per_anno <- imprese_clean %>%
  group_by(anno, vintage) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = vintage, values_from = n, values_fill = 0)

print(vintage_per_anno)
cat("\n")

# Salvataggio dati preparati
cat("Salvataggio dati preparati...\n")
saveRDS(imprese_clean, "output/imprese/imprese_clean.rds")
cat("Salvato: output/imprese/imprese_clean.rds\n")

saveRDS(imprese_nazionale, "output/imprese/imprese_nazionale.rds")
cat("Salvato: output/imprese/imprese_nazionale.rds\n")

saveRDS(vintage_doc, "output/imprese/vintage_documentation.rds")
cat("Salvato: output/imprese/vintage_documentation.rds\n\n")

# 5. Analisi statistiche (10_analisi_imprese.R) -----

cat("==== Sezione 5: Analisi Statistiche ====\n\n")

cat("NOTA IMPORTANTE:\n")
cat("MHOUR_JV e un INDICE (base 2021=100), NON valori assoluti.\n")
cat("- Codice '0015' = TOTALE INDUSTRIA E SERVIZI (aggregato ufficiale)\n")
cat("- I valori rappresentano ore lavorate relative al 2021\n")
cat("- Non vanno sommati tra settori diversi\n\n")

# 5.1 Estrazione totale economia (codice 0015)
cat("--- 5.1 Estrazione Totale Economia (codice 0015) ---\n\n")

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

saveRDS(totale_economia, "output/imprese/totale_economia_0015.rds")
saveRDS(totale_wide, "output/imprese/totale_economia_0015_wide.rds")
cat("Salvato: output/imprese/totale_economia_0015.rds\n")
cat("Salvato: output/imprese/totale_economia_0015_wide.rds\n\n")

# 5.2 Statistiche descrittive per settore
cat("--- 5.2 Statistiche Descrittive per Settore ---\n\n")

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

saveRDS(stat_settori, "output/imprese/stat_descrittive_settori.rds")
cat("Salvato: output/imprese/stat_descrittive_settori.rds\n\n")

# 5.3 Ranking settori per indice ore (ultimo trimestre)
cat("--- 5.3 Ranking Settori per Indice Ore (2025-Q3) ---\n\n")

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

saveRDS(ranking_2025Q3, "output/imprese/ranking_settori_indice_2025Q3.rds")
cat("Salvato: output/imprese/ranking_settori_indice_2025Q3.rds\n\n")

# 5.4 Evoluzione temporale totale economia
cat("--- 5.4 Evoluzione Temporale Totale Economia ---\n\n")

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

saveRDS(evoluzione_totale, "output/imprese/evoluzione_totale_economia.rds")
cat("Salvato: output/imprese/evoluzione_totale_economia.rds\n\n")

# 5.5 Evoluzione settoriale (top 5 settori)
cat("--- 5.5 Evoluzione Settoriale (Top 5 Settori) ---\n\n")

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

saveRDS(evoluzione_settori, "output/imprese/evoluzione_settori_top5.rds")
cat("Salvato: output/imprese/evoluzione_settori_top5.rds\n\n")

# 5.6 Confronto dimensione aziendale (totale economia)
cat("--- 5.6 Confronto Dimensione Aziendale ---\n\n")

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
  "NOTA: W_GE10 (imprese >=10 dipendenti) e un sottoinsieme di W_GE1 (imprese >=1 dipendente).\n"
)
cat(
  "Il gap misura la quota delle imprese >=10 dip sul totale ore lavorate.\n\n"
)

saveRDS(
  confronto_dimensione,
  "output/imprese/confronto_dimensione_aziendale.rds"
)
cat("Salvato: output/imprese/confronto_dimensione_aziendale.rds\n\n")

# 5.7 Analisi volatilita settoriale
cat("--- 5.7 Analisi Volatilita Settoriale ---\n\n")

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

cat("Volatilita settoriale (ordinata per SD variazioni trimestrali):\n")
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

saveRDS(volatilita_settori, "output/imprese/volatilita_settori.rds")
cat("Salvato: output/imprese/volatilita_settori.rds\n\n")

# 5.8 Decomposizione crescita 2015-2025 per settore
cat("--- 5.8 Decomposizione Crescita 2015-2025 ---\n\n")

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

saveRDS(
  decomp_crescita_settori,
  "output/imprese/decomposizione_crescita_settori.rds"
)
saveRDS(decomp_totale, "output/imprese/decomposizione_crescita_totale.rds")
cat("Salvato: output/imprese/decomposizione_crescita_settori.rds\n")
cat("Salvato: output/imprese/decomposizione_crescita_totale.rds\n\n")

# 5.9 Sintesi generale
cat("--- 5.9 Sintesi Statistica Generale ---\n\n")

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

  # Volatilita
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
cat("  Indice piu alto 2025-Q3:", sintesi$settore_max_indice_2025, "\n")
cat("  Indice piu basso 2025-Q3:", sintesi$settore_min_indice_2025, "\n\n")

cat("Dimensione aziendale (W_GE10 vs W_GE1):\n")
cat("  Gap medio periodo:", sprintf("%.2f%%", sintesi$gap_W_GE10_medio), "\n")
cat("  Gap 2025-Q3:", sprintf("%.2f%%", sintesi$gap_W_GE10_2025Q3), "\n")
cat("  (W_GE10 >=10 dip e sottoinsieme di W_GE1 >=1 dip)\n\n")

cat("Volatilita:\n")
cat("  Settore piu volatile:", sintesi$settore_piu_volatile, "\n")
cat("  Settore meno volatile:", sintesi$settore_meno_volatile, "\n\n")

saveRDS(sintesi, "output/imprese/sintesi_imprese.rds")
cat("Salvato: output/imprese/sintesi_imprese.rds\n\n")

# 6. Visualizzazioni (11_visualizzazioni_imprese.R) -----

cat("==== Sezione 6: Visualizzazioni ====\n\n")

cat("Generazione 13 grafici in corso...\n\n")

# 6.1 Grafico 21: Evoluzione totale economia
cat("1. Evoluzione indice ore totale economia (W_GE1 e W_GE10)...\n")

p21_data <- totale_wide %>%
  select(periodo, anno, trimestre, anno_trimestre, W_GE1, W_GE10) %>%
  pivot_longer(
    cols = c(W_GE1, W_GE10),
    names_to = "dimensione",
    values_to = "indice"
  )

p21 <- p21_data %>%
  ggplot(aes(x = anno_trimestre, y = indice, color = dimensione)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5, alpha = 0.6) +
  geom_hline(
    yintercept = 100,
    linetype = "dashed",
    color = "gray40",
    alpha = 0.7
  ) +
  geom_vline(
    xintercept = 2020.25,
    linetype = "dashed",
    color = "red",
    alpha = 0.5
  ) +
  annotate(
    "text",
    x = 2020.25,
    y = 120,
    label = "COVID-19",
    hjust = -0.1,
    color = "red",
    size = 3
  ) +
  annotate(
    "text",
    x = 2021,
    y = 100,
    label = "Base 2021",
    hjust = 0,
    vjust = -0.5,
    color = "gray40",
    size = 3
  ) +
  scale_color_manual(
    values = c("W_GE1" = "#2E86AB", "W_GE10" = "#A23B72"),
    labels = c("W_GE1" = ">=1 dipendente", "W_GE10" = ">=10 dipendenti")
  ) +
  scale_y_continuous(breaks = seq(70, 120, 10)) +
  labs(
    title = "Evoluzione Indice Ore Lavorate - Totale Economia",
    subtitle = "Italia 2015-2025 (indice base 2021=100, dati destagionalizzati)",
    x = "Anno",
    y = "Indice ore lavorate (2021=100)",
    color = "Dimensione aziendale",
    caption = "Fonte: ISTAT - MHOUR_JV_2021"
  ) +
  theme_salari()

ggsave(
  "output/imprese/grafici/21_evoluzione_indice_totale.png",
  p21,
  width = 10,
  height = 6,
  dpi = 300
)

# 6.2 Grafico 22: Evoluzione top 5 settori
cat("2. Evoluzione top 5 settori...\n")

p22 <- evoluzione_settori %>%
  ggplot(aes(
    x = anno + (trimestre - 1) / 4,
    y = ore_migliaia,
    color = settore_macro_label
  )) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.2, alpha = 0.6) +
  geom_hline(
    yintercept = 100,
    linetype = "dashed",
    color = "gray40",
    alpha = 0.5
  ) +
  scale_color_viridis_d(option = "plasma") +
  labs(
    title = "Evoluzione Indice Ore Top 5 Settori (2025-Q3)",
    subtitle = "Italia 2015-2025 (indice base 2021=100, W_GE1)",
    x = "Anno",
    y = "Indice ore lavorate (2021=100)",
    color = "Settore"
  ) +
  theme_salari() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 2))

ggsave(
  "output/imprese/grafici/22_evoluzione_top5_settori.png",
  p22,
  width = 10,
  height = 6,
  dpi = 300
)

# 6.3 Grafico 23: Crescita settoriale 2015-2025
cat("3. Crescita settoriale 2015-2025...\n")

p23 <- decomp_crescita_settori %>%
  mutate(
    settore_label = reorder(settore_macro_label, var_percentuale),
    crescita_tipo = ifelse(var_percentuale >= 0, "Positiva", "Negativa")
  ) %>%
  ggplot(aes(x = var_percentuale, y = settore_label, fill = crescita_tipo)) +
  geom_col() +
  geom_vline(xintercept = 0, linetype = "solid", color = "black") +
  scale_fill_manual(
    values = c("Positiva" = "#2E86AB", "Negativa" = "#A23B72")
  ) +
  labs(
    title = "Crescita Settoriale 2015-Q1 to 2025-Q3",
    subtitle = "Variazione percentuale indice ore lavorate (W_GE1)",
    x = "Variazione %",
    y = NULL,
    fill = "Tipo crescita"
  ) +
  theme_salari() +
  theme(axis.text.y = element_text(size = 8))

ggsave(
  "output/imprese/grafici/23_crescita_settoriale.png",
  p23,
  width = 10,
  height = 8,
  dpi = 300
)

# 6.4 Grafico 24: Confronto W_GE1 vs W_GE10 (gap percentuale)
cat("4. Gap percentuale W_GE10 vs W_GE1...\n")

p24 <- confronto_dimensione %>%
  ggplot(aes(x = anno_trimestre, y = diff_percentuale)) +
  geom_line(linewidth = 1, color = "#A23B72") +
  geom_point(size = 1.5, alpha = 0.6, color = "#A23B72") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  annotate(
    "text",
    x = 2017,
    y = 0,
    label = "W_GE10 = W_GE1",
    hjust = 0,
    vjust = -0.5,
    color = "gray40",
    size = 3
  ) +
  annotate(
    "rect",
    xmin = 2015,
    xmax = 2026,
    ymin = -Inf,
    ymax = 0,
    fill = "#A23B72",
    alpha = 0.1
  ) +
  annotate(
    "text",
    x = 2020,
    y = -2,
    label = "W_GE10 (>=10 dip) sottoinsieme di W_GE1",
    hjust = 0.5,
    color = "#A23B72",
    size = 3
  ) +
  labs(
    title = "Differenziale Indice Ore: W_GE10 vs W_GE1",
    subtitle = "Totale economia 2015-2025 (W_GE10 >=10 dip e sottoinsieme di W_GE1 >=1 dip)",
    x = "Anno",
    y = "Gap percentuale (%)",
    caption = "Nota: W_GE10 (imprese con 10 o piu dipendenti) e sottoinsieme di W_GE1 (imprese con 1 o piu dipendenti)"
  ) +
  theme_salari()

ggsave(
  "output/imprese/grafici/24_gap_W_GE10_W_GE1.png",
  p24,
  width = 10,
  height = 6,
  dpi = 300
)

# 6.5 Grafico 25: Ranking settori 2025-Q3
cat("5. Ranking settori per indice (2025-Q3)...\n")

top_bottom <- bind_rows(
  ranking_2025Q3 %>% head(8) %>% mutate(gruppo = "Top 8"),
  ranking_2025Q3 %>% tail(8) %>% mutate(gruppo = "Bottom 8")
)

p25 <- top_bottom %>%
  mutate(settore_label = reorder(settore_macro_label, ore_migliaia)) %>%
  ggplot(aes(x = ore_migliaia, y = settore_label, fill = gruppo)) +
  geom_col() +
  geom_vline(xintercept = 100, linetype = "dashed", color = "gray40") +
  scale_fill_manual(values = c("Top 8" = "#2E86AB", "Bottom 8" = "#A23B72")) +
  labs(
    title = "Ranking Settori per Indice Ore (2025-Q3)",
    subtitle = "Top 8 e Bottom 8 settori (indice base 2021=100, W_GE1)",
    x = "Indice ore lavorate (2021=100)",
    y = NULL,
    fill = "Gruppo"
  ) +
  theme_salari() +
  theme(axis.text.y = element_text(size = 8))

ggsave(
  "output/imprese/grafici/25_ranking_settori_2025Q3.png",
  p25,
  width = 10,
  height = 8,
  dpi = 300
)

# 6.6 Grafico 26: Heatmap settori x tempo
cat("6. Heatmap settori x tempo...\n")

heatmap_data <- evoluzione_settori %>%
  mutate(anno_trim = paste0(anno, "-Q", trimestre))

# Seleziona solo alcuni periodi per leggibilita
periodi_selezionati <- heatmap_data %>%
  distinct(anno_trim, anno, trimestre) %>%
  filter(trimestre == 1 | (anno == 2025 & trimestre == 3)) %>%
  pull(anno_trim)

p26 <- heatmap_data %>%
  filter(anno_trim %in% periodi_selezionati) %>%
  ggplot(aes(x = anno_trim, y = settore_macro_label, fill = ore_migliaia)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "plasma") +
  labs(
    title = "Heatmap Indice Ore per Settore nel Tempo",
    subtitle = "Top 5 settori, Q1 di ogni anno (indice base 2021=100)",
    x = "Periodo",
    y = NULL,
    fill = "Indice"
  ) +
  theme_salari() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8)
  )

ggsave(
  "output/imprese/grafici/26_heatmap_settori_tempo.png",
  p26,
  width = 10,
  height = 6,
  dpi = 300
)

# 6.7 Grafico 27: Variazioni trimestrali e annuali
cat("7. Variazioni trimestrali e annuali totale economia...\n")

p27_data <- evoluzione_totale %>%
  select(periodo, anno, trimestre, var_trim_W_GE1, var_anno_W_GE1) %>%
  filter(!is.na(var_trim_W_GE1)) %>%
  pivot_longer(
    cols = c(var_trim_W_GE1, var_anno_W_GE1),
    names_to = "tipo_var",
    values_to = "variazione"
  ) %>%
  mutate(
    tipo_var_label = ifelse(
      tipo_var == "var_trim_W_GE1",
      "Trimestrale",
      "Annuale"
    )
  )

p27 <- p27_data %>%
  ggplot(aes(
    x = anno + (trimestre - 1) / 4,
    y = variazione,
    color = tipo_var_label
  )) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  geom_vline(
    xintercept = 2020.25,
    linetype = "dashed",
    color = "red",
    alpha = 0.5
  ) +
  scale_color_manual(
    values = c("Trimestrale" = "#2E86AB", "Annuale" = "#A23B72")
  ) +
  labs(
    title = "Variazioni Indice Ore - Totale Economia (W_GE1)",
    subtitle = "Italia 2015-2025 (variazioni % trimestrali e annuali)",
    x = "Anno",
    y = "Variazione %",
    color = "Tipo variazione"
  ) +
  theme_salari()

ggsave(
  "output/imprese/grafici/27_variazioni_totale.png",
  p27,
  width = 10,
  height = 6,
  dpi = 300
)

# 6.8 Grafico 28: Volatilita settoriale
cat("8. Volatilita settoriale...\n")

p28 <- volatilita_settori %>%
  mutate(settore_label = reorder(settore_macro_label, sd_var_trim)) %>%
  ggplot(aes(x = sd_var_trim, y = settore_label)) +
  geom_col(fill = "#2E86AB") +
  labs(
    title = "Volatilita Settoriale (Deviazione Standard Variazioni Trimestrali)",
    subtitle = "Top 5 settori 2015-2025 (W_GE1)",
    x = "SD variazioni % trimestrali",
    y = NULL
  ) +
  theme_salari()

ggsave(
  "output/imprese/grafici/28_volatilita_settori.png",
  p28,
  width = 10,
  height = 6,
  dpi = 300
)

# 6.9 Grafico 29: Scatter volatilita vs crescita
cat("9. Scatter volatilita vs crescita settoriale...\n")

scatter_data <- volatilita_settori %>%
  left_join(
    decomp_crescita_settori %>% select(settore_macro, var_percentuale),
    by = "settore_macro"
  )

p29 <- scatter_data %>%
  ggplot(aes(x = var_percentuale, y = sd_var_trim, label = settore_macro)) +
  geom_point(size = 3, color = "#2E86AB", alpha = 0.6) +
  geom_text(hjust = 0, vjust = 0, nudge_x = 1, nudge_y = 0.1, size = 3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  labs(
    title = "Relazione Volatilita - Crescita Settoriale",
    subtitle = "Top 5 settori 2015-2025 (W_GE1)",
    x = "Crescita 2015-2025 (%)",
    y = "Volatilita (SD variazioni % trimestrali)"
  ) +
  theme_salari()

ggsave(
  "output/imprese/grafici/29_scatter_volatilita_crescita.png",
  p29,
  width = 10,
  height = 6,
  dpi = 300
)

# 6.10 Grafico 30: Box plot indici per settore
cat("10. Box plot distribuzione indici per settore...\n")

p30_data <- stat_settori %>%
  filter(dim_aziendale == "W_GE1") %>%
  mutate(settore_label = reorder(settore_macro_label, indice_medio))

p30 <- p30_data %>%
  ggplot(aes(y = settore_label)) +
  geom_segment(
    aes(x = indice_min, xend = indice_max, yend = settore_label),
    linewidth = 1,
    color = "#2E86AB"
  ) +
  geom_point(aes(x = indice_medio), size = 3, color = "#A23B72") +
  geom_vline(xintercept = 100, linetype = "dashed", color = "gray40") +
  labs(
    title = "Distribuzione Indice Ore per Settore (2015-2025)",
    subtitle = "Range min-max e media per settore (W_GE1)",
    x = "Indice ore lavorate (2021=100)",
    y = NULL
  ) +
  theme_salari() +
  theme(axis.text.y = element_text(size = 7))

ggsave(
  "output/imprese/grafici/30_boxplot_settori.png",
  p30,
  width = 10,
  height = 8,
  dpi = 300
)

# 6.11 Grafico 31: Decomposizione CAGR
cat("11. CAGR settoriale 2015-2025...\n")

p31 <- decomp_crescita_settori %>%
  mutate(
    settore_label = reorder(settore_macro_label, cagr),
    cagr_tipo = ifelse(cagr >= 0, "Positivo", "Negativo")
  ) %>%
  ggplot(aes(x = cagr, y = settore_label, fill = cagr_tipo)) +
  geom_col() +
  geom_vline(xintercept = 0, linetype = "solid", color = "black") +
  scale_fill_manual(
    values = c("Positivo" = "#2E86AB", "Negativo" = "#A23B72")
  ) +
  labs(
    title = "CAGR Settoriale 2015-2025",
    subtitle = "Tasso di crescita annuale composto indice ore (W_GE1)",
    x = "CAGR (%)",
    y = NULL,
    fill = "Tipo"
  ) +
  theme_salari() +
  theme(axis.text.y = element_text(size = 8))

ggsave(
  "output/imprese/grafici/31_cagr_settoriale.png",
  p31,
  width = 10,
  height = 8,
  dpi = 300
)

# 6.12 Grafico 32: Shock COVID per settore
cat("12. Shock COVID-19 per settore...\n")

shock_covid <- evoluzione_settori %>%
  filter(anno %in% c(2019, 2020)) %>%
  group_by(settore_macro, settore_macro_label, anno) %>%
  summarise(
    indice_medio = mean(ore_migliaia, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = anno,
    values_from = indice_medio,
    names_prefix = "anno_"
  ) %>%
  mutate(
    shock = ((anno_2020 - anno_2019) / anno_2019) * 100,
    settore_label = reorder(settore_macro_label, shock)
  )

p32 <- shock_covid %>%
  ggplot(aes(x = shock, y = settore_label, fill = shock < 0)) +
  geom_col() +
  geom_vline(xintercept = 0, linetype = "solid", color = "black") +
  scale_fill_manual(
    values = c("TRUE" = "#A23B72", "FALSE" = "#2E86AB"),
    guide = "none"
  ) +
  labs(
    title = "Impatto COVID-19 per Settore (2019 vs 2020)",
    subtitle = "Variazione % media annuale indice ore (Top 5 settori, W_GE1)",
    x = "Variazione % 2019-2020",
    y = NULL
  ) +
  theme_salari()

ggsave(
  "output/imprese/grafici/32_shock_covid_settori.png",
  p32,
  width = 10,
  height = 6,
  dpi = 300
)

# 6.13 Grafico 33: Recupero post-COVID
cat("13. Recupero post-COVID...\n")

recupero_covid <- totale_wide %>%
  filter(anno %in% 2019:2023) %>%
  select(periodo, anno, trimestre, W_GE1) %>%
  mutate(
    baseline_2019 = W_GE1[anno == 2019 & trimestre == 4],
    recupero_pct = (W_GE1 - baseline_2019) / baseline_2019 * 100
  )

p33 <- recupero_covid %>%
  ggplot(aes(x = anno + (trimestre - 1) / 4, y = recupero_pct)) +
  geom_line(linewidth = 1, color = "#2E86AB") +
  geom_point(size = 1.5, alpha = 0.6, color = "#2E86AB") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  geom_vline(
    xintercept = 2019.75,
    linetype = "dashed",
    color = "red",
    alpha = 0.5
  ) +
  annotate(
    "text",
    x = 2019.75,
    y = 5,
    label = "Pre-COVID",
    hjust = -0.1,
    color = "red",
    size = 3
  ) +
  annotate(
    "rect",
    xmin = 2020,
    xmax = 2020.75,
    ymin = -Inf,
    ymax = Inf,
    fill = "red",
    alpha = 0.1
  ) +
  labs(
    title = "Recupero Post-COVID Totale Economia",
    subtitle = "Variazione % rispetto a 2019-Q4 (baseline pre-COVID, W_GE1)",
    x = "Anno",
    y = "Variazione % vs 2019-Q4"
  ) +
  theme_salari()

ggsave(
  "output/imprese/grafici/33_recupero_post_covid.png",
  p33,
  width = 10,
  height = 6,
  dpi = 300
)

# 7. Salvataggio finale -----

cat("\n==== Sezione 7: Riepilogo Finale ====\n\n")

cat("Pipeline imprese completata con successo.\n\n")

cat("File RDS salvati in output/imprese/:\n")
cat("- imprese_clean.rds\n")
cat("- imprese_nazionale.rds\n")
cat("- vintage_documentation.rds\n")
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
cat("- sintesi_imprese.rds\n\n")

cat("Grafici PNG salvati in output/imprese/grafici/:\n")
cat("21. Evoluzione indice totale economia (W_GE1 e W_GE10)\n")
cat("22. Evoluzione top 5 settori\n")
cat("23. Crescita settoriale 2015-2025\n")
cat("24. Gap percentuale W_GE10 vs W_GE1\n")
cat("25. Ranking settori 2025-Q3\n")
cat("26. Heatmap settori x tempo\n")
cat("27. Variazioni trimestrali e annuali totale economia\n")
cat("28. Volatilita settoriale\n")
cat("29. Scatter volatilita vs crescita\n")
cat("30. Box plot distribuzione indici per settore\n")
cat("31. CAGR settoriale\n")
cat("32. Shock COVID-19 per settore\n")
cat("33. Recupero post-COVID\n\n")

cat("Totale grafici generati: 13\n")
cat("==== Pipeline completata ====\n")
