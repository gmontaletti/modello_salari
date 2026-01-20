# 09_prepara_imprese.R -----
# Preparazione dati imprese: caricamento, cleaning e classificazione settori
# Autore: Giampaolo Montaletti (ORCID: 0009-0002-5327-1122)
# Data: 2026-01-15

# 1. Setup ambiente -----
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(data.table)
})

cat("==== Preparazione Dati Imprese ====\n\n")

# 2. Funzioni utility -----

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

# RIMOSSO: funzioni inventate per classificazione NACE
# Useremo istatlab::add_istat_labels() per labels ufficiali

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
      # Converti a character prima di elaborare (apply_labels può creare fattori)
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
  cat("  Applicazione priorità vintage...\n")
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

  # Gestione vintage con priorità (più recente vince)
  # Priorità: MHOUR_JV_2021 > MHOUR_JV_2 > MHOUR_JV_1 > Other
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
    # Seleziona solo il vintage con priorità più alta (numero più basso)
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

# 3. Caricamento dati -----

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

# 4. Pulizia e trasformazione -----

cat("Pulizia e trasformazione dati...\n")
imprese_clean <- clean_imprese_data(imprese_raw, period_start = "2015-Q1")

cat("\nCreazione aggregati nazionali...\n")
imprese_nazionale <- aggregate_national_level(imprese_clean)

cat("\nDocumentazione vintages...\n")
vintage_doc <- document_vintages(imprese_clean)

cat("\n")
print(vintage_doc)
cat("\n")

# 5. Validazione -----

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

# 6. Salvataggio -----

cat("==== Salvataggio Dataset ====\n\n")

if (!dir.exists("output")) {
  dir.create("output", recursive = TRUE)
}

saveRDS(imprese_clean, "output/imprese_clean.rds")
cat("Salvato: output/imprese_clean.rds\n")

saveRDS(imprese_nazionale, "output/imprese_nazionale.rds")
cat("Salvato: output/imprese_nazionale.rds\n")

saveRDS(vintage_doc, "output/vintage_documentation.rds")
cat("Salvato: output/vintage_documentation.rds\n")

cat("\n==== Script completato con successo ====\n")
