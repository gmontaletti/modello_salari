# 01_prepara_dati_racli.R -----
# Preparazione dati RACLI: caricamento, cleaning e integrazione
# Autore: Giampaolo Montaletti (ORCID: 0009-0002-5327-1122)
# Data: 2026-01-14

# 1. Setup ambiente -----
suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(tidyr)
})

cat("==== Preparazione Dati RACLI ====\n\n")

# 2. Funzioni utility -----

# Identifica livello geografico NUTS
get_geo_level <- function(code) {
  code <- as.character(code)
  n <- nchar(code)
  case_when(
    code == "IT" ~ "Nazionale",
    n == 3 ~ "Ripartizione",
    n == 4 ~ "Regione",
    n >= 5 ~ "Provincia",
    TRUE ~ "Altro"
  )
}

# Carica tabella di lookup ripartizioni da CL_ITTER107
load_ripartizione_lookup <- function() {
  codelists <- readRDS("meta/codelists.rds")
  cl_itter <- codelists[["CL_ITTER107"]]

  # Estrai etichette ripartizione (codici a 3 caratteri: ITC, ITD, ITE, ITF, ITG)
  ripart_lookup <- cl_itter[
    nchar(id_description) == 3 &
      id_description %in% c("ITC", "ITD", "ITE", "ITF", "ITG"),
    .(code = id_description, label = it_description)
  ]

  # Aggiungi livello nazionale
  ripart_lookup <- rbind(
    data.table::data.table(code = "IT", label = "Italia"),
    ripart_lookup
  )

  return(ripart_lookup)
}

# Mappa codice NUTS a ripartizione usando etichette ISTAT ufficiali
get_ripartizione <- function(code, ripart_lookup = NULL) {
  if (is.null(ripart_lookup)) {
    ripart_lookup <- load_ripartizione_lookup()
  }

  code <- as.character(code)

  # Per ogni codice, estrai prefisso ripartizione
  ripart_code <- case_when(
    code == "IT" ~ "IT",
    nchar(code) >= 3 ~ substr(code, 1, 3),
    TRUE ~ code
  )

  # Gestisci mappature alternative ITH → ITD e ITI → ITE
  ripart_code <- case_when(
    ripart_code == "ITH" ~ "ITD",
    ripart_code == "ITI" ~ "ITE",
    TRUE ~ ripart_code
  )

  # Gestisci province speciali IT1XX (nuove province post-2004)
  # IT108 = Monza e della Brianza (Lombardia = ITC)
  # IT109 = Fermo (Marche = ITE)
  # IT110 = Barletta-Andria-Trani (Puglia = ITF)
  # IT111 = Sud Sardegna (Sardegna = ITG)
  ripart_code <- case_when(
    code == "IT108" ~ "ITC",
    code == "IT109" ~ "ITE",
    code == "IT110" ~ "ITF",
    code == "IT111" ~ "ITG",
    TRUE ~ ripart_code
  )

  # Lookup etichetta ufficiale dalla codelist
  labels <- ripart_lookup$label[match(ripart_code, ripart_lookup$code)]

  # Ritorna codice se non trovata etichetta (fallback)
  ifelse(is.na(labels), ripart_code, labels)
}

# Carica mapping ITTER107 -> COD_UTS
load_itter_mapping <- function() {
  mapping_file <- "meta/mapping_itter_cod_uts.rds"
  if (!file.exists(mapping_file)) {
    cat(
      "ATTENZIONE: mapping ITTER107 non trovato, eseguire 00_crea_mapping_itter.R\n"
    )
    return(NULL)
  }
  readRDS(mapping_file)
}

# Aggiunge codici ISTAT (cod_uts, cod_reg, cod_rip) ai dati
add_istat_codes <- function(data, mapping) {
  if (is.null(mapping)) {
    return(data)
  }

  # Join per codice ITTER107 (campo 'area')
  data_with_codes <- data %>%
    left_join(
      mapping[, c("itter107", "cod_uts", "cod_reg", "cod_rip")],
      by = c("area" = "itter107")
    )

  # Verifica match rate per province
  prov_data <- data_with_codes[data_with_codes$geo_level == "Provincia", ]
  if (nrow(prov_data) > 0) {
    n_matched <- sum(!is.na(prov_data$cod_uts))
    n_total <- nrow(prov_data)
    match_rate <- n_matched / n_total * 100

    if (match_rate < 100) {
      unmatched <- unique(prov_data$area[is.na(prov_data$cod_uts)])
      cat(
        "  ATTENZIONE: match rate codici ISTAT:",
        sprintf("%.1f%%", match_rate),
        "\n"
      )
      cat("  Province non matchate:", paste(unmatched, collapse = ", "), "\n")
    }
  }

  return(data_with_codes)
}

# Pulisce e standardizza dataset RACLI
clean_racli_data <- function(data, itter_mapping = NULL) {
  if (is.null(data)) {
    return(NULL)
  }

  # Rinomina colonne base
  data_clean <- data %>%
    mutate(
      anno = tempo_temp,
      area = as.character(REF_AREA),
      area_label = as.character(REF_AREA_label),
      # Mappa tipo dato a nomi user-friendly
      tipo_dato_clean = case_when(
        grepl("_AV_", DATA_TYPE) ~ "salario_medio",
        grepl("_FIRD_", DATA_TYPE) ~ "D1",
        grepl("_MED_", DATA_TYPE) ~ "salario_mediano",
        grepl("_NIND_", DATA_TYPE) ~ "D9",
        TRUE ~ as.character(DATA_TYPE)
      )
    )

  # Aggiungi colonne dimensionali se presenti
  if ("SEX" %in% names(data_clean)) {
    data_clean <- data_clean %>%
      mutate(sesso = as.character(SEX_label))
  }

  if ("AGE" %in% names(data_clean)) {
    data_clean <- data_clean %>%
      mutate(eta = as.character(AGE_label))
  }

  if ("EDU_LEV_HIGHEST" %in% names(data_clean)) {
    data_clean <- data_clean %>%
      mutate(educazione = as.character(EDU_LEV_HIGHEST_label))
  }

  if ("TYPE_OF_CONTRACT" %in% names(data_clean)) {
    data_clean <- data_clean %>%
      mutate(contratto = as.character(TYPE_OF_CONTRACT_label))
  }

  if ("EMPLOYESS_CLASS" %in% names(data_clean)) {
    data_clean <- data_clean %>%
      mutate(dim_aziendale = as.character(EMPLOYESS_CLASS_label))
  }

  # Identifica colonne ID per pivot
  id_cols <- c("anno", "area", "area_label")

  if ("sesso" %in% names(data_clean)) {
    id_cols <- c(id_cols, "sesso")
  }
  if ("eta" %in% names(data_clean)) {
    id_cols <- c(id_cols, "eta")
  }
  if ("educazione" %in% names(data_clean)) {
    id_cols <- c(id_cols, "educazione")
  }
  if ("contratto" %in% names(data_clean)) {
    id_cols <- c(id_cols, "contratto")
  }
  if ("dim_aziendale" %in% names(data_clean)) {
    id_cols <- c(id_cols, "dim_aziendale")
  }

  # Pivota tipo_dato
  data_wide <- data_clean %>%
    select(all_of(c(id_cols, "tipo_dato_clean", "valore"))) %>%
    pivot_wider(
      names_from = tipo_dato_clean,
      values_from = valore,
      values_fn = mean
    )

  # Aggiungi D5 come alias di salario_mediano se mancante
  if ("salario_mediano" %in% names(data_wide) && !"D5" %in% names(data_wide)) {
    data_wide <- data_wide %>%
      mutate(D5 = salario_mediano)
  }

  # Aggiungi geo_level e ripartizione
  data_wide <- data_wide %>%
    mutate(
      geo_level = get_geo_level(area),
      ripartizione = get_ripartizione(area)
    )

  # Calcola variabili derivate se abbiamo decili
  if (all(c("D1", "D5", "D9") %in% names(data_wide))) {
    data_wide <- data_wide %>%
      mutate(
        D9_D1 = D9 / D1,
        D9_D5 = D9 / D5,
        D5_D1 = D5 / D1
      )
  }

  return(data_wide)
}

# Pulisce e standardizza dataset RACLI settoriale (File 17)
clean_racli_sector_data <- function(data) {
  if (is.null(data)) {
    return(NULL)
  }

  # Rinomina e mappa colonne
  data_clean <- data %>%
    mutate(
      anno = tempo_temp,
      settore_code = as.character(ECON_ACTIVITY_NACE_2007),
      settore = as.character(ECON_ACTIVITY_NACE_2007_label),
      sesso = as.character(SEX_label),
      tipo_dato_clean = case_when(
        grepl("_AV_", DATA_TYPE) ~ "salario_medio",
        grepl("_FIRD_", DATA_TYPE) ~ "D1",
        grepl("_MED_", DATA_TYPE) ~ "salario_mediano",
        grepl("_NIND_", DATA_TYPE) ~ "D9",
        TRUE ~ as.character(DATA_TYPE)
      )
    )

  # Pivota tipo_dato
  data_wide <- data_clean %>%
    select(anno, settore_code, settore, sesso, tipo_dato_clean, valore) %>%
    pivot_wider(
      names_from = tipo_dato_clean,
      values_from = valore,
      values_fn = mean
    )

  # Aggiungi D5 come alias di salario_mediano
  if ("salario_mediano" %in% names(data_wide) && !"D5" %in% names(data_wide)) {
    data_wide <- data_wide %>%
      mutate(D5 = salario_mediano)
  }

  # Calcola variabili derivate se abbiamo decili
  if (all(c("D1", "D5", "D9") %in% names(data_wide))) {
    data_wide <- data_wide %>%
      mutate(
        D9_D1 = D9 / D1,
        D9_D5 = D9 / D5,
        D5_D1 = D5 / D1
      )
  }

  # Identifica tipo settore (sezione principale vs dettaglio)
  data_wide <- data_wide %>%
    mutate(
      tipo_settore = case_when(
        settore_code %in% c("0010", "0011", "0020", "0038") ~ "Aggregato",
        nchar(settore_code) == 2 ~ "Sezione NACE",
        TRUE ~ "Dettaglio"
      )
    )

  return(data_wide)
}

# 3. Caricamento e preparazione dati -----

cat("Caricamento file RACLI...\n")

# File 8: SEX (sesso)
cat("  File 8: dati per sesso...")
racli_8_raw <- readRDS("racli/F_533_957_DF_DCSC_RACLI_8.rds")
cat(" OK (", nrow(racli_8_raw), "righe)\n")

# File 11: Livello educativo
cat("  File 11: livello educativo...")
racli_11_raw <- readRDS("racli/F_533_957_DF_DCSC_RACLI_11.rds")
cat(" OK (", nrow(racli_11_raw), "righe)\n")

# File 12: Tipo contratto
cat("  File 12: tipo contratto...")
racli_12_raw <- readRDS("racli/F_533_957_DF_DCSC_RACLI_12.rds")
cat(" OK (", nrow(racli_12_raw), "righe)\n")

# File 17: Settori NACE 2007
cat("  File 17: settori economici...")
racli_17_raw <- readRDS("racli/F_533_957_DF_DCSC_RACLI_17.rds")
cat(" OK (", nrow(racli_17_raw), "righe)\n")

cat("\n")

# 4. Carica mapping ITTER107 -> COD_UTS -----

cat("Caricamento mapping ITTER107 -> COD_UTS...\n")
itter_mapping <- load_itter_mapping()
if (!is.null(itter_mapping)) {
  cat("  Mapping caricato:", nrow(itter_mapping), "codici\n\n")
} else {
  cat(
    "  ATTENZIONE: mapping non disponibile, codici ISTAT non saranno aggiunti\n\n"
  )
}

# 5. Pulizia e trasformazione -----

cat("Pulizia e trasformazione dati...\n")

# Dataset 1: Sesso (File 8)
cat("  Preparazione dataset per sesso...")
dati_settore_sesso <- clean_racli_data(racli_8_raw)
dati_settore_sesso <- add_istat_codes(dati_settore_sesso, itter_mapping)
# Aggiungi colonna settore fittizia per compatibilità con script 02
dati_settore_sesso <- dati_settore_sesso %>%
  mutate(settore = "Totale economia")

cat(" OK (", nrow(dati_settore_sesso), "righe)\n")

# Dataset 2: Educazione (File 11)
cat("  Preparazione dataset educazione...")
dati_educazione <- clean_racli_data(racli_11_raw)
dati_educazione <- add_istat_codes(dati_educazione, itter_mapping)
dati_educazione <- dati_educazione %>%
  mutate(settore = "Totale economia")

cat(" OK (", nrow(dati_educazione), "righe)\n")

# Dataset 3: Tipo contratto (File 12)
cat("  Preparazione dataset tipo contratto...")
dati_contratto <- clean_racli_data(racli_12_raw)
dati_contratto <- add_istat_codes(dati_contratto, itter_mapping)
dati_contratto <- dati_contratto %>%
  mutate(settore = "Totale economia")

cat(" OK (", nrow(dati_contratto), "righe)\n")

# Dataset 4: Settori NACE (File 17)
cat("  Preparazione dataset settori NACE...")
dati_settori <- clean_racli_sector_data(racli_17_raw)
cat(" OK (", nrow(dati_settori), "righe)\n")

cat("\n")

# 6. Validazione -----

cat("==== Validazione Dati ====\n\n")

validate_dataset <- function(data, name) {
  cat("Dataset:", name, "\n")
  cat("  Righe:", nrow(data), "\n")
  cat("  Anni:", paste(sort(unique(data$anno)), collapse = ", "), "\n")
  cat(
    "  Livelli geografici:",
    paste(unique(data$geo_level), collapse = ", "),
    "\n"
  )

  if ("salario_mediano" %in% names(data)) {
    cat(
      "  Salario mediano range: €",
      sprintf("%.2f", min(data$salario_mediano, na.rm = TRUE)),
      " - €",
      sprintf("%.2f", max(data$salario_mediano, na.rm = TRUE)),
      "\n"
    )
  }

  # Check decili ordinati
  if (all(c("D1", "D5", "D9") %in% names(data))) {
    violazioni <- data %>%
      filter(!is.na(D1) & !is.na(D5) & !is.na(D9)) %>%
      mutate(ok = (D1 <= D5) & (D5 <= D9)) %>%
      summarise(pct_ok = mean(ok) * 100) %>%
      pull(pct_ok)

    cat("  Ordinamento decili corretto:", sprintf("%.1f%%", violazioni), "\n")
  }

  # Check codici ISTAT per province
  if ("cod_uts" %in% names(data)) {
    prov_data <- data[data$geo_level == "Provincia", ]
    if (nrow(prov_data) > 0) {
      n_with_code <- sum(!is.na(prov_data$cod_uts))
      n_total <- nrow(prov_data)
      cat(
        "  Codici ISTAT province:",
        n_with_code,
        "/",
        n_total,
        "(",
        sprintf("%.1f%%", n_with_code / n_total * 100),
        ")\n"
      )
    }
  }

  cat("\n")
}

validate_dataset(dati_settore_sesso, "Sesso")
validate_dataset(dati_educazione, "Livello Educativo")
validate_dataset(dati_contratto, "Tipo Contratto")

# Validazione specifica per dati settoriali
cat("Dataset: Settori NACE\n")
cat("  Righe:", nrow(dati_settori), "\n")
cat("  Anni:", paste(sort(unique(dati_settori$anno)), collapse = ", "), "\n")
cat(
  "  Settori totali:",
  length(unique(dati_settori$settore_code)),
  "\n"
)
cat(
  "  Sezioni NACE principali:",
  sum(
    dati_settori$tipo_settore == "Sezione NACE" &
      dati_settori$sesso == "totale" &
      dati_settori$anno == 2022
  ),
  "\n"
)
if ("salario_mediano" %in% names(dati_settori)) {
  cat(
    "  Salario mediano range: €",
    sprintf("%.2f", min(dati_settori$salario_mediano, na.rm = TRUE)),
    " - €",
    sprintf("%.2f", max(dati_settori$salario_mediano, na.rm = TRUE)),
    "\n"
  )
}
cat("\n")

# 7. Salvataggio -----

cat("==== Salvataggio Dataset ====\n\n")

if (!dir.exists("output")) {
  dir.create("output", recursive = TRUE)
}

saveRDS(dati_settore_sesso, "output/dati_settore_sesso.rds")
cat("Salvato: output/dati_settore_sesso.rds\n")

saveRDS(dati_educazione, "output/dati_educazione.rds")
cat("Salvato: output/dati_educazione.rds\n")

saveRDS(dati_contratto, "output/dati_contratto.rds")
cat("Salvato: output/dati_contratto.rds\n")

saveRDS(dati_settori, "output/dati_settori.rds")
cat("Salvato: output/dati_settori.rds\n")

cat("\n==== Script completato con successo ====\n")
