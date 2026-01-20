# racli_prep.R -----
# Pipeline completo analisi RACLI: preparazione, analisi descrittive,
# disuguaglianza, clustering, regressioni, visualizzazioni, cartografie, validazione
# Autore: Giampaolo Montaletti (ORCID: 0009-0002-5327-1122)
# Data: 2026-01-20

# 1. Setup e librerie -----

cat("==== Pipeline RACLI - Setup ====\n\n")

# Installa pacchetti se mancanti
install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(
      pkg,
      repos = "https://cloud.r-project.org",
      dependencies = TRUE
    )
    library(pkg, character.only = TRUE)
  }
}

install_if_missing("data.table")
install_if_missing("dplyr")
install_if_missing("tidyr")
install_if_missing("ggplot2")
install_if_missing("knitr")
install_if_missing("kableExtra")
install_if_missing("ineq")
install_if_missing("cluster")
install_if_missing("factoextra")
install_if_missing("lmtest")
install_if_missing("sandwich")
install_if_missing("patchwork")
install_if_missing("scales")
install_if_missing("sf")
install_if_missing("ggspatial")
install_if_missing("viridis")

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(knitr)
  library(kableExtra)
  library(ineq)
  library(cluster)
  library(factoextra)
  library(lmtest)
  library(sandwich)
  library(patchwork)
  library(scales)
  library(sf)
  library(ggspatial)
  library(viridis)
})

# Crea directory output
if (!dir.exists("output/racli")) {
  dir.create("output/racli", recursive = TRUE)
}
if (!dir.exists("output/racli/grafici")) {
  dir.create("output/racli/grafici", recursive = TRUE)
}

cat("Directory output create: output/racli/, output/racli/grafici/\n\n")

# 2. Funzioni utility (da 01_prepara_dati_racli.R) -----

cat("==== Caricamento Funzioni Utility ====\n\n")

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

  ripart_lookup <- cl_itter[
    nchar(id_description) == 3 &
      id_description %in% c("ITC", "ITD", "ITE", "ITF", "ITG"),
    .(code = id_description, label = it_description)
  ]

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

  ripart_code <- case_when(
    code == "IT" ~ "IT",
    nchar(code) >= 3 ~ substr(code, 1, 3),
    TRUE ~ code
  )

  ripart_code <- case_when(
    ripart_code == "ITH" ~ "ITD",
    ripart_code == "ITI" ~ "ITE",
    TRUE ~ ripart_code
  )

  ripart_code <- case_when(
    code == "IT108" ~ "ITC",
    code == "IT109" ~ "ITE",
    code == "IT110" ~ "ITF",
    code == "IT111" ~ "ITG",
    TRUE ~ ripart_code
  )

  labels <- ripart_lookup$label[match(ripart_code, ripart_lookup$code)]
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

  data_with_codes <- data %>%
    left_join(
      mapping[, c("itter107", "cod_uts", "cod_reg", "cod_rip")],
      by = c("area" = "itter107")
    )

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

  data_clean <- data %>%
    mutate(
      anno = tempo_temp,
      area = as.character(REF_AREA),
      area_label = as.character(REF_AREA_label),
      tipo_dato_clean = case_when(
        grepl("_AV_", DATA_TYPE) ~ "salario_medio",
        grepl("_FIRD_", DATA_TYPE) ~ "D1",
        grepl("_MED_", DATA_TYPE) ~ "salario_mediano",
        grepl("_NIND_", DATA_TYPE) ~ "D9",
        TRUE ~ as.character(DATA_TYPE)
      )
    )

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

  data_wide <- data_clean %>%
    select(all_of(c(id_cols, "tipo_dato_clean", "valore"))) %>%
    pivot_wider(
      names_from = tipo_dato_clean,
      values_from = valore,
      values_fn = mean
    )

  if ("salario_mediano" %in% names(data_wide) && !"D5" %in% names(data_wide)) {
    data_wide <- data_wide %>%
      mutate(D5 = salario_mediano)
  }

  data_wide <- data_wide %>%
    mutate(
      geo_level = get_geo_level(area),
      ripartizione = get_ripartizione(area)
    )

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

  data_wide <- data_clean %>%
    select(anno, settore_code, settore, sesso, tipo_dato_clean, valore) %>%
    pivot_wider(
      names_from = tipo_dato_clean,
      values_from = valore,
      values_fn = mean
    )

  if ("salario_mediano" %in% names(data_wide) && !"D5" %in% names(data_wide)) {
    data_wide <- data_wide %>%
      mutate(D5 = salario_mediano)
  }

  if (all(c("D1", "D5", "D9") %in% names(data_wide))) {
    data_wide <- data_wide %>%
      mutate(
        D9_D1 = D9 / D1,
        D9_D5 = D9 / D5,
        D5_D1 = D5 / D1
      )
  }

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

# Pulisce e standardizza dataset RACLI settore x territorio (File 6)
clean_racli_sector_territory_data <- function(data) {
  if (is.null(data)) {
    return(NULL)
  }

  ripart_lookup <- load_ripartizione_lookup()

  data_clean <- data %>%
    mutate(
      anno = tempo_temp,
      area = as.character(REF_AREA),
      area_label = as.character(REF_AREA_label),
      settore_code = as.character(ECON_ACTIVITY_NACE_2007),
      settore = as.character(ECON_ACTIVITY_NACE_2007_label),
      sesso = as.character(SEX_label),
      dim_aziendale = as.character(EMPLOYESS_CLASS_label),
      contratto_occ = as.character(CONTARCTUAL_OCCUPATION_label),
      salario_mediano = valore
    )

  data_clean <- data_clean %>%
    mutate(
      geo_level = get_geo_level(area),
      ripartizione = get_ripartizione(area, ripart_lookup)
    )

  data_wide <- data_clean %>%
    select(
      anno,
      area,
      area_label,
      geo_level,
      ripartizione,
      settore_code,
      settore,
      sesso,
      dim_aziendale,
      contratto_occ,
      salario_mediano
    )

  data_wide <- data_wide %>%
    mutate(
      tipo_settore = case_when(
        settore_code %in% c("0010", "0011", "0020", "0038") ~ "Aggregato",
        nchar(settore_code) == 1 &
          grepl("^[A-Z]$", settore_code) ~ "Sezione NACE",
        TRUE ~ "Dettaglio"
      )
    )

  return(data_wide)
}

# 3. Funzioni analisi (da 02-08) -----

# Gini approssimato da decili
calc_gini_from_deciles <- function(D1, D5, D9) {
  approx_dist <- c(
    rep(D1, 10),
    rep(mean(c(D1, D5)), 40),
    rep(mean(c(D5, D9)), 40),
    rep(D9, 10)
  )
  return(ineq::Gini(approx_dist))
}

# Funzione validazione dataset
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
      "  Salario mediano range: EUR",
      sprintf("%.2f", min(data$salario_mediano, na.rm = TRUE)),
      " - EUR",
      sprintf("%.2f", max(data$salario_mediano, na.rm = TRUE)),
      "\n"
    )
  }

  if (all(c("D1", "D5", "D9") %in% names(data))) {
    violazioni <- data %>%
      filter(!is.na(D1) & !is.na(D5) & !is.na(D9)) %>%
      mutate(ok = (D1 <= D5) & (D5 <= D9)) %>%
      summarise(pct_ok = mean(ok) * 100) %>%
      pull(pct_ok)

    cat("  Ordinamento decili corretto:", sprintf("%.1f%%", violazioni), "\n")
  }

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

# Formatta crescita percentuale
format_crescita <- function(x) {
  sprintf("%+.1f%%", x)
}

# Theme per grafici
theme_salari <- function() {
  theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(
        size = 11,
        hjust = 0.5,
        margin = margin(b = 10)
      ),
      plot.caption = element_text(size = 8, hjust = 1, margin = margin(t = 10)),
      axis.title = element_text(size = 11),
      axis.text = element_text(size = 10),
      legend.position = "right",
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 9),
      panel.grid.minor = element_blank()
    )
}

cat("Funzioni utility caricate\n\n")

# 4. Preparazione dati (01_prepara_dati_racli.R) -----

cat("==== 4. Preparazione Dati RACLI ====\n\n")

cat("Caricamento file RACLI...\n")

cat("  File 8: dati per sesso...")
racli_8_raw <- readRDS("racli/F_533_957_DF_DCSC_RACLI_8.rds")
cat(" OK (", nrow(racli_8_raw), "righe)\n")

cat("  File 11: livello educativo...")
racli_11_raw <- readRDS("racli/F_533_957_DF_DCSC_RACLI_11.rds")
cat(" OK (", nrow(racli_11_raw), "righe)\n")

cat("  File 12: tipo contratto...")
racli_12_raw <- readRDS("racli/F_533_957_DF_DCSC_RACLI_12.rds")
cat(" OK (", nrow(racli_12_raw), "righe)\n")

cat("  File 17: settori economici...")
racli_17_raw <- readRDS("racli/F_533_957_DF_DCSC_RACLI_17.rds")
cat(" OK (", nrow(racli_17_raw), "righe)\n")

cat("  File 6: settore x territorio...")
racli_6_raw <- readRDS("racli/F_533_957_DF_DCSC_RACLI_6.rds")
cat(" OK (", nrow(racli_6_raw), "righe)\n")

cat("\nCaricamento mapping ITTER107 -> COD_UTS...\n")
itter_mapping <- load_itter_mapping()
if (!is.null(itter_mapping)) {
  cat("  Mapping caricato:", nrow(itter_mapping), "codici\n\n")
} else {
  cat(
    "  ATTENZIONE: mapping non disponibile, codici ISTAT non saranno aggiunti\n\n"
  )
}

cat("Pulizia e trasformazione dati...\n")

cat("  Preparazione dataset per sesso...")
dati_settore_sesso <- clean_racli_data(racli_8_raw)
dati_settore_sesso <- add_istat_codes(dati_settore_sesso, itter_mapping)
dati_settore_sesso <- dati_settore_sesso %>%
  mutate(settore = "Totale economia")
cat(" OK (", nrow(dati_settore_sesso), "righe)\n")

cat("  Preparazione dataset educazione...")
dati_educazione <- clean_racli_data(racli_11_raw)
dati_educazione <- add_istat_codes(dati_educazione, itter_mapping)
dati_educazione <- dati_educazione %>%
  mutate(settore = "Totale economia")
cat(" OK (", nrow(dati_educazione), "righe)\n")

cat("  Preparazione dataset tipo contratto...")
dati_contratto <- clean_racli_data(racli_12_raw)
dati_contratto <- add_istat_codes(dati_contratto, itter_mapping)
dati_contratto <- dati_contratto %>%
  mutate(settore = "Totale economia")
cat(" OK (", nrow(dati_contratto), "righe)\n")

cat("  Preparazione dataset settori NACE...")
dati_settori <- clean_racli_sector_data(racli_17_raw)
cat(" OK (", nrow(dati_settori), "righe)\n")

cat("  Preparazione dataset settore x territorio...")
dati_settori_territorio <- clean_racli_sector_territory_data(racli_6_raw)
cat(" OK (", nrow(dati_settori_territorio), "righe)\n")

cat("\nValidazione dati preparati...\n")
validate_dataset(dati_settore_sesso, "Sesso")
validate_dataset(dati_educazione, "Livello Educativo")
validate_dataset(dati_contratto, "Tipo Contratto")

cat("Dataset: Settori NACE\n")
cat("  Righe:", nrow(dati_settori), "\n")
cat("  Anni:", paste(sort(unique(dati_settori$anno)), collapse = ", "), "\n")
cat("  Settori totali:", length(unique(dati_settori$settore_code)), "\n\n")

cat("Dataset: Settori x Territorio\n")
cat("  Righe:", nrow(dati_settori_territorio), "\n")
cat(
  "  Anni:",
  paste(sort(unique(dati_settori_territorio$anno)), collapse = ", "),
  "\n"
)
cat("  Aree geografiche:", length(unique(dati_settori_territorio$area)), "\n\n")

# Salvataggio dati preparati
saveRDS(dati_settore_sesso, "output/racli/dati_settore_sesso.rds")
saveRDS(dati_educazione, "output/racli/dati_educazione.rds")
saveRDS(dati_contratto, "output/racli/dati_contratto.rds")
saveRDS(dati_settori, "output/racli/dati_settori.rds")
saveRDS(dati_settori_territorio, "output/racli/dati_settori_territorio.rds")

cat("Salvati dataset preparati in output/racli/\n\n")

# 5. Analisi descrittive (02_analisi_descrittive_racli.R) -----

cat("==== 5. Analisi Descrittive RACLI ====\n\n")

# Ranking settori per salario mediano 2022
cat("Ranking settori per salario mediano (2022)...\n")

ranking_settori_2022 <- dati_settore_sesso %>%
  filter(anno == 2022, geo_level == "Nazionale") %>%
  group_by(settore) %>%
  summarise(
    salario_mediano = mean(salario_mediano, na.rm = TRUE),
    salario_medio = mean(salario_medio, na.rm = TRUE),
    D1 = mean(D1, na.rm = TRUE),
    D9 = mean(D9, na.rm = TRUE),
    rapporto_D9_D1 = D9 / D1,
    .groups = "drop"
  ) %>%
  arrange(desc(salario_mediano)) %>%
  mutate(
    rank = row_number(),
    salario_mediano_fmt = sprintf("EUR%.2f", salario_mediano),
    D9_D1_fmt = sprintf("%.2f", rapporto_D9_D1)
  )

saveRDS(ranking_settori_2022, "output/racli/ranking_settori_2022.rds")

# Gap di genere
cat("Calcolo gap di genere...\n")

gap_gender <- dati_settore_sesso %>%
  filter(geo_level == "Nazionale", sesso %in% c("maschi", "femmine")) %>%
  select(anno, settore, sesso, salario_mediano) %>%
  pivot_wider(
    names_from = sesso,
    values_from = salario_mediano,
    names_prefix = "salario_"
  ) %>%
  mutate(
    gap_assoluto = salario_maschi - salario_femmine,
    gap_percentuale = ((salario_maschi - salario_femmine) / salario_femmine) *
      100,
    rapporto_M_F = salario_maschi / salario_femmine
  ) %>%
  filter(!is.na(gap_percentuale))

gap_gender_2022 <- gap_gender %>%
  filter(anno == 2022) %>%
  arrange(desc(gap_percentuale)) %>%
  mutate(
    gap_pct_fmt = sprintf("%.1f%%", gap_percentuale),
    rapporto_fmt = sprintf("%.2f", rapporto_M_F)
  )

gap_gender_tempo <- gap_gender %>%
  group_by(anno) %>%
  summarise(
    gap_medio = mean(gap_percentuale, na.rm = TRUE),
    gap_mediano = median(gap_percentuale, na.rm = TRUE),
    rapporto_M_F_medio = mean(rapporto_M_F, na.rm = TRUE),
    .groups = "drop"
  )

saveRDS(gap_gender, "output/racli/gap_gender.rds")
saveRDS(gap_gender_2022, "output/racli/gap_gender_2022.rds")
saveRDS(gap_gender_tempo, "output/racli/gap_gender_tempo.rds")

# Gap educativo
cat("Calcolo gap educativo...\n")

if ("educazione" %in% names(dati_educazione)) {
  gap_educazione_2022 <- dati_educazione %>%
    filter(anno == 2022, geo_level == "Nazionale") %>%
    group_by(educazione) %>%
    summarise(
      salario_mediano = mean(salario_mediano, na.rm = TRUE),
      salario_medio = mean(salario_medio, na.rm = TRUE),
      D9_D1 = mean(D9 / D1, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(salario_mediano)) %>%
    mutate(
      salario_fmt = sprintf("EUR%.2f", salario_mediano),
      D9_D1_fmt = sprintf("%.2f", D9_D1)
    )

  saveRDS(gap_educazione_2022, "output/racli/gap_educazione_2022.rds")
}

# Gap per tipo contratto
cat("Calcolo gap per tipo contratto...\n")

if ("contratto" %in% names(dati_contratto)) {
  gap_contratto_2022 <- dati_contratto %>%
    filter(anno == 2022, geo_level == "Nazionale") %>%
    group_by(contratto) %>%
    summarise(
      salario_mediano = mean(salario_mediano, na.rm = TRUE),
      salario_medio = mean(salario_medio, na.rm = TRUE),
      D9_D1 = mean(D9 / D1, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(salario_mediano)) %>%
    mutate(
      salario_fmt = sprintf("EUR%.2f", salario_mediano),
      D9_D1_fmt = sprintf("%.2f", D9_D1)
    )

  saveRDS(gap_contratto_2022, "output/racli/gap_contratto_2022.rds")
}

# Statistiche per ripartizione geografica
cat("Statistiche per ripartizione geografica...\n")

stat_ripartizioni_2022 <- dati_settore_sesso %>%
  filter(anno == 2022, geo_level == "Ripartizione") %>%
  group_by(ripartizione) %>%
  summarise(
    salario_mediano = mean(salario_mediano, na.rm = TRUE),
    salario_medio = mean(salario_medio, na.rm = TRUE),
    D1 = mean(D1, na.rm = TRUE),
    D9 = mean(D9, na.rm = TRUE),
    D9_D1 = D9 / D1,
    cv = sd(salario_mediano, na.rm = TRUE) / salario_mediano,
    n_obs = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(salario_mediano))

saveRDS(stat_ripartizioni_2022, "output/racli/stat_ripartizioni_2022.rds")

# Evoluzione top 5 settori
cat("Evoluzione temporale top 5 settori...\n")

top5_settori <- ranking_settori_2022 %>%
  head(5) %>%
  pull(settore)

evoluzione_top5 <- dati_settore_sesso %>%
  filter(geo_level == "Nazionale", settore %in% top5_settori) %>%
  group_by(anno, settore) %>%
  summarise(
    salario_mediano = mean(salario_mediano, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(settore, anno)

saveRDS(evoluzione_top5, "output/racli/evoluzione_top5_settori.rds")

# Crescita salariale 2014-2022
cat("Crescita salariale 2014-2022...\n")

crescita_settori <- dati_settore_sesso %>%
  filter(geo_level == "Nazionale", anno %in% c(2014, 2022)) %>%
  group_by(settore, anno) %>%
  summarise(
    salario_mediano = mean(salario_mediano, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = anno,
    values_from = salario_mediano,
    names_prefix = "sal_"
  ) %>%
  mutate(
    crescita_assoluta = sal_2022 - sal_2014,
    crescita_percentuale = ((sal_2022 - sal_2014) / sal_2014) * 100,
    crescita_annua = ((sal_2022 / sal_2014)^(1 / 8) - 1) * 100
  ) %>%
  arrange(desc(crescita_percentuale))

saveRDS(crescita_settori, "output/racli/crescita_settori_2014_2022.rds")

# Sintesi descrittive
sintesi <- list(
  n_settori = length(unique(dati_settore_sesso$settore)),
  n_anni = length(unique(dati_settore_sesso$anno)),
  n_ripartizioni = sum(stat_ripartizioni_2022$ripartizione != "Italia"),
  salario_mediano_nazionale_2022 = mean(
    dati_settore_sesso %>%
      filter(anno == 2022, geo_level == "Nazionale") %>%
      pull(salario_mediano),
    na.rm = TRUE
  ),
  gap_gender_medio_2022 = mean(gap_gender_2022$gap_percentuale, na.rm = TRUE),
  settore_max_salario = ranking_settori_2022$settore[1],
  settore_min_salario = ranking_settori_2022$settore[nrow(
    ranking_settori_2022
  )],
  crescita_media_2014_2022 = mean(
    crescita_settori$crescita_percentuale,
    na.rm = TRUE
  )
)

saveRDS(sintesi, "output/racli/sintesi_descrittive.rds")

# Ranking settori NACE
cat("Ranking settori NACE per salario (2022)...\n")

ranking_settori_nace_2022 <- dati_settori %>%
  filter(anno == 2022, sesso == "totale", tipo_settore == "Sezione NACE") %>%
  arrange(desc(salario_mediano)) %>%
  mutate(
    rank = row_number(),
    salario_mediano_fmt = sprintf("EUR%.2f", salario_mediano),
    salario_medio_fmt = sprintf("EUR%.2f", salario_medio),
    D9_D1_fmt = sprintf("%.2f", D9_D1)
  )

saveRDS(ranking_settori_nace_2022, "output/racli/ranking_settori_nace_2022.rds")

# Dispersione salariale per settore
cat("Dispersione salariale per settore (2022)...\n")

dispersione_settori_2022 <- dati_settori %>%
  filter(anno == 2022, sesso == "totale", tipo_settore == "Sezione NACE") %>%
  select(
    settore_code,
    settore,
    salario_mediano,
    salario_medio,
    D1,
    D5,
    D9,
    D9_D1,
    D9_D5,
    D5_D1
  ) %>%
  arrange(desc(D9_D1)) %>%
  mutate(rank_dispersione = row_number())

saveRDS(dispersione_settori_2022, "output/racli/dispersione_settori_2022.rds")

# Gap di genere per settore NACE
cat("Gap di genere per settore NACE...\n")

gap_gender_settori <- dati_settori %>%
  filter(tipo_settore == "Sezione NACE", sesso %in% c("maschi", "femmine")) %>%
  select(anno, settore_code, settore, sesso, salario_mediano) %>%
  pivot_wider(
    names_from = sesso,
    values_from = salario_mediano,
    names_prefix = "salario_"
  ) %>%
  mutate(
    gap_assoluto = salario_maschi - salario_femmine,
    gap_percentuale = ((salario_maschi - salario_femmine) / salario_femmine) *
      100,
    rapporto_M_F = salario_maschi / salario_femmine
  ) %>%
  filter(!is.na(gap_percentuale))

gap_gender_settori_2022 <- gap_gender_settori %>%
  filter(anno == 2022) %>%
  arrange(desc(gap_percentuale)) %>%
  mutate(rank_gap = row_number())

saveRDS(gap_gender_settori, "output/racli/gap_gender_settori.rds")
saveRDS(gap_gender_settori_2022, "output/racli/gap_gender_settori_2022.rds")

# Interazione Settore x Territorio
cat("Interazione settore x territorio (2022)...\n")

dati_st_2022 <- dati_settori_territorio %>%
  filter(
    anno == 2022,
    sesso == "totale",
    geo_level == "Ripartizione",
    tipo_settore == "Sezione NACE"
  ) %>%
  select(area, area_label, ripartizione, settore_code, settore, salario_mediano)

gap_territoriale_settori <- dati_st_2022 %>%
  mutate(
    macro_area = case_when(
      area %in% c("ITC", "ITD") ~ "Nord",
      area %in% c("ITF", "ITG") ~ "Sud",
      area == "ITE" ~ "Centro",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(macro_area)) %>%
  group_by(settore_code, settore, macro_area) %>%
  summarise(
    salario_mediano = mean(salario_mediano, na.rm = TRUE),
    n_ripart = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = macro_area,
    values_from = c(salario_mediano, n_ripart),
    names_sep = "_"
  ) %>%
  mutate(
    gap_nord_sud_assoluto = salario_mediano_Nord - salario_mediano_Sud,
    gap_nord_sud_pct = ((salario_mediano_Nord - salario_mediano_Sud) /
      salario_mediano_Sud) *
      100,
    rapporto_nord_sud = salario_mediano_Nord / salario_mediano_Sud
  ) %>%
  arrange(desc(gap_nord_sud_pct))

cv_territoriale_settori <- dati_st_2022 %>%
  group_by(settore_code, settore) %>%
  summarise(
    n_ripartizioni = n(),
    salario_medio_ripart = mean(salario_mediano, na.rm = TRUE),
    sd_territoriale = sd(salario_mediano, na.rm = TRUE),
    cv_territoriale = sd_territoriale / salario_medio_ripart,
    salario_min = min(salario_mediano, na.rm = TRUE),
    salario_max = max(salario_mediano, na.rm = TRUE),
    range_salariale = salario_max - salario_min,
    .groups = "drop"
  ) %>%
  arrange(desc(cv_territoriale))

matrice_settore_ripart <- dati_st_2022 %>%
  select(settore, ripartizione, salario_mediano) %>%
  pivot_wider(names_from = ripartizione, values_from = salario_mediano)

saveRDS(
  gap_territoriale_settori,
  "output/racli/gap_territoriale_settori_2022.rds"
)
saveRDS(
  cv_territoriale_settori,
  "output/racli/cv_territoriale_settori_2022.rds"
)
saveRDS(matrice_settore_ripart, "output/racli/matrice_settore_ripart_2022.rds")
saveRDS(dati_st_2022, "output/racli/dati_settori_territorio_2022.rds")

sintesi_st <- list(
  n_settori = length(unique(dati_st_2022$settore_code)),
  n_ripartizioni = length(unique(dati_st_2022$area)),
  gap_nord_sud_medio = mean(
    gap_territoriale_settori$gap_nord_sud_pct,
    na.rm = TRUE
  ),
  cv_territoriale_medio = mean(
    cv_territoriale_settori$cv_territoriale,
    na.rm = TRUE
  )
)

saveRDS(sintesi_st, "output/racli/sintesi_settori_territorio.rds")

cat("Analisi descrittive completate\n\n")

# 6. Analisi distributiva (03_analisi_distributiva.R) -----

cat("==== 6. Analisi Distributiva ====\n\n")

# Calcolo indici di disuguaglianza
cat("Calcolo indici di disuguaglianza...\n")

indici_disug <- dati_settore_sesso %>%
  filter(sesso == "totale") %>%
  mutate(
    gini_approx = mapply(calc_gini_from_deciles, D1, D5, D9),
    D9_D1 = D9 / D1,
    D9_D5 = D9 / D5,
    D5_D1 = D5 / D1
  ) %>%
  select(
    anno,
    area,
    area_label,
    geo_level,
    ripartizione,
    salario_mediano,
    D1,
    D5,
    D9,
    gini_approx,
    D9_D1,
    D9_D5,
    D5_D1
  )

indici_nazionale_2022 <- indici_disug %>%
  filter(anno == 2022, geo_level == "Nazionale") %>%
  summarise(
    salario_mediano = mean(salario_mediano, na.rm = TRUE),
    gini = mean(gini_approx, na.rm = TRUE),
    D9_D1 = mean(D9_D1, na.rm = TRUE),
    D9_D5 = mean(D9_D5, na.rm = TRUE),
    D5_D1 = mean(D5_D1, na.rm = TRUE)
  )

cat("  Gini nazionale 2022:", sprintf("%.3f", indici_nazionale_2022$gini), "\n")
cat(
  "  D9/D1 nazionale 2022:",
  sprintf("%.2f", indici_nazionale_2022$D9_D1),
  "\n\n"
)

evoluzione_disug <- indici_disug %>%
  filter(geo_level == "Nazionale") %>%
  group_by(anno) %>%
  summarise(
    gini = mean(gini_approx, na.rm = TRUE),
    D9_D1 = mean(D9_D1, na.rm = TRUE),
    D9_D5 = mean(D9_D5, na.rm = TRUE),
    .groups = "drop"
  )

saveRDS(indici_disug, "output/racli/indici_disuguaglianza.rds")
saveRDS(evoluzione_disug, "output/racli/evoluzione_disuguaglianza.rds")

# Disuguaglianza per ripartizione
cat("Disuguaglianza per ripartizione...\n")

disug_ripartizioni_2022 <- indici_disug %>%
  filter(anno == 2022, geo_level == "Ripartizione") %>%
  group_by(ripartizione) %>%
  summarise(
    salario_mediano = mean(salario_mediano, na.rm = TRUE),
    gini = mean(gini_approx, na.rm = TRUE),
    D9_D1 = mean(D9_D1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(gini))

saveRDS(disug_ripartizioni_2022, "output/racli/disug_ripartizioni_2022.rds")

# Mobilita temporale
cat("Mobilita temporale (quartili province)...\n")

province_mobilita <- indici_disug %>%
  filter(geo_level == "Provincia", anno %in% c(2014, 2022)) %>%
  select(anno, area, area_label, salario_mediano) %>%
  pivot_wider(
    names_from = anno,
    values_from = salario_mediano,
    names_prefix = "sal_"
  ) %>%
  filter(!is.na(sal_2014) & !is.na(sal_2022))

if (nrow(province_mobilita) > 0) {
  province_mobilita <- province_mobilita %>%
    mutate(
      quartile_2014 = ntile(sal_2014, 4),
      quartile_2022 = ntile(sal_2022, 4),
      cambio_quartile = quartile_2022 - quartile_2014,
      mobilita = case_when(
        cambio_quartile > 0 ~ "Ascendente",
        cambio_quartile < 0 ~ "Discendente",
        TRUE ~ "Stabile"
      )
    )

  matrice_transizione <- province_mobilita %>%
    count(quartile_2014, quartile_2022) %>%
    pivot_wider(
      names_from = quartile_2022,
      values_from = n,
      values_fill = 0,
      names_prefix = "Q2022_"
    )

  saveRDS(province_mobilita, "output/racli/mobilita_province.rds")
  saveRDS(matrice_transizione, "output/racli/matrice_transizione.rds")
}

# Sigma convergenza
cat("Test convergenza territoriale...\n")

sigma_convergence <- indici_disug %>%
  filter(geo_level %in% c("Provincia", "Regione")) %>%
  group_by(anno) %>%
  summarise(
    cv = sd(salario_mediano, na.rm = TRUE) /
      mean(salario_mediano, na.rm = TRUE),
    sd_log = sd(log(salario_mediano), na.rm = TRUE),
    .groups = "drop"
  )

saveRDS(sigma_convergence, "output/racli/sigma_convergence.rds")

# Beta convergenza
if (nrow(province_mobilita) > 0) {
  beta_convergence <- province_mobilita %>%
    mutate(
      log_sal_2014 = log(sal_2014),
      crescita_2014_2022 = log(sal_2022 / sal_2014),
      tasso_crescita_annuo = (crescita_2014_2022 / 8) * 100
    ) %>%
    filter(!is.na(log_sal_2014) & !is.na(tasso_crescita_annuo))

  if (nrow(beta_convergence) > 10) {
    model_beta <- lm(
      tasso_crescita_annuo ~ log_sal_2014,
      data = beta_convergence
    )
    saveRDS(beta_convergence, "output/racli/beta_convergence.rds")
    saveRDS(model_beta, "output/racli/model_beta_convergence.rds")
  }
}

# Indici disuguaglianza settoriali
cat("Indici disuguaglianza settoriali...\n")

indici_disug_settori <- dati_settori %>%
  filter(sesso == "totale", tipo_settore == "Sezione NACE") %>%
  select(
    anno,
    settore_code,
    settore,
    salario_mediano,
    salario_medio,
    D1,
    D5,
    D9,
    D9_D1,
    D9_D5,
    D5_D1
  )

evoluzione_disug_settori <- indici_disug_settori %>%
  group_by(anno) %>%
  summarise(
    n_settori = n(),
    D9_D1_medio = mean(D9_D1, na.rm = TRUE),
    D9_D1_sd = sd(D9_D1, na.rm = TRUE),
    D9_D5_medio = mean(D9_D5, na.rm = TRUE),
    D5_D1_medio = mean(D5_D1, na.rm = TRUE),
    .groups = "drop"
  )

saveRDS(indici_disug_settori, "output/racli/indici_disuguaglianza_settori.rds")
saveRDS(
  evoluzione_disug_settori,
  "output/racli/evoluzione_disuguaglianza_settori.rds"
)

# Sintesi distributiva
sintesi_distributiva <- list(
  gini_2022 = indici_nazionale_2022$gini,
  D9_D1_2022 = indici_nazionale_2022$D9_D1,
  gini_2014 = evoluzione_disug %>% filter(anno == 2014) %>% pull(gini),
  variazione_gini = indici_nazionale_2022$gini -
    (evoluzione_disug %>% filter(anno == 2014) %>% pull(gini)),
  n_province_mobilita = nrow(province_mobilita)
)

saveRDS(sintesi_distributiva, "output/racli/sintesi_distributiva.rds")

cat("Analisi distributiva completata\n\n")

# 7. Clustering (04_analisi_clustering.R) -----

cat("==== 7. Clustering Province ====\n\n")

# Preparazione dati per clustering
cat("Preparazione dati clustering...\n")

dati_clustering <- indici_disug %>%
  filter(anno == 2022, geo_level == "Provincia") %>%
  select(area, area_label, salario_mediano, gini_approx, D9_D1)

if (file.exists("output/racli/mobilita_province.rds")) {
  mobilita <- readRDS("output/racli/mobilita_province.rds")
  dati_clustering <- dati_clustering %>%
    left_join(
      mobilita %>% select(area, sal_2014, sal_2022),
      by = "area"
    ) %>%
    mutate(crescita_2014_2022 = ((sal_2022 - sal_2014) / sal_2014) * 100)
}

dati_clustering <- dati_clustering %>%
  filter(!is.na(salario_mediano) & !is.na(gini_approx))

cat("  Province per clustering:", nrow(dati_clustering), "\n")

# Matrice per clustering
vars_cluster <- c("salario_mediano", "gini_approx", "D9_D1")
if ("crescita_2014_2022" %in% names(dati_clustering)) {
  vars_cluster <- c(vars_cluster, "crescita_2014_2022")
}

X_raw <- dati_clustering %>% select(all_of(vars_cluster))
col_sd <- apply(X_raw, 2, sd, na.rm = TRUE)
vars_valid <- names(col_sd)[col_sd > 0]
vars_cluster <- vars_valid

X <- X_raw %>% select(all_of(vars_cluster)) %>% scale()
rows_complete <- complete.cases(X)
if (sum(!rows_complete) > 0) {
  X <- X[rows_complete, ]
  dati_clustering <- dati_clustering[rows_complete, ]
}
rownames(X) <- dati_clustering$area_label

# Determinazione numero ottimale cluster
cat("Determinazione numero ottimale cluster...\n")

set.seed(123)
wss <- sapply(1:10, function(k) {
  kmeans(X, centers = k, nstart = 25)$tot.withinss
})

sil_width <- sapply(2:10, function(k) {
  km <- kmeans(X, centers = k, nstart = 25)
  ss <- silhouette(km$cluster, dist(X))
  mean(ss[, 3])
})

sil_df <- data.frame(k = 2:10, silhouette = sil_width)
k_opt <- sil_df$k[which.max(sil_df$silhouette)]

cat("  Numero ottimale cluster (massimo silhouette): k =", k_opt, "\n\n")

# K-means clustering
cat("K-means clustering (k =", k_opt, ")...\n")

set.seed(123)
km_result <- kmeans(X, centers = k_opt, nstart = 25)
dati_clustering$cluster_km <- km_result$cluster

cluster_stats <- dati_clustering %>%
  group_by(cluster_km) %>%
  summarise(
    n_province = n(),
    sal_medio = mean(salario_mediano, na.rm = TRUE),
    sal_sd = sd(salario_mediano, na.rm = TRUE),
    gini_medio = mean(gini_approx, na.rm = TRUE),
    D9_D1_medio = mean(D9_D1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(sal_medio))

saveRDS(km_result, "output/racli/kmeans_result.rds")

# Hierarchical clustering
cat("Hierarchical clustering...\n")

dist_matrix <- dist(X, method = "euclidean")
hc_result <- hclust(dist_matrix, method = "ward.D2")
cluster_hc <- cutree(hc_result, k = k_opt)
dati_clustering$cluster_hc <- cluster_hc

saveRDS(hc_result, "output/racli/hclust_result.rds")

# Caratterizzazione cluster
cluster_labels <- cluster_stats %>%
  mutate(
    label = case_when(
      row_number() == 1 ~ "Alto reddito",
      row_number() == k_opt ~ "Basso reddito",
      sal_medio > median(cluster_stats$sal_medio) ~ "Medio-alto",
      TRUE ~ "Medio-basso"
    )
  ) %>%
  select(cluster_km, label)

dati_clustering <- dati_clustering %>%
  left_join(cluster_labels, by = "cluster_km")

saveRDS(dati_clustering, "output/racli/dati_clustering.rds")

sintesi_clustering <- list(
  n_province = nrow(dati_clustering),
  k_ottimale = k_opt,
  silhouette_medio = max(sil_width),
  variabili_usate = vars_cluster
)

saveRDS(sintesi_clustering, "output/racli/sintesi_clustering.rds")

cat("Clustering completato\n\n")

# 8. Regressioni determinanti (05_regressioni_determinanti.R) -----

cat("==== 8. Regressioni Determinanti Salariali ====\n\n")

# Prepara dataset per regressioni
cat("Preparazione dataset regressioni...\n")

dati_province <- dati_settore_sesso %>%
  filter(geo_level == "Provincia", sesso == "totale") %>%
  select(
    anno,
    area,
    area_label,
    ripartizione,
    salario_mediano,
    D1,
    D5,
    D9,
    D9_D1
  )

if ("cluster_km" %in% names(dati_clustering)) {
  dati_province <- dati_province %>%
    left_join(
      dati_clustering %>% select(area, cluster_km, label),
      by = "area"
    )
}

dati_regressione <- dati_province %>%
  mutate(
    log_salario = log(salario_mediano),
    anno_norm = anno - 2014,
    ripartizione_cat = factor(
      ripartizione,
      levels = c("Nord-ovest", "Nord-est", "Centro", "Sud", "Isole")
    )
  ) %>%
  filter(!is.na(log_salario))

cat("  Osservazioni per regressione:", nrow(dati_regressione), "\n")

# Modello OLS pooled
cat("Modello OLS pooled...\n")

model_ols <- lm(
  log_salario ~ ripartizione_cat + anno_norm + D9_D1,
  data = dati_regressione
)

coef_ols <- coeftest(model_ols, vcov = vcovCL, cluster = ~area)
r2_ols <- summary(model_ols)$r.squared

cat("  R-squared:", sprintf("%.4f", r2_ols), "\n")

saveRDS(model_ols, "output/racli/model_ols.rds")
saveRDS(coef_ols, "output/racli/coef_ols.rds")

# Modello gap di genere
cat("Modello determinanti gap di genere...\n")

dati_gender <- dati_settore_sesso %>%
  filter(geo_level == "Provincia", sesso %in% c("maschi", "femmine")) %>%
  select(anno, area, area_label, ripartizione, sesso, salario_mediano) %>%
  pivot_wider(names_from = sesso, values_from = salario_mediano) %>%
  mutate(
    gap_gender = (maschi - femmine) / femmine * 100,
    anno_norm = anno - 2014,
    ripartizione_cat = factor(ripartizione)
  ) %>%
  filter(!is.na(gap_gender))

if (nrow(dati_gender) > 50) {
  model_gender <- lm(
    gap_gender ~ ripartizione_cat + anno_norm,
    data = dati_gender
  )
  saveRDS(model_gender, "output/racli/model_gender.rds")
}

# Premio educativo
cat("Analisi premio educativo...\n")

dati_edu_provincia <- dati_educazione %>%
  filter(geo_level == "Provincia", anno == 2022) %>%
  filter(
    educazione %in%
      c(
        "laurea e post-laurea",
        "nessun titolo di studio, licenza di scuola elementare e media"
      )
  ) %>%
  select(area, area_label, educazione, salario_mediano) %>%
  pivot_wider(names_from = educazione, values_from = salario_mediano)

if (ncol(dati_edu_provincia) > 2) {
  names(dati_edu_provincia) <- c(
    "area",
    "area_label",
    "salario_laurea",
    "salario_base"
  )
  dati_edu_provincia <- dati_edu_provincia %>%
    mutate(
      premio_educativo = (salario_laurea - salario_base) / salario_base * 100
    ) %>%
    filter(!is.na(premio_educativo))

  if (nrow(dati_edu_provincia) > 10) {
    saveRDS(dati_edu_provincia, "output/racli/premio_educativo_province.rds")
  }
}

# Premio contratto permanente
cat("Analisi premio contratto permanente...\n")

dati_contr_provincia <- dati_contratto %>%
  filter(geo_level == "Provincia", anno == 2022) %>%
  filter(contratto %in% c("tempo indeterminato", "tempo determinato")) %>%
  select(area, area_label, contratto, salario_mediano) %>%
  pivot_wider(
    names_from = contratto,
    values_from = salario_mediano,
    names_prefix = "salario_"
  )

names(dati_contr_provincia) <- gsub(" ", "_", names(dati_contr_provincia))

if (
  "salario_tempo_indeterminato" %in%
    names(dati_contr_provincia) &&
    "salario_tempo_determinato" %in% names(dati_contr_provincia)
) {
  dati_contr_provincia <- dati_contr_provincia %>%
    mutate(
      premio_permanente = (salario_tempo_indeterminato -
        salario_tempo_determinato) /
        salario_tempo_determinato *
        100
    ) %>%
    filter(!is.na(premio_permanente))

  if (nrow(dati_contr_provincia) > 10) {
    saveRDS(dati_contr_provincia, "output/racli/premio_contratto_province.rds")
  }
}

# Modello OLS con settori e interazioni
cat("Modello OLS con settori e interazioni...\n")

dati_ols_settori <- dati_settori_territorio %>%
  filter(
    tipo_settore == "Sezione NACE",
    ripartizione != "Italia",
    sesso %in% c("maschi", "femmine"),
    dim_aziendale == "totale",
    contratto_occ == "dirigente; impiegato"
  ) %>%
  mutate(
    log_salario = log(salario_mediano),
    settore_cat = relevel(factor(settore), ref = "attivita manifatturiere"),
    ripartizione_cat = relevel(factor(ripartizione), ref = "Nord-ovest"),
    sesso_cat = relevel(factor(sesso), ref = "maschi"),
    anno_norm = anno - 2014
  ) %>%
  filter(!is.na(log_salario) & is.finite(log_salario))

if (nrow(dati_ols_settori) > 100) {
  model_settori <- lm(
    log_salario ~ settore_cat +
      ripartizione_cat +
      sesso_cat +
      anno_norm +
      settore_cat:sesso_cat +
      settore_cat:ripartizione_cat +
      sesso_cat:ripartizione_cat,
    data = dati_ols_settori
  )

  coef_settori_robust <- coeftest(model_settori, vcov = vcovHC, type = "HC1")

  coef_df <- data.frame(
    variabile = rownames(coef_settori_robust),
    coef = coef_settori_robust[, 1],
    se = coef_settori_robust[, 2],
    t_stat = coef_settori_robust[, 3],
    p_value = coef_settori_robust[, 4],
    row.names = NULL
  )

  differenziali_settori <- coef_df %>%
    filter(grepl("^settore_cat", variabile) & !grepl(":", variabile)) %>%
    mutate(
      settore = gsub("settore_cat", "", variabile),
      premio_pct = (exp(coef) - 1) * 100,
      significativo = p_value < 0.05
    ) %>%
    arrange(desc(premio_pct))

  interaz_genere <- coef_df %>%
    filter(grepl("settore_cat.*:sesso_cat", variabile)) %>%
    mutate(
      settore = gsub("settore_cat(.+):sesso_catfemmine", "\\1", variabile),
      diff_gap_pct = (exp(coef) - 1) * 100,
      significativo = p_value < 0.05
    )

  interaz_territorio <- coef_df %>%
    filter(grepl("settore_cat.*:ripartizione_cat", variabile)) %>%
    mutate(
      componenti = gsub(
        "settore_cat(.+):ripartizione_cat(.+)",
        "\\1|\\2",
        variabile
      ),
      settore = sapply(strsplit(componenti, "\\|"), `[`, 1),
      ripartizione = sapply(strsplit(componenti, "\\|"), `[`, 2),
      diff_terr_pct = (exp(coef) - 1) * 100,
      significativo = p_value < 0.05
    ) %>%
    select(-componenti)

  saveRDS(model_settori, "output/racli/model_ols_settori.rds")
  saveRDS(coef_df, "output/racli/coef_ols_settori.rds")
  saveRDS(
    differenziali_settori,
    "output/racli/differenziali_settoriali_stimati.rds"
  )
  saveRDS(interaz_genere, "output/racli/interazioni_settore_genere.rds")
  saveRDS(interaz_territorio, "output/racli/interazioni_settore_territorio.rds")

  cat(
    "  R-squared modello settoriale:",
    sprintf("%.4f", summary(model_settori)$r.squared),
    "\n"
  )
}

# Sintesi regressioni
anno_coef <- coef(model_ols)["anno_norm"]
crescita_annua <- (exp(anno_coef) - 1) * 100

sintesi_regressioni <- list(
  n_obs_ols = nrow(dati_regressione),
  r2_ols = r2_ols,
  crescita_annua_pct = crescita_annua,
  n_province = length(unique(dati_regressione$area)),
  anni_coperti = range(dati_regressione$anno)
)

saveRDS(sintesi_regressioni, "output/racli/sintesi_regressioni.rds")

cat("Regressioni completate\n\n")

# 9. Visualizzazioni (06_visualizzazioni_racli.R) -----

cat("==== 9. Visualizzazioni RACLI ====\n\n")

cat("Generazione grafici...\n")

# 1. Evoluzione temporale salari
p1 <- dati_settore_sesso %>%
  filter(geo_level == "Nazionale", sesso == "totale") %>%
  ggplot(aes(x = anno, y = salario_mediano)) +
  geom_line(linewidth = 1, color = "#2E86AB") +
  geom_point(size = 2, color = "#2E86AB") +
  scale_y_continuous(labels = label_dollar(prefix = "EUR", suffix = "")) +
  labs(
    title = "Evoluzione Salario Mediano Nazionale",
    subtitle = "Italia 2014-2022",
    x = "Anno",
    y = "Salario mediano orario (EUR/h)"
  ) +
  theme_salari()

ggsave(
  "output/racli/grafici/01_evoluzione_salari_nazionale.png",
  p1,
  width = 10,
  height = 6,
  dpi = 300
)

# 2. Gap di genere nel tempo
p2 <- gap_gender_tempo %>%
  ggplot(aes(x = anno, y = gap_medio)) +
  geom_line(linewidth = 1, color = "#A23B72") +
  geom_point(size = 2, color = "#A23B72") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title = "Evoluzione Gap di Genere",
    subtitle = "Differenza salariale Maschi vs Femmine",
    x = "Anno",
    y = "Gap di genere (%)"
  ) +
  theme_salari()

ggsave(
  "output/racli/grafici/02_gap_genere_tempo.png",
  p2,
  width = 10,
  height = 6,
  dpi = 300
)

# 3. Salari per ripartizione
p3 <- dati_settore_sesso %>%
  filter(geo_level == "Ripartizione", sesso == "totale", anno == 2022) %>%
  mutate(ripartizione = reorder(ripartizione, salario_mediano)) %>%
  ggplot(aes(x = ripartizione, y = salario_mediano, fill = ripartizione)) +
  geom_col(show.legend = FALSE) +
  scale_fill_viridis_d(option = "plasma") +
  scale_y_continuous(labels = label_dollar(prefix = "EUR", suffix = "")) +
  labs(
    title = "Salari Mediani per Ripartizione Geografica",
    subtitle = "Anno 2022",
    x = NULL,
    y = "Salario mediano orario (EUR/h)"
  ) +
  theme_salari() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  "output/racli/grafici/03_salari_ripartizioni.png",
  p3,
  width = 10,
  height = 6,
  dpi = 300
)

# 4. Evoluzione Gini
p4 <- evoluzione_disug %>%
  ggplot(aes(x = anno, y = gini)) +
  geom_line(linewidth = 1, color = "#F18F01") +
  geom_point(size = 2, color = "#F18F01") +
  labs(
    title = "Evoluzione Indice di Gini",
    subtitle = "Disuguaglianza salariale nazionale",
    x = "Anno",
    y = "Indice di Gini"
  ) +
  theme_salari()

ggsave(
  "output/racli/grafici/04_evoluzione_gini.png",
  p4,
  width = 10,
  height = 6,
  dpi = 300
)

# 5. Rapporto D9/D1
p5 <- evoluzione_disug %>%
  ggplot(aes(x = anno, y = D9_D1)) +
  geom_line(linewidth = 1, color = "#C73E1D") +
  geom_point(size = 2, color = "#C73E1D") +
  labs(
    title = "Rapporto Interdecile D9/D1",
    subtitle = "Quanto guadagna il 90 percentile rispetto al 10",
    x = "Anno",
    y = "Rapporto D9/D1"
  ) +
  theme_salari()

ggsave(
  "output/racli/grafici/05_rapporto_D9_D1.png",
  p5,
  width = 10,
  height = 6,
  dpi = 300
)

# 6. Box plot salari per ripartizione
p6 <- indici_disug %>%
  filter(anno == 2022, geo_level == "Provincia") %>%
  mutate(ripartizione = reorder(ripartizione, salario_mediano, median)) %>%
  ggplot(aes(x = ripartizione, y = salario_mediano, fill = ripartizione)) +
  geom_boxplot(show.legend = FALSE, alpha = 0.7) +
  scale_fill_viridis_d(option = "plasma") +
  scale_y_continuous(labels = label_dollar(prefix = "EUR", suffix = "")) +
  labs(
    title = "Distribuzione Salari Provinciali per Ripartizione",
    subtitle = "Anno 2022 (box plot)",
    x = NULL,
    y = "Salario mediano orario (EUR/h)"
  ) +
  theme_salari() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  "output/racli/grafici/06_boxplot_salari_ripartizioni.png",
  p6,
  width = 10,
  height = 6,
  dpi = 300
)

# 7. Scatter disuguaglianza vs salario
p7 <- indici_disug %>%
  filter(anno == 2022, geo_level == "Provincia") %>%
  ggplot(aes(x = salario_mediano, y = gini_approx, color = ripartizione)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  scale_x_continuous(labels = label_dollar(prefix = "EUR", suffix = "")) +
  scale_color_viridis_d(option = "plasma") +
  labs(
    title = "Disuguaglianza vs Livello Salariale",
    subtitle = "Province italiane 2022",
    x = "Salario mediano (EUR/h)",
    y = "Indice di Gini",
    color = "Ripartizione"
  ) +
  theme_salari()

ggsave(
  "output/racli/grafici/07_scatter_gini_salario.png",
  p7,
  width = 10,
  height = 6,
  dpi = 300
)

# 8. Cluster province
p8 <- dati_clustering %>%
  ggplot(aes(x = salario_mediano, y = gini_approx, color = label)) +
  geom_point(size = 2.5, alpha = 0.7) +
  scale_x_continuous(labels = label_dollar(prefix = "EUR", suffix = "")) +
  scale_color_manual(
    values = c("Alto reddito" = "#2E86AB", "Basso reddito" = "#A23B72")
  ) +
  labs(
    title = paste0("Clustering Province (K-means, k=", k_opt, ")"),
    subtitle = "Basato su salario mediano, Gini e crescita 2014-2022",
    x = "Salario mediano 2022 (EUR/h)",
    y = "Indice di Gini 2022",
    color = "Cluster"
  ) +
  theme_salari()

ggsave(
  "output/racli/grafici/08_cluster_province.png",
  p8,
  width = 10,
  height = 6,
  dpi = 300
)

# 9. Premio educativo
if (exists("gap_educazione_2022")) {
  p9 <- gap_educazione_2022 %>%
    mutate(educazione = reorder(educazione, salario_mediano)) %>%
    ggplot(aes(x = educazione, y = salario_mediano, fill = educazione)) +
    geom_col(show.legend = FALSE) +
    scale_fill_viridis_d(option = "viridis") +
    scale_y_continuous(labels = label_dollar(prefix = "EUR", suffix = "")) +
    labs(
      title = "Salari per Livello Educativo",
      subtitle = "Italia 2022",
      x = NULL,
      y = "Salario mediano orario (EUR/h)"
    ) +
    theme_salari() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  ggsave(
    "output/racli/grafici/09_salari_educazione.png",
    p9,
    width = 10,
    height = 6,
    dpi = 300
  )
}

# 10. Premio contratto
if (exists("gap_contratto_2022")) {
  p10 <- gap_contratto_2022 %>%
    mutate(contratto = reorder(contratto, salario_mediano)) %>%
    ggplot(aes(x = contratto, y = salario_mediano, fill = contratto)) +
    geom_col(show.legend = FALSE) +
    scale_fill_viridis_d(option = "mako") +
    scale_y_continuous(labels = label_dollar(prefix = "EUR", suffix = "")) +
    labs(
      title = "Salari per Tipo Contratto",
      subtitle = "Italia 2022",
      x = NULL,
      y = "Salario mediano orario (EUR/h)"
    ) +
    theme_salari() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  ggsave(
    "output/racli/grafici/10_salari_contratto.png",
    p10,
    width = 10,
    height = 6,
    dpi = 300
  )
}

# 11. Salari maschi vs femmine per ripartizione
p11 <- dati_settore_sesso %>%
  filter(
    geo_level == "Ripartizione",
    sesso %in% c("maschi", "femmine"),
    anno == 2022
  ) %>%
  mutate(ripartizione = reorder(ripartizione, salario_mediano, mean)) %>%
  ggplot(aes(x = ripartizione, y = salario_mediano, fill = sesso)) +
  geom_col(position = "dodge") +
  scale_fill_manual(
    values = c("maschi" = "#2E86AB", "femmine" = "#A23B72"),
    labels = c("Maschi", "Femmine")
  ) +
  scale_y_continuous(labels = label_dollar(prefix = "EUR", suffix = "")) +
  labs(
    title = "Salari per Sesso e Ripartizione",
    subtitle = "Anno 2022",
    x = NULL,
    y = "Salario mediano orario (EUR/h)",
    fill = "Sesso"
  ) +
  theme_salari() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  "output/racli/grafici/11_salari_sesso_ripartizioni.png",
  p11,
  width = 10,
  height = 6,
  dpi = 300
)

# 12. Fan chart decili
decili_data <- dati_settore_sesso %>%
  filter(geo_level == "Nazionale", sesso == "totale") %>%
  select(anno, D1, D5, D9)

p12 <- ggplot(decili_data, aes(x = anno)) +
  geom_ribbon(aes(ymin = D1, ymax = D9), alpha = 0.2, fill = "#2E86AB") +
  geom_line(aes(y = D1, color = "D1"), linewidth = 1) +
  geom_line(aes(y = D5, color = "D5"), linewidth = 1) +
  geom_line(aes(y = D9, color = "D9"), linewidth = 1) +
  scale_color_manual(
    values = c("D1" = "#C73E1D", "D5" = "#2E86AB", "D9" = "#06A77D"),
    labels = c("D1 (10 percentile)", "D5 (Mediana)", "D9 (90 percentile)")
  ) +
  scale_y_continuous(labels = label_dollar(prefix = "EUR", suffix = "")) +
  labs(
    title = "Fan Chart Distribuzione Salariale",
    subtitle = "Evoluzione decili D1, D5, D9 (2014-2022)",
    x = "Anno",
    y = "Salario orario (EUR/h)",
    color = "Decile"
  ) +
  theme_salari()

ggsave(
  "output/racli/grafici/12_fan_chart_decili.png",
  p12,
  width = 10,
  height = 6,
  dpi = 300
)

# 13. Convergenza sigma
p13 <- sigma_convergence %>%
  ggplot(aes(x = anno, y = cv)) +
  geom_line(linewidth = 1, color = "#F18F01") +
  geom_point(size = 2, color = "#F18F01") +
  labs(
    title = "Sigma-Convergenza Territoriale",
    subtitle = "Coefficiente di variazione salari provinciali",
    x = "Anno",
    y = "Coefficiente di variazione"
  ) +
  theme_salari()

ggsave(
  "output/racli/grafici/13_sigma_convergenza.png",
  p13,
  width = 10,
  height = 6,
  dpi = 300
)

# 14. Beta convergenza
if (file.exists("output/racli/beta_convergence.rds")) {
  beta_conv <- readRDS("output/racli/beta_convergence.rds")

  p14 <- beta_conv %>%
    ggplot(aes(x = log_sal_2014, y = tasso_crescita_annuo)) +
    geom_point(size = 2, alpha = 0.6, color = "#2E86AB") +
    geom_smooth(method = "lm", se = TRUE, color = "#C73E1D") +
    scale_x_continuous(labels = function(x) sprintf("EUR%.1f", exp(x))) +
    scale_y_continuous(labels = label_percent(scale = 1)) +
    labs(
      title = "Beta-Convergenza: Province Povere Crescono Piu Velocemente?",
      subtitle = "Regressione crescita 2014-2022 su livello iniziale",
      x = "Salario 2014 (log scale, EUR/h)",
      y = "Tasso crescita annuo (%)"
    ) +
    theme_salari()

  ggsave(
    "output/racli/grafici/14_beta_convergenza.png",
    p14,
    width = 10,
    height = 6,
    dpi = 300
  )
}

# 15. Top/Bottom 10 province
province_ranking <- indici_disug %>%
  filter(anno == 2022, geo_level == "Provincia") %>%
  arrange(desc(salario_mediano))

top_bottom <- bind_rows(
  province_ranking %>% head(10) %>% mutate(gruppo = "Top 10"),
  province_ranking %>% tail(10) %>% mutate(gruppo = "Bottom 10")
) %>%
  mutate(area_label = reorder(area_label, salario_mediano))

p15 <- top_bottom %>%
  ggplot(aes(x = area_label, y = salario_mediano, fill = gruppo)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("Top 10" = "#06A77D", "Bottom 10" = "#C73E1D")) +
  scale_y_continuous(labels = label_dollar(prefix = "EUR", suffix = "")) +
  labs(
    title = "Top 10 e Bottom 10 Province per Salario Mediano",
    subtitle = "Anno 2022",
    x = NULL,
    y = "Salario mediano orario (EUR/h)",
    fill = NULL
  ) +
  theme_salari()

ggsave(
  "output/racli/grafici/15_top_bottom_province.png",
  p15,
  width = 10,
  height = 8,
  dpi = 300
)

# 16. Ranking settori NACE
ranking_plot_data <- ranking_settori_nace_2022 %>%
  head(20) %>%
  mutate(
    settore_short = ifelse(
      nchar(settore) > 50,
      paste0(substr(settore, 1, 47), "..."),
      settore
    ),
    settore_short = reorder(settore_short, salario_mediano)
  )

p16 <- ranking_plot_data %>%
  ggplot(aes(x = settore_short, y = salario_mediano, fill = salario_mediano)) +
  geom_col(show.legend = FALSE) +
  geom_text(
    aes(label = sprintf("EUR%.2f", salario_mediano)),
    hjust = -0.1,
    size = 3
  ) +
  coord_flip() +
  scale_fill_viridis_c(option = "plasma") +
  scale_y_continuous(
    labels = label_dollar(prefix = "EUR", suffix = ""),
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Ranking Settori NACE per Salario Mediano",
    subtitle = "Top 20 settori - Anno 2022",
    x = NULL,
    y = "Salario mediano orario (EUR/h)"
  ) +
  theme_salari() +
  theme(axis.text.y = element_text(size = 8))

ggsave(
  "output/racli/grafici/16_salari_settori_nace.png",
  p16,
  width = 12,
  height = 10,
  dpi = 300
)

# 17. Scatter salari vs dispersione settori
p17 <- dispersione_settori_2022 %>%
  ggplot(aes(x = salario_mediano, y = D9_D1)) +
  geom_point(aes(size = salario_medio), alpha = 0.7, color = "#2E86AB") +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "#C73E1D",
    linetype = "dashed"
  ) +
  geom_text(
    aes(label = settore_code),
    hjust = -0.2,
    vjust = 0.5,
    size = 2.5,
    alpha = 0.7
  ) +
  scale_x_continuous(labels = label_dollar(prefix = "EUR", suffix = "")) +
  scale_size_continuous(
    name = "Salario Medio",
    labels = label_dollar(prefix = "EUR", suffix = "")
  ) +
  labs(
    title = "Salari vs Dispersione Salariale per Settore NACE",
    subtitle = "Anno 2022 - Codici NACE 2007",
    x = "Salario mediano (EUR/h)",
    y = "Rapporto interdecile D9/D1"
  ) +
  theme_salari()

ggsave(
  "output/racli/grafici/17_scatter_salari_dispersione_settori.png",
  p17,
  width = 12,
  height = 8,
  dpi = 300
)

# 18. Gap genere per settore
gap_plot_data <- gap_gender_settori_2022 %>%
  head(20) %>%
  mutate(
    settore_short = ifelse(
      nchar(settore) > 50,
      paste0(substr(settore, 1, 47), "..."),
      settore
    ),
    settore_short = reorder(settore_short, gap_percentuale)
  )

p18 <- gap_plot_data %>%
  ggplot(aes(x = settore_short, y = gap_percentuale, fill = gap_percentuale)) +
  geom_col(show.legend = FALSE) +
  geom_text(
    aes(label = sprintf("%.1f%%", gap_percentuale)),
    hjust = -0.1,
    size = 3
  ) +
  coord_flip() +
  scale_fill_gradient2(
    low = "#06A77D",
    mid = "#F18F01",
    high = "#C73E1D",
    midpoint = median(gap_plot_data$gap_percentuale)
  ) +
  scale_y_continuous(
    labels = label_percent(scale = 1),
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Gap Salariale di Genere per Settore NACE",
    subtitle = "Top 20 settori con maggior differenziale M/F - Anno 2022",
    x = NULL,
    y = "Gap di genere (%)"
  ) +
  theme_salari() +
  theme(axis.text.y = element_text(size = 8))

ggsave(
  "output/racli/grafici/18_gap_genere_settori.png",
  p18,
  width = 12,
  height = 10,
  dpi = 300
)

# 19. Heatmap settori x ripartizioni
heatmap_data <- dati_st_2022 %>%
  mutate(
    settore_short = ifelse(
      nchar(settore) > 35,
      paste0(substr(settore, 1, 32), "..."),
      settore
    )
  )

ripart_order <- c("Nord-ovest", "Nord-est", "Centro", "Sud", "Isole")
heatmap_data$ripartizione <- factor(
  heatmap_data$ripartizione,
  levels = ripart_order
)

settori_ord <- heatmap_data %>%
  group_by(settore_short) %>%
  summarise(sal_medio = mean(salario_mediano, na.rm = TRUE)) %>%
  arrange(desc(sal_medio)) %>%
  pull(settore_short)

heatmap_data$settore_short <- factor(
  heatmap_data$settore_short,
  levels = rev(settori_ord)
)

p19 <- heatmap_data %>%
  ggplot(aes(x = ripartizione, y = settore_short, fill = salario_mediano)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_label(
    aes(label = sprintf("EUR%.1f", salario_mediano)),
    size = 2.3,
    fill = "white",
    alpha = 0.85,
    label.size = 0,
    label.padding = unit(0.12, "lines")
  ) +
  scale_fill_viridis_c(option = "plasma", name = "Salario\nmediano\n(EUR/h)") +
  labs(
    title = "Salario Mediano per Settore e Ripartizione Geografica",
    subtitle = "Anno 2022 - Sezioni NACE",
    x = NULL,
    y = NULL
  ) +
  theme_salari() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 8)
  )

ggsave(
  "output/racli/grafici/19_heatmap_settori_ripartizioni.png",
  p19,
  width = 12,
  height = 12,
  dpi = 300
)

# 20. Gap Nord-Sud per settore
gap_plot_ns <- gap_territoriale_settori %>%
  filter(!is.na(gap_nord_sud_pct)) %>%
  mutate(
    settore_short = ifelse(
      nchar(settore) > 40,
      paste0(substr(settore, 1, 37), "..."),
      settore
    ),
    settore_short = reorder(settore_short, gap_nord_sud_pct)
  )

p20 <- gap_plot_ns %>%
  ggplot(aes(
    x = settore_short,
    y = gap_nord_sud_pct,
    fill = gap_nord_sud_pct
  )) +
  geom_col(show.legend = FALSE) +
  geom_text(
    aes(
      label = sprintf("%.1f%%", gap_nord_sud_pct),
      hjust = ifelse(gap_nord_sud_pct >= 0, -0.1, 1.1)
    ),
    size = 3
  ) +
  coord_flip() +
  scale_fill_gradient2(
    low = "#06A77D",
    mid = "#F18F01",
    high = "#C73E1D",
    midpoint = median(gap_plot_ns$gap_nord_sud_pct, na.rm = TRUE)
  ) +
  scale_y_continuous(
    labels = label_percent(scale = 1),
    expand = expansion(mult = c(0.05, 0.15))
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title = "Gap Salariale Nord-Sud per Settore NACE",
    subtitle = "Anno 2022 - Differenza % (Nord - Sud) / Sud",
    x = NULL,
    y = "Gap Nord-Sud (%)"
  ) +
  theme_salari() +
  theme(axis.text.y = element_text(size = 8))

ggsave(
  "output/racli/grafici/20_gap_nord_sud_settori.png",
  p20,
  width = 12,
  height = 10,
  dpi = 300
)

cat("Visualizzazioni completate: 20 grafici generati\n\n")

# 10. Cartografie (07_cartografie_italia.R) -----

cat("==== 10. Cartografie Italia ====\n\n")

# Carica shapefile ISTAT
shp_file <- "data/shp/province_italia_istat.rds"
if (file.exists(shp_file)) {
  italy_sf <- readRDS(shp_file)
  cat("Shapefile province ISTAT:", nrow(italy_sf), "geometrie\n")

  # Verifica che cod_uts sia presente
  if ("cod_uts" %in% names(dati_settore_sesso)) {
    # Filtra dati provinciali 2022
    dati_province_2022 <- dati_settore_sesso %>%
      filter(geo_level == "Provincia", sesso == "totale", anno == 2022) %>%
      select(area, area_label, ripartizione, salario_mediano, D9_D1, cod_uts)

    # Join shapefile con dati RACLI su COD_UTS
    map_data <- italy_sf %>%
      left_join(dati_province_2022, by = c("COD_UTS" = "cod_uts"))

    n_matched <- sum(!is.na(map_data$salario_mediano))
    n_total <- nrow(italy_sf)
    match_rate <- n_matched / n_total * 100

    cat(
      "Province matchate:",
      n_matched,
      "/",
      n_total,
      "(",
      sprintf("%.1f%%", match_rate),
      ")\n\n"
    )

    # Join con clustering
    if ("cluster_km" %in% names(dati_clustering)) {
      map_data <- map_data %>%
        left_join(
          dati_clustering %>% select(area, cluster_km, label),
          by = "area"
        )
    }

    # Mappa 1: Salari mediani provinciali 2022
    cat("Generazione mappe...\n")

    p_map1 <- ggplot(map_data) +
      geom_sf(aes(fill = salario_mediano), color = "white", size = 0.1) +
      scale_fill_viridis_c(
        option = "viridis",
        name = "Salario\n(EUR/h)",
        na.value = "gray90"
      ) +
      annotation_scale(location = "bl", width_hint = 0.3) +
      annotation_north_arrow(
        location = "tr",
        style = north_arrow_fancy_orienteering,
        height = unit(1.5, "cm"),
        width = unit(1.5, "cm")
      ) +
      labs(
        title = "Retribuzioni Orarie Mediane per Provincia",
        subtitle = "Anno 2022 - Fonte: ISTAT RACLI",
        caption = "Elaborazione: Giampaolo Montaletti"
      ) +
      theme_void() +
      theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(
          size = 11,
          hjust = 0.5,
          margin = margin(b = 10)
        ),
        plot.caption = element_text(
          size = 8,
          hjust = 1,
          margin = margin(t = 10)
        ),
        legend.position = "right"
      )

    ggsave(
      "output/racli/grafici/21_mappa_salari_province_2022.png",
      p_map1,
      width = 10,
      height = 12,
      dpi = 300
    )

    # Mappa 2: Crescita salariale 2014-2022
    crescita_province <- dati_settore_sesso %>%
      filter(
        geo_level == "Provincia",
        sesso == "totale",
        anno %in% c(2014, 2022)
      ) %>%
      select(area, area_label, anno, salario_mediano, cod_uts) %>%
      tidyr::pivot_wider(
        names_from = anno,
        values_from = salario_mediano,
        names_prefix = "sal_"
      ) %>%
      mutate(crescita_pct = (sal_2022 - sal_2014) / sal_2014 * 100)

    map_crescita <- italy_sf %>%
      left_join(crescita_province, by = c("COD_UTS" = "cod_uts"))

    p_map2 <- ggplot(map_crescita) +
      geom_sf(aes(fill = crescita_pct), color = "white", size = 0.1) +
      scale_fill_gradient2(
        low = "#2166AC",
        mid = "white",
        high = "#B2182B",
        midpoint = 5,
        name = "Crescita\n(%)",
        na.value = "gray90"
      ) +
      annotation_scale(location = "bl", width_hint = 0.3) +
      annotation_north_arrow(
        location = "tr",
        style = north_arrow_fancy_orienteering,
        height = unit(1.5, "cm"),
        width = unit(1.5, "cm")
      ) +
      labs(
        title = "Crescita Salariale per Provincia",
        subtitle = "Periodo 2014-2022 - Fonte: ISTAT RACLI",
        caption = "Elaborazione: Giampaolo Montaletti"
      ) +
      theme_void() +
      theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(
          size = 11,
          hjust = 0.5,
          margin = margin(b = 10)
        ),
        plot.caption = element_text(
          size = 8,
          hjust = 1,
          margin = margin(t = 10)
        ),
        legend.position = "right"
      )

    ggsave(
      "output/racli/grafici/22_mappa_crescita_salari_2014_2022.png",
      p_map2,
      width = 10,
      height = 12,
      dpi = 300
    )

    # Mappa 3: Disuguaglianza D9/D1
    p_map3 <- ggplot(map_data) +
      geom_sf(aes(fill = D9_D1), color = "white", size = 0.1) +
      scale_fill_viridis_c(
        option = "plasma",
        name = "D9/D1",
        na.value = "gray90"
      ) +
      annotation_scale(location = "bl", width_hint = 0.3) +
      annotation_north_arrow(
        location = "tr",
        style = north_arrow_fancy_orienteering,
        height = unit(1.5, "cm"),
        width = unit(1.5, "cm")
      ) +
      labs(
        title = "Disuguaglianza Salariale per Provincia",
        subtitle = "Rapporto D9/D1 - Anno 2022 - Fonte: ISTAT RACLI",
        caption = "Elaborazione: Giampaolo Montaletti"
      ) +
      theme_void() +
      theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(
          size = 11,
          hjust = 0.5,
          margin = margin(b = 10)
        ),
        plot.caption = element_text(
          size = 8,
          hjust = 1,
          margin = margin(t = 10)
        ),
        legend.position = "right"
      )

    ggsave(
      "output/racli/grafici/23_mappa_disuguaglianza_D9_D1.png",
      p_map3,
      width = 10,
      height = 12,
      dpi = 300
    )

    # Mappa 4: Cluster
    if ("cluster_km" %in% names(map_data)) {
      map_data <- map_data %>% mutate(cluster_factor = factor(cluster_km))

      p_map4 <- ggplot(map_data) +
        geom_sf(aes(fill = cluster_factor), color = "white", size = 0.1) +
        scale_fill_brewer(
          palette = "Set2",
          name = "Cluster",
          na.value = "gray90"
        ) +
        annotation_scale(location = "bl", width_hint = 0.3) +
        annotation_north_arrow(
          location = "tr",
          style = north_arrow_fancy_orienteering,
          height = unit(1.5, "cm"),
          width = unit(1.5, "cm")
        ) +
        labs(
          title = "Clustering Province per Profilo Salariale",
          subtitle = "K-means clustering - Anno 2022 - Fonte: ISTAT RACLI",
          caption = "Elaborazione: Giampaolo Montaletti"
        ) +
        theme_void() +
        theme(
          plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(
            size = 11,
            hjust = 0.5,
            margin = margin(b = 10)
          ),
          plot.caption = element_text(
            size = 8,
            hjust = 1,
            margin = margin(t = 10)
          ),
          legend.position = "right"
        )

      ggsave(
        "output/racli/grafici/24_mappa_cluster_province.png",
        p_map4,
        width = 10,
        height = 12,
        dpi = 300
      )
    }

    saveRDS(map_data, "output/racli/map_data_province.rds")
    cat("Cartografie completate\n\n")
  } else {
    cat("Colonna cod_uts non trovata, cartografie saltate\n\n")
  }
} else {
  cat("Shapefile ISTAT non trovato, cartografie saltate\n")
  cat("Eseguire prima: Rscript 00_download_shapefile.R\n\n")
}

# 11. Validazione (08_validazione_dati.R) -----

cat("==== 11. Validazione Dati RACLI ====\n\n")

# Check 1: Copertura temporale
cat("Check 1: Copertura temporale...\n")

anni_attesi <- 2014:2022
test_anni_sesso <- all(anni_attesi %in% unique(dati_settore_sesso$anno))
test_anni_edu <- all(anni_attesi %in% unique(dati_educazione$anno))
test_anni_contr <- all(anni_attesi %in% unique(dati_contratto$anno))

cat(
  "  Copertura completa (sesso):",
  ifelse(test_anni_sesso, "OK", "FAIL"),
  "\n"
)
cat(
  "  Copertura completa (educazione):",
  ifelse(test_anni_edu, "OK", "FAIL"),
  "\n"
)
cat(
  "  Copertura completa (contratto):",
  ifelse(test_anni_contr, "OK", "FAIL"),
  "\n\n"
)

# Check 2: Missing values
cat("Check 2: Missing values...\n")

check_missing <- function(data, dataset_name) {
  missing_rate <- data %>%
    summarise(across(everything(), ~ mean(is.na(.)))) %>%
    pivot_longer(everything(), names_to = "var", values_to = "pct_missing") %>%
    filter(pct_missing > 0.05)

  if (nrow(missing_rate) > 0) {
    cat(
      "  ",
      dataset_name,
      ": variabili con >5% missing -",
      nrow(missing_rate),
      "\n"
    )
  } else {
    cat("  ", dataset_name, ": OK (nessuna variabile >5% missing)\n")
  }
}

check_missing(dati_settore_sesso, "dati_sesso")
check_missing(dati_educazione, "dati_educazione")
check_missing(dati_contratto, "dati_contratto")
cat("\n")

# Check 3: Ordinamento decili
cat("Check 3: Ordinamento decili...\n")

check_decili <- function(data, dataset_name) {
  check <- data %>%
    filter(!is.na(D1), !is.na(D5), !is.na(D9)) %>%
    mutate(ordine_ok = (D1 <= D5) & (D5 <= D9))

  pct_violazioni <- mean(!check$ordine_ok, na.rm = TRUE) * 100
  cat(
    "  ",
    dataset_name,
    ": violazioni",
    sprintf("%.2f%%", pct_violazioni),
    ifelse(pct_violazioni < 1, "(OK)", "(ATTENZIONE)"),
    "\n"
  )
}

check_decili(dati_settore_sesso, "dati_sesso")
check_decili(dati_educazione, "dati_educazione")
check_decili(dati_contratto, "dati_contratto")
cat("\n")

# Sintesi validazione
sintesi_validazione <- list(
  copertura_temporale = list(
    sesso = test_anni_sesso,
    educazione = test_anni_edu,
    contratto = test_anni_contr
  ),
  data_esecuzione = Sys.time()
)

saveRDS(sintesi_validazione, "output/racli/sintesi_validazione.rds")

cat("Validazione completata\n\n")

# 12. Epilogo -----

cat("==== Pipeline RACLI Completata ====\n\n")

# Elenca file creati
cat("File RDS creati in output/racli/:\n")
rds_files <- list.files("output/racli", pattern = "\\.rds$", full.names = FALSE)
for (f in rds_files) {
  cat("  -", f, "\n")
}

cat("\nGrafici PNG creati in output/racli/grafici/:\n")
png_files <- list.files(
  "output/racli/grafici",
  pattern = "\\.png$",
  full.names = FALSE
)
for (f in png_files) {
  cat("  -", f, "\n")
}

cat("\n==== Script completato con successo ====\n")
cat("Data esecuzione:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
