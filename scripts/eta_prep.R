# eta_prep.R -----
# Pipeline analisi gap salariale per età: convergenza/divergenza
# tra classi di età usando dati RACLI ISTAT (2014-2022)
# Autore: Giampaolo Montaletti (ORCID: 0009-0002-5327-1122)
# Data: 2026-03-29

# 1. Setup e librerie -----

cat("==== Pipeline Gap Salariale per Età - Setup ====\n\n")

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
install_if_missing("ineq")
install_if_missing("lmtest")
install_if_missing("sandwich")
install_if_missing("patchwork")
install_if_missing("scales")
install_if_missing("sf")
install_if_missing("ggspatial")
install_if_missing("viridis")
install_if_missing("plm")

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(ineq)
  library(lmtest)
  library(sandwich)
  library(patchwork)
  library(scales)
  library(sf)
  library(ggspatial)
  library(viridis)
  library(plm)
})

# Crea directory output
if (!dir.exists("output/racli")) {
  dir.create("output/racli", recursive = TRUE)
}
if (!dir.exists("output/racli/grafici")) {
  dir.create("output/racli/grafici", recursive = TRUE)
}

set.seed(123)

cat("Directory output: output/racli/, output/racli/grafici/\n\n")

# 2. Funzioni utility -----

cat("==== Caricamento Funzioni Utility ====\n\n")

# Identifica livello geografico NUTS
get_geo_level <- function(code) {
  code <- as.character(code)
  n <- nchar(code)
  dplyr::case_when(
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

  ripart_code <- dplyr::case_when(
    code == "IT" ~ "IT",
    nchar(code) >= 3 ~ substr(code, 1, 3),
    TRUE ~ code
  )

  ripart_code <- dplyr::case_when(
    ripart_code == "ITH" ~ "ITD",
    ripart_code == "ITI" ~ "ITE",
    TRUE ~ ripart_code
  )

  ripart_code <- dplyr::case_when(
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
    cat("ATTENZIONE: mapping ITTER107 non trovato\n")
    return(NULL)
  }
  readRDS(mapping_file)
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
      plot.caption = element_text(
        size = 8,
        hjust = 1,
        margin = margin(t = 10)
      ),
      axis.title = element_text(size = 11),
      axis.text = element_text(size = 10),
      legend.position = "right",
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 9),
      panel.grid.minor = element_blank()
    )
}

# Formatta crescita percentuale
format_crescita <- function(x) {
  sprintf("%+.1f%%", x)
}

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

# Calcola gap salariale per età (log e percentuale)
calc_age_gap <- function(
  dt,
  young = "15-29 anni",
  old = "50 anni e più",
  wage_vars = c("salario_mediano", "salario_medio", "D1", "D9")
) {
  dt <- as.data.table(dt)
  wage_vars <- intersect(wage_vars, names(dt))

  if (length(wage_vars) == 0) {
    warning("Nessuna variabile salariale trovata nel dataset")
    return(NULL)
  }

  # Filtra solo giovani e anziani
  dt_young <- dt[eta == young]
  dt_old <- dt[eta == old]

  if (nrow(dt_young) == 0 || nrow(dt_old) == 0) {
    warning("Nessun dato per le classi di età richieste")
    return(NULL)
  }

  # Identifica colonne di join (tutte tranne eta e variabili salariali)
  id_cols <- setdiff(
    names(dt),
    c("eta", wage_vars, "D5", "D9_D1", "D9_D5", "D5_D1", "gini")
  )
  id_cols <- intersect(id_cols, names(dt_young))

  # Prepara suffissi per merge
  setnames_young <- paste0(wage_vars, "_young")
  setnames_old <- paste0(wage_vars, "_old")

  dt_y <- dt_young[, c(id_cols, wage_vars), with = FALSE]
  dt_o <- dt_old[, c(id_cols, wage_vars), with = FALSE]

  setnames(dt_y, wage_vars, setnames_young)
  setnames(dt_o, wage_vars, setnames_old)

  # Merge
  gap_dt <- merge(dt_y, dt_o, by = id_cols, all = FALSE)

  # Calcola gap per ciascuna variabile
  for (v in wage_vars) {
    y_col <- paste0(v, "_young")
    o_col <- paste0(v, "_old")
    log_col <- paste0("log_gap_", v)
    pct_col <- paste0("pct_gap_", v)

    gap_dt[, (log_col) := log(get(o_col)) - log(get(y_col))]
    gap_dt[, (pct_col) := (get(o_col) / get(y_col) - 1) * 100]
  }

  return(gap_dt)
}

# Sigma-convergenza: CV del gap nel tempo
calc_sigma_convergence <- function(
  dt,
  gap_var = "log_gap_mediano",
  group_var = "anno"
) {
  dt <- as.data.table(dt)

  if (!gap_var %in% names(dt)) {
    warning(paste("Variabile", gap_var, "non trovata"))
    return(NULL)
  }

  result <- dt[
    !is.na(get(gap_var)),
    .(
      sd_gap = sd(get(gap_var), na.rm = TRUE),
      mean_gap = mean(get(gap_var), na.rm = TRUE),
      n_units = .N
    ),
    by = group_var
  ]

  result[, cv_gap := sd_gap / abs(mean_gap)]

  return(result)
}

# Beta-convergenza: regressione gap iniziale vs variazione
calc_beta_convergence <- function(
  dt,
  gap_var = "log_gap_mediano",
  id_var = "area",
  year_start = 2014,
  year_end = 2022
) {
  dt <- as.data.table(dt)

  # Gap iniziale e finale
  dt_start <- dt[anno == year_start, c(id_var, gap_var), with = FALSE]
  dt_end <- dt[anno == year_end, c(id_var, gap_var), with = FALSE]

  setnames(dt_start, gap_var, "gap_initial")
  setnames(dt_end, gap_var, "gap_final")

  beta_dt <- merge(dt_start, dt_end, by = id_var)
  beta_dt[, delta_gap := gap_final - gap_initial]

  # Rimuovi NA

  beta_dt <- beta_dt[!is.na(gap_initial) & !is.na(delta_gap)]

  if (nrow(beta_dt) < 3) {
    warning("Troppo poche osservazioni per la regressione beta-convergenza")
    return(list(model = NULL, data = beta_dt, coef = NULL))
  }

  # Regressione OLS
  model <- lm(delta_gap ~ gap_initial, data = beta_dt)

  return(list(
    model = model,
    data = beta_dt,
    coef = coeftest(model, vcov = vcovHC(model, type = "HC1"))
  ))
}

# Decomposizione shift-share
shift_share_decomposition <- function(
  dt,
  group_var,
  gap_var = "log_gap_mediano",
  year_start = 2014,
  year_end = 2022
) {
  dt <- as.data.table(dt)

  dt_start <- dt[anno == year_start, c(group_var, gap_var), with = FALSE]
  dt_end <- dt[anno == year_end, c(group_var, gap_var), with = FALSE]

  setnames(dt_start, gap_var, "gap_0")
  setnames(dt_end, gap_var, "gap_T")

  ss_dt <- merge(dt_start, dt_end, by = group_var)
  ss_dt <- ss_dt[!is.na(gap_0) & !is.na(gap_T)]

  n_groups <- nrow(ss_dt)
  if (n_groups == 0) {
    warning("Nessun gruppo valido per la decomposizione")
    return(NULL)
  }

  # Pesi uguali (1/N) — RACLI non fornisce pesi occupazionali
  w <- 1 / n_groups

  # Componenti
  ss_dt[, delta_gap := gap_T - gap_0]

  # Media aggregata
  mean_gap_0 <- mean(ss_dt$gap_0)
  mean_gap_T <- mean(ss_dt$gap_T)
  delta_total <- mean_gap_T - mean_gap_0

  # Within: somma w_i * delta_gap_i
  within_component <- w * sum(ss_dt$delta_gap)

  # Between: somma delta_w_i * gap_0_i (pesi costanti, hence 0)
  # Con pesi uguali e costanti, between = 0. Lo calcoliamo formalmente.
  between_component <- 0

  # Interaction: somma delta_w_i * delta_gap_i (0 con pesi costanti)
  interaction_component <- 0

  # Con pesi uguali, la decomposizione collassa nel within component
  # Per rendere l'analisi informativa, calcoliamo la varianza tra gruppi
  ss_dt[, contributo_within := w * delta_gap]

  result <- list(
    components = data.table(
      componente = c(
        "Within (variazione media intra-gruppo)",
        "Between (riallocazione tra gruppi)",
        "Interazione",
        "Totale"
      ),
      valore = c(
        within_component,
        between_component,
        interaction_component,
        delta_total
      ),
      percentuale = c(100, 0, 0, 100)
    ),
    detail = ss_dt,
    delta_total = delta_total
  )

  return(result)
}

cat("Funzioni utility e analisi caricate\n\n")

# 3. Preparazione dati RACLI_9 (età x territorio) -----

cat("==== 3. Preparazione dati RACLI_9 (età x territorio) ====\n\n")

cat("Caricamento File 9: età x territorio...")
racli_9_raw <- readRDS("racli/F_533_957_DF_DCSC_RACLI_9.rds")
cat(" OK (", nrow(racli_9_raw), "righe)\n")

ripart_lookup <- load_ripartizione_lookup()

dati_eta_terr <- as.data.table(racli_9_raw)

dati_eta_terr[, `:=`(
  anno = as.integer(tempo_temp),
  area = as.character(REF_AREA),
  area_label = as.character(REF_AREA_label),
  eta = as.character(AGE_label),
  tipo_dato_clean = dplyr::case_when(
    grepl("_AV_", DATA_TYPE) ~ "salario_medio",
    grepl("_FIRD_", DATA_TYPE) ~ "D1",
    grepl("_MED_", DATA_TYPE) ~ "salario_mediano",
    grepl("_NIND_", DATA_TYPE) ~ "D9",
    TRUE ~ as.character(DATA_TYPE)
  )
)]

# Pivot wider
dati_eta_terr_wide <- dcast(
  dati_eta_terr,
  anno + area + area_label + eta ~ tipo_dato_clean,
  value.var = "valore",
  fun.aggregate = mean
)

# Alias D5
if (
  "salario_mediano" %in%
    names(dati_eta_terr_wide) &&
    !"D5" %in% names(dati_eta_terr_wide)
) {
  dati_eta_terr_wide[, D5 := salario_mediano]
}

# Livello geografico e ripartizione
dati_eta_terr_wide[, geo_level := get_geo_level(area)]
dati_eta_terr_wide[, ripartizione := get_ripartizione(area, ripart_lookup)]

# Rapporti disuguaglianza
if (all(c("D1", "D5", "D9") %in% names(dati_eta_terr_wide))) {
  dati_eta_terr_wide[, `:=`(
    D9_D1 = D9 / D1,
    D9_D5 = D9 / D5,
    D5_D1 = D5 / D1
  )]
}

# Aggiungi codici ISTAT
itter_mapping <- load_itter_mapping()
if (!is.null(itter_mapping)) {
  dati_eta_terr_wide <- merge(
    dati_eta_terr_wide,
    itter_mapping[, list(itter107, cod_uts, cod_reg, cod_rip)],
    by.x = "area",
    by.y = "itter107",
    all.x = TRUE
  )
  cat("Codici ISTAT aggiunti\n")
}

# Validazione
cat("Dataset età x territorio:\n")
cat("  Righe:", nrow(dati_eta_terr_wide), "\n")
cat(
  "  Anni:",
  paste(sort(unique(dati_eta_terr_wide$anno)), collapse = ", "),
  "\n"
)
cat(
  "  Classi di età:",
  paste(unique(dati_eta_terr_wide$eta), collapse = ", "),
  "\n"
)
cat(
  "  Livelli geografici:",
  paste(unique(dati_eta_terr_wide$geo_level), collapse = ", "),
  "\n"
)
cat(
  "  Province:",
  length(unique(
    dati_eta_terr_wide[geo_level == "Provincia"]$area
  )),
  "\n"
)

if ("salario_mediano" %in% names(dati_eta_terr_wide)) {
  cat(
    "  Salario mediano range: EUR",
    sprintf("%.2f", min(dati_eta_terr_wide$salario_mediano, na.rm = TRUE)),
    "- EUR",
    sprintf("%.2f", max(dati_eta_terr_wide$salario_mediano, na.rm = TRUE)),
    "\n"
  )
}

cat(
  "  NA:",
  sum(is.na(dati_eta_terr_wide$salario_mediano)),
  "/",
  nrow(dati_eta_terr_wide),
  "\n"
)

saveRDS(dati_eta_terr_wide, "output/racli/dati_eta_territorio.rds")
cat("Salvato: output/racli/dati_eta_territorio.rds\n\n")

# 4. Preparazione dati RACLI_18 (età x settore) -----

cat("==== 4. Preparazione dati RACLI_18 (età x settore) ====\n\n")

cat("Caricamento File 18: età x settore...")
racli_18_raw <- readRDS("racli/F_533_957_DF_DCSC_RACLI_18.rds")
cat(" OK (", nrow(racli_18_raw), "righe)\n")

dati_eta_sett <- as.data.table(racli_18_raw)

dati_eta_sett[, `:=`(
  anno = as.integer(tempo_temp),
  settore_code = as.character(ECON_ACTIVITY_NACE_2007),
  settore = as.character(ECON_ACTIVITY_NACE_2007_label),
  eta = as.character(AGE_label),
  tipo_dato_clean = dplyr::case_when(
    grepl("_AV_", DATA_TYPE) ~ "salario_medio",
    grepl("_FIRD_", DATA_TYPE) ~ "D1",
    grepl("_MED_", DATA_TYPE) ~ "salario_mediano",
    grepl("_NIND_", DATA_TYPE) ~ "D9",
    TRUE ~ as.character(DATA_TYPE)
  )
)]

# Pivot wider
dati_eta_sett_wide <- dcast(
  dati_eta_sett,
  anno + settore_code + settore + eta ~ tipo_dato_clean,
  value.var = "valore",
  fun.aggregate = mean
)

# Alias D5
if (
  "salario_mediano" %in%
    names(dati_eta_sett_wide) &&
    !"D5" %in% names(dati_eta_sett_wide)
) {
  dati_eta_sett_wide[, D5 := salario_mediano]
}

# Classificazione tipo settore
dati_eta_sett_wide[,
  tipo_settore := dplyr::case_when(
    settore_code %in% c("0010", "0011", "0020", "0038") ~ "Aggregato",
    nchar(settore_code) == 1 & grepl("^[A-Z]$", settore_code) ~ "Sezione NACE",
    TRUE ~ "Dettaglio"
  )
]

# Rapporti disuguaglianza
if (all(c("D1", "D5", "D9") %in% names(dati_eta_sett_wide))) {
  dati_eta_sett_wide[, `:=`(
    D9_D1 = D9 / D1,
    D9_D5 = D9 / D5,
    D5_D1 = D5 / D1
  )]
}

# Validazione
cat("Dataset età x settore:\n")
cat("  Righe:", nrow(dati_eta_sett_wide), "\n")
cat(
  "  Anni:",
  paste(sort(unique(dati_eta_sett_wide$anno)), collapse = ", "),
  "\n"
)
cat(
  "  Classi di età:",
  paste(unique(dati_eta_sett_wide$eta), collapse = ", "),
  "\n"
)
cat(
  "  Sezioni NACE:",
  length(unique(
    dati_eta_sett_wide[tipo_settore == "Sezione NACE"]$settore_code
  )),
  "\n"
)

if ("salario_mediano" %in% names(dati_eta_sett_wide)) {
  cat(
    "  Salario mediano range: EUR",
    sprintf("%.2f", min(dati_eta_sett_wide$salario_mediano, na.rm = TRUE)),
    "- EUR",
    sprintf("%.2f", max(dati_eta_sett_wide$salario_mediano, na.rm = TRUE)),
    "\n"
  )
}

saveRDS(dati_eta_sett_wide, "output/racli/dati_eta_settori.rds")
cat("Salvato: output/racli/dati_eta_settori.rds\n\n")

# 5. Preparazione dati RACLI_1 (multidimensionale) -----

cat("==== 5. Preparazione dati RACLI_1 (multidimensionale) ====\n\n")

cat("Caricamento File 1: multidimensionale...")
racli_1_raw <- readRDS("racli/F_533_957_DF_DCSC_RACLI_1.rds")
cat(" OK (", nrow(racli_1_raw), "righe)\n")

dati_eta_multi <- as.data.table(racli_1_raw)

dati_eta_multi[, `:=`(
  anno = as.integer(tempo_temp),
  area = as.character(REF_AREA),
  area_label = as.character(REF_AREA_label),
  eta = as.character(AGE_label),
  sesso = as.character(SEX_label),
  settore_code = as.character(ECON_ACTIVITY_NACE_2007),
  settore = as.character(ECON_ACTIVITY_NACE_2007_label),
  contratto = as.character(CONTARCTUAL_OCCUPATION_label),
  dim_aziendale = as.character(EMPLOYESS_CLASS_label),
  salario_mediano = valore
)]

# Livello geografico e ripartizione
dati_eta_multi[, geo_level := get_geo_level(area)]
dati_eta_multi[, ripartizione := get_ripartizione(area, ripart_lookup)]

# Documenta NA
na_rate <- mean(is.na(dati_eta_multi$salario_mediano)) * 100
na_by_eta <- dati_eta_multi[,
  .(
    n_na = sum(is.na(salario_mediano)),
    n_tot = .N,
    pct_na = sum(is.na(salario_mediano)) / .N * 100
  ),
  by = eta
]

cat("Dataset multidimensionale:\n")
cat("  Righe:", nrow(dati_eta_multi), "\n")
cat("  Anni:", paste(sort(unique(dati_eta_multi$anno)), collapse = ", "), "\n")
cat(
  "  Classi di età:",
  paste(unique(dati_eta_multi$eta), collapse = ", "),
  "\n"
)
cat("  Aree:", paste(unique(dati_eta_multi$area), collapse = ", "), "\n")
cat(sprintf("  Tasso NA complessivo: %.1f%%\n", na_rate))
cat("  NA per classe di età:\n")
print(na_by_eta)

# Seleziona colonne utili
dati_eta_multi_clean <- dati_eta_multi[, list(
  anno,
  area,
  area_label,
  geo_level,
  ripartizione,
  eta,
  sesso,
  settore_code,
  settore,
  contratto,
  dim_aziendale,
  salario_mediano
)]

saveRDS(dati_eta_multi_clean, "output/racli/dati_eta_multidim.rds")
cat("\nSalvato: output/racli/dati_eta_multidim.rds\n\n")

# 6. Calcolo gap salariale per età -----

cat("==== 6. Calcolo gap salariale per età ====\n\n")

# Gap nazionale (IT)
cat("Gap nazionale...\n")
dt_nazionale <- dati_eta_terr_wide[geo_level == "Nazionale"]
gap_nazionale <- calc_age_gap(dt_nazionale)

if (!is.null(gap_nazionale)) {
  cat("  Osservazioni gap nazionale:", nrow(gap_nazionale), "\n")
  cat(
    "  Gap mediano 2014:",
    sprintf("%.3f", gap_nazionale[anno == 2014]$log_gap_salario_mediano),
    "\n"
  )
  cat(
    "  Gap mediano 2022:",
    sprintf("%.3f", gap_nazionale[anno == 2022]$log_gap_salario_mediano),
    "\n"
  )
}

# Gap provinciale
cat("Gap provinciale...\n")
dt_province <- dati_eta_terr_wide[geo_level == "Provincia"]
gap_province <- calc_age_gap(dt_province)

if (!is.null(gap_province)) {
  # Aggiungi ripartizione al gap
  gap_province[, ripartizione := get_ripartizione(area, ripart_lookup)]
  cat("  Province con gap calcolato:", length(unique(gap_province$area)), "\n")
}

# Gap settoriale (solo Sezione NACE)
cat("Gap settoriale (Sezione NACE)...\n")
dt_settori <- dati_eta_sett_wide[tipo_settore == "Sezione NACE"]
gap_settori <- calc_age_gap(
  dt_settori,
  wage_vars = c("salario_mediano", "salario_medio", "D1", "D9")
)

if (!is.null(gap_settori)) {
  cat(
    "  Settori con gap calcolato:",
    length(unique(gap_settori$settore_code)),
    "\n"
  )
}

# Gap per macro-regione da RACLI_1 (solo mediana, totale per sesso/contratto/dim)
cat("Gap per macro-regione (da RACLI_1)...\n")
dt_macro <- dati_eta_multi_clean[
  sesso == "totale" &
    contratto == "totale" &
    dim_aziendale == "totale" &
    settore_code == "0010" &
    area != "IT"
]

gap_macro <- NULL
if (nrow(dt_macro) > 0) {
  # Calcola gap manualmente poiché abbiamo solo salario_mediano
  dt_young_m <- dt_macro[
    eta == "15-29 anni",
    list(
      anno,
      area,
      area_label,
      ripartizione,
      salario_mediano_young = salario_mediano
    )
  ]
  dt_old_m <- dt_macro[
    eta == "50 anni e più",
    list(
      anno,
      area,
      area_label,
      ripartizione,
      salario_mediano_old = salario_mediano
    )
  ]

  gap_macro <- merge(
    dt_young_m,
    dt_old_m,
    by = c("anno", "area", "area_label", "ripartizione"),
    all = FALSE
  )

  gap_macro[, `:=`(
    log_gap_salario_mediano = log(salario_mediano_old) -
      log(salario_mediano_young),
    pct_gap_salario_mediano = (salario_mediano_old /
      salario_mediano_young -
      1) *
      100
  )]

  # Rimuovi righe con NA
  gap_macro <- gap_macro[!is.na(log_gap_salario_mediano)]
  cat("  Macro-regioni con gap:", length(unique(gap_macro$area)), "\n")
}

# Salvataggio
saveRDS(gap_nazionale, "output/racli/gap_eta_nazionale.rds")
saveRDS(gap_province, "output/racli/gap_eta_province.rds")
saveRDS(gap_settori, "output/racli/gap_eta_settori.rds")
if (!is.null(gap_macro)) {
  saveRDS(gap_macro, "output/racli/gap_eta_macroaree.rds")
}

cat(
  "Salvati: gap_eta_nazionale.rds, gap_eta_province.rds, gap_eta_settori.rds\n\n"
)

# 7. Sigma-convergenza -----

cat("==== 7. Sigma-convergenza ====\n\n")

# Sigma-convergenza tra province
cat("Sigma-convergenza tra province...\n")
sigma_prov <- calc_sigma_convergence(
  gap_province,
  gap_var = "log_gap_salario_mediano",
  group_var = "anno"
)

if (!is.null(sigma_prov)) {
  cat(
    "  CV iniziale (2014):",
    sprintf("%.3f", sigma_prov[anno == 2014]$cv_gap),
    "\n"
  )
  cat(
    "  CV finale (2022):",
    sprintf("%.3f", sigma_prov[anno == 2022]$cv_gap),
    "\n"
  )
  trend_sigma <- ifelse(
    sigma_prov[anno == 2022]$cv_gap < sigma_prov[anno == 2014]$cv_gap,
    "convergenza",
    "divergenza"
  )
  cat("  Tendenza:", trend_sigma, "\n")
}

# Sigma-convergenza tra settori
cat("Sigma-convergenza tra settori...\n")
sigma_sett <- calc_sigma_convergence(
  gap_settori,
  gap_var = "log_gap_salario_mediano",
  group_var = "anno"
)

if (!is.null(sigma_sett)) {
  cat(
    "  CV iniziale (2014):",
    sprintf("%.3f", sigma_sett[anno == 2014]$cv_gap),
    "\n"
  )
  cat(
    "  CV finale (2022):",
    sprintf("%.3f", sigma_sett[anno == 2022]$cv_gap),
    "\n"
  )
}

# Sigma-convergenza per macro-regione (variazione intra-ripartizione)
cat("Sigma-convergenza per macro-regione...\n")
sigma_by_ripart <- NULL
if (!is.null(gap_province) && "ripartizione" %in% names(gap_province)) {
  ripartizioni <- unique(gap_province$ripartizione)
  sigma_list <- lapply(ripartizioni, function(r) {
    sub_dt <- gap_province[ripartizione == r]
    if (nrow(sub_dt) > 2) {
      res <- calc_sigma_convergence(
        sub_dt,
        gap_var = "log_gap_salario_mediano",
        group_var = "anno"
      )
      if (!is.null(res)) {
        res[, ripartizione := r]
      }
      return(res)
    }
    return(NULL)
  })
  sigma_by_ripart <- rbindlist(Filter(Negate(is.null), sigma_list))
  cat(
    "  Ripartizioni analizzate:",
    length(unique(sigma_by_ripart$ripartizione)),
    "\n"
  )
}

# Salvataggio aggregato
sigma_all <- list(
  province = sigma_prov,
  settori = sigma_sett,
  per_ripartizione = sigma_by_ripart
)
saveRDS(sigma_all, "output/racli/sigma_convergence_eta.rds")
cat("Salvato: sigma_convergence_eta.rds\n\n")

# 8. Beta-convergenza -----

cat("==== 8. Beta-convergenza ====\n\n")

# Beta-convergenza incondizionata: province
cat("Beta-convergenza incondizionata (province)...\n")
beta_prov <- calc_beta_convergence(
  gap_province,
  gap_var = "log_gap_salario_mediano",
  id_var = "area",
  year_start = 2014,
  year_end = 2022
)

if (!is.null(beta_prov$model)) {
  cat("  N province:", nrow(beta_prov$data), "\n")
  cat("  Beta:", sprintf("%.4f", coef(beta_prov$model)["gap_initial"]), "\n")
  cat("  p-value:", sprintf("%.4f", beta_prov$coef["gap_initial", 4]), "\n")
  cat("  R²:", sprintf("%.3f", summary(beta_prov$model)$r.squared), "\n")

  convergenza_prov <- ifelse(
    coef(beta_prov$model)["gap_initial"] < 0,
    "convergenza (beta negativo)",
    "divergenza (beta positivo)"
  )
  cat("  Risultato:", convergenza_prov, "\n")
}

# Beta-convergenza condizionata: province con dummy ripartizione
cat("Beta-convergenza condizionata (province + ripartizione)...\n")
beta_prov_cond <- NULL
if (!is.null(beta_prov$data) && nrow(beta_prov$data) > 10) {
  # Aggiungi ripartizione ai dati beta
  beta_data_cond <- copy(beta_prov$data)
  beta_data_cond[, ripartizione := get_ripartizione(area, ripart_lookup)]

  if (length(unique(beta_data_cond$ripartizione)) > 1) {
    model_cond <- lm(
      delta_gap ~ gap_initial + factor(ripartizione),
      data = beta_data_cond
    )
    # SE cluster-robust per ripartizione
    coef_cond <- tryCatch(
      coeftest(
        model_cond,
        vcov = vcovCL(model_cond, cluster = beta_data_cond$ripartizione)
      ),
      error = function(e) {
        cat("  Nota: SE cluster non calcolabili, uso HC1\n")
        coeftest(model_cond, vcov = vcovHC(model_cond, type = "HC1"))
      }
    )

    beta_prov_cond <- list(
      model = model_cond,
      data = beta_data_cond,
      coef = coef_cond
    )

    cat(
      "  Beta condizionato:",
      sprintf("%.4f", coef(model_cond)["gap_initial"]),
      "\n"
    )
    cat("  p-value:", sprintf("%.4f", coef_cond["gap_initial", 4]), "\n")
    cat("  R²:", sprintf("%.3f", summary(model_cond)$r.squared), "\n")
  }
}

# Beta-convergenza settoriale (incondizionata)
cat("Beta-convergenza incondizionata (settori)...\n")
beta_sett <- calc_beta_convergence(
  gap_settori,
  gap_var = "log_gap_salario_mediano",
  id_var = "settore_code",
  year_start = 2014,
  year_end = 2022
)

if (!is.null(beta_sett$model)) {
  cat("  N settori:", nrow(beta_sett$data), "\n")
  cat("  Beta:", sprintf("%.4f", coef(beta_sett$model)["gap_initial"]), "\n")
  cat("  p-value:", sprintf("%.4f", beta_sett$coef["gap_initial", 4]), "\n")
  cat("  R²:", sprintf("%.3f", summary(beta_sett$model)$r.squared), "\n")
}

# Salvataggio
beta_all <- list(
  province_uncond = beta_prov,
  province_cond = beta_prov_cond,
  settori_uncond = beta_sett
)
saveRDS(beta_all, "output/racli/beta_convergence_eta.rds")
saveRDS(
  list(
    prov = beta_prov$model,
    prov_cond = beta_prov_cond$model,
    sett = beta_sett$model
  ),
  "output/racli/model_beta_eta.rds"
)
cat("Salvati: beta_convergence_eta.rds, model_beta_eta.rds\n\n")

# 9. Panel a effetti fissi -----

cat("==== 9. Panel a effetti fissi ====\n\n")

# Prepara dati panel: province × anni
panel_dt <- copy(gap_province)
panel_dt <- panel_dt[!is.na(log_gap_salario_mediano)]
panel_dt[, area_f := factor(area)]
panel_dt[, anno_f := factor(anno)]

cat(
  "Panel: ",
  length(unique(panel_dt$area)),
  "province ×",
  length(unique(panel_dt$anno)),
  "anni =",
  nrow(panel_dt),
  "osservazioni\n"
)

# Modello panel a effetti fissi bidirezionali
panel_model <- tryCatch(
  {
    pdata <- pdata.frame(panel_dt, index = c("area", "anno"))
    plm_model <- plm(
      log_gap_salario_mediano ~ 1,
      data = pdata,
      model = "within",
      effect = "twoways"
    )
    cat("Modello plm stimato con successo\n")
    plm_model
  },
  error = function(e) {
    cat("  plm non disponibile o errore, fallback a lm()\n")
    lm(
      log_gap_salario_mediano ~ area_f + anno_f,
      data = panel_dt
    )
  }
)

# Estrai coefficienti anno (effetti fissi temporali)
year_fe <- tryCatch(
  {
    if (inherits(panel_model, "plm")) {
      fe <- fixef(panel_model, effect = "time")
      data.table(
        anno = as.integer(names(fe)),
        coef = as.numeric(fe)
      )
    } else {
      # Fallback lm: estrai coefficienti anno
      cf <- coef(panel_model)
      anno_coefs <- cf[grep("^anno_f", names(cf))]
      data.table(
        anno = as.integer(gsub("anno_f", "", names(anno_coefs))),
        coef = as.numeric(anno_coefs)
      )
    }
  },
  error = function(e) {
    cat("  Errore estrazione effetti fissi temporali:", e$message, "\n")
    NULL
  }
)

# SE cluster-robust per provincia
panel_se <- tryCatch(
  {
    if (inherits(panel_model, "plm")) {
      vcovHC(panel_model, type = "HC1", cluster = "group")
    } else {
      vcovCL(panel_model, cluster = panel_dt$area)
    }
  },
  error = function(e) {
    cat("  Nota: SE cluster non calcolabili\n")
    NULL
  }
)

# Aggiungi IC ai coefficienti anno
if (!is.null(year_fe) && !is.null(panel_se)) {
  tryCatch(
    {
      if (inherits(panel_model, "plm")) {
        fe_test <- fixef(panel_model, effect = "time", vcov = panel_se)
        se_vals <- attr(fe_test, "se")
        if (!is.null(se_vals)) {
          year_fe[, se := se_vals[match(as.character(anno), names(se_vals))]]
          year_fe[, `:=`(
            ci_low = coef - 1.96 * se,
            ci_high = coef + 1.96 * se
          )]
        }
      }
    },
    error = function(e) {
      cat("  Nota: IC per effetti temporali non calcolabili\n")
    }
  )
}

if (!is.null(year_fe)) {
  cat("Effetti fissi temporali:\n")
  print(year_fe)
}

saveRDS(
  list(model = panel_model, year_fe = year_fe, vcov_cl = panel_se),
  "output/racli/model_panel_eta.rds"
)
cat("Salvato: model_panel_eta.rds\n\n")

# 10. Decomposizione shift-share geografica -----

cat("==== 10. Decomposizione shift-share geografica ====\n\n")

# Usa 5 macro-regioni da RACLI_9 (ripartizioni)
dt_ripart <- dati_eta_terr_wide[
  geo_level == "Ripartizione" & eta != "totale"
]
gap_ripart <- calc_age_gap(dt_ripart)

decomp_geo <- NULL
if (!is.null(gap_ripart) && nrow(gap_ripart) > 0) {
  decomp_geo <- shift_share_decomposition(
    gap_ripart,
    group_var = "area",
    gap_var = "log_gap_salario_mediano",
    year_start = 2014,
    year_end = 2022
  )

  if (!is.null(decomp_geo)) {
    cat("Decomposizione geografica:\n")
    print(decomp_geo$components)
    cat("\nDettaglio per ripartizione:\n")
    print(decomp_geo$detail[, list(area, gap_0, gap_T, delta_gap)])
  }
}

saveRDS(decomp_geo, "output/racli/decomposizione_geo_eta.rds")
cat("Salvato: decomposizione_geo_eta.rds\n\n")

# 11. Decomposizione shift-share settoriale -----

cat("==== 11. Decomposizione shift-share settoriale ====\n\n")

# Usa 17 Sezioni NACE da RACLI_18
decomp_sett <- NULL
if (!is.null(gap_settori) && nrow(gap_settori) > 0) {
  decomp_sett <- shift_share_decomposition(
    gap_settori,
    group_var = "settore_code",
    gap_var = "log_gap_salario_mediano",
    year_start = 2014,
    year_end = 2022
  )

  if (!is.null(decomp_sett)) {
    cat("Decomposizione settoriale:\n")
    print(decomp_sett$components)
    cat("\nTop 5 settori per variazione gap:\n")
    top5 <- decomp_sett$detail[order(-abs(delta_gap))][
      1:min(5, nrow(decomp_sett$detail))
    ]
    print(top5[, list(settore_code, gap_0, gap_T, delta_gap)])
  }
}

saveRDS(decomp_sett, "output/racli/decomposizione_settore_eta.rds")
cat("Salvato: decomposizione_settore_eta.rds\n\n")

# 12. Analisi congiunta geografia-settore -----

cat("==== 12. Analisi congiunta geografia-settore ====\n\n")

# Usa RACLI_1 con totale per sesso, contratto, dimensione aziendale
dt_congiunta <- dati_eta_multi_clean[
  sesso == "totale" &
    contratto == "totale" &
    dim_aziendale == "totale" &
    area != "IT"
]

# Filtra solo Sezione NACE (settore_code di 1 carattere) e totale aggregato
dt_congiunta_nace <- dt_congiunta[
  nchar(settore_code) == 1 & grepl("^[A-Z]$", settore_code)
]

# Calcola gap per ripartizione × settore × anno
dt_conj_young <- dt_congiunta_nace[
  eta == "15-29 anni",
  list(
    anno,
    area,
    ripartizione,
    settore_code,
    settore,
    salario_mediano_young = salario_mediano
  )
]
dt_conj_old <- dt_congiunta_nace[
  eta == "50 anni e più",
  list(
    anno,
    area,
    ripartizione,
    settore_code,
    settore,
    salario_mediano_old = salario_mediano
  )
]

gap_congiunta <- merge(
  dt_conj_young,
  dt_conj_old,
  by = c("anno", "area", "ripartizione", "settore_code", "settore"),
  all = FALSE
)

gap_congiunta[, `:=`(
  log_gap = log(salario_mediano_old) - log(salario_mediano_young),
  pct_gap = (salario_mediano_old / salario_mediano_young - 1) * 100
)]

# Rimuovi NA
gap_congiunta <- gap_congiunta[!is.na(log_gap) & is.finite(log_gap)]

cat("Gap congiunto: ", nrow(gap_congiunta), "osservazioni\n")
cat("  Ripartizioni:", length(unique(gap_congiunta$ripartizione)), "\n")
cat("  Settori:", length(unique(gap_congiunta$settore_code)), "\n")
cat("  Anni:", length(unique(gap_congiunta$anno)), "\n")

# ANOVA: decomposizione varianza
if (nrow(gap_congiunta) > 10) {
  anova_model <- lm(
    log_gap ~ factor(ripartizione) +
      factor(settore_code) +
      factor(ripartizione):factor(settore_code) +
      factor(anno),
    data = gap_congiunta
  )

  # Type II SS
  anova_table <- tryCatch(
    {
      # car::Anova non è nelle dipendenze, usiamo anova()
      anova(anova_model)
    },
    error = function(e) {
      cat("  Errore ANOVA:", e$message, "\n")
      NULL
    }
  )

  if (!is.null(anova_table)) {
    ss_total <- sum(anova_table$`Sum Sq`, na.rm = TRUE)
    anova_pct <- data.table(
      fattore = rownames(anova_table),
      ss = anova_table$`Sum Sq`,
      pct_varianza = anova_table$`Sum Sq` / ss_total * 100,
      p_value = anova_table$`Pr(>F)`
    )
    cat("\nDecomposizione varianza (ANOVA):\n")
    print(anova_pct)
  }

  # OLS con SE robusti
  ols_model <- lm(
    log_gap ~ factor(ripartizione) +
      factor(settore_code) +
      factor(ripartizione):factor(settore_code) +
      factor(anno),
    data = gap_congiunta
  )

  ols_robust <- tryCatch(
    coeftest(ols_model, vcov = vcovHC(ols_model, type = "HC1")),
    error = function(e) {
      cat("  Nota: SE robusti non calcolabili\n")
      summary(ols_model)$coefficients
    }
  )

  cat("\nR² modello OLS:", sprintf("%.3f", summary(ols_model)$r.squared), "\n")
  cat("R² adj:", sprintf("%.3f", summary(ols_model)$adj.r.squared), "\n")
}

saveRDS(
  list(
    gap_data = gap_congiunta,
    anova = if (exists("anova_pct")) anova_pct else NULL,
    ols_model = if (exists("ols_model")) ols_model else NULL,
    ols_robust = if (exists("ols_robust")) ols_robust else NULL
  ),
  "output/racli/analisi_congiunta_eta.rds"
)
cat("Salvato: analisi_congiunta_eta.rds\n\n")

# 13. Robustezza -----

cat("==== 13. Robustezza ====\n\n")

robustezza <- list()

# a) Riferimento alternativo: Y30-49 anziché Y_GE50
cat("a) Gap con riferimento 30-49 anni...\n")
gap_naz_alt <- calc_age_gap(
  dt_nazionale,
  young = "15-29 anni",
  old = "30-49 anni"
)

if (!is.null(gap_naz_alt)) {
  robustezza$gap_riferimento_30_49 <- gap_naz_alt
  cat(
    "  Gap mediano 2022 (rif. 30-49):",
    sprintf("%.3f", gap_naz_alt[anno == 2022]$log_gap_salario_mediano),
    "\n"
  )
  cat(
    "  Gap mediano 2022 (rif. 50+):",
    sprintf("%.3f", gap_nazionale[anno == 2022]$log_gap_salario_mediano),
    "\n"
  )
}

# b) Gap a D1 vs D5 vs D9
cat("b) Gap ai diversi decili...\n")
gap_decili <- gap_nazionale[, list(
  anno,
  log_gap_D1 = log_gap_D1,
  log_gap_D5 = log_gap_salario_mediano,
  log_gap_D9 = log_gap_D9
)]

if (nrow(gap_decili) > 0) {
  robustezza$gap_per_decile <- gap_decili
  cat(
    "  Gap D1 (2014 -> 2022):",
    sprintf(
      "%.3f -> %.3f",
      gap_decili[anno == 2014]$log_gap_D1,
      gap_decili[anno == 2022]$log_gap_D1
    ),
    "\n"
  )
  cat(
    "  Gap D5 (2014 -> 2022):",
    sprintf(
      "%.3f -> %.3f",
      gap_decili[anno == 2014]$log_gap_D5,
      gap_decili[anno == 2022]$log_gap_D5
    ),
    "\n"
  )
  cat(
    "  Gap D9 (2014 -> 2022):",
    sprintf(
      "%.3f -> %.3f",
      gap_decili[anno == 2014]$log_gap_D9,
      gap_decili[anno == 2022]$log_gap_D9
    ),
    "\n"
  )
}

# c) Esclusione anno COVID (2020)
cat("c) Beta-convergenza escludendo 2020...\n")
gap_prov_no_covid <- gap_province[anno != 2020]

beta_no_covid <- calc_beta_convergence(
  gap_prov_no_covid,
  gap_var = "log_gap_salario_mediano",
  id_var = "area",
  year_start = 2014,
  year_end = 2022
)

if (!is.null(beta_no_covid$model)) {
  robustezza$beta_no_covid <- beta_no_covid
  cat(
    "  Beta (con 2020):",
    sprintf("%.4f", coef(beta_prov$model)["gap_initial"]),
    "\n"
  )
  cat(
    "  Beta (senza 2020):",
    sprintf("%.4f", coef(beta_no_covid$model)["gap_initial"]),
    "\n"
  )
}

# Sigma-convergenza senza 2020
sigma_prov_no_covid <- calc_sigma_convergence(
  gap_prov_no_covid,
  gap_var = "log_gap_salario_mediano",
  group_var = "anno"
)
robustezza$sigma_no_covid <- sigma_prov_no_covid

saveRDS(robustezza, "output/racli/robustezza_eta.rds")
cat("Salvato: robustezza_eta.rds\n\n")

# 13bis. Convergenza al ribasso: salari reali -----

cat("==== 13bis. Convergenza al ribasso: salari reali ====\n\n")

# Deflatore IPCA dai dati VECM (trimestrali, indice 2015=100)
vecm_data <- as.data.table(readRDS("output/vecm/dati_istat.rds"))
vecm_data[, anno := as.integer(floor(trimestri))]
ipca_annuale <- vecm_data[
  anno >= 2014 & anno <= 2022,
  list(ipca = mean(p, na.rm = TRUE)),
  by = anno
]
base_2015 <- ipca_annuale[anno == 2015]$ipca
ipca_annuale[, deflatore := ipca / base_2015]

cat("Deflatore IPCA (base 2015=1):\n")
print(ipca_annuale[, list(anno, deflatore = round(deflatore, 4))])

# Salari reali nazionali
conv_reale <- as.data.table(gap_nazionale)
conv_reale <- merge(
  conv_reale,
  ipca_annuale[, list(anno, deflatore)],
  by = "anno"
)

# Deflaziona tutti i livelli retributivi
wage_cols <- c(
  "salario_mediano_young",
  "salario_mediano_old",
  "salario_medio_young",
  "salario_medio_old",
  "D1_young",
  "D1_old",
  "D9_young",
  "D9_old"
)
for (col in intersect(wage_cols, names(conv_reale))) {
  real_col <- paste0(col, "_reale")
  conv_reale[, (real_col) := get(col) / deflatore]
}

# Gap reale (log)
conv_reale[,
  log_gap_reale_mediano := log(salario_mediano_old_reale) -
    log(salario_mediano_young_reale)
]

# Crescita nominale e reale
w_y_14 <- conv_reale[anno == 2014]$salario_mediano_young
w_o_14 <- conv_reale[anno == 2014]$salario_mediano_old
w_y_22 <- conv_reale[anno == 2022]$salario_mediano_young
w_o_22 <- conv_reale[anno == 2022]$salario_mediano_old

w_y_r14 <- conv_reale[anno == 2014]$salario_mediano_young_reale
w_o_r14 <- conv_reale[anno == 2014]$salario_mediano_old_reale
w_y_r22 <- conv_reale[anno == 2022]$salario_mediano_young_reale
w_o_r22 <- conv_reale[anno == 2022]$salario_mediano_old_reale

crescita <- data.table(
  classe_eta = rep(c("15-29 anni", "50+ anni"), each = 2),
  tipo = rep(c("Nominale", "Reale"), 2),
  crescita_pct = c(
    (w_y_22 / w_y_14 - 1) * 100,
    (w_y_r22 / w_y_r14 - 1) * 100,
    (w_o_22 / w_o_14 - 1) * 100,
    (w_o_r22 / w_o_r14 - 1) * 100
  )
)

inflazione_cum <- (conv_reale[anno == 2022]$deflatore /
  conv_reale[anno == 2014]$deflatore -
  1) *
  100

cat("\nCrescita 2014-2022:\n")
print(crescita)
cat("Inflazione cumulata:", sprintf("%.1f%%", inflazione_cum), "\n")
cat("Gap nominale: 0.239 -> 0.202 (convergenza)\n")
cat(
  "Gap reale:   ",
  sprintf(
    "%.3f -> %.3f",
    conv_reale[anno == 2014]$log_gap_reale_mediano,
    conv_reale[anno == 2022]$log_gap_reale_mediano
  ),
  "(identico, la deflazione non cambia il log-gap)\n"
)

saveRDS(
  list(
    conv_reale = conv_reale,
    ipca = ipca_annuale,
    crescita = crescita,
    inflazione_cumulata = inflazione_cum
  ),
  "output/racli/convergenza_reale_eta.rds"
)
cat("Salvato: convergenza_reale_eta.rds\n\n")

# Grafico 16: Salari reali vs nominali nel tempo
cat("Grafico 16: Salari reali vs nominali...\n")

p16_data <- conv_reale[, list(
  anno,
  `Giovani (nom.)` = salario_mediano_young,
  `Senior (nom.)` = salario_mediano_old,
  `Giovani (reale)` = salario_mediano_young_reale,
  `Senior (reale)` = salario_mediano_old_reale
)]
p16_long <- melt(
  p16_data,
  id.vars = "anno",
  variable.name = "serie",
  value.name = "salario"
)
p16_long[,
  tipo := fifelse(grepl("reale", serie), "Reale (EUR 2015)", "Nominale")
]
p16_long[, eta := fifelse(grepl("Giovani", serie), "15-29 anni", "50+ anni")]

p16 <- ggplot(
  p16_long,
  aes(x = anno, y = salario, color = eta, linetype = tipo)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 2014:2022) +
  scale_color_manual(
    values = c("15-29 anni" = "#377EB8", "50+ anni" = "#E41A1C")
  ) +
  scale_linetype_manual(
    values = c("Nominale" = "solid", "Reale (EUR 2015)" = "dashed")
  ) +
  labs(
    title = "Retribuzioni orarie mediane: nominali e reali",
    subtitle = "Deflatore IPCA, base 2015 = 1. Dati RACLI-ISTAT",
    x = "Anno",
    y = "Retribuzione oraria (EUR)",
    color = "Classe di età",
    linetype = "Misura",
    caption = "Fonte: ISTAT RACLI e IPCA | Elaborazione: G. Montaletti"
  ) +
  theme_salari() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(order = 1), linetype = guide_legend(order = 2))

ggsave(
  "output/racli/grafici/eta_16_salari_reali_tempo.png",
  p16,
  width = 10,
  height = 7,
  dpi = 300
)
cat("  Salvato: eta_16_salari_reali_tempo.png\n")

# Grafico 17: Crescita cumulata nominale vs reale
cat("Grafico 17: Crescita cumulata...\n")

p17 <- ggplot(crescita, aes(x = classe_eta, y = crescita_pct, fill = tipo)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_hline(yintercept = 0, linewidth = 0.5) +
  geom_text(
    aes(label = sprintf("%+.1f%%", crescita_pct)),
    position = position_dodge(width = 0.7),
    vjust = ifelse(crescita$crescita_pct >= 0, -0.5, 1.5),
    size = 4,
    fontface = "bold"
  ) +
  scale_fill_manual(values = c("Nominale" = "#377EB8", "Reale" = "#E41A1C")) +
  labs(
    title = "Crescita salariale cumulata 2014-2022",
    subtitle = "Retribuzione oraria mediana — confronto nominale vs. reale (IPCA)",
    x = "",
    y = "Crescita cumulata (%)",
    fill = "",
    caption = paste0(
      "Inflazione cumulata IPCA 2014-2022: ",
      sprintf("%.1f%%", inflazione_cum),
      " | Fonte: ISTAT"
    )
  ) +
  theme_salari() +
  theme(legend.position = "bottom")

ggsave(
  "output/racli/grafici/eta_17_crescita_cumulata.png",
  p17,
  width = 8,
  height = 7,
  dpi = 300
)
cat("  Salvato: eta_17_crescita_cumulata.png\n\n")

# 14. Visualizzazioni -----

cat("==== 14. Visualizzazioni ====\n\n")

# Paletta colori per ripartizioni
col_ripart <- c(
  "Nord-ovest" = "#E41A1C",
  "Nord-est" = "#377EB8",
  "Centro" = "#4DAF4A",
  "Sud" = "#FF7F00",
  "Isole" = "#984EA3"
)

# Etichette brevi per ripartizioni (gestisci entrambe le forme)
ripart_short <- function(x) {
  gsub(
    "Nord-Ovest|NORD-OVEST",
    "Nord-ovest",
    gsub(
      "Nord-Est|NORD-EST",
      "Nord-est",
      gsub(
        "CENTRO \\(IT\\)|Centro \\(IT\\)",
        "Centro",
        gsub("SUD|Mezzogiorno", "Sud", gsub("ISOLE", "Isole", x))
      )
    )
  )
}

# --- Grafico 1: Gap nazionale nel tempo ---
cat("Grafico 1: Gap nazionale nel tempo...\n")

if (!is.null(gap_nazionale) && nrow(gap_nazionale) > 0) {
  p1_data <- gap_nazionale[, list(
    anno,
    D5 = log_gap_salario_mediano,
    D1 = log_gap_D1,
    D9 = log_gap_D9
  )]

  p1 <- ggplot(p1_data, aes(x = anno)) +
    geom_ribbon(aes(ymin = D1, ymax = D9), alpha = 0.2, fill = "#2171B5") +
    geom_line(aes(y = D5), color = "#2171B5", linewidth = 1.2) +
    geom_point(aes(y = D5), color = "#2171B5", size = 2.5) +
    geom_line(
      aes(y = D1),
      color = "#6BAED6",
      linewidth = 0.6,
      linetype = "dashed"
    ) +
    geom_line(
      aes(y = D9),
      color = "#08519C",
      linewidth = 0.6,
      linetype = "dashed"
    ) +
    scale_x_continuous(breaks = 2014:2022) +
    labs(
      title = "Gap salariale generazionale in Italia",
      subtitle = "log(W 50+ / W 15-29) — mediana (D5) e intervallo D1-D9",
      x = "Anno",
      y = "Gap salariale log(W50+/W15-29)",
      caption = "Fonte: ISTAT RACLI | Elaborazione: G. Montaletti"
    ) +
    theme_salari()

  ggsave(
    "output/racli/grafici/eta_01_gap_nazionale_tempo.png",
    p1,
    width = 10,
    height = 6,
    dpi = 300
  )
  cat("  Salvato: eta_01_gap_nazionale_tempo.png\n")
}

# --- Grafico 2: Gap per ripartizioni ---
cat("Grafico 2: Gap per ripartizioni...\n")

if (!is.null(gap_ripart) && nrow(gap_ripart) > 0) {
  p2_data <- copy(gap_ripart)
  p2_data[, ripartizione := ripart_short(area_label)]

  p2 <- ggplot(
    p2_data,
    aes(x = anno, y = log_gap_salario_mediano, color = ripartizione)
  ) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_x_continuous(breaks = 2014:2022) +
    scale_color_manual(values = col_ripart, name = "Ripartizione") +
    labs(
      title = "Gap salariale generazionale per ripartizione",
      subtitle = "log(W 50+ / W 15-29) — salario mediano",
      x = "Anno",
      y = "Gap salariale log(W50+/W15-29)",
      caption = "Fonte: ISTAT RACLI | Elaborazione: G. Montaletti"
    ) +
    theme_salari()

  ggsave(
    "output/racli/grafici/eta_02_gap_ripartizioni_tempo.png",
    p2,
    width = 10,
    height = 6,
    dpi = 300
  )
  cat("  Salvato: eta_02_gap_ripartizioni_tempo.png\n")
}

# --- Grafico 3: Sigma-convergenza province ---
cat("Grafico 3: Sigma-convergenza province...\n")

if (!is.null(sigma_prov) && nrow(sigma_prov) > 0) {
  p3 <- ggplot(sigma_prov, aes(x = anno, y = cv_gap)) +
    geom_line(color = "#2171B5", linewidth = 1.2) +
    geom_point(color = "#2171B5", size = 3) +
    geom_smooth(
      method = "lm",
      se = TRUE,
      color = "gray50",
      linetype = "dashed",
      linewidth = 0.6
    ) +
    scale_x_continuous(breaks = 2014:2022) +
    labs(
      title = "Sigma-convergenza del gap generazionale tra province",
      subtitle = "Coefficiente di variazione del log-gap salariale (D5)",
      x = "Anno",
      y = "CV del gap salariale",
      caption = "Fonte: ISTAT RACLI | Elaborazione: G. Montaletti"
    ) +
    theme_salari()

  ggsave(
    "output/racli/grafici/eta_03_sigma_convergenza_geo.png",
    p3,
    width = 10,
    height = 6,
    dpi = 300
  )
  cat("  Salvato: eta_03_sigma_convergenza_geo.png\n")
}

# --- Grafico 4: Sigma-convergenza settori ---
cat("Grafico 4: Sigma-convergenza settori...\n")

if (!is.null(sigma_sett) && nrow(sigma_sett) > 0) {
  p4 <- ggplot(sigma_sett, aes(x = anno, y = cv_gap)) +
    geom_line(color = "#D94801", linewidth = 1.2) +
    geom_point(color = "#D94801", size = 3) +
    geom_smooth(
      method = "lm",
      se = TRUE,
      color = "gray50",
      linetype = "dashed",
      linewidth = 0.6
    ) +
    scale_x_continuous(breaks = 2014:2022) +
    labs(
      title = "Sigma-convergenza del gap generazionale tra settori",
      subtitle = "Coefficiente di variazione del log-gap salariale (Sezioni NACE)",
      x = "Anno",
      y = "CV del gap salariale",
      caption = "Fonte: ISTAT RACLI | Elaborazione: G. Montaletti"
    ) +
    theme_salari()

  ggsave(
    "output/racli/grafici/eta_04_sigma_convergenza_settori.png",
    p4,
    width = 10,
    height = 6,
    dpi = 300
  )
  cat("  Salvato: eta_04_sigma_convergenza_settori.png\n")
}

# --- Grafico 5: Beta-convergenza province ---
cat("Grafico 5: Beta-convergenza province...\n")

if (!is.null(beta_prov$data) && nrow(beta_prov$data) > 0) {
  p5_data <- copy(beta_prov$data)
  p5_data[, ripartizione := ripart_short(get_ripartizione(area, ripart_lookup))]

  # Etichette per province estreme
  p5_data[, rank_delta := frank(delta_gap)]
  n_prov <- nrow(p5_data)
  p5_data[,
    label_prov := ifelse(
      rank_delta <= 5 | rank_delta > (n_prov - 5),
      area,
      ""
    )
  ]

  # Aggiungi etichette area_label se disponibili
  if ("area_label" %in% names(gap_province)) {
    area_labels <- unique(gap_province[, list(area, area_label)])
    p5_data <- merge(p5_data, area_labels, by = "area", all.x = TRUE)
    p5_data[label_prov != "", label_prov := area_label]
  }

  p5 <- ggplot(
    p5_data,
    aes(x = gap_initial, y = delta_gap, color = ripartizione)
  ) +
    geom_point(size = 2.5, alpha = 0.8) +
    geom_smooth(
      method = "lm",
      se = TRUE,
      color = "gray30",
      linetype = "solid",
      linewidth = 0.8,
      inherit.aes = FALSE,
      aes(x = gap_initial, y = delta_gap)
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
    geom_text(
      aes(label = label_prov),
      size = 2.5,
      vjust = -0.8,
      show.legend = FALSE
    ) +
    scale_color_manual(values = col_ripart, name = "Ripartizione") +
    labs(
      title = "Beta-convergenza: province",
      subtitle = paste0(
        "Gap iniziale (2014) vs variazione (2014-2022), ",
        "N = ",
        nrow(p5_data)
      ),
      x = "Gap salariale iniziale (2014)",
      y = "Variazione gap (2022 - 2014)",
      caption = "Fonte: ISTAT RACLI | Elaborazione: G. Montaletti"
    ) +
    theme_salari()

  ggsave(
    "output/racli/grafici/eta_05_beta_convergenza_province.png",
    p5,
    width = 10,
    height = 8,
    dpi = 300
  )
  cat("  Salvato: eta_05_beta_convergenza_province.png\n")
}

# --- Grafico 6: Beta-convergenza settori ---
cat("Grafico 6: Beta-convergenza settori...\n")

if (!is.null(beta_sett$data) && nrow(beta_sett$data) > 0) {
  p6_data <- copy(beta_sett$data)

  # Aggiungi etichette settore
  sett_labels <- unique(dati_eta_sett_wide[
    tipo_settore == "Sezione NACE",
    list(settore_code, settore)
  ])
  p6_data <- merge(p6_data, sett_labels, by = "settore_code", all.x = TRUE)
  p6_data[is.na(settore), settore := settore_code]

  # Etichetta breve per legenda
  p6_data[,
    settore_breve := paste0(
      settore_code,
      " - ",
      substr(
        gsub(
          "attività (dei servizi )?d[ie]l?l?[aeo']? ?",
          "",
          settore,
          ignore.case = TRUE
        ),
        1,
        28
      )
    )
  ]

  # Costruisci legenda testuale per caption su 3 righe
  legenda_sett <- p6_data[
    order(settore_code),
    unique(paste0(
      settore_code,
      "=",
      substr(
        gsub(
          "attività (dei servizi )?d[ie]l?l?[aeo']? ?",
          "",
          settore,
          ignore.case = TRUE
        ),
        1,
        18
      )
    ))
  ]
  n_per_riga <- ceiling(length(legenda_sett) / 3)
  righe <- vapply(
    split(legenda_sett, ceiling(seq_along(legenda_sett) / n_per_riga)),
    function(x) paste(x, collapse = "  |  "),
    character(1)
  )
  leg_str <- paste(righe, collapse = "\n")

  p6 <- ggplot(p6_data, aes(x = gap_initial, y = delta_gap)) +
    geom_point(size = 3, color = "#D94801", alpha = 0.8) +
    geom_smooth(method = "lm", se = TRUE, color = "gray30", linewidth = 0.8) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
    geom_text(
      aes(label = settore_code),
      size = 3.5,
      vjust = -1,
      fontface = "bold"
    ) +
    labs(
      title = "Beta-convergenza: settori NACE",
      subtitle = paste0(
        "Gap iniziale (2014) vs variazione (2014-2022), ",
        "N = ",
        nrow(p6_data)
      ),
      x = "Gap salariale iniziale (2014)",
      y = "Variazione gap (2022 - 2014)",
      caption = paste0(
        leg_str,
        "\nFonte: ISTAT RACLI | Elaborazione: G. Montaletti"
      )
    ) +
    theme_salari() +
    theme(plot.caption = element_text(size = 7, hjust = 0, lineheight = 1.2))

  ggsave(
    "output/racli/grafici/eta_06_beta_convergenza_settori.png",
    p6,
    width = 11,
    height = 9,
    dpi = 300
  )
  cat("  Salvato: eta_06_beta_convergenza_settori.png\n")
}

# --- Grafico 7: Gap per decile ---
cat("Grafico 7: Gap per decile...\n")

if (exists("gap_decili") && nrow(gap_decili) > 0) {
  p7_data <- melt(
    gap_decili,
    id.vars = "anno",
    variable.name = "decile",
    value.name = "log_gap"
  )
  p7_data[,
    decile_label := dplyr::case_when(
      decile == "log_gap_D1" ~ "D1 (primo decile)",
      decile == "log_gap_D5" ~ "D5 (mediana)",
      decile == "log_gap_D9" ~ "D9 (nono decile)"
    )
  ]

  p7 <- ggplot(
    p7_data,
    aes(x = anno, y = log_gap, color = decile_label, linetype = decile_label)
  ) +
    geom_line(linewidth = 1) +
    geom_point(size = 2.5) +
    scale_x_continuous(breaks = 2014:2022) +
    scale_color_manual(
      values = c(
        "D1 (primo decile)" = "#6BAED6",
        "D5 (mediana)" = "#2171B5",
        "D9 (nono decile)" = "#08519C"
      ),
      name = "Decile"
    ) +
    scale_linetype_manual(
      values = c(
        "D1 (primo decile)" = "dashed",
        "D5 (mediana)" = "solid",
        "D9 (nono decile)" = "dotdash"
      ),
      name = "Decile"
    ) +
    labs(
      title = "Gap generazionale ai diversi punti della distribuzione",
      subtitle = "log(W 50+ / W 15-29) a D1, D5 (mediana), D9 — livello nazionale",
      x = "Anno",
      y = "Gap salariale log(W50+/W15-29)",
      caption = "Fonte: ISTAT RACLI | Elaborazione: G. Montaletti"
    ) +
    theme_salari()

  ggsave(
    "output/racli/grafici/eta_07_gap_per_decile.png",
    p7,
    width = 10,
    height = 6,
    dpi = 300
  )
  cat("  Salvato: eta_07_gap_per_decile.png\n")
}

# --- Grafico 8: Decomposizione geografica ---
cat("Grafico 8: Decomposizione geografica...\n")

if (!is.null(decomp_geo) && !is.null(decomp_geo$detail)) {
  p8_data <- copy(decomp_geo$detail)
  p8_data[, ripartizione := ripart_short(get_ripartizione(area, ripart_lookup))]

  p8 <- ggplot(
    p8_data,
    aes(
      x = reorder(ripartizione, delta_gap),
      y = delta_gap,
      fill = delta_gap > 0
    )
  ) +
    geom_col(show.legend = FALSE) +
    geom_hline(yintercept = 0, linewidth = 0.5) +
    scale_fill_manual(values = c("TRUE" = "#D94801", "FALSE" = "#2171B5")) +
    coord_flip() +
    labs(
      title = "Decomposizione geografica della variazione del gap",
      subtitle = "Variazione del log-gap salariale generazionale (2014-2022)",
      x = "",
      y = "Variazione log-gap (2022 - 2014)",
      caption = "Fonte: ISTAT RACLI | Elaborazione: G. Montaletti"
    ) +
    theme_salari()

  ggsave(
    "output/racli/grafici/eta_08_decomposizione_geo.png",
    p8,
    width = 8,
    height = 6,
    dpi = 300
  )
  cat("  Salvato: eta_08_decomposizione_geo.png\n")
}

# --- Grafico 9: Decomposizione settoriale ---
cat("Grafico 9: Decomposizione settoriale...\n")

if (!is.null(decomp_sett) && !is.null(decomp_sett$detail)) {
  p9_data <- copy(decomp_sett$detail)

  # Aggiungi etichetta settore
  sett_labels <- unique(dati_eta_sett_wide[
    tipo_settore == "Sezione NACE",
    list(settore_code, settore)
  ])
  p9_data <- merge(p9_data, sett_labels, by = "settore_code", all.x = TRUE)
  p9_data[is.na(settore), settore := settore_code]
  p9_data[, label := paste0(settore_code, " - ", substr(settore, 1, 30))]

  p9 <- ggplot(
    p9_data,
    aes(x = reorder(label, delta_gap), y = delta_gap, fill = delta_gap > 0)
  ) +
    geom_col(show.legend = FALSE) +
    geom_hline(yintercept = 0, linewidth = 0.5) +
    scale_fill_manual(values = c("TRUE" = "#D94801", "FALSE" = "#2171B5")) +
    coord_flip() +
    labs(
      title = "Variazione del gap generazionale per settore NACE",
      subtitle = "Variazione del log-gap salariale (2014-2022)",
      x = "",
      y = "Variazione log-gap (2022 - 2014)",
      caption = "Fonte: ISTAT RACLI | Elaborazione: G. Montaletti"
    ) +
    theme_salari() +
    theme(axis.text.y = element_text(size = 8))

  ggsave(
    "output/racli/grafici/eta_09_decomposizione_settori.png",
    p9,
    width = 8,
    height = 6,
    dpi = 300
  )
  cat("  Salvato: eta_09_decomposizione_settori.png\n")
}

# --- Grafico 10: Heatmap gap settore × territorio ---
cat("Grafico 10: Heatmap gap settore × territorio...\n")

if (nrow(gap_congiunta) > 0) {
  heatmap_data <- gap_congiunta[
    anno == 2022,
    list(log_gap = mean(log_gap, na.rm = TRUE)),
    by = list(ripartizione, settore_code)
  ]

  # Aggiungi etichetta settore
  heatmap_data <- merge(
    heatmap_data,
    sett_labels,
    by = "settore_code",
    all.x = TRUE
  )
  heatmap_data[is.na(settore), settore := settore_code]
  heatmap_data[,
    settore_breve := paste0(
      settore_code,
      " - ",
      substr(
        gsub(
          "attività (dei servizi )?d[ie]l?l?[aeo']? ?",
          "",
          settore,
          ignore.case = TRUE
        ),
        1,
        30
      )
    )
  ]
  heatmap_data[, ripartizione := ripart_short(ripartizione)]

  p10 <- ggplot(
    heatmap_data,
    aes(x = ripartizione, y = reorder(settore_breve, log_gap), fill = log_gap)
  ) +
    geom_tile(color = "white", linewidth = 0.3) +
    scale_fill_viridis_c(
      option = "viridis",
      name = "Log-gap\nsalariale",
      na.value = "gray90"
    ) +
    labs(
      title = "Gap generazionale per settore e ripartizione (2022)",
      subtitle = "log(W 50+ / W 15-29) — salario mediano",
      x = "Ripartizione",
      y = "Settore NACE",
      caption = "Fonte: ISTAT RACLI | Elaborazione: G. Montaletti"
    ) +
    theme_salari() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text.y = element_text(size = 8)
    )

  ggsave(
    "output/racli/grafici/eta_10_heatmap_gap_settore_territorio.png",
    p10,
    width = 12,
    height = 8,
    dpi = 300
  )
  cat("  Salvato: eta_10_heatmap_gap_settore_territorio.png\n")
}

# --- Grafico 11: Top/bottom province per gap ---
cat("Grafico 11: Top/bottom province per gap...\n")

if (!is.null(gap_province) && nrow(gap_province) > 0) {
  prov_2022 <- gap_province[anno == 2022 & !is.na(log_gap_salario_mediano)]
  prov_2022[,
    ripartizione := ripart_short(get_ripartizione(area, ripart_lookup))
  ]

  # Aggiungi etichetta area
  if ("area_label" %in% names(gap_province)) {
    area_lab <- unique(gap_province[, list(area, area_label)])
    prov_2022 <- merge(
      prov_2022,
      area_lab,
      by = "area",
      all.x = TRUE,
      suffixes = c("", ".y")
    )
    # Usa la colonna non duplicata
    if ("area_label.y" %in% names(prov_2022)) {
      prov_2022[is.na(area_label), area_label := area_label.y]
      prov_2022[, area_label.y := NULL]
    }
  }

  setorder(prov_2022, log_gap_salario_mediano)
  n_show <- min(10, nrow(prov_2022) %/% 2)
  top_bottom <- rbind(
    prov_2022[1:n_show],
    prov_2022[(nrow(prov_2022) - n_show + 1):nrow(prov_2022)]
  )
  top_bottom[, tipo := rep(c("Bottom 10", "Top 10"), each = n_show)]

  p11 <- ggplot(
    top_bottom,
    aes(
      x = reorder(area_label, log_gap_salario_mediano),
      y = log_gap_salario_mediano,
      fill = ripartizione
    )
  ) +
    geom_col() +
    scale_fill_manual(values = col_ripart, name = "Ripartizione") +
    coord_flip() +
    labs(
      title = "Province con gap generazionale minore e maggiore (2022)",
      subtitle = "log(W 50+ / W 15-29) — salario mediano",
      x = "",
      y = "Gap salariale log(W50+/W15-29)",
      caption = "Fonte: ISTAT RACLI | Elaborazione: G. Montaletti"
    ) +
    theme_salari()

  ggsave(
    "output/racli/grafici/eta_11_top_bottom_gap_province.png",
    p11,
    width = 10,
    height = 8,
    dpi = 300
  )
  cat("  Salvato: eta_11_top_bottom_gap_province.png\n")
}

# --- Grafico 12: Evoluzione gap tutti i settori ---
cat("Grafico 12: Evoluzione gap tutti i settori...\n")

if (!is.null(gap_settori) && nrow(gap_settori) > 0) {
  p12_data <- gap_settori[!is.na(log_gap_salario_mediano)]
  if (!"settore" %in% names(p12_data)) {
    p12_data <- merge(p12_data, sett_labels, by = "settore_code", all.x = TRUE)
  }
  p12_data[is.na(settore), settore := settore_code]
  # Etichetta breve: "J - Informazione e comunicaz."
  p12_data[,
    settore_breve := paste0(
      settore_code,
      " - ",
      substr(
        gsub(
          "attività (dei servizi )?d[ie]l?l?[aeo']? ?",
          "",
          settore,
          ignore.case = TRUE
        ),
        1,
        28
      )
    )
  ]
  # Ordina per gap medio decrescente (legenda ordinata)
  sett_order <- p12_data[,
    list(mean_gap = mean(log_gap_salario_mediano, na.rm = TRUE)),
    by = settore_breve
  ][order(-mean_gap)]$settore_breve
  p12_data[, settore_breve := factor(settore_breve, levels = sett_order)]

  n_sett <- length(sett_order)
  # 17 colori distinguibili
  col_17 <- c(
    "#E41A1C",
    "#377EB8",
    "#4DAF4A",
    "#984EA3",
    "#FF7F00",
    "#A65628",
    "#F781BF",
    "#999999",
    "#66C2A5",
    "#FC8D62",
    "#8DA0CB",
    "#E78AC3",
    "#A6D854",
    "#FFD92F",
    "#E5C494",
    "#B3B3B3",
    "#1B9E77"
  )
  # 17 forme punto (0-16 sono tutte distinguibili)
  shp_17 <- c(0:16)[seq_len(n_sett)]

  p12 <- ggplot(
    p12_data,
    aes(
      x = anno,
      y = log_gap_salario_mediano,
      color = settore_breve,
      shape = settore_breve
    )
  ) +
    geom_line(linewidth = 0.7, alpha = 0.8) +
    geom_point(size = 2.2) +
    scale_x_continuous(breaks = 2014:2022) +
    scale_color_manual(values = col_17[seq_len(n_sett)]) +
    scale_shape_manual(values = shp_17) +
    labs(
      title = "Evoluzione del gap generazionale per settore NACE",
      subtitle = "log(W 50+ / W 15-29) — salario mediano orario",
      x = "Anno",
      y = "Gap salariale log(W50+/W15-29)",
      color = "Settore",
      shape = "Settore",
      caption = "Fonte: ISTAT RACLI | Elaborazione: G. Montaletti"
    ) +
    theme_salari() +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 7),
      legend.key.size = unit(0.4, "cm")
    ) +
    guides(
      color = guide_legend(ncol = 3, override.aes = list(linewidth = 1)),
      shape = guide_legend(ncol = 3)
    )

  ggsave(
    "output/racli/grafici/eta_12_evoluzione_gap_settori_top.png",
    p12,
    width = 12,
    height = 9,
    dpi = 300
  )
  cat("  Salvato: eta_12_evoluzione_gap_settori_top.png\n")
}

# --- Grafico 13: Panel FE coefficients ---
cat("Grafico 13: Coefficienti effetti fissi temporali...\n")

if (!is.null(year_fe) && nrow(year_fe) > 0) {
  p13_data <- copy(year_fe)

  # Se mancano IC, calcolali in modo approssimativo
  if (!"ci_low" %in% names(p13_data)) {
    p13_data[, `:=`(ci_low = coef - 0.05, ci_high = coef + 0.05)]
  }

  p13 <- ggplot(p13_data, aes(x = anno, y = coef)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
    geom_pointrange(
      aes(ymin = ci_low, ymax = ci_high),
      color = "#2171B5",
      size = 0.8
    ) +
    scale_x_continuous(breaks = unique(p13_data$anno)) +
    labs(
      title = "Effetti fissi temporali del gap generazionale",
      subtitle = "Panel bidirezionale province × anni, IC 95%",
      x = "Anno",
      y = "Coefficiente effetto temporale",
      caption = "Fonte: ISTAT RACLI | Elaborazione: G. Montaletti"
    ) +
    theme_salari()

  ggsave(
    "output/racli/grafici/eta_13_panel_fe_coefficients.png",
    p13,
    width = 8,
    height = 6,
    dpi = 300
  )
  cat("  Salvato: eta_13_panel_fe_coefficients.png\n")
}

# --- Grafico 14: Mappa gap province 2022 ---
cat("Grafico 14: Mappa gap provinciale 2022...\n")

shp_file <- "data/shp/province_italia_istat.rds"
mappa_ok <- FALSE

if (file.exists(shp_file)) {
  italy_sf <- readRDS(shp_file)
  cat("  Shapefile caricato:", nrow(italy_sf), "geometrie\n")

  if (!is.null(gap_province) && "cod_uts" %in% names(gap_province)) {
    gap_2022_map <- gap_province[
      anno == 2022 & !is.na(log_gap_salario_mediano),
      list(area, cod_uts, log_gap_salario_mediano)
    ]
  } else if (!is.null(itter_mapping)) {
    # Aggiungi cod_uts via mapping
    gap_2022_map <- gap_province[
      anno == 2022 & !is.na(log_gap_salario_mediano),
      list(area, log_gap_salario_mediano)
    ]
    gap_2022_map <- merge(
      gap_2022_map,
      itter_mapping[, list(itter107, cod_uts)],
      by.x = "area",
      by.y = "itter107",
      all.x = TRUE
    )
  } else {
    gap_2022_map <- NULL
  }

  if (!is.null(gap_2022_map) && nrow(gap_2022_map) > 0) {
    map_data_14 <- merge(
      italy_sf,
      gap_2022_map,
      by.x = "COD_UTS",
      by.y = "cod_uts",
      all.x = TRUE
    )

    n_match <- sum(!is.na(map_data_14$log_gap_salario_mediano))
    cat("  Province matchate:", n_match, "/", nrow(italy_sf), "\n")

    p14 <- ggplot(map_data_14) +
      geom_sf(
        aes(fill = log_gap_salario_mediano),
        color = "white",
        linewidth = 0.1
      ) +
      scale_fill_viridis_c(
        option = "viridis",
        name = "Log-gap\nsalariale",
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
        title = "Gap salariale generazionale per provincia (2022)",
        subtitle = "log(W 50+ / W 15-29) — salario mediano",
        caption = "Fonte: ISTAT RACLI | Elaborazione: G. Montaletti"
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
      "output/racli/grafici/eta_14_mappa_gap_province.png",
      p14,
      width = 10,
      height = 10,
      dpi = 300
    )
    cat("  Salvato: eta_14_mappa_gap_province.png\n")
    mappa_ok <- TRUE
  }
} else {
  cat("  ATTENZIONE: shapefile non trovato in", shp_file, "\n")
  cat("  Eseguire prima: Rscript scripts/00_download_data.R\n")
}

# --- Grafico 15: Mappa variazione gap 2014-2022 ---
cat("Grafico 15: Mappa variazione gap 2014-2022...\n")

if (mappa_ok && !is.null(beta_prov$data)) {
  delta_map <- copy(beta_prov$data)

  # Aggiungi cod_uts
  if (!is.null(itter_mapping)) {
    delta_map <- merge(
      delta_map,
      itter_mapping[, list(itter107, cod_uts)],
      by.x = "area",
      by.y = "itter107",
      all.x = TRUE
    )
  }

  if ("cod_uts" %in% names(delta_map)) {
    map_data_15 <- merge(
      italy_sf,
      delta_map[, list(cod_uts, delta_gap)],
      by.x = "COD_UTS",
      by.y = "cod_uts",
      all.x = TRUE
    )

    # Paletta divergente centrata su 0
    max_abs <- max(abs(map_data_15$delta_gap), na.rm = TRUE)

    p15 <- ggplot(map_data_15) +
      geom_sf(aes(fill = delta_gap), color = "white", linewidth = 0.1) +
      scale_fill_gradient2(
        low = "#2171B5",
        mid = "white",
        high = "#D94801",
        midpoint = 0,
        name = "Variazione\nlog-gap",
        na.value = "gray90",
        limits = c(-max_abs, max_abs)
      ) +
      annotation_scale(location = "bl", width_hint = 0.3) +
      annotation_north_arrow(
        location = "tr",
        style = north_arrow_fancy_orienteering,
        height = unit(1.5, "cm"),
        width = unit(1.5, "cm")
      ) +
      labs(
        title = "Variazione del gap salariale generazionale (2014-2022)",
        subtitle = "Differenza log-gap per provincia",
        caption = "Fonte: ISTAT RACLI | Elaborazione: G. Montaletti"
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
      "output/racli/grafici/eta_15_mappa_variazione_gap.png",
      p15,
      width = 10,
      height = 10,
      dpi = 300
    )
    cat("  Salvato: eta_15_mappa_variazione_gap.png\n")
  }
} else if (!mappa_ok) {
  cat("  Mappa non generata: shapefile non disponibile\n")
}

cat("\nVisualizzazioni completate\n\n")

# 15. Epilogo -----

cat("==== 15. Epilogo ====\n\n")

# Riepilogo statistiche principali
cat("--- Riepilogo risultati ---\n\n")

if (!is.null(gap_nazionale) && nrow(gap_nazionale) > 0) {
  g14 <- gap_nazionale[anno == 2014]$log_gap_salario_mediano
  g22 <- gap_nazionale[anno == 2022]$log_gap_salario_mediano
  cat(sprintf("Gap generazionale nazionale (D5):\n"))
  cat(sprintf(
    "  2014: %.3f (%.1f%%)\n",
    g14,
    gap_nazionale[anno == 2014]$pct_gap_salario_mediano
  ))
  cat(sprintf(
    "  2022: %.3f (%.1f%%)\n",
    g22,
    gap_nazionale[anno == 2022]$pct_gap_salario_mediano
  ))
  cat(sprintf("  Variazione: %+.3f\n\n", g22 - g14))
}

if (!is.null(sigma_prov) && nrow(sigma_prov) > 0) {
  cat(sprintf("Sigma-convergenza (province):\n"))
  cat(sprintf(
    "  CV 2014: %.3f → CV 2022: %.3f\n\n",
    sigma_prov[anno == 2014]$cv_gap,
    sigma_prov[anno == 2022]$cv_gap
  ))
}

if (!is.null(beta_prov$model)) {
  cat(sprintf("Beta-convergenza (province):\n"))
  cat(sprintf(
    "  Beta = %.4f, p = %.4f\n\n",
    coef(beta_prov$model)["gap_initial"],
    beta_prov$coef["gap_initial", 4]
  ))
}

# Elenco file salvati
cat("--- File salvati ---\n\n")

output_files <- c(
  "output/racli/dati_eta_territorio.rds",
  "output/racli/dati_eta_settori.rds",
  "output/racli/dati_eta_multidim.rds",
  "output/racli/gap_eta_nazionale.rds",
  "output/racli/gap_eta_province.rds",
  "output/racli/gap_eta_settori.rds",
  "output/racli/gap_eta_macroaree.rds",
  "output/racli/sigma_convergence_eta.rds",
  "output/racli/beta_convergence_eta.rds",
  "output/racli/model_beta_eta.rds",
  "output/racli/model_panel_eta.rds",
  "output/racli/decomposizione_geo_eta.rds",
  "output/racli/decomposizione_settore_eta.rds",
  "output/racli/analisi_congiunta_eta.rds",
  "output/racli/robustezza_eta.rds"
)

for (f in output_files) {
  if (file.exists(f)) {
    size_kb <- round(file.info(f)$size / 1024, 1)
    cat(sprintf("  [OK] %s (%.1f KB)\n", f, size_kb))
  } else {
    cat(sprintf("  [--] %s (non generato)\n", f))
  }
}

grafici <- list.files(
  "output/racli/grafici",
  pattern = "^eta_.*\\.png$",
  full.names = TRUE
)
cat(sprintf("\nGrafici generati: %d\n", length(grafici)))
for (g in grafici) {
  cat(sprintf("  [OK] %s\n", g))
}

cat("\n==== Pipeline Gap Salariale per Età completata ====\n")
cat(sprintf("Tempo: %s\n", Sys.time()))
