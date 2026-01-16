# 05_regressioni_determinanti.R -----
# Modelli di regressione: determinanti salariali
# Autore: Giampaolo Montaletti (ORCID: 0009-0002-5327-1122)
# Data: 2026-01-14

# 1. Setup ambiente -----

# Installa pacchetti se mancanti
if (!require("lmtest", quietly = TRUE)) {
  install.packages("lmtest", repos = "https://cloud.r-project.org")
}
if (!require("sandwich", quietly = TRUE)) {
  install.packages("sandwich", repos = "https://cloud.r-project.org")
}

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lmtest)
  library(sandwich)
  library(knitr)
})

cat("==== Regressioni Determinanti Salariali ====\n\n")

# Carica dati
dati_sesso <- readRDS("output/dati_settore_sesso.rds")
dati_educazione <- readRDS("output/dati_educazione.rds")
dati_contratto <- readRDS("output/dati_contratto.rds")
dati_clustering <- readRDS("output/dati_clustering.rds")

# 2. Prepara dataset per regressioni -----

cat("==== Preparazione Dataset Regressioni ====\n\n")

# Dataset pooled: province × anno con caratteristiche
dati_province <- dati_sesso %>%
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

# Aggiungi cluster se disponibile
if ("cluster_km" %in% names(dati_clustering)) {
  dati_province <- dati_province %>%
    left_join(
      dati_clustering %>%
        select(area, cluster_km, label),
      by = "area"
    )
}

# Crea variabili
dati_regressione <- dati_province %>%
  mutate(
    log_salario = log(salario_mediano),
    anno_norm = anno - 2014, # Anno normalizzato (0 = 2014)
    ripartizione_cat = factor(
      ripartizione,
      levels = c("Nord-ovest", "Nord-est", "Centro", "Sud", "Isole")
    )
  ) %>%
  filter(!is.na(log_salario))

cat("Osservazioni per regressione:", nrow(dati_regressione), "\n")
cat("Province:", length(unique(dati_regressione$area)), "\n")
cat(
  "Anni:",
  paste(sort(unique(dati_regressione$anno)), collapse = ", "),
  "\n\n"
)

# 3. Modello 1: OLS pooled (cross-section × time) -----

cat("==== Modello 1: OLS Pooled ====\n\n")

# Regressione: log(salario) ~ ripartizione + anno + disuguaglianza
model_ols <- lm(
  log_salario ~ ripartizione_cat + anno_norm + D9_D1,
  data = dati_regressione
)

# Errori standard robusti (clustered per provincia)
coef_ols <- coeftest(model_ols, vcov = vcovCL, cluster = ~area)

cat("Coefficienti OLS (errori standard robusti):\n")
print(coef_ols)

# R-squared
r2_ols <- summary(model_ols)$r.squared
cat("\n\nR-squared:", sprintf("%.4f", r2_ols), "\n\n")

# Interpretazione coefficienti
cat("Interpretazione coefficienti:\n")
coef_ripart <- coef(model_ols)[grep("ripartizione", names(coef(model_ols)))]
for (i in 1:length(coef_ripart)) {
  ripart_name <- gsub("ripartizione_cat", "", names(coef_ripart)[i])
  premio_pct <- (exp(coef_ripart[i]) - 1) * 100
  cat(
    "  ",
    ripart_name,
    ": ",
    sprintf("%+.1f%%", premio_pct),
    " vs Nord-Ovest\n"
  )
}

anno_coef <- coef(model_ols)["anno_norm"]
crescita_annua <- (exp(anno_coef) - 1) * 100
cat("  Crescita annua: ", sprintf("%.2f%%", crescita_annua), "\n")

D9_D1_coef <- coef(model_ols)["D9_D1"]
cat(
  "  Elasticità D9/D1: ",
  sprintf("%.3f", D9_D1_coef),
  " (maggiore disuguaglianza → ",
  ifelse(D9_D1_coef > 0, "salari più alti", "salari più bassi"),
  ")\n\n"
)

saveRDS(model_ols, "output/model_ols.rds")
saveRDS(coef_ols, "output/coef_ols.rds")
cat("Salvato: output/model_ols.rds\n")
cat("Salvato: output/coef_ols.rds\n\n")

# 4. Modello 2: Gap di genere -----

cat("==== Modello 2: Determinanti Gap di Genere ====\n\n")

# Prepara dati gender
dati_gender <- dati_sesso %>%
  filter(geo_level == "Provincia", sesso %in% c("maschi", "femmine")) %>%
  select(anno, area, area_label, ripartizione, sesso, salario_mediano) %>%
  pivot_wider(
    names_from = sesso,
    values_from = salario_mediano
  ) %>%
  mutate(
    gap_gender = (maschi - femmine) / femmine * 100,
    anno_norm = anno - 2014,
    ripartizione_cat = factor(ripartizione)
  ) %>%
  filter(!is.na(gap_gender))

if (nrow(dati_gender) > 50) {
  # Regressione gap gender
  model_gender <- lm(
    gap_gender ~ ripartizione_cat + anno_norm,
    data = dati_gender
  )

  cat("Determinanti gap di genere:\n")
  print(summary(model_gender)$coefficients)

  cat(
    "\n\nR-squared:",
    sprintf("%.4f", summary(model_gender)$r.squared),
    "\n\n"
  )

  # Trend temporale
  trend_coef <- coef(model_gender)["anno_norm"]
  cat(
    "Trend temporale gap: ",
    sprintf("%+.2f", trend_coef),
    "punti percentuali/anno\n"
  )
  cat(
    "Interpretazione: il gap",
    ifelse(trend_coef < 0, "si riduce", "aumenta"),
    "di",
    sprintf("%.2f%%", abs(trend_coef)),
    "all'anno\n\n"
  )

  saveRDS(model_gender, "output/model_gender.rds")
  cat("Salvato: output/model_gender.rds\n\n")
} else {
  cat("Dati insufficienti per modello gap di genere\n\n")
}

# 5. Modello 3: Premio educativo -----

cat("==== Modello 3: Premio Educativo ====\n\n")

# Analisi premio educativo per provincia
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
  pivot_wider(
    names_from = educazione,
    values_from = salario_mediano
  )

# Rinomina colonne
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
  cat("Premio educativo per provincia (2022):\n")
  cat(
    "  Media:",
    sprintf("%.1f%%", mean(dati_edu_provincia$premio_educativo, na.rm = TRUE)),
    "\n"
  )
  cat(
    "  Mediana:",
    sprintf(
      "%.1f%%",
      median(dati_edu_provincia$premio_educativo, na.rm = TRUE)
    ),
    "\n"
  )
  cat(
    "  Range:",
    sprintf(
      "%.1f%% - %.1f%%",
      min(dati_edu_provincia$premio_educativo, na.rm = TRUE),
      max(dati_edu_provincia$premio_educativo, na.rm = TRUE)
    ),
    "\n\n"
  )

  # Top/bottom province per premio
  cat("Top 5 province per premio educativo:\n")
  print(
    dati_edu_provincia %>%
      arrange(desc(premio_educativo)) %>%
      head(5) %>%
      select(area_label, premio_educativo) %>%
      mutate(premio_fmt = sprintf("%.1f%%", premio_educativo)) %>%
      kable()
  )

  saveRDS(dati_edu_provincia, "output/premio_educativo_province.rds")
  cat("\nSalvato: output/premio_educativo_province.rds\n\n")
} else {
  cat("Dati insufficienti per analisi premio educativo\n\n")
}

# 6. Modello 4: Premio contratto permanente -----

cat("==== Modello 4: Premio Contratto Permanente ====\n\n")

# Analisi premio contratto permanente
dati_contr_provincia <- dati_contratto %>%
  filter(geo_level == "Provincia", anno == 2022) %>%
  filter(contratto %in% c("tempo indeterminato", "tempo determinato")) %>%
  select(area, area_label, contratto, salario_mediano) %>%
  pivot_wider(
    names_from = contratto,
    values_from = salario_mediano,
    names_prefix = "salario_"
  )

# Rinomina
names(dati_contr_provincia) <- gsub(" ", "_", names(dati_contr_provincia))

dati_contr_provincia <- dati_contr_provincia %>%
  mutate(
    premio_permanente = (salario_tempo_indeterminato -
      salario_tempo_determinato) /
      salario_tempo_determinato *
      100
  ) %>%
  filter(!is.na(premio_permanente))

if (nrow(dati_contr_provincia) > 10) {
  cat("Premio contratto permanente per provincia (2022):\n")
  cat(
    "  Media:",
    sprintf(
      "%.1f%%",
      mean(dati_contr_provincia$premio_permanente, na.rm = TRUE)
    ),
    "\n"
  )
  cat(
    "  Mediana:",
    sprintf(
      "%.1f%%",
      median(dati_contr_provincia$premio_permanente, na.rm = TRUE)
    ),
    "\n"
  )
  cat(
    "  Range:",
    sprintf(
      "%.1f%% - %.1f%%",
      min(dati_contr_provincia$premio_permanente, na.rm = TRUE),
      max(dati_contr_provincia$premio_permanente, na.rm = TRUE)
    ),
    "\n\n"
  )

  saveRDS(dati_contr_provincia, "output/premio_contratto_province.rds")
  cat("Salvato: output/premio_contratto_province.rds\n\n")
} else {
  cat("Dati insufficienti per analisi premio contratto\n\n")
}

# 7. Sintesi regressioni -----

cat("==== Sintesi Analisi Regressione ====\n\n")

sintesi_regressioni <- list(
  n_obs_ols = nrow(dati_regressione),
  r2_ols = r2_ols,
  crescita_annua_pct = crescita_annua,
  n_province = length(unique(dati_regressione$area)),
  anni_coperti = range(dati_regressione$anno),
  premio_educativo_medio = ifelse(
    exists("dati_edu_provincia"),
    mean(dati_edu_provincia$premio_educativo, na.rm = TRUE),
    NA
  ),
  premio_permanente_medio = ifelse(
    exists("dati_contr_provincia"),
    mean(dati_contr_provincia$premio_permanente, na.rm = TRUE),
    NA
  )
)

cat("Osservazioni OLS:", sintesi_regressioni$n_obs_ols, "\n")
cat("R-squared OLS:", sprintf("%.4f", sintesi_regressioni$r2_ols), "\n")
cat(
  "Crescita salariale annua stimata:",
  sprintf("%.2f%%", sintesi_regressioni$crescita_annua_pct),
  "\n"
)
if (!is.na(sintesi_regressioni$premio_educativo_medio)) {
  cat(
    "Premio educativo medio (laurea vs base):",
    sprintf("%.1f%%", sintesi_regressioni$premio_educativo_medio),
    "\n"
  )
}
if (!is.na(sintesi_regressioni$premio_permanente_medio)) {
  cat(
    "Premio contratto permanente medio:",
    sprintf("%.1f%%", sintesi_regressioni$premio_permanente_medio),
    "\n"
  )
}

saveRDS(sintesi_regressioni, "output/sintesi_regressioni.rds")
cat("\nSalvato: output/sintesi_regressioni.rds\n\n")

cat("==== Script completato con successo ====\n")
cat("File creati nella directory output/:\n")
cat("- model_ols.rds\n")
cat("- coef_ols.rds\n")
cat("- model_gender.rds (se disponibile)\n")
cat("- premio_educativo_province.rds (se disponibile)\n")
cat("- premio_contratto_province.rds (se disponibile)\n")
cat("- sintesi_regressioni.rds\n")
