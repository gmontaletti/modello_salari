# 03_analisi_distributiva.R -----
# Analisi distributiva: disuguaglianza, mobilit\u00e0 temporale, convergenza
# Autore: Giampaolo Montaletti (ORCID: 0009-0002-5327-1122)
# Data: 2026-01-14

# 1. Setup ambiente -----
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(knitr)
  library(kableExtra)
  library(ineq)
})

cat("==== Analisi Distributiva RACLI ====\n\n")

# Carica dati preparati
dati_sesso <- readRDS("output/dati_settore_sesso.rds")
dati_settori <- readRDS("output/dati_settori.rds")

# 2. Indici di disuguaglianza -----

cat("==== Calcolo Indici Disuguaglianza ====\n\n")

# Gini approssimato da decili
calc_gini_from_deciles <- function(D1, D5, D9) {
  # Approssimazione distribuzione con 5 punti
  approx_dist <- c(
    rep(D1, 10),
    rep(mean(c(D1, D5)), 40),
    rep(mean(c(D5, D9)), 40),
    rep(D9, 10)
  )
  return(ineq::Gini(approx_dist))
}

# Calcola indici per area geografica e anno
indici_disug <- dati_sesso %>%
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

cat(
  "Indici disuguaglianza calcolati per",
  nrow(indici_disug),
  "osservazioni\n\n"
)

# Sintesi nazionale 2022
indici_nazionale_2022 <- indici_disug %>%
  filter(anno == 2022, geo_level == "Nazionale") %>%
  summarise(
    salario_mediano = mean(salario_mediano, na.rm = TRUE),
    gini = mean(gini_approx, na.rm = TRUE),
    D9_D1 = mean(D9_D1, na.rm = TRUE),
    D9_D5 = mean(D9_D5, na.rm = TRUE),
    D5_D1 = mean(D5_D1, na.rm = TRUE)
  )

cat("Indici nazionali 2022:\n")
cat("  Gini:", sprintf("%.3f", indici_nazionale_2022$gini), "\n")
cat("  D9/D1:", sprintf("%.2f", indici_nazionale_2022$D9_D1), "\n")
cat("  D9/D5:", sprintf("%.2f", indici_nazionale_2022$D9_D5), "\n")
cat("  D5/D1:", sprintf("%.2f", indici_nazionale_2022$D5_D1), "\n\n")

# Evoluzione temporale disuguaglianza
evoluzione_disug <- indici_disug %>%
  filter(geo_level == "Nazionale") %>%
  group_by(anno) %>%
  summarise(
    gini = mean(gini_approx, na.rm = TRUE),
    D9_D1 = mean(D9_D1, na.rm = TRUE),
    D9_D5 = mean(D9_D5, na.rm = TRUE),
    .groups = "drop"
  )

cat("Evoluzione disuguaglianza nazionale:\n")
print(
  evoluzione_disug %>%
    mutate(
      gini_fmt = sprintf("%.3f", gini),
      D9_D1_fmt = sprintf("%.2f", D9_D1),
      D9_D5_fmt = sprintf("%.2f", D9_D5)
    ) %>%
    kable(digits = 3)
)

saveRDS(indici_disug, "output/indici_disuguaglianza.rds")
saveRDS(evoluzione_disug, "output/evoluzione_disuguaglianza.rds")
cat("\nSalvato: output/indici_disuguaglianza.rds\n")
cat("Salvato: output/evoluzione_disuguaglianza.rds\n\n")

# 3. Disuguaglianza per ripartizione -----

cat("==== Disuguaglianza per Ripartizione ====\n\n")

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

cat("Disuguaglianza per ripartizione (2022):\n")
print(
  disug_ripartizioni_2022 %>%
    mutate(
      sal_fmt = sprintf("€%.2f", salario_mediano),
      gini_fmt = sprintf("%.3f", gini),
      D9_D1_fmt = sprintf("%.2f", D9_D1)
    ) %>%
    kable(digits = 3)
)

saveRDS(disug_ripartizioni_2022, "output/disug_ripartizioni_2022.rds")
cat("\nSalvato: output/disug_ripartizioni_2022.rds\n\n")

# 4. Mobilità temporale (quartili) -----

cat("==== Mobilità Temporale (Quartili Province) ====\n\n")

# Estrai dati provinciali 2014 e 2022
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
  # Assegna quartili
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

  # Matrice transizione
  matrice_transizione <- province_mobilita %>%
    count(quartile_2014, quartile_2022) %>%
    pivot_wider(
      names_from = quartile_2022,
      values_from = n,
      values_fill = 0,
      names_prefix = "Q2022_"
    )

  cat("Matrice transizione quartili (2014 → 2022):\n")
  print(kable(matrice_transizione))

  # Statistiche mobilità
  mobilita_stats <- province_mobilita %>%
    count(mobilita) %>%
    mutate(percentuale = n / sum(n) * 100)

  cat("\n\nStatistiche mobilità:\n")
  print(
    mobilita_stats %>%
      mutate(pct_fmt = sprintf("%.1f%%", percentuale)) %>%
      kable()
  )

  saveRDS(province_mobilita, "output/mobilita_province.rds")
  saveRDS(matrice_transizione, "output/matrice_transizione.rds")
  cat("\nSalvato: output/mobilita_province.rds\n")
  cat("Salvato: output/matrice_transizione.rds\n\n")
} else {
  cat("Dati provinciali insufficienti per analisi mobilità\n\n")
}

# 5. Convergenza σ e β -----

cat("==== Test Convergenza Territoriale ====\n\n")

# σ-convergenza: riduzione dispersione nel tempo
sigma_convergence <- indici_disug %>%
  filter(geo_level %in% c("Provincia", "Regione")) %>%
  group_by(anno) %>%
  summarise(
    cv = sd(salario_mediano, na.rm = TRUE) /
      mean(salario_mediano, na.rm = TRUE),
    sd_log = sd(log(salario_mediano), na.rm = TRUE),
    .groups = "drop"
  )

cat("σ-convergenza (coefficiente variazione nel tempo):\n")
print(
  sigma_convergence %>%
    mutate(
      cv_fmt = sprintf("%.4f", cv),
      sd_log_fmt = sprintf("%.4f", sd_log)
    ) %>%
    kable(digits = 4)
)

# Test convergenza: CV 2014 vs 2022
cv_2014 <- sigma_convergence %>% filter(anno == 2014) %>% pull(cv)
cv_2022 <- sigma_convergence %>% filter(anno == 2022) %>% pull(cv)

if (length(cv_2014) > 0 && length(cv_2022) > 0) {
  delta_cv <- cv_2022 - cv_2014
  cat("\n\nVariazione CV (2014-2022):", sprintf("%.4f", delta_cv))
  cat(" (", ifelse(delta_cv < 0, "convergenza", "divergenza"), ")\n\n")
}

# β-convergenza: province povere crescono più velocemente?
if (nrow(province_mobilita) > 0) {
  beta_convergence <- province_mobilita %>%
    mutate(
      log_sal_2014 = log(sal_2014),
      crescita_2014_2022 = log(sal_2022 / sal_2014),
      tasso_crescita_annuo = (crescita_2014_2022 / 8) * 100
    ) %>%
    filter(!is.na(log_sal_2014) & !is.na(tasso_crescita_annuo))

  if (nrow(beta_convergence) > 10) {
    # Regressione: crescita ~ livello iniziale
    model_beta <- lm(
      tasso_crescita_annuo ~ log_sal_2014,
      data = beta_convergence
    )
    beta_coef <- coef(model_beta)[2]
    beta_pval <- summary(model_beta)$coefficients[2, 4]

    cat("β-convergenza (regressione crescita su livello iniziale):\n")
    cat("  Coefficiente β:", sprintf("%.4f", beta_coef))
    cat(" (p-value:", sprintf("%.4f", beta_pval), ")\n")
    cat(
      "  Interpretazione:",
      ifelse(
        beta_coef < 0 & beta_pval < 0.05,
        "convergenza significativa",
        "no convergenza significativa"
      ),
      "\n\n"
    )

    saveRDS(beta_convergence, "output/beta_convergence.rds")
    saveRDS(model_beta, "output/model_beta_convergence.rds")
    cat("Salvato: output/beta_convergence.rds\n")
    cat("Salvato: output/model_beta_convergence.rds\n\n")
  } else {
    cat("Dati insufficienti per test β-convergenza\n\n")
  }
} else {
  cat("Dati provinciali non disponibili per β-convergenza\n\n")
}

saveRDS(sigma_convergence, "output/sigma_convergence.rds")
cat("Salvato: output/sigma_convergence.rds\n\n")

# 6. Indici di disuguaglianza settoriali -----

cat("==== Indici Disuguaglianza Settoriali ====\n\n")

# Calcola indici per settore NACE e anno
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

cat(
  "Indici disuguaglianza settoriali calcolati per",
  nrow(indici_disug_settori),
  "osservazioni\n\n"
)

# Sintesi settoriale 2022
indici_settori_2022 <- indici_disug_settori %>%
  filter(anno == 2022) %>%
  summarise(
    n_settori = n(),
    salario_mediano_medio = mean(salario_mediano, na.rm = TRUE),
    D9_D1_medio = mean(D9_D1, na.rm = TRUE),
    D9_D1_min = min(D9_D1, na.rm = TRUE),
    D9_D1_max = max(D9_D1, na.rm = TRUE),
    D9_D5_medio = mean(D9_D5, na.rm = TRUE),
    D5_D1_medio = mean(D5_D1, na.rm = TRUE)
  )

cat("Sintesi disuguaglianza settoriale 2022:\n")
cat("  N. settori:", indici_settori_2022$n_settori, "\n")
cat("  D9/D1 medio:", sprintf("%.2f", indici_settori_2022$D9_D1_medio), "\n")
cat(
  "  D9/D1 range:",
  sprintf("%.2f", indici_settori_2022$D9_D1_min),
  "-",
  sprintf("%.2f", indici_settori_2022$D9_D1_max),
  "\n"
)
cat("  D9/D5 medio:", sprintf("%.2f", indici_settori_2022$D9_D5_medio), "\n")
cat("  D5/D1 medio:", sprintf("%.2f", indici_settori_2022$D5_D1_medio), "\n\n")

# Evoluzione temporale disuguaglianza settoriale
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

cat("Evoluzione disuguaglianza settoriale:\n")
print(
  evoluzione_disug_settori %>%
    mutate(
      D9_D1_fmt = sprintf("%.2f (±%.2f)", D9_D1_medio, D9_D1_sd),
      D9_D5_fmt = sprintf("%.2f", D9_D5_medio),
      D5_D1_fmt = sprintf("%.2f", D5_D1_medio)
    ) %>%
    select(anno, n_settori, D9_D1_fmt, D9_D5_fmt, D5_D1_fmt) %>%
    kable(
      col.names = c("Anno", "N. Settori", "D9/D1 (media±sd)", "D9/D5", "D5/D1"),
      digits = 2
    )
)

saveRDS(indici_disug_settori, "output/indici_disuguaglianza_settori.rds")
saveRDS(
  evoluzione_disug_settori,
  "output/evoluzione_disuguaglianza_settori.rds"
)
cat("\nSalvato: output/indici_disuguaglianza_settori.rds\n")
cat("Salvato: output/evoluzione_disuguaglianza_settori.rds\n\n")

# 7. Sintesi analisi distributiva -----

cat("==== Sintesi Analisi Distributiva ====\n\n")

sintesi_distributiva <- list(
  gini_2022 = indici_nazionale_2022$gini,
  D9_D1_2022 = indici_nazionale_2022$D9_D1,
  gini_2014 = evoluzione_disug %>% filter(anno == 2014) %>% pull(gini),
  variazione_gini = indici_nazionale_2022$gini -
    (evoluzione_disug %>% filter(anno == 2014) %>% pull(gini)),
  n_province_mobilita = ifelse(
    exists("province_mobilita"),
    nrow(province_mobilita),
    0
  ),
  convergenza_sigma = ifelse(exists("delta_cv"), delta_cv < 0, NA),
  convergenza_beta = ifelse(exists("beta_coef"), beta_coef < 0, NA)
)

cat("Indice Gini 2022:", sprintf("%.3f", sintesi_distributiva$gini_2022), "\n")
cat(
  "Rapporto D9/D1 2022:",
  sprintf("%.2f", sintesi_distributiva$D9_D1_2022),
  "\n"
)
if (length(sintesi_distributiva$gini_2014) > 0) {
  cat(
    "Variazione Gini 2014-2022:",
    sprintf("%+.3f", sintesi_distributiva$variazione_gini),
    "\n"
  )
}
if (!is.na(sintesi_distributiva$convergenza_sigma)) {
  cat(
    "σ-convergenza:",
    ifelse(sintesi_distributiva$convergenza_sigma, "SI", "NO"),
    "\n"
  )
}
if (!is.na(sintesi_distributiva$convergenza_beta)) {
  cat(
    "β-convergenza:",
    ifelse(sintesi_distributiva$convergenza_beta, "SI", "NO"),
    "\n"
  )
}

saveRDS(sintesi_distributiva, "output/sintesi_distributiva.rds")
cat("\nSalvato: output/sintesi_distributiva.rds\n\n")

cat("==== Script completato con successo ====\n")
cat("File creati nella directory output/:\n")
cat("- indici_disuguaglianza.rds\n")
cat("- evoluzione_disuguaglianza.rds\n")
cat("- disug_ripartizioni_2022.rds\n")
cat("- mobilita_province.rds (se disponibile)\n")
cat("- matrice_transizione.rds (se disponibile)\n")
cat("- sigma_convergence.rds\n")
cat("- beta_convergence.rds (se disponibile)\n")
cat("- indici_disuguaglianza_settori.rds\n")
cat("- evoluzione_disuguaglianza_settori.rds\n")
cat("- sintesi_distributiva.rds\n")
