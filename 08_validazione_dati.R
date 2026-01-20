# 08_validazione_dati.R -----
# Validazione integrità dati RACLI e confronto con serie VECM
# Autore: Giampaolo Montaletti (ORCID: 0009-0002-5327-1122)
# Data: 2026-01-14

# 1. Setup ambiente -----

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

cat("==== Validazione Dati RACLI ====\n\n")

# Carica dati
dati_sesso <- readRDS("output/dati_settore_sesso.rds")
dati_educazione <- readRDS("output/dati_educazione.rds")
dati_contratto <- readRDS("output/dati_contratto.rds")

# 2. Check 1: Verifica copertura temporale completa -----

cat("==== Check 1: Copertura Temporale ====\n\n")

anni_attesi <- 2014:2022
anni_presenti_sesso <- unique(dati_sesso$anno)
anni_presenti_edu <- unique(dati_educazione$anno)
anni_presenti_contr <- unique(dati_contratto$anno)

cat("Anni attesi: ", paste(anni_attesi, collapse = ", "), "\n")
cat(
  "Anni presenti (sesso): ",
  paste(sort(anni_presenti_sesso), collapse = ", "),
  "\n"
)
cat(
  "Anni presenti (educazione): ",
  paste(sort(anni_presenti_edu), collapse = ", "),
  "\n"
)
cat(
  "Anni presenti (contratto): ",
  paste(sort(anni_presenti_contr), collapse = ", "),
  "\n\n"
)

# Test
test_anni_sesso <- all(anni_attesi %in% anni_presenti_sesso)
test_anni_edu <- all(anni_attesi %in% anni_presenti_edu)
test_anni_contr <- all(anni_attesi %in% anni_presenti_contr)

cat(
  "Copertura completa (sesso): ",
  ifelse(test_anni_sesso, "✓ OK", "✗ FAIL"),
  "\n"
)
cat(
  "Copertura completa (educazione): ",
  ifelse(test_anni_edu, "✓ OK", "✗ FAIL"),
  "\n"
)
cat(
  "Copertura completa (contratto): ",
  ifelse(test_anni_contr, "✓ OK", "✗ FAIL"),
  "\n\n"
)

# 3. Check 2: Verifica pattern missing values -----

cat("==== Check 2: Missing Values ====\n\n")

check_missing <- function(data, dataset_name) {
  cat("Dataset:", dataset_name, "\n")

  missing_rate <- data %>%
    summarise(across(everything(), ~ mean(is.na(.)))) %>%
    pivot_longer(everything(), names_to = "var", values_to = "pct_missing") %>%
    arrange(desc(pct_missing))

  # Mostra solo variabili con missing > 0
  missing_non_zero <- missing_rate %>%
    filter(pct_missing > 0)

  if (nrow(missing_non_zero) > 0) {
    cat("  Variabili con missing:\n")
    print(
      missing_non_zero %>%
        mutate(pct_fmt = sprintf("%.2f%%", pct_missing * 100)) %>%
        select(var, pct_fmt)
    )

    # Verifica soglia 5%
    high_missing <- missing_non_zero %>%
      filter(pct_missing > 0.05)

    if (nrow(high_missing) > 0) {
      cat("\n  ⚠ ATTENZIONE: variabili con >5% missing:\n")
      print(high_missing)
    } else {
      cat("\n  ✓ OK: nessuna variabile supera soglia 5%\n")
    }
  } else {
    cat("  ✓ OK: nessun missing value\n")
  }

  cat("\n")
}

check_missing(dati_sesso, "dati_sesso")
check_missing(dati_educazione, "dati_educazione")
check_missing(dati_contratto, "dati_contratto")

# 4. Check 3: Verifica ordinamento decili (D1 <= D5 <= D9) -----

cat("==== Check 3: Ordinamento Decili ====\n\n")

check_decili <- function(data, dataset_name) {
  cat("Dataset:", dataset_name, "\n")

  check <- data %>%
    filter(!is.na(D1), !is.na(D5), !is.na(D9)) %>%
    mutate(
      ordine_ok = (D1 <= D5) & (D5 <= D9),
      violazione_D1_D5 = D1 > D5,
      violazione_D5_D9 = D5 > D9
    )

  n_violazioni <- sum(!check$ordine_ok, na.rm = TRUE)
  pct_violazioni <- mean(!check$ordine_ok, na.rm = TRUE) * 100

  cat("  Osservazioni totali con decili: ", nrow(check), "\n")
  cat(
    "  Violazioni ordinamento: ",
    n_violazioni,
    sprintf(" (%.2f%%)", pct_violazioni),
    "\n"
  )

  if (pct_violazioni < 1) {
    cat("  ✓ OK: violazioni < 1%\n")
  } else {
    cat("  ⚠ ATTENZIONE: violazioni >= 1%\n")

    # Dettaglio violazioni
    violazioni_D1_D5 <- sum(check$violazione_D1_D5, na.rm = TRUE)
    violazioni_D5_D9 <- sum(check$violazione_D5_D9, na.rm = TRUE)

    cat("    - D1 > D5: ", violazioni_D1_D5, "\n")
    cat("    - D5 > D9: ", violazioni_D5_D9, "\n")
  }

  cat("\n")
}

check_decili(dati_sesso, "dati_sesso")
check_decili(dati_educazione, "dati_educazione")
check_decili(dati_contratto, "dati_contratto")

# 5. Check 4: Outlier detection (Tukey's fences) -----

cat("==== Check 4: Outlier Detection ====\n\n")

# Usa dati nazionali per outlier detection
dati_nazionali <- dati_sesso %>%
  filter(geo_level == "Nazionale", sesso == "totale") %>%
  select(anno, area_label, salario_mediano) %>%
  filter(!is.na(salario_mediano))

if (nrow(dati_nazionali) > 0) {
  # Tukey's fences
  Q1 <- quantile(dati_nazionali$salario_mediano, 0.25, na.rm = TRUE)
  Q3 <- quantile(dati_nazionali$salario_mediano, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1

  lower_fence <- Q1 - 1.5 * IQR
  upper_fence <- Q3 + 1.5 * IQR

  outliers <- dati_nazionali %>%
    mutate(
      is_outlier = (salario_mediano < lower_fence) |
        (salario_mediano > upper_fence),
      tipo_outlier = case_when(
        salario_mediano < lower_fence ~ "basso",
        salario_mediano > upper_fence ~ "alto",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(is_outlier)

  cat("Statistiche distribuzione salario mediano nazionale:\n")
  cat("  Q1 = ", sprintf("%.2f", Q1), "\n")
  cat("  Q3 = ", sprintf("%.2f", Q3), "\n")
  cat("  IQR = ", sprintf("%.2f", IQR), "\n")
  cat("  Lower fence = ", sprintf("%.2f", lower_fence), "\n")
  cat("  Upper fence = ", sprintf("%.2f", upper_fence), "\n\n")

  cat(
    "Outliers rilevati: ",
    nrow(outliers),
    "/",
    nrow(dati_nazionali),
    sprintf(" (%.1f%%)", nrow(outliers) / nrow(dati_nazionali) * 100),
    "\n"
  )

  if (nrow(outliers) > 0) {
    cat("\nDettaglio outliers:\n")
    print(
      outliers %>%
        select(anno, area_label, salario_mediano, tipo_outlier) %>%
        arrange(salario_mediano)
    )
  } else {
    cat("✓ OK: nessun outlier rilevato\n")
  }
} else {
  cat("⚠ Dati nazionali non disponibili per outlier detection\n")
}

cat("\n")

# 6. Check 5: Confronto RACLI vs VECM (periodo sovrapposto 2014-2022) -----

cat("==== Check 5: Confronto RACLI vs Serie VECM ====\n\n")

# Verifica esistenza dati VECM
if (!file.exists("data/dati_istat.rds")) {
  cat("⚠ File data/dati_istat.rds non trovato\n")
  cat("  Impossibile eseguire confronto RACLI-VECM\n\n")
} else {
  # Carica dati VECM
  data_vecm <- readRDS("data/dati_istat.rds")

  # Aggrega RACLI a livello nazionale (totale)
  racli_nazionale <- dati_sesso %>%
    filter(geo_level == "Nazionale", sesso == "totale") %>%
    select(anno, salario_mediano) %>%
    rename(salario_medio_racli = salario_mediano)

  # Prepara dati VECM (solo periodo 2014-2022)
  # Assumo che data_vecm abbia colonne: date, w_nom
  if ("date" %in% names(data_vecm) && "w_nom" %in% names(data_vecm)) {
    data_vecm$year <- as.numeric(format(data_vecm$date, "%Y"))

    vecm_annuale <- data_vecm %>%
      filter(year >= 2014, year <= 2022) %>%
      group_by(year) %>%
      summarise(w_nom_vecm = mean(w_nom, na.rm = TRUE), .groups = "drop")

    # Merge RACLI e VECM
    confronto <- racli_nazionale %>%
      left_join(vecm_annuale, by = c("anno" = "year"))

    # Calcola correlazione
    if (
      sum(
        !is.na(confronto$salario_medio_racli) & !is.na(confronto$w_nom_vecm)
      ) >=
        2
    ) {
      cor_test <- cor.test(confronto$salario_medio_racli, confronto$w_nom_vecm)

      cat("Periodo sovrapposto: 2014-2022\n")
      cat(
        "Osservazioni disponibili: ",
        sum(
          !is.na(confronto$salario_medio_racli) & !is.na(confronto$w_nom_vecm)
        ),
        "\n\n"
      )

      cat("Correlazione RACLI-VECM:\n")
      cat("  r = ", sprintf("%.4f", cor_test$estimate), "\n")
      cat("  p-value = ", sprintf("%.4f", cor_test$p.value), "\n")
      cat(
        "  95% CI = [",
        sprintf("%.3f, %.3f", cor_test$conf.int[1], cor_test$conf.int[2]),
        "]\n\n"
      )

      if (cor_test$estimate >= 0.90) {
        cat("  ✓ OK: correlazione molto forte (r >= 0.90)\n")
      } else if (cor_test$estimate >= 0.70) {
        cat("  ✓ OK: correlazione forte (r >= 0.70)\n")
      } else {
        cat("  ⚠ ATTENZIONE: correlazione moderata/debole (r < 0.70)\n")
      }

      # Salva confronto
      saveRDS(confronto, "output/confronto_racli_vecm.rds")
      cat("\nSalvato: output/confronto_racli_vecm.rds\n")

      # Plot confronto
      p_confronto <- ggplot(
        confronto,
        aes(x = w_nom_vecm, y = salario_medio_racli)
      ) +
        geom_point(size = 3, color = "#2E86AB") +
        geom_smooth(
          method = "lm",
          se = TRUE,
          color = "#A23B72",
          fill = "#A23B72",
          alpha = 0.2
        ) +
        geom_abline(
          slope = 1,
          intercept = 0,
          linetype = "dashed",
          color = "gray40"
        ) +
        labs(
          title = "Validazione RACLI vs VECM",
          subtitle = sprintf(
            "Periodo 2014-2022 | Correlazione r = %.3f",
            cor_test$estimate
          ),
          x = "Salari VECM (indice 2015=100)",
          y = "Salari RACLI (€/ora mediana)"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 11),
          axis.title = element_text(size = 11)
        )

      ggsave(
        "output/grafici/16_validazione_racli_vecm.png",
        plot = p_confronto,
        width = 8,
        height = 6,
        dpi = 300
      )

      cat("Salvato: output/grafici/16_validazione_racli_vecm.png\n")
    } else {
      cat("⚠ Dati insufficienti per calcolare correlazione\n")
    }
  } else {
    cat("⚠ Struttura dati VECM non riconosciuta\n")
    cat("  Colonne presenti:", paste(names(data_vecm), collapse = ", "), "\n")
  }
}

cat("\n")

# 7. Sintesi validazione -----

cat("==== Sintesi Validazione ====\n\n")

sintesi_validazione <- list(
  copertura_temporale = list(
    sesso = test_anni_sesso,
    educazione = test_anni_edu,
    contratto = test_anni_contr
  ),
  data_esecuzione = Sys.time()
)

saveRDS(sintesi_validazione, "output/sintesi_validazione.rds")

cat("Validazione completata con successo\n")
cat("File salvato: output/sintesi_validazione.rds\n\n")

cat("==== Script completato ====\n")
