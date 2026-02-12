# preliminare_prep.R -----
# Preparazione dati per report analisi preliminare
# Autore: Giampaolo Montaletti
# Progetto: modello_salari

# 1. Setup e librerie -----
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(zoo)
  library(scales)
  library(corrplot)
  library(gridExtra)
  library(kableExtra)
})

# 2. Creazione directory output -----
if (!dir.exists("output/preliminare")) {
  dir.create("output/preliminare", recursive = TRUE)
}
if (!dir.exists("output/preliminare/grafici")) {
  dir.create("output/preliminare/grafici")
}

# 3. Theme personalizzato -----
theme_salari <- function() {
  theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(color = "grey40", size = 10),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
}
theme_set(theme_salari())

# 4. Caricamento dati -----
# Verifica esistenza dati VECM
if (!file.exists("data/dati_istat.rds")) {
  message("NOTA: data/dati_istat.rds non trovato.")
  message("Eseguire prima vecm_prep.R per generare i dati.")
  message("Alternativa: il report preliminare genera i dati inline.")
} else {
  data_istat <- readRDS("data/dati_istat.rds")

  # Calcola variabili derivate
  data_istat <- data_istat %>%
    mutate(
      w_real = w_nom / p * 100,
      log_w_real = log(w_real),
      g_w_real = c(rep(NA, 4), diff(log(w_real), lag = 4)) * 100
    )

  # Salva dataset preparato
  saveRDS(data_istat, "output/preliminare/dati_preliminare.rds")
  message(
    "Dataset preparato salvato in output/preliminare/dati_preliminare.rds"
  )
}

# 5. Riepilogo -----
cat("\n========================================\n")
cat("PRELIMINARE_PREP.R - Completato\n")
cat("========================================\n")
cat("\nPer generare il report:\n")
cat("  rmarkdown::render('preliminare.Rmd')\n")
cat("\nOutput disponibili:\n")
if (file.exists("output/preliminare/dati_preliminare.rds")) {
  cat("  - output/preliminare/dati_preliminare.rds\n")
}
