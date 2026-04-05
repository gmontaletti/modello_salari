# salari_reali_mensile.R -----
# Pipeline mensile: salari reali Italia (IPCA vs retribuzione contrattuale)
# Autore: Giampaolo Montaletti (ORCID: 0009-0002-5327-1122)
# Progetto: modello_salari

# 1. Setup -----

cat("==== Pipeline Salari Reali Mensile ====\n")
cat("Avvio:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

install_if_missing <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      install.packages(pkg, repos = "https://cloud.r-project.org")
      library(pkg, character.only = TRUE)
    }
  }
}

install_if_missing(c("data.table", "ggplot2", "scales", "here", "remotes"))

if (!require("istatlab", quietly = TRUE)) {
  remotes::install_github("gmontaletti/istatlab")
}

suppressPackageStartupMessages({
  library(data.table)
  library(istatlab)
  library(ggplot2)
  library(scales)
  library(here)
})

# Directory output
output_dir <- "output/salari_reali"
grafici_dir <- file.path(output_dir, "grafici")
for (d in c(output_dir, grafici_dir)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

set.seed(123)

# Palette CVD-safe (Okabe-Ito)
col_palette <- c(
  "Salario nominale" = "#0072B2",
  "IPCA" = "#D55E00",
  "Salario reale" = "#009E73"
)

# 2. Download dati ISTAT -----

cat("==== 2. Download dati ISTAT ====\n\n")

# IPCA - usa 168_761 (base 2025, più aggiornata) con fallback su 168_756 (base 2015)
cat("Download IPCA (168_761, base 2025)...\n")
ipca_base <- "2025"
ipca_raw <- tryCatch(download_istat_data("168_761"), error = function(e) NULL)
if (is.null(ipca_raw) || !is.data.frame(ipca_raw) || nrow(ipca_raw) == 0) {
  cat("  168_761 non disponibile, provo 168_756 (base 2015)...\n")
  ipca_base <- "2015"
  ipca_raw <- tryCatch(download_istat_data("168_756"), error = function(e) NULL)
}
# Salva cache solo se download valido (non sovrascrivere con NULL)
if (!is.null(ipca_raw) && is.data.frame(ipca_raw) && nrow(ipca_raw) > 0) {
  cache_ipca <- if (ipca_base == "2025") {
    "data/ipca_b2025.rds"
  } else {
    "data/ipca.rds"
  }
  saveRDS(ipca_raw, cache_ipca)
  cat("  OK (", nrow(ipca_raw), "righe)\n")
} else {
  cat("  Download non riuscito, uso cache locale...\n")
  if (file.exists("data/ipca_b2025.rds")) {
    ipca_raw <- readRDS("data/ipca_b2025.rds")
    if (!is.null(ipca_raw) && is.data.frame(ipca_raw) && nrow(ipca_raw) > 0) {
      ipca_base <- "2025"
    } else {
      ipca_raw <- NULL
    }
  }
  if (is.null(ipca_raw) || nrow(ipca_raw) == 0) {
    ipca_raw <- readRDS("data/ipca.rds")
    ipca_base <- "2015"
  }
  cat("  Cache caricata (base", ipca_base, ")\n")
}

pausa <- 15 + runif(1, min = 1, max = 12)
cat("  Pausa", round(pausa, 1), "s...\n")
Sys.sleep(pausa)

# Retribuzione contrattuale
cat("Download retribuzione contrattuale (155_358)...\n")
retr_raw <- tryCatch(download_istat_data("155_358"), error = function(e) NULL)
if (!is.null(retr_raw) && is.data.frame(retr_raw) && nrow(retr_raw) > 0) {
  saveRDS(retr_raw, "data/retr_oraria.rds")
  cat("  OK (", nrow(retr_raw), "righe)\n")
} else {
  cat("  Download non riuscito, uso cache locale...\n")
  retr_raw <- readRDS("data/retr_oraria.rds")
  cat("  Cache caricata\n")
}

cat("\n")

# 3. Estrazione serie target -----

cat("==== 3. Estrazione serie target ====\n\n")

ipca_raw <- as.data.table(ipca_raw)
retr_raw <- as.data.table(retr_raw)

# IPCA: indice generale Italia, numeri indici
# Supporta sia 168_761 (ECOICOP_2) che 168_756 (E_COICOP_REV_ISTAT)
coicop_col <- intersect(c("ECOICOP_2", "E_COICOP_REV_ISTAT"), names(ipca_raw))[
  1
]
cat("  Colonna classificazione:", coicop_col, "(base", ipca_base, ")\n")
# Identifica il DATA_TYPE principale (serie più lunga)
ipca_candidates <- ipca_raw[
  get(coicop_col) == "00" & MEASURE == 4 & REF_AREA == "IT"
]
main_dtype <- ipca_candidates[, .N, by = DATA_TYPE][which.max(N), DATA_TYPE]
cat("  DATA_TYPE principale:", main_dtype, "\n")

ipca <- ipca_candidates[
  DATA_TYPE == main_dtype,
  .(periodo = ObsDimension, ipca = as.numeric(ObsValue))
]
# Rimuovi eventuali duplicati residui
ipca <- unique(ipca, by = "periodo")

# Ribasamento a 2015=100 se base è 2025
if (ipca_base == "2025") {
  ipca[, data_tmp := as.Date(paste0(periodo, "-01"))]
  base_val <- ipca[year(data_tmp) == 2015, mean(ipca, na.rm = TRUE)]
  if (!is.na(base_val) && base_val > 0) {
    ipca[, ipca := ipca / base_val * 100]
    cat(
      "  IPCA ribasato a 2015=100 (fattore:",
      sprintf("%.4f", 100 / base_val),
      ")\n"
    )
  }
  ipca[, data_tmp := NULL]
}
cat("IPCA:", nrow(ipca), "osservazioni mensili\n")
cat("  Range:", min(ipca$periodo), "-", max(ipca$periodo), "\n")

# Retribuzione contrattuale oraria
# Usa WAGE_H_2021 (base dic. 2021=100, serie più aggiornata) con fallback su WAGE_H_2
# Filtra: totale economia (0010), totale dipendenti (PROF_STATUS_EMP=10), mensile
retr <- retr_raw[
  DATA_TYPE == "WAGE_H_2021" &
    REF_AREA == "IT" &
    FREQ == "M" &
    ECON_ACTIVITY_NACE_2007 == "0010" &
    PROF_STATUS_EMP == 10,
  .(periodo = ObsDimension, w_nominale = as.numeric(ObsValue))
]
wage_base <- "2021"
cat("  WAGE_H_2021 (base 2021):", nrow(retr), "osservazioni\n")

# Fallback su WAGE_H_2 se WAGE_H_2021 vuoto
if (nrow(retr) == 0) {
  cat("  WAGE_H_2021 vuoto, provo WAGE_H_2 (base 2015)...\n")
  retr <- retr_raw[
    DATA_TYPE == "WAGE_H_2" &
      REF_AREA == "IT" &
      FREQ == "M" &
      ECON_ACTIVITY_NACE_2007 == "0010",
    .(periodo = ObsDimension, w_nominale = as.numeric(ObsValue))
  ]
  wage_base <- "2015"
  cat("  WAGE_H_2 (base 2015):", nrow(retr), "osservazioni\n")
}

# Rimuovi duplicati se necessario
if (nrow(retr) > 0 && retr[, .N, by = periodo][, any(N > 1)]) {
  cat("  Duplicati trovati, aggregazione media per periodo...\n")
  retr <- retr[, .(w_nominale = mean(w_nominale, na.rm = TRUE)), by = periodo]
}

# Ribasamento a 2015=100 se necessario (per coerenza con IPCA base 2015)
if (wage_base == "2021") {
  retr[, data_tmp := as.Date(paste0(periodo, "-01"))]
  base_val <- retr[year(data_tmp) == 2015, mean(w_nominale, na.rm = TRUE)]
  if (!is.na(base_val) && base_val > 0) {
    retr[, w_nominale := w_nominale / base_val * 100]
    cat(
      "  Ribasato a 2015=100 (fattore:",
      sprintf("%.4f", 100 / base_val),
      ")\n"
    )
  }
  retr[, data_tmp := NULL]
}

cat("Retribuzioni:", nrow(retr), "osservazioni mensili\n")
cat("  Range:", min(retr$periodo), "-", max(retr$periodo), "\n\n")

# 4. Merge e calcolo salario reale -----

cat("==== 4. Calcolo salario reale ====\n\n")

# Parsare date
ipca[, data := as.Date(paste0(periodo, "-01"))]
retr[, data := as.Date(paste0(periodo, "-01"))]

# Merge: IPCA dal primo mese salari in poi (no backfill pre-salari)
primo_w <- min(retr$data)
data_ultimo_w <- max(retr$data)
ipca_filtered <- ipca[data >= primo_w]

setkey(ipca_filtered, data)
setkey(retr, data)
dt <- merge(ipca_filtered, retr, by = "data", all.x = TRUE)
setorder(dt, data)

# Forward fill: stima lineare per mesi dopo l'ultimo salario osservato
dt[, w_stimato := data > data_ultimo_w]
n_stimati <- sum(dt$w_stimato)
if (n_stimati > 0) {
  # Regressione lineare sugli ultimi 5 anni di salario osservato
  retr_recent <- retr[data >= data_ultimo_w - 365.25 * 5]
  retr_recent[, t := as.numeric(data - min(data)) / 30.44]
  fit <- lm(w_nominale ~ t, data = retr_recent)

  # Proiezione per i mesi mancanti
  t0 <- as.numeric(data_ultimo_w - min(retr_recent$data)) / 30.44
  dt[
    w_stimato == TRUE,
    w_nominale := predict(
      fit,
      newdata = data.table(t = t0 + as.numeric(data - data_ultimo_w) / 30.44)
    )
  ]

  cat(
    "  Salario stimato (regressione lineare 5 anni) per",
    n_stimati,
    "mesi:",
    format(data_ultimo_w + 32, "%b %Y"),
    "→",
    format(max(dt$data), "%b %Y"),
    "\n"
  )
  cat("    Trend:", sprintf("%+.3f/mese", coef(fit)["t"]), "\n")
  cat("    R²:", sprintf("%.3f", summary(fit)$r.squared), "\n")
}

# Pulizia colonne periodo dal merge
if ("periodo.x" %in% names(dt)) {
  dt[, periodo := fifelse(is.na(periodo.x), format(data, "%Y-%m"), periodo.x)]
  dt[, c("periodo.x", "periodo.y") := NULL]
}

cat("Serie combinata:", nrow(dt), "mesi\n")
cat(
  "  Da:",
  format(min(dt$data), "%B %Y"),
  "a",
  format(max(dt$data), "%B %Y"),
  "\n"
)

# Calcola salario reale deflazionato
dt[, w_reale := w_nominale / ipca * 100]

# Variazioni tendenziali (anno su anno, 12 mesi)
dt[, var_ipca := (ipca / shift(ipca, 12) - 1) * 100]
dt[, var_w_nom := (w_nominale / shift(w_nominale, 12) - 1) * 100]
dt[, var_w_reale := (w_reale / shift(w_reale, 12) - 1) * 100]

# Variazione cumulata dal 2015
base_2015 <- dt[year(data) == 2015, mean(w_reale, na.rm = TRUE)]
dt[, w_reale_cum_2015 := (w_reale / base_2015 - 1) * 100]

# Colonne helper
dt[, `:=`(
  anno = year(data),
  mese = month(data)
)]

cat("  Salario reale ultimo mese:", sprintf("%.1f", dt[.N, w_reale]), "\n")
cat("  Var. reale YoY:", sprintf("%+.1f%%", dt[.N, var_w_reale]), "\n")
cat("  Var. nominale YoY:", sprintf("%+.1f%%", dt[.N, var_w_nom]), "\n")
cat("  Inflazione YoY:", sprintf("%+.1f%%", dt[.N, var_ipca]), "\n\n")

# 5. Salvataggio output -----

cat("==== 5. Salvataggio output ====\n\n")

# Serie mensili
saveRDS(dt, file.path(output_dir, "serie_mensili.rds"))
cat("  Salvato:", file.path(output_dir, "serie_mensili.rds"), "\n")

# Stima convergenza a 100
ultimo <- dt[.N]
w_reale_ultimo <- ultimo$w_reale
gap_a_100 <- 100 - w_reale_ultimo

# Tasso di crescita dalla fase di recupero (dal minimo storico in poi)
min_idx <- which.min(dt$w_reale)
data_minimo <- dt$data[min_idx]
w_reale_minimo <- dt$w_reale[min_idx]
n_mesi_recovery <- nrow(dt) - min_idx

if (n_mesi_recovery >= 3) {
  tasso_mensile_recovery <- (w_reale_ultimo / w_reale_minimo)^(1 /
    n_mesi_recovery) -
    1
} else {
  tasso_mensile_recovery <- NA
}

# Mesi stimati per raggiungere indice 100 al tasso della fase di recupero
if (!is.na(tasso_mensile_recovery) && tasso_mensile_recovery > 0) {
  mesi_a_convergenza <- ceiling(
    log(100 / w_reale_ultimo) / log(1 + tasso_mensile_recovery)
  )
  data_convergenza <- seq.Date(
    ultimo$data,
    by = "month",
    length.out = mesi_a_convergenza + 1
  )[mesi_a_convergenza + 1]
} else {
  mesi_a_convergenza <- NA
  data_convergenza <- NA
}

cat("  Stima convergenza (fase di recupero):\n")
cat("    Indice attuale:", sprintf("%.1f", w_reale_ultimo), "\n")
cat(
  "    Minimo storico:",
  sprintf("%.1f", w_reale_minimo),
  paste0("(", format(data_minimo, "%B %Y"), ")\n")
)
cat("    Mesi di recupero:", n_mesi_recovery, "\n")
cat("    Gap a 100:", sprintf("%.1f", gap_a_100), "punti\n")
if (!is.na(tasso_mensile_recovery)) {
  cat(
    "    Tasso mensile (recovery):",
    sprintf("%.3f%%", tasso_mensile_recovery * 100),
    "\n"
  )
}
if (!is.na(mesi_a_convergenza)) {
  cat("    Mesi stimati:", mesi_a_convergenza, "\n")
  cat("    Data stimata:", format(data_convergenza, "%B %Y"), "\n")
}
cat("\n")

# Metadata
metadata <- list(
  data_ultimo = ultimo$data,
  periodo_ultimo = format(ultimo$data, "%B %Y"),
  var_w_reale_yoy = ultimo$var_w_reale,
  var_w_nom_yoy = ultimo$var_w_nom,
  var_ipca_yoy = ultimo$var_ipca,
  cum_reale_2015 = ultimo$w_reale_cum_2015,
  w_reale_ultimo = w_reale_ultimo,
  gap_a_100 = gap_a_100,
  w_reale_minimo = w_reale_minimo,
  data_minimo = data_minimo,
  n_mesi_recovery = n_mesi_recovery,
  tasso_mensile_recovery = tasso_mensile_recovery,
  mesi_a_convergenza = mesi_a_convergenza,
  data_convergenza = data_convergenza,
  data_ultimo_w_osservato = data_ultimo_w,
  n_mesi_stimati = n_stimati,
  n_mesi = nrow(dt),
  data_primo = min(dt$data),
  generato_il = Sys.time()
)
saveRDS(metadata, file.path(output_dir, "metadata.rds"))
cat("  Salvato:", file.path(output_dir, "metadata.rds"), "\n\n")

# 6. Grafici -----

cat("==== 6. Generazione grafici ====\n\n")

theme_salari_reali <- function() {
  theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(color = "grey40", size = 10),
      plot.caption = element_text(size = 8, color = "grey50"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.title = element_blank()
    )
}

# Grafico 1: Serie indici dal 2015
cat("Grafico 1: Serie indici...\n")

dt_plot <- dt[anno >= 2015]
dt_long <- melt(
  dt_plot,
  id.vars = "data",
  measure.vars = c("w_nominale", "ipca", "w_reale"),
  variable.name = "serie",
  value.name = "indice"
)
dt_long[,
  serie := factor(
    serie,
    levels = c("w_nominale", "ipca", "w_reale"),
    labels = c("Salario nominale", "IPCA", "Salario reale")
  )
]

p1 <- ggplot(dt_long, aes(x = data, y = indice, color = serie)) +
  geom_line(linewidth = 0.8) +
  geom_hline(yintercept = 100, linetype = "dashed", alpha = 0.4) +
  scale_color_manual(values = col_palette) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(90, 140, 5)) +
  labs(
    title = "Salari nominali, prezzi e salari reali in Italia",
    subtitle = paste0(
      "Indici base 2015=100, dati mensili. Ultimo dato: ",
      format(max(dt$data), "%B %Y")
    ),
    x = NULL,
    y = "Indice (2015=100)",
    caption = "Fonte: ISTAT (dataflow 168_756 e 155_358). Elaborazione propria."
  ) +
  theme_salari_reali()

ggsave(
  file.path(grafici_dir, "01_serie_indici.png"),
  p1,
  width = 10,
  height = 5,
  dpi = 300
)
cat("  Salvato:", file.path(grafici_dir, "01_serie_indici.png"), "\n")

# Grafico 2: Variazioni tendenziali ultimi 24 mesi
cat("Grafico 2: Variazioni YoY...\n")

dt_var <- dt[data >= max(data) - 365 * 2 + 1]
dt_var_long <- melt(
  dt_var,
  id.vars = "data",
  measure.vars = c("var_w_nom", "var_ipca", "var_w_reale"),
  variable.name = "serie",
  value.name = "variazione"
)
dt_var_long[,
  serie := factor(
    serie,
    levels = c("var_w_nom", "var_ipca", "var_w_reale"),
    labels = c("Salario nominale", "IPCA", "Salario reale")
  )
]

p2 <- ggplot(dt_var_long, aes(x = data, y = variazione, color = serie)) +
  geom_line(linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "solid", alpha = 0.3) +
  scale_color_manual(values = col_palette) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b\n%Y") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = "Variazioni tendenziali anno su anno",
    subtitle = paste0(
      "Ultimi 24 mesi (",
      format(min(dt_var$data), "%b %Y"),
      " - ",
      format(max(dt_var$data), "%b %Y"),
      ")"
    ),
    x = NULL,
    y = "Variazione % (t/t-12)",
    caption = "Fonte: ISTAT. Elaborazione propria."
  ) +
  theme_salari_reali()

ggsave(
  file.path(grafici_dir, "02_variazioni_yoy.png"),
  p2,
  width = 10,
  height = 5,
  dpi = 300
)
cat("  Salvato:", file.path(grafici_dir, "02_variazioni_yoy.png"), "\n\n")

# 7. Rendering report -----

cat("==== 7. Rendering report ====\n\n")

tryCatch(
  {
    rmarkdown::render(
      input = "reports/salari_reali.Rmd",
      output_dir = "reports/",
      quiet = TRUE
    )
    cat("Report generato: reports/salari_reali.pdf\n")
  },
  error = function(e) {
    cat("Errore rendering report:", conditionMessage(e), "\n")
    cat("I dati sono comunque disponibili in", output_dir, "\n")
  }
)

# 8. Riepilogo -----

cat("\n==== Pipeline Completata ====\n")
cat("Fine:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")
cat("Output:\n")
cat("  - Dati:", file.path(output_dir, "serie_mensili.rds"), "\n")
cat("  - Metadata:", file.path(output_dir, "metadata.rds"), "\n")
cat("  - Grafico 1:", file.path(grafici_dir, "01_serie_indici.png"), "\n")
cat("  - Grafico 2:", file.path(grafici_dir, "02_variazioni_yoy.png"), "\n")
cat("  - Report: reports/salari_reali.pdf\n")
