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

install_if_missing(c(
  "data.table",
  "ggplot2",
  "scales",
  "here",
  "remotes",
  "sandwich"
))

if (!require("istatlab", quietly = TRUE)) {
  remotes::install_github("gmontaletti/istatlab")
}

suppressPackageStartupMessages({
  library(data.table)
  library(istatlab)
  library(ggplot2)
  library(scales)
  library(here)
  library(sandwich)
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

# Fonte stabilizzata: IPCA 168_761 (base 2025) e retribuzione 155_358 (WAGE_H_2021).
# Filtri SDMX posizionali al download per ridurre il payload di oltre 99%.
# Fallback solo su cache locale in caso di errore di rete.

# IPCA - dimensioni: FREQ . REF_AREA . DATA_TYPE . MEASURE . ECOICOP_2
# Filtro: M (mensile) . IT (Italia) . [tutti DATA_TYPE] . 4 (numeri indice) . 00 (indice generale)
ipca_filter <- build_sdmx_filter_key(
  n_dims = 5,
  dim_values = list("1" = "M", "2" = "IT", "4" = "4", "5" = "00")
)
cat("Download IPCA (168_761, filtro SDMX:", ipca_filter, ")...\n")
ipca_raw <- tryCatch(
  download_istat_data("168_761", filter = ipca_filter),
  error = function(e) NULL
)
if (!is.null(ipca_raw) && is.data.frame(ipca_raw) && nrow(ipca_raw) > 0) {
  saveRDS(ipca_raw, "data/ipca_b2025.rds")
  cat("  OK (", nrow(ipca_raw), "righe)\n")
} else {
  cat("  Download non riuscito, uso cache locale...\n")
  ipca_raw <- readRDS("data/ipca_b2025.rds")
  cat("  Cache caricata\n")
}

pausa <- 15 + runif(1, min = 1, max = 12)
cat("  Pausa", round(pausa, 1), "s...\n")
Sys.sleep(pausa)

# Retribuzione contrattuale - dimensioni:
# FREQ . REF_AREA . DATA_TYPE . ADJUSTMENT . PROF_STATUS_EMP . ECON_ACTIVITY_NACE_2007
# Filtro: M . IT . WAGE_H_2021 (base dic. 2021) . [tutti] . 10 (dipendenti) . 0010 (totale economia)
retr_filter <- build_sdmx_filter_key(
  n_dims = 6,
  dim_values = list(
    "1" = "M",
    "2" = "IT",
    "3" = "WAGE_H_2021",
    "5" = "10",
    "6" = "0010"
  )
)
cat(
  "Download retribuzione contrattuale (155_358, filtro SDMX:",
  retr_filter,
  ")...\n"
)
retr_raw <- tryCatch(
  download_istat_data("155_358", filter = retr_filter),
  error = function(e) NULL
)
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

# IPCA: il filtro SDMX ha già pinato FREQ=M, REF_AREA=IT, MEASURE=4, ECOICOP_2=00.
# Resta da scegliere il DATA_TYPE principale (serie più lunga fra quelli rimasti).
main_dtype <- ipca_raw[, .N, by = DATA_TYPE][which.max(N), DATA_TYPE]
cat("  DATA_TYPE principale:", main_dtype, "\n")

ipca <- ipca_raw[
  DATA_TYPE == main_dtype,
  .(periodo = ObsDimension, ipca = as.numeric(ObsValue))
]
ipca <- unique(ipca, by = "periodo")

# Ribasamento alla media 2020 = 100 (168_761 nasce in base 2025).
anno_base <- 2020
ipca[, data_tmp := as.Date(paste0(periodo, "-01"))]
base_val <- ipca[year(data_tmp) == anno_base, mean(ipca, na.rm = TRUE)]
if (!is.na(base_val) && base_val > 0) {
  ipca[, ipca := ipca / base_val * 100]
  cat(
    "  IPCA ribasato a media 2020 = 100 (fattore:",
    sprintf("%.4f", 100 / base_val),
    ")\n"
  )
}
ipca[, data_tmp := NULL]
cat("IPCA:", nrow(ipca), "osservazioni mensili\n")
cat("  Range:", min(ipca$periodo), "-", max(ipca$periodo), "\n")

# Retribuzione contrattuale oraria: il filtro SDMX ha già pinato
# DATA_TYPE=WAGE_H_2021, FREQ=M, REF_AREA=IT, PROF_STATUS_EMP=10, ECON_ACTIVITY=0010.
retr <- retr_raw[, .(periodo = ObsDimension, w_nominale = as.numeric(ObsValue))]
cat("  WAGE_H_2021 (base dic. 2021):", nrow(retr), "osservazioni\n")

if (nrow(retr) > 0 && retr[, .N, by = periodo][, any(N > 1)]) {
  cat("  Duplicati trovati, aggregazione media per periodo...\n")
  retr <- retr[, .(w_nominale = mean(w_nominale, na.rm = TRUE)), by = periodo]
}

# Ribasamento alla media 2020 = 100 per coerenza con IPCA.
retr[, data_tmp := as.Date(paste0(periodo, "-01"))]
base_val <- retr[year(data_tmp) == anno_base, mean(w_nominale, na.rm = TRUE)]
if (!is.na(base_val) && base_val > 0) {
  retr[, w_nominale := w_nominale / base_val * 100]
  cat(
    "  Ribasato a media 2020 = 100 (fattore:",
    sprintf("%.4f", 100 / base_val),
    ")\n"
  )
}
retr[, data_tmp := NULL]

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

# Variazione cumulata dalla media 2020
base_2020 <- dt[year(data) == anno_base, mean(w_reale, na.rm = TRUE)]
dt[, w_reale_cum_2020 := (w_reale / base_2020 - 1) * 100]

# Colonne helper
dt[, `:=`(
  anno = year(data),
  mese = month(data)
)]

cat("  Salario reale ultimo mese:", sprintf("%.1f", dt[.N, w_reale]), "\n")
cat("  Var. reale YoY:", sprintf("%+.1f%%", dt[.N, var_w_reale]), "\n")
cat("  Var. nominale YoY:", sprintf("%+.1f%%", dt[.N, var_w_nom]), "\n")
cat("  Inflazione YoY:", sprintf("%+.1f%%", dt[.N, var_ipca]), "\n\n")

# 4b. Elasticità / pass-through salari→prezzi -----

cat("==== 4b. Pass-through salari verso prezzi (finestra mobile) ====\n\n")

# Elasticità del salario reale ai prezzi: poiché log(w_reale) = log(w_nom) - log(p),
# ε = dlog(w_reale)/dlog(p) = γ - 1, dove γ = dlog(w_nom)/dlog(p) è il pass-through
# (grado di indicizzazione: γ=1 piena, γ=0 nulla). Stima OLS su finestra mobile di
# differenze logaritmiche a 12 mesi, con errori standard HAC (Newey-West, lag 11
# per le differenze sovrapposte). I mesi con salario stimato sono esclusi dalla
# regressione per non fabbricare un pass-through spurio dal trend di proiezione.

gamma_window <- 36L
min_obs_finestra <- 20L

# Differenze logaritmiche a 12 mesi (sovrapposte → autocorrelazione MA(11))
dt[, dln_w := log(w_nominale) - log(shift(w_nominale, 12L))]
dt[, dln_p := log(ipca) - log(shift(ipca, 12L))]

# Colonne di output (NA fino alla prima finestra completa)
dt[, `:=`(gamma_pt = NA_real_, gamma_se = NA_real_)]

n_righe <- nrow(dt)
for (i in gamma_window:n_righe) {
  finestra <- dt[(i - gamma_window + 1L):i]
  usabili <- finestra[
    !is.na(dln_w) & !is.na(dln_p) & w_stimato == FALSE
  ]
  if (nrow(usabili) < min_obs_finestra) {
    next
  }

  fit_pt <- tryCatch(
    lm(dln_w ~ dln_p, data = usabili),
    error = function(e) NULL
  )
  if (is.null(fit_pt)) {
    next
  }

  g <- unname(coef(fit_pt)["dln_p"])
  se <- tryCatch(
    {
      v <- sandwich::NeweyWest(
        fit_pt,
        lag = 11,
        prewhite = FALSE,
        adjust = TRUE
      )
      unname(sqrt(diag(v))["dln_p"])
    },
    error = function(e) NA_real_
  )
  set(dt, i = i, j = "gamma_pt", value = g)
  set(dt, i = i, j = "gamma_se", value = se)
}

# Elasticità del salario reale ai prezzi
dt[, eps_real := gamma_pt - 1]

# Prima stima disponibile e ultima stima "onesta" (su mese a salario osservato)
data_gamma_primo <- dt[!is.na(gamma_pt), min(data)]
idx_onesti <- dt[!is.na(gamma_pt) & w_stimato == FALSE, which = TRUE]
ultima_gamma_idx <- if (length(idx_onesti)) max(idx_onesti) else NA_integer_

if (!is.na(ultima_gamma_idx)) {
  gamma_ultimo <- dt$gamma_pt[ultima_gamma_idx]
  gamma_se_ultimo <- dt$gamma_se[ultima_gamma_idx]
  data_gamma_ultimo <- dt$data[ultima_gamma_idx]
} else {
  gamma_ultimo <- NA_real_
  gamma_se_ultimo <- NA_real_
  data_gamma_ultimo <- as.Date(NA)
}
eps_ultimo <- gamma_ultimo - 1

# Medie di episodio (caratterizzazione del cambiamento nel tempo)
gamma_pre_surge <- dt[
  data <= as.Date("2020-12-01") & !is.na(gamma_pt),
  mean(gamma_pt)
]
gamma_surge <- dt[
  data >= as.Date("2021-01-01") &
    data <= as.Date("2023-12-01") &
    !is.na(gamma_pt),
  mean(gamma_pt)
]
gamma_post_surge <- dt[
  data >= as.Date("2024-01-01") & !is.na(gamma_pt),
  mean(gamma_pt)
]

cat("  Finestra mobile:", gamma_window, "mesi\n")
cat("  Prima stima disponibile:", format(data_gamma_primo, "%B %Y"), "\n")
cat(
  "  Pass-through ultimo (mese osservato",
  format(data_gamma_ultimo, "%b %Y"),
  "):",
  sprintf(
    "gamma %.2f | eps reale %.2f | SE %.3f",
    gamma_ultimo,
    eps_ultimo,
    gamma_se_ultimo
  ),
  "\n"
)
cat(
  "  Pass-through medio pre-surge (<=2020):",
  sprintf("%.2f", gamma_pre_surge),
  "\n"
)
cat(
  "  Pass-through medio surge (2021-2023):",
  sprintf("%.2f", gamma_surge),
  "\n"
)
cat(
  "  Pass-through medio post-surge (>=2024):",
  sprintf("%.2f", gamma_post_surge),
  "\n\n"
)

# 5. Salvataggio output -----

cat("==== 5. Salvataggio output ====\n\n")

# Serie mensili
saveRDS(dt, file.path(output_dir, "serie_mensili.rds"))
cat("  Salvato:", file.path(output_dir, "serie_mensili.rds"), "\n")

# Stima convergenza a 100: trend lineare (OLS) sulla fase di recupero
ultimo <- dt[.N]
w_reale_ultimo <- ultimo$w_reale
gap_a_100 <- 100 - w_reale_ultimo

# Fase di recupero: dal minimo storico del salario reale all'ultimo dato
min_idx <- which.min(dt$w_reale)
data_minimo <- dt$data[min_idx]
w_reale_minimo <- dt$w_reale[min_idx]
n_mesi_recovery <- nrow(dt) - min_idx

# Valori di default (nessuna convergenza stimabile)
slope_recovery <- NA
r2_recovery <- NA
mesi_a_convergenza <- NA
data_convergenza <- NA
data_convergenza_min <- NA
data_convergenza_max <- NA
proj_band <- NULL

if (n_mesi_recovery >= 6) {
  # t = mesi dal minimo storico; modello lineare w_reale ~ t
  recovery <- dt[min_idx:nrow(dt), .(data, w_reale)]
  recovery[, t := as.numeric(data - data_minimo) / 30.44]
  fit_rec <- lm(w_reale ~ t, data = recovery)
  slope_recovery <- unname(coef(fit_rec)["t"])
  r2_recovery <- summary(fit_rec)$r.squared
  t_ultimo <- as.numeric(ultimo$data - data_minimo) / 30.44

  if (!is.na(slope_recovery) && slope_recovery > 0) {
    # Proiezione mensile dall'ultimo dato, orizzonte massimo 30 anni
    n_horizon <- 360
    proj_dates <- seq.Date(
      ultimo$data,
      by = "month",
      length.out = n_horizon + 1
    )
    proj_t <- t_ultimo + as.numeric(proj_dates - ultimo$data) / 30.44
    pred <- predict(
      fit_rec,
      newdata = data.frame(t = proj_t),
      interval = "confidence",
      level = 0.95
    )
    proj_band <- data.table(
      data = proj_dates,
      fit = pred[, "fit"],
      lwr = pred[, "lwr"],
      upr = pred[, "upr"]
    )

    # Primo mese in cui ciascuna curva raggiunge l'indice 100
    primo_100 <- function(v) {
      idx <- which(v >= 100)[1]
      if (is.na(idx)) as.Date(NA) else proj_dates[idx]
    }
    data_convergenza <- primo_100(proj_band$fit)
    data_convergenza_min <- primo_100(proj_band$upr) # banda alta: convergenza più precoce
    data_convergenza_max <- primo_100(proj_band$lwr) # banda bassa: convergenza più tardiva

    if (!is.na(data_convergenza)) {
      mesi_a_convergenza <- round(
        as.numeric(data_convergenza - ultimo$data) / 30.44
      )
      # Tronca la banda al raggiungimento di 100 (limite inferiore, o stima centrale)
      end_date <- if (!is.na(data_convergenza_max)) {
        data_convergenza_max
      } else {
        data_convergenza
      }
      proj_band <- proj_band[data <= end_date]
    }
  }
}

cat("  Stima convergenza (trend lineare sulla fase di recupero):\n")
cat("    Indice attuale:", sprintf("%.1f", w_reale_ultimo), "\n")
cat(
  "    Minimo storico:",
  sprintf("%.1f", w_reale_minimo),
  paste0("(", format(data_minimo, "%B %Y"), ")\n")
)
cat("    Mesi di recupero:", n_mesi_recovery, "\n")
cat("    Gap a 100:", sprintf("%.1f", gap_a_100), "punti\n")
if (!is.na(slope_recovery)) {
  cat(
    "    Pendenza trend:",
    sprintf("%+.3f punti/mese (R2 %.3f)", slope_recovery, r2_recovery),
    "\n"
  )
}
if (!is.na(mesi_a_convergenza)) {
  cat("    Mesi stimati:", mesi_a_convergenza, "\n")
  cat("    Data stimata:", format(data_convergenza, "%B %Y"), "\n")
  if (!is.na(data_convergenza_min) && !is.na(data_convergenza_max)) {
    cat(
      "    IC 95%:",
      format(data_convergenza_min, "%b %Y"),
      "-",
      format(data_convergenza_max, "%b %Y"),
      "\n"
    )
  }
}
cat("\n")

# Metadata
metadata <- list(
  data_ultimo = ultimo$data,
  periodo_ultimo = format(ultimo$data, "%B %Y"),
  var_w_reale_yoy = ultimo$var_w_reale,
  var_w_nom_yoy = ultimo$var_w_nom,
  var_ipca_yoy = ultimo$var_ipca,
  cum_reale_2020 = ultimo$w_reale_cum_2020,
  w_reale_ultimo = w_reale_ultimo,
  gap_a_100 = gap_a_100,
  w_reale_minimo = w_reale_minimo,
  data_minimo = data_minimo,
  n_mesi_recovery = n_mesi_recovery,
  slope_recovery = slope_recovery,
  r2_recovery = r2_recovery,
  mesi_a_convergenza = mesi_a_convergenza,
  data_convergenza = data_convergenza,
  data_convergenza_min = data_convergenza_min,
  data_convergenza_max = data_convergenza_max,
  proj_band = proj_band,
  data_ultimo_w_osservato = data_ultimo_w,
  n_mesi_stimati = n_stimati,
  n_mesi = nrow(dt),
  data_primo = min(dt$data),
  gamma_ultimo = gamma_ultimo,
  eps_ultimo = eps_ultimo,
  gamma_se_ultimo = gamma_se_ultimo,
  data_gamma_ultimo = data_gamma_ultimo,
  gamma_pre_surge = gamma_pre_surge,
  gamma_surge = gamma_surge,
  gamma_post_surge = gamma_post_surge,
  gamma_window = gamma_window,
  gamma_data_primo = data_gamma_primo,
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

# Grafico 1: Serie indici dal 2020
cat("Grafico 1: Serie indici...\n")

dt_plot <- dt[year(data) >= anno_base]
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
  scale_y_continuous(breaks = seq(80, 140, 5)) +
  labs(
    title = "Salari nominali, prezzi e salari reali in Italia",
    subtitle = paste0(
      "Indici base media 2020 = 100, dati mensili. Ultimo dato: ",
      format(max(dt$data), "%B %Y")
    ),
    x = NULL,
    y = "Indice (media 2020 = 100)",
    caption = "Fonte: ISTAT (dataflow 168_761 e 155_358). Elaborazione propria."
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

# Grafico 3: Pass-through salari→prezzi (finestra mobile)
cat("Grafico 3: Pass-through salari verso prezzi...\n")

dt_pt <- dt[!is.na(gamma_pt)]

p3 <- ggplot(dt_pt, aes(x = data, y = gamma_pt)) +
  annotate(
    "rect",
    xmin = as.Date("2021-01-01"),
    xmax = as.Date("2023-12-01"),
    ymin = -Inf,
    ymax = Inf,
    fill = "grey70",
    alpha = 0.20
  ) +
  geom_hline(yintercept = 1, linetype = "dashed", alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.5) +
  geom_line(linewidth = 0.8, color = "#CC79A7") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title = "Pass-through dei salari ai prezzi",
    subtitle = paste0(
      "Quota di inflazione trasferita ai salari nominali, finestra mobile ",
      gamma_window,
      " mesi. Prima stima: ",
      format(data_gamma_primo, "%b %Y"),
      ". Area grigia: ondata inflazionistica 2021-2023."
    ),
    x = NULL,
    y = expression("Pass-through " * gamma),
    caption = paste0(
      "1 = piena indicizzazione, 0 = nessuna; elasticita salario reale = pass-through - 1. ",
      "Fonte: ISTAT. Elaborazione propria."
    )
  ) +
  theme_salari_reali()

ggsave(
  file.path(grafici_dir, "03_pass_through.png"),
  p3,
  width = 10,
  height = 5,
  dpi = 300
)
cat("  Salvato:", file.path(grafici_dir, "03_pass_through.png"), "\n\n")

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
cat("  - Grafico 3:", file.path(grafici_dir, "03_pass_through.png"), "\n")
cat("  - Report: reports/salari_reali.pdf\n")
