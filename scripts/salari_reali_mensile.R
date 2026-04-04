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

# IPCA
cat("Download IPCA (168_756)...\n")
ipca_raw <- tryCatch(
  {
    dt <- download_istat_data("168_756")
    saveRDS(dt, "data/ipca.rds")
    cat("  OK\n")
    dt
  },
  error = function(e) {
    cat("  Download fallito:", conditionMessage(e), "\n")
    cat("  Uso dati locali...\n")
    readRDS("data/ipca.rds")
  }
)

pausa <- 15 + runif(1, min = 1, max = 12)
cat("  Pausa", round(pausa, 1), "s...\n")
Sys.sleep(pausa)

# Retribuzione contrattuale
cat("Download retribuzione contrattuale (155_358)...\n")
retr_raw <- tryCatch(
  {
    dt <- download_istat_data("155_358")
    if (!is.null(dt) && nrow(dt) > 0) {
      saveRDS(dt, "data/retr_oraria.rds")
      cat("  OK (", nrow(dt), "righe)\n")
      dt
    } else {
      stop("Download vuoto")
    }
  },
  error = function(e) {
    cat("  Download fallito:", conditionMessage(e), "\n")
    # Fallback: prova sub-dataflow più piccolo
    cat("  Tentativo sub-dataflow 155_358_DF_DCSC_RETRATECO1_1...\n")
    tryCatch(
      {
        dt <- download_istat_data("155_358_DF_DCSC_RETRATECO1_1")
        if (!is.null(dt) && nrow(dt) > 0) {
          saveRDS(dt, "data/retr_oraria.rds")
          cat("  OK via sub-dataflow (", nrow(dt), "righe)\n")
          dt
        } else {
          cat("  Anche sub-dataflow vuoto, uso dati locali...\n")
          readRDS("data/retr_oraria.rds")
        }
      },
      error = function(e2) {
        cat("  Anche sub-dataflow fallito:", conditionMessage(e2), "\n")
        cat("  Uso dati locali...\n")
        readRDS("data/retr_oraria.rds")
      }
    )
  }
)

cat("\n")

# 3. Estrazione serie target -----

cat("==== 3. Estrazione serie target ====\n\n")

ipca_raw <- as.data.table(ipca_raw)
retr_raw <- as.data.table(retr_raw)

# IPCA: indice generale Italia, numeri indici (base 2015=100)
ipca <- ipca_raw[
  E_COICOP_REV_ISTAT == "00" &
    MEASURE == 4 &
    REF_AREA == "IT",
  .(periodo = ObsDimension, ipca = as.numeric(ObsValue))
]
cat("IPCA:", nrow(ipca), "osservazioni mensili\n")
cat("  Range:", min(ipca$periodo), "-", max(ipca$periodo), "\n")

# Retribuzione contrattuale oraria: indice base dic. 2015=100
# Filtra per totale economia, dati mensili
retr <- retr_raw[
  DATA_TYPE == "WAGE_H_2" &
    REF_AREA == "IT" &
    FREQ == "M",
  .(periodo = ObsDimension, w_nominale = as.numeric(ObsValue))
]

# Se ci sono duplicati per ECON_ACTIVITY_NACE_2007 o PROF_STATUS_EMP, filtra totale
if (nrow(retr) == 0) {
  # Prova senza filtro DATA_TYPE e cerca alternative
  cat("  WAGE_H_2 non trovato, ricerca alternative...\n")
  retr_types <- unique(retr_raw$DATA_TYPE)
  wage_types <- grep("WAGE_H", retr_types, value = TRUE)
  cat("  Tipi disponibili:", paste(wage_types, collapse = ", "), "\n")
  if (length(wage_types) > 0) {
    # Usa il primo tipo di indice orario disponibile con base 2015
    for (wt in c("WAGE_H_2", "WAGE_H_2021", "WAGE_H_1", "WAGE_H")) {
      if (wt %in% wage_types) {
        retr <- retr_raw[
          DATA_TYPE == wt &
            REF_AREA == "IT" &
            FREQ == "M",
          .(periodo = ObsDimension, w_nominale = as.numeric(ObsValue))
        ]
        if (nrow(retr) > 0) {
          cat("  Usato DATA_TYPE:", wt, "\n")
          break
        }
      }
    }
  }
}

# Rimuovi duplicati se necessario (media per periodo)
if (retr[, .N, by = periodo][, any(N > 1)]) {
  cat("  Duplicati trovati, aggregazione media per periodo...\n")
  retr <- retr[, .(w_nominale = mean(w_nominale, na.rm = TRUE)), by = periodo]
}

cat("Retribuzioni:", nrow(retr), "osservazioni mensili\n")
cat("  Range:", min(retr$periodo), "-", max(retr$periodo), "\n\n")

# 4. Merge e calcolo salario reale -----

cat("==== 4. Calcolo salario reale ====\n\n")

# Parsare date
ipca[, data := as.Date(paste0(periodo, "-01"))]
retr[, data := as.Date(paste0(periodo, "-01"))]

# Merge
setkey(ipca, data)
setkey(retr, data)
dt <- merge(ipca, retr, by = "data")
setorder(dt, data)

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

# Metadata
ultimo <- dt[.N]
metadata <- list(
  data_ultimo = ultimo$data,
  periodo_ultimo = format(ultimo$data, "%B %Y"),
  var_w_reale_yoy = ultimo$var_w_reale,
  var_w_nom_yoy = ultimo$var_w_nom,
  var_ipca_yoy = ultimo$var_ipca,
  cum_reale_2015 = ultimo$w_reale_cum_2015,
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
