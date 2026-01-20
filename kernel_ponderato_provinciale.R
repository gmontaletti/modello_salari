# kernel_ponderato_provinciale.R
# Stima kernel density delle medie salariali provinciali ponderata per dipendenti
# Autore: Giampaolo Montaletti
# Data: 2025-01-19

# 1. Setup -----

suppressPackageStartupMessages({
  library(istatlab)
  library(data.table)
  library(dplyr)
  library(ggplot2)
})

set.seed(123)

cat("==== Kernel Density Ponderata per Dipendenti ====\n\n")

# Parametri
anno_selezionato <- 2022

# 2. Carica dati RACLI (medie salariali provinciali) -----

cat("=== 1. Caricamento dati RACLI ===\n\n")

# Carica dati da file processato se esiste, altrimenti da raw
if (file.exists("output/dati_settore_sesso.rds")) {
  dati_racli <- readRDS("output/dati_settore_sesso.rds")
  cat("Dati RACLI caricati da output/dati_settore_sesso.rds\n")
} else {
  # Carica da file raw e processa
  racli_raw <- readRDS("racli/F_533_957_DF_DCSC_RACLI_8.rds")
  setDT(racli_raw)

  # Estrai medie salariali
  dati_racli <- racli_raw[
    grepl("_AV_", DATA_TYPE),
    .(
      area = REF_AREA,
      area_label = REF_AREA_label,
      anno = as.integer(format(as.Date(tempo), "%Y")),
      sesso = tolower(SEX_label),
      salario_medio = valore
    )
  ]

  # Aggiungi livello geografico
  dati_racli[,
    geo_level := fifelse(
      area == "IT",
      "Nazionale",
      fifelse(
        nchar(area) == 3,
        "Ripartizione",
        fifelse(nchar(area) == 4, "Regione", "Provincia")
      )
    )
  ]

  cat("Dati RACLI processati da file raw\n")
}

# Filtra province e anno selezionato
medie_province <- dati_racli %>%
  filter(
    anno == anno_selezionato,
    geo_level == "Provincia",
    sesso == "totale"
  ) %>%
  select(area, area_label, salario_medio) %>%
  filter(!is.na(salario_medio)) %>%
  distinct()

setDT(medie_province)

cat("Anno:", anno_selezionato, "\n")
cat("Province con dati salariali:", nrow(medie_province), "\n")
cat(
  "Range salario medio: EUR",
  sprintf("%.2f", min(medie_province$salario_medio)),
  "-",
  sprintf("%.2f", max(medie_province$salario_medio)),
  "/h\n\n"
)

# 3. Scarica dati dipendenti per provincia e genere -----

cat("=== 2. Scaricamento dati occupati per provincia e genere ===\n\n")

# Dataset ISTAT: Occupati per sesso (150_938_DF_DCCV_OCCUPATIT1_21)
occupati_raw <- download_istat_data("150_938_DF_DCCV_OCCUPATIT1_21")
setDT(occupati_raw)

# Filtra province e anno
occupati <- occupati_raw[
  nchar(REF_AREA) >= 5 &
    grepl(paste0("^", anno_selezionato, "$"), ObsDimension)
]

# Crea tabella occupati per sesso
occupati_sesso <- occupati[, .(
  area = REF_AREA,
  sesso = fifelse(SEX == 1, "maschi", fifelse(SEX == 2, "femmine", "totale")),
  occupati_migliaia = ObsValue
)]

# Pivot per avere colonne separate
occupati_wide <- dcast(
  occupati_sesso,
  area ~ sesso,
  value.var = "occupati_migliaia"
)

cat("Province con dati occupazionali:", nrow(occupati_wide), "\n")
cat(
  "Range occupati (migliaia):",
  sprintf("%.1f", min(occupati_wide$totale)),
  "-",
  sprintf("%.1f", max(occupati_wide$totale)),
  "\n\n"
)

# 4. Join dati salariali e occupazionali -----

cat("=== 3. Unione dati salariali e occupazionali ===\n\n")

# Mapping codici NUTS (alcuni codici differiscono tra RACLI e RFL)
# Trentino-Alto Adige ha codici diversi: ITD10/ITD20 (RACLI) vs ITH10/ITH20 (RFL)
mapping_nuts <- data.table(
  area_racli = c("ITD10", "ITD20"),
  area_rfl = c("ITH10", "ITH20")
)

# Crea colonna area_join per medie_province
medie_province[, area_join := area]
for (i in seq_len(nrow(mapping_nuts))) {
  medie_province[
    area == mapping_nuts$area_racli[i],
    area_join := mapping_nuts$area_rfl[i]
  ]
}

# Join per codice area (usando area_join)
dati_uniti <- merge(
  medie_province,
  occupati_wide,
  by.x = "area_join",
  by.y = "area",
  all = FALSE
)

n_matched <- nrow(dati_uniti)
n_racli <- nrow(medie_province)
n_occupati <- nrow(occupati_wide)

cat("Province RACLI:", n_racli, "\n")
cat("Province occupati:", n_occupati, "\n")
cat("Province unite (match):", n_matched, "\n")
cat("Match rate:", sprintf("%.1f%%", 100 * n_matched / n_racli), "\n\n")

if (n_matched < n_racli) {
  non_matched <- setdiff(medie_province$area_join, occupati_wide$area)
  cat(
    "Province RACLI senza match:",
    paste(non_matched, collapse = ", "),
    "\n"
  )
  # Nota: Bolzano (ITH10) e Trento (ITH20) non hanno dati occupati a livello
  # provinciale nel dataset RFL - sono aggregate a livello regionale (ITD1, ITD2)
  cat(
    "  (Nota: Trentino-Alto Adige non ha dati provinciali nel dataset RFL)\n\n"
  )
}

# 5. Stima kernel density ponderata -----

cat("=== 4. Stima Kernel Density Ponderata ===\n\n")

# Prepara vettori
x <- dati_uniti$salario_medio
w <- dati_uniti$totale # pesi = numero occupati in migliaia

# Normalizza pesi
w_norm <- w / sum(w)

cat("Somma pesi normalizzati:", sum(w_norm), "\n")
cat("Peso medio:", sprintf("%.4f", mean(w_norm)), "\n")
cat("Range pesi:", sprintf("%.4f - %.4f", min(w_norm), max(w_norm)), "\n\n")

# Stima kernel ponderata con bandwidth Sheather-Jones
# La funzione density() accetta weights per kernel ponderata
kde_ponderata <- density(
  x = x,
  weights = w_norm,
  bw = "SJ",
  n = 512
)

# Stima kernel non ponderata per confronto
kde_non_ponderata <- density(
  x = x,
  bw = "SJ",
  n = 512
)

cat("Bandwidth ponderata (SJ):", sprintf("%.4f", kde_ponderata$bw), "\n")
cat(
  "Bandwidth non ponderata (SJ):",
  sprintf("%.4f", kde_non_ponderata$bw),
  "\n"
)
cat("Punti griglia:", length(kde_ponderata$x), "\n\n")

# Calcola log-densita (evitando log(0))
log_density_pond <- log(kde_ponderata$y + 1e-10)
log_density_non_pond <- log(kde_non_ponderata$y + 1e-10)

# 6. Statistiche kernel -----

# Trova moda (picco della densita)
idx_moda_pond <- which.max(kde_ponderata$y)
moda_pond <- kde_ponderata$x[idx_moda_pond]

idx_moda_non_pond <- which.max(kde_non_ponderata$y)
moda_non_pond <- kde_non_ponderata$x[idx_moda_non_pond]

# Media ponderata
media_ponderata <- sum(x * w_norm)
media_non_ponderata <- mean(x)

cat("Moda kernel ponderata:", sprintf("%.2f", moda_pond), "EUR/h\n")
cat("Moda kernel non ponderata:", sprintf("%.2f", moda_non_pond), "EUR/h\n")
cat("Media ponderata:", sprintf("%.2f", media_ponderata), "EUR/h\n")
cat("Media non ponderata:", sprintf("%.2f", media_non_ponderata), "EUR/h\n\n")

# 7. Prepara dati per plot -----

df_kde <- data.frame(
  x = kde_ponderata$x,
  density_pond = kde_ponderata$y,
  log_density_pond = log_density_pond,
  density_non_pond = kde_non_ponderata$y,
  log_density_non_pond = log_density_non_pond
)

# 8. Grafico log-kernel density ponderata (PDF) -----

cat("=== 5. Generazione Grafico PDF ===\n\n")

# Crea directory output se non esiste
if (!dir.exists("output/grafici")) {
  dir.create("output/grafici", recursive = TRUE)
}

# Apri dispositivo PDF
pdf(
  "output/grafici/26_log_kernel_ponderata_dipendenti.pdf",
  width = 10,
  height = 7
)

# Plot log-densita ponderata
p1 <- ggplot(df_kde, aes(x = x)) +
  # Curva log-kernel ponderata
  geom_line(
    aes(y = log_density_pond, color = "Ponderata"),
    linewidth = 1.2
  ) +
  # Curva log-kernel non ponderata per confronto
  geom_line(
    aes(y = log_density_non_pond, color = "Non ponderata"),
    linewidth = 0.8,
    linetype = "dashed"
  ) +
  # Rug plot dei dati osservati (dimensione proporzionale al peso)
  geom_rug(
    data = dati_uniti,
    aes(x = salario_medio, y = NULL),
    color = "darkred",
    alpha = 0.5,
    sides = "b"
  ) +
  # Linea verticale sulla moda ponderata
  geom_vline(
    xintercept = moda_pond,
    color = "steelblue",
    linetype = "dashed",
    linewidth = 0.8
  ) +
  # Annotazione moda
  annotate(
    "text",
    x = moda_pond + 0.3,
    y = max(log_density_pond) - 0.3,
    label = sprintf("Moda pond. = EUR%.2f/h", moda_pond),
    color = "steelblue",
    hjust = 0,
    size = 3.5
  ) +
  # Annotazione bandwidth e statistiche
  annotate(
    "text",
    x = max(kde_ponderata$x) - 0.5,
    y = min(log_density_pond) + 1.5,
    label = sprintf(
      "Bandwidth (SJ) = %.3f\nn = %d province\nMedia pond. = EUR%.2f/h",
      kde_ponderata$bw,
      n_matched,
      media_ponderata
    ),
    hjust = 1,
    size = 3,
    color = "gray40"
  ) +
  # Scala colori
  scale_color_manual(
    name = "Kernel",
    values = c("Ponderata" = "steelblue", "Non ponderata" = "gray50")
  ) +
  # Labels
  labs(
    title = sprintf(
      "Log-Kernel Density delle Medie Salariali Provinciali (%d)",
      anno_selezionato
    ),
    subtitle = "Stima ponderata per numero di dipendenti (migliaia) vs non ponderata",
    x = "Salario medio orario (EUR/h)",
    y = "Log-densita",
    caption = "Fonte: ISTAT-RACLI (salari) + ISTAT RFL (occupati)"
  ) +
  # Scala x (usa limiti del kernel per evitare rimozione punti)
  scale_x_continuous(
    limits = c(min(kde_ponderata$x), max(kde_ponderata$x)),
    breaks = seq(floor(min(x)), ceiling(max(x)), 1)
  ) +
  # Tema
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40", size = 11),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(color = "gray50", size = 9),
    legend.position = "bottom"
  )

print(p1)

dev.off()

cat("Salvato: output/grafici/26_log_kernel_ponderata_dipendenti.pdf\n\n")

# 9. Grafico confronto scale (PDF) -----

pdf("output/grafici/27_kernel_ponderata_confronto.pdf", width = 12, height = 5)

par(mfrow = c(1, 2), mar = c(5, 4, 4, 2))

# Pannello 1: Densita scala normale
plot(
  kde_ponderata,
  main = sprintf("Kernel Density Ponderata (%d)", anno_selezionato),
  xlab = "Salario medio (EUR/h)",
  ylab = "Densita",
  col = "steelblue",
  lwd = 2
)
lines(kde_non_ponderata, col = "gray50", lty = 2)
rug(x, col = "darkred")
abline(v = moda_pond, col = "steelblue", lty = 2)
abline(v = moda_non_pond, col = "gray50", lty = 3)
legend(
  "topright",
  legend = c("Ponderata", "Non ponderata", "Dati"),
  col = c("steelblue", "gray50", "darkred"),
  lty = c(1, 2, NA),
  pch = c(NA, NA, "|"),
  lwd = c(2, 1, 1),
  cex = 0.8
)

# Pannello 2: Log-densita
plot(
  kde_ponderata$x,
  log_density_pond,
  type = "l",
  main = sprintf("Log-Kernel Density Ponderata (%d)", anno_selezionato),
  xlab = "Salario medio (EUR/h)",
  ylab = "Log-densita",
  col = "steelblue",
  lwd = 2
)
lines(kde_non_ponderata$x, log_density_non_pond, col = "gray50", lty = 2)
rug(x, col = "darkred")
abline(v = moda_pond, col = "steelblue", lty = 2)

dev.off()

cat("Salvato: output/grafici/27_kernel_ponderata_confronto.pdf\n\n")

# 10. Grafico per genere (maschi vs femmine) -----

cat("=== 6. Analisi per Genere ===\n\n")

# Ricarica dati RACLI per genere
if (file.exists("output/dati_settore_sesso.rds")) {
  dati_genere <- readRDS("output/dati_settore_sesso.rds") %>%
    filter(
      anno == anno_selezionato,
      geo_level == "Provincia",
      sesso %in% c("maschi", "femmine")
    ) %>%
    select(area, sesso, salario_medio) %>%
    filter(!is.na(salario_medio))

  setDT(dati_genere)

  # Pivot per avere maschi e femmine come colonne
  salari_genere <- dcast(dati_genere, area ~ sesso, value.var = "salario_medio")

  # Applica mapping NUTS per join corretto
  salari_genere[, area_join := area]
  for (i in seq_len(nrow(mapping_nuts))) {
    salari_genere[
      area == mapping_nuts$area_racli[i],
      area_join := mapping_nuts$area_rfl[i]
    ]
  }

  # Join con occupati per genere
  dati_genere_uniti <- merge(
    salari_genere,
    occupati_wide[, .(area, maschi_occ = maschi, femmine_occ = femmine)],
    by.x = "area_join",
    by.y = "area",
    all = FALSE
  )

  cat("Province con dati per genere:", nrow(dati_genere_uniti), "\n")

  # Kernel ponderata per maschi
  w_maschi <- dati_genere_uniti$maschi_occ / sum(dati_genere_uniti$maschi_occ)
  kde_maschi <- density(
    dati_genere_uniti$maschi,
    weights = w_maschi,
    bw = "SJ",
    n = 512
  )

  # Kernel ponderata per femmine
  w_femmine <- dati_genere_uniti$femmine_occ /
    sum(dati_genere_uniti$femmine_occ)
  kde_femmine <- density(
    dati_genere_uniti$femmine,
    weights = w_femmine,
    bw = "SJ",
    n = 512
  )

  cat(
    "Moda maschi:",
    sprintf("%.2f", kde_maschi$x[which.max(kde_maschi$y)]),
    "EUR/h\n"
  )
  cat(
    "Moda femmine:",
    sprintf("%.2f", kde_femmine$x[which.max(kde_femmine$y)]),
    "EUR/h\n"
  )
  cat(
    "Gender pay gap (mode):",
    sprintf(
      "%.1f%%",
      100 *
        (kde_maschi$x[which.max(kde_maschi$y)] -
          kde_femmine$x[which.max(kde_femmine$y)]) /
        kde_maschi$x[which.max(kde_maschi$y)]
    ),
    "\n\n"
  )

  # Plot per genere
  pdf("output/grafici/28_log_kernel_genere.pdf", width = 10, height = 7)

  df_genere <- data.frame(
    x = c(kde_maschi$x, kde_femmine$x),
    log_density = c(log(kde_maschi$y + 1e-10), log(kde_femmine$y + 1e-10)),
    genere = rep(c("Maschi", "Femmine"), each = 512)
  )

  p2 <- ggplot(df_genere, aes(x = x, y = log_density, color = genere)) +
    geom_line(linewidth = 1.2) +
    scale_color_manual(
      name = "Genere",
      values = c("Maschi" = "dodgerblue", "Femmine" = "coral")
    ) +
    labs(
      title = sprintf(
        "Log-Kernel Density per Genere - Medie Salariali Provinciali (%d)",
        anno_selezionato
      ),
      subtitle = "Stime ponderate per numero di dipendenti (migliaia)",
      x = "Salario medio orario (EUR/h)",
      y = "Log-densita",
      caption = "Fonte: ISTAT-RACLI (salari) + ISTAT RFL (occupati)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(color = "gray40", size = 11),
      panel.grid.minor = element_blank(),
      plot.caption = element_text(color = "gray50", size = 9),
      legend.position = "bottom"
    )

  print(p2)
  dev.off()

  cat("Salvato: output/grafici/28_log_kernel_genere.pdf\n\n")
}

# 11. Salva risultati -----

risultati <- list(
  anno = anno_selezionato,
  n_province = n_matched,
  dati = dati_uniti,
  kernel_ponderata = kde_ponderata,
  kernel_non_ponderata = kde_non_ponderata,
  moda_ponderata = moda_pond,
  moda_non_ponderata = moda_non_pond,
  media_ponderata = media_ponderata,
  media_non_ponderata = media_non_ponderata,
  statistiche = list(
    salario_min = min(x),
    salario_max = max(x),
    salario_sd = sd(x),
    occupati_totali = sum(dati_uniti$totale)
  )
)

saveRDS(risultati, "output/risultati_kernel_ponderata.rds")
cat("Salvato: output/risultati_kernel_ponderata.rds\n\n")

# 12. Sintesi finale -----

cat("==== SINTESI ====\n\n")
cat(sprintf("Anno analizzato: %d\n", anno_selezionato))
cat(sprintf("Province analizzate: %d\n", n_matched))
cat(sprintf("Occupati totali: %.0f migliaia\n", sum(dati_uniti$totale)))
cat(sprintf("\nKernel ponderata:\n"))
cat(sprintf("  Moda: EUR%.2f/h\n", moda_pond))
cat(sprintf("  Media ponderata: EUR%.2f/h\n", media_ponderata))
cat(sprintf("  Bandwidth (SJ): %.4f\n", kde_ponderata$bw))
cat(sprintf("\nKernel non ponderata:\n"))
cat(sprintf("  Moda: EUR%.2f/h\n", moda_non_pond))
cat(sprintf("  Media: EUR%.2f/h\n", media_non_ponderata))
cat(sprintf("  Bandwidth (SJ): %.4f\n", kde_non_ponderata$bw))
cat("\nFile generati:\n")
cat("  - output/grafici/26_log_kernel_ponderata_dipendenti.pdf\n")
cat("  - output/grafici/27_kernel_ponderata_confronto.pdf\n")
cat("  - output/grafici/28_log_kernel_genere.pdf\n")
cat("  - output/risultati_kernel_ponderata.rds\n")
