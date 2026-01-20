# kernel_mediane_provinciali.R
# Stima kernel density delle mediane salariali provinciali
# Autore: Giampaolo Montaletti
# Data: 2025-01-19

# 1. Setup -----

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

set.seed(123)

cat("==== Kernel Density Mediane Provinciali RACLI ====\n\n")

# 2. Carica dati -----

dati <- readRDS("output/dati_settore_sesso.rds")

# Seleziona anno (default 2022)
anno_selezionato <- 2022

# 3. Estrai mediane provinciali -----

mediane_province <- dati %>%
  filter(
    anno == anno_selezionato,
    geo_level == "Provincia",
    sesso == "totale"
  ) %>%
  select(area, area_label, salario_mediano) %>%
  filter(!is.na(salario_mediano)) %>%
  arrange(salario_mediano)

n_province <- nrow(mediane_province)

cat("Anno:", anno_selezionato, "\n")
cat("Numero province:", n_province, "\n")
cat(
  "Range mediane: €",
  sprintf("%.2f", min(mediane_province$salario_mediano)),
  "- €",
  sprintf("%.2f", max(mediane_province$salario_mediano)),
  "/h\n"
)
cat("Media:", sprintf("%.2f", mean(mediane_province$salario_mediano)), "€/h\n")
cat(
  "Mediana:",
  sprintf("%.2f", median(mediane_province$salario_mediano)),
  "€/h\n\n"
)

# 4. Stima kernel density -----

cat("==== Stima Kernel Density ====\n\n")

# Vettore delle mediane
x <- mediane_province$salario_mediano

# Stima kernel con bandwidth ottimale (Sheather-Jones)
kde <- density(x, bw = "SJ", n = 512)

cat("Bandwidth (SJ):", sprintf("%.4f", kde$bw), "\n")
cat("Punti griglia:", length(kde$x), "\n\n")

# Calcola log-densità (evitando log(0))
log_density <- log(kde$y + 1e-10)

# Dataframe per plot
df_kde <- data.frame(
  x = kde$x,
  density = kde$y,
  log_density = log_density
)

# 5. Statistiche kernel -----

# Trova moda (picco della densità)
idx_moda <- which.max(kde$y)
moda <- kde$x[idx_moda]

cat("Moda (picco kernel):", sprintf("%.2f", moda), "€/h\n")
cat("Densità massima:", sprintf("%.4f", max(kde$y)), "\n")
cat("Log-densità massima:", sprintf("%.4f", max(log_density)), "\n\n")

# 6. Grafico log-kernel density -----

cat("==== Generazione Grafico PDF ====\n\n")

# Apri dispositivo PDF
pdf(
  "output/grafici/24_log_kernel_mediane_provinciali.pdf",
  width = 10,
  height = 7
)

# Plot log-densità
p <- ggplot(df_kde, aes(x = x, y = log_density)) +
  # Curva log-kernel
  geom_line(color = "steelblue", linewidth = 1.2) +
  # Area sotto la curva
  geom_ribbon(
    aes(ymin = min(log_density) - 0.5, ymax = log_density),
    fill = "steelblue",
    alpha = 0.2
  ) +
  # Rug plot dei dati osservati
  geom_rug(
    data = mediane_province,
    aes(x = salario_mediano, y = NULL),
    color = "darkred",
    alpha = 0.5,
    sides = "b"
  ) +
  # Linea verticale sulla moda
  geom_vline(
    xintercept = moda,
    color = "darkgreen",
    linetype = "dashed",
    linewidth = 0.8
  ) +
  # Annotazione moda

  annotate(
    "text",
    x = moda + 0.3,
    y = max(log_density) - 0.5,
    label = sprintf("Moda = €%.2f/h", moda),
    color = "darkgreen",
    hjust = 0,
    size = 4
  ) +
  # Annotazione bandwidth
  annotate(
    "text",
    x = max(kde$x) - 1,
    y = min(log_density) + 1,
    label = sprintf(
      "Bandwidth (SJ) = %.3f\nn = %d province",
      kde$bw,
      n_province
    ),
    hjust = 1,
    size = 3.5,
    color = "gray40"
  ) +
  # Labels
  labs(
    title = sprintf(
      "Log-Kernel Density delle Mediane Salariali Provinciali (%d)",
      anno_selezionato
    ),
    subtitle = "Stima non parametrica con kernel gaussiano e bandwidth Sheather-Jones",
    x = "Salario mediano orario (€/h)",
    y = "Log-densità",
    caption = "Fonte: ISTAT-RACLI | Rug plot: osservazioni provinciali"
  ) +
  # Scala x
  scale_x_continuous(
    limits = c(min(x) - 1, max(x) + 1),
    breaks = seq(8, 16, 1)
  ) +
  # Tema
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40", size = 11),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(color = "gray50", size = 9)
  )

print(p)

# Chiudi PDF
dev.off()

cat("Salvato: output/grafici/24_log_kernel_mediane_provinciali.pdf\n\n")

# 7. Grafico aggiuntivo: confronto densità e log-densità -----

pdf("output/grafici/25_kernel_confronto_scale.pdf", width = 12, height = 5)

# Due pannelli affiancati
par(mfrow = c(1, 2), mar = c(5, 4, 4, 2))

# Pannello 1: Densità normale
plot(
  kde,
  main = sprintf("Kernel Density - Mediane Provinciali %d", anno_selezionato),
  xlab = "Salario mediano (€/h)",
  ylab = "Densità",
  col = "steelblue",
  lwd = 2
)
rug(x, col = "darkred")
abline(v = moda, col = "darkgreen", lty = 2)
legend(
  "topright",
  legend = c("Kernel", "Moda", "Dati"),
  col = c("steelblue", "darkgreen", "darkred"),
  lty = c(1, 2, NA),
  pch = c(NA, NA, "|"),
  lwd = c(2, 1, 1)
)

# Pannello 2: Log-densità
plot(
  kde$x,
  log_density,
  type = "l",
  main = sprintf(
    "Log-Kernel Density - Mediane Provinciali %d",
    anno_selezionato
  ),
  xlab = "Salario mediano (€/h)",
  ylab = "Log-densità",
  col = "steelblue",
  lwd = 2
)
rug(x, col = "darkred")
abline(v = moda, col = "darkgreen", lty = 2)

dev.off()

cat("Salvato: output/grafici/25_kernel_confronto_scale.pdf\n\n")

# 8. Output dati -----

risultati_kernel <- list(
  anno = anno_selezionato,
  n_province = n_province,
  mediane = mediane_province,
  kernel = kde,
  moda = moda,
  bandwidth = kde$bw,
  statistiche = list(
    media = mean(x),
    mediana = median(x),
    sd = sd(x),
    min = min(x),
    max = max(x)
  )
)

saveRDS(risultati_kernel, "output/risultati_kernel_provinciale.rds")
cat("Salvato: output/risultati_kernel_provinciale.rds\n\n")

# 9. Sintesi -----

cat("==== SINTESI ====\n\n")
cat(sprintf("Anno analizzato: %d\n", anno_selezionato))
cat(sprintf("Province: %d\n", n_province))
cat(sprintf("Salario mediano medio: €%.2f/h\n", mean(x)))
cat(sprintf("Moda (kernel): €%.2f/h\n", moda))
cat(sprintf("Bandwidth ottimale: %.4f\n", kde$bw))
cat(sprintf("Deviazione standard: %.2f\n", sd(x)))
cat("\nFile generati:\n")
cat("  - output/grafici/24_log_kernel_mediane_provinciali.pdf\n")
cat("  - output/grafici/25_kernel_confronto_scale.pdf\n")
cat("  - output/risultati_kernel_provinciale.rds\n")
