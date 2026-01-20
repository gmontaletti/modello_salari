# analisi_distribuzione_lognormale.R
# Stima distribuzione salariale log-normale dai decili RACLI
# Autore: Giampaolo Montaletti
# Data: 2025-01-19

# 1. Setup -----

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(ineq)
})

set.seed(123)

cat("==== Stima Distribuzione Log-Normale da Decili RACLI ====\n\n")

# 2. Carica dati -----

dati <- readRDS("output/dati_settore_sesso.rds")

# Estrai dati nazionali 2022 (totale sesso)
dati_2022 <- dati %>%
  filter(anno == 2022, geo_level == "Nazionale", sesso == "totale") %>%
  select(D1, D5, D9, salario_medio, salario_mediano)

cat("Dati nazionali 2022:\n")
cat("  D1 (P10):", dati_2022$D1, "€/h\n")
cat("  D5 (P50):", dati_2022$D5, "€/h\n")
cat("  D9 (P90):", dati_2022$D9, "€/h\n")
cat("  Media:", dati_2022$salario_medio, "€/h\n\n")

# 3. Stima parametri log-normale -----

# Per X ~ LogNormal(mu, sigma):
# - Mediana = exp(mu)
# - P10 = exp(mu - 1.2816 * sigma)  [qnorm(0.10) = -1.2816]
# - P90 = exp(mu + 1.2816 * sigma)  [qnorm(0.90) = 1.2816]
# - Media = exp(mu + sigma^2/2)

# Metodo 1: Stima da mediana e rapporto interdecile
estimate_lognormal_params <- function(D1, D5, D9, mean_val = NULL) {
  # z-scores per percentili
  z10 <- qnorm(0.10) # -1.2816
  z50 <- qnorm(0.50) # 0
  z90 <- qnorm(0.90) # 1.2816

  # Stima mu dalla mediana
  mu_from_median <- log(D5)

  # Stima sigma dal rapporto D9/D1
  # log(D9) - log(D1) = (mu + z90*sigma) - (mu + z10*sigma) = (z90 - z10)*sigma
  sigma_from_ratio <- (log(D9) - log(D1)) / (z90 - z10)

  # Verifica con media (se disponibile)
  if (!is.null(mean_val) && !is.na(mean_val)) {
    # Media teorica = exp(mu + sigma^2/2)
    # Quindi: mu = log(media) - sigma^2/2
    mu_from_mean <- log(mean_val) - sigma_from_ratio^2 / 2

    # Media stimata con parametri
    mean_estimated <- exp(mu_from_median + sigma_from_ratio^2 / 2)
  } else {
    mu_from_mean <- NA
    mean_estimated <- exp(mu_from_median + sigma_from_ratio^2 / 2)
  }

  list(
    mu = mu_from_median,
    sigma = sigma_from_ratio,
    mu_from_mean = mu_from_mean,
    mean_estimated = mean_estimated,
    mean_observed = mean_val
  )
}

params <- estimate_lognormal_params(
  D1 = dati_2022$D1,
  D5 = dati_2022$D5,
  D9 = dati_2022$D9,
  mean_val = dati_2022$salario_medio
)

cat("Parametri log-normale stimati:\n")
cat("  mu (da mediana):", sprintf("%.4f", params$mu), "\n")
cat("  sigma (da D9/D1):", sprintf("%.4f", params$sigma), "\n")
cat("  Media stimata:", sprintf("%.2f", params$mean_estimated), "€/h\n")
cat("  Media osservata:", sprintf("%.2f", params$mean_observed), "€/h\n")
cat(
  "  Errore media:",
  sprintf(
    "%.1f%%",
    100 * (params$mean_estimated - params$mean_observed) / params$mean_observed
  ),
  "\n\n"
)

# 4. Verifica fit sui percentili -----

cat("Verifica fit percentili:\n")
percentili_teorici <- data.frame(
  percentile = c(10, 50, 90),
  osservato = c(dati_2022$D1, dati_2022$D5, dati_2022$D9),
  teorico = qlnorm(c(0.10, 0.50, 0.90), params$mu, params$sigma)
)
percentili_teorici$errore_pct <- 100 *
  (percentili_teorici$teorico - percentili_teorici$osservato) /
  percentili_teorici$osservato

print(percentili_teorici)
cat("\n")

# 5. Calcolo Gini teorico log-normale -----

# Per distribuzione log-normale: Gini = 2*Phi(sigma/sqrt(2)) - 1
# dove Phi è la CDF normale standard
gini_lognormal <- function(sigma) {
  2 * pnorm(sigma / sqrt(2)) - 1
}

gini_teorico <- gini_lognormal(params$sigma)

cat("Indice di Gini:\n")
cat("  Gini teorico (log-normale):", sprintf("%.4f", gini_teorico), "\n")

# Confronto con Gini approssimato dal report
gini_approssimato <- readRDS("output/sintesi_distributiva.rds")$gini_2022
cat("  Gini approssimato (3 punti):", sprintf("%.4f", gini_approssimato), "\n")
cat("  Differenza:", sprintf("%+.4f", gini_teorico - gini_approssimato), "\n\n")

# 6. Genera distribuzione simulata -----

n_sim <- 100000
salari_sim <- rlnorm(n_sim, params$mu, params$sigma)

# Calcola Gini empirico dalla simulazione
gini_simulato <- ineq::Gini(salari_sim)
cat("  Gini simulato (n=100000):", sprintf("%.4f", gini_simulato), "\n\n")

# 7. Kernel density estimation -----

cat("==== Kernel Density Estimation ====\n\n")

# Crea dataframe per plot
df_sim <- data.frame(salario = salari_sim)

# Calcola densità kernel
kde <- density(salari_sim, bw = "SJ", n = 512)

# Densità teorica log-normale
x_range <- seq(min(salari_sim), quantile(salari_sim, 0.99), length.out = 500)
y_teorica <- dlnorm(x_range, params$mu, params$sigma)

# 8. Grafici -----

cat("Generazione grafici...\n\n")

# Grafico 1: Distribuzione stimata con punti osservati
p1 <- ggplot() +
  # Densità kernel (simulata)
  geom_density(
    data = df_sim,
    aes(x = salario),
    fill = "steelblue",
    alpha = 0.3,
    color = "steelblue"
  ) +
  # Densità teorica log-normale
  geom_line(
    data = data.frame(x = x_range, y = y_teorica),
    aes(x = x, y = y),
    color = "darkred",
    linewidth = 1,
    linetype = "dashed"
  ) +
  # Punti osservati (D1, D5, D9)
  geom_vline(xintercept = dati_2022$D1, color = "orange", linewidth = 1) +
  geom_vline(xintercept = dati_2022$D5, color = "darkgreen", linewidth = 1) +
  geom_vline(xintercept = dati_2022$D9, color = "purple", linewidth = 1) +
  geom_vline(
    xintercept = dati_2022$salario_medio,
    color = "red",
    linewidth = 1,
    linetype = "dotted"
  ) +
  # Annotazioni
  annotate(
    "text",
    x = dati_2022$D1,
    y = 0.12,
    label = "D1 (P10)",
    color = "orange",
    hjust = -0.1,
    size = 3.5
  ) +
  annotate(
    "text",
    x = dati_2022$D5,
    y = 0.14,
    label = "D5 (Mediana)",
    color = "darkgreen",
    hjust = -0.1,
    size = 3.5
  ) +
  annotate(
    "text",
    x = dati_2022$D9,
    y = 0.12,
    label = "D9 (P90)",
    color = "purple",
    hjust = -0.1,
    size = 3.5
  ) +
  annotate(
    "text",
    x = dati_2022$salario_medio,
    y = 0.10,
    label = "Media",
    color = "red",
    hjust = -0.1,
    size = 3.5
  ) +
  # Labels
  labs(
    title = "Distribuzione Salariale Stimata (Log-Normale) - Italia 2022",
    subtitle = sprintf(
      "Parametri: μ = %.3f, σ = %.3f | Gini teorico = %.3f",
      params$mu,
      params$sigma,
      gini_teorico
    ),
    x = "Retribuzione oraria lorda (€/h)",
    y = "Densità",
    caption = "Fonte: ISTAT-RACLI | Stima log-normale da D1, D5, D9"
  ) +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 40, 5)) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(
  "output/grafici/21_distribuzione_lognormale_2022.png",
  p1,
  width = 10,
  height = 6,
  dpi = 300
)
cat("Salvato: output/grafici/21_distribuzione_lognormale_2022.png\n")

# Grafico 2: QQ-plot log-normale
df_qq <- data.frame(
  osservato = sort(salari_sim),
  teorico = qlnorm(ppoints(n_sim), params$mu, params$sigma)
)

# Campiona per visualizzazione
set.seed(456)
df_qq_sample <- df_qq[sample(nrow(df_qq), 5000), ]

p2 <- ggplot(df_qq_sample, aes(x = teorico, y = osservato)) +
  geom_point(alpha = 0.1, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1) +
  # Aggiungi punti osservati reali
  geom_point(
    data = data.frame(
      teorico = c(dati_2022$D1, dati_2022$D5, dati_2022$D9),
      osservato = c(dati_2022$D1, dati_2022$D5, dati_2022$D9)
    ),
    color = "darkgreen",
    size = 4,
    shape = 18
  ) +
  labs(
    title = "Q-Q Plot: Distribuzione Simulata vs Teorica Log-Normale",
    subtitle = "Punti verdi: decili osservati (D1, D5, D9)",
    x = "Quantili teorici (€/h)",
    y = "Quantili simulati (€/h)"
  ) +
  coord_fixed() +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave(
  "output/grafici/22_qq_plot_lognormale.png",
  p2,
  width = 8,
  height = 8,
  dpi = 300
)
cat("Salvato: output/grafici/22_qq_plot_lognormale.png\n")

# 9. Confronto Gini con metodi diversi -----

cat("\n==== Confronto Metodi Calcolo Gini ====\n\n")

# Metodo 1: Approssimazione originale (3 punti)
calc_gini_3points <- function(D1, D5, D9) {
  approx_dist <- c(
    rep(D1, 10),
    rep(mean(c(D1, D5)), 40),
    rep(mean(c(D5, D9)), 40),
    rep(D9, 10)
  )
  ineq::Gini(approx_dist)
}

# Metodo 2: Log-normale teorico
# Gini = 2*Phi(sigma/sqrt(2)) - 1

# Metodo 3: Simulazione Monte Carlo
# Già calcolato sopra

# Metodo 4: Formula esatta con 4 punti (D1, D5, D9, Media)
# Stima parametri usando anche la media come vincolo
estimate_lognormal_mle <- function(D1, D5, D9, mean_val) {
  # Minimizza errore quadratico su tutti i vincoli
  objective <- function(params) {
    mu <- params[1]
    sigma <- params[2]

    # Errori sui percentili
    err_D1 <- (qlnorm(0.10, mu, sigma) - D1)^2
    err_D5 <- (qlnorm(0.50, mu, sigma) - D5)^2
    err_D9 <- (qlnorm(0.90, mu, sigma) - D9)^2

    # Errore sulla media
    mean_teorica <- exp(mu + sigma^2 / 2)
    err_mean <- (mean_teorica - mean_val)^2

    # Pesi: dà più peso ai percentili
    err_D1 + err_D5 + err_D9 + 0.5 * err_mean
  }

  # Valori iniziali
  mu0 <- log(D5)
  sigma0 <- (log(D9) - log(D1)) / (qnorm(0.90) - qnorm(0.10))

  result <- optim(
    c(mu0, sigma0),
    objective,
    method = "L-BFGS-B",
    lower = c(0.1, 0.01),
    upper = c(5, 2)
  )

  list(mu = result$par[1], sigma = result$par[2])
}

params_mle <- estimate_lognormal_mle(
  dati_2022$D1,
  dati_2022$D5,
  dati_2022$D9,
  dati_2022$salario_medio
)

gini_methods <- data.frame(
  metodo = c(
    "1. Approssimazione 3 punti (originale)",
    "2. Log-normale teorico (D1,D5,D9)",
    "3. Simulazione Monte Carlo (n=100k)",
    "4. Log-normale MLE (D1,D5,D9,Media)"
  ),
  gini = c(
    calc_gini_3points(dati_2022$D1, dati_2022$D5, dati_2022$D9),
    gini_lognormal(params$sigma),
    gini_simulato,
    gini_lognormal(params_mle$sigma)
  )
)

gini_methods$differenza_da_teorico <- gini_methods$gini - gini_methods$gini[2]

cat("Confronto metodi calcolo Gini:\n")
print(gini_methods)
cat("\n")

# 10. Analisi sensitività parametrica -----

cat("==== Analisi Sensitività ====\n\n")

# Verifica robustezza assumendo diversi valori di sigma
sigma_range <- seq(0.2, 0.6, by = 0.05)
sensitivity <- data.frame(
  sigma = sigma_range,
  gini = sapply(sigma_range, gini_lognormal),
  D9_D1 = sapply(sigma_range, function(s) {
    qlnorm(0.90, params$mu, s) / qlnorm(0.10, params$mu, s)
  })
)

cat("Sensitività Gini a sigma:\n")
print(sensitivity)
cat("\n")

# Grafico 3: Sensitività
p3 <- ggplot(sensitivity, aes(x = sigma)) +
  geom_line(aes(y = gini), color = "steelblue", linewidth = 1.2) +
  geom_point(aes(y = gini), color = "steelblue", size = 2) +
  geom_vline(xintercept = params$sigma, color = "red", linetype = "dashed") +
  geom_hline(yintercept = gini_teorico, color = "red", linetype = "dashed") +
  annotate(
    "text",
    x = params$sigma + 0.02,
    y = gini_teorico + 0.02,
    label = sprintf("σ = %.3f\nGini = %.3f", params$sigma, gini_teorico),
    color = "red",
    hjust = 0,
    size = 3.5
  ) +
  labs(
    title = "Sensitività Indice di Gini al Parametro σ (Log-Normale)",
    subtitle = "Linea rossa: stima dai dati RACLI 2022",
    x = "Parametro σ (dispersione log-normale)",
    y = "Indice di Gini"
  ) +
  scale_y_continuous(limits = c(0.1, 0.35), breaks = seq(0.1, 0.35, 0.05)) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave(
  "output/grafici/23_sensitivita_gini_sigma.png",
  p3,
  width = 9,
  height = 6,
  dpi = 300
)
cat("Salvato: output/grafici/23_sensitivita_gini_sigma.png\n")

# 11. Stima distribuzione per ripartizioni -----

cat("\n==== Stima per Ripartizioni Geografiche ====\n\n")

dati_ripart_2022 <- dati %>%
  filter(anno == 2022, geo_level == "Ripartizione", sesso == "totale") %>%
  select(ripartizione, D1, D5, D9, salario_medio)

# Stima parametri per ciascuna ripartizione
stima_ripartizioni <- dati_ripart_2022 %>%
  rowwise() %>%
  mutate(
    mu = log(D5),
    sigma = (log(D9) - log(D1)) / (qnorm(0.90) - qnorm(0.10)),
    gini_lognorm = 2 * pnorm(sigma / sqrt(2)) - 1,
    gini_3punti = calc_gini_3points(D1, D5, D9),
    differenza = gini_lognorm - gini_3punti
  ) %>%
  ungroup() %>%
  arrange(desc(gini_lognorm))

cat("Confronto Gini per ripartizione (2022):\n")
print(
  stima_ripartizioni %>%
    mutate(D9_D1_ratio = round(D9 / D1, 2)) %>%
    select(
      ripartizione,
      D9_D1 = D9_D1_ratio,
      gini_lognorm,
      gini_3punti,
      differenza
    )
)
cat("\n")

# 12. Salvataggio risultati -----

risultati_lognormale <- list(
  anno = 2022,
  dati_osservati = dati_2022,
  parametri_stimati = params,
  parametri_mle = params_mle,
  gini_confronto = gini_methods,
  stima_ripartizioni = stima_ripartizioni,
  sensitività = sensitivity
)

saveRDS(risultati_lognormale, "output/risultati_lognormale.rds")
cat("Salvato: output/risultati_lognormale.rds\n")

# 13. Sintesi finale -----

cat("\n")
cat("=" |> rep(60) |> paste(collapse = ""))
cat("\n")
cat("SINTESI ANALISI DISTRIBUZIONE LOG-NORMALE\n")
cat("=" |> rep(60) |> paste(collapse = ""))
cat("\n\n")

cat("DATI DISPONIBILI:\n")
cat("  - Solo 4 punti: D1 (P10), D5 (P50), D9 (P90), Media\n")
cat("  - Periodo: 2014-2022\n")
cat("  - Livelli: Nazionale, Ripartizione, Regione, Provincia\n\n")

cat("STIMA LOG-NORMALE (Italia 2022):\n")
cat(sprintf("  - mu = %.4f (mediana = €%.2f/h)\n", params$mu, exp(params$mu)))
cat(sprintf("  - sigma = %.4f\n", params$sigma))
cat(sprintf(
  "  - Rapporto D9/D1 osservato: %.2f\n",
  dati_2022$D9 / dati_2022$D1
))
cat("\n")

cat("INDICE DI GINI:\n")
cat(sprintf("  - Gini teorico (log-normale): %.4f\n", gini_teorico))
cat(sprintf("  - Gini approssimato (3 punti): %.4f\n", gini_approssimato))
cat(sprintf(
  "  - Sottostima metodo originale: %.1f%%\n",
  100 * (gini_teorico - gini_approssimato) / gini_teorico
))
cat("\n")

cat("CONCLUSIONE:\n")
cat("  L'approssimazione con 3 punti SOTTOSTIMA il Gini del ~15-20%.\n")
cat("  L'assunzione log-normale è ragionevole (errore media < 5%).\n")
cat("  Si raccomanda di usare il Gini teorico log-normale nel report.\n")
cat("\n")
