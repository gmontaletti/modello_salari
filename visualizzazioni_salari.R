# ==============================================================================
# VISUALIZZAZIONI MODELLO DETERMINANTI SALARI
# ==============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(scales)

# Tema personalizzato
theme_salari <- function() {
  theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(color = "grey40", size = 10),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
}

# ------------------------------------------------------------------------------
# 1. SERIE STORICHE PRINCIPALI
# ------------------------------------------------------------------------------

plot_time_series <- function(data) {
  # Salari reali vs Produttività
  p1 <- ggplot(data, aes(x = date)) +
    geom_line(aes(y = scale(w_real), color = "Salari Reali"), linewidth = 0.8) +
    geom_line(aes(y = scale(prod), color = "Produttività"), linewidth = 0.8) +
    scale_color_manual(
      values = c("Salari Reali" = "#E63946", "Produttività" = "#457B9D")
    ) +
    labs(
      title = "Salari Reali e Produttività in Italia",
      subtitle = "Serie standardizzate (media=0, sd=1)",
      x = NULL,
      y = "Valore standardizzato",
      color = NULL
    ) +
    theme_salari()

  # Disoccupazione vs Occupazione
  p2 <- ggplot(data, aes(x = date)) +
    geom_line(aes(y = u, color = "Disoccupazione"), linewidth = 0.8) +
    geom_line(
      aes(y = 100 - occ, color = "Non Occupazione"),
      linewidth = 0.8,
      linetype = "dashed"
    ) +
    scale_color_manual(
      values = c("Disoccupazione" = "#E63946", "Non Occupazione" = "#F4A261")
    ) +
    labs(
      title = "Indicatori del Mercato del Lavoro",
      subtitle = "Tasso di disoccupazione e non occupazione (%)",
      x = NULL,
      y = "%",
      color = NULL
    ) +
    theme_salari()

  # Cuneo fiscale e Precarietà
  p3 <- ggplot(data, aes(x = date)) +
    geom_line(aes(y = cuneo, color = "Cuneo Fiscale"), linewidth = 0.8) +
    geom_line(aes(y = prec * 3, color = "Precarietà (×3)"), linewidth = 0.8) +
    scale_color_manual(
      values = c("Cuneo Fiscale" = "#9B5DE5", "Precarietà (×3)" = "#F72585")
    ) +
    labs(
      title = "Variabili Istituzionali",
      subtitle = "Cuneo fiscale (%) e quota contratti a termine",
      x = NULL,
      y = "%",
      color = NULL
    ) +
    theme_salari()

  # Crescita salari reali
  p4 <- data %>%
    filter(!is.na(g_w_real)) %>%
    ggplot(aes(x = date, y = g_w_real * 100)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_line(color = "#E63946", linewidth = 0.8) +
    geom_smooth(method = "loess", span = 0.3, se = FALSE, color = "#1D3557") +
    labs(
      title = "Crescita Salari Reali (anno su anno)",
      subtitle = "Con trend LOESS",
      x = NULL,
      y = "%"
    ) +
    theme_salari()

  grid.arrange(p1, p2, p3, p4, ncol = 2)
}

# ------------------------------------------------------------------------------
# 2. RELAZIONE SALARI-PRODUTTIVITÀ
# ------------------------------------------------------------------------------

plot_wage_productivity <- function(data) {
  data_plot <- data %>%
    mutate(
      periodo = case_when(
        year <= 2007 ~ "1995-2007",
        year <= 2019 ~ "2008-2019",
        TRUE ~ "2020-2024"
      )
    )

  ggplot(data_plot, aes(x = prod, y = w_real)) +
    geom_point(aes(color = periodo), alpha = 0.6, size = 2) +
    geom_smooth(method = "lm", se = TRUE, color = "#1D3557", fill = "#457B9D") +
    scale_color_manual(
      values = c(
        "1995-2007" = "#2A9D8F",
        "2008-2019" = "#E9C46A",
        "2020-2024" = "#E63946"
      )
    ) +
    labs(
      title = "Relazione Salari Reali - Produttività",
      subtitle = "Evidenza di stagnazione congiunta dal 2008",
      x = "Produttività (€/ora)",
      y = "Salari Reali (indice)",
      color = "Periodo"
    ) +
    theme_salari()
}

# ------------------------------------------------------------------------------
# 3. CURVA DI PHILLIPS
# ------------------------------------------------------------------------------

plot_phillips_curve <- function(data) {
  data_plot <- data %>%
    filter(!is.na(g_w_real)) %>%
    mutate(
      periodo = case_when(
        year <= 2007 ~ "1995-2007",
        year <= 2019 ~ "2008-2019",
        TRUE ~ "2020-2024"
      )
    )

  ggplot(data_plot, aes(x = u, y = g_w_real * 100)) +
    geom_point(aes(color = periodo), alpha = 0.6, size = 2) +
    geom_smooth(method = "lm", se = TRUE, color = "#1D3557", fill = "#457B9D") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    scale_color_manual(
      values = c(
        "1995-2007" = "#2A9D8F",
        "2008-2019" = "#E9C46A",
        "2020-2024" = "#E63946"
      )
    ) +
    labs(
      title = "Curva di Phillips Salariale - Italia",
      subtitle = "Relazione disoccupazione-crescita salari reali",
      x = "Tasso di Disoccupazione (%)",
      y = "Crescita Salari Reali (%)",
      color = "Periodo"
    ) +
    theme_salari()
}

# ------------------------------------------------------------------------------
# 4. FUNZIONI DI RISPOSTA IMPULSIVA
# ------------------------------------------------------------------------------

plot_irf_custom <- function(irf_obj, title, shock_name, response_name) {
  # Estrai dati IRF
  irf_data <- data.frame(
    horizon = 0:(length(irf_obj$irf[[1]]) - 1),
    response = irf_obj$irf[[1]],
    lower = irf_obj$Lower[[1]],
    upper = irf_obj$Upper[[1]]
  )

  ggplot(irf_data, aes(x = horizon)) +
    geom_ribbon(
      aes(ymin = lower, ymax = upper),
      fill = "#457B9D",
      alpha = 0.3
    ) +
    geom_line(aes(y = response), color = "#E63946", linewidth = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    labs(
      title = title,
      subtitle = paste("Shock:", shock_name, "→ Risposta:", response_name),
      x = "Trimestri",
      y = "Risposta"
    ) +
    theme_salari()
}

# ------------------------------------------------------------------------------
# 5. DECOMPOSIZIONE VARIANZA
# ------------------------------------------------------------------------------

plot_fevd_custom <- function(fevd_obj, variable = "w") {
  fevd_data <- as.data.frame(fevd_obj[[variable]])
  fevd_data$horizon <- 1:nrow(fevd_data)

  fevd_long <- fevd_data %>%
    pivot_longer(
      cols = -horizon,
      names_to = "shock",
      values_to = "contribution"
    ) %>%
    mutate(
      shock = factor(
        shock,
        levels = c("w", "p", "prod", "u"),
        labels = c("Salari", "Prezzi", "Produttività", "Disoccupazione")
      )
    )

  ggplot(fevd_long, aes(x = horizon, y = contribution * 100, fill = shock)) +
    geom_area(alpha = 0.8) +
    scale_fill_manual(
      values = c(
        "Salari" = "#E63946",
        "Prezzi" = "#F4A261",
        "Produttività" = "#457B9D",
        "Disoccupazione" = "#2A9D8F"
      )
    ) +
    labs(
      title = "Decomposizione della Varianza - Salari",
      subtitle = "Contributo di ogni shock alla varianza dell'errore di previsione",
      x = "Orizzonte (trimestri)",
      y = "Contributo (%)",
      fill = "Fonte shock"
    ) +
    theme_salari()
}

# ------------------------------------------------------------------------------
# 6. CONFRONTO PERIODI
# ------------------------------------------------------------------------------

plot_period_comparison <- function(data) {
  period_stats <- data %>%
    mutate(
      periodo = case_when(
        year <= 2007 ~ "1995-2007",
        year <= 2019 ~ "2008-2019",
        TRUE ~ "2020-2024"
      )
    ) %>%
    group_by(periodo) %>%
    summarise(
      `Salario Reale` = mean(w_real, na.rm = TRUE),
      `Produttività` = mean(prod, na.rm = TRUE),
      `Disoccupazione` = mean(u, na.rm = TRUE),
      `Occupazione` = mean(occ, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_longer(-periodo, names_to = "indicatore", values_to = "valore")

  # Normalizza per confronto
  period_stats <- period_stats %>%
    group_by(indicatore) %>%
    mutate(
      valore_norm = (valore - min(valore)) / (max(valore) - min(valore)) * 100
    )

  ggplot(period_stats, aes(x = indicatore, y = valore_norm, fill = periodo)) +
    geom_col(position = "dodge", alpha = 0.8) +
    scale_fill_manual(
      values = c(
        "1995-2007" = "#2A9D8F",
        "2008-2019" = "#E9C46A",
        "2020-2024" = "#E63946"
      )
    ) +
    labs(
      title = "Confronto Indicatori per Periodo",
      subtitle = "Valori normalizzati (0-100)",
      x = NULL,
      y = "Indice normalizzato",
      fill = "Periodo"
    ) +
    theme_salari() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# ------------------------------------------------------------------------------
# 7. MATRICE CORRELAZIONI
# ------------------------------------------------------------------------------

plot_correlation_matrix <- function(data) {
  cor_data <- data %>%
    select(w_real, prod, u, occ, cuneo, prec, pil) %>%
    na.omit()

  cor_matrix <- cor(cor_data)

  # Usa corrplot se disponibile, altrimenti heatmap base
  if (require(corrplot, quietly = TRUE)) {
    corrplot(
      cor_matrix,
      method = "color",
      type = "upper",
      col = colorRampPalette(c("#E63946", "white", "#457B9D"))(200),
      addCoef.col = "black",
      number.cex = 0.7,
      tl.col = "black",
      tl.srt = 45,
      title = "Matrice Correlazioni",
      mar = c(0, 0, 2, 0)
    )
  } else {
    heatmap(
      cor_matrix,
      scale = "none",
      col = colorRampPalette(c("#E63946", "white", "#457B9D"))(100)
    )
  }
}

# ------------------------------------------------------------------------------
# ESECUZIONE VISUALIZZAZIONI
# ------------------------------------------------------------------------------

generate_all_plots <- function(data, irf_results = NULL, fevd_results = NULL) {
  cat("\nGenerazione grafici...\n")

  # Serie storiche
  cat("  - Serie storiche principali\n")
  plot_time_series(data)

  # Salari-Produttività
  cat("  - Relazione salari-produttività\n")
  print(plot_wage_productivity(data))

  # Curva di Phillips
  cat("  - Curva di Phillips\n")
  print(plot_phillips_curve(data))

  # Confronto periodi
  cat("  - Confronto periodi\n")
  print(plot_period_comparison(data))

  # IRF e FEVD se disponibili
  if (!is.null(irf_results)) {
    cat("  - Funzioni di risposta impulsiva\n")
    print(plot_irf_custom(
      irf_results$irf_prod,
      "Risposta Salari a Shock Produttività",
      "Produttività +1%",
      "Salari"
    ))
  }

  if (!is.null(fevd_results)) {
    cat("  - Decomposizione varianza\n")
    print(plot_fevd_custom(fevd_results))
  }

  cat("\nGrafici completati.\n")
}

# ==============================================================================
# NUOVE VISUALIZZAZIONI DIAGNOSTICHE AVANZATE
# ==============================================================================

# ------------------------------------------------------------------------------
# 8. CUSUM STABILITY PLOT
# ------------------------------------------------------------------------------

plot_cusum <- function(
  cusum_result,
  title = "Test CUSUM - Stabilità Parametrica"
) {
  if (!inherits(cusum_result, "efp")) {
    stop("Richiesto oggetto efp da strucchange::efp()")
  }

  # Extract CUSUM process
  cusum_values <- as.numeric(cusum_result$process)
  n <- length(cusum_values)
  fractions <- seq(0, 1, length.out = n)

  # Calculate boundaries (standard 5% significance)
  # Boundary formula: a + b * fraction
  a <- 0.948
  b <- 0.0
  boundary <- a + b * abs(fractions - 0.5)

  cusum_df <- data.frame(
    fraction = fractions,
    cusum = cusum_values,
    upper = boundary,
    lower = -boundary
  )

  ggplot(cusum_df, aes(x = fraction)) +
    geom_ribbon(
      aes(ymin = lower, ymax = upper),
      fill = "#F4A261",
      alpha = 0.3
    ) +
    geom_line(aes(y = cusum), color = "#E63946", linewidth = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    labs(
      title = title,
      subtitle = "Processo empirico di fluttuazione con bande di confidenza 5%",
      x = "Frazione del campione",
      y = "CUSUM"
    ) +
    theme_salari()
}

# ------------------------------------------------------------------------------
# 9. STRUCTURAL BREAK VISUALIZATION
# ------------------------------------------------------------------------------

plot_structural_breaks <- function(break_results, data) {
  if (!require("strucchange", quietly = TRUE)) {
    stop("Richiesto pacchetto strucchange")
  }

  data_model <- data %>%
    filter(!is.na(g_w_real)) %>%
    mutate(time_idx = 1:n())

  # Get F-statistics
  wage_eq <- g_w_real ~ g_prod + u + cuneo + prec

  tryCatch(
    {
      fstats <- Fstats(wage_eq, data = data_model, from = 0.15)

      fstats_df <- data.frame(
        date = data_model$date[as.integer(names(fstats$Fstats))],
        fstat = as.numeric(fstats$Fstats)
      )

      # Critical values
      cv_5 <- fstats$critval

      p <- ggplot(fstats_df, aes(x = date, y = fstat)) +
        geom_line(color = "#457B9D", linewidth = 0.8) +
        geom_hline(yintercept = cv_5, linetype = "dashed", color = "red") +
        annotate(
          "text",
          x = min(fstats_df$date),
          y = cv_5 + 2,
          label = "Valore critico 5%",
          hjust = 0,
          size = 3,
          color = "red"
        )

      # Add vertical lines for detected breaks
      if (!is.null(break_results$breakpoints$breakpoints)) {
        bp_indices <- na.omit(break_results$breakpoints$breakpoints)
        if (length(bp_indices) > 0) {
          bp_dates <- data_model$date[bp_indices]
          p <- p +
            geom_vline(
              xintercept = bp_dates,
              linetype = "dashed",
              color = "#E63946",
              linewidth = 1
            )
        }
      }

      p <- p +
        labs(
          title = "Rilevamento Break Strutturali (Bai-Perron)",
          subtitle = "Statistiche F sequenziali con date di break stimate",
          x = NULL,
          y = "Statistica F"
        ) +
        theme_salari()

      return(p)
    },
    error = function(e) {
      message("Errore nel calcolo F-statistics: ", e$message)
      return(NULL)
    }
  )
}

# ------------------------------------------------------------------------------
# 10. ROLLING COEFFICIENT EVOLUTION
# ------------------------------------------------------------------------------

plot_rolling_coefficients <- function(rolling_results) {
  if (is.null(rolling_results) || nrow(rolling_results) == 0) {
    message("Nessun risultato rolling disponibile")
    return(NULL)
  }

  # Filter out extreme outliers for visualization
  rolling_clean <- rolling_results %>%
    filter(
      !is.na(beta_prod),
      abs(beta_prod) < 10,
      abs(beta_u) < 1
    )

  if (nrow(rolling_clean) < 5) {
    message("Dati insufficienti dopo pulizia outlier")
    return(NULL)
  }

  p1 <- ggplot(rolling_clean, aes(x = end_date)) +
    geom_line(aes(y = beta_prod), color = "#457B9D", linewidth = 1) +
    geom_hline(yintercept = 0.85, linetype = "dashed", color = "grey50") +
    geom_vline(
      xintercept = as.Date(c("2008-09-01", "2012-06-01", "2020-03-01")),
      linetype = "dotted",
      color = "red",
      alpha = 0.7
    ) +
    annotate(
      "text",
      x = as.Date("2008-09-01"),
      y = max(rolling_clean$beta_prod, na.rm = TRUE),
      label = "Crisi 2008",
      hjust = 0,
      vjust = 1,
      size = 2.5
    ) +
    labs(
      title = "Evoluzione Elasticità Salari-Produttività",
      subtitle = "Stime VECM Rolling-Window (60 trimestri)",
      x = NULL,
      y = "Elasticità β(prod)"
    ) +
    theme_salari()

  p2 <- ggplot(rolling_clean, aes(x = end_date)) +
    geom_line(aes(y = beta_u), color = "#E63946", linewidth = 1) +
    geom_hline(yintercept = -0.015, linetype = "dashed", color = "grey50") +
    geom_vline(
      xintercept = as.Date(c("2008-09-01", "2012-06-01", "2020-03-01")),
      linetype = "dotted",
      color = "red",
      alpha = 0.7
    ) +
    labs(
      title = "Evoluzione Semi-elasticità Salari-Disoccupazione",
      subtitle = "Stime VECM Rolling-Window (60 trimestri)",
      x = NULL,
      y = "Semi-elasticità β(u)"
    ) +
    theme_salari()

  p3 <- ggplot(rolling_clean, aes(x = end_date)) +
    geom_line(aes(y = alpha_w), color = "#2A9D8F", linewidth = 1) +
    geom_hline(yintercept = -0.15, linetype = "dashed", color = "grey50") +
    labs(
      title = "Evoluzione Velocità Aggiustamento Salari",
      subtitle = "Coefficiente α(w) - convergenza a equilibrio",
      x = NULL,
      y = "Velocità α(w)"
    ) +
    theme_salari()

  grid.arrange(p1, p2, p3, ncol = 1)
}

# ------------------------------------------------------------------------------
# 11. EIGENVALUE STABILITY PLOT
# ------------------------------------------------------------------------------

plot_eigenvalues <- function(var_model) {
  # Extract eigenvalues from companion matrix
  if (inherits(var_model, "vec2var")) {
    A_mats <- var_model$A
    K <- var_model$K
    p <- var_model$p

    # Build companion matrix
    companion <- matrix(0, K * p, K * p)
    for (i in 1:p) {
      companion[1:K, ((i - 1) * K + 1):(i * K)] <- A_mats[[i]]
    }
    if (p > 1) {
      companion[(K + 1):(K * p), 1:(K * (p - 1))] <- diag(K * (p - 1))
    }
    eigenvalues <- eigen(companion)$values
  } else if (inherits(var_model, "varest")) {
    eigenvalues <- roots(var_model, modulus = FALSE)
  } else {
    stop("Oggetto non supportato: richiesto vec2var o varest")
  }

  eig_df <- data.frame(
    real = Re(eigenvalues),
    imag = Im(eigenvalues),
    modulus = Mod(eigenvalues)
  )

  # Unit circle
  circle_df <- data.frame(
    x = cos(seq(0, 2 * pi, length.out = 100)),
    y = sin(seq(0, 2 * pi, length.out = 100))
  )

  ggplot() +
    geom_path(
      data = circle_df,
      aes(x = x, y = y),
      linetype = "dashed",
      color = "grey50"
    ) +
    geom_point(
      data = eig_df,
      aes(x = real, y = imag, color = modulus),
      size = 4
    ) +
    scale_color_gradient2(
      low = "#2A9D8F",
      mid = "#F4A261",
      high = "#E63946",
      midpoint = 0.9,
      limits = c(0, 1.1)
    ) +
    coord_fixed(xlim = c(-1.3, 1.3), ylim = c(-1.3, 1.3)) +
    labs(
      title = "Autovalori Matrice Companion",
      subtitle = "Stabilità VECM: radici dentro cerchio unitario = stabilità",
      x = "Parte Reale",
      y = "Parte Immaginaria",
      color = "Modulo"
    ) +
    theme_salari() +
    theme(legend.position = "right")
}

# ------------------------------------------------------------------------------
# 12. RESIDUAL DIAGNOSTICS PANEL
# ------------------------------------------------------------------------------

plot_residual_diagnostics <- function(var_model) {
  # Extract residuals
  if (inherits(var_model, "vec2var")) {
    resids <- residuals(var_model)
  } else if (inherits(var_model, "varest")) {
    resids <- residuals(var_model)
  } else {
    stop("Oggetto non supportato")
  }

  # Focus on wage equation residuals
  w_resid <- resids[, 1]
  resid_df <- data.frame(
    t = 1:length(w_resid),
    residual = w_resid
  )

  # Time series plot
  p1 <- ggplot(resid_df, aes(x = t, y = residual)) +
    geom_line(color = "#457B9D") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    labs(title = "Residui nel tempo", x = "Osservazione", y = "Residuo") +
    theme_salari()

  # Histogram with normal curve
  p2 <- ggplot(resid_df, aes(x = residual)) +
    geom_histogram(
      aes(y = after_stat(density)),
      bins = 25,
      fill = "#457B9D",
      color = "white",
      alpha = 0.7
    ) +
    stat_function(
      fun = dnorm,
      args = list(mean = mean(w_resid), sd = sd(w_resid)),
      color = "#E63946",
      linewidth = 1
    ) +
    labs(title = "Distribuzione residui", x = "Residuo", y = "Densità") +
    theme_salari()

  # Q-Q plot
  p3 <- ggplot(resid_df, aes(sample = residual)) +
    stat_qq(color = "#457B9D") +
    stat_qq_line(color = "#E63946") +
    labs(
      title = "Q-Q Plot Normalità",
      x = "Quantili teorici",
      y = "Quantili campionari"
    ) +
    theme_salari()

  # ACF plot
  acf_data <- acf(w_resid, plot = FALSE, lag.max = 20)
  acf_df <- data.frame(lag = acf_data$lag[-1], acf = acf_data$acf[-1])
  ci <- qnorm(0.975) / sqrt(length(w_resid))

  p4 <- ggplot(acf_df, aes(x = lag, y = acf)) +
    geom_hline(yintercept = c(-ci, ci), linetype = "dashed", color = "red") +
    geom_hline(yintercept = 0, color = "grey50") +
    geom_segment(aes(xend = lag, yend = 0), color = "#457B9D") +
    geom_point(color = "#457B9D", size = 2) +
    labs(title = "ACF Residui", x = "Lag", y = "Autocorrelazione") +
    theme_salari()

  grid.arrange(p1, p2, p3, p4, ncol = 2)
}

# ------------------------------------------------------------------------------
# 13. FORECAST FAN CHART
# ------------------------------------------------------------------------------

plot_forecast_fan <- function(var_model, data, n_ahead = 12) {
  # Generate forecasts
  fcast <- predict(var_model, n.ahead = n_ahead, ci = 0.95)

  # Extract wage forecasts
  w_fcast <- data.frame(
    horizon = 1:n_ahead,
    mean = fcast$fcst$w[, "fcst"],
    lower95 = fcast$fcst$w[, "lower"],
    upper95 = fcast$fcst$w[, "upper"]
  )

  # Add intermediate intervals (approximation)
  w_fcast$lower80 <- w_fcast$mean -
    1.28 * (w_fcast$mean - w_fcast$lower95) / 1.96
  w_fcast$upper80 <- w_fcast$mean +
    1.28 * (w_fcast$upper95 - w_fcast$mean) / 1.96
  w_fcast$lower50 <- w_fcast$mean -
    0.67 * (w_fcast$mean - w_fcast$lower95) / 1.96
  w_fcast$upper50 <- w_fcast$mean +
    0.67 * (w_fcast$upper95 - w_fcast$mean) / 1.96

  # Historical data (last 40 observations)
  data_clean <- data %>%
    filter(!is.na(log_w)) %>%
    tail(40)

  hist_df <- data.frame(
    horizon = seq(-nrow(data_clean) + 1, 0),
    value = data_clean$log_w
  )

  ggplot() +
    # Historical line
    geom_line(
      data = hist_df,
      aes(x = horizon, y = value),
      color = "#457B9D",
      linewidth = 0.8
    ) +
    # Fan chart ribbons
    geom_ribbon(
      data = w_fcast,
      aes(x = horizon, ymin = lower95, ymax = upper95),
      fill = "#E63946",
      alpha = 0.2
    ) +
    geom_ribbon(
      data = w_fcast,
      aes(x = horizon, ymin = lower80, ymax = upper80),
      fill = "#E63946",
      alpha = 0.3
    ) +
    geom_ribbon(
      data = w_fcast,
      aes(x = horizon, ymin = lower50, ymax = upper50),
      fill = "#E63946",
      alpha = 0.4
    ) +
    # Forecast line
    geom_line(
      data = w_fcast,
      aes(x = horizon, y = mean),
      color = "#E63946",
      linewidth = 1
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    labs(
      title = "Previsioni Salari con Fan Chart",
      subtitle = "Intervalli di confidenza al 50%, 80% e 95%",
      x = "Orizzonte (trimestri da oggi)",
      y = "Log Salari Nominali"
    ) +
    theme_salari()
}

# ------------------------------------------------------------------------------
# 14. IRF FAN CHART (IMPROVED)
# ------------------------------------------------------------------------------

plot_irf_fan <- function(irf_obj, title = "Risposta Impulsiva") {
  # Extract data
  irf_data <- data.frame(
    horizon = 0:(length(irf_obj$irf[[1]]) - 1),
    response = irf_obj$irf[[1]],
    lower95 = irf_obj$Lower[[1]],
    upper95 = irf_obj$Upper[[1]]
  )

  # Approximate intermediate bands
  irf_data$lower80 <- irf_data$response -
    1.28 * (irf_data$response - irf_data$lower95) / 1.96
  irf_data$upper80 <- irf_data$response +
    1.28 * (irf_data$upper95 - irf_data$response) / 1.96
  irf_data$lower50 <- irf_data$response -
    0.67 * (irf_data$response - irf_data$lower95) / 1.96
  irf_data$upper50 <- irf_data$response +
    0.67 * (irf_data$upper95 - irf_data$response) / 1.96

  ggplot(irf_data, aes(x = horizon)) +
    geom_ribbon(
      aes(ymin = lower95, ymax = upper95),
      fill = "#457B9D",
      alpha = 0.2
    ) +
    geom_ribbon(
      aes(ymin = lower80, ymax = upper80),
      fill = "#457B9D",
      alpha = 0.3
    ) +
    geom_ribbon(
      aes(ymin = lower50, ymax = upper50),
      fill = "#457B9D",
      alpha = 0.4
    ) +
    geom_line(aes(y = response), color = "#E63946", linewidth = 1.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    labs(
      title = title,
      subtitle = "Intervalli di confidenza bootstrap al 50%, 80% e 95%",
      x = "Trimestri",
      y = "Risposta"
    ) +
    theme_salari()
}

# ------------------------------------------------------------------------------
# 15. COINTEGRATION RANK SENSITIVITY HEATMAP
# ------------------------------------------------------------------------------

plot_coint_sensitivity <- function(sensitivity_results) {
  if (is.null(sensitivity_results$lag_sensitivity)) {
    message("Risultati sensitività non disponibili")
    return(NULL)
  }

  sens_long <- sensitivity_results$lag_sensitivity %>%
    pivot_longer(
      cols = c(rank_trace, rank_eigen),
      names_to = "test",
      values_to = "rank"
    ) %>%
    mutate(test = ifelse(test == "rank_trace", "Trace", "Max Eigenvalue"))

  ggplot(sens_long, aes(x = as.factor(lag), y = test, fill = as.factor(rank))) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = rank), color = "white", size = 5, fontface = "bold") +
    scale_fill_manual(
      values = c(
        "0" = "#E63946",
        "1" = "#F4A261",
        "2" = "#2A9D8F",
        "3" = "#457B9D",
        "4" = "#1D3557"
      ),
      name = "Rango"
    ) +
    labs(
      title = "Sensitività Rango Cointegrazione",
      subtitle = "Rango stimato per diversi ritardi e test",
      x = "Ritardi (K)",
      y = "Test"
    ) +
    theme_salari() +
    theme(legend.position = "right")
}

# ==============================================================================
# FUNZIONE MASTER PER TUTTE LE VISUALIZZAZIONI
# ==============================================================================

generate_all_plots <- function(
  data,
  irf_results = NULL,
  fevd_results = NULL,
  stability_results = NULL,
  break_results = NULL,
  rolling_results = NULL,
  var_model = NULL,
  sensitivity_results = NULL
) {
  cat("\nGenerazione grafici...\n")

  # Serie storiche
  cat("  - Serie storiche principali\n")
  plot_time_series(data)

  # Salari-Produttività
  cat("  - Relazione salari-produttività\n")
  print(plot_wage_productivity(data))

  # Curva di Phillips
  cat("  - Curva di Phillips\n")
  print(plot_phillips_curve(data))

  # Confronto periodi
  cat("  - Confronto periodi\n")
  print(plot_period_comparison(data))

  # IRF e FEVD se disponibili
  if (!is.null(irf_results)) {
    cat("  - Funzioni di risposta impulsiva (fan chart)\n")
    print(plot_irf_fan(
      irf_results$irf_prod,
      "Risposta Salari a Shock Produttività"
    ))
  }

  if (!is.null(fevd_results)) {
    cat("  - Decomposizione varianza\n")
    print(plot_fevd_custom(fevd_results))
  }

  # Nuove visualizzazioni diagnostiche
  if (!is.null(stability_results)) {
    cat("  - CUSUM stability plot\n")
    tryCatch(
      {
        print(plot_cusum(stability_results$ols_cusum$process))
      },
      error = function(e) message("CUSUM plot non generabile: ", e$message)
    )
  }

  if (!is.null(break_results)) {
    cat("  - Structural breaks\n")
    tryCatch(
      {
        p <- plot_structural_breaks(break_results, data)
        if (!is.null(p)) print(p)
      },
      error = function(e) message("Break plot non generabile: ", e$message)
    )
  }

  if (!is.null(rolling_results)) {
    cat("  - Rolling coefficients\n")
    tryCatch(
      {
        plot_rolling_coefficients(rolling_results)
      },
      error = function(e) message("Rolling plot non generabile: ", e$message)
    )
  }

  if (!is.null(var_model)) {
    cat("  - Eigenvalue stability\n")
    tryCatch(
      {
        print(plot_eigenvalues(var_model))
      },
      error = function(e) message("Eigenvalue plot non generabile: ", e$message)
    )

    cat("  - Residual diagnostics\n")
    tryCatch(
      {
        plot_residual_diagnostics(var_model)
      },
      error = function(e) message("Residual plot non generabile: ", e$message)
    )

    cat("  - Forecast fan chart\n")
    tryCatch(
      {
        print(plot_forecast_fan(var_model, data))
      },
      error = function(e) message("Forecast plot non generabile: ", e$message)
    )
  }

  if (!is.null(sensitivity_results)) {
    cat("  - Cointegration sensitivity\n")
    tryCatch(
      {
        p <- plot_coint_sensitivity(sensitivity_results)
        if (!is.null(p)) print(p)
      },
      error = function(e) {
        message("Sensitivity plot non generabile: ", e$message)
      }
    )
  }

  cat("\nGrafici completati.\n")
}

# Esempio di utilizzo (richiede dati generati dallo script principale):
# generate_all_plots(
#   data = data_istat,
#   irf_results = irf_results,
#   fevd_results = fevd_results,
#   stability_results = stability_results,
#   break_results = break_results,
#   rolling_results = rolling_results,
#   var_model = vecm_results$var,
#   sensitivity_results = sensitivity_results
# )

cat(
  "Script visualizzazioni caricato. Usare generate_all_plots(data, ...) per generare i grafici.\n"
)
