# ==============================================================================
# VISUALIZZAZIONI MODELLO DETERMINANTI SALARI
# ==============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(patchwork)
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
  # Calcola variabili derivate se mancanti
  if (
    !"log_w_real" %in% names(data) && all(c("log_w", "log_p") %in% names(data))
  ) {
    data <- data %>%
      mutate(log_w_real = log_w - log_p)
  }

  if (!"w_real" %in% names(data) && "log_w_real" %in% names(data)) {
    data <- data %>%
      mutate(w_real = exp(log_w_real))
  }

  if (!"prod" %in% names(data) && "log_prod" %in% names(data)) {
    data <- data %>%
      mutate(prod = exp(log_prod))
  }

  if (!"g_w_real" %in% names(data) && "log_w_real" %in% names(data)) {
    data <- data %>%
      mutate(g_w_real = c(rep(NA, 4), diff(log_w_real, 4)))
  }

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
  # Calcola variabili derivate se mancanti
  if (
    !"log_w_real" %in% names(data) && all(c("log_w", "log_p") %in% names(data))
  ) {
    data <- data %>%
      mutate(log_w_real = log_w - log_p)
  }

  if (!"w_real" %in% names(data) && "log_w_real" %in% names(data)) {
    data <- data %>%
      mutate(w_real = exp(log_w_real))
  }

  if (!"prod" %in% names(data) && "log_prod" %in% names(data)) {
    data <- data %>%
      mutate(prod = exp(log_prod))
  }

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
  # Calcola variabili derivate se mancanti
  if (
    !"log_w_real" %in% names(data) && all(c("log_w", "log_p") %in% names(data))
  ) {
    data <- data %>%
      mutate(log_w_real = log_w - log_p)
  }

  if (!"g_w_real" %in% names(data) && "log_w_real" %in% names(data)) {
    data <- data %>%
      mutate(g_w_real = c(rep(NA, 4), diff(log_w_real, 4)))
  }

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
  # Calcola variabili derivate se mancanti
  if (
    !"log_w_real" %in% names(data) && all(c("log_w", "log_p") %in% names(data))
  ) {
    data <- data %>%
      mutate(log_w_real = log_w - log_p)
  }

  if (!"w_real" %in% names(data) && "log_w_real" %in% names(data)) {
    data <- data %>%
      mutate(w_real = exp(log_w_real))
  }

  if (!"prod" %in% names(data) && "log_prod" %in% names(data)) {
    data <- data %>%
      mutate(prod = exp(log_prod))
  }

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
  # Calcola variabili derivate se mancanti
  if (
    !"log_w_real" %in% names(data) && all(c("log_w", "log_p") %in% names(data))
  ) {
    data <- data %>%
      mutate(log_w_real = log_w - log_p)
  }

  if (!"w_real" %in% names(data) && "log_w_real" %in% names(data)) {
    data <- data %>%
      mutate(w_real = exp(log_w_real))
  }

  if (!"prod" %in% names(data) && "log_prod" %in% names(data)) {
    data <- data %>%
      mutate(prod = exp(log_prod))
  }

  if (!"pil" %in% names(data) && "log_pil" %in% names(data)) {
    data <- data %>%
      mutate(pil = exp(log_pil))
  }

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

  # Calcola variabili derivate se mancanti
  if (
    !"log_w_real" %in% names(data) && all(c("log_w", "log_p") %in% names(data))
  ) {
    data <- data %>%
      mutate(log_w_real = log_w - log_p)
  }

  if (!"g_w_real" %in% names(data) && "log_w_real" %in% names(data)) {
    data <- data %>%
      mutate(g_w_real = c(rep(NA, 4), diff(log_w_real, 4)))
  }

  if (!"g_prod" %in% names(data) && "log_prod" %in% names(data)) {
    data <- data %>%
      mutate(g_prod = c(rep(NA, 4), diff(log_prod, 4)))
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
  # Extract the impulse variable name (first element of named list)
  impulse_name <- names(irf_obj$irf)[1]

  # Get the response vector and CI bounds
  response_vec <- irf_obj$irf[[impulse_name]]
  lower_vec <- irf_obj$Lower[[impulse_name]]
  upper_vec <- irf_obj$Upper[[impulse_name]]

  # Handle case where response is a matrix (multiple response variables)
  if (is.matrix(response_vec)) {
    response_vec <- response_vec[, 1]
    lower_vec <- lower_vec[, 1]
    upper_vec <- upper_vec[, 1]
  }

  # Check if CI bounds are available
  has_ci <- length(lower_vec) > 0 && length(upper_vec) > 0

  # Build data frame
  irf_data <- data.frame(
    horizon = 0:(length(response_vec) - 1),
    response = as.numeric(response_vec)
  )

  if (has_ci) {
    irf_data$lower95 <- as.numeric(lower_vec)
    irf_data$upper95 <- as.numeric(upper_vec)

    # Approximate intermediate bands
    irf_data$lower80 <- irf_data$response -
      1.28 * (irf_data$response - irf_data$lower95) / 1.96
    irf_data$upper80 <- irf_data$response +
      1.28 * (irf_data$upper95 - irf_data$response) / 1.96
    irf_data$lower50 <- irf_data$response -
      0.67 * (irf_data$response - irf_data$lower95) / 1.96
    irf_data$upper50 <- irf_data$response +
      0.67 * (irf_data$upper95 - irf_data$response) / 1.96

    p <- ggplot(irf_data, aes(x = horizon)) +
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
  } else {
    # No CI available - plot response only
    p <- ggplot(irf_data, aes(x = horizon, y = response)) +
      geom_line(color = "#E63946", linewidth = 1.2) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
      labs(
        title = title,
        subtitle = "Risposta impulsiva (senza intervalli di confidenza)",
        x = "Trimestri",
        y = "Risposta"
      ) +
      theme_salari()
  }

  return(p)
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

# ==============================================================================
# VISUALIZZAZIONI TVP-VAR
# ==============================================================================

# ------------------------------------------------------------------------------
# 16. COEFFICIENTI TVP NEL TEMPO
# ------------------------------------------------------------------------------

plot_tvp_coefficients <- function(
  tvp_coefs,
  coef_name = "elasticity_prod",
  title = "Evoluzione Coefficienti TVP-VAR"
) {
  if (is.null(tvp_coefs) || !coef_name %in% names(tvp_coefs)) {
    message("Coefficiente ", coef_name, " non disponibile")
    return(NULL)
  }

  # Prepara dati
  plot_df <- data.frame(
    date = tvp_coefs$date,
    value = tvp_coefs[[coef_name]]
  )

  # Calcola media mobile per smoothing
  plot_df$smooth <- zoo::rollmean(
    plot_df$value,
    k = 4,
    fill = NA,
    align = "center"
  )

  # Date di crisi per annotazioni
  crisis_dates <- as.Date(c("2008-09-15", "2012-06-01", "2020-03-01"))
  crisis_labels <- c("Lehman", "Crisi debito", "COVID")

  p <- ggplot(plot_df, aes(x = date)) +
    geom_line(aes(y = value), color = "#457B9D", alpha = 0.4, linewidth = 0.5) +
    geom_line(aes(y = smooth), color = "#E63946", linewidth = 1.2) +
    geom_vline(
      xintercept = crisis_dates,
      linetype = "dashed",
      color = "grey50",
      alpha = 0.7
    ) +
    annotate(
      "text",
      x = crisis_dates,
      y = max(plot_df$value, na.rm = TRUE),
      label = crisis_labels,
      hjust = 0,
      vjust = 1,
      size = 2.5,
      angle = 90
    ) +
    labs(
      title = title,
      subtitle = paste("Coefficiente:", coef_name),
      x = NULL,
      y = "Valore coefficiente"
    ) +
    theme_salari()

  return(p)
}

# ------------------------------------------------------------------------------
# 17. HEATMAP COEFFICIENTI TVP
# ------------------------------------------------------------------------------

plot_tvp_heatmap <- function(tvp_coefs, exclude_cols = c("date", "periodo")) {
  if (is.null(tvp_coefs)) {
    message("Coefficienti TVP non disponibili")
    return(NULL)
  }

  # Seleziona solo colonne numeriche
  numeric_cols <- setdiff(names(tvp_coefs), exclude_cols)
  numeric_cols <- numeric_cols[sapply(tvp_coefs[numeric_cols], is.numeric)]

  if (length(numeric_cols) == 0) {
    message("Nessun coefficiente numerico trovato")
    return(NULL)
  }

  # Prepara matrice
  coef_matrix <- as.matrix(tvp_coefs[, numeric_cols])

  # Normalizza per visualizzazione
  coef_scaled <- scale(coef_matrix)

  # Converti in formato long
  coef_long <- data.frame(
    date = rep(tvp_coefs$date, ncol(coef_scaled)),
    coefficient = rep(numeric_cols, each = nrow(coef_scaled)),
    value = as.vector(coef_scaled)
  )

  p <- ggplot(coef_long, aes(x = date, y = coefficient, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(
      low = "#2A9D8F",
      mid = "white",
      high = "#E63946",
      midpoint = 0,
      name = "Valore\n(standardizzato)"
    ) +
    labs(
      title = "Evoluzione Coefficienti TVP-VAR",
      subtitle = "Heatmap valori standardizzati nel tempo",
      x = NULL,
      y = "Coefficiente"
    ) +
    theme_salari() +
    theme(
      axis.text.y = element_text(size = 8),
      legend.position = "right"
    )

  return(p)
}

# ------------------------------------------------------------------------------
# 18. ELASTICITÀ SALARI-PRODUTTIVITÀ NEL TEMPO
# ------------------------------------------------------------------------------

plot_tvp_elasticity <- function(
  tvp_coefs,
  vecm_elasticity = NULL,
  title = "Risposta dei Salari alla Produttività"
) {
  if (is.null(tvp_coefs) || !"elasticity_prod" %in% names(tvp_coefs)) {
    message("Coefficienti TVP non disponibili")
    return(NULL)
  }

  plot_df <- data.frame(
    date = tvp_coefs$date,
    tvp = tvp_coefs$elasticity_prod
  )

  # Aggiungi media mobile
  plot_df$tvp_smooth <- zoo::rollmean(
    plot_df$tvp,
    k = 8,
    fill = NA,
    align = "center"
  )

  # Calcola banda proporzionale ai dati
  data_sd <- sd(plot_df$tvp, na.rm = TRUE)
  ribbon_width <- max(0.01, data_sd * 0.5)

  p <- ggplot(plot_df, aes(x = date)) +
    geom_ribbon(
      aes(
        ymin = tvp_smooth - ribbon_width,
        ymax = tvp_smooth + ribbon_width
      ),
      fill = "#457B9D",
      alpha = 0.2
    ) +
    geom_line(aes(y = tvp), color = "#457B9D", alpha = 0.3, linewidth = 0.5) +
    geom_line(aes(y = tvp_smooth), color = "#457B9D", linewidth = 1.2) +
    geom_hline(
      yintercept = 0,
      linetype = "solid",
      color = "grey30",
      linewidth = 0.5
    )

  # Aggiungi riferimento VECM se disponibile (nota: scala diversa)
  if (!is.null(vecm_elasticity)) {
    p <- p +
      geom_hline(
        yintercept = vecm_elasticity,
        linetype = "dashed",
        color = "#E63946",
        linewidth = 1
      ) +
      annotate(
        "text",
        x = min(plot_df$date),
        y = vecm_elasticity + 0.05,
        label = paste("VECM lungo periodo:", round(vecm_elasticity, 2)),
        hjust = 0,
        color = "#E63946",
        size = 3
      )
  }

  # Annotazioni crisi
  crisis_dates <- as.Date(c("2008-09-15", "2012-06-01", "2020-03-01"))

  p <- p +
    geom_vline(
      xintercept = crisis_dates,
      linetype = "dotted",
      color = "grey50",
      alpha = 0.7
    ) +
    labs(
      title = title,
      subtitle = "Coefficienti TVP-VAR con media mobile (8 trimestri)",
      x = NULL,
      y = "Coefficiente risposta (trimestrale)"
    ) +
    theme_salari()

  return(p)
}

# ------------------------------------------------------------------------------
# 19. IRF PER PERIODI SELEZIONATI
# ------------------------------------------------------------------------------

plot_tvp_irf_selected <- function(
  tvp_irf_results,
  title = "IRF Tempo-Varianti: Shock Produttività → Salari"
) {
  if (is.null(tvp_irf_results) || is.null(tvp_irf_results$irf)) {
    message("IRF TVP non disponibili")
    return(NULL)
  }

  irf_list <- tvp_irf_results$irf
  n_ahead <- tvp_irf_results$n_ahead

  # Costruisci dataframe con tutti gli IRF
  irf_df <- data.frame()

  for (period_name in names(irf_list)) {
    irf_entry <- irf_list[[period_name]]

    # Estrai oggetto tvirf e indice temporale dalla nuova struttura
    irf_obj <- irf_entry$tvirf
    time_idx <- irf_entry$time_idx

    # Estrai valori IRF dalla struttura tvIRF
    # La struttura è: irf_obj$irf$<impulse_name>[time, 1, horizon]
    if (!is.null(irf_obj) && inherits(irf_obj, "tvirf")) {
      impulse_name <- irf_obj$impulse

      irf_values <- tryCatch(
        {
          # Estrai la matrice IRF per l'impulso
          irf_array <- irf_obj$irf[[impulse_name]]

          # L'array ha dimensioni [n_obs, 1, n_ahead+1]
          # Usa l'indice temporale corretto salvato durante il calcolo IRF
          # (non mid_idx che dava lo stesso valore per tutti i periodi)
          as.numeric(irf_array[time_idx, 1, ])
        },
        error = function(e) {
          message(paste(
            "Errore estrazione IRF per",
            period_name,
            ":",
            e$message
          ))
          return(NULL)
        }
      )

      if (!is.null(irf_values) && length(irf_values) > 0) {
        temp_df <- data.frame(
          horizon = 0:(length(irf_values) - 1),
          response = irf_values,
          period = period_name
        )
        irf_df <- rbind(irf_df, temp_df)
      }
    }
  }

  if (nrow(irf_df) == 0) {
    message("Nessun IRF valido trovato")
    return(NULL)
  }

  # Colori per periodi
  n_periods <- length(unique(irf_df$period))
  colors <- c("#2A9D8F", "#E9C46A", "#F4A261", "#E63946")[1:n_periods]

  p <- ggplot(irf_df, aes(x = horizon, y = response, color = period)) +
    geom_line(linewidth = 1.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    scale_color_manual(values = colors) +
    labs(
      title = title,
      subtitle = "Confronto risposta a shock unitario in diversi periodi",
      x = "Trimestri",
      y = "Risposta",
      color = "Periodo"
    ) +
    theme_salari() +
    theme(legend.position = "bottom")

  return(p)
}

# ------------------------------------------------------------------------------
# 20. CONFRONTO VECM vs TVP-VAR
# ------------------------------------------------------------------------------

plot_tvp_comparison <- function(
  tvp_coefs,
  rolling_results = NULL,
  vecm_elasticity = 0.85,
  title = "Confronto Stime: VECM vs Rolling vs TVP-VAR"
) {
  if (is.null(tvp_coefs)) {
    message("Coefficienti TVP non disponibili")
    return(NULL)
  }

  # Prepara dati TVP
  tvp_df <- data.frame(
    date = tvp_coefs$date,
    value = tvp_coefs$elasticity_prod,
    method = "TVP-VAR"
  )

  # Aggiungi rolling se disponibile
  if (!is.null(rolling_results) && "beta_prod" %in% names(rolling_results)) {
    rolling_df <- data.frame(
      date = rolling_results$end_date,
      value = rolling_results$beta_prod,
      method = "Rolling VECM"
    )
    plot_df <- rbind(tvp_df, rolling_df)
  } else {
    plot_df <- tvp_df
  }

  p <- ggplot(plot_df, aes(x = date, y = value, color = method)) +
    geom_line(linewidth = 0.8, alpha = 0.7) +
    geom_hline(
      yintercept = vecm_elasticity,
      linetype = "dashed",
      color = "black",
      linewidth = 1
    ) +
    annotate(
      "text",
      x = min(plot_df$date),
      y = vecm_elasticity + 0.05,
      label = paste("VECM costante:", vecm_elasticity),
      hjust = 0,
      size = 3
    ) +
    scale_color_manual(
      values = c(
        "TVP-VAR" = "#457B9D",
        "Rolling VECM" = "#E63946"
      )
    ) +
    labs(
      title = title,
      subtitle = "Elasticità salari-produttività con diversi metodi",
      x = NULL,
      y = "Elasticità",
      color = "Metodo"
    ) +
    theme_salari() +
    theme(legend.position = "bottom")

  return(p)
}

# ------------------------------------------------------------------------------
# 21. SEMI-ELASTICITÀ ALLA DISOCCUPAZIONE
# ------------------------------------------------------------------------------

plot_tvp_phillips <- function(
  tvp_coefs,
  title = "Evoluzione Curva di Phillips (TVP-VAR)"
) {
  if (is.null(tvp_coefs) || !"semielast_u" %in% names(tvp_coefs)) {
    message("Semi-elasticità disoccupazione non disponibile")
    return(NULL)
  }

  plot_df <- data.frame(
    date = tvp_coefs$date,
    value = tvp_coefs$semielast_u
  )

  plot_df$smooth <- zoo::rollmean(
    plot_df$value,
    k = 8,
    fill = NA,
    align = "center"
  )

  p <- ggplot(plot_df, aes(x = date)) +
    geom_line(aes(y = value), color = "#E63946", alpha = 0.3, linewidth = 0.5) +
    geom_line(aes(y = smooth), color = "#E63946", linewidth = 1.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_vline(
      xintercept = as.Date(c("2008-09-15", "2012-06-01", "2020-03-01")),
      linetype = "dotted",
      color = "grey50"
    ) +
    labs(
      title = title,
      subtitle = "Semi-elasticità salari-disoccupazione nel tempo",
      x = NULL,
      y = "Semi-elasticità"
    ) +
    theme_salari()

  return(p)
}

# ------------------------------------------------------------------------------
# 22. VISUALIZZAZIONI SHOCK REALISTICI (SCALA NATURALE)
# ------------------------------------------------------------------------------

# 22.1 Grafico IRF singolo periodo con scala naturale ----
plot_realistic_irf_single <- function(
  irf_result,
  title = NULL,
  subtitle = NULL,
  y_label = "Variazione Salari Reali (%)",
  x_label = "Trimestri",
  show_zero_line = TRUE,
  color = "#1D3557"
) {
  if (is.null(irf_result)) {
    message("Nessun risultato IRF fornito")
    return(NULL)
  }

  # Prepara dati per il plot
  plot_df <- data.frame(
    horizon = irf_result$horizons,
    response = irf_result$response_pct
  )

  # Titolo automatico se non specificato
  if (is.null(title)) {
    title <- paste0(
      "Risposta dei Salari Reali a Shock ",
      irf_result$shock_description
    )
  }

  # Sottotitolo automatico
  if (is.null(subtitle)) {
    subtitle <- paste0(
      "Periodo: ",
      irf_result$period,
      " | Scala: variazione percentuale | Fattore: ",
      round(irf_result$scaling_factor, 2),
      "×"
    )
  }

  # Crea plot
  p <- ggplot(plot_df, aes(x = horizon, y = response)) +
    geom_line(color = color, linewidth = 1.2) +
    geom_point(color = color, size = 2, alpha = 0.7) +
    labs(
      title = title,
      subtitle = subtitle,
      x = x_label,
      y = y_label
    ) +
    theme_salari()

  # Aggiungi linea zero
  if (show_zero_line) {
    p <- p + geom_hline(yintercept = 0, linetype = "dashed", color = "grey50")
  }

  # Annotazione shock
  max_y <- max(plot_df$response)
  min_y <- min(plot_df$response)
  y_range <- max_y - min_y

  p <- p +
    annotate(
      "text",
      x = max(plot_df$horizon) * 0.75,
      y = min_y + y_range * 0.95,
      label = paste0("Shock: ", irf_result$shock_description),
      hjust = 0,
      size = 3.5,
      color = "grey30",
      fontface = "italic"
    )

  return(p)
}

# 22.2 Confronto IRF tra periodi multipli ----
plot_realistic_irf_comparison <- function(
  irf_comparison,
  title = NULL,
  subtitle = "Confronto tra periodi storici",
  y_label = "Variazione Salari Reali (%)",
  x_label = "Trimestri",
  show_legend = TRUE,
  period_colors = c(
    "2005" = "#2A9D8F",
    "2015" = "#E9C46A",
    "2023" = "#E63946"
  )
) {
  if (is.null(irf_comparison) || length(irf_comparison) == 0) {
    message("Nessun risultato IRF fornito per confronto")
    return(NULL)
  }

  # Combina dati da tutti i periodi
  plot_list <- list()
  for (period_name in names(irf_comparison)) {
    irf_data <- irf_comparison[[period_name]]
    if (!is.null(irf_data)) {
      plot_list[[period_name]] <- data.frame(
        horizon = irf_data$horizons,
        response = irf_data$response_pct,
        period = period_name,
        stringsAsFactors = FALSE
      )
    }
  }

  plot_df <- do.call(rbind, plot_list)

  # Titolo automatico
  if (is.null(title)) {
    shock_type <- attr(irf_comparison, "shock_type")
    shock_value <- attr(irf_comparison, "shock_value")
    title <- paste0(
      "Evoluzione Temporale della Risposta a Shock ",
      toupper(shock_type),
      " ",
      ifelse(shock_value > 0, "+", ""),
      shock_value,
      ifelse(shock_type %in% c("prod", "p", "w"), "%", "pp")
    )
  }

  # Crea plot
  p <- ggplot(
    plot_df,
    aes(x = horizon, y = response, color = period, group = period)
  ) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2, alpha = 0.7) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    scale_color_manual(
      values = period_colors,
      name = "Periodo"
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = x_label,
      y = y_label
    ) +
    theme_salari()

  if (!show_legend) {
    p <- p + theme(legend.position = "none")
  } else {
    p <- p +
      theme(
        legend.position = "bottom",
        legend.title = element_text(face = "bold")
      )
  }

  # Annotazioni eventi storici
  event_dates <- c(2008, 2012, 2020)
  max_y <- max(plot_df$response)

  return(p)
}

# 22.3 Griglia comparativa 4 shock ----
plot_realistic_irf_grid <- function(
  irf_list,
  main_title = "Risposta Salari Reali a Diversi Shock (Periodo Recente)",
  shock_labels = list(
    prod = "Produttività +10%",
    unemp = "Disoccupazione +2pp",
    infl = "Inflazione +5%",
    wedge = "Cuneo Fiscale -3pp"
  )
) {
  if (is.null(irf_list) || length(irf_list) == 0) {
    message("Nessun risultato IRF fornito per griglia")
    return(NULL)
  }

  # Prepara lista plot
  plots <- list()

  # Plot 1: Produttività
  if (!is.null(irf_list$prod)) {
    plots$prod <- plot_realistic_irf_single(
      irf_list$prod,
      title = shock_labels$prod,
      subtitle = NULL,
      y_label = "Δ Salari (%)",
      color = "#2A9D8F"
    ) +
      theme(plot.title = element_text(size = 11))
  }

  # Plot 2: Disoccupazione
  if (!is.null(irf_list$unemp)) {
    plots$unemp <- plot_realistic_irf_single(
      irf_list$unemp,
      title = shock_labels$unemp,
      subtitle = NULL,
      y_label = "Δ Salari (%)",
      color = "#E76F51"
    ) +
      theme(plot.title = element_text(size = 11))
  }

  # Plot 3: Inflazione
  if (!is.null(irf_list$infl)) {
    plots$infl <- plot_realistic_irf_single(
      irf_list$infl,
      title = shock_labels$infl,
      subtitle = NULL,
      y_label = "Δ Salari (%)",
      color = "#E9C46A"
    ) +
      theme(plot.title = element_text(size = 11))
  }

  # Plot 4: Cuneo fiscale
  if (!is.null(irf_list$wedge)) {
    plots$wedge <- plot_realistic_irf_single(
      irf_list$wedge,
      title = shock_labels$wedge,
      subtitle = NULL,
      y_label = "Δ Salari (%)",
      color = "#457B9D"
    ) +
      theme(plot.title = element_text(size = 11))
  }

  # Combina in griglia 2x2
  if (length(plots) >= 4) {
    combined <- (plots$prod + plots$unemp) / (plots$infl + plots$wedge)
    combined <- combined +
      plot_annotation(
        title = main_title,
        theme = theme(plot.title = element_text(size = 14, face = "bold"))
      )
    return(combined)
  } else {
    message("Servono almeno 4 shock per la griglia 2x2")
    return(NULL)
  }
}

# 22.4 Tabella numerica IRF ----
create_irf_table <- function(
  irf_result,
  horizons = c(1, 4, 8, 12, 20),
  caption = "Risposta a Shock Realistico",
  digits = 2
) {
  if (is.null(irf_result)) {
    message("Nessun risultato IRF fornito")
    return(NULL)
  }

  # Estrai valori agli orizzonti specificati
  idx <- match(horizons, irf_result$horizons)
  idx <- idx[!is.na(idx)]

  if (length(idx) == 0) {
    message("Nessun orizzonte valido trovato")
    return(NULL)
  }

  table_df <- data.frame(
    Orizzonte = paste0(irf_result$horizons[idx], " trim"),
    `Risposta Log` = round(irf_result$response_log[idx], digits),
    `Risposta %` = paste0(
      ifelse(irf_result$response_pct[idx] > 0, "+", ""),
      round(irf_result$response_pct[idx], digits),
      "%"
    ),
    check.names = FALSE
  )

  # Usa kable se disponibile
  if (requireNamespace("kableExtra", quietly = TRUE)) {
    table_output <- kableExtra::kable(
      table_df,
      caption = paste0(
        caption,
        " (",
        irf_result$shock_description,
        ", Periodo: ",
        irf_result$period,
        ")"
      ),
      booktabs = TRUE,
      align = c("l", "r", "r")
    ) %>%
      kableExtra::kable_styling(
        latex_options = c("striped", "hold_position"),
        full_width = FALSE
      )
    return(table_output)
  } else {
    # Fallback a base R
    return(table_df)
  }
}

# ------------------------------------------------------------------------------
# 23. DIAGNOSTICA CONVERGENZA MCMC (per shrinkTVP)
# ------------------------------------------------------------------------------

plot_tvp_posterior <- function(
  tvp_bayesian,
  param_idx = 1,
  title = "Diagnostica Posteriore MCMC"
) {
  if (is.null(tvp_bayesian) || is.null(tvp_bayesian$model)) {
    message("Modello bayesiano non disponibile")
    return(NULL)
  }

  # Estrai catena MCMC per un parametro
  beta_chain <- tvp_bayesian$model$beta[, param_idx, ]

  # Traceplot
  trace_df <- data.frame(
    iteration = 1:nrow(beta_chain),
    value = beta_chain[, ncol(beta_chain)] # Ultimo periodo
  )

  p1 <- ggplot(trace_df, aes(x = iteration, y = value)) +
    geom_line(color = "#457B9D", alpha = 0.5) +
    labs(
      title = "Traceplot MCMC",
      x = "Iterazione",
      y = "Valore"
    ) +
    theme_salari()

  # Densità posteriore
  p2 <- ggplot(trace_df, aes(x = value)) +
    geom_density(fill = "#457B9D", alpha = 0.5) +
    labs(
      title = "Densità Posteriore",
      x = "Valore",
      y = "Densità"
    ) +
    theme_salari()

  gridExtra::grid.arrange(p1, p2, ncol = 2)
}

# ------------------------------------------------------------------------------
# 23. FUNZIONE MASTER PLOT TVP
# ------------------------------------------------------------------------------

generate_tvp_plots <- function(
  tvpvar_results,
  rolling_results = NULL,
  vecm_elasticity = 0.85
) {
  cat("\nGenerazione grafici TVP-VAR...\n")

  if (is.null(tvpvar_results)) {
    cat("   Risultati TVP-VAR non disponibili.\n")
    return(invisible(NULL))
  }

  # Coefficienti TVP
  if (!is.null(tvpvar_results$coefficients)) {
    cat("  - Evoluzione coefficienti TVP\n")
    print(plot_tvp_coefficients(tvpvar_results$coefficients))

    cat("  - Elasticità salari-produttività\n")
    print(plot_tvp_elasticity(
      tvpvar_results$coefficients,
      vecm_elasticity = vecm_elasticity
    ))

    cat("  - Heatmap coefficienti\n")
    print(plot_tvp_heatmap(tvpvar_results$coefficients))

    cat("  - Semi-elasticità Phillips\n")
    print(plot_tvp_phillips(tvpvar_results$coefficients))

    # Confronto con rolling
    if (!is.null(rolling_results)) {
      cat("  - Confronto metodi\n")
      print(plot_tvp_comparison(
        tvpvar_results$coefficients,
        rolling_results,
        vecm_elasticity
      ))
    }
  }

  # IRF tempo-varianti
  if (!is.null(tvpvar_results$irf)) {
    cat("  - IRF per periodi selezionati\n")
    print(plot_tvp_irf_selected(tvpvar_results$irf))
  }

  cat("\nGrafici TVP-VAR completati.\n")
}

cat(
  "Funzioni TVP-VAR caricate. Usare generate_tvp_plots(tvpvar_results) per generare i grafici.\n"
)
