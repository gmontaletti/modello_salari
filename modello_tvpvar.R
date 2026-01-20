# ==============================================================================
# MODELLO TVP-VAR: PARAMETRI VARIABILI NEL TEMPO
# ==============================================================================
# Autore: Giampaolo Montaletti
# Data: Gennaio 2025
# Estensione del modello VECM con coefficienti tempo-varianti
# ==============================================================================

# 1. Configurazione Ambiente -----

packages_tvp <- c(
  "shrinkTVP",
  "tvReg",
  "vars",
  "dplyr",
  "tidyr",
  "ggplot2",
  "zoo"
)

install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cran.r-project.org", quiet = TRUE)
    library(pkg, character.only = TRUE)
  }
}

invisible(sapply(packages_tvp, install_if_missing))

cat(
  "
================================================================================
        MODELLO TVP-VAR - PARAMETRI VARIABILI NEL TEMPO
        Estensione del framework VECM
================================================================================
\n"
)

# 2. Preparazione Dati -----

prepare_tvp_data <- function(data) {
  cat("\n[1] Preparazione dati per TVP-VAR...\n")

  # Seleziona variabili core (come VECM)
  data_tvp <- data %>%
    select(date, year, quarter, log_w, log_p, log_prod, u) %>%
    na.omit()

  # Crea matrice per stima
  y_matrix <- data_tvp %>%
    select(log_w, log_p, log_prod, u) %>%
    as.matrix()

  colnames(y_matrix) <- c("w", "p", "prod", "u")

  # Date per etichettatura
  dates <- data_tvp$date

  cat("   Osservazioni:", nrow(y_matrix), "\n")
  cat("   Variabili:", ncol(y_matrix), "\n")
  cat("   Periodo:", min(data_tvp$year), "-", max(data_tvp$year), "\n")

  return(list(
    y_matrix = y_matrix,
    dates = dates,
    data = data_tvp
  ))
}

# 3. Stima Modello TVP con shrinkTVP -----

estimate_tvp_bayesian <- function(
  y_matrix,
  target_var = "w",
  niter = 10000,
  burnin = 2000
) {
  cat("\n[2] Stima modello TVP bayesiano (shrinkTVP)...\n")
  cat("   Variabile target:", target_var, "\n")
  cat("   Iterazioni MCMC:", niter, "\n")
  cat("   Burn-in:", burnin, "\n")

  # Prepara regressori (lag delle variabili)
  n <- nrow(y_matrix)
  k <- ncol(y_matrix)

  # Costruisci matrice regressori con lag
  y_lag1 <- rbind(NA, y_matrix[-n, ])
  y_lag2 <- rbind(NA, NA, y_matrix[-c(n - 1, n), ])

  # Variabile dipendente (differenze per stazionarietà)
  y_dep <- diff(y_matrix[, target_var])

  # Regressori: livelli laggati + differenze laggate
  X <- cbind(
    intercept = 1,
    y_matrix[-1, target_var] - y_matrix[-n, target_var], # Lag diff
    y_lag1[-1, ], # Livelli lag 1
    y_lag2[-1, ] # Livelli lag 2
  )

  # Rimuovi righe con NA
  valid_idx <- complete.cases(X)
  y_dep <- y_dep[valid_idx[-1]]
  X <- X[valid_idx[-1], ]

  cat("   Osservazioni valide:", length(y_dep), "\n")
  cat("   Regressori:", ncol(X), "\n")

  # Stima shrinkTVP
  cat("\n   Avvio stima MCMC...\n")

  tvp_fit <- tryCatch(
    {
      shrinkTVP(
        y_dep ~ X - 1,
        niter = niter,
        nburn = burnin,
        display_progress = TRUE
      )
    },
    error = function(e) {
      cat("   Errore nella stima:", e$message, "\n")
      return(NULL)
    }
  )

  if (!is.null(tvp_fit)) {
    cat("\n   Stima completata.\n")

    # Estrai coefficienti tempo-varianti
    beta_median <- apply(tvp_fit$beta, c(1, 2), median)
    beta_lower <- apply(tvp_fit$beta, c(1, 2), quantile, probs = 0.025)
    beta_upper <- apply(tvp_fit$beta, c(1, 2), quantile, probs = 0.975)

    cat("\n   Dimensioni beta:", dim(beta_median), "\n")
  }

  return(list(
    model = tvp_fit,
    beta_median = beta_median,
    beta_lower = beta_lower,
    beta_upper = beta_upper,
    X = X,
    y = y_dep,
    target_var = target_var
  ))
}

# 4. Stima Modello tvVAR con Kernel (tvReg) -----

estimate_tvvar_kernel <- function(y_matrix, dates, p = 2, bandwidth = NULL) {
  cat("\n[3] Stima modello tvVAR kernel (tvReg)...\n")
  cat("   Lag:", p, "\n")

  # Converti in ts object
  y_ts <- ts(y_matrix, frequency = 4)

  # Stima tvVAR
  tvvar_fit <- tryCatch(
    {
      tvVAR(
        y = y_ts,
        p = p,
        type = "const",
        bw = bandwidth # NULL = automatic bandwidth selection
      )
    },
    error = function(e) {
      cat("   Errore nella stima:", e$message, "\n")
      return(NULL)
    }
  )

  if (!is.null(tvvar_fit)) {
    cat("   Stima completata.\n")
    cat("   Bandwidth selezionata:", tvvar_fit$bw, "\n")
  }

  return(list(
    model = tvvar_fit,
    dates = dates[(p + 1):length(dates)],
    p = p
  ))
}

# 5. Estrazione Coefficienti Tempo-Varianti -----

extract_tvp_coefficients <- function(tvvar_result, dates) {
  cat("\n[4] Estrazione coefficienti tempo-varianti...\n")

  if (is.null(tvvar_result$model)) {
    cat("   Modello non disponibile.\n")
    return(NULL)
  }

  tvvar_model <- tvvar_result$model

  # Estrai coefficienti per l'equazione dei salari
  # tvVAR restituisce coefficienti come lista di matrici (una per equazione)
  coefs <- coef(tvvar_model)

  # Coefficienti per equazione salari (prima equazione - "w")
  coefs_w <- coefs[["w"]]

  n_obs <- nrow(coefs_w)
  n_coefs <- ncol(coefs_w)

  # Crea dataframe con coefficienti nel tempo
  coef_names <- colnames(coefs_w)

  coef_df <- data.frame(
    date = tvvar_result$dates[1:n_obs]
  )

  for (i in 1:n_coefs) {
    coef_df[[coef_names[i]]] <- coefs_w[, i]
  }

  # Calcola elasticità implicita salari-produttività
  # β(prod) dal VAR in livelli log = elasticità
  prod_cols <- grep("prod", coef_names, value = TRUE)
  if (length(prod_cols) > 0) {
    coef_df$elasticity_prod <- rowSums(coef_df[, prod_cols, drop = FALSE])
  }

  # Semi-elasticità alla disoccupazione
  u_cols <- grep("\\.u$", coef_names, value = TRUE)
  if (length(u_cols) > 0) {
    coef_df$semielast_u <- rowSums(coef_df[, u_cols, drop = FALSE])
  }

  cat("   Coefficienti estratti per", nrow(coef_df), "periodi.\n")

  return(coef_df)
}

# 6. IRF Tempo-Varianti -----

compute_tvp_irf <- function(
  tvvar_result,
  n_ahead = 20,
  selected_periods = NULL
) {
  cat("\n[5] Calcolo IRF tempo-varianti...\n")

  if (is.null(tvvar_result$model)) {
    cat("   Modello non disponibile.\n")
    return(NULL)
  }

  tvvar_model <- tvvar_result$model
  dates <- tvvar_result$dates

  # Se non specificati, seleziona periodi chiave
  if (is.null(selected_periods)) {
    # Seleziona inizio, metà, fine del campione
    n <- length(dates)
    selected_idx <- c(
      round(n * 0.1), # Inizio (2002 circa)
      round(n * 0.4), # Pre-crisi (2007 circa)
      round(n * 0.6), # Post-crisi (2012 circa)
      round(n * 0.9) # Recente (2022 circa)
    )
    selected_periods <- dates[selected_idx]
  }

  cat("   Periodi selezionati:", as.character(selected_periods), "\n")
  cat("   Orizzonte IRF:", n_ahead, "trimestri\n")

  # Calcola IRF per ogni periodo selezionato
  irf_list <- list()

  for (i in seq_along(selected_periods)) {
    period <- selected_periods[i]
    period_idx <- which(dates == period)[1]

    if (!is.na(period_idx)) {
      # Calcola IRF al tempo t
      irf_t <- tryCatch(
        {
          tvIRF(
            tvvar_model,
            impulse = "prod",
            response = "w",
            n.ahead = n_ahead,
            t = period_idx
          )
        },
        error = function(e) {
          cat(
            "   Errore IRF per periodo",
            as.character(period),
            ":",
            e$message,
            "\n"
          )
          return(NULL)
        }
      )

      if (!is.null(irf_t)) {
        # Salva sia l'oggetto IRF che l'indice temporale usato
        irf_list[[as.character(period)]] <- list(
          tvirf = irf_t,
          time_idx = period_idx
        )
      }
    }
  }

  cat("   IRF calcolati per", length(irf_list), "periodi.\n")

  return(list(
    irf = irf_list,
    selected_periods = selected_periods,
    n_ahead = n_ahead
  ))
}

# ==============================================================================
# FUNZIONI PER SHOCK REALISTICI IN SCALA NATURALE
# ==============================================================================

# Funzione A: Calcola scaling factor per shock realistici -----

rescale_shock_realistic <- function(
  shock_type,
  shock_value,
  data,
  var_model = NULL
) {
  # Shock_type: "prod", "u", "p", "cuneo"
  # Shock_value: valore target (es. 10 per +10%, 2 per +2pp)
  # Data: dataset con variabili
  # var_model: modello VAR per estrarre dev.std. residui (opzionale)

  # Mappa nomi variabili
  var_map <- list(
    prod = "log_prod",
    u = "u",
    p = "log_p",
    w = "log_w"
  )

  var_name <- var_map[[shock_type]]
  if (is.null(var_name)) {
    stop("Tipo shock non riconosciuto: ", shock_type)
  }

  # Calcola dev.std. dei residui
  if (!is.null(var_model)) {
    # Usa residui del modello VAR
    residuals_matrix <- residuals(var_model)
    var_col_idx <- which(colnames(residuals_matrix) == var_name)
    if (length(var_col_idx) > 0) {
      sigma <- sd(residuals_matrix[, var_col_idx], na.rm = TRUE)
    } else {
      # Fallback: stima da differenze prime
      sigma <- sd(diff(data[[var_name]]), na.rm = TRUE)
    }
  } else {
    # Stima da differenze prime dei dati
    sigma <- sd(diff(data[[var_name]]), na.rm = TRUE)
  }

  # Calcola scaling factor
  if (shock_type %in% c("prod", "p", "w")) {
    # Variabili in log: shock% → log(1 + shock%/100)
    shock_log <- log(1 + shock_value / 100)
    scaling_factor <- shock_log / sigma
  } else {
    # Variabili in livelli (u, cuneo): shock diretto
    scaling_factor <- shock_value / sigma
  }

  return(list(
    scaling_factor = scaling_factor,
    sigma = sigma,
    shock_description = paste0(
      toupper(shock_type),
      " ",
      ifelse(shock_value > 0, "+", ""),
      shock_value,
      ifelse(shock_type %in% c("prod", "p", "w"), "%", "pp")
    )
  ))
}

# Funzione B: Calcola IRF per shock realistico singolo periodo -----

compute_realistic_irf_tvp <- function(
  tvvar_result,
  shock_type = "prod",
  response_var = "w",
  shock_value = 10,
  period_idx = NULL,
  period_date = NULL,
  n_ahead = 20,
  data = NULL,
  var_model = NULL
) {
  # Se period_date specificato, trova indice
  if (!is.null(period_date) && is.null(period_idx)) {
    period_idx <- which(tvvar_result$dates == period_date)[1]
    if (is.na(period_idx)) {
      stop("Periodo non trovato: ", period_date)
    }
  }

  # Se nessun periodo, usa l'ultimo
  if (is.null(period_idx)) {
    period_idx <- length(tvvar_result$dates)
  }

  period_label <- as.character(tvvar_result$dates[period_idx])

  # Calcola scaling factor
  scale_info <- rescale_shock_realistic(
    shock_type = shock_type,
    shock_value = shock_value,
    data = data,
    var_model = var_model
  )

  # Calcola IRF standard tramite tvIRF
  tvvar_model <- tvvar_result$model

  irf_standard <- tryCatch(
    {
      tvIRF(
        tvvar_model,
        impulse = shock_type,
        response = response_var,
        n.ahead = n_ahead,
        t = period_idx
      )
    },
    error = function(e) {
      warning("Errore calcolo IRF: ", e$message)
      return(NULL)
    }
  )

  if (is.null(irf_standard)) {
    return(NULL)
  }

  # Estrai valori IRF
  # Struttura: irf$irf[[impulse_name]][time, shock, horizon]
  impulse_name <- names(irf_standard$irf)[1]
  irf_array <- irf_standard$irf[[impulse_name]]

  # Dimensioni: [time=1, shock=1, horizon=n_ahead+1]
  irf_values_log <- as.numeric(irf_array[1, 1, ])

  # Scala IRF
  irf_scaled_log <- irf_values_log * scale_info$scaling_factor

  # Converti da log points a %
  if (response_var %in% c("w", "p", "prod")) {
    # Variabili in log: exp(delta_log) - 1
    response_pct <- (exp(irf_scaled_log) - 1) * 100
  } else {
    # Variabili in livelli: già in punti percentuali
    response_pct <- irf_scaled_log
  }

  return(list(
    horizons = 0:n_ahead,
    response_log = irf_scaled_log,
    response_pct = response_pct,
    shock_description = scale_info$shock_description,
    period = period_label,
    period_idx = period_idx,
    scaling_factor = scale_info$scaling_factor,
    sigma = scale_info$sigma
  ))
}

# Funzione C: Confronta IRF per lo stesso shock in periodi diversi -----

compute_realistic_irf_comparison <- function(
  tvvar_result,
  shock_type = "prod",
  response_var = "w",
  shock_value = 10,
  periods_idx = NULL,
  periods_labels = c("2005", "2015", "2023"),
  n_ahead = 20,
  data = NULL,
  var_model = NULL
) {
  # Se periods_idx non specificato, usa quartili del campione
  if (is.null(periods_idx)) {
    n <- length(tvvar_result$dates)
    # Seleziona periodi distribuiti: 20%, 60%, 90%
    periods_idx <- c(
      round(n * 0.2), # ~2005
      round(n * 0.6), # ~2015
      round(n * 0.9) # ~2023
    )
  }

  # Calcola IRF per ogni periodo
  irf_list <- list()

  for (i in seq_along(periods_idx)) {
    period_idx <- periods_idx[i]
    period_name <- if (i <= length(periods_labels)) {
      periods_labels[i]
    } else {
      paste0("period_", i)
    }

    irf_result <- compute_realistic_irf_tvp(
      tvvar_result = tvvar_result,
      shock_type = shock_type,
      response_var = response_var,
      shock_value = shock_value,
      period_idx = period_idx,
      n_ahead = n_ahead,
      data = data,
      var_model = var_model
    )

    if (!is.null(irf_result)) {
      irf_list[[period_name]] <- irf_result
    }
  }

  # Aggiungi metadati
  attr(irf_list, "shock_type") <- shock_type
  attr(irf_list, "shock_value") <- shock_value
  attr(irf_list, "response_var") <- response_var

  return(irf_list)
}

# 7. Confronto con VECM Baseline -----

compare_vecm_tvpvar <- function(vecm_beta, tvp_coef_df) {
  cat("\n[6] Confronto VECM vs TVP-VAR...\n")

  if (is.null(tvp_coef_df)) {
    cat("   Coefficienti TVP non disponibili.\n")
    return(NULL)
  }

  # Elasticità VECM (costante)
  # Assumiamo sia già calcolata e passata come parametro
  vecm_elasticity <- vecm_beta

  comparison <- data.frame(
    date = tvp_coef_df$date,
    tvp_elasticity = tvp_coef_df$elasticity_prod,
    vecm_elasticity = vecm_elasticity
  )

  # Statistiche di confronto
  cat("\n   --- Statistiche confronto ---\n")
  cat("   VECM elasticità (costante):", round(vecm_elasticity, 3), "\n")
  cat(
    "   TVP elasticità media:",
    round(mean(tvp_coef_df$elasticity_prod, na.rm = TRUE), 3),
    "\n"
  )
  cat(
    "   TVP elasticità min:",
    round(min(tvp_coef_df$elasticity_prod, na.rm = TRUE), 3),
    "\n"
  )
  cat(
    "   TVP elasticità max:",
    round(max(tvp_coef_df$elasticity_prod, na.rm = TRUE), 3),
    "\n"
  )
  cat(
    "   TVP elasticità sd:",
    round(sd(tvp_coef_df$elasticity_prod, na.rm = TRUE), 3),
    "\n"
  )

  return(comparison)
}

# 8. Funzione Master per Stima Completa -----

run_tvpvar_analysis <- function(data, niter = 10000, burnin = 2000, p_var = 2) {
  cat("\n")
  cat(
    "================================================================================\n"
  )
  cat("        ANALISI TVP-VAR COMPLETA\n")
  cat(
    "================================================================================\n"
  )

  # Prepara dati
  tvp_data <- prepare_tvp_data(data)

  # Stima modello tvVAR kernel
  tvvar_result <- estimate_tvvar_kernel(
    y_matrix = tvp_data$y_matrix,
    dates = tvp_data$dates,
    p = p_var
  )

  # Estrai coefficienti tempo-varianti
  tvp_coefs <- NULL
  if (!is.null(tvvar_result$model)) {
    tvp_coefs <- extract_tvp_coefficients(tvvar_result, tvp_data$dates)
  }

  # Calcola IRF tempo-varianti
  tvp_irf <- NULL
  if (!is.null(tvvar_result$model)) {
    tvp_irf <- compute_tvp_irf(tvvar_result)
  }

  # Opzionale: stima bayesiana con shrinkTVP
  # (più lenta, commentata di default)
  # tvp_bayesian <- estimate_tvp_bayesian(
  #   y_matrix = tvp_data$y_matrix,
  #   target_var = "w",
  #   niter = niter,
  #   burnin = burnin
  # )

  cat("\n")
  cat(
    "================================================================================\n"
  )
  cat("        ANALISI TVP-VAR COMPLETATA\n")
  cat(
    "================================================================================\n"
  )

  return(list(
    data = tvp_data,
    tvvar = tvvar_result,
    coefficients = tvp_coefs,
    irf = tvp_irf
    # bayesian = tvp_bayesian  # Se attivato
  ))
}

# 9. Esecuzione Analisi -----

# Carica dati (se non già presenti)
if (!exists("data_istat")) {
  data_istat <- readRDS("data/dati_istat.rds")
  data_istat <- data_istat %>%
    mutate(
      w_real = (w_nom / p) * 100,
      log_w = log(w_nom),
      log_p = log(p),
      log_prod = log(prod)
    )
}

# Esegui analisi TVP-VAR
tvpvar_results <- run_tvpvar_analysis(
  data = data_istat,
  p_var = 2
)

# 10. Riepilogo Risultati -----

print_tvpvar_summary <- function(results) {
  cat("\n")
  cat(
    "================================================================================\n"
  )
  cat("                    RIEPILOGO RISULTATI TVP-VAR\n")
  cat(
    "================================================================================\n"
  )

  if (!is.null(results$coefficients)) {
    coefs <- results$coefficients

    cat("\n[1] EVOLUZIONE ELASTICITÀ SALARI-PRODUTTIVITÀ\n")

    # Dividi in periodi
    coefs$periodo <- cut(
      as.numeric(format(coefs$date, "%Y")),
      breaks = c(1999, 2007, 2012, 2019, 2026),
      labels = c("2000-2007", "2008-2012", "2013-2019", "2020-2025")
    )

    period_stats <- coefs %>%
      group_by(periodo) %>%
      summarise(
        elasticity_mean = mean(elasticity_prod, na.rm = TRUE),
        elasticity_sd = sd(elasticity_prod, na.rm = TRUE),
        .groups = "drop"
      )

    print(period_stats)

    cat("\n[2] SEMI-ELASTICITÀ ALLA DISOCCUPAZIONE\n")
    if ("semielast_u" %in% names(coefs)) {
      period_stats_u <- coefs %>%
        group_by(periodo) %>%
        summarise(
          semielast_mean = mean(semielast_u, na.rm = TRUE),
          semielast_sd = sd(semielast_u, na.rm = TRUE),
          .groups = "drop"
        )
      print(period_stats_u)
    }
  }

  if (!is.null(results$irf)) {
    cat("\n[3] IRF DISPONIBILI PER PERIODI:\n")
    cat("   ", names(results$irf$irf), "\n")
  }

  cat(
    "\n================================================================================\n"
  )
}

print_tvpvar_summary(tvpvar_results)

cat("\n>>> Analisi TVP-VAR completata. Oggetto 'tvpvar_results' disponibile.\n")
