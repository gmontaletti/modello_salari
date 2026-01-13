# functions.R
# Funzioni helper per il workflow targets di download dati ISTAT
# Progetto: Modello Salari Montaletti

# 1. Configurazione serie ISTAT -----

#' Definizione delle serie ISTAT da scaricare
#' @return data.table con series_code, variable_name, frequency, description
get_target_series <- function() {
  data.table::data.table(
    series_code = c(
      "DCSP_RETRIBCONTR",
      "DCSC_OROS",
      "DCCN_TNA",
      "DCCN_PILLAV",
      "DCCN_ORE",
      "DCCV_TAXDISOC",
      "DCCV_TAXOCCU",
      "DCSP_IPCA"
    ),
    variable_name = c(
      "w_nom",
      "w_ula",
      "pil",
      "va",
      "ore",
      "u",
      "occ",
      "p"
    ),
    frequency = c(
      "M",  # Mensile

      "Q",  # Trimestrale
      "Q",
      "Q",
      "Q",
      "Q",
      "Q",
      "M"
    ),
    description = c(
      "Retribuzioni contrattuali orarie",
      "Retribuzioni lorde per ULA (OROS)",
      "PIL reale",
      "Valore Aggiunto",
      "Ore lavorate",
      "Tasso di disoccupazione",
      "Tasso di occupazione",
      "IPCA - Indice prezzi al consumo armonizzato"
    )
  )
}

# 2. Ricerca dataset nei metadata -----

#' Cerca i dataset ISTAT che contengono le serie richieste
#' I metadata di istatlab sono un data.table con colonne: id, Name.it, dsdRef
#' @param metadata data.table scaricato da istatlab::download_metadata()
#' @param series_config data.table da get_target_series()
#' @return data.table con series_code, dataset_id, dataset_description
find_datasets_for_series <- function(metadata, series_config = get_target_series()) {

 # Verifica struttura metadata (è un data.table, non una lista)
  if (!data.table::is.data.table(metadata)) {
    data.table::setDT(metadata)
  }

  # Verifica colonne richieste
  required_cols <- c("id", "Name.it")
  if (!all(required_cols %in% names(metadata))) {
    stop(
      "Metadata deve contenere colonne: ",
      paste(required_cols, collapse = ", "),
      ". Colonne presenti: ",
      paste(names(metadata), collapse = ", ")
    )
  }

  # Cerca corrispondenze nei nomi italiani dei dataflow
  results <- lapply(series_config$series_code, function(code) {
    # Cerca il codice nel nome italiano
    matches <- grep(code, metadata$Name.it, ignore.case = TRUE)

    if (length(matches) == 0) {
      # Prova ricerca per parti del codice (es. DCSP -> cerca "DCSP")
      code_parts <- strsplit(code, "_")[[1]]
      for (part in code_parts) {
        if (nchar(part) >= 3) {  # Solo parti significative
          matches <- grep(part, metadata$Name.it, ignore.case = TRUE)
          if (length(matches) > 0) break
        }
      }
    }

    if (length(matches) == 0) {
      # Prova ricerca per descrizione dalla config
      desc <- series_config[series_code == code, description]
      if (length(desc) > 0 && nchar(desc) > 0) {
        # Cerca parole chiave nella descrizione
        keywords <- unlist(strsplit(tolower(desc), " "))
        keywords <- keywords[nchar(keywords) >= 4]  # Parole significative
        for (kw in keywords[1:min(3, length(keywords))]) {
          matches <- grep(kw, metadata$Name.it, ignore.case = TRUE)
          if (length(matches) > 0) break
        }
      }
    }

    if (length(matches) > 0) {
      data.table::data.table(
        series_code = code,
        dataset_id = metadata$id[matches[1]],
        dataset_description = metadata$Name.it[matches[1]]
      )
    } else {
      data.table::data.table(
        series_code = code,
        dataset_id = NA_character_,
        dataset_description = NA_character_
      )
    }
  })

  result <- data.table::rbindlist(results)

  # Report serie non trovate
  missing <- result[is.na(dataset_id)]
  if (nrow(missing) > 0) {
    warning(
      "Serie non trovate nei metadata: ",
      paste(missing$series_code, collapse = ", "),
      "\nSuggerimento: usare search_dataflows() per cercare manualmente."
    )
  }

  # Report serie trovate
  found <- result[!is.na(dataset_id)]
  if (nrow(found) > 0) {
    message("Dataset trovati:")
    for (i in seq_len(nrow(found))) {
      message(sprintf("  %s -> %s (%s)",
        found$series_code[i],
        found$dataset_id[i],
        substr(found$dataset_description[i], 1, 50)
      ))
    }
  }

  result
}

# 3. Download wrapper -----

#' Scarica un dataset ISTAT con gestione errori
#' @param dataset_id ID del dataset ISTAT
#' @param start_time Anno di inizio (default 1995)
#' @return data.table o NULL se errore
safe_download <- function(dataset_id, start_time = "1995") {
  tryCatch({
    istatlab::download_istat_data(
      dataset_id = dataset_id,
      start_time = start_time,
      use_cache = TRUE,
      verbose = TRUE
    )
  }, error = function(e) {
    warning(sprintf("Errore download dataset %s: %s", dataset_id, e$message))
    NULL
  })
}

# 4. Estrazione serie specifiche -----
#' Estrai una serie specifica dal dataset scaricato
#' @param labeled_data data.table con etichette applicate
#' @param series_code Codice serie da estrarre
#' @return data.table con tempo, valore, series_code
extract_series <- function(labeled_data, series_code) {
  if (is.null(labeled_data) || nrow(labeled_data) == 0) {
    return(data.table::data.table(
      tempo = as.Date(character()),
      valore = numeric(),
      series_code = character()
    ))
  }

  # Cerca colonne che contengono il codice serie
  cols <- names(labeled_data)

  # Filtra righe che corrispondono alla serie
  # La logica dipende dalla struttura dei dati ISTAT
  dt <- data.table::copy(labeled_data)

  # Assicurati che tempo sia Date

  if ("tempo" %in% cols) {
    dt[, tempo := as.Date(tempo)]
  }

  # Seleziona colonne rilevanti
  keep_cols <- intersect(c("tempo", "valore", "ObsValue"), cols)

  if (!"valore" %in% cols && "ObsValue" %in% cols) {
    data.table::setnames(dt, "ObsValue", "valore")
    keep_cols <- c("tempo", "valore")
  }

  result <- dt[, ..keep_cols]
  result[, series_code := series_code]

  result
}

# 5. Aggregazione temporale -----

#' Aggrega serie mensili a trimestrali
#' @param dt data.table con colonne tempo, valore
#' @param method Metodo aggregazione: "mean", "sum", "last"
#' @return data.table aggregato a livello trimestrale
aggregate_to_quarterly <- function(dt, method = "mean") {
  if (nrow(dt) == 0) return(dt)

  dt <- data.table::copy(dt)

  # Estrai anno e trimestre

  dt[, `:=`(
    year = data.table::year(tempo),
    quarter = data.table::quarter(tempo)
  )]

  # Aggrega per trimestre
  agg_fn <- switch(method,
    "mean" = mean,
    "sum" = sum,
    "last" = function(x) x[length(x)],
    mean
  )

  result <- dt[, .(
    valore = agg_fn(valore, na.rm = TRUE)
  ), by = .(year, quarter, series_code)]

  # Ricostruisci data trimestrale (primo giorno del trimestre)
  result[, tempo := as.Date(sprintf("%d-%02d-01", year, (quarter - 1) * 3 + 1))]
  result[, c("year", "quarter") := NULL]

  data.table::setcolorder(result, c("tempo", "valore", "series_code"))
  result
}

# 6. Combinazione serie -----

#' Combina tutte le serie in un unico dataset wide
#' @param series_list Lista di data.table da extract_series()
#' @return data.table wide con una colonna per serie
combine_series <- function(series_list) {
  # Filtra NULL

  series_list <- Filter(Negate(is.null), series_list)


  if (length(series_list) == 0) {
    stop("Nessuna serie da combinare")
  }

  # Combina in formato long
  combined <- data.table::rbindlist(series_list, fill = TRUE)

  # Pivot a wide
  wide <- data.table::dcast(
    combined,
    tempo ~ series_code,
    value.var = "valore",
    fun.aggregate = mean
  )

  # Ordina per tempo

  data.table::setorder(wide, tempo)

  wide
}

# 7. Calcolo variabili derivate -----

#' Calcola variabili derivate per il modello VECM
#' @param dt data.table con serie base
#' @return data.table con variabili aggiuntive
compute_derived_variables <- function(dt) {
  dt <- data.table::copy(dt)

  # Produttività = PIL / Ore lavorate
  if (all(c("pil", "ore") %in% names(dt))) {
    dt[, prod := pil / ore]
  }

  # Salario reale = Salario nominale / Prezzi
  if (all(c("w_nom", "p") %in% names(dt))) {
    dt[, w_real := w_nom / p * 100]
  }


  # Log-trasformazioni
  log_vars <- c("w_nom", "p", "prod", "w_real")
  for (var in log_vars) {
    if (var %in% names(dt)) {
      dt[, paste0("log_", var) := log(get(var))]
    }
  }

  # Tassi di crescita YoY
  growth_vars <- c("w_nom", "p", "prod", "pil")
  for (var in growth_vars) {
    if (var %in% names(dt)) {
      dt[, paste0("g_", var) := (get(var) / data.table::shift(get(var), 4) - 1) * 100]
    }
  }

  dt
}

# 8. Validazione output -----

#' Valida il dataset finale
#' @param dt data.table finale
#' @return Lista con statistiche di validazione
validate_output <- function(dt) {
  required_vars <- c("tempo", "w_nom", "p", "u")

  missing_vars <- setdiff(required_vars, names(dt))

  list(
    n_obs = nrow(dt),
    n_vars = ncol(dt),
    date_range = range(dt$tempo, na.rm = TRUE),
    missing_required = missing_vars,
    na_counts = sapply(dt, function(x) sum(is.na(x))),
    valid = length(missing_vars) == 0 && nrow(dt) > 0
  )
}
