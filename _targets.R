# _targets.R
# Pipeline targets per download dati ISTAT - Modello Salari Montaletti
# Autore: Giampaolo Montaletti
# Data: 2025-12-10

library(targets)
library(tarchetypes)

# 1. Setup istatlab -----

# Carica istatlab da path locale (non su CRAN)
istatlab_path <- "~/Documents/funzioni/istatlab"
if (dir.exists(istatlab_path)) {
  devtools::load_all(istatlab_path, quiet = TRUE)
} else {
  stop("istatlab non trovato in: ", istatlab_path)
}

# 2. Opzioni targets -----

tar_option_set(
  packages = c("data.table"),
  format = "qs",  # Formato compresso per cache
  error = "continue"  # Continua anche se un target fallisce
)

# Carica funzioni helper
tar_source("R/functions.R")

# 3. Definizione pipeline -----

list(

 # 3.1 Verifica disponibilità API ISTAT -----
 tar_target(
   api_status,
   {
     status <- istatlab::check_istat_api(verbose = TRUE)
     if (!status) {
       warning("API ISTAT non disponibile. I dati potrebbero provenire dalla cache.")
     }
     status
   }
 ),

 # 3.2 Configurazione serie target -----
 tar_target(
   series_config,
   get_target_series()
 ),

  # 3.3 Download metadata ISTAT -----
  tar_target(
    metadata,
    {
      message("Downloading ISTAT metadata...")
      istatlab::download_metadata()
    }
  ),

  # 3.4 Mapping serie -> dataset ID -----
  tar_target(
    dataset_mapping,
    {
      message("Cercando dataset per le serie richieste...")
      find_datasets_for_series(metadata, series_config)
    }
  ),

  # 3.5 Lista dataset ID univoci da scaricare -----
  tar_target(
    dataset_ids,
    {
      ids <- unique(na.omit(dataset_mapping$dataset_id))
      message(sprintf("Dataset da scaricare: %d", length(ids)))
      ids
    }
  ),

  # 3.6 Download datasets (branching dinamico) -----
  tar_target(
    raw_datasets,
    {
      message(sprintf("Downloading dataset: %s", dataset_ids))
      safe_download(dataset_ids, start_time = "1995")
    },
    pattern = map(dataset_ids)
  ),

  # 3.7 Applica etichette ai dati scaricati -----
  tar_target(
    labeled_datasets,
    {
      if (is.null(raw_datasets)) {
        NULL
      } else {
        message("Applying labels...")
        istatlab::apply_labels(raw_datasets)
      }
    },
    pattern = map(raw_datasets)
  ),

  # 3.8 Estrai serie specifiche da ogni dataset -----
  tar_target(
    extracted_series,
    {
      # Trova quali serie appartengono a questo dataset
      current_id <- attr(labeled_datasets, "dataset_id")
      if (is.null(current_id)) {
        current_id <- labeled_datasets$id[1]
      }

      series_in_dataset <- dataset_mapping[dataset_id == current_id, series_code]

      lapply(series_in_dataset, function(code) {
        extract_series(labeled_datasets, code)
      })
    },
    pattern = map(labeled_datasets)
  ),

  # 3.9 Aggrega serie mensili a trimestrali -----
  tar_target(
    quarterly_series,
    {
      # Flatten la lista di liste
      all_series <- unlist(extracted_series, recursive = FALSE)

      lapply(all_series, function(s) {
        if (is.null(s) || nrow(s) == 0) return(s)

        code <- s$series_code[1]
        freq <- series_config[series_code == code, frequency]

        if (length(freq) > 0 && freq == "M") {
          aggregate_to_quarterly(s, method = "mean")
        } else {
          s
        }
      })
    }
  ),

  # 3.10 Combina tutte le serie in dataset wide -----
  tar_target(
    combined_data,
    {
      message("Combinando serie in dataset unico...")
      combine_series(quarterly_series)
    }
  ),

  # 3.11 Calcola variabili derivate -----
  tar_target(
    istat_data,
    {
      message("Calcolando variabili derivate...")
      compute_derived_variables(combined_data)
    }
  ),

  # 3.12 Validazione finale -----
  tar_target(
    validation_report,
    {
      report <- validate_output(istat_data)
      message(sprintf(
        "Dataset finale: %d osservazioni, %d variabili, periodo %s - %s",
        report$n_obs,
        report$n_vars,
        report$date_range[1],
        report$date_range[2]
      ))
      if (!report$valid) {
        warning("Validazione fallita. Variabili mancanti: ",
                paste(report$missing_required, collapse = ", "))
      }
      report
    }
  ),

  # 3.13 Export CSV per compatibilità -----
  tar_target(
    export_csv,
    {
      output_path <- "data/istat_data.csv"
      dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
      data.table::fwrite(istat_data, output_path)
      message(sprintf("Dataset esportato in: %s", output_path))
      output_path
    },
    format = "file"
  )
)
