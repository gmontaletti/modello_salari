# 00_download_shapefile.R -----
# Download shapefile province italiane da ISTAT via situas
# Autore: Giampaolo Montaletti (ORCID: 0009-0002-5327-1122)
# Data: 2026-01-16

# 1. Setup -----

cat("==== Download Shapefile Province Italiane (ISTAT via situas) ====\n\n")

# Installa situas se mancante
if (!require("situas", quietly = TRUE)) {
  cat("Installazione pacchetto situas da GitHub...\n")
  if (!require("remotes", quietly = TRUE)) {
    install.packages("remotes", repos = "https://cloud.r-project.org")
  }
  remotes::install_github("gmontaletti/situas")
}

suppressPackageStartupMessages({
  library(situas)
  library(sf)
})

# 2. Crea directory -----

if (!dir.exists("data")) {
  dir.create("data")
  cat("Creata directory: data/\n")
}

if (!dir.exists("data/shp")) {
  dir.create("data/shp")
  cat("Creata directory: data/shp/\n")
}

if (!dir.exists("data/shp/istat")) {
  dir.create("data/shp/istat")
  cat("Creata directory: data/shp/istat/\n")
}

# 3. Download shapefile province ISTAT via situas -----

cat("\nDownload shapefile province ISTAT via situas...\n")

tryCatch(
  {
    # Prima scarica i boundaries (usando data 2025-01-01 per disponibilità)
    cat("Download boundaries ISTAT...\n")
    download_istat_boundaries(
      date = "2025-01-01",
      territorial_levels = "province",
      verbose = TRUE
    )

    # Usa prepare_territorial_maps per preparare lo shapefile
    files <- prepare_territorial_maps(
      territorial_level = "province",
      output_dir = "data/shp/istat",
      date = "2025-01-01",
      simplify = TRUE,
      verbose = TRUE
    )

    cat("\nFile creati:\n")
    print(files)

    # Verifica che il file RDS esista
    if (file.exists(files$rds)) {
      italy_sf <- readRDS(files$rds)
      cat("\nShapefile province caricato:\n")
      cat("  N. province:", nrow(italy_sf), "\n")
      cat("  CRS:", st_crs(italy_sf)$input, "\n")
      cat("  Colonne:", paste(names(italy_sf), collapse = ", "), "\n\n")

      # Crea link simbolico o copia per retrocompatibilità
      file.copy(
        files$rds,
        "data/shp/province_italia_istat.rds",
        overwrite = TRUE
      )
      cat("Copiato: data/shp/province_italia_istat.rds\n")
    }

    cat("\n✓ Shapefile province ISTAT pronto per le cartografie\n")
  },
  error = function(e) {
    cat("✗ Errore durante download via situas:\n")
    cat("  ", conditionMessage(e), "\n\n")

    cat("Tentativo alternativo: download diretto da ISTAT...\n")

    tryCatch(
      {
        # URL ISTAT per confini generalizzati 2024
        url_istat <- "https://www.istat.it/storage/cartografia/confini_amministrativi/generalizzati/2024/Limiti01012024_g.zip"

        temp_zip <- tempfile(fileext = ".zip")
        cat("  Download da:", url_istat, "\n")
        download.file(url_istat, temp_zip, mode = "wb", quiet = FALSE)

        # Estrai in directory temporanea
        temp_dir <- tempdir()
        unzip(temp_zip, exdir = temp_dir)

        # Cerca shapefile province (ProvCM)
        shp_files <- list.files(
          temp_dir,
          pattern = "ProvCM.*[.]shp$",
          recursive = TRUE,
          full.names = TRUE
        )

        if (length(shp_files) > 0) {
          cat("  Shapefile trovato:", shp_files[1], "\n")

          province_istat <- st_read(shp_files[1], quiet = TRUE)

          # Formatta COD_UTS come carattere a 3 cifre (per compatibilità con situas)
          province_istat$COD_UTS <- sprintf(
            "%03d",
            as.integer(province_istat$COD_UTS)
          )

          # Formatta anche altri codici
          province_istat$COD_REG <- sprintf(
            "%02d",
            as.integer(province_istat$COD_REG)
          )

          # Salva
          saveRDS(province_istat, "data/shp/istat/province_istat.rds")
          file.copy(
            "data/shp/istat/province_istat.rds",
            "data/shp/province_italia_istat.rds",
            overwrite = TRUE
          )

          cat("  Salvato: data/shp/province_italia_istat.rds\n")
          cat("  N. province:", nrow(province_istat), "\n")
          cat("  Colonne:", paste(names(province_istat), collapse = ", "), "\n")
          cat("✓ Shapefile ISTAT pronto\n")
        } else {
          stop("Shapefile province non trovato nell'archivio ISTAT")
        }

        unlink(temp_zip)
      },
      error = function(e2) {
        cat("✗ Anche download ISTAT diretto fallito:\n")
        cat("  ", conditionMessage(e2), "\n\n")

        # Fallback: usa GADM esistente con warning
        if (file.exists("data/shp/province_italia_gadm.rds")) {
          cat("⚠ Utilizzo shapefile GADM esistente (non ISTAT)\n")
          cat("  Per join corretto, aggiungere colonna COD_UTS manualmente\n")
        } else {
          cat("Nessun shapefile disponibile.\n")
          cat("Opzioni:\n")
          cat(
            "  1. Scarica manualmente da: https://www.istat.it/it/archivio/222527\n"
          )
          cat(
            "  2. Oppure da: https://gadm.org/download_country.html (ITA, level 2)\n"
          )
        }
      }
    )
  }
)

cat("\n==== Script completato ====\n")
