# 00_download_data.R -----
# Pipeline consolidato per download dati: economici ISTAT, RACLI, shapefiles, mapping geografico
# Autore: Giampaolo Montaletti (ORCID: 0009-0002-5327-1122)
# Progetto: modello_salari
# Data: 2026-01-20

# 1. Setup ambiente -----

cat("==== Pipeline Download Dati ====\n")
cat("Avvio:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# Funzione per installazione pacchetti mancanti
install_if_missing <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      cat("Installazione pacchetto:", pkg, "\n")
      install.packages(pkg, repos = "https://cloud.r-project.org")
      library(pkg, character.only = TRUE)
    }
  }
}

# Pacchetti CRAN
install_if_missing(c("data.table", "rsdmx", "sf", "remotes"))

# Pacchetti GitHub
if (!require("istatlab", quietly = TRUE)) {
  cat("Installazione pacchetto istatlab da GitHub...\n")
  remotes::install_github("gmontaletti/istatlab")
}

if (!require("situas", quietly = TRUE)) {
  cat("Installazione pacchetto situas da GitHub...\n")
  remotes::install_github("gmontaletti/situas")
}

# Caricamento librerie
suppressPackageStartupMessages({
  library(data.table)
  library(istatlab)
  library(rsdmx)
  library(situas)
  library(sf)
})

cat("Pacchetti caricati.\n\n")

# 2. Creazione directory -----

cat("Creazione directory...\n")

dirs <- c("data", "data/shp", "data/shp/istat", "racli", "meta")

for (d in dirs) {
  if (!dir.exists(d)) {
    dir.create(d, recursive = TRUE)
    cat("  Creata:", d, "\n")
  }
}

cat("\n")

# 3. Download dati economici ISTAT + OECD -----

cat("==== Sezione 3: Download dati economici ISTAT + OECD ====\n\n")

# Scarica metadati ISTAT
cat("Download metadati ISTAT...\n")
tryCatch(
  {
    tutti <- download_metadata()
    cat("  Metadati ISTAT scaricati.\n")
  },
  error = function(e) {
    cat("  Errore download metadati:", conditionMessage(e), "\n")
  }
)

# Scarica codelists per dataset rilevanti
cat("\nDownload codelists ISTAT...\n")
tryCatch(
  {
    codelists <- download_codelists(
      dataset_ids = c(
        "163_184", # PIL trimestrale
        "151_874", # Disoccupazione
        "155_358", # Retribuzione oraria contrattuale
        "92_507", # Ore lavorate CN
        "168_756", # IPCA
        "150_872", # Tasso occupazione
        "92_506", # Componenti PIL
        "150_875", # Occupati per tipo contratto
        "533_957", # RACLI
        "98_197", # Produttivita
        "534_1037" # Imprese con dipendenti
      )
    )
    saveRDS(codelists, "meta/codelists.rds")
    cat("  Codelists salvati: meta/codelists.rds\n")
  },
  error = function(e) {
    cat("  Errore download codelists:", conditionMessage(e), "\n")
  }
)

# Download dati OECD (tax wedge)
cat("\nDownload dati OECD (tax wedge)...\n")
tryCatch(
  {
    oecd_url <- "https://sdmx.oecd.org/public/rest/data/OECD.CTP.TPS,DSD_TAX_WAGES_COMP@DF_TW_COMP,2.1/.AV_TW..S_C0.AW100._Z.A?startPeriod=1995"
    datiocse <- rsdmx::readSDMX(oecd_url)
    datiocsedf <- as.data.frame(datiocse)
    saveRDS(datiocsedf, "data/oecdtax.rds")
    cat("  Salvato: data/oecdtax.rds\n")
  },
  error = function(e) {
    cat("  Errore download OECD:", conditionMessage(e), "\n")
  }
)

# Lista dataset ISTAT da scaricare
istat_datasets <- list(
  list(
    id = "534_1037",
    file = "data/imprese_con_dipendenti.rds",
    desc = "Imprese con dipendenti"
  ),
  list(id = "98_197", file = "data/produttivita.rds", desc = "Produttivita"),
  list(id = "163_88", file = "data/ore_lavorate.rds", desc = "Ore lavorate"),
  list(id = "163_184", file = "data/pil.rds", desc = "PIL trimestrale"),
  list(
    id = "151_874",
    file = "data/disoccupazione.rds",
    desc = "Disoccupazione mensile"
  ),
  list(
    id = "155_358",
    file = "data/retr_oraria.rds",
    desc = "Retribuzione oraria contrattuale"
  ),
  list(id = "92_507", file = "data/ore_lav_cn.rds", desc = "Ore lavorate CN"),
  list(id = "168_756", file = "data/ipca.rds", desc = "IPCA mensile base 2015"),
  list(
    id = "150_872",
    file = "data/tasso_occupazione.rds",
    desc = "Tasso di occupazione"
  ),
  list(
    id = "92_506",
    file = "data/pil_componenti.rds",
    desc = "Componenti del PIL"
  ),
  list(
    id = "150_875",
    file = "data/occupati_stabili.rds",
    desc = "Occupati per tipo contratto"
  )
)

cat("\nDownload dataset ISTAT...\n")
for (ds in istat_datasets) {
  tryCatch(
    {
      cat("  ", ds$desc, " (", ds$id, ")...", sep = "")
      dt <- download_istat_data(ds$id)
      saveRDS(dt, ds$file)
      cat(" OK\n")
    },
    error = function(e) {
      cat(" ERRORE:", conditionMessage(e), "\n")
    }
  )
}

cat("\n")

# 4. Download dati RACLI -----

cat("==== Sezione 4: Download dati RACLI ====\n\n")

cat("Download RACLI (25 dataflow)...\n")

# Download raw
for (i in 1:25) {
  tryCatch(
    {
      nome <- paste0("533_957_DF_DCSC_RACLI_", i)
      cat("  ", nome, "...", sep = "")
      dt <- download_istat_data(nome)
      nomefile <- paste0("racli/F_", nome, ".rds")
      saveRDS(dt, nomefile)
      cat(" OK\n")
    },
    error = function(e) {
      cat(" ERRORE:", conditionMessage(e), "\n")
    }
  )
}

# Applica labels
cat("\nApplicazione labels RACLI...\n")
for (i in 1:25) {
  tryCatch(
    {
      nome <- paste0("533_957_DF_DCSC_RACLI_", i)
      nomefile <- paste0("racli/F_", nome, ".rds")
      if (file.exists(nomefile)) {
        dt <- readRDS(nomefile)
        dtl <- istatlab::apply_labels(dt)
        saveRDS(dtl, nomefile)
        cat("  Labels applicate:", nome, "\n")
      }
    },
    error = function(e) {
      cat("  Errore labels", nome, ":", conditionMessage(e), "\n")
    }
  )
}

cat("\n")

# 5. Download shapefile province ISTAT -----

cat("==== Sezione 5: Download Shapefile Province ISTAT ====\n\n")

tryCatch(
  {
    # Metodo primario: situas
    cat("Download shapefile via situas...\n")

    # Download boundaries ISTAT
    download_istat_boundaries(
      date = "2025-01-01",
      territorial_levels = "province",
      verbose = TRUE
    )

    # Prepara shapefile
    files <- prepare_territorial_maps(
      territorial_level = "province",
      output_dir = "data/shp/istat",
      date = "2025-01-01",
      simplify = TRUE,
      verbose = TRUE
    )

    cat("\nFile creati:\n")
    print(files)

    # Verifica e copia per retrocompatibilita
    if (file.exists(files$rds)) {
      italy_sf <- readRDS(files$rds)
      cat("\nShapefile province caricato:\n")
      cat("  N. province:", nrow(italy_sf), "\n")
      cat("  CRS:", st_crs(italy_sf)$input, "\n")
      cat("  Colonne:", paste(names(italy_sf), collapse = ", "), "\n")

      file.copy(
        files$rds,
        "data/shp/province_italia_istat.rds",
        overwrite = TRUE
      )
      cat("  Copiato: data/shp/province_italia_istat.rds\n")
    }

    cat("\nShapefile province ISTAT pronto.\n")
  },
  error = function(e) {
    cat("Errore situas:", conditionMessage(e), "\n\n")
    cat("Tentativo download diretto ISTAT...\n")

    tryCatch(
      {
        url_istat <- "https://www.istat.it/storage/cartografia/confini_amministrativi/generalizzati/2024/Limiti01012024_g.zip"
        temp_zip <- tempfile(fileext = ".zip")

        cat("  Download da:", url_istat, "\n")
        download.file(url_istat, temp_zip, mode = "wb", quiet = FALSE)

        temp_dir <- tempdir()
        unzip(temp_zip, exdir = temp_dir)

        shp_files <- list.files(
          temp_dir,
          pattern = "ProvCM.*[.]shp$",
          recursive = TRUE,
          full.names = TRUE
        )

        if (length(shp_files) > 0) {
          cat("  Shapefile trovato:", shp_files[1], "\n")

          province_istat <- st_read(shp_files[1], quiet = TRUE)

          # Formatta codici
          province_istat$COD_UTS <- sprintf(
            "%03d",
            as.integer(province_istat$COD_UTS)
          )
          province_istat$COD_REG <- sprintf(
            "%02d",
            as.integer(province_istat$COD_REG)
          )

          saveRDS(province_istat, "data/shp/istat/province_istat.rds")
          file.copy(
            "data/shp/istat/province_istat.rds",
            "data/shp/province_italia_istat.rds",
            overwrite = TRUE
          )

          cat("  Salvato: data/shp/province_italia_istat.rds\n")
          cat("  N. province:", nrow(province_istat), "\n")
          cat("  Shapefile ISTAT pronto.\n")
        } else {
          stop("Shapefile province non trovato nell'archivio ISTAT")
        }

        unlink(temp_zip)
      },
      error = function(e2) {
        cat(
          "Anche download ISTAT diretto fallito:",
          conditionMessage(e2),
          "\n\n"
        )

        if (file.exists("data/shp/province_italia_gadm.rds")) {
          cat("Utilizzo shapefile GADM esistente (non ISTAT).\n")
          cat("Per join corretto, aggiungere colonna COD_UTS manualmente.\n")
        } else {
          cat("Nessun shapefile disponibile.\n")
          cat("Opzioni:\n")
          cat("  1. Scarica da: https://www.istat.it/it/archivio/222527\n")
          cat(
            "  2. Oppure da: https://gadm.org/download_country.html (ITA, level 2)\n"
          )
        }
      }
    )
  }
)

cat("\n")

# 6. Creazione mapping ITTER107 <-> COD_UTS -----

cat("==== Sezione 6: Creazione Mapping ITTER107 <-> COD_UTS ====\n\n")

# Funzione normalizzazione nomi per matching
normalize_name <- function(x) {
  x <- toupper(trimws(x))
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x <- gsub("[^A-Z ]", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

# Carica dati province da situas (NUTS 2024)
cat("Caricamento dati province da SITUAS (pfun=64)...\n")
tryCatch(
  {
    prov_situas <- as.data.table(get_situas_tables(pfun = 64, verbose = FALSE))
    cat("  Province situas:", nrow(prov_situas), "\n")

    # Prepara tabella situas con nome normalizzato
    situas_lookup <- prov_situas[, .(
      cod_uts = COD_UTS,
      cod_reg = COD_REG,
      cod_rip = COD_RIP,
      den_uts = DEN_UTS,
      den_reg = DEN_REG,
      sigla = SIGLA_AUTOMOBILISTICA,
      nuts3_2024 = sapply(COD_NUTS3_2024, function(x) x[1])
    )]
    situas_lookup[, name_norm := normalize_name(den_uts)]
  },
  error = function(e) {
    cat("  Errore caricamento situas:", conditionMessage(e), "\n")
    situas_lookup <- data.table()
  }
)

# Carica codici ITTER107 da codelists (usati in RACLI)
cat("Caricamento codici ITTER107 da codelists...\n")
tryCatch(
  {
    if (!exists("codelists")) {
      codelists <- readRDS("meta/codelists.rds")
    }
    cl_itter <- as.data.table(codelists[["CL_ITTER107"]])

    # Filtra solo province (5 caratteri, esclusi aggregati)
    itter_prov <- cl_itter[
      nchar(id_description) == 5 &
        !grepl("^(ITCDE|ITNI|ITTOT)", id_description),
      .(itter107 = id_description, label_itter = it_description)
    ]
    itter_prov[, name_norm := normalize_name(label_itter)]

    cat("  Codici ITTER107 province:", nrow(itter_prov), "\n\n")

    # Match per nome normalizzato
    cat("Matching per nome provincia normalizzato...\n")

    mapping_itter <- merge(
      itter_prov,
      situas_lookup,
      by = "name_norm",
      all.x = TRUE
    )

    # Gestisci casi speciali
    special_cases <- data.table(
      itter107 = c(
        "ITH10",
        "ITH20",
        "ITF65",
        "IT111",
        "ITG25",
        "ITG26",
        "ITG27",
        "ITG28",
        "ITG29",
        "ITG2A",
        "ITG2B",
        "ITG2C"
      ),
      cod_uts_fix = c(
        "021",
        "022",
        "280",
        "111",
        "090",
        "091",
        "292",
        "095",
        "090",
        "091",
        "111",
        "111"
      )
    )

    # Tabella province storiche
    historical_provinces <- data.table(
      cod_uts = c("090", "091", "095", "111", "280", "292"),
      cod_reg = c("20", "20", "20", "20", "18", "20"),
      cod_rip = c(5L, 5L, 5L, 5L, 4L, 5L),
      den_uts = c(
        "Sassari",
        "Nuoro",
        "Oristano",
        "Sud Sardegna",
        "Reggio Calabria",
        "Cagliari"
      ),
      den_reg = c(
        "Sardegna",
        "Sardegna",
        "Sardegna",
        "Sardegna",
        "Calabria",
        "Sardegna"
      ),
      sigla = c("SS", "NU", "OR", "SU", "RC", "CA")
    )

    # Applica fix per casi speciali
    for (i in seq_len(nrow(special_cases))) {
      code <- special_cases$itter107[i]
      fix <- special_cases$cod_uts_fix[i]

      if (
        code %in%
          mapping_itter$itter107 &&
          is.na(mapping_itter[itter107 == code, cod_uts])
      ) {
        situas_row <- situas_lookup[cod_uts == fix]

        if (nrow(situas_row) > 0) {
          mapping_itter[
            itter107 == code,
            `:=`(
              cod_uts = situas_row$cod_uts,
              cod_reg = situas_row$cod_reg,
              cod_rip = situas_row$cod_rip,
              den_uts = situas_row$den_uts,
              den_reg = situas_row$den_reg,
              sigla = situas_row$sigla,
              nuts3_2024 = situas_row$nuts3_2024
            )
          ]
        } else {
          hist_row <- historical_provinces[cod_uts == fix]
          if (nrow(hist_row) > 0) {
            mapping_itter[
              itter107 == code,
              `:=`(
                cod_uts = hist_row$cod_uts,
                cod_reg = hist_row$cod_reg,
                cod_rip = hist_row$cod_rip,
                den_uts = hist_row$den_uts,
                den_reg = hist_row$den_reg,
                sigla = hist_row$sigla
              )
            ]
          }
        }
      } else if (!code %in% mapping_itter$itter107) {
        situas_row <- situas_lookup[cod_uts == fix]
        if (nrow(situas_row) > 0) {
          new_row <- data.table(
            itter107 = code,
            label_itter = situas_row$den_uts,
            cod_uts = situas_row$cod_uts,
            cod_reg = situas_row$cod_reg,
            cod_rip = situas_row$cod_rip,
            den_uts = situas_row$den_uts,
            den_reg = situas_row$den_reg,
            sigla = situas_row$sigla,
            name_norm = normalize_name(situas_row$den_uts),
            nuts3_2024 = situas_row$nuts3_2024
          )
          mapping_itter <- rbind(mapping_itter, new_row, fill = TRUE)
        } else {
          hist_row <- historical_provinces[cod_uts == fix]
          if (nrow(hist_row) > 0) {
            new_row <- data.table(
              itter107 = code,
              label_itter = hist_row$den_uts,
              cod_uts = hist_row$cod_uts,
              cod_reg = hist_row$cod_reg,
              cod_rip = hist_row$cod_rip,
              den_uts = hist_row$den_uts,
              den_reg = hist_row$den_reg,
              sigla = hist_row$sigla,
              name_norm = normalize_name(hist_row$den_uts)
            )
            mapping_itter <- rbind(mapping_itter, new_row, fill = TRUE)
          }
        }
      }
    }

    # Report matching
    n_matched <- sum(!is.na(mapping_itter$cod_uts))
    n_total <- nrow(mapping_itter)
    cat("  Matchati:", n_matched, "/", n_total, "\n")

    if (n_matched < n_total) {
      unmatched <- mapping_itter[is.na(cod_uts), .(itter107, label_itter)]
      cat("  Non matchati:\n")
      print(unmatched)
    }
    cat("\n")

    # Verifica copertura con RACLI effettivo
    cat("Verifica copertura con dati RACLI effettivi...\n")

    if (file.exists("racli/F_533_957_DF_DCSC_RACLI_8.rds")) {
      racli_raw <- readRDS("racli/F_533_957_DF_DCSC_RACLI_8.rds")

      racli_labels <- unique(racli_raw[, .(
        itter107 = as.character(REF_AREA),
        label_racli = as.character(REF_AREA_label)
      )])

      racli_prov <- racli_labels[
        nchar(itter107) >= 5 &
          !grepl("^(IT[0-9]|ITCDE|ITNI|ITTOT)", itter107)
      ]

      cat("  Codici provinciali RACLI:", nrow(racli_prov), "\n")

      direct_match <- racli_prov[itter107 %in% mapping_itter$itter107]
      cat("  Match diretto ITTER107:", nrow(direct_match), "\n")

      unmatched_racli <- racli_prov[!itter107 %in% mapping_itter$itter107]
      if (nrow(unmatched_racli) > 0) {
        unmatched_racli[, name_norm := normalize_name(label_racli)]

        name_match <- merge(
          unmatched_racli,
          mapping_itter[, .(name_norm, itter107_mapping = itter107, cod_uts)],
          by = "name_norm",
          all.x = TRUE
        )

        matched_by_name <- name_match[!is.na(cod_uts)]
        still_unmatched <- name_match[is.na(cod_uts)]

        cat("  Match per nome:", nrow(matched_by_name), "\n")

        if (nrow(matched_by_name) > 0) {
          extra_mapping <- matched_by_name[, .(
            itter107 = itter107,
            label_itter = label_racli,
            cod_uts = cod_uts
          )]

          extra_mapping <- merge(
            extra_mapping,
            situas_lookup[, .(
              cod_uts,
              cod_reg,
              cod_rip,
              den_uts,
              den_reg,
              sigla,
              nuts3_2024
            )],
            by = "cod_uts",
            all.x = TRUE
          )
          extra_mapping[, name_norm := normalize_name(label_itter)]

          mapping_itter <- rbind(mapping_itter, extra_mapping, fill = TRUE)
        }

        if (nrow(still_unmatched) > 0) {
          cat("  Ancora non matchati:", nrow(still_unmatched), "\n")
          print(still_unmatched[, .(itter107, label_racli)])
        }
      }

      final_racli_codes <- racli_prov$itter107
      final_matched <- sum(final_racli_codes %in% mapping_itter$itter107)
      cat(
        "\n  Match rate finale:",
        final_matched,
        "/",
        length(final_racli_codes),
        "(",
        round(final_matched / length(final_racli_codes) * 100, 1),
        "%)\n"
      )
    }

    cat("\n")

    # Fix codici Sardegna per shapefile ISTAT 2024
    sardegna_shapefile_fix <- data.table(
      itter107 = c("ITG25", "ITG26", "ITG27", "ITG28"),
      cod_uts_shp = c("090", "091", "292", "095"),
      den_uts_shp = c("Sassari", "Nuoro", "Cagliari", "Oristano")
    )

    for (i in seq_len(nrow(sardegna_shapefile_fix))) {
      code <- sardegna_shapefile_fix$itter107[i]
      if (code %in% mapping_itter$itter107) {
        mapping_itter[
          itter107 == code,
          cod_uts := sardegna_shapefile_fix$cod_uts_shp[i]
        ]
        mapping_itter[
          itter107 == code,
          den_uts := sardegna_shapefile_fix$den_uts_shp[i]
        ]
      }
    }

    cat("  Fix Sardegna applicato per shapefile ISTAT 2024\n")

    # Pulisci e riordina mapping
    mapping_itter <- mapping_itter[
      !is.na(cod_uts),
      .(itter107, cod_uts, cod_reg, cod_rip, den_uts, den_reg, sigla)
    ]
    mapping_itter <- unique(mapping_itter, by = "itter107")
    setkey(mapping_itter, itter107)

    # Salva mapping
    cat("Salvataggio mapping...\n")
    saveRDS(mapping_itter, "meta/mapping_itter_cod_uts.rds")
    cat("  Salvato: meta/mapping_itter_cod_uts.rds\n\n")

    # Riepilogo
    cat("==== Riepilogo Mapping Finale ====\n\n")
    cat("Righe totali:", nrow(mapping_itter), "\n")
    cat(
      "Province uniche (cod_uts):",
      length(unique(mapping_itter$cod_uts)),
      "\n"
    )
    cat(
      "Codici ITTER107 unici:",
      length(unique(mapping_itter$itter107)),
      "\n\n"
    )

    cat("Prime 15 righe:\n")
    print(head(mapping_itter, 15))

    cat("\nDistribuzione per ripartizione:\n")
    print(mapping_itter[, .N, by = .(cod_rip, den_reg)][order(cod_rip)])
  },
  error = function(e) {
    cat("  Errore creazione mapping:", conditionMessage(e), "\n")
  }
)

# 7. Riepilogo finale -----

cat("\n==== Pipeline Completata ====\n")
cat("Fine:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# Verifica file creati
cat("File creati:\n")

check_files <- list(
  "data/oecdtax.rds" = "OECD tax wedge",
  "data/pil.rds" = "PIL trimestrale",
  "data/disoccupazione.rds" = "Disoccupazione",
  "data/retr_oraria.rds" = "Retribuzione oraria",
  "data/ipca.rds" = "IPCA",
  "data/produttivita.rds" = "Produttivita",
  "racli/F_533_957_DF_DCSC_RACLI_1.rds" = "RACLI (primo file)",
  "data/shp/province_italia_istat.rds" = "Shapefile province",
  "meta/mapping_itter_cod_uts.rds" = "Mapping ITTER107-COD_UTS",
  "meta/codelists.rds" = "Codelists ISTAT"
)

for (f in names(check_files)) {
  status <- if (file.exists(f)) "OK" else "MANCANTE"
  cat("  [", status, "] ", check_files[[f]], " (", f, ")\n", sep = "")
}

cat("\n")
