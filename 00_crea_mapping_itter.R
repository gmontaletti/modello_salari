# 00_crea_mapping_itter.R -----
# Crea tabella mapping ITTER107 (RACLI) <-> COD_UTS (situas) via nome provincia
# Gestisce differenze tra versioni NUTS (2016 in RACLI vs 2024 in situas)
# Autore: Giampaolo Montaletti (ORCID: 0009-0002-5327-1122)
# Data: 2026-01-16

# 1. Setup -----

cat("==== Creazione Mapping ITTER107 <-> COD_UTS ====\n\n")

suppressPackageStartupMessages({
  library(situas)
  library(data.table)
})

# Funzione normalizzazione nomi per matching
normalize_name <- function(x) {
  x <- toupper(trimws(x))
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x <- gsub("[^A-Z ]", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

# 2. Carica dati province da situas (NUTS 2024) -----

cat("Caricamento dati province da SITUAS (pfun=64)...\n")
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
  nuts3_2024 = sapply(COD_NUTS3_2024, function(x) x[1]) # Prendi primo se lista
)]
situas_lookup[, name_norm := normalize_name(den_uts)]

# 3. Carica codici ITTER107 da codelists (usati in RACLI) -----

cat("Caricamento codici ITTER107 da codelists...\n")
codelists <- readRDS("meta/codelists.rds")
cl_itter <- as.data.table(codelists[["CL_ITTER107"]])

# Filtra solo province (5 caratteri, esclusi aggregati)
itter_prov <- cl_itter[
  nchar(id_description) == 5 &
    !grepl("^(ITCDE|ITNI|ITTOT)", id_description),
  .(itter107 = id_description, label_itter = it_description)
]
itter_prov[, name_norm := normalize_name(label_itter)]

cat("  Codici ITTER107 province:", nrow(itter_prov), "\n\n")

# 4. Match per nome normalizzato -----

cat("Matching per nome provincia normalizzato...\n")

# Join per nome normalizzato
mapping_itter <- merge(
  itter_prov,
  situas_lookup,
  by = "name_norm",
  all.x = TRUE
)

# Gestisci casi speciali che potrebbero non matchare automaticamente
# Include province con nomi diversi o riorganizzazioni territoriali
# NOTA: i codici Sardegna mappano ai codici dello shapefile ISTAT 2024
special_cases <- data.table(
  itter107 = c(
    "ITH10", # Bolzano/Bozen
    "ITH20", # Trento
    "ITF65", # Reggio di Calabria -> Reggio Calabria
    "IT111", # Sud Sardegna
    "ITG25", # Sassari (vecchio cod 312 in situas -> 090 in shapefile 2024)
    "ITG26", # Nuoro (vecchio cod 114 in situas -> 091 in shapefile 2024)
    "ITG27", # Cagliari (vecchio cod 318 in situas -> 292 in shapefile 2024)
    "ITG28", # Oristano (vecchio cod 115 in situas -> 095 in shapefile 2024)
    "ITG29", # Olbia-Tempio (ora parte di Sassari)
    "ITG2A", # Ogliastra (ora parte di Nuoro)
    "ITG2B", # Medio Campidano (ora parte di Sud Sardegna)
    "ITG2C" # Carbonia-Iglesias (ora parte di Sud Sardegna)
  ),
  cod_uts_fix = c(
    "021", # Bolzano
    "022", # Trento
    "280", # Reggio Calabria (codice shapefile 2024)
    "111", # Sud Sardegna (codice shapefile 2024)
    "090", # Sassari (codice shapefile 2024)
    "091", # Nuoro (codice shapefile 2024)
    "292", # Cagliari (codice shapefile 2024)
    "095", # Oristano (codice shapefile 2024)
    "090", # Olbia-Tempio -> Sassari
    "091", # Ogliastra -> Nuoro
    "111", # Medio Campidano -> Sud Sardegna
    "111" # Carbonia-Iglesias -> Sud Sardegna
  )
)

# Tabella di fallback per province storiche/riorganizzate non in situas attuale
# Queste province potrebbero non esistere più con lo stesso codice ma i dati RACLI le usano ancora
# NOTA: I codici Sardegna sono stati aggiornati per matchare lo shapefile ISTAT 2024
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

# Applica fix per casi speciali non matchati
for (i in seq_len(nrow(special_cases))) {
  code <- special_cases$itter107[i]
  fix <- special_cases$cod_uts_fix[i]

  # Verifica se il codice esiste nel mapping e non ha già un cod_uts
  if (
    code %in%
      mapping_itter$itter107 &&
      is.na(mapping_itter[itter107 == code, cod_uts])
  ) {
    # Prima cerca in situas
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
      # Usa tabella province storiche come fallback
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
    # Il codice non esiste nel mapping, aggiungilo
    # Cerca in situas o usa dati storici
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

# 5. Verifica copertura con RACLI effettivo -----

cat("Verifica copertura con dati RACLI effettivi...\n")

if (file.exists("racli/F_533_957_DF_DCSC_RACLI_8.rds")) {
  racli_raw <- readRDS("racli/F_533_957_DF_DCSC_RACLI_8.rds")

  # Estrai codici RACLI provinciali unici con etichette
  racli_labels <- unique(racli_raw[, .(
    itter107 = as.character(REF_AREA),
    label_racli = as.character(REF_AREA_label)
  )])

  # Filtra province (5+ caratteri, no aggregati)
  racli_prov <- racli_labels[
    nchar(itter107) >= 5 &
      !grepl("^(IT[0-9]|ITCDE|ITNI|ITTOT)", itter107)
  ]

  cat("  Codici provinciali RACLI:", nrow(racli_prov), "\n")

  # Match diretto per codice ITTER107
  direct_match <- racli_prov[itter107 %in% mapping_itter$itter107]
  cat("  Match diretto ITTER107:", nrow(direct_match), "\n")

  # Per i non matchati direttamente, prova match per nome
  unmatched_racli <- racli_prov[!itter107 %in% mapping_itter$itter107]
  if (nrow(unmatched_racli) > 0) {
    unmatched_racli[, name_norm := normalize_name(label_racli)]

    # Cerca match per nome nel mapping
    name_match <- merge(
      unmatched_racli,
      mapping_itter[, .(name_norm, itter107_mapping = itter107, cod_uts)],
      by = "name_norm",
      all.x = TRUE
    )

    matched_by_name <- name_match[!is.na(cod_uts)]
    still_unmatched <- name_match[is.na(cod_uts)]

    cat("  Match per nome:", nrow(matched_by_name), "\n")

    # Aggiungi mapping per codici RACLI non standard
    if (nrow(matched_by_name) > 0) {
      extra_mapping <- matched_by_name[, .(
        itter107 = itter107,
        label_itter = label_racli,
        cod_uts = cod_uts
      )]

      # Aggiungi info complete da situas
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

      # Combina con mapping esistente
      mapping_itter <- rbind(mapping_itter, extra_mapping, fill = TRUE)
    }

    if (nrow(still_unmatched) > 0) {
      cat("  Ancora non matchati:", nrow(still_unmatched), "\n")
      print(still_unmatched[, .(itter107, label_racli)])
    }
  }

  # Ricalcola match rate finale
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

# 6. Fix codici Sardegna per shapefile ISTAT 2024 -----

# I codici province Sardegna nello shapefile ISTAT 2024 sono diversi da situas
# Forza sovrascrittura per garantire match con shapefile
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

# 7. Pulisci e riordina mapping -----

# Rimuovi colonne temporanee e duplicati
mapping_itter <- mapping_itter[
  !is.na(cod_uts),
  .(
    itter107,
    cod_uts,
    cod_reg,
    cod_rip,
    den_uts,
    den_reg,
    sigla
  )
]
mapping_itter <- unique(mapping_itter, by = "itter107")
setkey(mapping_itter, itter107)

# 8. Salva mapping -----

cat("Salvataggio mapping...\n")

if (!dir.exists("meta")) {
  dir.create("meta", recursive = TRUE)
}

saveRDS(mapping_itter, "meta/mapping_itter_cod_uts.rds")
cat("  Salvato: meta/mapping_itter_cod_uts.rds\n\n")

# 9. Riepilogo -----

cat("==== Riepilogo Mapping Finale ====\n\n")

cat("Righe totali:", nrow(mapping_itter), "\n")
cat("Province uniche (cod_uts):", length(unique(mapping_itter$cod_uts)), "\n")
cat("Codici ITTER107 unici:", length(unique(mapping_itter$itter107)), "\n\n")

cat("Prime 15 righe:\n")
print(head(mapping_itter, 15))

cat("\nDistribuzione per ripartizione:\n")
print(mapping_itter[, .N, by = .(cod_rip, den_reg)][order(cod_rip)])

cat("\n==== Script completato ====\n")
