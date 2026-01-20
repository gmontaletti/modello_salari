# 02_analisi_descrittive_racli.R -----
# Analisi descrittive RACLI: ranking settori, gap gender/age, statistiche
# Autore: Giampaolo Montaletti (ORCID: 0009-0002-5327-1122)
# Data: 2026-01-14

# 1. Setup e caricamento dati -----
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(knitr)
  library(kableExtra)
})

cat("==== Analisi Descrittive RACLI ====\n\n")

# Carica dati preparati
dati_settore_sesso <- readRDS("output/dati_settore_sesso.rds")
dati_educazione <- readRDS("output/dati_educazione.rds")
dati_contratto <- readRDS("output/dati_contratto.rds")
dati_settori <- readRDS("output/dati_settori.rds")

cat("Dati caricati:\n")
cat("- Settore × Sesso:", nrow(dati_settore_sesso), "righe\n")
cat("- Educazione:", nrow(dati_educazione), "righe\n")
cat("- Contratto:", nrow(dati_contratto), "righe\n")
cat("- Settori NACE:", nrow(dati_settori), "righe\n\n")

# 2. Ranking settori per salario mediano 2022 -----
cat("==== Ranking Settori per Salario Mediano (2022) ====\n\n")

ranking_settori_2022 <- dati_settore_sesso %>%
  filter(anno == 2022, geo_level == "Nazionale") %>%
  group_by(settore) %>%
  summarise(
    salario_mediano = mean(salario_mediano, na.rm = TRUE),
    salario_medio = mean(salario_medio, na.rm = TRUE),
    D1 = mean(D1, na.rm = TRUE),
    D9 = mean(D9, na.rm = TRUE),
    rapporto_D9_D1 = D9 / D1,
    .groups = "drop"
  ) %>%
  arrange(desc(salario_mediano)) %>%
  mutate(
    rank = row_number(),
    salario_mediano_fmt = sprintf("€%.2f", salario_mediano),
    D9_D1_fmt = sprintf("%.2f", rapporto_D9_D1)
  )

# Top 10 e Bottom 10 settori
cat("Top 10 settori per salario mediano:\n")
print(
  ranking_settori_2022 %>%
    select(rank, settore, salario_mediano_fmt, D9_D1_fmt) %>%
    head(10) %>%
    kable(
      col.names = c("Rank", "Settore", "Salario Mediano", "D9/D1"),
      align = c("r", "l", "r", "r")
    )
)

cat("\n\nBottom 10 settori per salario mediano:\n")
print(
  ranking_settori_2022 %>%
    select(rank, settore, salario_mediano_fmt, D9_D1_fmt) %>%
    tail(10) %>%
    kable(
      col.names = c("Rank", "Settore", "Salario Mediano", "D9/D1"),
      align = c("r", "l", "r", "r")
    )
)

# Salva ranking
saveRDS(ranking_settori_2022, "output/ranking_settori_2022.rds")
cat("\n\nSalvato: output/ranking_settori_2022.rds\n\n")

# 3. Gap di genere (gender gap) -----
cat("==== Gap di Genere per Settore ====\n\n")

# Calcolo gap gender per settore e anno
gap_gender <- dati_settore_sesso %>%
  filter(geo_level == "Nazionale", sesso %in% c("maschi", "femmine")) %>%
  select(anno, settore, sesso, salario_mediano) %>%
  pivot_wider(
    names_from = sesso,
    values_from = salario_mediano,
    names_prefix = "salario_"
  ) %>%
  mutate(
    gap_assoluto = salario_maschi - salario_femmine,
    gap_percentuale = ((salario_maschi - salario_femmine) / salario_femmine) *
      100,
    rapporto_M_F = salario_maschi / salario_femmine
  ) %>%
  filter(!is.na(gap_percentuale))

# Gap gender per settore nel 2022
gap_gender_2022 <- gap_gender %>%
  filter(anno == 2022) %>%
  arrange(desc(gap_percentuale)) %>%
  mutate(
    gap_pct_fmt = sprintf("%.1f%%", gap_percentuale),
    rapporto_fmt = sprintf("%.2f", rapporto_M_F)
  )

cat("Settori con maggior gap di genere (2022):\n")
print(
  gap_gender_2022 %>%
    select(
      settore,
      salario_femmine,
      salario_maschi,
      gap_pct_fmt,
      rapporto_fmt
    ) %>%
    head(10) %>%
    kable(
      col.names = c(
        "Settore",
        "Salario F (€/h)",
        "Salario M (€/h)",
        "Gap %",
        "Rapporto M/F"
      ),
      align = c("l", "r", "r", "r", "r"),
      digits = 2
    )
)

# Evoluzione gap gender medio nel tempo
gap_gender_tempo <- gap_gender %>%
  group_by(anno) %>%
  summarise(
    gap_medio = mean(gap_percentuale, na.rm = TRUE),
    gap_mediano = median(gap_percentuale, na.rm = TRUE),
    rapporto_M_F_medio = mean(rapporto_M_F, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n\nEvoluzione gap di genere medio nel tempo:\n")
print(
  gap_gender_tempo %>%
    mutate(
      gap_medio_fmt = sprintf("%.1f%%", gap_medio),
      rapporto_fmt = sprintf("%.3f", rapporto_M_F_medio)
    ) %>%
    kable(
      col.names = c(
        "Anno",
        "Gap Medio %",
        "Gap Mediano %",
        "Rapporto M/F",
        "Gap Fmt",
        "Rapporto Fmt"
      ),
      digits = 2
    )
)

# Salva risultati gap gender
saveRDS(gap_gender, "output/gap_gender.rds")
saveRDS(gap_gender_2022, "output/gap_gender_2022.rds")
saveRDS(gap_gender_tempo, "output/gap_gender_tempo.rds")
cat("\nSalvato: output/gap_gender*.rds\n\n")

# 4. Gap educativo -----
cat("==== Gap Educativo ====\n\n")

# Preparazione dati educazione per analisi gap
# Assumo che la variabile educazione sia presente nel dataset
if ("educazione" %in% names(dati_educazione)) {
  gap_educazione_2022 <- dati_educazione %>%
    filter(anno == 2022, geo_level == "Nazionale") %>%
    group_by(educazione) %>%
    summarise(
      salario_mediano = mean(salario_mediano, na.rm = TRUE),
      salario_medio = mean(salario_medio, na.rm = TRUE),
      D9_D1 = mean(D9 / D1, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(salario_mediano)) %>%
    mutate(
      salario_fmt = sprintf("€%.2f", salario_mediano),
      D9_D1_fmt = sprintf("%.2f", D9_D1)
    )

  cat("Salari mediani per livello educativo (2022):\n")
  print(
    gap_educazione_2022 %>%
      kable(
        col.names = c(
          "Livello Educativo",
          "Salario Mediano",
          "Salario Medio",
          "D9/D1",
          "Fmt Mediano",
          "Fmt D9/D1"
        ),
        digits = 2
      )
  )

  saveRDS(gap_educazione_2022, "output/gap_educazione_2022.rds")
  cat("\nSalvato: output/gap_educazione_2022.rds\n\n")
} else {
  cat("Variabile 'educazione' non trovata nel dataset educazione\n")
  cat(
    "Colonne disponibili:",
    paste(names(dati_educazione), collapse = ", "),
    "\n\n"
  )
}

# 5. Gap per tipo contratto -----
cat("==== Gap per Tipo Contratto ====\n\n")

if ("contratto" %in% names(dati_contratto)) {
  gap_contratto_2022 <- dati_contratto %>%
    filter(anno == 2022, geo_level == "Nazionale") %>%
    group_by(contratto) %>%
    summarise(
      salario_mediano = mean(salario_mediano, na.rm = TRUE),
      salario_medio = mean(salario_medio, na.rm = TRUE),
      D9_D1 = mean(D9 / D1, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(salario_mediano)) %>%
    mutate(
      salario_fmt = sprintf("€%.2f", salario_mediano),
      D9_D1_fmt = sprintf("%.2f", D9_D1)
    )

  cat("Salari mediani per tipo contratto (2022):\n")
  print(
    gap_contratto_2022 %>%
      kable(
        col.names = c(
          "Tipo Contratto",
          "Salario Mediano",
          "Salario Medio",
          "D9/D1",
          "Fmt Mediano",
          "Fmt D9/D1"
        ),
        digits = 2
      )
  )

  saveRDS(gap_contratto_2022, "output/gap_contratto_2022.rds")
  cat("\nSalvato: output/gap_contratto_2022.rds\n\n")
} else {
  cat("Variabile 'contratto' non trovata nel dataset contratto\n")
  cat(
    "Colonne disponibili:",
    paste(names(dati_contratto), collapse = ", "),
    "\n\n"
  )
}

# 6. Statistiche descrittive per ripartizione geografica -----
cat("==== Statistiche per Ripartizione Geografica (2022) ====\n\n")

stat_ripartizioni_2022 <- dati_settore_sesso %>%
  filter(anno == 2022, geo_level == "Ripartizione") %>%
  group_by(ripartizione) %>%
  summarise(
    salario_mediano = mean(salario_mediano, na.rm = TRUE),
    salario_medio = mean(salario_medio, na.rm = TRUE),
    D1 = mean(D1, na.rm = TRUE),
    D9 = mean(D9, na.rm = TRUE),
    D9_D1 = D9 / D1,
    cv = sd(salario_mediano, na.rm = TRUE) / salario_mediano,
    n_obs = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(salario_mediano)) %>%
  mutate(
    salario_fmt = sprintf("€%.2f", salario_mediano),
    D9_D1_fmt = sprintf("%.2f", D9_D1),
    cv_fmt = sprintf("%.3f", cv)
  )

cat("Salari per ripartizione geografica:\n")
print(
  stat_ripartizioni_2022 %>%
    kable(
      col.names = c(
        "Ripartizione",
        "Salario Mediano",
        "Salario Medio",
        "D1",
        "D9",
        "D9/D1",
        "CV",
        "N.Obs",
        "Fmt Med",
        "Fmt D9/D1",
        "Fmt CV"
      ),
      digits = 2
    )
)

saveRDS(stat_ripartizioni_2022, "output/stat_ripartizioni_2022.rds")
cat("\nSalvato: output/stat_ripartizioni_2022.rds\n\n")

# 7. Evoluzione temporale salari settoriali (top 5 settori) -----
cat("==== Evoluzione Temporale Top 5 Settori ====\n\n")

top5_settori <- ranking_settori_2022 %>%
  head(5) %>%
  pull(settore)

evoluzione_top5 <- dati_settore_sesso %>%
  filter(geo_level == "Nazionale", settore %in% top5_settori) %>%
  group_by(anno, settore) %>%
  summarise(
    salario_mediano = mean(salario_mediano, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(settore, anno)

cat("Evoluzione salari mediani top 5 settori:\n")
evoluzione_wide <- evoluzione_top5 %>%
  pivot_wider(
    names_from = anno,
    values_from = salario_mediano,
    names_prefix = "anno_"
  )

print(kable(evoluzione_wide, digits = 2))

saveRDS(evoluzione_top5, "output/evoluzione_top5_settori.rds")
cat("\nSalvato: output/evoluzione_top5_settori.rds\n\n")

# 8. Crescita salariale 2014-2022 per settore -----
cat("==== Crescita Salariale 2014-2022 per Settore ====\n\n")

crescita_settori <- dati_settore_sesso %>%
  filter(geo_level == "Nazionale", anno %in% c(2014, 2022)) %>%
  group_by(settore, anno) %>%
  summarise(
    salario_mediano = mean(salario_mediano, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = anno,
    values_from = salario_mediano,
    names_prefix = "sal_"
  ) %>%
  mutate(
    crescita_assoluta = sal_2022 - sal_2014,
    crescita_percentuale = ((sal_2022 - sal_2014) / sal_2014) * 100,
    crescita_annua = ((sal_2022 / sal_2014)^(1 / 8) - 1) * 100
  ) %>%
  arrange(desc(crescita_percentuale)) %>%
  mutate(
    crescita_pct_fmt = sprintf("%.1f%%", crescita_percentuale),
    crescita_annua_fmt = sprintf("%.2f%%", crescita_annua)
  )

cat("Top 10 settori per crescita salariale 2014-2022:\n")
print(
  crescita_settori %>%
    select(
      settore,
      sal_2014,
      sal_2022,
      crescita_pct_fmt,
      crescita_annua_fmt
    ) %>%
    head(10) %>%
    kable(
      col.names = c(
        "Settore",
        "Salario 2014",
        "Salario 2022",
        "Crescita %",
        "Crescita Annua %"
      ),
      digits = 2
    )
)

cat("\n\nBottom 10 settori per crescita salariale:\n")
print(
  crescita_settori %>%
    select(
      settore,
      sal_2014,
      sal_2022,
      crescita_pct_fmt,
      crescita_annua_fmt
    ) %>%
    tail(10) %>%
    kable(
      col.names = c(
        "Settore",
        "Salario 2014",
        "Salario 2022",
        "Crescita %",
        "Crescita Annua %"
      ),
      digits = 2
    )
)

saveRDS(crescita_settori, "output/crescita_settori_2014_2022.rds")
cat("\nSalvato: output/crescita_settori_2014_2022.rds\n\n")

# 9. Sintesi statistica generale -----
cat("==== Sintesi Statistica Generale ====\n\n")

sintesi <- list(
  n_settori = length(unique(dati_settore_sesso$settore)),
  n_anni = length(unique(dati_settore_sesso$anno)),
  n_ripartizioni = sum(stat_ripartizioni_2022$ripartizione != "Italia"),
  salario_mediano_nazionale_2022 = mean(
    dati_settore_sesso %>%
      filter(anno == 2022, geo_level == "Nazionale") %>%
      pull(salario_mediano),
    na.rm = TRUE
  ),
  gap_gender_medio_2022 = mean(gap_gender_2022$gap_percentuale, na.rm = TRUE),
  settore_max_salario = ranking_settori_2022$settore[1],
  settore_min_salario = ranking_settori_2022$settore[nrow(
    ranking_settori_2022
  )],
  crescita_media_2014_2022 = mean(
    crescita_settori$crescita_percentuale,
    na.rm = TRUE
  )
)

cat("Numero settori analizzati:", sintesi$n_settori, "\n")
cat("Periodo temporale:", sintesi$n_anni, "anni (2014-2022)\n")
cat("Numero ripartizioni:", sintesi$n_ripartizioni, "\n")
cat(
  "Salario mediano nazionale 2022: €",
  sprintf("%.2f", sintesi$salario_mediano_nazionale_2022),
  "/ora\n"
)
cat(
  "Gap di genere medio 2022:",
  sprintf("%.1f%%", sintesi$gap_gender_medio_2022),
  "\n"
)
cat("Settore con salario più alto:", sintesi$settore_max_salario, "\n")
cat("Settore con salario più basso:", sintesi$settore_min_salario, "\n")
cat(
  "Crescita salariale media 2014-2022:",
  sprintf("%.1f%%", sintesi$crescita_media_2014_2022),
  "\n"
)

saveRDS(sintesi, "output/sintesi_descrittive.rds")
cat("\nSalvato: output/sintesi_descrittive.rds\n\n")

# 10. Ranking settori NACE per salario mediano 2022 -----
cat("==== Ranking Settori NACE per Salario (2022) ====\n\n")

# Filtra solo sezioni NACE principali (esclude aggregati e dettagli)
ranking_settori_nace_2022 <- dati_settori %>%
  filter(
    anno == 2022,
    sesso == "totale",
    tipo_settore == "Sezione NACE"
  ) %>%
  arrange(desc(salario_mediano)) %>%
  mutate(
    rank = row_number(),
    salario_mediano_fmt = sprintf("€%.2f", salario_mediano),
    salario_medio_fmt = sprintf("€%.2f", salario_medio),
    D9_D1_fmt = sprintf("%.2f", D9_D1)
  )

cat("Numero sezioni NACE:", nrow(ranking_settori_nace_2022), "\n\n")

cat("Top 10 settori per salario mediano:\n")
print(
  ranking_settori_nace_2022 %>%
    select(rank, settore, salario_mediano_fmt, salario_medio_fmt, D9_D1_fmt) %>%
    head(10) %>%
    kable(
      col.names = c(
        "Rank",
        "Settore",
        "Salario Mediano",
        "Salario Medio",
        "D9/D1"
      ),
      align = c("r", "l", "r", "r", "r")
    )
)

cat("\n\nBottom 10 settori per salario mediano:\n")
print(
  ranking_settori_nace_2022 %>%
    select(rank, settore, salario_mediano_fmt, salario_medio_fmt, D9_D1_fmt) %>%
    tail(10) %>%
    kable(
      col.names = c(
        "Rank",
        "Settore",
        "Salario Mediano",
        "Salario Medio",
        "D9/D1"
      ),
      align = c("r", "l", "r", "r", "r")
    )
)

saveRDS(ranking_settori_nace_2022, "output/ranking_settori_nace_2022.rds")
cat("\nSalvato: output/ranking_settori_nace_2022.rds\n\n")

# 11. Dispersione salariale per settore 2022 -----
cat("==== Dispersione Salariale per Settore (2022) ====\n\n")

dispersione_settori_2022 <- dati_settori %>%
  filter(
    anno == 2022,
    sesso == "totale",
    tipo_settore == "Sezione NACE"
  ) %>%
  select(
    settore_code,
    settore,
    salario_mediano,
    salario_medio,
    D1,
    D5,
    D9,
    D9_D1,
    D9_D5,
    D5_D1
  ) %>%
  arrange(desc(D9_D1)) %>%
  mutate(
    rank_dispersione = row_number(),
    D9_D1_fmt = sprintf("%.2f", D9_D1),
    D9_D5_fmt = sprintf("%.2f", D9_D5),
    D5_D1_fmt = sprintf("%.2f", D5_D1)
  )

cat("Settori con maggiore dispersione salariale (D9/D1):\n")
print(
  dispersione_settori_2022 %>%
    select(settore, salario_mediano, D9_D1_fmt, D9_D5_fmt, D5_D1_fmt) %>%
    head(10) %>%
    kable(
      col.names = c("Settore", "Salario Med.", "D9/D1", "D9/D5", "D5/D1"),
      align = c("l", "r", "r", "r", "r"),
      digits = 2
    )
)

cat("\n\nSettori con minore dispersione salariale (D9/D1):\n")
print(
  dispersione_settori_2022 %>%
    select(settore, salario_mediano, D9_D1_fmt, D9_D5_fmt, D5_D1_fmt) %>%
    tail(10) %>%
    kable(
      col.names = c("Settore", "Salario Med.", "D9/D1", "D9/D5", "D5/D1"),
      align = c("l", "r", "r", "r", "r"),
      digits = 2
    )
)

saveRDS(dispersione_settori_2022, "output/dispersione_settori_2022.rds")
cat("\nSalvato: output/dispersione_settori_2022.rds\n\n")

# 12. Gap di genere per settore NACE -----
cat("==== Gap di Genere per Settore NACE ====\n\n")

gap_gender_settori <- dati_settori %>%
  filter(
    tipo_settore == "Sezione NACE",
    sesso %in% c("maschi", "femmine")
  ) %>%
  select(anno, settore_code, settore, sesso, salario_mediano) %>%
  pivot_wider(
    names_from = sesso,
    values_from = salario_mediano,
    names_prefix = "salario_"
  ) %>%
  mutate(
    gap_assoluto = salario_maschi - salario_femmine,
    gap_percentuale = ((salario_maschi - salario_femmine) / salario_femmine) *
      100,
    rapporto_M_F = salario_maschi / salario_femmine
  ) %>%
  filter(!is.na(gap_percentuale))

# Gap gender per settore nel 2022
gap_gender_settori_2022 <- gap_gender_settori %>%
  filter(anno == 2022) %>%
  arrange(desc(gap_percentuale)) %>%
  mutate(
    rank_gap = row_number(),
    gap_pct_fmt = sprintf("%.1f%%", gap_percentuale),
    rapporto_fmt = sprintf("%.2f", rapporto_M_F)
  )

cat("Settori con maggior gap di genere (2022):\n")
print(
  gap_gender_settori_2022 %>%
    select(
      settore,
      salario_femmine,
      salario_maschi,
      gap_pct_fmt,
      rapporto_fmt
    ) %>%
    head(10) %>%
    kable(
      col.names = c(
        "Settore",
        "Salario F (€/h)",
        "Salario M (€/h)",
        "Gap %",
        "Rapporto M/F"
      ),
      align = c("l", "r", "r", "r", "r"),
      digits = 2
    )
)

cat("\n\nSettori con minor gap di genere (2022):\n")
print(
  gap_gender_settori_2022 %>%
    select(
      settore,
      salario_femmine,
      salario_maschi,
      gap_pct_fmt,
      rapporto_fmt
    ) %>%
    tail(10) %>%
    kable(
      col.names = c(
        "Settore",
        "Salario F (€/h)",
        "Salario M (€/h)",
        "Gap %",
        "Rapporto M/F"
      ),
      align = c("l", "r", "r", "r", "r"),
      digits = 2
    )
)

saveRDS(gap_gender_settori, "output/gap_gender_settori.rds")
saveRDS(gap_gender_settori_2022, "output/gap_gender_settori_2022.rds")
cat("\nSalvato: output/gap_gender_settori*.rds\n\n")

cat("==== Script completato con successo ====\n")
cat("File creati nella directory output/:\n")
cat("- ranking_settori_2022.rds\n")
cat("- gap_gender.rds, gap_gender_2022.rds, gap_gender_tempo.rds\n")
cat("- gap_educazione_2022.rds (se disponibile)\n")
cat("- gap_contratto_2022.rds (se disponibile)\n")
cat("- stat_ripartizioni_2022.rds\n")
cat("- evoluzione_top5_settori.rds\n")
cat("- crescita_settori_2014_2022.rds\n")
cat("- sintesi_descrittive.rds\n")
cat("- ranking_settori_nace_2022.rds\n")
cat("- dispersione_settori_2022.rds\n")
cat("- gap_gender_settori.rds, gap_gender_settori_2022.rds\n")
