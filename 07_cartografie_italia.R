# 07_cartografie_italia.R -----
# Cartografie differenziali salariali province italiane
# Join basato su codici ISTAT (COD_UTS), NON su normalizzazione stringhe
# Autore: Giampaolo Montaletti (ORCID: 0009-0002-5327-1122)
# Data: 2026-01-16

# 1. Setup ambiente -----

if (!require("sf", quietly = TRUE)) {
  install.packages(
    "sf",
    repos = "https://cloud.r-project.org",
    dependencies = TRUE
  )
}
if (!require("ggspatial", quietly = TRUE)) {
  install.packages(
    "ggspatial",
    repos = "https://cloud.r-project.org",
    dependencies = TRUE
  )
}

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(ggplot2)
  library(viridis)
  library(ggspatial)
})

cat("==== Cartografie Differenziali Salariali Italia ====\n\n")

# 2. Carica shapefile ISTAT e dati -----

cat("==== Caricamento Dati ====\n\n")

# Shapefile province ISTAT (con COD_UTS)
shp_file <- "data/shp/province_italia_istat.rds"
if (!file.exists(shp_file)) {
  cat("ERRORE: shapefile ISTAT non trovato.\n")
  cat("Eseguire prima: Rscript 00_download_shapefile.R\n")
  stop("Shapefile mancante")
}

italy_sf <- readRDS(shp_file)
cat("Shapefile province ISTAT:", nrow(italy_sf), "geometrie\n")
cat("  Colonna join: COD_UTS\n")

# Dati RACLI (con cod_uts)
dati_sesso <- readRDS("output/dati_settore_sesso.rds")

# Verifica che cod_uts sia presente
if (!"cod_uts" %in% names(dati_sesso)) {
  cat("ERRORE: colonna cod_uts non trovata nei dati RACLI.\n")
  cat(
    "Eseguire prima: Rscript 00_crea_mapping_itter.R && Rscript 01_prepara_dati_racli.R\n"
  )
  stop("Codici ISTAT mancanti")
}

# Verifica file clustering (opzionale)
dati_clustering <- NULL
if (file.exists("output/dati_clustering.rds")) {
  dati_clustering <- readRDS("output/dati_clustering.rds")
}

# Filtra dati provinciali 2022
dati_province_2022 <- dati_sesso %>%
  filter(geo_level == "Provincia", sesso == "totale", anno == 2022) %>%
  select(area, area_label, ripartizione, salario_mediano, D9_D1, cod_uts)

cat("Dati RACLI province 2022:", nrow(dati_province_2022), "osservazioni\n\n")

# 3. Join shapefile con dati RACLI su COD_UTS -----

cat("==== Join Shapefile + Dati RACLI (su COD_UTS) ====\n\n")

# Join per codice ISTAT COD_UTS
map_data <- italy_sf %>%
  left_join(
    dati_province_2022,
    by = c("COD_UTS" = "cod_uts")
  )

# Check join success
n_matched <- sum(!is.na(map_data$salario_mediano))
n_total <- nrow(italy_sf)
match_rate <- n_matched / n_total * 100

cat(
  "Province matchate:",
  n_matched,
  "/",
  n_total,
  "(",
  sprintf("%.1f%%", match_rate),
  ")\n"
)

if (match_rate < 100) {
  unmatched <- italy_sf$DEN_UTS[is.na(map_data$salario_mediano)]
  cat("Non matchate:", paste(head(unmatched, 10), collapse = ", "), "\n")
} else {
  cat("✓ 100% match rate raggiunto (join su codici ISTAT)\n")
}

cat("\n")

# 4. Join con clustering -----

if (!is.null(dati_clustering) && "cluster_km" %in% names(dati_clustering)) {
  map_data <- map_data %>%
    left_join(
      dati_clustering %>% select(area, cluster_km, label),
      by = "area"
    )
  cat("Clustering data aggiunto\n\n")
}

# 5. Crea directory output grafici -----

if (!dir.exists("output/grafici")) {
  dir.create("output/grafici", recursive = TRUE)
}

# 6. Mappa 1: Salari mediani provinciali 2022 -----

cat("==== Mappa 1: Salari Mediani Provinciali 2022 ====\n")

p1 <- ggplot(map_data) +
  geom_sf(aes(fill = salario_mediano), color = "white", size = 0.1) +
  scale_fill_viridis_c(
    option = "viridis",
    name = "Salario\n(EUR/h)",
    na.value = "gray90",
    breaks = seq(10, 14, 1)
  ) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(
    location = "tr",
    style = north_arrow_fancy_orienteering,
    height = unit(1.5, "cm"),
    width = unit(1.5, "cm")
  ) +
  labs(
    title = "Retribuzioni Orarie Mediane per Provincia",
    subtitle = "Anno 2022 - Fonte: ISTAT RACLI",
    caption = "Elaborazione: Giampaolo Montaletti | Join su codici ISTAT (COD_UTS)"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(
      size = 11,
      hjust = 0.5,
      margin = margin(b = 10)
    ),
    plot.caption = element_text(size = 8, hjust = 1, margin = margin(t = 10)),
    legend.position = "right",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9)
  )

ggsave(
  "output/grafici/17_mappa_salari_province_2022.png",
  plot = p1,
  width = 10,
  height = 12,
  dpi = 300
)

cat("Salvato: output/grafici/17_mappa_salari_province_2022.png\n\n")

# 7. Mappa 2: Crescita salariale 2014-2022 -----

cat("==== Mappa 2: Crescita Salariale 2014-2022 ====\n")

# Calcola crescita per provincia
crescita_province <- dati_sesso %>%
  filter(
    geo_level == "Provincia",
    sesso == "totale",
    anno %in% c(2014, 2022)
  ) %>%
  select(area, area_label, anno, salario_mediano, cod_uts) %>%
  tidyr::pivot_wider(
    names_from = anno,
    values_from = salario_mediano,
    names_prefix = "sal_"
  ) %>%
  mutate(
    crescita_pct = (sal_2022 - sal_2014) / sal_2014 * 100
  )

# Join con shapefile usando COD_UTS
map_crescita <- italy_sf %>%
  left_join(crescita_province, by = c("COD_UTS" = "cod_uts"))

p2 <- ggplot(map_crescita) +
  geom_sf(aes(fill = crescita_pct), color = "white", size = 0.1) +
  scale_fill_gradient2(
    low = "#2166AC",
    mid = "white",
    high = "#B2182B",
    midpoint = 5,
    name = "Crescita\n(%)",
    na.value = "gray90"
  ) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(
    location = "tr",
    style = north_arrow_fancy_orienteering,
    height = unit(1.5, "cm"),
    width = unit(1.5, "cm")
  ) +
  labs(
    title = "Crescita Salariale per Provincia",
    subtitle = "Periodo 2014-2022 - Fonte: ISTAT RACLI",
    caption = "Elaborazione: Giampaolo Montaletti | Join su codici ISTAT (COD_UTS)"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(
      size = 11,
      hjust = 0.5,
      margin = margin(b = 10)
    ),
    plot.caption = element_text(size = 8, hjust = 1, margin = margin(t = 10)),
    legend.position = "right",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9)
  )

ggsave(
  "output/grafici/18_mappa_crescita_salari_2014_2022.png",
  plot = p2,
  width = 10,
  height = 12,
  dpi = 300
)

cat("Salvato: output/grafici/18_mappa_crescita_salari_2014_2022.png\n\n")

# 8. Mappa 3: Rapporto interdecile D9/D1 -----

cat("==== Mappa 3: Disuguaglianza (D9/D1) ====\n")

p3 <- ggplot(map_data) +
  geom_sf(aes(fill = D9_D1), color = "white", size = 0.1) +
  scale_fill_viridis_c(
    option = "plasma",
    name = "D9/D1",
    na.value = "gray90"
  ) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(
    location = "tr",
    style = north_arrow_fancy_orienteering,
    height = unit(1.5, "cm"),
    width = unit(1.5, "cm")
  ) +
  labs(
    title = "Disuguaglianza Salariale per Provincia",
    subtitle = "Rapporto D9/D1 - Anno 2022 - Fonte: ISTAT RACLI",
    caption = "Elaborazione: Giampaolo Montaletti | Join su codici ISTAT (COD_UTS)"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(
      size = 11,
      hjust = 0.5,
      margin = margin(b = 10)
    ),
    plot.caption = element_text(size = 8, hjust = 1, margin = margin(t = 10)),
    legend.position = "right",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9)
  )

ggsave(
  "output/grafici/19_mappa_disuguaglianza_D9_D1.png",
  plot = p3,
  width = 10,
  height = 12,
  dpi = 300
)

cat("Salvato: output/grafici/19_mappa_disuguaglianza_D9_D1.png\n\n")

# 9. Mappa 4: Cluster k-means -----

if (!is.null(dati_clustering) && "cluster_km" %in% names(map_data)) {
  cat("==== Mappa 4: Cluster Province ====\n")

  map_data <- map_data %>%
    mutate(cluster_factor = factor(cluster_km))

  p4 <- ggplot(map_data) +
    geom_sf(aes(fill = cluster_factor), color = "white", size = 0.1) +
    scale_fill_brewer(
      palette = "Set2",
      name = "Cluster",
      na.value = "gray90",
      labels = function(x) ifelse(!is.na(x), paste("Cluster", x), "N/A")
    ) +
    annotation_scale(location = "bl", width_hint = 0.3) +
    annotation_north_arrow(
      location = "tr",
      style = north_arrow_fancy_orienteering,
      height = unit(1.5, "cm"),
      width = unit(1.5, "cm")
    ) +
    labs(
      title = "Clustering Province per Profilo Salariale",
      subtitle = "K-means clustering - Anno 2022 - Fonte: ISTAT RACLI",
      caption = "Elaborazione: Giampaolo Montaletti | Join su codici ISTAT (COD_UTS)"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(
        size = 11,
        hjust = 0.5,
        margin = margin(b = 10)
      ),
      plot.caption = element_text(size = 8, hjust = 1, margin = margin(t = 10)),
      legend.position = "right",
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 9)
    )

  ggsave(
    "output/grafici/20_mappa_cluster_province.png",
    plot = p4,
    width = 10,
    height = 12,
    dpi = 300
  )

  cat("Salvato: output/grafici/20_mappa_cluster_province.png\n\n")
} else {
  cat("Dati clustering non disponibili, mappa cluster saltata\n\n")
}

# 10. Salva dataset per analisi future -----

saveRDS(map_data, "output/map_data_province.rds")
cat("Salvato: output/map_data_province.rds\n")

# 11. Sintesi finale -----

cat("\n==== Script completato con successo ====\n")
cat("Match rate join:", sprintf("%.1f%%", match_rate), "\n")
cat("Metodo join: codici ISTAT (COD_UTS), non normalizzazione stringhe\n\n")
cat("Mappe create:\n")
cat("  1. output/grafici/17_mappa_salari_province_2022.png\n")
cat("  2. output/grafici/18_mappa_crescita_salari_2014_2022.png\n")
cat("  3. output/grafici/19_mappa_disuguaglianza_D9_D1.png\n")
if (!is.null(dati_clustering) && "cluster_km" %in% names(map_data)) {
  cat("  4. output/grafici/20_mappa_cluster_province.png\n")
}
