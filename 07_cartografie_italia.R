# 07_cartografie_italia.R -----
# Cartografie differenziali salariali province italiane
# Autore: Giampaolo Montaletti (ORCID: 0009-0002-5327-1122)
# Data: 2026-01-14

# 1. Setup ambiente -----

# Installa pacchetti se mancanti
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

# Funzione di normalizzazione nomi province per matching robusto
normalize_province_name <- function(x) {
  x <- toupper(trimws(x))
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x <- gsub("-", " ", x)
  x <- gsub("\\s+", " ", x)
  # Gestisci variazioni note tra RACLI e GADM
  x <- gsub("REGGIO NELL'EMILIA", "REGGIO EMILIA", x)
  x <- gsub("REGGIO DI CALABRIA", "REGGIO CALABRIA", x)
  x <- gsub("PESARO E URBINO", "PESARO URBINO", x)
  x <- gsub("MASSA CARRARA", "MASSA", x)
  x <- gsub("VERBANO CUSIO OSSOLA", "VERBANIA", x)
  x <- gsub("MONZA E DELLA BRIANZA", "MONZA BRIANZA", x)
  x <- gsub("BARLETTA ANDRIA TRANI", "BARLETTA", x)
  x <- gsub("SUD SARDEGNA", "CARBONIA IGLESIAS", x)
  return(x)
}

# 2. Carica shapefile e dati -----

cat("==== Caricamento Dati ====\n\n")

# Shapefile province
italy_sf <- readRDS("data/shp/province_italia_gadm.rds")
cat("Shapefile province: ", nrow(italy_sf), " geometrie\n")

# Dati RACLI
dati_sesso <- readRDS("output/dati_settore_sesso.rds")
dati_clustering <- readRDS("output/dati_clustering.rds")

# Filtra dati provinciali 2022
dati_province_2022 <- dati_sesso %>%
  filter(geo_level == "Provincia", sesso == "totale", anno == 2022) %>%
  select(area, area_label, ripartizione, salario_mediano, D9_D1)

cat("Dati RACLI province 2022: ", nrow(dati_province_2022), " osservazioni\n\n")

# 3. Join shapefile con dati RACLI -----

cat("==== Join Shapefile + Dati RACLI ====\n\n")

# Join per nome provincia con normalizzazione robusta
# GADM usa NAME_2 per nome provincia
map_data <- italy_sf %>%
  mutate(provincia_clean = normalize_province_name(NAME_2)) %>%
  left_join(
    dati_province_2022 %>%
      mutate(provincia_clean = normalize_province_name(area_label)),
    by = "provincia_clean"
  )

# Check join success
n_matched <- sum(!is.na(map_data$salario_mediano))
cat("Province matchate: ", n_matched, "/", nrow(italy_sf), "\n")

if (n_matched < 50) {
  cat("⚠ ATTENZIONE: poche province matchate, verificare nomi\n")
  cat("\nEsempio GADM:\n")
  print(head(italy_sf$NAME_2, 10))
  cat("\nEsempio RACLI:\n")
  print(head(dati_province_2022$area_label, 10))
}

cat("\n")

# 4. Join con clustering -----

if ("cluster_km" %in% names(dati_clustering)) {
  map_data <- map_data %>%
    left_join(
      dati_clustering %>% select(area, cluster_km, label),
      by = "area"
    )

  cat("Clustering data aggiunto\n\n")
}

# 5. Mappa 1: Salari mediani provinciali 2022 -----

cat("==== Mappa 1: Salari Mediani Provinciali 2022 ====\n")

p1 <- ggplot(map_data) +
  geom_sf(aes(fill = salario_mediano), color = "white", size = 0.1) +
  scale_fill_viridis_c(
    option = "viridis",
    name = "Salario\n(€/h)",
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
    caption = "Elaborazione: Giampaolo Montaletti"
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

# 6. Mappa 2: Crescita salariale 2014-2022 -----

cat("==== Mappa 2: Crescita Salariale 2014-2022 ====\n")

# Calcola crescita per provincia
crescita_province <- dati_sesso %>%
  filter(
    geo_level == "Provincia",
    sesso == "totale",
    anno %in% c(2014, 2022)
  ) %>%
  select(area, area_label, anno, salario_mediano) %>%
  tidyr::pivot_wider(
    names_from = anno,
    values_from = salario_mediano,
    names_prefix = "sal_"
  ) %>%
  mutate(
    crescita_pct = (sal_2022 - sal_2014) / sal_2014 * 100,
    provincia_clean = normalize_province_name(area_label)
  )

# Join con shapefile usando normalizzazione robusta
map_crescita <- italy_sf %>%
  mutate(provincia_clean = normalize_province_name(NAME_2)) %>%
  left_join(crescita_province, by = "provincia_clean")

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
    caption = "Elaborazione: Giampaolo Montaletti"
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

# 7. Mappa 3: Rapporto interdecile D9/D1 -----

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
    caption = "Elaborazione: Giampaolo Montaletti"
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

# 8. Mappa 4: Cluster k-means -----

if ("cluster_km" %in% names(map_data)) {
  cat("==== Mappa 4: Cluster Province ====\n")

  # Converte cluster in factor per colori categoriali
  map_data <- map_data %>%
    mutate(cluster_factor = factor(cluster_km))

  p4 <- ggplot(map_data) +
    geom_sf(aes(fill = cluster_factor), color = "white", size = 0.1) +
    scale_fill_brewer(
      palette = "Set2",
      name = "Cluster",
      na.value = "gray90",
      labels = function(x) {
        ifelse(!is.na(x), paste("Cluster", x), "N/A")
      }
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
      caption = "Elaborazione: Giampaolo Montaletti"
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
  cat("⚠ Dati clustering non disponibili, mappa cluster saltata\n\n")
}

# 9. Salva dataset completo per analisi future -----

saveRDS(map_data, "output/map_data_province.rds")
cat("Salvato: output/map_data_province.rds\n")

# 10. Sintesi -----

cat("\n==== Script completato con successo ====\n")
cat("Mappe create:\n")
cat("  1. output/grafici/17_mappa_salari_province_2022.png\n")
cat("  2. output/grafici/18_mappa_crescita_salari_2014_2022.png\n")
cat("  3. output/grafici/19_mappa_disuguaglianza_D9_D1.png\n")
if ("cluster_km" %in% names(map_data)) {
  cat("  4. output/grafici/20_mappa_cluster_province.png\n")
}
