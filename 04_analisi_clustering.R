# 04_analisi_clustering.R -----
# Clustering province: k-means e hierarchical clustering
# Autore: Giampaolo Montaletti (ORCID: 0009-0002-5327-1122)
# Data: 2026-01-14

# 1. Setup ambiente -----

# Installa pacchetti se mancanti
if (!require("factoextra", quietly = TRUE)) {
  install.packages(
    "factoextra",
    repos = "https://cloud.r-project.org",
    dependencies = TRUE
  )
}

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(cluster)
  library(factoextra)
})

cat("==== Clustering Province RACLI ====\n\n")

# Carica dati
indici_disug <- readRDS("output/indici_disuguaglianza.rds")

# 2. Preparazione dati per clustering -----

cat("==== Preparazione Dati Clustering ====\n\n")

# Estrai province 2022 con variabili rilevanti
dati_clustering <- indici_disug %>%
  filter(anno == 2022, geo_level == "Provincia") %>%
  select(area, area_label, salario_mediano, gini_approx, D9_D1)

# Aggiungi crescita da mobilità se disponibile
if (file.exists("output/mobilita_province.rds")) {
  mobilita <- readRDS("output/mobilita_province.rds")
  dati_clustering <- dati_clustering %>%
    left_join(
      mobilita %>% select(area, sal_2014, sal_2022),
      by = "area"
    ) %>%
    mutate(
      crescita_2014_2022 = ((sal_2022 - sal_2014) / sal_2014) * 100
    )
}

# Rimuovi NA
dati_clustering <- dati_clustering %>%
  filter(!is.na(salario_mediano) & !is.na(gini_approx))

cat("Province per clustering:", nrow(dati_clustering), "\n")
cat("Variabili utilizzate:\n")
print(names(dati_clustering))
cat("\n")

# Matrice per clustering (solo variabili numeriche)
vars_cluster <- c("salario_mediano", "gini_approx", "D9_D1")
if ("crescita_2014_2022" %in% names(dati_clustering)) {
  vars_cluster <- c(vars_cluster, "crescita_2014_2022")
}

X_raw <- dati_clustering %>%
  select(all_of(vars_cluster))

# Rimuovi colonne con varianza zero
col_sd <- apply(X_raw, 2, sd, na.rm = TRUE)
vars_valid <- names(col_sd)[col_sd > 0]

if (length(vars_valid) < length(vars_cluster)) {
  cat(
    "Rimosse variabili con varianza zero:",
    paste(setdiff(vars_cluster, vars_valid), collapse = ", "),
    "\n"
  )
  vars_cluster <- vars_valid
}

X <- X_raw %>%
  select(all_of(vars_cluster)) %>%
  scale() # Standardizza

# Rimuovi righe con NA dopo scaling
rows_complete <- complete.cases(X)
if (sum(!rows_complete) > 0) {
  cat("Rimosse", sum(!rows_complete), "province con valori mancanti\n")
  X <- X[rows_complete, ]
  dati_clustering <- dati_clustering[rows_complete, ]
}

rownames(X) <- dati_clustering$area_label

cat("Matrice clustering:", nrow(X), "× ", ncol(X), " (standardizzata)\n\n")

# 3. Determinazione numero ottimale cluster -----

cat("==== Determinazione Numero Ottimale Cluster ====\n\n")

# Elbow method
set.seed(123)
wss <- sapply(1:10, function(k) {
  kmeans(X, centers = k, nstart = 25)$tot.withinss
})

# Silhouette method
sil_width <- sapply(2:10, function(k) {
  km <- kmeans(X, centers = k, nstart = 25)
  ss <- silhouette(km$cluster, dist(X))
  mean(ss[, 3])
})

cat("Silhouette width medio per k:\n")
sil_df <- data.frame(
  k = 2:10,
  silhouette = sil_width
)
print(sil_df)

# Numero ottimale (massimo silhouette)
k_opt <- sil_df$k[which.max(sil_df$silhouette)]
cat("\n\nNumero ottimale cluster (massimo silhouette): k =", k_opt, "\n\n")

# 4. K-means clustering -----

cat("==== K-means Clustering (k =", k_opt, ") ====\n\n")

set.seed(123)
km_result <- kmeans(X, centers = k_opt, nstart = 25)

# Aggiungi cluster ai dati
dati_clustering$cluster_km <- km_result$cluster

# Statistiche per cluster
cluster_stats <- dati_clustering %>%
  group_by(cluster_km) %>%
  summarise(
    n_province = n(),
    sal_medio = mean(salario_mediano, na.rm = TRUE),
    sal_sd = sd(salario_mediano, na.rm = TRUE),
    gini_medio = mean(gini_approx, na.rm = TRUE),
    D9_D1_medio = mean(D9_D1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(sal_medio))

cat("Statistiche cluster:\n")
print(
  cluster_stats %>%
    mutate(
      sal_fmt = sprintf("€%.2f ± %.2f", sal_medio, sal_sd),
      gini_fmt = sprintf("%.3f", gini_medio),
      D9_D1_fmt = sprintf("%.2f", D9_D1_medio)
    ) %>%
    knitr::kable()
)

# Esempi province per cluster
cat("\n\nEsempi province per cluster:\n")
for (k in 1:k_opt) {
  province_k <- dati_clustering %>%
    filter(cluster_km == k) %>%
    arrange(desc(salario_mediano)) %>%
    head(5) %>%
    pull(area_label)

  cat("\nCluster", k, ":", paste(province_k, collapse = ", "), "\n")
}

saveRDS(km_result, "output/kmeans_result.rds")
saveRDS(dati_clustering, "output/dati_clustering.rds")
cat("\n\nSalvato: output/kmeans_result.rds\n")
cat("Salvato: output/dati_clustering.rds\n\n")

# 5. Hierarchical clustering -----

cat("==== Hierarchical Clustering ====\n\n")

# Calcola distanze
dist_matrix <- dist(X, method = "euclidean")

# Hierarchical clustering (Ward's method)
hc_result <- hclust(dist_matrix, method = "ward.D2")

# Taglia dendrogramma a k_opt cluster
cluster_hc <- cutree(hc_result, k = k_opt)
dati_clustering$cluster_hc <- cluster_hc

# Confronto k-means vs hierarchical
cat("Confronto k-means vs hierarchical:\n")
table_confronto <- table(
  KMeans = dati_clustering$cluster_km,
  Hierarchical = dati_clustering$cluster_hc
)
print(table_confronto)

# Accordo tra metodi
accordo <- sum(diag(table_confronto)) / sum(table_confronto)
cat("\nAccordo tra metodi:", sprintf("%.1f%%", accordo * 100), "\n\n")

saveRDS(hc_result, "output/hclust_result.rds")
cat("Salvato: output/hclust_result.rds\n\n")

# 6. Caratterizzazione cluster -----

cat("==== Caratterizzazione Cluster ====\n\n")

# Interpretazione cluster (basata su k-means)
cluster_labels <- cluster_stats %>%
  mutate(
    label = case_when(
      row_number() == 1 ~ "Alto reddito",
      row_number() == k_opt ~ "Basso reddito",
      sal_medio > median(cluster_stats$sal_medio) ~ "Medio-alto",
      TRUE ~ "Medio-basso"
    )
  ) %>%
  select(cluster_km, label)

dati_clustering <- dati_clustering %>%
  left_join(cluster_labels, by = "cluster_km")

cat("Etichette cluster:\n")
cluster_summary <- dati_clustering %>%
  group_by(cluster_km, label) %>%
  summarise(
    n = n(),
    esempio_province = paste(head(area_label, 3), collapse = ", "),
    .groups = "drop"
  ) %>%
  arrange(cluster_km)

print(knitr::kable(cluster_summary))

saveRDS(dati_clustering, "output/dati_clustering.rds")
cat("\nAggiornato: output/dati_clustering.rds\n\n")

# 7. Sintesi clustering -----

cat("==== Sintesi Clustering ====\n\n")

sintesi_clustering <- list(
  n_province = nrow(dati_clustering),
  k_ottimale = k_opt,
  silhouette_medio = max(sil_width),
  accordo_metodi = accordo,
  variabili_usate = vars_cluster
)

cat("Province analizzate:", sintesi_clustering$n_province, "\n")
cat("Numero cluster ottimale:", sintesi_clustering$k_ottimale, "\n")
cat(
  "Silhouette medio:",
  sprintf("%.3f", sintesi_clustering$silhouette_medio),
  "\n"
)
cat(
  "Accordo k-means/hierarchical:",
  sprintf("%.1f%%", sintesi_clustering$accordo_metodi * 100),
  "\n"
)
cat(
  "Variabili utilizzate:",
  paste(sintesi_clustering$variabili_usate, collapse = ", "),
  "\n"
)

saveRDS(sintesi_clustering, "output/sintesi_clustering.rds")
cat("\nSalvato: output/sintesi_clustering.rds\n\n")

cat("==== Script completato con successo ====\n")
cat("File creati nella directory output/:\n")
cat("- kmeans_result.rds\n")
cat("- hclust_result.rds\n")
cat("- dati_clustering.rds\n")
cat("- sintesi_clustering.rds\n")
