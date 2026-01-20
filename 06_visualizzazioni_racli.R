# 06_visualizzazioni_racli.R -----
# Visualizzazioni grafici RACLI
# Autore: Giampaolo Montaletti (ORCID: 0009-0002-5327-1122)
# Data: 2026-01-14

# 1. Setup ambiente -----
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(patchwork)
  library(scales)
})

cat("==== Visualizzazioni RACLI ====\n\n")

# Carica theme esistente
source("visualizzazioni_salari.R")

# Crea directory per grafici
if (!dir.exists("output/grafici")) {
  dir.create("output/grafici", recursive = TRUE)
}

# Carica dati
dati_sesso <- readRDS("output/dati_settore_sesso.rds")
gap_gender_tempo <- readRDS("output/gap_gender_tempo.rds")
evoluzione_disug <- readRDS("output/evoluzione_disuguaglianza.rds")
indici_disug <- readRDS("output/indici_disuguaglianza.rds")
dati_clustering <- readRDS("output/dati_clustering.rds")
gap_educazione_2022 <- readRDS("output/gap_educazione_2022.rds")
gap_contratto_2022 <- readRDS("output/gap_contratto_2022.rds")

cat("Dati caricati. Generazione grafici in corso...\n\n")

# 2. Evoluzione temporale salari -----

cat("1. Evoluzione temporale salari nazionali...\n")

p1 <- dati_sesso %>%
  filter(geo_level == "Nazionale", sesso == "totale") %>%
  ggplot(aes(x = anno, y = salario_mediano)) +
  geom_line(linewidth = 1, color = "#2E86AB") +
  geom_point(size = 2, color = "#2E86AB") +
  scale_y_continuous(labels = label_dollar(prefix = "€", suffix = "")) +
  labs(
    title = "Evoluzione Salario Mediano Nazionale",
    subtitle = "Italia 2014-2022",
    x = "Anno",
    y = "Salario mediano orario (€/h)"
  ) +
  theme_salari()

ggsave(
  "output/grafici/01_evoluzione_salari_nazionale.png",
  p1,
  width = 10,
  height = 6,
  dpi = 300
)

# 3. Gap di genere nel tempo -----

cat("2. Gap di genere nel tempo...\n")

p2 <- gap_gender_tempo %>%
  ggplot(aes(x = anno, y = gap_medio)) +
  geom_line(linewidth = 1, color = "#A23B72") +
  geom_point(size = 2, color = "#A23B72") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title = "Evoluzione Gap di Genere",
    subtitle = "Differenza salariale Maschi vs Femmine",
    x = "Anno",
    y = "Gap di genere (%)"
  ) +
  theme_salari()

ggsave(
  "output/grafici/02_gap_genere_tempo.png",
  p2,
  width = 10,
  height = 6,
  dpi = 300
)

# 4. Salari per ripartizione -----

cat("3. Salari per ripartizione geografica...\n")

p3 <- dati_sesso %>%
  filter(geo_level == "Ripartizione", sesso == "totale", anno == 2022) %>%
  mutate(ripartizione = reorder(ripartizione, salario_mediano)) %>%
  ggplot(aes(x = ripartizione, y = salario_mediano, fill = ripartizione)) +
  geom_col(show.legend = FALSE) +
  scale_fill_viridis_d(option = "plasma") +
  scale_y_continuous(labels = label_dollar(prefix = "€", suffix = "")) +
  labs(
    title = "Salari Mediani per Ripartizione Geografica",
    subtitle = "Anno 2022",
    x = NULL,
    y = "Salario mediano orario (€/h)"
  ) +
  theme_salari() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  "output/grafici/03_salari_ripartizioni.png",
  p3,
  width = 10,
  height = 6,
  dpi = 300
)

# 5. Evoluzione disuguaglianza (Gini) -----

cat("4. Evoluzione indice Gini...\n")

p4 <- evoluzione_disug %>%
  ggplot(aes(x = anno, y = gini)) +
  geom_line(linewidth = 1, color = "#F18F01") +
  geom_point(size = 2, color = "#F18F01") +
  scale_y_continuous(limits = c(0.16, 0.17)) +
  labs(
    title = "Evoluzione Indice di Gini",
    subtitle = "Disuguaglianza salariale nazionale",
    x = "Anno",
    y = "Indice di Gini"
  ) +
  theme_salari()

ggsave(
  "output/grafici/04_evoluzione_gini.png",
  p4,
  width = 10,
  height = 6,
  dpi = 300
)

# 6. Rapporto D9/D1 nel tempo -----

cat("5. Rapporto interdecile D9/D1...\n")

p5 <- evoluzione_disug %>%
  ggplot(aes(x = anno, y = D9_D1)) +
  geom_line(linewidth = 1, color = "#C73E1D") +
  geom_point(size = 2, color = "#C73E1D") +
  labs(
    title = "Rapporto Interdecile D9/D1",
    subtitle = "Quanto guadagna il 90° percentile rispetto al 10°",
    x = "Anno",
    y = "Rapporto D9/D1"
  ) +
  theme_salari()

ggsave(
  "output/grafici/05_rapporto_D9_D1.png",
  p5,
  width = 10,
  height = 6,
  dpi = 300
)

# 7. Box plot salari per ripartizione (con distribuzione) -----

cat("6. Distribuzione salari per ripartizione...\n")

p6 <- indici_disug %>%
  filter(anno == 2022, geo_level == "Provincia") %>%
  mutate(ripartizione = reorder(ripartizione, salario_mediano, median)) %>%
  ggplot(aes(x = ripartizione, y = salario_mediano, fill = ripartizione)) +
  geom_boxplot(show.legend = FALSE, alpha = 0.7) +
  scale_fill_viridis_d(option = "plasma") +
  scale_y_continuous(labels = label_dollar(prefix = "€", suffix = "")) +
  labs(
    title = "Distribuzione Salari Provinciali per Ripartizione",
    subtitle = "Anno 2022 (box plot)",
    x = NULL,
    y = "Salario mediano orario (€/h)"
  ) +
  theme_salari() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  "output/grafici/06_boxplot_salari_ripartizioni.png",
  p6,
  width = 10,
  height = 6,
  dpi = 300
)

# 8. Scatter plot disuguaglianza vs salario -----

cat("7. Disuguaglianza vs livello salariale...\n")

p7 <- indici_disug %>%
  filter(anno == 2022, geo_level == "Provincia") %>%
  ggplot(aes(x = salario_mediano, y = gini_approx, color = ripartizione)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  scale_x_continuous(labels = label_dollar(prefix = "€", suffix = "")) +
  scale_color_viridis_d(option = "plasma") +
  labs(
    title = "Disuguaglianza vs Livello Salariale",
    subtitle = "Province italiane 2022",
    x = "Salario mediano (€/h)",
    y = "Indice di Gini",
    color = "Ripartizione"
  ) +
  theme_salari()

ggsave(
  "output/grafici/07_scatter_gini_salario.png",
  p7,
  width = 10,
  height = 6,
  dpi = 300
)

# 9. Cluster k-means -----

cat("8. Visualizzazione cluster province...\n")

p8 <- dati_clustering %>%
  ggplot(aes(x = salario_mediano, y = gini_approx, color = label)) +
  geom_point(size = 2.5, alpha = 0.7) +
  scale_x_continuous(labels = label_dollar(prefix = "€", suffix = "")) +
  scale_color_manual(
    values = c("Alto reddito" = "#2E86AB", "Basso reddito" = "#A23B72")
  ) +
  labs(
    title = "Clustering Province (K-means, k=2)",
    subtitle = "Basato su salario mediano, Gini e crescita 2014-2022",
    x = "Salario mediano 2022 (€/h)",
    y = "Indice di Gini 2022",
    color = "Cluster"
  ) +
  theme_salari()

ggsave(
  "output/grafici/08_cluster_province.png",
  p8,
  width = 10,
  height = 6,
  dpi = 300
)

# 10. Premio educativo -----

cat("9. Premio educativo per livello...\n")

p9 <- gap_educazione_2022 %>%
  mutate(educazione = reorder(educazione, salario_mediano)) %>%
  ggplot(aes(x = educazione, y = salario_mediano, fill = educazione)) +
  geom_col(show.legend = FALSE) +
  scale_fill_viridis_d(option = "viridis") +
  scale_y_continuous(labels = label_dollar(prefix = "€", suffix = "")) +
  labs(
    title = "Salari per Livello Educativo",
    subtitle = "Italia 2022",
    x = NULL,
    y = "Salario mediano orario (€/h)"
  ) +
  theme_salari() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  "output/grafici/09_salari_educazione.png",
  p9,
  width = 10,
  height = 6,
  dpi = 300
)

# 11. Premio contratto permanente -----

cat("10. Salari per tipo contratto...\n")

p10 <- gap_contratto_2022 %>%
  mutate(contratto = reorder(contratto, salario_mediano)) %>%
  ggplot(aes(x = contratto, y = salario_mediano, fill = contratto)) +
  geom_col(show.legend = FALSE) +
  scale_fill_viridis_d(option = "mako") +
  scale_y_continuous(labels = label_dollar(prefix = "€", suffix = "")) +
  labs(
    title = "Salari per Tipo Contratto",
    subtitle = "Italia 2022",
    x = NULL,
    y = "Salario mediano orario (€/h)"
  ) +
  theme_salari() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  "output/grafici/10_salari_contratto.png",
  p10,
  width = 10,
  height = 6,
  dpi = 300
)

# 12. Salari maschi vs femmine per ripartizione -----

cat("11. Salari maschi vs femmine per ripartizione...\n")

p11 <- dati_sesso %>%
  filter(
    geo_level == "Ripartizione",
    sesso %in% c("maschi", "femmine"),
    anno == 2022
  ) %>%
  mutate(ripartizione = reorder(ripartizione, salario_mediano, mean)) %>%
  ggplot(aes(x = ripartizione, y = salario_mediano, fill = sesso)) +
  geom_col(position = "dodge") +
  scale_fill_manual(
    values = c("maschi" = "#2E86AB", "femmine" = "#A23B72"),
    labels = c("Maschi", "Femmine")
  ) +
  scale_y_continuous(labels = label_dollar(prefix = "€", suffix = "")) +
  labs(
    title = "Salari per Sesso e Ripartizione",
    subtitle = "Anno 2022",
    x = NULL,
    y = "Salario mediano orario (€/h)",
    fill = "Sesso"
  ) +
  theme_salari() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  "output/grafici/11_salari_sesso_ripartizioni.png",
  p11,
  width = 10,
  height = 6,
  dpi = 300
)

# 13. Fan chart decili (D1, D5, D9) -----

cat("12. Fan chart distribuzione decili...\n")

decili_data <- dati_sesso %>%
  filter(geo_level == "Nazionale", sesso == "totale") %>%
  select(anno, D1, D5, D9)

p12 <- ggplot(decili_data, aes(x = anno)) +
  geom_ribbon(aes(ymin = D1, ymax = D9), alpha = 0.2, fill = "#2E86AB") +
  geom_line(aes(y = D1, color = "D1"), linewidth = 1) +
  geom_line(aes(y = D5, color = "D5"), linewidth = 1) +
  geom_line(aes(y = D9, color = "D9"), linewidth = 1) +
  scale_color_manual(
    values = c("D1" = "#C73E1D", "D5" = "#2E86AB", "D9" = "#06A77D"),
    labels = c("D1 (10° percentile)", "D5 (Mediana)", "D9 (90° percentile)")
  ) +
  scale_y_continuous(labels = label_dollar(prefix = "€", suffix = "")) +
  labs(
    title = "Fan Chart Distribuzione Salariale",
    subtitle = "Evoluzione decili D1, D5, D9 (2014-2022)",
    x = "Anno",
    y = "Salario orario (€/h)",
    color = "Decile"
  ) +
  theme_salari()

ggsave(
  "output/grafici/12_fan_chart_decili.png",
  p12,
  width = 10,
  height = 6,
  dpi = 300
)

# 14. Convergenza sigma (CV nel tempo) -----

cat("13. Convergenza sigma...\n")

sigma_conv <- readRDS("output/sigma_convergence.rds")

p13 <- sigma_conv %>%
  ggplot(aes(x = anno, y = cv)) +
  geom_line(linewidth = 1, color = "#F18F01") +
  geom_point(size = 2, color = "#F18F01") +
  labs(
    title = "σ-Convergenza Territoriale",
    subtitle = "Coefficiente di variazione salari provinciali",
    x = "Anno",
    y = "Coefficiente di variazione"
  ) +
  theme_salari()

ggsave(
  "output/grafici/13_sigma_convergenza.png",
  p13,
  width = 10,
  height = 6,
  dpi = 300
)

# 15. Beta convergenza (scatter) -----

cat("14. Beta convergenza...\n")

if (file.exists("output/beta_convergence.rds")) {
  beta_conv <- readRDS("output/beta_convergence.rds")

  p14 <- beta_conv %>%
    ggplot(aes(x = log_sal_2014, y = tasso_crescita_annuo)) +
    geom_point(size = 2, alpha = 0.6, color = "#2E86AB") +
    geom_smooth(method = "lm", se = TRUE, color = "#C73E1D") +
    scale_x_continuous(labels = function(x) sprintf("€%.1f", exp(x))) +
    scale_y_continuous(labels = label_percent(scale = 1)) +
    labs(
      title = "β-Convergenza: Province Povere Crescono Più Velocemente?",
      subtitle = "Regressione crescita 2014-2022 su livello iniziale",
      x = "Salario 2014 (log scale, €/h)",
      y = "Tasso crescita annuo (%)"
    ) +
    theme_salari()

  ggsave(
    "output/grafici/14_beta_convergenza.png",
    p14,
    width = 10,
    height = 6,
    dpi = 300
  )
}

# 16. Top 10 e Bottom 10 province per salario -----

cat("15. Top/Bottom 10 province...\n")

province_ranking <- indici_disug %>%
  filter(anno == 2022, geo_level == "Provincia") %>%
  arrange(desc(salario_mediano))

top_bottom <- bind_rows(
  province_ranking %>% head(10) %>% mutate(gruppo = "Top 10"),
  province_ranking %>% tail(10) %>% mutate(gruppo = "Bottom 10")
) %>%
  mutate(area_label = reorder(area_label, salario_mediano))

p15 <- top_bottom %>%
  ggplot(aes(x = area_label, y = salario_mediano, fill = gruppo)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("Top 10" = "#06A77D", "Bottom 10" = "#C73E1D")) +
  scale_y_continuous(labels = label_dollar(prefix = "€", suffix = "")) +
  labs(
    title = "Top 10 e Bottom 10 Province per Salario Mediano",
    subtitle = "Anno 2022",
    x = NULL,
    y = "Salario mediano orario (€/h)",
    fill = NULL
  ) +
  theme_salari()

ggsave(
  "output/grafici/15_top_bottom_province.png",
  p15,
  width = 10,
  height = 8,
  dpi = 300
)

# 17. Ranking settori NACE per salario mediano -----

cat("16. Ranking settori NACE per salario mediano...\n")

# Carica dati settoriali
ranking_settori_nace <- readRDS("output/ranking_settori_nace_2022.rds")

# Prepara dati per grafico (limita a primi 20 per leggibilità)
ranking_plot_data <- ranking_settori_nace %>%
  head(20) %>%
  mutate(
    settore_short = ifelse(
      nchar(settore) > 50,
      paste0(substr(settore, 1, 47), "..."),
      settore
    ),
    settore_short = reorder(settore_short, salario_mediano)
  )

p16 <- ranking_plot_data %>%
  ggplot(aes(x = settore_short, y = salario_mediano, fill = salario_mediano)) +
  geom_col(show.legend = FALSE) +
  geom_text(
    aes(label = sprintf("€%.2f", salario_mediano)),
    hjust = -0.1,
    size = 3
  ) +
  coord_flip() +
  scale_fill_viridis_c(option = "plasma") +
  scale_y_continuous(
    labels = label_dollar(prefix = "€", suffix = ""),
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Ranking Settori NACE per Salario Mediano",
    subtitle = "Top 20 settori - Anno 2022",
    x = NULL,
    y = "Salario mediano orario (€/h)"
  ) +
  theme_salari() +
  theme(axis.text.y = element_text(size = 8))

ggsave(
  "output/grafici/16_salari_settori_nace.png",
  p16,
  width = 12,
  height = 10,
  dpi = 300
)

# 18. Scatter plot salari vs dispersione per settore -----

cat("17. Scatter salari vs dispersione per settore...\n")

dispersione_settori <- readRDS("output/dispersione_settori_2022.rds")

p17 <- dispersione_settori %>%
  ggplot(aes(x = salario_mediano, y = D9_D1)) +
  geom_point(aes(size = salario_medio), alpha = 0.7, color = "#2E86AB") +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "#C73E1D",
    linetype = "dashed"
  ) +
  geom_text(
    aes(label = settore_code),
    hjust = -0.2,
    vjust = 0.5,
    size = 2.5,
    alpha = 0.7
  ) +
  scale_x_continuous(labels = label_dollar(prefix = "€", suffix = "")) +
  scale_size_continuous(
    name = "Salario Medio",
    labels = label_dollar(prefix = "€", suffix = "")
  ) +
  labs(
    title = "Salari vs Dispersione Salariale per Settore NACE",
    subtitle = "Anno 2022 - Codici NACE 2007",
    x = "Salario mediano (€/h)",
    y = "Rapporto interdecile D9/D1",
    caption = "Linea: regressione lineare; dimensione: salario medio"
  ) +
  theme_salari()

ggsave(
  "output/grafici/17_scatter_salari_dispersione_settori.png",
  p17,
  width = 12,
  height = 8,
  dpi = 300
)

# 19. Gap di genere per settore -----

cat("18. Gap di genere per settore NACE...\n")

gap_gender_settori <- readRDS("output/gap_gender_settori_2022.rds")

# Prepara dati (top 20 per gap)
gap_plot_data <- gap_gender_settori %>%
  head(20) %>%
  mutate(
    settore_short = ifelse(
      nchar(settore) > 50,
      paste0(substr(settore, 1, 47), "..."),
      settore
    ),
    settore_short = reorder(settore_short, gap_percentuale)
  )

p18 <- gap_plot_data %>%
  ggplot(aes(x = settore_short, y = gap_percentuale, fill = gap_percentuale)) +
  geom_col(show.legend = FALSE) +
  geom_text(
    aes(label = sprintf("%.1f%%", gap_percentuale)),
    hjust = -0.1,
    size = 3
  ) +
  coord_flip() +
  scale_fill_gradient2(
    low = "#06A77D",
    mid = "#F18F01",
    high = "#C73E1D",
    midpoint = median(gap_plot_data$gap_percentuale)
  ) +
  scale_y_continuous(
    labels = label_percent(scale = 1),
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Gap Salariale di Genere per Settore NACE",
    subtitle = "Top 20 settori con maggior differenziale M/F - Anno 2022",
    x = NULL,
    y = "Gap di genere (%)",
    caption = "Gap = (Salario M - Salario F) / Salario F × 100"
  ) +
  theme_salari() +
  theme(axis.text.y = element_text(size = 8))

ggsave(
  "output/grafici/18_gap_genere_settori.png",
  p18,
  width = 12,
  height = 10,
  dpi = 300
)

# 19. Heatmap settori × ripartizioni -----

cat("19. Heatmap settori × ripartizioni...\n")

# Carica dati settore × territorio
dati_st_2022 <- readRDS("output/dati_settori_territorio_2022.rds")

# Prepara dati per heatmap
heatmap_data <- dati_st_2022 %>%
  mutate(
    settore_short = ifelse(
      nchar(settore) > 35,
      paste0(substr(settore, 1, 32), "..."),
      settore
    )
  )

# Ordina ripartizioni da Nord a Sud
ripart_order <- c("Nord-ovest", "Nord-est", "Centro", "Sud", "Isole")
heatmap_data$ripartizione <- factor(
  heatmap_data$ripartizione,
  levels = ripart_order
)

# Ordina settori per salario mediano medio
settori_ord <- heatmap_data %>%
  group_by(settore_short) %>%
  summarise(sal_medio = mean(salario_mediano, na.rm = TRUE)) %>%
  arrange(desc(sal_medio)) %>%
  pull(settore_short)

heatmap_data$settore_short <- factor(
  heatmap_data$settore_short,
  levels = rev(settori_ord)
)

p19 <- heatmap_data %>%
  ggplot(aes(x = ripartizione, y = settore_short, fill = salario_mediano)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_label(
    aes(label = sprintf("€%.1f", salario_mediano)),
    size = 2.3,
    fill = "white",
    alpha = 0.85,
    label.size = 0,
    label.padding = unit(0.12, "lines")
  ) +
  scale_fill_viridis_c(
    option = "plasma",
    name = "Salario\nmediano\n(€/h)",
    labels = label_dollar(prefix = "€", suffix = "")
  ) +
  labs(
    title = "Salario Mediano per Settore e Ripartizione Geografica",
    subtitle = "Anno 2022 - Sezioni NACE",
    x = NULL,
    y = NULL
  ) +
  theme_salari() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 8),
    legend.position = "right"
  )

ggsave(
  "output/grafici/19_heatmap_settori_ripartizioni.png",
  p19,
  width = 12,
  height = 12,
  dpi = 300
)

# 20. Bar chart gap Nord-Sud per settore -----

cat("20. Gap Nord-Sud per settore...\n")

# Carica dati gap territoriale
gap_territoriale <- readRDS("output/gap_territoriale_settori_2022.rds")

# Prepara dati per grafico
gap_plot_data <- gap_territoriale %>%
  filter(!is.na(gap_nord_sud_pct)) %>%
  mutate(
    settore_short = ifelse(
      nchar(settore) > 40,
      paste0(substr(settore, 1, 37), "..."),
      settore
    ),
    settore_short = reorder(settore_short, gap_nord_sud_pct),
    tipo_gap = ifelse(gap_nord_sud_pct > 10, "Alto", "Basso/Medio")
  )

p20 <- gap_plot_data %>%
  ggplot(aes(
    x = settore_short,
    y = gap_nord_sud_pct,
    fill = gap_nord_sud_pct
  )) +
  geom_col(show.legend = FALSE) +
  geom_text(
    aes(
      label = sprintf("%.1f%%", gap_nord_sud_pct),
      hjust = ifelse(gap_nord_sud_pct >= 0, -0.1, 1.1)
    ),
    size = 3
  ) +
  coord_flip() +
  scale_fill_gradient2(
    low = "#06A77D",
    mid = "#F18F01",
    high = "#C73E1D",
    midpoint = median(gap_plot_data$gap_nord_sud_pct, na.rm = TRUE)
  ) +
  scale_y_continuous(
    labels = label_percent(scale = 1),
    expand = expansion(mult = c(0.05, 0.15))
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title = "Gap Salariale Nord-Sud per Settore NACE",
    subtitle = "Anno 2022 - Differenza % (Nord - Sud) / Sud",
    x = NULL,
    y = "Gap Nord-Sud (%)",
    caption = paste(
      "Nord = Nord-ovest + Nord-est; Sud = Sud + Isole",
      "\nValori positivi: salari più alti al Nord"
    )
  ) +
  theme_salari() +
  theme(axis.text.y = element_text(size = 8))

ggsave(
  "output/grafici/20_gap_nord_sud_settori.png",
  p20,
  width = 12,
  height = 10,
  dpi = 300
)

cat("\n==== Generazione grafici completata ====\n")
cat("Grafici salvati in: output/grafici/\n")
cat("Totale grafici generati: 20\n\n")

# Elenca grafici creati
grafici_creati <- list.files(
  "output/grafici",
  pattern = "\\.png$",
  full.names = FALSE
)
cat("Elenco grafici:\n")
for (g in grafici_creati) {
  cat(" -", g, "\n")
}
