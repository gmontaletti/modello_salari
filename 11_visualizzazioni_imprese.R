# 11_visualizzazioni_imprese.R -----
# Visualizzazioni grafici imprese con dipendenti
# NOTA: MHOUR_JV è un INDICE (base 2021=100), non valori assoluti
# Autore: Giampaolo Montaletti (ORCID: 0009-0002-5327-1122)
# Data: 2026-01-15

# 1. Setup ambiente -----
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(patchwork)
  library(scales)
  library(viridis)
})

cat("==== Visualizzazioni Imprese ====\n\n")

# Carica theme esistente
source("visualizzazioni_salari.R")

# Crea directory per grafici
if (!dir.exists("output/grafici")) {
  dir.create("output/grafici", recursive = TRUE)
}

# Carica dati
cat("Caricamento dati...\n")
totale_wide <- readRDS("output/totale_economia_0015_wide.rds")
evoluzione_totale <- readRDS("output/evoluzione_totale_economia.rds")
evoluzione_settori <- readRDS("output/evoluzione_settori_top5.rds")
confronto_dimensione <- readRDS("output/confronto_dimensione_aziendale.rds")
ranking_settori <- readRDS("output/ranking_settori_indice_2025Q3.rds")
volatilita_settori <- readRDS("output/volatilita_settori.rds")
decomp_crescita <- readRDS("output/decomposizione_crescita_settori.rds")
decomp_totale <- readRDS("output/decomposizione_crescita_totale.rds")
stat_settori <- readRDS("output/stat_descrittive_settori.rds")

cat("Dati caricati. Generazione grafici in corso...\n\n")

# 2. Grafico 21: Evoluzione totale economia -----

cat("1. Evoluzione indice ore totale economia (W_GE1 e W_GE10)...\n")

p21_data <- totale_wide %>%
  select(periodo, anno, trimestre, anno_trimestre, W_GE1, W_GE10) %>%
  pivot_longer(
    cols = c(W_GE1, W_GE10),
    names_to = "dimensione",
    values_to = "indice"
  )

p21 <- p21_data %>%
  ggplot(aes(x = anno_trimestre, y = indice, color = dimensione)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5, alpha = 0.6) +
  geom_hline(
    yintercept = 100,
    linetype = "dashed",
    color = "gray40",
    alpha = 0.7
  ) +
  geom_vline(
    xintercept = 2020.25,
    linetype = "dashed",
    color = "red",
    alpha = 0.5
  ) +
  annotate(
    "text",
    x = 2020.25,
    y = 120,
    label = "COVID-19",
    hjust = -0.1,
    color = "red",
    size = 3
  ) +
  annotate(
    "text",
    x = 2021,
    y = 100,
    label = "Base 2021",
    hjust = 0,
    vjust = -0.5,
    color = "gray40",
    size = 3
  ) +
  scale_color_manual(
    values = c("W_GE1" = "#2E86AB", "W_GE10" = "#A23B72"),
    labels = c("W_GE1" = "≥1 dipendente", "W_GE10" = "≥10 dipendenti")
  ) +
  scale_y_continuous(breaks = seq(70, 120, 10)) +
  labs(
    title = "Evoluzione Indice Ore Lavorate - Totale Economia",
    subtitle = "Italia 2015-2025 (indice base 2021=100, dati destagionalizzati)",
    x = "Anno",
    y = "Indice ore lavorate (2021=100)",
    color = "Dimensione aziendale",
    caption = "Fonte: ISTAT - MHOUR_JV_2021"
  ) +
  theme_salari()

ggsave(
  "output/grafici/21_evoluzione_indice_totale.png",
  p21,
  width = 10,
  height = 6,
  dpi = 300
)

# 3. Grafico 22: Evoluzione top 5 settori -----

cat("2. Evoluzione top 5 settori...\n")

p22 <- evoluzione_settori %>%
  ggplot(aes(
    x = anno + (trimestre - 1) / 4,
    y = ore_migliaia,
    color = settore_macro_label
  )) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.2, alpha = 0.6) +
  geom_hline(
    yintercept = 100,
    linetype = "dashed",
    color = "gray40",
    alpha = 0.5
  ) +
  scale_color_viridis_d(option = "plasma") +
  labs(
    title = "Evoluzione Indice Ore Top 5 Settori (2025-Q3)",
    subtitle = "Italia 2015-2025 (indice base 2021=100, W_GE1)",
    x = "Anno",
    y = "Indice ore lavorate (2021=100)",
    color = "Settore"
  ) +
  theme_salari() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 2))

ggsave(
  "output/grafici/22_evoluzione_top5_settori.png",
  p22,
  width = 10,
  height = 6,
  dpi = 300
)

# 4. Grafico 23: Crescita settoriale 2015-2025 -----

cat("3. Crescita settoriale 2015-2025...\n")

p23 <- decomp_crescita %>%
  mutate(
    settore_label = reorder(settore_macro_label, var_percentuale),
    crescita_tipo = ifelse(var_percentuale >= 0, "Positiva", "Negativa")
  ) %>%
  ggplot(aes(x = var_percentuale, y = settore_label, fill = crescita_tipo)) +
  geom_col() +
  geom_vline(xintercept = 0, linetype = "solid", color = "black") +
  scale_fill_manual(
    values = c("Positiva" = "#2E86AB", "Negativa" = "#A23B72")
  ) +
  labs(
    title = "Crescita Settoriale 2015-Q1 to 2025-Q3",
    subtitle = "Variazione percentuale indice ore lavorate (W_GE1)",
    x = "Variazione %",
    y = NULL,
    fill = "Tipo crescita"
  ) +
  theme_salari() +
  theme(axis.text.y = element_text(size = 8))

ggsave(
  "output/grafici/23_crescita_settoriale.png",
  p23,
  width = 10,
  height = 8,
  dpi = 300
)

# 5. Grafico 24: Confronto W_GE1 vs W_GE10 (gap percentuale) -----

cat("4. Gap percentuale W_GE10 vs W_GE1...\n")

p24 <- confronto_dimensione %>%
  ggplot(aes(x = anno_trimestre, y = diff_percentuale)) +
  geom_line(linewidth = 1, color = "#A23B72") +
  geom_point(size = 1.5, alpha = 0.6, color = "#A23B72") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  annotate(
    "text",
    x = 2017,
    y = 0,
    label = "W_GE10 = W_GE1",
    hjust = 0,
    vjust = -0.5,
    color = "gray40",
    size = 3
  ) +
  annotate(
    "rect",
    xmin = 2015,
    xmax = 2026,
    ymin = -Inf,
    ymax = 0,
    fill = "#A23B72",
    alpha = 0.1
  ) +
  annotate(
    "text",
    x = 2020,
    y = -2,
    label = "W_GE10 (≥10 dip) sottoinsieme di W_GE1",
    hjust = 0.5,
    color = "#A23B72",
    size = 3
  ) +
  labs(
    title = "Differenziale Indice Ore: W_GE10 vs W_GE1",
    subtitle = "Totale economia 2015-2025 (W_GE10 ≥10 dip è sottoinsieme di W_GE1 ≥1 dip)",
    x = "Anno",
    y = "Gap percentuale (%)",
    caption = "Nota: W_GE10 (imprese con 10 o più dipendenti) è sottoinsieme di W_GE1 (imprese con 1 o più dipendenti)"
  ) +
  theme_salari()

ggsave(
  "output/grafici/24_gap_W_GE10_W_GE1.png",
  p24,
  width = 10,
  height = 6,
  dpi = 300
)

# 6. Grafico 25: Ranking settori 2025-Q3 -----

cat("5. Ranking settori per indice (2025-Q3)...\n")

top_bottom <- bind_rows(
  ranking_settori %>% head(8) %>% mutate(gruppo = "Top 8"),
  ranking_settori %>% tail(8) %>% mutate(gruppo = "Bottom 8")
)

p25 <- top_bottom %>%
  mutate(settore_label = reorder(settore_macro_label, ore_migliaia)) %>%
  ggplot(aes(x = ore_migliaia, y = settore_label, fill = gruppo)) +
  geom_col() +
  geom_vline(xintercept = 100, linetype = "dashed", color = "gray40") +
  scale_fill_manual(values = c("Top 8" = "#2E86AB", "Bottom 8" = "#A23B72")) +
  labs(
    title = "Ranking Settori per Indice Ore (2025-Q3)",
    subtitle = "Top 8 e Bottom 8 settori (indice base 2021=100, W_GE1)",
    x = "Indice ore lavorate (2021=100)",
    y = NULL,
    fill = "Gruppo"
  ) +
  theme_salari() +
  theme(axis.text.y = element_text(size = 8))

ggsave(
  "output/grafici/25_ranking_settori_2025Q3.png",
  p25,
  width = 10,
  height = 8,
  dpi = 300
)

# 7. Grafico 26: Heatmap settori × tempo -----

cat("6. Heatmap settori × tempo...\n")

heatmap_data <- evoluzione_settori %>%
  mutate(anno_trim = paste0(anno, "-Q", trimestre))

# Seleziona solo alcuni periodi per leggibilità
periodi_selezionati <- heatmap_data %>%
  distinct(anno_trim, anno, trimestre) %>%
  filter(trimestre == 1 | (anno == 2025 & trimestre == 3)) %>%
  pull(anno_trim)

p26 <- heatmap_data %>%
  filter(anno_trim %in% periodi_selezionati) %>%
  ggplot(aes(x = anno_trim, y = settore_macro_label, fill = ore_migliaia)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "plasma") +
  labs(
    title = "Heatmap Indice Ore per Settore nel Tempo",
    subtitle = "Top 5 settori, Q1 di ogni anno (indice base 2021=100)",
    x = "Periodo",
    y = NULL,
    fill = "Indice"
  ) +
  theme_salari() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8)
  )

ggsave(
  "output/grafici/26_heatmap_settori_tempo.png",
  p26,
  width = 10,
  height = 6,
  dpi = 300
)

# 8. Grafico 27: Variazioni trimestrali e annuali -----

cat("7. Variazioni trimestrali e annuali totale economia...\n")

p27_data <- evoluzione_totale %>%
  select(periodo, anno, trimestre, var_trim_W_GE1, var_anno_W_GE1) %>%
  filter(!is.na(var_trim_W_GE1)) %>%
  pivot_longer(
    cols = c(var_trim_W_GE1, var_anno_W_GE1),
    names_to = "tipo_var",
    values_to = "variazione"
  ) %>%
  mutate(
    tipo_var_label = ifelse(
      tipo_var == "var_trim_W_GE1",
      "Trimestrale",
      "Annuale"
    )
  )

p27 <- p27_data %>%
  ggplot(aes(
    x = anno + (trimestre - 1) / 4,
    y = variazione,
    color = tipo_var_label
  )) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  geom_vline(
    xintercept = 2020.25,
    linetype = "dashed",
    color = "red",
    alpha = 0.5
  ) +
  scale_color_manual(
    values = c("Trimestrale" = "#2E86AB", "Annuale" = "#A23B72")
  ) +
  labs(
    title = "Variazioni Indice Ore - Totale Economia (W_GE1)",
    subtitle = "Italia 2015-2025 (variazioni % trimestrali e annuali)",
    x = "Anno",
    y = "Variazione %",
    color = "Tipo variazione"
  ) +
  theme_salari()

ggsave(
  "output/grafici/27_variazioni_totale.png",
  p27,
  width = 10,
  height = 6,
  dpi = 300
)

# 9. Grafico 28: Volatilità settoriale -----

cat("8. Volatilità settoriale...\n")

p28 <- volatilita_settori %>%
  mutate(settore_label = reorder(settore_macro_label, sd_var_trim)) %>%
  ggplot(aes(x = sd_var_trim, y = settore_label)) +
  geom_col(fill = "#2E86AB") +
  labs(
    title = "Volatilità Settoriale (Deviazione Standard Variazioni Trimestrali)",
    subtitle = "Top 5 settori 2015-2025 (W_GE1)",
    x = "SD variazioni % trimestrali",
    y = NULL
  ) +
  theme_salari()

ggsave(
  "output/grafici/28_volatilita_settori.png",
  p28,
  width = 10,
  height = 6,
  dpi = 300
)

# 10. Grafico 29: Scatter volatilità vs crescita -----

cat("9. Scatter volatilità vs crescita settoriale...\n")

scatter_data <- volatilita_settori %>%
  left_join(
    decomp_crescita %>% select(settore_macro, var_percentuale),
    by = "settore_macro"
  )

p29 <- scatter_data %>%
  ggplot(aes(x = var_percentuale, y = sd_var_trim, label = settore_macro)) +
  geom_point(size = 3, color = "#2E86AB", alpha = 0.6) +
  geom_text(hjust = 0, vjust = 0, nudge_x = 1, nudge_y = 0.1, size = 3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  labs(
    title = "Relazione Volatilità - Crescita Settoriale",
    subtitle = "Top 5 settori 2015-2025 (W_GE1)",
    x = "Crescita 2015-2025 (%)",
    y = "Volatilità (SD variazioni % trimestrali)"
  ) +
  theme_salari()

ggsave(
  "output/grafici/29_scatter_volatilita_crescita.png",
  p29,
  width = 10,
  height = 6,
  dpi = 300
)

# 11. Grafico 30: Box plot indici per settore -----

cat("10. Box plot distribuzione indici per settore...\n")

p30_data <- stat_settori %>%
  filter(dim_aziendale == "W_GE1") %>%
  mutate(settore_label = reorder(settore_macro_label, indice_medio))

p30 <- p30_data %>%
  ggplot(aes(y = settore_label)) +
  geom_segment(
    aes(x = indice_min, xend = indice_max, yend = settore_label),
    linewidth = 1,
    color = "#2E86AB"
  ) +
  geom_point(aes(x = indice_medio), size = 3, color = "#A23B72") +
  geom_vline(xintercept = 100, linetype = "dashed", color = "gray40") +
  labs(
    title = "Distribuzione Indice Ore per Settore (2015-2025)",
    subtitle = "Range min-max e media per settore (W_GE1)",
    x = "Indice ore lavorate (2021=100)",
    y = NULL
  ) +
  theme_salari() +
  theme(axis.text.y = element_text(size = 7))

ggsave(
  "output/grafici/30_boxplot_settori.png",
  p30,
  width = 10,
  height = 8,
  dpi = 300
)

# 12. Grafico 31: Decomposizione CAGR -----

cat("11. CAGR settoriale 2015-2025...\n")

p31 <- decomp_crescita %>%
  mutate(
    settore_label = reorder(settore_macro_label, cagr),
    cagr_tipo = ifelse(cagr >= 0, "Positivo", "Negativo")
  ) %>%
  ggplot(aes(x = cagr, y = settore_label, fill = cagr_tipo)) +
  geom_col() +
  geom_vline(xintercept = 0, linetype = "solid", color = "black") +
  scale_fill_manual(
    values = c("Positivo" = "#2E86AB", "Negativo" = "#A23B72")
  ) +
  labs(
    title = "CAGR Settoriale 2015-2025",
    subtitle = "Tasso di crescita annuale composto indice ore (W_GE1)",
    x = "CAGR (%)",
    y = NULL,
    fill = "Tipo"
  ) +
  theme_salari() +
  theme(axis.text.y = element_text(size = 8))

ggsave(
  "output/grafici/31_cagr_settoriale.png",
  p31,
  width = 10,
  height = 8,
  dpi = 300
)

# 13. Grafico 32: Shock COVID per settore -----

cat("12. Shock COVID-19 per settore...\n")

shock_covid <- evoluzione_settori %>%
  filter(anno %in% c(2019, 2020)) %>%
  group_by(settore_macro, settore_macro_label, anno) %>%
  summarise(
    indice_medio = mean(ore_migliaia, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = anno,
    values_from = indice_medio,
    names_prefix = "anno_"
  ) %>%
  mutate(
    shock = ((anno_2020 - anno_2019) / anno_2019) * 100,
    settore_label = reorder(settore_macro_label, shock)
  )

p32 <- shock_covid %>%
  ggplot(aes(x = shock, y = settore_label, fill = shock < 0)) +
  geom_col() +
  geom_vline(xintercept = 0, linetype = "solid", color = "black") +
  scale_fill_manual(
    values = c("TRUE" = "#A23B72", "FALSE" = "#2E86AB"),
    guide = "none"
  ) +
  labs(
    title = "Impatto COVID-19 per Settore (2019 vs 2020)",
    subtitle = "Variazione % media annuale indice ore (Top 5 settori, W_GE1)",
    x = "Variazione % 2019-2020",
    y = NULL
  ) +
  theme_salari()

ggsave(
  "output/grafici/32_shock_covid_settori.png",
  p32,
  width = 10,
  height = 6,
  dpi = 300
)

# 14. Grafico 33: Recupero post-COVID -----

cat("13. Recupero post-COVID...\n")

recupero_covid <- totale_wide %>%
  filter(anno %in% 2019:2023) %>%
  select(periodo, anno, trimestre, W_GE1) %>%
  mutate(
    baseline_2019 = W_GE1[anno == 2019 & trimestre == 4],
    recupero_pct = (W_GE1 - baseline_2019) / baseline_2019 * 100
  )

p33 <- recupero_covid %>%
  ggplot(aes(x = anno + (trimestre - 1) / 4, y = recupero_pct)) +
  geom_line(linewidth = 1, color = "#2E86AB") +
  geom_point(size = 1.5, alpha = 0.6, color = "#2E86AB") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  geom_vline(
    xintercept = 2019.75,
    linetype = "dashed",
    color = "red",
    alpha = 0.5
  ) +
  annotate(
    "text",
    x = 2019.75,
    y = 5,
    label = "Pre-COVID",
    hjust = -0.1,
    color = "red",
    size = 3
  ) +
  annotate(
    "rect",
    xmin = 2020,
    xmax = 2020.75,
    ymin = -Inf,
    ymax = Inf,
    fill = "red",
    alpha = 0.1
  ) +
  labs(
    title = "Recupero Post-COVID Totale Economia",
    subtitle = "Variazione % rispetto a 2019-Q4 (baseline pre-COVID, W_GE1)",
    x = "Anno",
    y = "Variazione % vs 2019-Q4"
  ) +
  theme_salari()

ggsave(
  "output/grafici/33_recupero_post_covid.png",
  p33,
  width = 10,
  height = 6,
  dpi = 300
)

# Riepilogo -----

cat("\n==== Generazione grafici completata ====\n")
cat("Grafici salvati in: output/grafici/\n")
cat("Totale grafici generati: 13\n\n")

cat("Elenco grafici:\n")
cat("21. Evoluzione indice totale economia (W_GE1 e W_GE10)\n")
cat("22. Evoluzione top 5 settori\n")
cat("23. Crescita settoriale 2015-2025\n")
cat("24. Gap percentuale W_GE10 vs W_GE1\n")
cat("25. Ranking settori 2025-Q3\n")
cat("26. Heatmap settori × tempo\n")
cat("27. Variazioni trimestrali e annuali totale economia\n")
cat("28. Volatilità settoriale\n")
cat("29. Scatter volatilità vs crescita\n")
cat("30. Box plot distribuzione indici per settore\n")
cat("31. CAGR settoriale\n")
cat("32. Shock COVID-19 per settore\n")
cat("33. Recupero post-COVID\n")
