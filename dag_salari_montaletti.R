# =============================================================================
# DAG delle Determinanti dei Salari secondo i testi di Giampaolo Montaletti
# =============================================================================
# Autore: Analisi dei testi pubblicati su Il Sussidiario (2009-2025)
# Data: Dicembre 2025
# =============================================================================
 
# Caricamento librerie necessarie
library(dagitty)
library(ggdag)
library(ggplot2)

# -----------------------------------------------------------------------------
# DEFINIZIONE DEL DAG
# -----------------------------------------------------------------------------
# Il DAG rappresenta le relazioni causali tra le determinanti dei salari
# come emergono dall'analisi dei testi di Montaletti.
#
# Le variabili principali identificate sono:
# - W: Salari reali
# - PROD: Produttivita del lavoro
# - INV: Investimenti e innovazione
# - INFL: Inflazione
# - CONTR: Contrattazione collettiva (ritardi rinnovi)
# - CUNEO: Cuneo fiscale
# - DOM_INT: Domanda interna
# - DOM_EST: Domanda estera/esportazioni
# - PREC: Precarietà (contratti a termine)
# - ISTR: Istruzione e capitale umano
# - PART: Partecipazione al lavoro (tasso occupazione)
# - DEMO: Fattori demografici (invecchiamento)
# - IRREG: Lavoro irregolare
# - SETT: Struttura settoriale (servizi vs industria)
# - POL_ATT: Politiche attive del lavoro
# - DISUG: Disuguaglianze (territoriali, genere, età)
# -----------------------------------------------------------------------------

dag_montaletti <- dagitty('
dag {
  # Variabile outcome principale
  W [outcome, pos="0,0"]
  
  # Variabili esogene (determinanti primarie)
  DEMO [exposure, pos="-4,2"]
  ISTR [exposure, pos="-2,2"]
  INV [exposure, pos="0,2"]
  INFL [exposure, pos="2,2"]
  DOM_EST [exposure, pos="4,2"]
  
  # Variabili intermedie (primo livello)
  PROD [pos="-1,1"]
  CONTR [pos="1,1"]
  PART [pos="-3,1"]
  SETT [pos="3,1"]
  
  # Variabili intermedie (secondo livello)
  PREC [pos="-2,0.5"]
  CUNEO [pos="2,0.5"]
  DOM_INT [pos="0,-1"]
  IRREG [pos="-4,0"]
  POL_ATT [pos="4,0"]
  
  # Variabile conseguenza
  DISUG [pos="0,-2"]
  
  # -----------------------------------------------------------------------------
  # RELAZIONI CAUSALI
  # -----------------------------------------------------------------------------
  
  # Produttività -> Salari (relazione centrale in Montaletti)
  PROD -> W
  
  # Investimenti e innovazione -> Produttività -> Salari
  INV -> PROD
  
  # Istruzione/Capitale umano -> Produttività
  ISTR -> PROD
  
  # Istruzione -> Salari (effetto diretto: premio laurea)
  ISTR -> W
  
  # Contrattazione -> Salari (ritardi nei rinnovi deprimono i salari)
  CONTR -> W
  
  # Inflazione -> Salari (erode potere acquisto)
  INFL -> W
  
  # Inflazione -> Contrattazione (spinge ai rinnovi)
  INFL -> CONTR
  
  # Cuneo fiscale -> Salari (riduzione aumenta netto)
  CUNEO -> W
  
  # Precarietà -> Salari (contratti a termine pagano meno)
  PREC -> W
  
  # Struttura settoriale -> Salari (settori a basso VA = bassi salari)
  SETT -> W
  
  # Struttura settoriale -> Precarietà
  SETT -> PREC
  
  # Domanda estera -> Struttura settoriale
  DOM_EST -> SETT
  
  # Domanda estera -> Salari (competizione al ribasso)
  DOM_EST -> W
  
  # Partecipazione -> Salari (più offerta può comprimere salari)
  PART -> W
  
  # Partecipazione -> Produttività
  PART -> PROD
  
  # Demografia -> Partecipazione
  DEMO -> PART
  
  # Politiche attive -> Partecipazione
  POL_ATT -> PART
  
  # Politiche attive -> Istruzione/Formazione
  POL_ATT -> ISTR
  
  # Salari -> Domanda interna (circolo vizioso)
  W -> DOM_INT
  
  # Domanda interna -> Investimenti
  DOM_INT -> INV
  
  # Lavoro irregolare -> Salari (comprime salari regolari)
  IRREG -> W
  
  # Istruzione -> Lavoro irregolare (bassa istruzione = più irregolare)
  ISTR -> IRREG
  
  # Salari -> Disuguaglianze
  W -> DISUG
  
  # Precarietà -> Disuguaglianze
  PREC -> DISUG
  
  # Struttura settoriale -> Produttività
  SETT -> PROD
  
  # Investimenti -> Struttura settoriale
  INV -> SETT
}
')

# -----------------------------------------------------------------------------
# VISUALIZZAZIONE DEL DAG
# -----------------------------------------------------------------------------

# Conversione in formato ggdag
dag_tidy <- tidy_dagitty(dag_montaletti)

# Etichette italiane per le variabili
etichette <- c(
  "W" = "Salari\nReali",
  "PROD" = "Produttività",
  "INV" = "Investimenti\ne Innovazione",
  "INFL" = "Inflazione",
  "CONTR" = "Contrattazione\nCollettiva",
  "CUNEO" = "Cuneo\nFiscale",
  "DOM_INT" = "Domanda\nInterna",
  "DOM_EST" = "Domanda\nEstera",
  "PREC" = "Precarietà\n(Tempo Det.)",
  "ISTR" = "Istruzione\ne Formazione",
  "PART" = "Tasso\nOccupazione",
  "DEMO" = "Demografia\n(Invecch.)",
  "IRREG" = "Lavoro\nIrregolare",
  "SETT" = "Struttura\nSettoriale",
  "POL_ATT" = "Politiche\nAttive",
  "DISUG" = "Disuguaglianze"
)

# Aggiunta etichette al dataset
dag_tidy$data <- dag_tidy$data %>%
  mutate(label_it = etichette[name])

# -----------------------------------------------------------------------------
# GRAFICO PRINCIPALE
# -----------------------------------------------------------------------------

# Colori per tipo di variabile
colori_nodi <- c(
  "W" = "#E63946",           # Rosso - outcome
  "PROD" = "#457B9D",        # Blu - variabile chiave
  "INV" = "#2A9D8F",         # Verde acqua
  "INFL" = "#E9C46A",        # Giallo
  "CONTR" = "#F4A261",       # Arancione
  "CUNEO" = "#9B5DE5",       # Viola
  "DOM_INT" = "#00B4D8",     # Azzurro
  "DOM_EST" = "#90BE6D",     # Verde
  "PREC" = "#F72585",        # Rosa
  "ISTR" = "#4361EE",        # Blu elettrico
  "PART" = "#7209B7",        # Viola scuro
  "DEMO" = "#3A0CA3",        # Indaco
  "IRREG" = "#560BAD",       # Viola intenso
  "SETT" = "#480CA8",        # Viola scuro
  "POL_ATT" = "#3F37C9",     # Blu viola
  "DISUG" = "#B5179E"        # Magenta
)

# Grafico con ggdag
p_dag <- ggplot(dag_tidy, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges(
    edge_width = 0.5,
    edge_colour = "grey40",
    arrow_directed = grid::arrow(length = grid::unit(6, "pt"), type = "closed")
  ) +
  geom_dag_point(aes(color = name), size = 18, alpha = 0.9) +
  geom_dag_text(aes(label = label_it), color = "white", size = 2.5, 
                fontface = "bold", lineheight = 0.9) +
  scale_color_manual(values = colori_nodi) +
  theme_dag() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10, color = "grey40"),
    plot.caption = element_text(hjust = 1, size = 8, color = "grey50")
  ) +
  labs(
    title = "Determinanti dei Salari nelle Economie Occidentali",
    subtitle = "DAG basato sull'analisi dei testi di Giampaolo Montaletti (2009-2025)",
    caption = "Fonte: Articoli pubblicati su Il Sussidiario | Elaborazione: Dicembre 2025"
  )

# Salvataggio grafico
ggsave("/home/claude/dag_salari_montaletti.png", p_dag, 
       width = 14, height = 10, dpi = 300, bg = "white")

# -----------------------------------------------------------------------------
# DAG SEMPLIFICATO (versione ridotta)
# -----------------------------------------------------------------------------

dag_semplificato <- dagitty('
dag {
  W [outcome, pos="0,0"]
  PROD [pos="-1.5,1"]
  INV [pos="-1.5,2"]
  ISTR [pos="0,2"]
  CONTR [pos="1.5,1"]
  INFL [pos="1.5,2"]
  PREC [pos="-1,0.5"]
  DOM_INT [pos="0,-1"]
  
  INV -> PROD
  ISTR -> PROD
  PROD -> W
  CONTR -> W
  INFL -> W
  INFL -> CONTR
  PREC -> W
  W -> DOM_INT
  DOM_INT -> INV
}
')

dag_semplificato_tidy <- tidy_dagitty(dag_semplificato)

etichette_simp <- c(
  "W" = "Salari",
  "PROD" = "Produttività",
  "INV" = "Investimenti",
  "ISTR" = "Istruzione",
  "CONTR" = "Contrattazione",
  "INFL" = "Inflazione",
  "PREC" = "Precarietà",
  "DOM_INT" = "Domanda Int."
)

dag_semplificato_tidy$data <- dag_semplificato_tidy$data %>%
  mutate(label_it = etichette_simp[name])

p_dag_simp <- ggplot(dag_semplificato_tidy, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges(
    edge_width = 0.6,
    edge_colour = "grey30",
    arrow_directed = grid::arrow(length = grid::unit(8, "pt"), type = "closed")
  ) +
  geom_dag_point(aes(color = name), size = 22, alpha = 0.9) +
  geom_dag_text(aes(label = label_it), color = "white", size = 3.2, fontface = "bold") +
  scale_color_manual(values = colori_nodi) +
  theme_dag() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10, color = "grey40")
  ) +
  labs(
    title = "Determinanti dei Salari: Schema Semplificato",
    subtitle = "Il circolo vizioso salari-domanda-investimenti-produttività"
  )

ggsave("/home/claude/dag_salari_semplificato.png", p_dag_simp, 
       width = 10, height = 8, dpi = 300, bg = "white")

# -----------------------------------------------------------------------------
# ANALISI DEL DAG
# -----------------------------------------------------------------------------

cat("\n========== ANALISI DEL DAG ==========\n\n")

# Percorsi causali verso i salari
cat("Percorsi causali diretti verso i Salari (W):\n")
print(paths(dag_montaletti, from = exposures(dag_montaletti), to = "W"))

cat("\n\nVariabili che influenzano i Salari:\n")
print(ancestors(dag_montaletti, "W"))

cat("\n\nVariabili influenzate dai Salari:\n")
print(descendants(dag_montaletti, "W"))

# Adjustment sets per stimare effetto produttività sui salari
cat("\n\nAdjustment set per stimare PROD -> W:\n")
print(adjustmentSets(dag_montaletti, exposure = "PROD", outcome = "W"))

# Adjustment sets per stimare effetto istruzione sui salari
cat("\n\nAdjustment set per stimare ISTR -> W:\n")
print(adjustmentSets(dag_montaletti, exposure = "ISTR", outcome = "W"))

cat("\n========================================\n")
cat("Grafici salvati in:\n")
cat("- /home/claude/dag_salari_montaletti.png (completo)\n")
cat("- /home/claude/dag_salari_semplificato.png (semplificato)\n")
cat("========================================\n")

