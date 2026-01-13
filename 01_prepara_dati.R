#  tutti i dataset 
library(istatlab)
library(ggplot2)
library(lubridate)
library(tempdisagg)
library(tsbox)
library(data.table)

n_quarters <- (2025 - 1995 + 1) * 4
dates <- seq(
  as.Date(paste0(1995, "-01-01")),
  by = "quarter",
  length.out = n_quarters
)
dates
qdates <- yearqtr(dates)



#  tasso disoccupazione ----

unemploy <- function() {
tass_storici <- fread("data/tassistorici.csv")
disoc <- readRDS("data/disoccupazione.rds")
disoc <- istatlab::apply_labels(disoc)
tdisco <- disoc[AGE == "Y15-64" & ADJUSTMENT == "Y", .(tempo, u = valore)]
ggplot(tdisco) + aes(x = tempo, y = u) + geom_line()
min(tdisco$tempo)
stdisocc <- td(tass_storici$disoccupazione ~ 1, to = 12, conversion = "mean")
calendario <- seq.Date(from = as.Date(paste0(min(tass_storici$anno), "-01-01")), to = as.Date(paste0(max(tass_storici$anno),"-12-10")), by = "m")
storico <- data.table(tempo = calendario, u = predict(stdisocc))
disoc <- rbindlist(list(storico[tempo < min(tdisco$tempo)]
               , tdisco
               ))
tunemp <- disoc[, .(u = mean(u)), .(tempo = yearqtr(tempo))]
tunemp <- tunemp[tempo >= 1995.0]
return(tunemp)
}

tunemp <- unemploy()
ggplot(tunemp) + aes(tempo, u) + geom_line()



#  tasso di occupazione ------

employ <- function()  {
  tass_storici <- fread("data/tassistorici.csv")
  taoc <- fread("data/Dal I 2004 - età (IT1,150_915_DF_DCCV_TAXOCCU1_6,1.0).csv")
taoc <- taoc[, .(tempo = yq(TIME_PERIOD), taoc = Osservazione)]
str(taoc)

stocc <- td(tass_storici$taocc ~ 1, to = 4, conversion = "mean")
summary(stocc)
# plot(predict(stocc))
caldate <- min(taoc$tempo)
calendario <- seq.Date(from = as.Date(paste0(min(tass_storici$anno), "-01-01")), to = as.Date(paste0(max(tass_storici$anno),"-12-10")), by = "quarter")
storico <- data.table(tempo = calendario, oc = predict(stocc))
str(storico)
storico <- storico[tempo < caldate]

occupazione <- rbindlist(list(storico, taoc))
occupazione[, tempo := yearqtr(tempo)]
occupazione <- occupazione[tempo >= 1995.00]
return(occupazione)
}

occupazione <- employ()

ggplot(occupazione) +
  aes(x = tempo, y = oc) +
  geom_line() +
  theme_minimal()


stabshare <- function() {

  prec_base <- 7
  prec_path <- c(
    seq(7, 9, length.out = 5*4), # 1995-1999
    seq(9, 11, length.out = 4*4) # 2000-2009
  )
  prec_noise <- rnorm(length(prec_path), 0, 0.3)
  precarietà <- prec_path + prec_noise  
  qdates[1:length(precarietà)]
  precar <- data.table(tempo = qdates[1:length(precarietà)], instabili = precarietà)
  
  # plot(precarietà)
  
stabili <- readRDS("data/occupati_stabili.rds")
stabili <- istatlab::apply_labels(stabili)
tdet <- stabili[AGE == "Y15-89" & 
                    ADJUSTMENT == "Y" &
                    POSIZ_PROF == 1 &
                  PERM_TEMP_EMPLOYEES == 1
                    ]
ttutti <- stabili[AGE == "Y15-89" & 
                  ADJUSTMENT == "Y" &
                  POSIZ_PROF == 1 &
                  PERM_TEMP_EMPLOYEES == 9
                  ]

perct <- merge(tdet[, .(tempo, tempo_det = valore)], ttutti[, .(tempo, valore)])

perct <- perct[, .(instabili = mean(tempo_det/valore)*100), .(tempo = yearqtr(tempo))]
perct <- rbindlist(list(precar, perct))
  return(perct)
}


perct <- stabshare()


ggplot(perct, aes(tempo, instabili)) + geom_line()

prezzi <- function() {
  
  stipca <- fread("data/ipca_storico.csv")
  
  ipca <- readRDS("data/ipca.rds")
  ipca <- apply_labels(ipca)
  toplot <- ipca[E_COICOP_REV_ISTAT == "00" & MEASURE == 4 , .(tempo, valore)]
  toplot <- toplot[, .(valore = mean(valore)), .(tempo = yearqtr(tempo))]
  annuali <- toplot[, mean(valore), round(tempo, 0)]
  raccordo <- last(stipca$Indici)/annuali[round == 2015, V1]
  stipca[, nindici := Indici/raccordo]
  i2000 <- data.table(tempo = qdates[qdates >= 2000 & qdates < 2001], valore = rep(stipca[ANNI == 2000, nindici], 4))
  i2001 <- data.table(tempo = qdates[qdates >= 2001 & qdates < 2002], valore = rep(stipca[ANNI == 2001, nindici], 4))
  toplot <- rbindlist(list(
    i2000, i2001, toplot
  ))
  
  return(toplot)
}


p1 <- prezzi()

ggplot(p1) +
  aes(x = tempo, y = valore) +
  geom_line() +
  theme_minimal()




wages <- function() {
  w_base <- 70 # Indice 1995
  w_growth_rate <- c(
    rep(0.012 / 4, 4*10) # 1995-2009: crescita moderata
  )
  w_noise <- rnorm(n_quarters, 0, 0.003)
  w_index <- numeric(n_quarters)
  w_index[1] <- w_base
  for (i in 2:n_quarters) {
    w_index[i] <- w_index[i - 1] * (1 + w_growth_rate[i] + w_noise[i])
  }
  
  suimul <- data.table(tempo = qdates, valore = w_index)
 suimul <- suimul[!is.na(valore)]
  
  
  retr_ora <- readRDS("data/retr_oraria.rds")
  retr_ora <- apply_labels(retr_ora)
  
  toplot <- retr_ora[ECON_ACTIVITY_NACE_2007 == "0010" &
                       PROF_STATUS_EMP == "10" &
                       FREQ == "M" &
                       DATA_TYPE == "WAGE_H_2021"
                     , .(tempo, valore)
  ]
  
toplot<- toplot[, .(valore = mean(valore)), .(tempo = yearqtr(tempo))]
toplot <- rbindlist(list(suimul, toplot))
}

w <- wages()

# log(w$valore)

ggplot(w) +
  aes(x = tempo, y = log(valore)) +
  geom_line() +
  theme_minimal()


dgp <- function() {
  pil <- readRDS("data/pil.rds")
  pil <- apply_labels(pil)
  toplot <- pil[ADJUSTMENT == "Y" &
                  VALUATION == "L_2020" &
                  DATA_TYPE_AGGR == "B1GQ_B_W2_S1"
                , .(tempo, valore)
  ]
  return(toplot)
}

pil1 <- dgp()


ggplot(pil1) +
  aes(x = tempo, y = valore) +
  geom_line() +
  theme_minimal()

investi <- function() {
  investimenti <- fread("data/Investimenti fissi lordi per tipo di investimento (IT1,163_1226_DF_DCCN_QNA1_2,1.0).csv")
  invest <- investimenti[ADJUSTMENT == "Y" &
                           DATA_TYPE_AGGR == "P51G_D_W0_S1" &
                           NONFIN_ASSETS == "N11"
                         
                         , .(tempo = TIME_PERIOD, ifl = Osservazione)]
  invest[, tempo := yearqtr(yq(tempo))]
}

ifl <- investi()

produttivita <- function() {
  ore_lavorate <- fread("data/Occupazione per branca di attività (IT1,163_88_DF_DCCN_OCCQSEC2010_1,1.0).csv")
  ore_lavorate <- ore_lavorate[DATA_TYPE_AGGR == "HW", .(tempo = TIME_PERIOD, ore = Osservazione)]
  prod_il <- fread("data/Prodotto interno lordo e principali componenti (IT1,163_184_DF_DCCN_PILQ_1,1.0).csv")
  prod <- prod_il[Territorio == "B1G_B_W2_S1" , .(tempo = Edizione, pil = as.numeric(TIME_PERIOD))]
  produttivita <- merge.data.table(prod, ore_lavorate, by = "tempo", all = T)
  produttivita[, produtt := pil/ore*1000]
  produttivita[, tempo := yearqtr(yq(tempo))]
  
}

prod1 <- produttivita()

ggplot(prod1) +
  aes(x = tempo, y= produtt, group = 1) +
  geom_line()

#  tasse ----

cuneo <- readRDS("data/oecdtax.rds") |> setDT()
cuneo <- cuneo[REF_AREA == "ITA", .(tempo = obsTime, valore = obsValue)]
qcuneo <- td(cuneo$valore ~ 0, to = 4, conversion = "mean")
predict(qcuneo) |> plot()
cuneo <- data.table(tempo = qdates[qdates >= 2000], valore = c(predict(qcuneo), rep(last(cuneo$valore), 4)))


# compila data.frame ----


dates <- dates[dates>="2000-01-01"]

data <- data.frame(
  date = dates[dates>="2000-01-01"], # length(date)
  trimestri = qdates[qdates >= 2000], # length(trimestri)
  year = as.numeric(format(dates, "%Y")), # length(year)
  quarter = as.numeric(format(dates, "%m")) / 3, # length(quarter)
  
  # Variabili principali (livelli)
  w_nom = w[tempo >= 2000, valore], # Salari nominali (indice) # length(w_nom)
  p = p1[tempo >= 2000, valore], # Prezzi al consumo (indice) # length(p)
  prod = c(prod1[tempo >= 2000, produtt], last(prod1$produtt)), # Produttività (€/ora) # # length(prod)
  u = tunemp[tempo >= 2000, u] , # Tasso disoccupazione (%) # length(u)
  
  # Variabili aggiuntive
  occ = c(occupazione[tempo >= 2000, oc], last(occupazione$oc)), # Tasso occupazione (%) # length(occ)
  cuneo = cuneo[tempo >= 2000, valore], # Cuneo fiscale (%) # length(cuneo)
  prec = perct[tempo >= 2000, instabili], # Quota tempo determinato (%) length(prec)
  inv = c(ifl[tempo >= 2000, ifl], last(ifl$ifl)), # Investimenti (mld €) length(inv)
  pil = c(pil1[tempo >= "2000-01-01", valore], rep(last(pil1$valore), 2)) # PIL reale (mld €) length(pil)
)


saveRDS(data, "data/dati_istat.rds")
