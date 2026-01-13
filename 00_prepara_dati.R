#  tutti i dataset 

tutti <- download_metadata()

# https://esploradati.istat.it/SDMXWS/rest/data/IT1,163_184_DF_DCCN_PILQ_1,1.0/Q...../ALL/?detail=full&startPeriod=2023-04-01&endPeriod=2025-09-30&dimensionAtObservation=TIME_PERIOD
#  dati del pil trimestrale

# https://esploradati.istat.it/SDMXWS/rest/data/IT1,155_358_DF_DCSC_RETRATECO1_7,1.0/A+M...../ALL/?detail=full&startPeriod=2024-11-01&endPeriod=2025-11-30&dimensionAtObservation=TIME_PERIOD
# retribuzione oraria contrattuale per dipendente
# https://esploradati.istat.it/SDMXWS/rest/data/IT1,155_374_DF_DCSC_RETRULAOROS_1_4,1.0/Q..../ALL/?detail=full&startPeriod=2023-04-01&endPeriod=2025-09-30&dimensionAtObservation=TIME_PERIOD
#  retribuzioni per ula

# https://sdmx.oecd.org/public/rest/dataflow/OECD.CTP.TPS/DSD_TAX_WAGES_COMP@DF_TW_COMP/2.1?references=all
# tasse sui salari


# renv::install("gmontaletti/istatlab")
# Y
# remotes::install_github("gmontaletti/istatlab", force = T)
# renv::snapshot()
library(istatlab)

dt <- download_istat_data("x") # 
saveRDS(dt, "data/x.rds")

codelists <- download_codelists(dataset_ids = c("163_184", 
                                                "151_874",
                                                "155_358",
                                                "92_507",
                                                "168_756",
                                                "150_872",
                                                "92_506",
                                                "150_875"
                                                ))

dt <- download_istat_data("163_184") # dati del pil trimestrale
saveRDS(dt, "data/pil.rds")

dt <- download_istat_data("151_874") # 
saveRDS(dt, "data/disoccupazione.rds") # disoccupazione mensile

dt <- download_istat_data("155_358") # retribuzione oraria contrattuale per dipendente 
saveRDS(dt, "data/retr_oraria.rds") # 

dt <- download_istat_data("92_507") # ore lavorate CN 
saveRDS(dt, "data/ore_lav_cn.rds") # 

dt <- download_istat_data("168_756") # 
saveRDS(dt, "data/ipca.rds") # 168_756 Ipca - mensili a tassazione costante (base 2015)

dt <- download_istat_data("150_872") # 
saveRDS(dt, "data/tasso_occupazione.rds") # 150_872 tasso di occupazione

dt <- download_istat_data("92_506") # 
saveRDS(dt, "data/pil_componenti.rds") # componenti del PIL

dt <- download_istat_data("150_875") # 
saveRDS(dt, "data/occupati_stabili.rds") # occupati per tipo di contratto
 
disoc <- readRDS("data/disoccupazione.rds")

disoc <- istatlab::apply_labels(disoc)
toplot <- disoc[AGE == "Y15-64" & ADJUSTMENT == "Y"]

library(ggplot2)
ggplot(toplot) +
  aes(x = tempo, y = valore) +
  geom_line() +
  theme_minimal()


