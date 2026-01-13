#  tutti i dataset 
library(istatlab)
library(data.table)

tutti <- download_metadata()

# https://esploradati.istat.it/SDMXWS/rest/data/IT1,163_184_DF_DCCN_PILQ_1,1.0/Q...../ALL/?detail=full&startPeriod=2023-04-01&endPeriod=2025-09-30&dimensionAtObservation=TIME_PERIOD
#  dati del pil trimestrale

# https://esploradati.istat.it/SDMXWS/rest/data/IT1,155_358_DF_DCSC_RETRATECO1_7,1.0/A+M...../ALL/?detail=full&startPeriod=2024-11-01&endPeriod=2025-11-30&dimensionAtObservation=TIME_PERIOD
# retribuzione oraria contrattuale per dipendente
# https://esploradati.istat.it/SDMXWS/rest/data/IT1,155_374_DF_DCSC_RETRULAOROS_1_4,1.0/Q..../ALL/?detail=full&startPeriod=2023-04-01&endPeriod=2025-09-30&dimensionAtObservation=TIME_PERIOD
#  retribuzioni per ula

oecd <- "https://sdmx.oecd.org/public/rest/data/OECD.CTP.TPS,DSD_TAX_WAGES_COMP@DF_TW_COMP,2.1/.AV_TW..S_C0.AW100._Z.A?startPeriod=1995"

datiocse <- rsdmx::readSDMX(oecd)

  
datiocsedf <- as.data.frame(datiocse)

saveRDS(datiocsedf, "data/oecdtax.rds")


codelists <- download_codelists(dataset_ids = c("163_184", 
                                                "151_874",
                                                "155_358",
                                                "92_507",
                                                "168_756",
                                                "150_872",
                                                "92_506",
                                                "150_875",
                                                "533_957",
                                                "98_197"
))


#  racli salari orari ----

dt <- download_istat_data("533_957") # 
saveRDS(dt, "data/racli.rds")

# dati produttività ----
dt <- download_istat_data("98_197") # 
saveRDS(dt, "data/produttivita.rds")

# ore lavorate

dt <- download_istat_data("163_88") # 
saveRDS(dt, "data/ore_lavorate.rds")



# renv::install("gmontaletti/istatlab")
# Y
# remotes::install_github("gmontaletti/istatlab", force = T)
# renv::snapshot()

dt <- download_istat_data("x") # 
saveRDS(dt, "data/x.rds")



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
 

