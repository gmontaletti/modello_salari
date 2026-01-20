#  tutti i dataset 
library(istatlab)
library(data.table)

tutti <- download_metadata()


codelists <- download_codelists(dataset_ids = c(
                                                "533_957"
                                                
))


#  racli salari orari ----
# 533_957_DF_DCSC_RACLI_1

# cicla su racli 
for (i in 1:25) {
  nome <- paste0("533_957_DF_DCSC_RACLI_", i)
  print(nome)
  dt <- download_istat_data(nome)
  nomefile <- paste0("racli/F_", nome, ".rds")
  saveRDS(dt, nomefile)
}
i = 12
for (i in 1:25) {
  nome <- paste0("533_957_DF_DCSC_RACLI_", i)
  nomefile <- paste0("racli/F_", nome, ".rds")
  dt <- readRDS(nomefile)
  dtl <- istatlab::apply_labels(dt)
  saveRDS(dtl, nomefile)
}


# dt <- download_istat_data("533_957_DF_DCSC_RACLI_1") # 
# saveRDS(dt, "data/racli.rds")


