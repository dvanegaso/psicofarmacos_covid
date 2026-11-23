
rm(list = ls())

library(tidyverse)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Principios activos (en código WHO)

principios_activos = c(
  "N05A",
  "N05B",
  "N05C",
  "N06A",
  "N03AE01"
)

# principios activos (en nombre comercial)

medicamentos_candidatos = data.frame(
  Name = c(
    
    # Antidepresivos
    "sertraline",
    "fluoxetine",
    "escitalopram",
    "trazodone", # no está
    "vortioxetine",
    "amitriptyline", 
    
    # Ansiolíticos benzodiazepínicos
    "alprazolam", 
    "lorazepam", 
    "diazepam", 
    "clobazam",
    
    # Hipnóticos y sedantes
    "zolpidem", 
    "zopiclone",
    
    # Antipsicóticos
    "quetiapine", 
    "risperidone", 
    "olanzapine",
    
    # control
    "losartan", # no está
    "statin",  # no está
    
    # sensibilidad
    "clonazepam"
  )
)


medicamentos_candidatos = dplyr::left_join(
  medicamentos_candidatos,
  read.csv("diccionarios/DDD.csv"),
  by = "Name") %>%
  select(Name, ATC_code) %>%
  group_by(Name) %>% 
  summarise(ATC_code = first(ATC_code))

write.csv(principios_activos, "bases/principios_activos.csv")
write.csv(medicamentos_candidatos, "bases/medicamentos_candidatos.csv")

rm(list = ls())
