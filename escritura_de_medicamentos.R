
# Principios activos

principios_activos = c(
  "N05A",
  "N05B",
  "N05C",
  "N06A",
  "N03AE01"
)

medicamentos_candidatos = data.frame(
  principioactivo = c(
    
    # Antidepresivos
    "sertraline",
    "fluoxetine",
    "escitalopram",
    "trazodone", # no está
    "vortioxetine",
    "amitriptiline", # no está
    
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

write.csv(principios_activos, "bases/principios_activos.csv")
write.csv(medicamentos_candidatos, "bases/medicamentos_candidatos.csv")
