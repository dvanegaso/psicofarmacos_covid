
# Lectura y selección de bases de datos.

# v 0.3:

# Se seleccionan los datos de la cohorte.

# Metodos para mg de cada medicamentos en otras unidades:

# 1. Cruzar base de datos CUM con SISMED: 6 de 26 medicamentos encontrados (informacion errónea).
# 2. Base de datos ficha.csv: No se conoce la veracidad de algunas unidades.


library(dplyr)
library(stringr)
library(tidyr)
library(tidytext)
library(readr)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

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

SISMED = list() # base completa.csv
ficha = list()  # base ficha.csv

for (i in 1:5) {
  temp_ficha = read.csv(
    paste0("bases_SISMED/",principios_activos[i],"_2026-10-26/ficha.csv"),
    dec = ".",
    sep = ",")
  temp_base = read.csv(
    paste0("bases_SISMED/",principios_activos[i],"_2026-10-26/completa.csv"),
    dec = ".",
    sep = ",")
  
  # guardado en la lista -------------------------
  SISMED[[i]] = temp_base
  ficha[[i]] = temp_ficha
  rm(temp_base, temp_ficha)
}

# Base completa
SISMED = bind_rows(SISMED)
ficha = bind_rows(ficha)

reemplazo = list(
  'á' = 'a',
  'é' = 'e',
  'í' = 'i',
  'ó' = 'o',
  'ú' = 'u'
)

# Estandarización de nombres

names(SISMED) = SISMED %>% 
  names() %>%
  str_replace_all("\\.","") %>% 
  str_to_lower() %>% 
  str_replace_all(c('á' = 'a',
                    'é' = 'e',
                    'í' = 'i',
                    'ó' = 'o',
                    'ú' = 'u'))
names(ficha) = ficha %>% 
  names() %>%
  str_replace_all("\\_","") %>% 
  str_to_lower() %>% 
  str_replace_all(c('á' = 'a',
                    'é' = 'e',
                    'í' = 'i',
                    'ó' = 'o',
                    'ú' = 'u'))


# Fecha de expedición del medicamento

SISMED = SISMED %>% separate(fecha, into = c("año", "mes", "n_dias"), sep = "-")



# último día del mes correspondiente
SISMED$n_dias = lubridate::days_in_month(
  lubridate::make_date(SISMED$año, SISMED$mes, 1)
)

SISMED$año = as.numeric(SISMED$año)
SISMED$mes = as.numeric(SISMED$mes)

# selección de años específicos 

# 2017 a 2025

SISMED = SISMED %>% filter(año > 2017, año < 2026)


# Estandarizacón de registro sanitario

SISMED = SISMED %>% mutate(principioactivo = str_to_lower(SISMED$principioactivo))

# se procede a revisar cuales datos contienen mediciones para todos los meses
for (u in 1:length(medicamentos_candidatos$principioactivo)) {
  
  print(medicamentos_candidatos$principioactivo[u])
  
  tabla = table(
    SISMED$mes[SISMED$principioactivo == medicamentos_candidatos$principioactivo[u]],
    SISMED$año[SISMED$principioactivo == medicamentos_candidatos$principioactivo[u]]
  )
  
  print(tabla)
  print(sum(tabla == 0))
}

# selección de principios cantidatos
SISMED = SISMED %>% filter(principioactivo %in% medicamentos_candidatos$principioactivo)

# Estandarización de Registro Sanitario 

SISMED = SISMED %>% mutate(registrosanitario = str_remove_all(registrosanitario, "\\s"))
ficha = ficha %>% mutate(registrosanitario = str_remove_all(registrosanitario, "\\s"))

# limpieza manual
SISMED$concentracion[SISMED$concentracion == "0.001 g/mL"] = "1 mg/mL"
SISMED$concentracion[SISMED$concentracion == "0.0025 g/mL"] = "2.5 mg/mL"
SISMED$concentracion[SISMED$concentracion == "0.004 g/mL"] = "4 mg/mL"

# seleccion de unidades a verificar

SISMED_ml = SISMED %>% filter(str_detect(concentracion, "mg/mL"))
SISMED_gr = anti_join(SISMED, SISMED_ml)

# visualizar las tablas faltantes

resumen_ml = SISMED_ml %>% 
  group_by(registrosanitario) %>% 
  summarise(formafarmaceutica = first(formafarmaceutica),
            producto = first(producto),
            titular = first(titular),
            principioactivo = first(principioactivo)
  )
# View(resumen_ml)


# Cargado base CUM (revisar medicamentos sin mg)

CUM = read.csv("CÓDIGO_ÚNICO_DE_MEDICAMENTOS_VIGENTES_20261025.csv", sep = ",", dec = ",") %>% 
  select(registrosanitario, principioactivo, unidadmedida, cantidad)
CUM = CUM %>% mutate(registrosanitario = str_remove_all(registrosanitario, "\\s"))
# selección de principios cantidatos
CUM = CUM %>% mutate(principioactivo = str_to_lower(principioactivo))
CUM = CUM %>% group_by(registrosanitario)
CUM = CUM %>% filter(principioactivo %in% medicamentos_candidatos$principioactivo) %>%
  group_by(registrosanitario) %>%
  summarise(
    principioactivo = first(principioactivo),
    unidadmedida = first(unidadmedida),
    cantidad = first(cantidad),
    .groups = "drop"
  )

# union sismed cum
SISMED_CUM = left_join(SISMED_ml, CUM, by = "registrosanitario")
SISMED_CUM = SISMED_CUM[!is.na(SISMED_CUM$cantidad),]



# 


head(SISMED_CUM)
table(SISMED_CUM$cantidad)

# por simplicidad podríamos empezar a calcular el DHD con los elementos con nada mas gramos

# join de bases

DDD = read.csv("DDD.csv", sep = ",", dec = ".") %>% select(ATC_code, Name, DDD, U)

DDD = DDD[DDD$U != "",] # eliminados los medicamentos sin unidades

DDD[which(DDD$U == "mmol"),]
DDD = DDD[(DDD$U != "mmol"),]

# corrección unidades
DDD$DDD[DDD$U == "g"] = DDD$DDD[DDD$U == "g"]*1000 # conversion de unidades
DDD$U = "mg"


# seleccion de activos de interes

colnames(DDD)[1] = "atc"
DDD = DDD %>% 
  filter(Name %in% medicamentos_candidatos$principioactivo) %>%
  group_by(Name) %>%
  summarise(
    atc = first(atc),
    Name = first(Name),
    DDD = first(DDD))

# bind SISMED con DDD
BASE_SISMED_DDD = left_join(SISMED_gr, DDD, by = "atc")

# bind SISMED_DDD con PROYECCIONES DANE

proyec_dane = read.csv("proyecciones_dane_2018_2025.csv")

BASE_SISMED_DDD_DANE = left_join(BASE_SISMED_DDD, proyec_dane, by = "año")

# limpieza base

BASE_SISMED_DDD_DANE = BASE_SISMED_DDD_DANE %>%
  mutate(
    cantidad = parse_number(str_extract(concentracion,  "\\d+(\\.\\d+)?\\s*mg"))
    )%>%
  replace_na(list(unidadescomercial = 0, unidadesinstitucional = 0)
             ) %>%
  mutate(
    vendidos_total = unidadescomercial + unidadesinstitucional
  ) %>%
  mutate(
    mg_vendidos_total = vendidos_total * cantidad) %>%
  mutate(
    DHD = ((mg_vendidos_total / DDD) / (total_poblacional * n_dias)) * 1000000
  )



