# para construir esta variable se necesita primero:

# v0.2: Compilación resistente. Verificar bases anteriores.
# v0.1: Ni compila, solo se separó del archivo original

#    datos                               | scripts previos     |
# ---------------------------------------|---------------------|
# 1. miligramos vendidos por medicamento | lectura_SISMED.R
# 2. población                           | proyecciones_dane.R
# 3. DDD                                 | scraper_ATC_DDD.R


library(dplyr)
library(stringr)
library(tidyr)
library(tidytext)
library(readr)

medicamentos_candidatos = read.csv("bases/medicamentos_candidatos.csv")[,2]
BASE_SISMED_DDD_DANE = read.csv("salidas/base_sismed_final.csv")

DDD = read.csv("diccionarios/DDD.csv", sep = ",", dec = ".") %>% select(ATC_code, Name, DDD, U)

DDD = DDD[DDD$U != "",] # eliminados los medicamentos sin unidades

DDD[which(DDD$U == "mmol"),]
DDD = DDD[(DDD$U != "mmol"),]

# corrección unidades
DDD$DDD[DDD$U == "g"] = DDD$DDD[DDD$U == "g"]*1000 # conversion de unidades
DDD$U = "mg"


# seleccion de activos de interes

colnames(DDD)[1] = "atc"
DDD = DDD %>% 
  filter(Name %in% medicamentos_candidatos) %>%
  group_by(Name) %>%
  summarise(
    atc = first(atc),
    Name = first(Name),
    DDD = first(DDD))

# bind SISMED con DDD
BASE_SISMED_DDD_DANE = left_join(BASE_SISMED_DDD_DANE, DDD, by = "atc")

# bind SISMED_DDD con PROYECCIONES DANE

proyec_dane = read.csv("diccionarios/proyecciones_dane_2017_2025.csv")

BASE_SISMED_DDD_DANE = left_join(BASE_SISMED_DDD_DANE, proyec_dane, by = "año")

BASE_SISMED_DDD_DANE$mes_medicion = (BASE_SISMED_DDD_DANE$año %% 2016) * 12

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
    DHD = ((mg_vendidos_total / DDD) / (total_poblacional * n_dias)) * 1000
  )

write.csv(BASE_SISMED_DDD_DANE,"salidas/base_DHD_final.csv")

