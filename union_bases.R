# para construir esta variable se necesita primero:

rm(list = ls())

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

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

medicamentos_candidatos = read.csv("bases/medicamentos_candidatos.csv")[,2]

BASE_SISMED_DDD_DANE = read.csv("salidas/base_sismed_final.csv") %>%
  select(cum, 
         atc, 
         grupoatc,
         atc5,
         principioactivo,
         concentracion,
         registrosanitario = registrosanitario.x,
         año,
         mes,
         n_dias,
         unidadescomercial,
         unidadesinstitucional,
         activoporumd,
         umdporunidad
         ) %>%
  mutate(
    activounidad = activoporumd * umdporunidad
  )

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

proyec_dane = read.csv("diccionarios/proyecciones_dane_mensual.csv") %>% 
  select(
    indice,
    fecha,
    total_poblacional
    )

BASE_SISMED_DDD_DANE$indice = (BASE_SISMED_DDD_DANE$año %% 2017) * 12 + BASE_SISMED_DDD_DANE$mes

BASE_SISMED_DDD_DANE = left_join(BASE_SISMED_DDD_DANE, proyec_dane, by = "indice")

# construccion DHD

BASE_SISMED_DDD_DANE = BASE_SISMED_DDD_DANE %>%
  replace_na( # limpieza de NA: se trata el no registro como 0
    list(unidadescomercial = 0, unidadesinstitucional = 0)
  ) %>%
  mutate(
    vendidos_total = unidadescomercial + unidadesinstitucional
  ) %>%
  mutate( # miligramos de principio activo
    mg_vendidos_total = vendidos_total * activounidad, 
    mg_vendidos_inst = unidadesinstitucional * activounidad,
    mg_vendidos_comm = unidadescomercial * activounidad
    )

write.csv(BASE_SISMED_DDD_DANE,"salidas/base_DHD_final.csv")

rm(list = ls())

