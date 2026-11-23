# analisis de series temporales

rm(list = ls())

library(dplyr)
library(stringr)
library(tidyr)
library(tidytext)
library(readr)
library(tsibble)

#
# v0.3: adicion de indicadoras para analiss
# v0.2: verificacion de la base con otros scripts
# v0.1: cargado y graficacion

# para 

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

datos = read.csv("salidas/base_DHD_final.csv")

# indicadoras necesarias  |  intervalo temporal
# ----------------------- # ------------------------------------------
# pandemia1:              |  confinamiento de marzo a septiembre
# pandemia2:              |  pendientes posteriores a septiembre 2020
# desabastecimiento:      |  no identificado, no incorporado

datos$fecha = as.Date(datos$fecha)

principio = list()
meses = list()

lista_principio = names(table(datos$principioactivo))

for (i in 1:length(lista_principio)) {
  
  # serializacion de datos (sumando principio activo mensual)
  
  datos_princ = datos %>%
    filter(
      principioactivo == lista_principio[i]
    ) %>%
    mutate(indice = as.integer(indice)) %>%
    select(
      principioactivo, # agrupamiento
      indice,          # mes de la cohorte
      # variables de interes --------------
      mg_vendidos_total,       
      mg_vendidos_inst,
      mg_vendidos_comm,
      mes, 
      fecha,
      DDD,
      total_poblacional,
      n_dias
    ) %>%
    group_by(indice) %>% 
    summarise(
      mg_vendidos_total = sum(mg_vendidos_total),
      mg_vendidos_inst = sum(mg_vendidos_inst),
      mg_vendidos_comm = sum(mg_vendidos_comm),
      mes = as.integer(first(mes)),
      medicamento = first(principioactivo),
      fecha = first(fecha),
      DDD = first(DDD),
      total_poblacional = first(total_poblacional),
      n_dias = first(n_dias)
    ) %>%
    as_tsibble(index = indice, regular = T) %>%
    mutate(
      pandemia1 = as.logical(fecha > "2020-03-01" & fecha < "2020-10-01"), 
      pandemia2 = as.logical(fecha > "2020-09-01"), 
      desabastecimiento = as.logical(fecha > "2023-02-01" & fecha < "2023-12-01"),
    )
  
  # CRUCIAL: Construcción de DHD
  datos_princ$DHDt = ((datos_princ$mg_vendidos_total / datos_princ$DDD) / (datos_princ$total_poblacional * datos_princ$n_dias)) * 1000
  datos_princ$DHDi = ((datos_princ$mg_vendidos_inst / datos_princ$DDD) / (datos_princ$total_poblacional * datos_princ$n_dias)) * 1000
  datos_princ$DHDc = ((datos_princ$mg_vendidos_comm / datos_princ$DDD) / (datos_princ$total_poblacional * datos_princ$n_dias)) * 1000
  
  # advertencia de gaps
  
  if(as.logical(has_gaps(datos_princ))){
    cat("ATENCION: Datos faltantes detectados en ", i,":",lista_principio[i], "\n")
    
    datos_princ = datos_princ %>% fill_gaps()
  }
  
  
  # selección de variables
  principio[[i]] = datos_princ %>% 
    select(
      DHDt,
      DHDi,
      DHDc,
      mes,
      medicamento, 
      fecha,
      indice,
      pandemia1,
      pandemia2,
      desabastecimiento
    )
  
  rm(datos_princ)
}

names(principio) = lista_principio

save(principio, file = "Lista Principios DHD.RData")

rm(list = ls())