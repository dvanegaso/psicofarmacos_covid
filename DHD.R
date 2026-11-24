# analisis de series temporales

rm(list = ls())

library(dplyr)
library(stringr)
library(tidyr)
library(tidytext)
library(readr)
library(tsibble)
library(lubridate)
library(zoo)

# v1.1: Imputación de datos faltantes.
# v1.0: Código probado y listo.
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

# distribución mensual de ventas de psicofarmacos

principio = list()
meses = list()

lista_principio = names(table(datos$principioactivo))

for (i in 1:length(lista_principio)) {
  
  # serializacion de datos (sumando principio activo mensual)
  
  princ_temp = datos %>%
    filter(
      principioactivo == lista_principio[i]
    ) %>%
    select(
      principioactivo, # agrupamiento
      indice,          # mes de la cohorte
      # variables de interes --------------
      mg_vendidos_total,       
      mg_vendidos_inst,
      mg_vendidos_comm,
      total_poblacional,
      n_dias,
      DDD,
      mes, 
      fecha,
    )  %>%
    mutate(indice = as.integer(indice)) %>%
    group_by(indice) %>% 
    summarise(
      DHDt = (sum(mg_vendidos_total)/ (first(DDD) * first(total_poblacional) * first(n_dias))) * 1000,
      DHDi = (sum(mg_vendidos_inst)/ (first(DDD) * first(total_poblacional) * first(n_dias))) * 1000,
      DHDc = (sum(mg_vendidos_comm)/ (first(DDD) * first(total_poblacional) * first(n_dias))) * 1000,
      mes = as.integer(first(mes)),
      fecha = first(fecha)
    ) %>%
    as_tsibble(index = indice, regular = T) %>%
    mutate(
      pandemia1 = as.logical(fecha > "2020-03-01" & fecha < "2020-10-01"), 
      pandemia2 = as.logical(fecha > "2020-09-01"), 
      desabastecimiento = as.logical(fecha > "2023-02-01" & fecha < "2023-12-01"),
    )
  
  # control de datos faltantes
  
  if(as.logical(has_gaps(princ_temp))){
    cat("Gaps detectados en ", i, "\n")
    
    princ_temp = princ_temp %>% fill_gaps()
  }
  
  principio[[i]] = princ_temp 
}

names(principio) = lista_principio

# imputacion

imputar_spline <- function(d, inicio = as.Date("2017-01-01")) {
  n <- nrow(d)
  d$imputado <- is.na(d$DHDi) | is.na(d$DHDc)
  
  # Completa fecha y mes desde el índice
  d$fecha <- seq(inicio, by = "month", length.out = n)
  d$mes   <- month(d$fecha)
  
  # Copia indicadoras desde la fila anterior
  for (v in c("pandemia1", "pandemia2", "desabastecimiento")) {
    d[[v]] <- na.locf(d[[v]], na.rm = FALSE)
  }
  
  # Spline en escala log para DHDi y DHDc
  for (v in c("DHDi", "DHDc")) {
    d[[v]] <- exp(na.spline(log(d[[v]]), x = seq_len(n), na.rm = FALSE))
  }
  
  # Recalcula DHDt solo donde falta
  d$DHDt <- ifelse(is.na(d$DHDt), d$DHDi + d$DHDc, d$DHDt)
  d
}

# diazepam: imputacion por nulidad

principio$diazepam <- imputar_spline(principio$diazepam)

principio$diazepam[92,2] = principio$diazepam[93,2]

# vortiozetine: imputacion por delecion

principio$vortioxetine <- imputar_spline(principio$vortioxetine)


save(principio, file = "Lista Principios DHD.RData")

rm(list = ls())
