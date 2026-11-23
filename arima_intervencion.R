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

# distribución mensual de ventas de psicofarmacos

par(mfrow = c(1,3))

ts.plot(datos %>% 
          group_by(mes, año) %>%
          summarise(DHD_total = sum(DHD_total), .groups = "drop") %>%
          pivot_wider(
            names_from = año,
            values_from = DHD_total,
            names_prefix = ""
          ) %>%
          select(-mes),
        main = "Total")

ts.plot(datos %>% 
          group_by(mes, año) %>%
          summarise(DHD_inst = sum(DHD_inst), .groups = "drop") %>%
          pivot_wider(
            names_from = año,
            values_from = DHD_inst,
            names_prefix = ""
          ) %>%
          select(-mes),
        main = "Institucional")

ts.plot(datos %>% 
          group_by(mes, año) %>%
          summarise(DHD_comm = sum(DHD_comm), .groups = "drop") %>%
          pivot_wider(
            names_from = año,
            values_from = DHD_comm,
            names_prefix = ""
          ) %>%
          select(-mes),
        main = "Comercial")

# parece que hay un componente de temporalidad de los datos

par(mfrow = c(1,1))

# serie de ventas de psicofarmacos entre 2017 y  2025

par(mfrow = c(1,3))

ts.plot(datos %>%
          group_by(indice) %>%
          summarise(DHD = sum(DHD_total)) %>% 
          select(DHD),
        main = "Total")

ts.plot(datos %>%
          group_by(indice) %>%
          summarise(DHD = sum(DHD_inst)) %>% 
          select(DHD),
        main = "Institucional")

ts.plot(datos %>%
          group_by(indice) %>%
          summarise(DHD = sum(DHD_comm)) %>% 
          select(DHD),
        main = "Comercial")

par(mfrow = c(1,1))

# al parece de manera agregada los DHDs se empezaron a comprar mas desde el 2023

# el script se divide en

principio = list()
meses = list()

lista_principio = names(table(datos$principioactivo))

for (i in 1:length(lista_principio)) {
  
  # serializacion de datos (sumando principio activo mensual)
  
  principio[[i]] = datos %>%
    filter(
      principioactivo == lista_principio[i]
    ) %>%
    mutate(indice = as.integer(indice)) %>%
    select(
      principioactivo, # agrupamiento
      indice,          # mes de la cohorte
      # variables de interes --------------
      DHD_total,       
      DHD_inst,
      DHD_comm,
      mes, 
      fecha,
    ) %>%
    group_by(indice) %>% 
    summarise(
      DHD_total = sum(DHD_total),
      DHD_inst = sum(DHD_inst),
      DHD_comm = sum(DHD_comm),
      mes = as.integer(first(mes)),
      medicamento = first(principioactivo),
      fecha = first(fecha)
    ) %>%
    as_tsibble(index = indice, regular = T) 
  
  # control de datos faltantes
  
  if(as.logical(has_gaps(principio[[i]]))){
    cat("Gaps detectados en ", i, "\n")
    
    principio[[i]] = principio[[i]] %>% fill_gaps()
    
    rep_tot = min(principio[[i]][,c(2)], na.rm = T)
    rep_ins = min(principio[[i]][,c(3)], na.rm = T)
    rep_com = min(principio[[i]][,c(4)], na.rm = T)
    
    # next
    principio[[i]][,c(2)] = imputeTS::na_replace(principio[[i]][,c(2)], rep_tot)
    principio[[i]][,c(3)] = imputeTS::na_replace(principio[[i]][,c(3)], rep_ins)
    principio[[i]][,c(4)] = imputeTS::na_replace(principio[[i]][,c(4)], rep_com)
    
  }
  
  
  layout(matrix(c(1,2,3), nrow = 3, byrow = TRUE))
  
  ts = as.ts(log(principio[[i]][,c(2)]))
  
  # control
  ts.plot(ts, main = paste(i, lista_principio[i]))
  acf(ts, main = "")
  pacf(ts, main = "")
}


layout(1)

names(principio) = lista_principio


t_reg = principio[[1]] %>%
  mutate(
    pandemia1 = as.logical(fecha > "2020-03-01" & fecha < "2020-10-01"), 
    pandemia2 = as.logical(fecha > "2020-09-01"), 
    desabastecimiento = as.logical(fecha > "2023-02-01" & fecha < "2023-12-01")
  ) %>%
  select(pandemia1, pandemia2, desabastecimiento, -indice) 

t_reg = as.matrix(
  c(
    as.numeric(t_reg$pandemia1),
    as.numeric(t_reg$pandemia2),
    as.numeric(t_reg$desabastecimiento)
  )
)

# comando auto arima

library(forecast)

arimax = list()

for(i in 1:length(lista_principio)){
  cat(lista_principio[i], "...")
  # 1. Extraer la serie y determinar su longitud
  serie_actual <- log(principio[[i]]$DHD_total)
  n_obs <- length(serie_actual)
  
  # 2. Recortar t_reg para que coincida con las observaciones de la serie
  # (Asumiendo que las primeras n_obs filas de t_reg corresponden al pasado)
  xreg_entrenamiento <- t_reg[1:n_obs, , drop = FALSE]
  
  # 3. Ajustar el modelo
  arimax[[i]] <- auto.arima(
    y = serie_actual,
    stepwise = FALSE, 
    xreg = xreg_entrenamiento, 
    approximation = FALSE, D = 12
  )
  
  cat(" completo\n")
}

names(arimax) = lista_principio

arimax
