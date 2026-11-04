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

# librerías de analisis de series de tiempo

# el siguiente codigo esta basado en el scripte Rcode.R de
#   https://github.com/gasparrini/2017_lopezbernal_IJE_codedata

library(foreign) ; library(tsModel) ; library("lmtest") ; library("Epi")
library("splines") ; library("vcd")

# el script se divide en

principio = list()
meses = list()

lista_principio = names(table(datos$principioactivo))

library(nlme)

for (i in 1:length(lista_principio)) {
  
  # serializacion de datos (sumando principio activo mensual)
  
  principio[[i]] = datos %>%
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
    filter(
      principioactivo == lista_principio[i]
      ) %>%
    mutate(indice = as.integer(indice)) %>%
    group_by(indice) %>% 
    summarise(
      DHD_total = sum(DHD_total),
      DHD_inst = sum(DHD_inst),
      DHD_comm = sum(DHD_comm),
      mes = as.integer(first(mes)),
      medicamento = first(principioactivo),
      fecha = first(fecha)
      ) %>%
    as_tsibble(index = indice, regular = T) %>%
    mutate(
      pandemia1 = as.logical(fecha > "2020-03-01" & fecha < "2020-10-01"), 
      pandemia2 = as.logical(fecha > "2020-09-01"), 
      desabastecimiento = as.logical(fecha > "2023-02-01" & fecha < "2023-12-01"),
              )
  
  # control de datos faltantes
  
  if(as.logical(has_gaps(principio[[i]]))){
    cat("Gaps detectados en ", i, "\n")
    
    principio[[i]] = principio[[i]] %>% fill_gaps()
  }
  
  # control
  meses[[i]] = summary(principio[[i]]$indice)
}


modelo = list()

# analisis de modelos

# Crear la carpeta si no existe
if (!dir.exists("graficos_iniciales")) {
  dir.create("graficos_iniciales", recursive = TRUE)
}

for (i in 1:length(lista_principio)) {
  
  if(i %in% c(4,12)){next} # corrección de base incompleta
  
  cat(lista_principio[i], "...")
  
  dat = principio[[i]]
  
  mes_cohorte = matrix(45, ncol = 1, nrow = 108)
  
  modelo_test = gls(
    model = log(DHD_total) ~
      
      # bloqueo 
      indice + 
      mes +
      
      # variables de interes
      pandemia1 + 
      pandemia2 + 
      desabastecimiento +
      I(pmax(indice - mes_cohorte, 0)) , 
    data = dat,
    method = "REML", 
    na.action = na.omit
  )
  
  png(
    paste0("graficos_iniciales/",lista_principio[i],".png"),
    width = 1000,
    height = 600,
    res = 120
    )
  par(mfrow = c(1,3))
  
  acf(residuals(modelo_test), main = "");
  pacf(residuals(modelo_test), main = lista_principio[i]); 
  plot(residuals(modelo_test), main = "")
  
  par(mfrow = c(1,1))
  dev.off()
  
  # print(summary(modelo_test))
  

  
  modelo[[i]] = modelo_test
  
  cat(" completo\n")
}

par(mfrow = c(1,1))


# separacion de medicamentos 

# graficacion de medicamentos

# analisis de series temporales por principio activo

# ajuste modelos del documento https://docs.google.com/document/d/18w68zk-du5C-d3LMvON1PJL6f2dNMgfD/edit?pli=1