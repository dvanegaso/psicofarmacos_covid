# analisis de series temporales

library(dplyr)
library(stringr)
library(tidyr)
library(tidytext)
library(readr)
library(tsibble)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

datos = read.csv("salidas/base_DHD_final.csv")

# distribución mensual de ventas de psicofarmacos

par(mfrow = c(1,3))

ts.plot(datos %>%
          group_by(mes) %>%
          summarise(DHD = sum(DHD_total)) %>% 
          select(DHD))

ts.plot(datos %>%
          group_by(mes) %>%
          summarise(DHD = sum(DHD_inst)) %>% 
          select(DHD))

ts.plot(datos %>%
          group_by(mes) %>%
          summarise(DHD = sum(DHD_comm)) %>% 
          select(DHD))

par(mfrow = c(1,1))

# serie de ventas de psicofarmacos entre 2017 y  2025

par(mfrow = c(1,3))

ts.plot(datos %>%
          group_by(mes_medicion) %>%
          summarise(DHD = sum(DHD_total)) %>% 
          select(DHD))

ts.plot(datos %>%
          group_by(mes_medicion) %>%
          summarise(DHD = sum(DHD_inst)) %>% 
          select(DHD))

ts.plot(datos %>%
          group_by(mes_medicion) %>%
          summarise(DHD = sum(DHD_comm)) %>% 
          select(DHD))

par(mfrow = c(1,1))

# el script se divide en

principio = list()
meses = list()

lista_principio = names(table(datos$principioactivo))


for (i in 1:length(lista_principio)) {
  
  principio[[i]] = datos %>%
    select(
      principioactivo,
      mes_medicion,
      DHD_total,
      DHD_inst,
      DHD_comm
      ) %>%
    filter(
      principioactivo == lista_principio[i]
      ) %>%
    mutate(mes_medicion = as.integer(mes_medicion)) %>%
    group_by(mes_medicion)%>% 
    summarise(
      DHD_total = sum(DHD_total),
      DHD_inst = sum(DHD_inst),
      DHD_comm = sum(DHD_comm)
      ) %>%
    as_tsibble(index = mes_medicion, regular = T)
  
  if(as.logical(has_gaps(principio[[i]]))){
    cat("Gaps detectados en ", i, "\n")
    
    principio[[i]] = principio[[i]] %>% fill_gaps()
  }

  meses[[i]] = summary(principio[[i]]$mes_medicion)
}
  
# [1] "diazepam"     "vortioxetine" con DATOS FALTANTES




# separacion de medicamentos 

# graficacion de medicamentos

# analisis de series temporales por principio activo

# ajuste modelos del documento https://docs.google.com/document/d/18w68zk-du5C-d3LMvON1PJL6f2dNMgfD/edit?pli=1