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

load("Lista Principios DHD.RData")

lista_principio = read.csv("salidas/base_DHD_final.csv")
lista_principio = names(table(lista_principio$principioactivo))


series_init = list()

for (i in 1:length(lista_principio)) {
  
  cat(i, ":",lista_principio[i],":\n")
  
  cat(lista_principio[i], "...")
  
  dat = principio[[i]]
  
  modelo.tmp = lm(
    formula = log(DHDt) ~
      
      # bloqueo 
      indice + 
      factor(mes) +
      
      # variables de interes
      pandemia1 + 
      pandemia2 + 
      desabastecimiento +
      I(pmax(indice - 45, 0)), # mes 45: marzo de 2020
    data = dat,
    na.action = na.omit
  )
  
  tmp.res = residuals(modelo.tmp)
  
  # graficación
  layout(matrix(c(1, 1, 1, 1, 2, 3, 4,5), nrow = 2, byrow = TRUE))
  glmtoolbox::envelope(modelo.tmp)
  ts.plot(principio[[i]][,c(1:3)])
  acf(tmp.res, main = "");
  pacf(tmp.res, main = lista_principio[i]); 
  plot(tmp.res, main = "")
  #qqnorm(tmp.res)
  #qqline(tmp.res, col = "red")
  
  layout(1)
  
  cat("--------------------------------------------------------------\n",
      "Pruebas de Normalidad de Residuales:\n",
      "Shapiro:", shapiro.test(tmp.res)$p, "\n",
      "anderso:", nortest::ad.test(tmp.res)$p, "\n",
      "lillief:", nortest::lillie.test(tmp.res)$p, "\n",
      "JrqBera:", tseries::jarque.bera.test(tmp.res)[3]$p.value, "\n ",
      "------------------------------------------------------------\n",
      "Pruebas de Autocorrelación de Residuales:\n",
      "DurbinW:", car::durbinWatsonTest(tmp.res),"\n",
      "Breusch:", lmtest::bgtest(modelo.tmp)[4]$p.value,"\n",
      "Lyug-Bx:", Box.test(tmp.res, type = "Ljung-Box")[3]$p.value,"\n",
      "-------------------------------------------------------------\n",
      "Pruebas de Heterocedasticidad de Residuales:\n",
      "Bre-Pgn:", lmtest::bptest(modelo.tmp)$p.value, "\n",
      "White  :", skedastic::white(modelo.tmp)$p.value, "\n",
      "GoldQunt:", skedastic::goldfeld_quandt(modelo.tmp)$p.value, "\n",
      "-------------------------------------------------------------\n",
      "Momentos de los Residuales:\n",
      "Asimetría:", moments::skewness(tmp.res),"\n",
      "Kurtosis :", moments::kurtosis(tmp.res),"\n \n \n",
      "-------------------------------------------------------------\n"
  )
  
  # print(summary(modelo.tmp))
  
  modelo_init[[i]] = modelo.tmp
}

names(modelo_init) = lista_principio

layout(1)

############################################################
################ MODELAMIENTO NEWEY-WEST ###################
############################################################

library(sandwich)

