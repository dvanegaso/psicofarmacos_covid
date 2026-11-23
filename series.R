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

