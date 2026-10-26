# Lectura de bases de poblacion

# v 0.1: Limpieza de base 

# Base Serie nacional de población por área, para el periodo 2018-2070
# Consultado de https://www.dane.gov.co/index.php/estadisticas-por-tema/demografia-y-poblacion/proyecciones-de-poblacion
# A la fecha 25 - 10 - 2026, 15:56 Hora COL

library(tidyverse)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

proyec_tmp = readxl::read_xlsx("PPED-AreaSexoEdadNac-2018-2070.xlsx", sheet = 3)

colnames(proyec_tmp) = c(proyec_tmp[5,1:3], proyec_tmp[6,4:309])

proyec_tmp = proyec_tmp[7:133,]

proyec = proyec_tmp[proyec_tmp$`ÁREA GEOGRÁFICA` == "Total",] 

rm(proyec_tmp)

proyec = proyec[!is.na(proyec$AÑO),]


