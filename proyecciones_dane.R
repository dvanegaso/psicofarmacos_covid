# Lectura de bases de poblacion

# v 1.0: Guardado de proyecciones de población enrte 2017 y 2025
# v 0.3: Guardado de variables necesarias: falta 2017 (retroproyección DANE)
# v 0.2: Guardado de diccionario
# v 0.1: Limpieza de base 

# Base Serie nacional de población por área, para el periodo 2018-2070
# Consultado de https://www.dane.gov.co/index.php/estadisticas-por-tema/demografia-y-poblacion/proyecciones-de-poblacion
# A la fecha 25 - 10 - 2026, 15:56 Hora COL

library(tidyverse)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# lectura y limpieza
proyec_tmp = readxl::read_xlsx("bases/PPED-AreaSexoEdadNac-2018-2070.xlsx", sheet = 3)

colnames(proyec_tmp) = c(
  proyec_tmp[5,1:3],
  proyec_tmp[6,4:309]
  )

proyec_tmp = proyec_tmp[7:133,]

names(proyec_tmp) = proyec_tmp %>%
  names() %>%
  str_replace_all(" ", "_")

proyec_tmp = proyec_tmp %>% 
  select(año = AÑO,
         total_poblacional = Total,
         ÁREA_GEOGRÁFICA
         ) %>% 
  filter(ÁREA_GEOGRÁFICA == "Total") %>%
  filter(año < 2026)


# Consultado de https://www.dane.gov.co/index.php/estadisticas-por-tema/demografia-y-poblacion/proyecciones-de-poblacion
# A la fecha 29 - 10 - 2026, 14:33 Hora COL
retyec_tmp = readxl::read_xlsx("bases/PPED-AreaNac-1950-2017.xlsx", sheet = 1)

colnames(retyec_tmp) = c(
  retyec_tmp[2,1],
  retyec_tmp[6,2:5]
  )

names(retyec_tmp) = retyec_tmp %>%
  names() %>%
  str_replace_all(" ", "_")

retyec_tmp = retyec_tmp %>% 
  select(año = AÑO,
         total_poblacional = Población,
         ÁREA_GEOGRÁFICA
  ) %>% 
  filter(ÁREA_GEOGRÁFICA == "Total") %>%
  filter(año == 2017)

# guardado
proyec = bind_rows(retyec_tmp, proyec_tmp) %>% select(año, total_poblacional)
write.csv(x = proyec, file = "diccionarios/proyecciones_dane_2017_2025.csv")


