# Interpolacion mensual de la poblacion total (DANE)

rm(list = ls())

# v 1.2: Salida de enero 2017 a diciembre 2025, indexada del mes 1 al 108
# v 1.1: Lectura directa de proyecciones y retroproyecciones (incluye 2017)
# v 1.0: Interpolacion mensual anclada al mes central del censo 2018

# uso de chat: https://claude.ai/share/47566b18-7fce-4238-96de-ecc2a4ce0aee
# bajo el manejo de claude sonnet 5.5

# v 0.4: Guardado de proyecciones de población enrte 2017 y 2025
# v 0.3: Guardado de variables necesarias: falta 2017 (retroproyección DANE)
# v 0.2: Guardado de diccionario
# v 0.1: Limpieza de base 


# Insumos (los mismos de proyecciones_dane.R):
#   bases/PPED-AreaSexoEdadNac-2018-2070.xlsx  (proyecciones, desde 2018)
#   bases/PPED-AreaNac-1950-2017.xlsx          (retroproyecciones, hasta 2017)
# Salida: diccionarios/proyecciones_dane_mensual.csv

# Periodo de recoleccion del CNPV 2018 (DANE, Ficha metodologica DSO-CNPV-FME-01 v2):
#   eCenso:          10 de enero  - 12 de abril de 2018
#   Operativo campo: 18 de abril  - 30 de octubre de 2018
# El censo cubre enero-octubre. Su mes central es junio.
# Cada total anual se ancla al mismo mes (junio), sin sesgo hacia una fase del censo.

library(tidyverse)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# parametros
año_inicio = 2016      # primer año de la retroproyeccion a usar (maximo 2017)
año_fin    = 2026      # ultimo año de la proyeccion a usar

mes_inicio_censo = 1    # enero
mes_fin_censo    = 10   # octubre
mes_ancla        = round((mes_inicio_censo + mes_fin_censo) / 2)  # junio

metodo = "monoH.FC"     # "monoH.FC" (spline monotono) o "lineal"

# ventana de salida: enero 2017 a diciembre 2025 (108 meses), indexada de 1 a 108
fecha_inicio_salida = as.Date("2017-01-01")
fecha_fin_salida    = as.Date("2025-12-01")
n_meses_esperado    = 108

# proyecciones 2018-2070 (limpieza de proyecciones_dane.R)
proyec_tmp = readxl::read_xlsx("bases/PPED-AreaSexoEdadNac-2018-2070.xlsx", sheet = 3)

# nombrado de variables
colnames(proyec_tmp) = c(
  proyec_tmp[5,1:3],
  proyec_tmp[6,4:309]
)

# cargado
proyec_tmp = proyec_tmp[7:133,]

# normalizacion de variables
names(proyec_tmp) = proyec_tmp %>%
  names() %>%
  str_replace_all(" ", "_")

# seleccion de variables necesarias
proyec_tmp = proyec_tmp %>%
  select(
    año = AÑO,
    total_poblacional = Total,
    ÁREA_GEOGRÁFICA
  ) %>%
  filter(
    ÁREA_GEOGRÁFICA == "Total"
    ) %>%
  mutate(
    año = as.integer(año),
    total_poblacional = as.numeric(total_poblacional)
    )

# retroproyecciones 1950-2017 (limpieza de proyecciones_dane.R)
retyec_tmp = readxl::read_xlsx("bases/PPED-AreaNac-1950-2017.xlsx", sheet = 1)

# cargado
colnames(retyec_tmp) = c(
  retyec_tmp[2,1],
  retyec_tmp[6,2:5]
)

# renombrado de variables
names(retyec_tmp) = retyec_tmp %>%
  names() %>%
  str_replace_all(" ", "_")

# selección de variables necesarias
retyec_tmp = retyec_tmp %>%
  select(
    año = AÑO,
    total_poblacional = Población,
    ÁREA_GEOGRÁFICA
  ) %>%
  filter(
    ÁREA_GEOGRÁFICA == "Total"
    ) %>%
  mutate(
    año = as.integer(año),
    total_poblacional = as.numeric(total_poblacional)
    )

# serie anual unida
proyec = bind_rows(retyec_tmp, proyec_tmp) %>%
  select(
    año, 
    total_poblacional
    ) %>%
  filter(
    año >= año_inicio,
    año <= año_fin
    ) %>%
  arrange(
    año
    )

# verificaciones 
stopifnot(!anyNA(proyec$año), !anyNA(proyec$total_poblacional))
stopifnot(!anyDuplicated(proyec$año))
stopifnot(all(diff(proyec$año) == 1))   # sin años faltantes

# indice mensual: t = año * 12 + (mes - 1)
# se anclan las series anuales en los meses de medición del censo para 
# realizar la interpolación
t_ancla = proyec$año * 12 + (mes_ancla - 1)
t_mes   = seq(min(t_ancla), max(t_ancla))


f = switch(
  metodo,
  lineal   = approxfun( # metodo de interpolacion lineal
    t_ancla,
    proyec$total_poblacional), 
  monoH.FC = splinefun(
    t_ancla,
    proyec$total_poblacional,
    method = "monoH.FC"
    ),
  stop("metodo no valido")
)

# resultado de interpolacion
mensual_total = tibble(
  año = t_mes %/% 12,
  mes = t_mes %% 12 + 1,
  fecha = as.Date(sprintf("%d-%02d-01", año, mes)),
  total_poblacional = f(t_mes),
  es_ancla = t_mes %in% t_ancla
)

# control: los anclajes conservan el valor original
control = mensual_total %>%
  filter(es_ancla) %>%
  inner_join(proyec, by = "año", suffix = c("_mensual", "_anual"))

stopifnot(all.equal(control$total_poblacional_mensual,
                    control$total_poblacional_anual))

# ventana de salida con indice del mes 1 al 108
mensual = mensual_total %>%
  filter(fecha >= fecha_inicio_salida, fecha <= fecha_fin_salida) %>%
  arrange(fecha) %>%
  mutate(indice = row_number()) %>%
  select(indice, año, mes, fecha, total_poblacional, es_ancla)

stopifnot(nrow(mensual) == n_meses_esperado)
stopifnot(identical(mensual$indice, 1:n_meses_esperado))

mensual = mensual %>% 
  select(
    indice,
    total_poblacional
    )

# guardado
write.csv(x = mensual,
          file = "diccionarios/proyecciones_dane_mensual.csv",
          row.names = FALSE)

rm(list = ls())
