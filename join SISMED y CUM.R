rm(list = ls())
# Lectura y selección de bases de datos.

# v 0.4: seleccion de variables necesarias y bases de datos de referencia

# Este codigo solamente selecciona 

# | Referencias:                                 | Base tomada:  |
# | ---------------------------------------------|---------------|
# | Cantidades de principio activo y medicamento | FICHA         |
# | Cantidades de medicamentos vendidos por mes  | SISMED        |

# Consideraciones:
# - Se tomaron solamente medicamentos vendidos en tableta.
# - Se hizo join de las bases por variable CUM, ya que muestra varias presentaciones
# de medicamentos con distintas unidades.


# Se seleccionan los datos de la cohorte.

# Metodos para mg de cada medicamentos en otras unidades:

# 1. Cruzar base de datos CUM con SISMED: 6 de 26 medicamentos encontrados (informacion errónea).
# 2. Base de datos ficha.csv: No se conoce la veracidad de algunas unidades.

library(dplyr)
library(stringr)
library(tidyr)
library(tidytext)
library(readr)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

principios_activos = read.csv("bases/principios_activos.csv") # se cargan principios activos
medicamentos_candidatos = read.csv("bases/medicamentos_candidatos.csv") # se cargan medicamentos candidatos

SISMED = list() # base completa.csv
FICHA = list()  # base ficha.csv

for (i in 1:5) {
  temp_FICHA = read.csv(
    paste0("bases/SISMED/",principios_activos[i,2],"_2026-10-26/ficha.csv"),
    dec = ".",
    sep = ",")
  temp_base = read.csv(
    paste0("bases/SISMED/",principios_activos[i,2],"_2026-10-26/completa.csv"),
    dec = ".",
    sep = ",")
  
  # guardado en la lista -------------------------
  SISMED[[i]] = temp_base
  FICHA[[i]] = temp_FICHA
  rm(temp_base, temp_FICHA)
}

################################################################################
########################## limpieza Base SISMED ################################
################################################################################

# normalización de variables 

# Base completa
SISMED = bind_rows(SISMED)

reemplazo = list(
  'á' = 'a',
  'é' = 'e',
  'í' = 'i',
  'ó' = 'o',
  'ú' = 'u'
)

# Estandarización de nombres
names(SISMED) = SISMED %>% 
  names() %>%
  str_replace_all("\\.","") %>% 
  str_to_lower() %>% 
  str_replace_all(c('á' = 'a',
                    'é' = 'e',
                    'í' = 'i',
                    'ó' = 'o',
                    'ú' = 'u'))

# Fecha de expedición del medicamento

SISMED = SISMED %>% separate(fecha, into = c("año", "mes", "n_dias"), sep = "-")
# último día del mes correspondiente
SISMED$n_dias = lubridate::days_in_month(
  lubridate::make_date(SISMED$año, SISMED$mes, 1)
)
SISMED$año = as.numeric(SISMED$año)
SISMED$mes = as.numeric(SISMED$mes)

# filtrado de años: 2017 a 2025

SISMED = SISMED %>% filter(año > 2016, año < 2026)

# Estandarizacón de registro sanitario

SISMED = SISMED %>% mutate(principioactivo = str_to_lower(SISMED$principioactivo))

# filtrado: principios activos dentro de los candidatos
SISMED = SISMED %>% 
  filter(
    atc %in% medicamentos_candidatos$ATC_code
    )



# Selección de variables SISMED ------------------------------------------------

SISMED = SISMED %>% 
  select(
    cum,
    atc, 
    grupoatc, 
    principioactivo, 
    concentracion, 
    registrosanitario, 
    año, 
    mes, 
    n_dias, 
    unidadescomercial, 
    unidadesinstitucional
    )


################################################################################
############################ limpieza Base FICHA ###############################
################################################################################

FICHA = bind_rows(FICHA)

# limpieza de nombres --------------------
names(FICHA) = FICHA %>% 
  names() %>%
  str_replace_all("\\_","") %>% 
  str_to_lower() %>% 
  str_replace_all(c('á' = 'a',
                    'é' = 'e',
                    'í' = 'i',
                    'ó' = 'o',
                    'ú' = 'u')
                  )

FICHA = FICHA %>% 
  mutate(
    registrosanitario = str_remove_all(registrosanitario, "\\s")
    )
# seleccion de variables
FICHA = FICHA %>%
  mutate(
    registrosanitario = str_remove_all(registrosanitario, "\\s")
  ) %>% 
  select(
    cum,
    atc5,
    registrosanitario, 
    activoporumd, 
    umdporunidad, 
    unidadactivo, 
    umd, producto
    )


################################################################################
############################ limpieza Base CUM #################################
################################################################################

# CUM: Esta variable no va a funcionar, las llaves CUM necesarias para JOIN no 
# son únicas en la base CUM.

CUM = read.csv(
  "bases/CÓDIGO_ÚNICO_DE_MEDICAMENTOS_VIGENTES_20261025.csv",
  sep = ",",
  dec = ",")

# normalizacion de nombres

CUM = CUM %>% 
  mutate(# normalizcion de nombres
    principioactivo = str_to_lower(principioactivo) 
    ) %>% 
  mutate(# normalizcion de nombres
    descripcionatc = str_to_lower(descripcionatc)
  ) %>% 
  filter(atc %in% medicamentos_candidatos$ATC_code)  # filtrado de variables de interes

# seleccion de nombres
CUM = CUM %>% 
  select(
    cum = expediente,
    registrosanitario,
    unidadmedida, 
    cantidadcum,
    cantidad,
    atc,
    unidadmedida, principioactivo
    ) %>% 
  mutate( 
    cantidad_medida = cantidadcum * cantidad
    )

SISMED_cum = SISMED %>% mutate(expediente = str_remove(cum, "-."))


# selección de principios cantidatos

CUM = CUM %>% group_by(cum) %>%
  group_by(cum) %>%
  summarise(
    principioactivo = first(principioactivo),
    unidadmedida = first(unidadmedida),
    cantidad = first(cantidad),
    .groups = "drop"
  ) %>% select(cum, unidadmedida, cantidad)


################################################################################
############################## UNIÓN DE BASES ##################################
################################################################################

# se selecciona la base mas completa con medicamentos en tabletas

# solamente la base FICHA cuenta con todos los registros conocidos

# se tendrán un total de 379 medicamentos (USANDO FICHA Y TABLETAS)

SISMED_final = left_join(SISMED, FICHA, by = "cum")

# se seleccionan los medicamentos en presentacion de tableta

SISMED_final$umdporunidad[SISMED_final$unidadactivo == "g"] = 1000 * SISMED_final$umdporunidad[SISMED_final$unidadactivo == "g"]

# se procede a revisar cuales datos contienen mediciones para todos los meses

# esto incluye tanto uso institucional como comercial

for (u in 1:length(medicamentos_candidatos$Name)) {
  print(medicamentos_candidatos$principioactivo[u])
  tabla = table(
    SISMED_final$mes[SISMED_final$principioactivo == medicamentos_candidatos$Name[u]],
    SISMED_final$año[SISMED_final$principioactivo == medicamentos_candidatos$Name[u]]
  )
  cat("---------------------", medicamentos_candidatos$Name[u], "---------------------")
  print(tabla)
  print(sum(tabla == 0))
  
}

# guardado
write.csv(SISMED_final, "salidas/base_sismed_final.csv")

rm(list = ls())
  
