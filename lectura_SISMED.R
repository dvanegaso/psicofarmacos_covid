
# Lectura y selección de bases de datos.

# v 0.3:

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

principios_activos = read.csv("bases/principios_activos.csv") # Principios activos
medicamentos_candidatos = read.csv("bases/medicamentos_candidatos.csv") # medicamentos candidatos

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
############################ Limpieza y cargado ################################
################################################################################

# SISMED - completa

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

# selección de años específicos 

# 2017 a 2025

SISMED = SISMED %>% filter(año > 2016, año < 2026)


# Estandarizacón de registro sanitario

SISMED = SISMED %>% mutate(principioactivo = str_to_lower(SISMED$principioactivo))

# se procede a revisar cuales datos contienen mediciones para todos los meses
for (u in 1:length(medicamentos_candidatos$principioactivo)) {
  
  print(medicamentos_candidatos$principioactivo[u])
  
  tabla = table(
    SISMED$mes[SISMED$principioactivo == medicamentos_candidatos$principioactivo[u]],
    SISMED$año[SISMED$principioactivo == medicamentos_candidatos$principioactivo[u]]
  )
  
  print(tabla)
  print(sum(tabla == 0))
}

# selección de principios cantidatos
SISMED = SISMED %>% filter(principioactivo %in% medicamentos_candidatos$principioactivo)

# Estandarización de Registro Sanitario 

SISMED = SISMED %>%
  mutate(
    registrosanitario = str_remove_all(registrosanitario, "\\s")
         )

# limpieza manual
SISMED$concentracion[SISMED$concentracion == "0.001 g/mL"] = "1 mg/mL"
SISMED$concentracion[SISMED$concentracion == "0.0025 g/mL"] = "2.5 mg/mL"
SISMED$concentracion[SISMED$concentracion == "0.004 g/mL"] = "4 mg/mL"

# seleccion de unidades a verificar

SISMED_ml = SISMED %>% filter(str_detect(concentracion, "mg/mL"))
SISMED_gr = anti_join(SISMED, SISMED_ml)

# visualizar las tablas faltantes

resumen_ml = SISMED_ml %>% 
  group_by(
    registrosanitario
    ) %>% 
  summarise(
    formafarmaceutica = first(formafarmaceutica),
    producto = first(producto),
    titular = first(titular),
    principioactivo = first(principioactivo)
  )
# View(resumen_ml)

# Selección de variables SISMED

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

reg_cum = SISMED %>% 
  select(cum) %>%
  group_by(cum) %>%
  summarise(n())

################################################################################
############################ Limpieza y cargado ################################
################################################################################

# SISMED - ficha


FICHA = bind_rows(FICHA)

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

FICHA = FICHA %>%
  mutate(
    registrosanitario = str_remove_all(registrosanitario, "\\s")
  ) %>%
  filter(cum %in% reg_cum$cum) %>%
  select(
    cum,
    atc5,
    registrosanitario, 
    activoporumd, 
    umdporunidad, 
    unidadactivo, 
    umd, producto
    )

reg_cum2 = FICHA %>% 
  select(cum) %>%
  group_by(cum) %>%
  summarise(n = n())

reg_cum3 = FICHA %>% 
  group_by(cum) %>%
  filter(umd == "TABLETA") %>%
  select(cum) %>%
  summarise(n = n())


################################################################################
############################ Limpieza y cargado ################################
################################################################################

# CUM

CUM = read.csv(
  "bases/CÓDIGO_ÚNICO_DE_MEDICAMENTOS_VIGENTES_20261025.csv",
  sep = ",",
  dec = ",")

# 

length(table(reg_cum$cum))

CUM = CUM %>% 
  mutate(
    principioactivo = str_to_lower(principioactivo)
    ) %>% 
  mutate(
    descripcionatc = str_to_lower(descripcionatc)
  ) %>%
  mutate(
    expediente = str_c(expediente, "-1")
  ) %>%
  mutate(
    registrosanitario = str_remove_all(registrosanitario, "\\s")
    ) %>% filter(expediente %in% reg_cum$cum)

# modificaciones
CUM = CUM %>% 
  select(
    cum = expediente,
    registrosanitario,
    unidadmedida, 
    cantidadcum,
    cantidad,
    unidadmedida, principioactivo
    ) %>% 
  mutate(cantidad_medida = cantidadcum * cantidad)


# selección de principios cantidatos

CUM = CUM %>% group_by(cum)
CUM = CUM %>% filter(principioactivo %in% medicamentos_candidatos$principioactivo) %>%
  group_by(cum) %>%
  summarise(
    principioactivo = first(principioactivo),
    unidadmedida = first(unidadmedida),
    cantidad = first(cantidad),
    .groups = "drop"
  ) %>% select(cum, unidadmedida, cantidad)

reg_cum1 = CUM %>% 
  select(cum) %>%
  group_by(cum) %>%
  summarise(n = n())

################################################################################
############################## UNIÓN DE BASES ##################################
################################################################################

# se selecciona la base mas completa con medicamentos en tabletas

# la base FICHA es la única con todos los datos
length(table(reg_cum$cum))
length(table(reg_cum1$cum)) # CU
length(table(reg_cum2$cum)) # FICHA
length(table(reg_cum3$cum)) # FICHA TABLETAS


# se tendrán un total de 379 medicamentos (USANDO FICHA Y TABLETAS)

FICHA = FICHA %>% filter(umd == "TABLETA")

SISMED_final = left_join(SISMED, FICHA, by = "cum")

write.csv(SISMED_final, "salidas/base_sismed_final.csv")

