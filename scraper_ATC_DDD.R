# Scraper de medicamentos

# v 1.0: Scraper completo, salida en DDD.csv -  Testeado

# Se van a guardar todos los DDD correspondientes a medicamentos de la siguiente lista ATC:
# Fuente del scraping: https://atcddd.fhi.no/atc_ddd_index/


# ATC     | Índices Disponibles --------------|
# N06A    | A, B, F, G, X                     |
# N05B    | A                                 |
# N05C    | A, B, C, D, E, F, H, J, M, X      |
# N05A    | A, B, C, D, E, F, G, H, L, N, X   |
# C09CA01 |                                   |
# C10AA   |                                   |
# N03AE01 |                                   |

# -------------------- Información previa de Scraping --------------------------

# 1) Link 

# El link de la pagina se compone de:
# https://atcddd.fhi.no/atc_ddd_index/?code=$XXXXX$&showdescription=no
# Donde $XXXXX$ es el código ATC

# 2) Composición

# Cada medicamento está incorporado en un argumento <tr> que de manera
# secuencial guarda 6 datos en un argumento <td>. Respectivamente:

# - ATC code
# - Name
# - DDD
# - U
# - Adm. R
# - Note

# Cada uno puede ser guardado sin texto

# 3) Filas de continuación

# El ATC code y el Name aparecen solo una vez por principio activo.
# Las filas siguientes (otra DDD u otra vía) tienen esas dos celdas vacías.
# El script rellena esas celdas con el último ATC code y Name leídos.

# -------------------------- Paquetes ------------------------------------------

rm(list = ls())

# install.packages(c("rvest", "xml2"))   # Ejecutar una vez si faltan
library(rvest)
library(xml2)

# -------------------------- Códigos a consultar -------------------------------

indices <- list(
  N06A = c("A", "B", "F", "G", "X"),
  N05B = c("A"),
  N05C = c("A", "B", "C", "D", "E", "F", "H", "J", "M", "X"),
  N05A = c("A", "B", "C", "D", "E", "F", "G", "H", "L", "N", "X")
)

codigos_grupo <- unlist(
  lapply(names(indices), function(prefijo) paste0(prefijo, indices[[prefijo]])),
  use.names = FALSE
)

codigos_extra <- c("C09CA01", "C10AA", "N03AE01")

codigos <- c(codigos_grupo, codigos_extra)

# -------------------------- Funciones -----------------------------------------

url_atc <- function(codigo) {
  paste0("https://atcddd.fhi.no/atc_ddd_index/?code=", codigo,
         "&showdescription=no")
}

# Reemplaza espacios duros (&nbsp;) y recorta espacios
limpiar <- function(x) {
  x <- gsub("\u00a0", " ", x, fixed = TRUE)
  trimws(x)
}

# Devuelve las filas <tr> de la tabla que tiene el encabezado "ATC code"
encontrar_filas <- function(pagina) {
  tablas <- html_elements(pagina, "table")
  for (tabla in tablas) {
    filas <- html_elements(tabla, xpath = ".//tr")
    if (length(filas) == 0) next
    primera <- limpiar(html_text(html_elements(filas[[1]], "td")))
    if (length(primera) > 0 && grepl("^ATC code", primera[1])) {
      return(filas)
    }
  }
  NULL
}

extraer_codigo <- function(codigo) {
  pagina <- read_html(url_atc(codigo), encoding = "UTF-8")
  filas  <- encontrar_filas(pagina)
  
  if (is.null(filas)) {
    warning("Sin tabla ATC para el código: ", codigo)
    return(NULL)
  }
  
  atc_actual    <- ""
  nombre_actual <- ""
  salida        <- list()
  
  # La fila 1 es el encabezado
  for (i in seq_along(filas)[-1]) {
    celdas <- limpiar(html_text(html_elements(filas[[i]], "td")))
    
    # Completar a 6 datos si faltan celdas
    if (length(celdas) < 6) celdas <- c(celdas, rep("", 6 - length(celdas)))
    celdas <- celdas[1:6]
    
    # Fila nueva de principio activo: actualizar ATC code y Name
    if (celdas[1] != "") {
      atc_actual    <- celdas[1]
      nombre_actual <- celdas[2]
    }
    
    salida[[length(salida) + 1]] <- data.frame(
      ATC_code = atc_actual,
      Name     = nombre_actual,
      DDD      = celdas[3],
      U        = celdas[4],
      Adm_R    = celdas[5],
      Note     = celdas[6],
      stringsAsFactors = FALSE
    )
  }
  
  if (length(salida) == 0) return(NULL)
  tabla <- do.call(rbind, salida)
  
  # Conservar solo los ATC code que pertenecen al código consultado.
  # Sirve para códigos de 5 niveles (ej. C09CA01) y para grupos (ej. N05AD).
  tabla <- tabla[startsWith(tabla$ATC_code, codigo), , drop = FALSE]
  if (nrow(tabla) == 0) return(NULL)
  
  tabla$Consulta <- codigo
  tabla
}

# -------------------------- Ejecución -----------------------------------------

resultados <- lapply(codigos, function(codigo) {
  message("Consultando: ", codigo)
  Sys.sleep(1)  # Pausa entre consultas
  tryCatch(
    extraer_codigo(codigo),
    error = function(e) {
      warning("Falla en ", codigo, ": ", conditionMessage(e))
      NULL
    }
  )
})

DDD <- do.call(rbind, resultados)
rownames(DDD) <- NULL

# Revisar resultado
print(head(DDD, 15))
message("Filas totales: ", nrow(DDD))

# -------------------------- Exportar ------------------------------------------



write.csv(DDD, "diccionarios/DDD.csv", row.names = FALSE, na = "", fileEncoding = "UTF-8")
message("Archivo guardado: ", normalizePath("DDD.csv"))

