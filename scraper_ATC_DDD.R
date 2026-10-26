# Scraper de medicamentos

# v 0.1: Tablas e identificacion de scraper

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

