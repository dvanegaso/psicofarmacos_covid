# =====================================================================
# PIPELINE DE ANÁLISIS DE SERIES DE TIEMPO EN R
# Autor del flujo: Daniel (Dpto. Estadística, UNAL)
# Hecho con Claude Sonnet 5.5 entre el 23 y 24 de octubre 2026
# Referencias base: Montgomery, Jennings y Kulahci (2015); Wei (2006);
#                   Box, Jenkins, Reinsel y Ljung (2015).
# Bibliografía completa de cada test: ver SKILL.md (sección Bibliografía).
#
# CÓMO USAR
#   1. Reemplaza el bloque "DATOS" por tu serie (objeto ts llamado y).
#   2. Ajusta la sección CONFIGURACIÓN.
#   3. Ejecuta el script completo, o paso a paso.
#
# LÓGICA DE DECISIÓN
#   Cada paso imprime "DECISIÓN: ... -> PASO X".
#   Si el test se cumple, el script sigue al paso indicado.
#   Si no se cumple, el script prueba el modelo alternativo.
#
# MAPA DE PASOS
#   0  Configuración, datos y funciones auxiliares
#   1  Exploración visual (serie, STL, ACF/PACF crudos, atípicos)
#   2  Varianza constante (Box-Cox)
#   3  Estacionariedad (ADF, PP, KPSS, Zivot-Andrews, OCSB)
#   4  Identificación (ACF/PACF) del orden ARIMA
#   5  Estimación (auto.arima + candidatos manuales)
#   6  Diagnóstico de residuales (Ljung-Box, normalidad, ARCH-LM)
#   6B Reintento con búsqueda ampliada si Ljung-Box falla
#   7  Modelos ARCH / GARCH si hay heterocedasticidad condicional
#   8  Cambio estructural (Bai-Perron, CUSUM, Chow)
#   9  Series de tiempo interrumpidas (ITS) con OLS / GLS / ARIMA+xreg
#   10 Regresión con mínimos cuadrados generalizados (GLS), opción extra
#   11 Pronóstico y validación cruzada (tsCV)
#   12 Prophet (Meta) para series semanales, opción extra
# =====================================================================


# =====================================================================
# PASO 0. CONFIGURACIÓN, DATOS Y FUNCIONES AUXILIARES
# =====================================================================

## 0.1 Paquetes ---------------------------------------------------------
req <- c("forecast", "tseries", "urca", "lmtest", "strucchange",
         "nlme", "sandwich", "zoo")
faltan <- req[!vapply(req, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltan))
  stop("Instala primero: install.packages(c(",
       paste0('"', faltan, '"', collapse = ", "), "))")
suppressPackageStartupMessages({
  library(forecast); library(tseries); library(urca)
  library(lmtest);   library(strucchange); library(nlme)
})
# Paquetes opcionales. El script avisa si faltan y usa un respaldo.
TIENE_RUGARCH <- requireNamespace("rugarch",  quietly = TRUE)
TIENE_PROPHET <- requireNamespace("prophet",  quietly = TRUE)

## 0.2 Configuración -----------------------------------------------------
ALFA               <- 0.05          # nivel de significancia de todos los tests
FRECUENCIA         <- 12            # 12 mensual, 4 trimestral, 52.18 semanal
INICIO             <- c(2010, 1)    # inicio de la serie (año, periodo)
H                  <- 12            # horizonte de pronóstico
FECHA_INTERVENCION <- as.Date("2019-01-01")  # NULL si no hay intervención
FORZAR_GARCH       <- FALSE         # TRUE para ejecutar el paso 7 siempre
EJECUTAR_PROPHET   <- TRUE          # paso 12 (necesita el paquete prophet)
GUARDAR_FIG        <- TRUE          # TRUE guarda PNG en DIR_SALIDA
DIR_SALIDA         <- "salida_series"
set.seed(2026)
if (GUARDAR_FIG) dir.create(DIR_SALIDA, showWarnings = FALSE)

## 0.3 Datos ---------------------------------------------------------------
# Reemplaza este bloque por tus datos. Ejemplo:
#   datos <- read.csv("mi_serie.csv")
#   y <- ts(datos$valor, start = c(2015, 1), frequency = 12)

simular_mensual <- function(n = 168) {
  # Tendencia + estacionalidad + errores AR(1) con ARCH(1)
  # + cambio de nivel y de pendiente en el mes 109 (enero 2019).
  t <- 1:n
  z <- rnorm(n); e <- numeric(n); h <- numeric(n); h[1] <- 1
  for (i in 2:n) { h[i] <- 0.4 + 0.5 * e[i - 1]^2; e[i] <- sqrt(h[i]) * z[i] }
  u <- as.numeric(stats::filter(e, 0.6, method = "recursive"))
  y <- 100 + 0.4 * t + 8 * sin(2 * pi * t / 12) + 4 * cos(2 * pi * t / 6) + u
  post <- t >= 109
  y[post] <- y[post] + 10 - 0.15 * (t[post] - 109)
  ts(y, start = INICIO, frequency = 12)
}
if (!exists("y")) y <- simular_mensual()
if (anyNA(y)) {
  warning("La serie tiene NA. Se interpolan con forecast::na.interp().")
  y <- na.interp(y)
}
m <- frequency(y)   # periodo estacional
n <- length(y)

## 0.4 Funciones auxiliares ---------------------------------------------
abrir_fig <- function(nombre, ancho = 1100, alto = 800) {
  if (GUARDAR_FIG)
    png(file.path(DIR_SALIDA, paste0(nombre, ".png")),
        width = ancho, height = alto, res = 110)
}
cerrar_fig <- function() if (GUARDAR_FIG) invisible(dev.off())
paso    <- function(txt) cat("\n", strrep("=", 70), "\n", txt, "\n",
                             strrep("=", 70), "\n", sep = "")
decidir <- function(msg) cat("  >> DECISIÓN:", msg, "\n")
p_fmt   <- function(p) format.pval(p, digits = 3, eps = 1e-4)

# ARCH-LM de Engle (1982): regresa e^2 contra sus rezagos.
# H0: no hay efecto ARCH. Equivale a FinTS::ArchTest.
arch_lm <- function(e, lags = 12) {
  e <- as.numeric(e) - mean(e); e2 <- e^2
  X <- embed(e2, lags + 1)
  r2 <- summary(lm(X[, 1] ~ X[, -1, drop = FALSE]))$r.squared
  est <- nrow(X) * r2
  list(statistic = est, df = lags,
       p.value = pchisq(est, lags, lower.tail = FALSE))
}

# Guía rápida de lectura de gráficos (imprime en consola).
guia <- function(tema) {
  g <- list(
    acf_pacf = c(
      "ACF y PACF: bandas = +-1.96/sqrt(n). 1 de 20 rezagos puede salir fuera por azar.",
      "ACF decae lento, casi lineal -> no estacionaria. Diferencia (d=1).",
      "ACF decae exponencial o sinusoidal + PACF corta en p -> AR(p). El orden es el ultimo rezago significativo de la PACF.",
      "ACF corta en q + PACF decae -> MA(q). El orden es el ultimo rezago significativo de la ACF.",
      "Ambas decaen -> ARMA(p,q). Empieza en (1,1) y compara por AICc.",
      "Picos en m, 2m, 3m que decaen lento -> raiz unitaria estacional. Usa D=1.",
      "ACF con pico en m (no en 2m) -> MA estacional Q=1. PACF con pico en m (no en 2m) -> AR estacional P=1.",
      "ACF(1) cerca de -0.5 tras diferenciar -> sobrediferenciacion. Quita una diferencia."),
    residuales = c(
      "Residuales vs tiempo: deben oscilar en torno a 0 sin patron.",
      "Forma de embudo -> varianza no constante. Usa Box-Cox o GARCH.",
      "Saltos o tramos con otra media -> cambio estructural. Ve al paso 8 y 9.",
      "ACF de residuales: todo dentro de bandas. Picos -> el modelo es insuficiente.",
      "ACF de residuales^2 con picos -> efecto ARCH. Ve al paso 7.",
      "QQ y histograma: colas pesadas -> usa distribucion t o bootstrap.",
      "p-valores de Ljung-Box por rezago: todos > 0.05."),
    raices = c(
      "Raices inversas AR/MA dentro del circulo unitario -> estacionario/invertible.",
      "Raiz inversa cerca de 1 -> casi raiz unitaria o sobreajuste. Simplifica el modelo."),
    cusum = c(
      "OLS-CUSUM: si la trayectoria cruza la banda roja -> parametros inestables.",
      "Bai-Perron: elige el numero de quiebres con el BIC minimo."),
    its = c(
      "ITS: la distancia entre la curva ajustada y la contrafactual es el efecto.",
      "Coeficiente 'interv' = cambio de nivel inmediato. Coeficiente 'post' = cambio de pendiente."),
    garch = c(
      "Volatilidad condicional con rachas altas y bajas -> agrupamiento de volatilidad.",
      "alpha + beta cercano a 1 -> volatilidad muy persistente (IGARCH).",
      "Residuales estandarizados deben pasar Ljung-Box y ARCH-LM."),
    prophet = c(
      "Componente tendencia: los cambios de pendiente marcan changepoints.",
      "Componente anual: forma de la estacionalidad. Si es ruidosa, baja yearly fourier_order.",
      "Si Prophet no supera a snaive/ARIMA en validacion, usa el modelo simple.")
  )
  cat("\n  GUIA RAPIDA [", tema, "]\n", sep = "")
  cat(paste0("   - ", g[[tema]]), sep = "\n")
}


# =====================================================================
# PASO 1. EXPLORACIÓN VISUAL
# =====================================================================
paso("PASO 1. Exploración visual")

# 1.1 Serie, tendencia suavizada, gráfico estacional y ACF/PACF crudos.
abrir_fig("01_exploracion")
op <- par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
plot(y, main = "Serie observada", ylab = "y", xlab = "Tiempo")
lines(lowess(time(y), y, f = 0.3), col = "red", lwd = 2)       # tendencia
if (m > 1) monthplot(y, main = "Gráfico estacional (por ciclo)") else plot.new()
Acf(y,  lag.max = min(36, n / 4), main = "ACF de la serie original")
Pacf(y, lag.max = min(36, n / 4), main = "PACF de la serie original")
par(op); 
cerrar_fig()
guia("acf_pacf")

# 1.2 Descomposición STL (Cleveland et al. 1990).
if (m > 1) {
  abrir_fig("01b_stl")
  plot(stl(y, s.window = "periodic", robust = TRUE),
       main = "Descomposición STL")
  cerrar_fig()
  cat("  STL: el 'remainder' debe parecer ruido. Si tiene estructura, falta modelar.\n")
}

# 1.3 Valores atípicos (Hyndman: residuales de STL + criterio IQR).
atip <- tsoutliers(y)
if (length(atip$index)) {
  cat("  Atípicos candidatos en posiciones:", atip$index, "\n")
  cat("  Revísalos. No los borres sin causa. Considera variables pulso en el paso 9.\n")
} else cat("  Sin atípicos evidentes.\n")


# =====================================================================
# PASO 2. VARIANZA CONSTANTE (BOX-COX)
# Montgomery cap. 2: estabiliza la varianza ANTES de diferenciar.
# =====================================================================
paso("PASO 2. Varianza constante")

lambda <- NULL    # NULL = sin transformación
if (min(y) > 0) {
  lam <- BoxCox.lambda(y, method = "guerrero")
  # Relación nivel-dispersión: sd por bloque anual contra media por bloque.
  bloque <- ceiling(seq_len(n) / max(m, 1))
  mu_b <- tapply(y, bloque, mean); sd_b <- tapply(y, bloque, sd)
  ct <- cor.test(mu_b, sd_b)
  abrir_fig("02_nivel_dispersion")
  plot(mu_b, sd_b, pch = 19, xlab = "Media por bloque", ylab = "Desv. est. por bloque",
       main = "Nivel vs dispersión")
  abline(lm(sd_b ~ mu_b), col = "red")
  cerrar_fig()
  cat(sprintf("  lambda (Guerrero) = %.3f | cor(media, sd) = %.2f, p = %s\n",
              lam, ct$estimate, p_fmt(ct$p.value)))
  if (ct$p.value < ALFA && abs(1 - lam) > 0.2) {
    lambda <- if (abs(lam) < 0.15) 0 else round(lam, 2)  # 0 = logaritmo
    decidir(sprintf("la varianza depende del nivel. Usa lambda = %s -> PASO 3", lambda))
  } else decidir("varianza estable. Sin transformación -> PASO 3")
} else cat("  Hay valores <= 0. Box-Cox no aplica. Considera sumar una constante.\n")
y_t <- if (is.null(lambda)) y else BoxCox(y, lambda)


# =====================================================================
# PASO 3. ESTACIONARIEDAD EN MEDIA
#   ADF  : H0 = raíz unitaria   (rechazar -> estacionaria)
#   PP   : H0 = raíz unitaria   (robusto a heterocedasticidad)
#   KPSS : H0 = estacionaria    (rechazar -> raíz unitaria)
# Se usan juntos (Montgomery; Wei cap. 9; Kwiatkowski et al. 1992).
# =====================================================================
paso("PASO 3. Estacionariedad")

quitar_estacionalidad_det <- function(x) {
  # Quita la media estacional (dummies) para que ADF/KPSS no se confundan
  # con una estacionalidad determinista. Solo se usa dentro de los tests.
  mm <- frequency(x)
  if (mm <= 1 || length(x) < 3 * mm) return(x)
  f <- factor(cycle(x)); tt <- seq_along(x)
  b <- coef(lm(as.numeric(x) ~ tt + f))
  s <- model.matrix(~ f)[, -1, drop = FALSE] %*% b[grep("^f", names(b))]
  x - as.numeric(s)
}

prueba_estacionariedad <- function(x_in, alfa = ALFA, deseason = FALSE) {
  x <- if (deseason) quitar_estacionalidad_det(x_in) else x_in
  nx <- length(x); tt <- seq_len(nx)
  # ¿Hay tendencia determinista? Define el tipo de test.
  p_tend <- summary(lm(as.numeric(x) ~ tt))$coefficients["tt", 4]
  con_tend <- p_tend < alfa
  tipo <- if (con_tend) "trend" else "drift"
  kmax <- floor(12 * (nx / 100)^0.25)               # regla de Schwert
  adf <- ur.df(as.numeric(x), type = tipo, lags = kmax, selectlags = "AIC")
  tau <- adf@teststat[1, 1]; crit <- adf@cval[1, "5pct"]
  adf_rechaza <- tau < crit                         # rechaza raíz unitaria
  pp  <- suppressWarnings(pp.test(as.numeric(x)))
  kp  <- suppressWarnings(kpss.test(as.numeric(x),
                                    null = if (con_tend) "Trend" else "Level"))
  pp_rechaza <- pp$p.value < alfa
  kp_rechaza <- kp$p.value < alfa                   # rechaza estacionariedad
  veredicto <-
    if (adf_rechaza && pp_rechaza && !kp_rechaza) "ESTACIONARIA"
    else if (!adf_rechaza && kp_rechaza)          "RAIZ_UNITARIA"
    else if (adf_rechaza && kp_rechaza)           "AMBIGUA"
    else                                          "SIN_PODER"
  cat(sprintf("  tipo=%s | ADF tau=%.2f (crit 5%%=%.2f) | PP p=%s | KPSS p=%s -> %s\n",
              tipo, tau, crit, p_fmt(pp$p.value), p_fmt(kp$p.value), veredicto))
  # Si no es claramente estacionaria, se pregunta si un QUIEBRE explica el
  # resultado (Perron 1989). Zivot-Andrews: H0 = raíz unitaria SIN quiebre.
  if (veredicto != "ESTACIONARIA") {
    za <- tryCatch(ur.za(as.numeric(x), model = if (con_tend) "both" else "intercept",
                         lag = floor(4 * (nx / 100)^0.25)), error = function(e) NULL)
    if (!is.null(za)) {
      za_rechaza <- za@teststat < za@cval[2]
      cat(sprintf("  Zivot-Andrews: est=%.2f, crit 5%%=%.2f -> %s (quiebre en t=%d)\n",
                  za@teststat, za@cval[2],
                  if (za_rechaza) "rechaza raíz unitaria" else "no rechaza", za@bpoint))
      if (za_rechaza && !(veredicto == "RAIZ_UNITARIA" && !kp_rechaza))
        veredicto <- "ESTAC_CON_QUIEBRE"
    }
  }
  list(veredicto = veredicto, con_tend = con_tend)
}

## 3.1 Diferenciación estacional (OCSB; Osborn et al. 1988) ---------------
x <- y_t; D <- 0; d <- 0
if (m > 1) {
  D <- nsdiffs(x, test = "ocsb", max.D = 1)
  if (D > 0) {
    x <- diff(x, lag = m, differences = D)
    decidir(sprintf("OCSB indica raíz unitaria estacional. D = %d", D))
  } else decidir("sin raíz unitaria estacional (estacionalidad determinista o nula). D = 0")
}

## 3.2 Diferenciación regular -------------------------------------------
QUIEBRE_ESTAC <- FALSE
repeat {
  r <- prueba_estacionariedad(x, deseason = (D == 0 && m > 1))
  con_tend_final <- r$con_tend
  if (r$veredicto == "ESTACIONARIA") {
    decidir(sprintf("serie estacionaria con d=%d, D=%d -> PASO 4", d, D)); break
  }
  if (r$veredicto == "ESTAC_CON_QUIEBRE") {
    QUIEBRE_ESTAC <- TRUE
    decidir(sprintf(paste("estacionaria alrededor de una tendencia con quiebre.",
                          "No se diferencia más (d=%d, D=%d). Se usará deriva y el PASO 8-9 -> PASO 4"), d, D))
    break
  }
  if (d >= 2) { decidir("d = 2 alcanzado. Se detiene para evitar sobrediferenciación."); break }
  x <- diff(x); d <- d + 1
  decidir(sprintf("raíz unitaria o evidencia mixta. Se diferencia. d = %d -> repite el test", d))
}

## 3.3 Estacionalidad determinista: regresores de Fourier --------------------
# Si OCSB dice D = 0 pero hay estacionalidad, se modela con senos y cosenos
# (regresión armónica dinámica; Hyndman y Athanasopoulos, fpp3 cap. 10).
K_best <- 0
if (m > 1 && D == 0) {
  Kmax <- min(6, floor(m / 2)); aic_K <- rep(NA_real_, Kmax)
  for (K in 1:Kmax) {
    f <- tryCatch(auto.arima(y, d = d, D = 0, lambda = lambda, seasonal = FALSE,
                             xreg = fourier(y, K = K), stepwise = TRUE,
                             approximation = TRUE, ic = "aicc"), error = function(e) NULL)
    if (!is.null(f)) aic_K[K] <- f$aicc
  }
  if (any(!is.na(aic_K))) {
    K_best <- which.min(aic_K)
    decidir(sprintf("términos de Fourier: K = %d (menor AICc entre 1..%d)", K_best, Kmax))
  }
}
# Serie para identificación: quita Fourier y tendencia determinista, si existen.
xr <- if (K_best > 0) as.matrix(fourier(y_t, K = K_best)) else NULL
if (!is.null(xr) && d > 0) xr <- diff(xr, differences = d)
tend <- if (d + D == 0 && con_tend_final) seq_along(x) else NULL
x_id <- x
if (!is.null(xr) || !is.null(tend)) {
  Xid <- cbind(tend, xr)
  x_id <- ts(residuals(lm(as.numeric(x) ~ Xid)), start = start(x), frequency = m)
}

abrir_fig("03_serie_estacionaria")
op <- par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
plot(x, main = sprintf("Serie transformada (d=%d, D=%d)", d, D), ylab = "")
abline(h = mean(x), col = "red")
hist(x, breaks = 20, main = "Histograma", xlab = "")
Acf(x,  lag.max = min(36, length(x) / 4), main = "ACF estacionaria")
Pacf(x, lag.max = min(36, length(x) / 4), main = "PACF estacionaria")
par(op); cerrar_fig()
if (acf(x, plot = FALSE, lag.max = 1)$acf[2] < -0.5)
  cat("  ALERTA: ACF(1) < -0.5. Posible sobrediferenciación. Prueba con una diferencia menos.\n")


# =====================================================================
# PASO 4. IDENTIFICACIÓN DEL ORDEN (ACF / PACF)
# Reglas de Box-Jenkins (Montgomery cap. 5; Wei cap. 6).
# La función imprime una SUGERENCIA. No sustituye el criterio del analista.
# =====================================================================
paso("PASO 4. Identificación")

sugerir_orden <- function(x, m = 1, alfa = ALFA) {
  nx <- length(x)
  lmax <- if (m > 1) min(2 * m + 2, floor(nx / 3)) else min(24, floor(nx / 3))
  ci <- qnorm(1 - alfa / 2) / sqrt(nx)
  a <- acf(x,  lag.max = lmax, plot = FALSE)$acf[-1]
  p <- pacf(x, lag.max = lmax, plot = FALSE)$acf
  ventana <- if (m > 1) min(m - 1, 12) else 12   # parte regular
  corte <- function(v, tol = 3) {
    sig <- abs(v[1:ventana]) > ci
    cand <- which(sig[1:min(6, ventana)])
    if (!length(cand)) return(if (sum(sig) <= 1) 0L else NA_integer_)
    k <- max(cand); resto <- sig[-(1:k)]
    if (length(resto) >= tol && sum(resto) <= 1) k else NA_integer_
  }
  cp <- corte(p); ca <- corte(a)
  if (!is.na(cp) && cp == 0 && !is.na(ca) && ca == 0) { pq <- c(0, 0); txt <- "Ruido blanco (0,0)" }
  else if (!is.na(cp) && is.na(ca)) { pq <- c(cp, 0); txt <- sprintf("AR(%d): PACF corta en %d, ACF decae", cp, cp) }
  else if (is.na(cp) && !is.na(ca)) { pq <- c(0, ca); txt <- sprintf("MA(%d): ACF corta en %d, PACF decae", ca, ca) }
  else if (is.na(cp) && is.na(ca))  { pq <- c(1, 1); txt <- "ARMA: ambas decaen. Parte de (1,1)" }
  else { pq <- c(cp, 0); txt <- sprintf("Ambas cortan. Prueba AR(%d) y MA(%d)", cp, ca) }
  PQ <- c(0, 0); txt_s <- ""
  if (m > 1 && length(a) >= 2 * m) {
    sa <- abs(a[c(m, 2 * m)]) > ci; sp <- abs(p[c(m, 2 * m)]) > ci
    Q <- as.integer(sa[1] && !sa[2]); P <- as.integer(sp[1] && !sp[2])
    if (P == 1 && Q == 1) P <- 0   # prefiere MA estacional (modelo 'airline')
    PQ <- c(P, Q)
    txt_s <- sprintf(" | Estacional: P=%d, Q=%d", P, Q)
  }
  cat("  Sugerencia:", txt, txt_s, "\n")
  list(p = pq[1], q = pq[2], P = PQ[1], Q = PQ[2])
}
ord <- sugerir_orden(x_id, m)
abrir_fig("04_identificacion", 1100, 500)
op <- par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
Acf(x_id,  lag.max = min(3 * m, length(x_id) / 3, 48), main = "ACF para identificar")
Pacf(x_id, lag.max = min(3 * m, length(x_id) / 3, 48), main = "PACF para identificar")
par(op); cerrar_fig(); guia("acf_pacf")
decidir("usa esta sugerencia como candidato manual -> PASO 5")


# =====================================================================
# PASO 5. ESTIMACIÓN
# Se fijan d y D para que el AICc sea comparable entre modelos.
# (El AICc NO compara modelos con distinto d, D o transformación.)
# =====================================================================
paso("PASO 5. Estimación")

# Deriva (tendencia):
#   d+D = 1 -> deriva si la media de la serie diferenciada != 0.
#   d+D = 0 -> deriva si hay tendencia determinista (regresión con errores ARMA).
usar_drift <- if (d + D == 1) {
  length(x) > 5 && t.test(as.numeric(x))$p.value < ALFA
} else if (d + D == 0) con_tend_final else FALSE
mk_spec <- function(p, q, P, Q, K = 0)
  list(p = p, d = d, q = q, P = if (K > 0) 0 else P, D = if (K > 0) 0 else D,
       Q = if (K > 0) 0 else Q, K = K, drift = usar_drift, mean = (d + D == 0))
fit_spec <- function(s, yy = y) {
  xr_ <- if (s$K > 0) fourier(yy, K = s$K) else NULL
  Arima(yy, order = c(s$p, s$d, s$q), seasonal = c(s$P, s$D, s$Q), xreg = xr_,
        lambda = lambda, include.drift = s$drift, include.mean = s$mean)
}
fc_spec <- function(s, fit, yy, h, ...) {
  xr_ <- if (s$K > 0) fourier(yy, K = s$K, h = h) else NULL
  forecast(fit, h = h, xreg = xr_, ...)
}
ajustar <- function(s) tryCatch(fit_spec(s), error = function(e) NULL)
spec_de_fit <- function(f, K = 0) {
  a <- f$arma
  list(p = a[1], d = a[6], q = a[2], P = a[3], D = a[7], Q = a[4], K = K,
       drift = "drift" %in% names(coef(f)), mean = "intercept" %in% names(coef(f)))
}
cand <- list(); specs <- list()
add <- function(nm, s, f = NULL) {
  if (is.null(f)) f <- ajustar(s)
  if (!is.null(f)) { cand[[nm]] <<- f; specs[[nm]] <<- s }
}
f_auto <- auto.arima(y, d = d, D = D, lambda = lambda, seasonal = (m > 1),
                     stepwise = FALSE, approximation = FALSE, ic = "aicc")
add("auto", spec_de_fit(f_auto), f_auto)
if (K_best > 0) {
  f_aF <- auto.arima(y, d = d, D = 0, lambda = lambda, seasonal = FALSE,
                     xreg = fourier(y, K = K_best), stepwise = FALSE,
                     approximation = FALSE, ic = "aicc")
  add(sprintf("auto_fourier_K%d", K_best), spec_de_fit(f_aF, K_best), f_aF)
}
add("manual_sugerido", mk_spec(ord$p, ord$q, ord$P, ord$Q, K_best))
add("AR1",    mk_spec(1, 0, ord$P, ord$Q, K_best))
add("MA1",    mk_spec(0, 1, ord$P, ord$Q, K_best))
add("ARMA11", mk_spec(1, 1, ord$P, ord$Q, K_best))
tabla <- data.frame(modelo = names(cand),
                    AICc = sapply(cand, `[[`, "aicc"),
                    BIC  = sapply(cand, `[[`, "bic"),
                    sigma2 = sapply(cand, `[[`, "sigma2"))
tabla <- tabla[order(tabla$AICc), ]
print(tabla, row.names = FALSE, digits = 5)
decidir("se prueba cada candidato, del menor al mayor AICc, en el PASO 6")


# =====================================================================
# PASO 6. DIAGNÓSTICO DE RESIDUALES
#   Ljung-Box (H0: residuales independientes)       -> ajuste del modelo
#   t de media cero                                  -> sesgo
#   Jarque-Bera / Shapiro-Wilk (H0: normalidad)      -> intervalos
#   ARCH-LM y McLeod-Li (H0: sin efecto ARCH)        -> paso 7
#   Breusch-Pagan sobre tiempo (H0: varianza const.) -> embudo
# =====================================================================
paso("PASO 6. Diagnóstico de residuales")

diagnostico <- function(fit, alfa = ALFA) {
  a <- fit$arma                                   # p, q, P, Q, m, d, D
  k <- sum(a[1:4]); dd <- a[6]; DD <- a[7]; mm <- a[5]
  e <- as.numeric(residuals(fit))
  skip <- dd + mm * DD                            # arranque por diferencias
  if (skip > 0) e <- e[-seq_len(skip)]
  ne <- length(e)
  L <- if (mm > 1) min(2 * mm, floor(ne / 5)) else min(10, floor(ne / 5))
  L <- max(L, k + 3)
  lb  <- Box.test(e,   lag = L, type = "Ljung-Box", fitdf = k)
  ml  <- Box.test(e^2, lag = L, type = "Ljung-Box")        # McLeod-Li
  al  <- arch_lm(e, lags = min(12, floor(ne / 5)))
  tm  <- t.test(e, mu = 0)
  jb  <- jarque.bera.test(e)
  sw  <- if (ne <= 5000) shapiro.test(e) else NULL
  bp  <- bptest(lm(e ~ seq_along(e)))
  res <- list(e = e, k = k, L = L, lb = lb, ml = ml, arch = al, tmedia = tm,
              jb = jb, sw = sw, bp = bp,
              ok_autocorr = lb$p.value > alfa,
              ok_arch     = (al$p.value > alfa) && (ml$p.value > alfa),
              ok_normal   = jb$p.value > alfa,
              ok_media    = tm$p.value > alfa,
              ok_varianza = bp$p.value > alfa)
  res
}
resumen_diag <- function(dg, nombre = "") {
  cat(sprintf("  [%s] LB p=%s | media p=%s | JB p=%s | SW p=%s | ARCH-LM p=%s | McLeod-Li p=%s | BP p=%s\n",
              nombre, p_fmt(dg$lb$p.value), p_fmt(dg$tmedia$p.value), p_fmt(dg$jb$p.value),
              if (is.null(dg$sw)) "NA" else p_fmt(dg$sw$p.value),
              p_fmt(dg$arch$p.value), p_fmt(dg$ml$p.value), p_fmt(dg$bp$p.value)))
}

graficar_residuales <- function(dg, fit, archivo = "06_residuales") {
  e <- dg$e; ne <- length(e)
  abrir_fig(archivo, 1300, 1000)
  op <- par(mfrow = c(2, 4), mar = c(4, 4, 3, 1))
  plot(e, type = "l", main = "Residuales vs tiempo", ylab = "e")
  abline(h = c(-2, 0, 2) * sd(e), lty = c(2, 1, 2), col = c("red", "black", "red"))
  Acf(e,  lag.max = min(36, ne / 4), main = "ACF de residuales")
  Pacf(e, lag.max = min(36, ne / 4), main = "PACF de residuales")
  hist(e, breaks = 20, freq = FALSE, main = "Histograma + normal", xlab = "e")
  curve(dnorm(x, mean(e), sd(e)), add = TRUE, col = "red", lwd = 2)
  qqnorm(e, main = "QQ-plot"); qqline(e, col = "red")
  Acf(e^2, lag.max = min(36, ne / 4), main = "ACF de residuales^2 (ARCH)")
  lmax <- min(24, floor(ne / 4)); lags <- (dg$k + 1):lmax
  pv <- sapply(lags, function(l) Box.test(e, lag = l, type = "Ljung-Box", fitdf = dg$k)$p.value)
  plot(lags, pv, ylim = c(0, 1), pch = 19, xlab = "Rezago",
       ylab = "p-valor", main = "Ljung-Box por rezago")
  abline(h = ALFA, lty = 2, col = "red")
  plot(tail(as.numeric(fitted(fit)), ne), e, main = "Residuales vs ajustados",
       xlab = "Ajustado", ylab = "e")
  abline(h = 0, col = "red")
  par(op); cerrar_fig()
  # Raíces inversas del AR y MA (plot.Arima de forecast).
  abrir_fig(paste0(archivo, "_raices"), 700, 600)
  try(plot(fit), silent = TRUE)
  cerrar_fig()
  guia("residuales"); guia("raices")
}

fit_final <- NULL; diag_final <- NULL
for (nm in tabla$modelo) {
  f <- cand[[nm]]; dg <- diagnostico(f)
  resumen_diag(dg, nm)
  if (dg$ok_autocorr) {
    fit_final <- f; diag_final <- dg; nombre_final <- nm; spec_final <- specs[[nm]]
    decidir(sprintf("'%s' pasa Ljung-Box (p=%s). Residuales ~ ruido blanco.",
                    nm, p_fmt(dg$lb$p.value)))
    break
  } else decidir(sprintf("'%s' NO pasa Ljung-Box. Se prueba el siguiente candidato.", nm))
}

## PASO 6B. Búsqueda ampliada si ningún candidato pasó --------------------
if (is.null(fit_final)) {
  paso("PASO 6B. Búsqueda ampliada de órdenes")
  grid <- expand.grid(p = 0:3, q = 0:3, P = 0:(m > 1 && K_best == 0),
                      Q = 0:(m > 1 && K_best == 0))
  grid <- grid[(grid$p + grid$q) > 0 | (grid$P + grid$Q) > 0, ]
  sp_l <- lapply(seq_len(nrow(grid)), function(i)
    mk_spec(grid$p[i], grid$q[i], grid$P[i], grid$Q[i], K_best))
  ajs <- lapply(sp_l, function(sp) suppressWarnings(ajustar(sp)))
  ok  <- !vapply(ajs, is.null, logical(1))
  ajs <- ajs[ok]; grid <- grid[ok, ]; sp_l <- sp_l[ok]
  aic <- vapply(ajs, `[[`, numeric(1), "aicc")
  for (i in order(aic)) {
    dg <- diagnostico(ajs[[i]])
    if (dg$ok_autocorr) {
      fit_final <- ajs[[i]]; diag_final <- dg; spec_final <- sp_l[[i]]
      nombre_final <- sprintf("grid(%d,%d,%d,%d)", grid$p[i], grid$q[i], grid$P[i], grid$Q[i])
      resumen_diag(dg, nombre_final)
      decidir(sprintf("%s pasa Ljung-Box -> continúa", nombre_final)); break
    }
  }
}
if (is.null(fit_final)) {
  fit_final <- cand[[tabla$modelo[1]]]; diag_final <- diagnostico(fit_final)
  spec_final <- specs[[tabla$modelo[1]]]
  nombre_final <- paste0(tabla$modelo[1], " (SIN pasar Ljung-Box)")
  decidir(paste("Ningún ARIMA deja residuales blancos. Hay estructura que ARIMA no captura.",
                "Revisa cambios estructurales (PASO 8), intervención (PASO 9) y regresión con",
                "errores ARMA (PASOS 9-10)."))
}
cat("\n  MODELO FINAL:", nombre_final, "\n"); print(fit_final)
cat("\n  Significancia de coeficientes (Wald z):\n"); print(coeftest(fit_final))
cat("  Si un coeficiente no es significativo, simplifica el modelo (parsimonia).\n")
graficar_residuales(diag_final, fit_final)

## Ramas de decisión después de Ljung-Box ---------------------------------
if (!diag_final$ok_media)
  decidir("la media de residuales != 0. Revisa deriva/constante (PASO 5).")
if (!diag_final$ok_normal)
  decidir("no hay normalidad. Los intervalos usarán bootstrap en el PASO 11.")
if (!diag_final$ok_arch || FORZAR_GARCH) {
  decidir("hay efecto ARCH en los residuales -> PASO 7 (GARCH)")
  IR_A_GARCH <- TRUE
} else {
  decidir("sin efecto ARCH. Se omite el PASO 7 -> PASO 8")
  IR_A_GARCH <- FALSE
}


# =====================================================================
# PASO 7. MODELOS ARCH / GARCH (Engle 1982; Bollerslev 1986)
# Se modela la varianza condicional de los residuales del ARIMA.
# Variantes: sGARCH, gjrGARCH (Glosten et al. 1993), eGARCH (Nelson 1991).
# Distribuciones: normal y t de Student.
# Enfoque en dos etapas: ARIMA primero, GARCH sobre sus residuales.
# =====================================================================
fit_garch <- NULL; sigma_h <- NULL
if (IR_A_GARCH) {
  paso("PASO 7. ARCH / GARCH")
  e <- diag_final$e
  # 7.1 Orden ARCH preliminar con PACF de e^2.
  abrir_fig("07a_pacf_cuadrados")
  Pacf(e^2, lag.max = 20, main = "PACF de residuales^2 (orden ARCH)")
  cerrar_fig()
  cat("  Regla: la PACF de e^2 corta en q -> ARCH(q). Si decae, usa GARCH(1,1).\n")

  if (TIENE_RUGARCH) {
    suppressPackageStartupMessages(library(rugarch))
    # 7.2 Rejilla de modelos. Se elige el de menor BIC que pase los chequeos.
    rejilla <- expand.grid(modelo = c("sGARCH", "gjrGARCH", "eGARCH"),
                           dist = c("norm", "std"),
                           orden = c("1,1", "1,2", "2,1"), stringsAsFactors = FALSE)
    ajustes <- list(); filas <- list()
    for (i in seq_len(nrow(rejilla))) {
      go <- as.integer(strsplit(rejilla$orden[i], ",")[[1]])
      spec <- ugarchspec(variance.model = list(model = rejilla$modelo[i], garchOrder = go),
                         mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                         distribution.model = rejilla$dist[i])
      f <- tryCatch(ugarchfit(spec, data = e, solver = "hybrid"), error = function(e) NULL)
      if (is.null(f) || f@fit$convergence != 0) next
      z  <- as.numeric(residuals(f, standardize = TRUE))
      p1 <- Box.test(z,   lag = 10, type = "Ljung-Box")$p.value
      p2 <- Box.test(z^2, lag = 10, type = "Ljung-Box")$p.value
      p3 <- arch_lm(z, 10)$p.value
      id <- paste(rejilla$modelo[i], rejilla$dist[i], rejilla$orden[i])
      ajustes[[id]] <- f
      filas[[id]] <- data.frame(modelo = id, BIC = infocriteria(f)["Bayes", 1],
                                LB_z = p1, LB_z2 = p2, ARCHLM_z = p3,
                                pasa = all(c(p1, p2, p3) > ALFA))
    }
    tg <- do.call(rbind, filas); tg <- tg[order(tg$BIC), ]
    print(tg, row.names = FALSE, digits = 4)
    ok <- tg[tg$pasa, ]
    elegido <- if (nrow(ok)) ok$modelo[1] else tg$modelo[1]
    if (nrow(ok)) decidir(sprintf("GARCH elegido: %s (pasa los chequeos).", elegido))
    else decidir(sprintf("Ningún GARCH pasa todo. Se usa el de menor BIC: %s. Revisa quiebres.", elegido))
    fit_garch <- ajustes[[elegido]]
    show(fit_garch)
    # 7.3 Gráficos: volatilidad condicional y residuales estandarizados.
    abrir_fig("07b_garch", 1100, 800)
    op <- par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
    plot(as.numeric(sigma(fit_garch)), type = "l", main = "Volatilidad condicional",
         ylab = "sigma_t", xlab = "t")
    z <- as.numeric(residuals(fit_garch, standardize = TRUE))
    plot(z, type = "l", main = "Residuales estandarizados", ylab = "z")
    Acf(z^2, main = "ACF de z^2 (debe ser ruido)")
    qqnorm(z, main = "QQ de z"); qqline(z, col = "red")
    par(op); cerrar_fig()
    guia("garch")
    sigma_h <- as.numeric(sigma(ugarchforecast(fit_garch, n.ahead = H)))
  } else {
    # Respaldo sin rugarch: tseries::garch (solo GARCH(p,q) normal).
    cat("  rugarch no está instalado. install.packages('rugarch') da más modelos.\n")
    cat("  Respaldo: tseries::garch GARCH(1,1) y ARCH(1).\n")
    res <- list(garch11 = tseries::garch(e, order = c(1, 1), trace = FALSE),
                arch1   = tseries::garch(e, order = c(0, 1), trace = FALSE))
    bics <- sapply(res, function(f) { ll <- as.numeric(logLik(f))
                                      -2 * ll + length(coef(f)) * log(length(e)) })
    print(round(bics, 2))
    fit_garch <- res[[which.min(bics)]]
    print(summary(fit_garch))
    z <- as.numeric(residuals(fit_garch)); z <- z[is.finite(z)]
    cat(sprintf("  Chequeo z: LB(z) p=%s | LB(z^2) p=%s\n",
                p_fmt(Box.test(z, 10, "Ljung-Box")$p.value),
                p_fmt(Box.test(z^2, 10, "Ljung-Box")$p.value)))
    cf <- coef(fit_garch); sig2 <- tail(fit_garch$fitted.values[, 1]^2, 1)
    uncond <- unname(cf[1] / (1 - sum(cf[-1])))     # varianza incondicional
    ph <- if (is.finite(uncond) && uncond > 0) uncond else var(e)
    sigma_h <- rep(sqrt(ph), H)                     # aproximación plana
    cat("  Nota: con tseries la banda del pronóstico usa varianza incondicional.\n")
    abrir_fig("07b_garch_tseries")
    plot(ts(fit_garch$fitted.values[, 1], start = start(y), frequency = m),
         main = "Volatilidad condicional (tseries)", ylab = "sigma_t")
    cerrar_fig()
  }
  decidir("GARCH listo -> PASO 8")
}


# =====================================================================
# PASO 8. CAMBIO ESTRUCTURAL
#   Bai-Perron (2003): varios quiebres de nivel y pendiente (BIC).
#   OLS-CUSUM (Brown-Durbin-Evans 1975; strucchange).
#   sup-F (Andrews 1993) y Chow (1960) para un quiebre.
# Se aplica a la serie desestacionalizada contra una tendencia lineal.
# =====================================================================
paso("PASO 8. Cambio estructural")

sa <- if (m > 1) as.numeric(seasadj(stl(y_t, s.window = "periodic"))) else as.numeric(y_t)
tt <- seq_along(sa)
bp  <- breakpoints(sa ~ tt, h = 0.15)
bpo <- breakpoints(bp)                        # número óptimo por BIC
quiebres <- if (all(is.na(bpo$breakpoints))) integer(0) else bpo$breakpoints
ef  <- efp(sa ~ tt, type = "OLS-CUSUM")
p_cusum <- sctest(ef)$p.value
fs  <- Fstats(sa ~ tt, from = 0.15)
p_supf <- sctest(fs, type = "supF")$p.value
cat(sprintf("  OLS-CUSUM p=%s | sup-F p=%s | quiebres Bai-Perron (BIC): %s\n",
            p_fmt(p_cusum), p_fmt(p_supf),
            if (length(quiebres)) paste(quiebres, collapse = ", ") else "ninguno"))
if (!is.null(FECHA_INTERVENCION)) {
  dec <- as.numeric(format(FECHA_INTERVENCION, "%Y")) +
         (as.numeric(format(FECHA_INTERVENCION, "%j")) - 1) / 365.25
  t0 <- which(time(y) >= dec - 0.5 / m)[1]       # primer periodo >= fecha
  chow <- tryCatch(sctest(sa ~ tt, type = "Chow", point = t0), error = function(e) NULL)
  if (!is.null(chow)) cat(sprintf("  Chow en t0=%d (%s): p=%s\n", t0, FECHA_INTERVENCION, p_fmt(chow$p.value)))
}
abrir_fig("08_cambio_estructural", 1100, 500)
op <- par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
plot(sa, type = "l", main = "Serie desest. + quiebres (Bai-Perron)", ylab = "", xlab = "t")
if (length(quiebres)) {
  lines(fitted(bpo), col = "red", lwd = 2); abline(v = quiebres, lty = 2, col = "blue")
}
plot(ef, main = "OLS-CUSUM")
par(op); cerrar_fig(); guia("cusum")

hay_quiebre <- length(quiebres) > 0 || p_cusum < ALFA || p_supf < ALFA
if (hay_quiebre) {
  decidir("hay evidencia de cambio estructural -> PASO 9 (ITS) obligatorio")
  if (is.null(FECHA_INTERVENCION) && length(quiebres)) {
    cat("  Sin intervención conocida. El quiebre detectado es EXPLORATORIO.\n")
    cat("  Una fecha elegida con los datos no permite inferencia causal.\n")
    t0 <- quiebres[1]
  }
} else decidir("sin cambio estructural. Se omite el PASO 9 si no hay intervención -> PASO 10/11")


# =====================================================================
# PASO 9. SERIES DE TIEMPO INTERRUMPIDAS (ITS)
# Regresión segmentada (Bernal, Cummins y Gasparrini 2017):
#   y_t = b0 + b1*t + b2*interv + b3*post + estacionalidad + error
#   b2 = cambio de NIVEL inmediato; b3 = cambio de PENDIENTE.
# Flujo: OLS -> si hay autocorrelación -> GLS con errores ARMA
#        -> si aún falla -> ARIMA con regresores (Box-Tiao; Montgomery cap. 6; Wei cap. 10).
# =====================================================================
# --- Función GLS reutilizable (también la usa el PASO 10) ---------------
ajustar_gls <- function(formula, datos, pmax = 2, qmax = 2) {
  # Elige la estructura corARMA(p,q) con menor AIC (ajuste ML), luego REML.
  res <- list(); tabla <- data.frame(p = integer(), q = integer(), AIC = numeric())
  for (p in 0:pmax) for (q in 0:qmax) {
    if (p == 0 && q == 0) next
    f <- tryCatch(gls(formula, data = datos, method = "ML",
                      correlation = corARMA(p = p, q = q, form = ~ t)),
                  error = function(e) NULL)
    if (is.null(f)) next
    tabla <- rbind(tabla, data.frame(p = p, q = q, AIC = AIC(f)))
    res[[paste(p, q)]] <- f
  }
  if (!nrow(tabla)) return(NULL)
  tabla <- tabla[order(tabla$AIC), ]
  b <- tabla[1, ]
  fit <- tryCatch(gls(formula, data = datos, method = "REML",
                      correlation = corARMA(p = b$p, q = b$q, form = ~ t)),
                  error = function(e) res[[paste(b$p, b$q)]])
  list(fit = fit, p = b$p, q = b$q, tabla = tabla)
}
tabla_coef <- function(fit) {
  if (inherits(fit, "gls")) {
    s <- summary(fit)$tTable; df <- fit$dims$N - fit$dims$p
    est <- s[, 1]; se <- s[, 2]
  } else {
    s <- summary(fit)$coefficients; df <- fit$df.residual
    est <- s[, 1]; se <- s[, 2]
  }
  tq <- qt(0.975, df)
  data.frame(coef = rownames(s), est = est, ee = se, inf95 = est - tq * se,
             sup95 = est + tq * se, p = 2 * pt(-abs(est / se), df), row.names = NULL)
}

fit_its <- NULL
if (!is.null(FECHA_INTERVENCION) || (hay_quiebre && exists("t0"))) {
  paso("PASO 9. Series de tiempo interrumpidas")
  if (!exists("t0") || is.na(t0)) stop("No se pudo ubicar la fecha de intervención en la serie.")
  tt <- seq_len(n)
  d_its <- data.frame(y = as.numeric(y), t = tt,
                      interv = as.integer(tt >= t0),
                      post   = pmax(0, tt - t0))
  cat(sprintf("  Intervención en t0 = %d (%s). Periodos pre: %d, post: %d\n",
              t0, round(time(y)[t0], 3), t0 - 1, n - t0 + 1))
  if (t0 - 1 < 8 || n - t0 + 1 < 8) cat("  AVISO: pocos puntos pre o post. Bernal sugiere >= 8 por segmento.\n")

  # 9.1 Estacionalidad con términos de Fourier. K se elige por AIC.
  if (m > 1) {
    Kmax <- min(5, floor((m - 1) / 2))
    four <- function(K, t) { M <- NULL
      for (k in 1:K) M <- cbind(M, sin(2 * pi * k * t / m), cos(2 * pi * k * t / m))
      colnames(M) <- paste0(rep(c("sen", "cos"), K), rep(1:K, each = 2)); M }
    aics <- sapply(1:Kmax, function(K) AIC(lm(y ~ ., data = cbind(d_its[, c("y", "t", "interv", "post")],
                                                                  as.data.frame(four(K, tt))))))
    K <- which.min(aics); decidir(sprintf("K de Fourier por AIC = %d", K))
    d_its <- cbind(d_its, as.data.frame(four(K, tt)))
  }
  regs <- setdiff(names(d_its), "y")
  f_its <- as.formula(paste("y ~", paste(regs, collapse = " + ")))

  # 9.1b Regresores para ARIMA con errores (Box-Tiao), incluye el futuro.
  # escalon = cambio de nivel; rampa = cambio de pendiente.
  tt_all <- seq_len(n + H)
  X_all <- cbind(tendencia = tt_all, escalon = as.integer(tt_all >= t0),
                 rampa = pmax(0, tt_all - t0))
  if (m > 1) X_all <- cbind(X_all, four(K, tt_all))
  fit_arima_its <- tryCatch(
    auto.arima(y, xreg = X_all[1:n, ], seasonal = FALSE, stepwise = FALSE,
               approximation = FALSE, ic = "aicc"),
    error = function(e) NULL)

  # 9.2 OLS y chequeo de autocorrelación (Durbin-Watson, Breusch-Godfrey, Ljung-Box).
  ols <- lm(f_its, data = d_its)
  dw  <- dwtest(ols); bg <- bgtest(ols, order = min(12, floor(n / 6)))
  lbo <- Box.test(residuals(ols), lag = min(12, floor(n / 5)), type = "Ljung-Box")
  cat(sprintf("  OLS: DW=%.2f (p=%s) | Breusch-Godfrey p=%s | Ljung-Box p=%s\n",
              dw$statistic, p_fmt(dw$p.value), p_fmt(bg$p.value), p_fmt(lbo$p.value)))
  ok_ols <- bg$p.value > ALFA && lbo$p.value > ALFA

  if (ok_ols) {
    decidir("sin autocorrelación residual. OLS es válido. Se reportan errores Newey-West por prudencia.")
    fit_its <- ols; tipo_its <- "OLS"
    print(coeftest(ols, vcov = NeweyWest(ols, prewhite = FALSE)))
  } else {
    decidir("hay autocorrelación en OLS -> GLS con errores ARMA")
    g <- ajustar_gls(f_its, d_its)
    if (!is.null(g)) {
      cat(sprintf("  GLS: corARMA(p=%d, q=%d) elegido por AIC\n", g$p, g$q))
      r_n <- residuals(g$fit, type = "normalized")
      lbg <- Box.test(r_n, lag = min(12, floor(n / 5)), type = "Ljung-Box", fitdf = g$p + g$q)
      cat(sprintf("  GLS residuales normalizados: Ljung-Box p=%s\n", p_fmt(lbg$p.value)))
      if (lbg$p.value > ALFA) {
        decidir("GLS deja residuales blancos. Se usa GLS.")
        fit_its <- g$fit; tipo_its <- sprintf("GLS corARMA(%d,%d)", g$p, g$q)
      }
    }
    if (is.null(fit_its)) {
      decidir("GLS no basta. Se usa ARIMA con regresores (Box-Tiao).")
      fit_its <- fit_arima_its; tipo_its <- "ARIMA con xreg"
    }
  }
  cat("\n  Modelo ITS final:", tipo_its, "\n")
  if (!inherits(fit_its, "Arima")) {
    tc <- tabla_coef(fit_its); print(tc, digits = 4, row.names = FALSE)
    b <- setNames(tc$est, tc$coef)
    cat(sprintf("\n  Cambio de NIVEL inmediato (interv): %.3f\n", b["interv"]))
    cat(sprintf("  Cambio de PENDIENTE por periodo (post): %.3f\n", b["post"]))
    cat(sprintf("  Efecto acumulado a %d periodos del inicio: %.3f\n", H, b["interv"] + b["post"] * H))
    # Contrafactual: mismo modelo sin intervención.
    d_cf <- d_its; d_cf$interv <- 0; d_cf$post <- 0
    cf <- as.numeric(predict(fit_its, newdata = d_cf))
    aj <- as.numeric(predict(fit_its, newdata = d_its))
    tm <- as.numeric(time(y))     # as.numeric evita lines.ts
    abrir_fig("09_its", 1100, 600)
    plot(tm, as.numeric(y), pch = 16, col = "grey50", xlab = "Tiempo", ylab = "y",
         main = paste("ITS:", tipo_its))
    lines(tm, aj, col = "blue", lwd = 2)
    lines(tm[t0:n], cf[t0:n], col = "red", lwd = 2, lty = 2)
    abline(v = tm[t0], lty = 3)
    legend("topleft", c("Observado", "Ajustado", "Contrafactual"),
           col = c("grey50", "blue", "red"), pch = c(16, NA, NA), lty = c(NA, 1, 2), bty = "n")
    cerrar_fig(); guia("its")
    # Residuales del ITS.
    abrir_fig("09b_its_residuales", 1100, 500)
    r_its <- if (inherits(fit_its, "gls")) residuals(fit_its, type = "normalized") else residuals(fit_its)
    op <- par(mfrow = c(1, 2)); Acf(r_its, main = "ACF residuales ITS"); Pacf(r_its, main = "PACF residuales ITS"); par(op)
    cerrar_fig()
  } else {
    b <- coef(fit_its); print(coeftest(fit_its))
    cat(sprintf("\n  Nivel (escalon): %.3f | Pendiente (rampa): %.3f\n", b["escalon"], b["rampa"]))
  }
  # 9.3 ARIMA con regresores de intervención: modelo para PRONOSTICAR.
  # GLS sirve para inferir. ARIMA+xreg propaga el error ARMA al futuro.
  if (!is.null(fit_arima_its)) {
    cat("\n  ARIMA con regresores de intervención (para pronóstico):\n")
    print(fit_arima_its)
    ka <- sum(fit_arima_its$arma[1:2])
    cat(sprintf("  Ljung-Box residuales p=%s\n", p_fmt(Box.test(residuals(fit_arima_its),
        lag = min(12, floor(n / 5)), type = "Ljung-Box", fitdf = ka)$p.value)))
    fc_its <- forecast(fit_arima_its, xreg = X_all[(n + 1):(n + H), ], h = H)
    abrir_fig("09c_pronostico_its", 1100, 600)
    plot(fc_its, main = "Pronóstico ARIMA + intervención (Box-Tiao)")
    cerrar_fig()
  }
  # Nota para conteos: usa glm(family = quasipoisson) con offset(log(poblacion))
  # y términos de Fourier. Ver Bernal et al. (2017).
  # Nota para efectos temporales: agrega un pulso (t == t0) o un decaimiento exponencial.
} else {
  cat("\n  PASO 9 omitido: no hay intervención ni quiebre detectado.\n")
}


# =====================================================================
# PASO 10. OPCIÓN EXTRA: REGRESIÓN CON MÍNIMOS CUADRADOS GENERALIZADOS (GLS)
# TL;DR:
#   OLS supone errores independientes y varianza constante.
#   Si los errores se autocorrelacionan, OLS es ineficiente y sus errores
#   estándar son falsos (p-valores demasiado optimistas).
#   GLS modela Var(e) = sigma^2 * V. Con V de un AR(p) o ARMA(p,q)
#   se estiman beta y la estructura de correlación a la vez.
#   Úsalo cuando el objetivo es INFERIR (tendencia, efecto de un regresor).
#   Para PRONOSTICAR, usa Arima(..., xreg=) que propaga el error ARMA.
# Flujo: OLS -> DW/BG -> si falla -> GLS (AR1 -> ARMA) -> Ljung-Box normalizado.
# Referencias: Montgomery cap. 3 y 6; Wei cap. 15; Pinheiro y Bates (2000).
# =====================================================================
paso("PASO 10. Regresión GLS (opción extra)")

d_gls <- data.frame(y = as.numeric(y), t = seq_len(n))
if (m > 1) d_gls$mes <- factor(cycle(y))
f_gls <- if (m > 1) y ~ t + mes else y ~ t
ols0 <- lm(f_gls, data = d_gls)
dw0 <- dwtest(ols0); bg0 <- bgtest(ols0, order = min(12, floor(n / 6)))
cat(sprintf("  OLS tendencia+estacional: DW=%.2f, BG p=%s\n", dw0$statistic, p_fmt(bg0$p.value)))
if (bg0$p.value > ALFA) {
  decidir("sin autocorrelación. OLS basta -> PASO 11")
} else {
  decidir("autocorrelación presente -> GLS corARMA")
  g0 <- ajustar_gls(f_gls, d_gls)
  if (!is.null(g0)) {
    cat(sprintf("  Estructura elegida: ARMA(%d,%d)\n", g0$p, g0$q))
    tc0 <- tabla_coef(g0$fit); print(tc0[tc0$coef %in% c("(Intercept)", "t"), ], digits = 4, row.names = FALSE)
    cat(sprintf("  Tendencia (OLS) = %.4f vs GLS = %.4f. Compara los errores estándar.\n",
                coef(ols0)["t"], coef(g0$fit)["t"]))
    cat(sprintf("  EE tendencia: OLS=%.4f | GLS=%.4f\n",
                summary(ols0)$coefficients["t", 2], summary(g0$fit)$tTable["t", 2]))
  }
}


# =====================================================================
# PASO 11. PRONÓSTICO Y VALIDACIÓN
# Montgomery cap. 2 y 5: valida con origen móvil y compara con benchmarks.
# =====================================================================
paso("PASO 11. Pronóstico y validación")

# 11.1 Pronóstico con intervalos. Bootstrap si no hay normalidad.
usar_boot <- !diag_final$ok_normal
fc <- fc_spec(spec_final, fit_final, y, H, level = c(80, 95),
              bootstrap = usar_boot, npaths = 2000)
abrir_fig("11_pronostico", 1100, 600)
plot(fc, main = sprintf("Pronóstico: %s%s", nombre_final, if (usar_boot) " (bootstrap)" else ""))
cerrar_fig()
print(fc)
guia_pron <- c("Intervalos que se ensanchan con h es lo esperado.",
               "Pronóstico plano en serie con tendencia -> falta deriva o d.",
               "Intervalos muy estrechos con residuales no blancos -> subestiman la incertidumbre.")
cat("\n  GUIA RAPIDA [pronostico]\n"); cat(paste0("   - ", guia_pron), sep = "\n")

# 11.2 Si hay GARCH, bandas con sigma condicional.
if (!is.null(sigma_h)) {
  mu_tr <- if (is.null(lambda)) as.numeric(fc$mean) else BoxCox(as.numeric(fc$mean), lambda)
  zc <- qnorm(0.975)
  lo <- mu_tr - zc * sigma_h; hi <- mu_tr + zc * sigma_h
  if (!is.null(lambda)) { lo <- InvBoxCox(lo, lambda); hi <- InvBoxCox(hi, lambda) }
  cat("\n  Bandas 95% con varianza GARCH (dos etapas):\n")
  print(data.frame(h = 1:H, media = as.numeric(fc$mean), inf95 = lo, sup95 = hi), digits = 4)
}

# 11.3 Validación de origen móvil contra benchmarks (Montgomery cap. 2).
# En cada origen se reajusta el modelo con los datos hasta ese punto.
cat("\n  Validación de origen móvil. Puede tardar unos segundos.\n")
cv_origen <- function(fun, y, h, initial, paso_o = 3) {
  nn <- length(y); E <- matrix(NA_real_, 0, h)
  for (o in seq(initial, nn - 1, by = paso_o)) {
    ytr <- window(y, end = time(y)[o])
    fcv <- tryCatch(fun(ytr, h), error = function(e) rep(NA_real_, h))
    real <- as.numeric(y)[(o + 1):min(o + h, nn)]
    err <- rep(NA_real_, h); err[seq_along(real)] <- real - fcv[seq_along(real)]
    E <- rbind(E, err)
  }
  sqrt(colMeans(E^2, na.rm = TRUE))
}
f_arima <- function(z, h) as.numeric(fc_spec(spec_final, fit_spec(spec_final, z), z, h)$mean)
f_ets   <- function(z, h) as.numeric(forecast(ets(z), h = h)$mean)
f_naive <- function(z, h) as.numeric(if (m > 1) snaive(z, h = h)$mean else naive(z, h = h)$mean)
hcv <- min(H, 12); ini <- max(floor(n * 0.6), 3 * m + 2)
tiene_its <- exists("fit_arima_its") && !is.null(fit_arima_its) && exists("t0")
if (tiene_its) ini <- max(ini, t0 + 8)    # origen común: hay datos post-intervención
cv <- data.frame(h = 1:hcv,
                 ARIMA = cv_origen(f_arima, y, hcv, ini),
                 ETS = cv_origen(f_ets, y, hcv, ini),
                 Benchmark = cv_origen(f_naive, y, hcv, ini))
if (tiene_its) {
  ord_its <- fit_arima_its$arma[c(1, 6, 2)]
  f_its_cv <- function(z, h) {
    o <- length(z)
    ft <- Arima(z, order = ord_its, xreg = X_all[1:o, ], include.mean = TRUE)
    as.numeric(forecast(ft, xreg = X_all[o + 1:h, , drop = FALSE], h = h)$mean)
  }
  cv$ARIMA_ITS <- cv_origen(f_its_cv, y, hcv, ini)
}
print(round(cv, 3), row.names = FALSE)
abrir_fig("11b_cv", 900, 600)
nc <- ncol(cv) - 1
matplot(cv$h, cv[, -1], type = "b", pch = 1:nc, col = 1:nc, lty = 1, xlab = "Horizonte",
        ylab = "RMSE", main = "RMSE por horizonte (origen móvil)")
legend("topleft", names(cv)[-1], col = 1:nc, pch = 1:nc, lty = 1, bty = "n")
cerrar_fig()
mejor <- names(which.min(colMeans(cv[, -1], na.rm = TRUE)))
decidir(sprintf("menor RMSE medio: %s. Si el benchmark gana, el modelo complejo no aporta.", mejor))
if (tiene_its && mejor == "ARIMA_ITS")
  cat("  El ARIMA con intervención pronostica mejor. Úsalo si el quiebre persiste.\n")


# =====================================================================
# PASO 12. OPCIÓN EXTRA: PROPHET (META) PARA SERIES SEMANALES
# Taylor y Letham (2018): y(t) = g(t) + s(t) + h(t) + e(t)
#   g = tendencia con puntos de cambio, s = Fourier, h = feriados.
# Para datos semanales se APAGA la estacionalidad semanal y diaria.
# Siempre compara con ARIMA + términos de Fourier y con un benchmark.
# =====================================================================
if (EJECUTAR_PROPHET && TIENE_PROPHET) {
  paso("PASO 12. Prophet para series semanales")
  suppressPackageStartupMessages(library(prophet))

  # 12.1 Datos semanales. Reemplaza por los tuyos: data.frame(ds = Date, y = numeric).
  nw <- 208
  ds <- seq(as.Date("2021-01-04"), by = "week", length.out = nw)
  tw <- seq_len(nw)
  yw <- 200 + 0.5 * tw + 25 * sin(2 * pi * tw / 52.18) + 10 * cos(4 * pi * tw / 52.18) +
        ifelse(tw > 130, -0.4 * (tw - 130), 0) + as.numeric(arima.sim(list(ar = 0.5), nw, sd = 6))
  df <- data.frame(ds = ds, y = yw)
  H_sem <- 26
  corte <- nw - H_sem
  train <- df[1:corte, ]; test <- df[(corte + 1):nw, ]

  # 12.2 Modelo. Parámetros clave:
  #   changepoint.prior.scale: más alto = tendencia más flexible (default 0.05).
  #   seasonality.prior.scale: más alto = estacionalidad más flexible (default 10).
  #   seasonality.mode: "multiplicative" si la amplitud crece con el nivel.
  #   yearly.seasonality: número de términos de Fourier (default 10).
  mp <- prophet(train, growth = "linear",
                yearly.seasonality = 8, weekly.seasonality = FALSE, daily.seasonality = FALSE,
                seasonality.mode = "additive",
                changepoint.prior.scale = 0.05, n.changepoints = 25,
                interval.width = 0.95)
  # Regresores extra (p. ej. intervención): df$interv <- ...; mp <- add_regressor(mp, "interv")
  # Feriados: mp <- add_country_holidays(mp, country_name = "CO")
  futuro <- make_future_dataframe(mp, periods = H_sem, freq = "week")
  pred <- predict(mp, futuro)

  abrir_fig("12_prophet", 1100, 600)
  print(plot(mp, pred) + ggplot2::ggtitle("Prophet: ajuste y pronóstico semanal"))
  cerrar_fig()
  abrir_fig("12b_prophet_componentes", 1000, 800)
  prophet_plot_components(mp, pred)
  cerrar_fig()
  guia("prophet")

  # 12.3 Validación en el conjunto de prueba contra ARIMA + Fourier y snaive.
  p_test <- tail(pred$yhat, H_sem)
  yw_ts <- ts(train$y, frequency = 365.25 / 7)
  Kf <- 6
  ax <- auto.arima(yw_ts, xreg = fourier(yw_ts, K = Kf), seasonal = FALSE)
  f_ax <- forecast(ax, xreg = fourier(yw_ts, K = Kf, h = H_sem))
  f_sn <- snaive(ts(train$y, frequency = 52), h = H_sem)
  met <- function(real, pr) c(RMSE = sqrt(mean((real - pr)^2)), MAE = mean(abs(real - pr)),
                              MAPE = 100 * mean(abs((real - pr) / real)))
  comp <- rbind(Prophet = met(test$y, p_test),
                ARIMA_Fourier = met(test$y, as.numeric(f_ax$mean)),
                SNaive = met(test$y, as.numeric(f_sn$mean)))
  print(round(comp, 2))
  decidir(sprintf("mejor modelo semanal por RMSE: %s", rownames(comp)[which.min(comp[, "RMSE"])]))

  # 12.4 Validación cruzada nativa de Prophet (ventana expansiva). Costosa: opcional.
  # dcv <- cross_validation(mp, initial = 730, period = 90, horizon = 180, units = "days")
  # print(performance_metrics(dcv)); plot_cross_validation_metric(dcv, metric = "rmse")
} else if (EJECUTAR_PROPHET) {
  cat("\n  PASO 12 omitido: instala prophet con install.packages('prophet').\n")
}

paso("FIN DEL PIPELINE")
if (GUARDAR_FIG) cat("  Figuras guardadas en:", normalizePath(DIR_SALIDA), "\n")
