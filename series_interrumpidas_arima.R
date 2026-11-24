# series interrumpidas: ARIMA con intervencion

rm(list = ls())

library(dplyr)
library(readr)
library(forecast)
library(ggplot2)

#
# v0.1: ARIMA con intervencion (pulso, bloque, escalon y rampa) con errores ARMA via auto.arima;
#       validacion con ventana ampliada e indicadora de desabastecimiento; graficos tipo forest

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

load("Lista Principios DHD.RData")

lista_principio = read.csv("salidas/base_DHD_final.csv")
lista_principio = names(table(lista_principio$principioactivo))

############################################################
######################## PARAMETROS ########################
############################################################

K_fourier = 3     # armonicos de fourier para la estacionalidad anual (2K parametros; K <= 6)
n_base    = 72    # ventana principal: enero de 2017 a diciembre de 2022
n_val     = 108   # ventana de validacion: hasta diciembre de 2025 (incluye desabastecimiento)
T_pulso   = 39    # marzo de 2020 (acaparamiento); NULL elimina el pulso
T_rampa   = 45    # igual que regresion_NW.R: pmax(indice - 45, 0), positiva desde octubre de 2020
nivel_ic  = 0.95
estacionalidad = "fourier"  # "fourier" (xreg + errores ARMA) o "sarima" (D = 1)

covariables = c("pulso_marzo", "pandemia1", "pandemia2", "rampa", "desabastecimiento")
if (is.null(T_pulso)) covariables = setdiff(covariables, "pulso_marzo")

############################################################
######################## FUNCIONES #########################
############################################################

# terminos de fourier de periodo 12 a partir del indice mensual
terminos_fourier = function(indice, K, periodo = 12) {
  
  stopifnot(K >= 1, K <= periodo / 2)
  
  S = lapply(1:K, function(k) {
    arg = 2 * pi * k * indice / periodo
    if (2 * k == periodo) {
      # en el armonico k = periodo/2 el seno es nulo: solo se usa el coseno
      out = cbind(cos(arg))
      colnames(out) = paste0("cos", k)
    } else {
      out = cbind(sin(arg), cos(arg))
      colnames(out) = paste0(c("sen", "cos"), k)
    }
    out
  })
  
  do.call(cbind, S)
}

# regresores de intervencion
#   pulso_marzo       : pulso en T_pulso
#   pandemia1         : bloque transitorio (abril a septiembre de 2020 segun la base)
#   pandemia2         : escalon desde octubre de 2020
#   rampa             : cambio de pendiente desde T_rampa
#   desabastecimiento : bloque (marzo a noviembre de 2023), solo en la validacion
regresores = function(dat, desabastecimiento = FALSE) {
  
  X = cbind(pandemia1 = as.numeric(dat$pandemia1),
            pandemia2 = as.numeric(dat$pandemia2),
            rampa     = pmax(dat$indice - T_rampa, 0))
  
  if (!is.null(T_pulso)) X = cbind(pulso_marzo = as.numeric(dat$indice == T_pulso), X)
  if (desabastecimiento) X = cbind(X, desabastecimiento = as.numeric(dat$desabastecimiento))
  
  X
}

# log(DHDt) con errores ARMA y regresores de intervencion
ajusta_its = function(dat, X, estacionalidad) {
  
  y = ts(log(dat$DHDt), start = c(2017, 1), frequency = 12)
  
  if (estacionalidad == "fourier") {
    auto.arima(y, xreg = cbind(X, terminos_fourier(dat$indice, K_fourier)), seasonal = FALSE,
               max.p = 2, max.q = 2, stepwise = FALSE, approximation = FALSE)
  } else {
    auto.arima(y, xreg = X, seasonal = TRUE, D = 1,
               max.p = 2, max.q = 2, max.P = 1, max.Q = 1,
               stepwise = FALSE, approximation = FALSE)
  }
}

# coeficientes (escala log) con IC bajo normalidad asintotica
coef_arima = function(modelo, nivel = 0.95) {
  
  est = coef(modelo)
  se  = sqrt(diag(modelo$var.coef))
  z   = qnorm(1 - (1 - nivel) / 2)
  
  out = data.frame(est - z * se, est, est + z * se,
                   row.names = names(est), check.names = FALSE)
  colnames(out) = c(paste0(100 * (1 - nivel) / 2, "%"), "estimacion",
                    paste0(100 * (1 + nivel) / 2, "%"))
  
  out
}

# tabla con error estandar, z y p-valor (escala log)
prueba_arima = function(modelo) {
  
  est = coef(modelo)
  se  = sqrt(diag(modelo$var.coef))
  z   = est / se
  
  data.frame(estimacion = est, error_est = se, z = z, p_valor = 2 * pnorm(-abs(z)),
             row.names = names(est))
}

# orden ARIMA como texto
orden_arima = function(m) {
  a = m$arma # p, q, P, Q, periodo, d, D
  sprintf("(%d,%d,%d)(%d,%d,%d)[%d]", a[1], a[6], a[2], a[3], a[7], a[4], a[5])
}

# graficos y pruebas de los residuales
diagnostico = function(modelo, nombre) {
  
  tmp.res = as.numeric(residuals(modelo))
  
  layout(matrix(c(1, 1, 1, 1, 2, 3, 4, 5), nrow = 2, byrow = TRUE))
  qqnorm(tmp.res, main = nombre); qqline(tmp.res)
  ts.plot(cbind(as.numeric(modelo$x), as.numeric(fitted(modelo))), col = c("black", "firebrick"))
  acf(tmp.res, main = "", na.action = na.pass)
  pacf(tmp.res, main = nombre, na.action = na.pass)
  plot(tmp.res, type = "l", main = "")
  layout(1)
  
  cat("--------------------------------------------------------------\n",
      nombre, orden_arima(modelo), "\n",
      "Pruebas de Normalidad de Residuales:\n",
      "Shapiro:", shapiro.test(tmp.res)$p, "\n",
      "anderso:", nortest::ad.test(tmp.res)$p, "\n",
      "lillief:", nortest::lillie.test(tmp.res)$p, "\n",
      "JrqBera:", tseries::jarque.bera.test(tmp.res)[3]$p.value, "\n",
      "Pruebas de Autocorrelación de Residuales:\n",
      "Lyug-Bx:", Box.test(tmp.res, lag = 12, type = "Ljung-Box",
                           fitdf = sum(modelo$arma[1:4]))$p.value, "\n",
      "Momentos de los Residuales:\n",
      "Asimetría:", moments::skewness(tmp.res), "\n",
      "Kurtosis :", moments::kurtosis(tmp.res), "\n",
      "--------------------------------------------------------------\n")
}

############################################################
######################## MODELAMIENTO ######################
############################################################

modelo_base = vector("list", length(lista_principio))
modelo_val  = vector("list", length(lista_principio))

for (i in 1:length(lista_principio)) {
  
  cat(i, ":", lista_principio[i], "...\n")
  
  dat = as.data.frame(principio[[i]])
  dat = dat[order(dat$indice), ]
  dat$fecha = as.Date(dat$fecha)
  stopifnot(nrow(dat) >= n_base)
  
  # modelo base: sin desabastecimiento (queda fuera de la ventana)
  dat_b = dat[1:n_base, ]
  m_b   = ajusta_its(dat_b, regresores(dat_b), estacionalidad)
  diagnostico(m_b, paste(lista_principio[i], "- base"))
  modelo_base[[i]] = m_b
  
  # modelo de validacion: ventana ampliada con indicadora de desabastecimiento
  # (la ventana ampliada necesita datos al menos hasta noviembre de 2023, indice 83)
  if (nrow(dat) >= 84) {
    dat_v = dat[1:min(n_val, nrow(dat)), ]
    m_v   = ajusta_its(dat_v, regresores(dat_v, desabastecimiento = TRUE), estacionalidad)
    diagnostico(m_v, paste(lista_principio[i], "- validacion"))
    modelo_val[[i]] = m_v
  } else {
    cat("Sin datos hasta el indice 84: se omite la validacion de", lista_principio[i], "\n")
  }
}

names(modelo_base) = lista_principio
names(modelo_val)  = lista_principio
modelo_val_ok = Filter(Negate(is.null), modelo_val)

layout(1)

############################################################
################ COEFICIENTES E INTERVALOS #################
############################################################

# listas de data frames: una fila por coeficiente y columnas 2.5%, estimacion, 97.5% (escala log)
coef_its_base = lapply(modelo_base,   coef_arima, nivel = nivel_ic)
coef_its_val  = lapply(modelo_val_ok, coef_arima, nivel = nivel_ic)

# tablas con error estandar, z y p-valor
prueba_its_base = lapply(modelo_base,   prueba_arima)
prueba_its_val  = lapply(modelo_val_ok, prueba_arima)

############################################################
############ COEFICIENTES EXPONENCIADOS (RAZONES) ##########
############################################################

# el modelo es log(DHDt): exp(b) es la razon multiplicativa sobre DHDt
coef_exp_base = lapply(coef_its_base, exp)
coef_exp_val  = lapply(coef_its_val,  exp)

fila_ic = function(tb, v, p, modelo) {
  if (!is.null(tb) && v %in% rownames(tb)) {
    data.frame(principio = p, modelo = modelo,
               li = tb[v, 1], estimacion = tb[v, 2], ls = tb[v, 3])
  } else {
    # la covariable no entro al modelo de ese principio
    data.frame(principio = p, modelo = modelo,
               li = NA_real_, estimacion = NA_real_, ls = NA_real_)
  }
}

# un data frame por covariable: una fila por principio activo y modelo
ic_covariables = lapply(covariables, function(v) {
  
  filas = lapply(lista_principio, function(p) {
    bind_rows(fila_ic(coef_exp_base[[p]], v, p, "Base"),
              fila_ic(coef_exp_val[[p]],  v, p, "Validación"))
  })
  
  out = bind_rows(filas)
  
  # significativo si el IC de la razon excluye 1 (equivale a excluir 0 en log)
  out$significativo = factor(ifelse(out$li > 1 | out$ls < 1, "Sí", "No"),
                             levels = c("No", "Sí"))
  
  out$modelo    = factor(out$modelo, levels = c("Base", "Validación"))
  out$principio = factor(out$principio, levels = rev(lista_principio))
  
  out
})
names(ic_covariables) = covariables

# tabla resumen de efectos (razon y cambio porcentual)
efectos_its = bind_rows(ic_covariables, .id = "covariable") %>%
  mutate(cambio_pct    = 100 * (estimacion - 1),
         cambio_pct_li = 100 * (li - 1),
         cambio_pct_ls = 100 * (ls - 1))

info_its = bind_rows(
  lapply(lista_principio, function(p) {
    bind_rows(
      if (!is.null(modelo_base[[p]])) data.frame(principio = p, modelo = "Base",
                                                 orden = orden_arima(modelo_base[[p]]),
                                                 aicc  = modelo_base[[p]]$aicc),
      if (!is.null(modelo_val[[p]]))  data.frame(principio = p, modelo = "Validación",
                                                 orden = orden_arima(modelo_val[[p]]),
                                                 aicc  = modelo_val[[p]]$aicc))
  }))

print(info_its)

write.csv(efectos_its, "salidas/its_arima_efectos.csv", row.names = FALSE)
write.csv(info_its,    "salidas/its_arima_modelos.csv", row.names = FALSE)

############################################################
################ GRAFICOS DE INTERVALOS ####################
############################################################

titulos = c(
  "pulso_marzo"       = "Pulso marzo de 2020 (acaparamiento)",
  "pandemia1"         = "Pandemia (fase 1)",
  "pandemia2"         = "Pandemia (fase 2)",
  "rampa"             = "Cambio de pendiente desde el mes 45 (razon mensual)",
  "desabastecimiento" = "Desabastecimiento"
)

grafico_ic = function(df, titulo) {
  
  ggplot(df, aes(y = principio, x = estimacion, xmin = li, xmax = ls, colour = modelo)) +
    geom_vline(xintercept = 1, linetype = "dashed") +
    geom_errorbar(width = 0.3, na.rm = TRUE, position = position_dodge(width = 0.6)) +
    geom_point(size = 2, na.rm = TRUE, position = position_dodge(width = 0.6)) +
    scale_x_log10() +
    scale_colour_manual(values = c("Base" = "firebrick", "Validación" = "steelblue"), drop = FALSE) +
    labs(title = titulo,
         x = "Razon exp(b) con IC 95% (ARIMA con intervencion, escala log)",
         y = NULL,
         colour = "Modelo") +
    theme_minimal()
}

graficos_ic = lapply(covariables, function(v) grafico_ic(ic_covariables[[v]], titulos[[v]]))
names(graficos_ic) = covariables

dir.create("graficos ARIMA intervencion", showWarnings = FALSE)

for (v in covariables) {
  ggsave(filename = paste0("graficos ARIMA intervencion/", titulos[[v]], ".png"),
         plot = graficos_ic[[v]], width = 7, height = 6)
}

# opcional: todas las covariables en un solo panel
ic_todas = dplyr::bind_rows(ic_covariables, .id = "covariable")
ic_todas$covariable = factor(titulos[ic_todas$covariable], levels = titulos[covariables])

grafico_ic(ic_todas, "Intervalos de confianza por covariable") +
  facet_wrap(~ covariable, scales = "free_x")
