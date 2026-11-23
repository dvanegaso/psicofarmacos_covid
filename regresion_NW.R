# analisis de series temporales

rm(list = ls())

library(dplyr)
library(stringr)
library(tidyr)
library(tidytext)
library(readr)
library(tsibble)
library(sandwich)
library(lmtest)
library(ggplot2)

#
# v0.6: coeficientes exponenciados, data frame de IC por covariable y graficos tipo forest
# v0.5: Newey-West con lm() + sandwich (gls no es compatible con sandwich);
#       se elimina el calculo manual de la matriz NW y la matriz de diseno
# v0.4: terminos de fourier, gls (nlme), qqnorm y lista de coeficientes con Newey-West
# v0.3: adicion de indicadoras para analiss
# v0.2: verificacion de la base con otros scripts
# v0.1: cargado y graficacion

# para

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

load("Lista Principios DHD.RData")

lista_principio = read.csv("salidas/base_DHD_final.csv")
lista_principio = names(table(lista_principio$principioactivo))

############################################################
######################## PARAMETROS ########################
############################################################

K_fourier = 3    # armonicos de fourier para la estacionalidad anual (2K parametros; K <= 6)
L_nw      = NULL # rezago de Newey-West; NULL usa floor(4 * (T / 100)^(2 / 9))
delta_eq  = NULL # margen de relevancia en escala log (ej. log(1.10) = +/-10%); NULL no dibuja +/-delta

covariables = c("pandemia1TRUE",
                "pandemia2TRUE",
                "desabastecimientoTRUE",
                "I(pmax(indice - 45, 0))")

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

# rezago recomendado para Newey-West
rezago_nw = function(n) floor(4 * (n / 100)^(2 / 9))

# matriz de covarianza Newey-West de un lm (pesos de Bartlett, sin prewhitening)
vcov_nw = function(modelo, L = NULL) {
  if (is.null(L)) L = rezago_nw(nobs(modelo))
  sandwich::NeweyWest(modelo, lag = L, prewhite = FALSE, adjust = TRUE)
}

# coeficientes (escala del modelo) con IC 95% bajo errores estandar Newey-West
# coefci() usa la t con df.residual(modelo) grados de libertad
coef_newey_west = function(modelo, L = NULL, nivel = 0.95) {
  
  V  = vcov_nw(modelo, L)
  ic = lmtest::coefci(modelo, vcov. = V, level = nivel)
  
  out = data.frame(ic[, 1], coef(modelo), ic[, 2],
                   row.names = names(coef(modelo)), check.names = FALSE)
  colnames(out) = c(paste0(100 * (1 - nivel) / 2, "%"), "estimacion",
                    paste0(100 * (1 + nivel) / 2, "%"))
  
  out
}

############################################################
######################## MODELAMIENTO ######################
############################################################

modelo_init = list() # gls (nlme)
modelo_lm   = list() # lm equivalente: base para sandwich / lmtest / skedastic

for (i in 1:length(lista_principio)) {
  
  cat(i, ":",lista_principio[i],":\n")
  
  cat(lista_principio[i], "...")
  
  dat = as.data.frame(principio[[i]])
  dat = dat[order(dat$indice), ]
  
  # estacionalidad por terminos de fourier (reemplaza factor(mes))
  fourier.tmp = terminos_fourier(dat$indice, K_fourier)
  dat = cbind(dat, fourier.tmp)
  
  fml.tmp = as.formula(paste(
    "log(DHDt) ~ indice +",
    paste(colnames(fourier.tmp), collapse = " + "),
    "+ pandemia1 + pandemia2 + desabastecimiento",
    "+ I(pmax(indice - 45, 0))" # mes 45 (septiembre de 2020 si indice 1 = enero de 2017)
  ))
  
  modelo.tmp = nlme::gls(
    model = fml.tmp,
    data = dat,
    correlation = NULL, # sin estructura corARMA
    na.action = na.omit
  )
  
  # lm con la misma formula y filas: sandwich, bgtest, bptest y skedastic no aceptan gls
  # (es identico al gls mientras no se use estructura de correlacion)
  lm.tmp = lm(fml.tmp, data = dat, na.action = na.omit)
  
  tmp.res = as.numeric(residuals(modelo.tmp))
  
  # graficación
  layout(matrix(c(1, 1, 1, 1, 2, 3, 4,5), nrow = 2, byrow = TRUE))
  qqnorm(tmp.res, main = ""); qqline(tmp.res, col = "red")
  ts.plot(principio[[i]][,c(1:3)])
  acf(tmp.res, main = "");
  pacf(tmp.res, main = lista_principio[i]);
  plot(tmp.res, main = "")
  
  layout(1)
  
  cat("--------------------------------------------------------------\n",
      "Pruebas de Normalidad de Residuales:\n",
      "Shapiro:", shapiro.test(tmp.res)$p, "\n",
      "anderso:", nortest::ad.test(tmp.res)$p, "\n",
      "lillief:", nortest::lillie.test(tmp.res)$p, "\n",
      "JrqBera:", tseries::jarque.bera.test(tmp.res)[3]$p.value, "\n ",
      "------------------------------------------------------------\n",
      "Pruebas de Autocorrelación de Residuales:\n",
      "DurbinW:", car::durbinWatsonTest(tmp.res),"\n",
      "Breusch:", lmtest::bgtest(lm.tmp)$p.value,"\n",
      "Lyug-Bx:", Box.test(tmp.res, type = "Ljung-Box")[3]$p.value,"\n",
      "-------------------------------------------------------------\n",
      "Pruebas de Heterocedasticidad de Residuales:\n",
      "Bre-Pgn:", lmtest::bptest(lm.tmp)$p.value, "\n",
      "White  :", skedastic::white(lm.tmp)$p.value, "\n",
      "GoldQunt:", skedastic::goldfeld_quandt(lm.tmp)$p.value, "\n",
      "-------------------------------------------------------------\n",
      "Momentos de los Residuales:\n",
      "Asimetría:", moments::skewness(tmp.res),"\n",
      "Kurtosis :", moments::kurtosis(tmp.res),"\n \n \n",
      "-------------------------------------------------------------\n"
  )
  
  # print(summary(modelo.tmp))
  
  modelo_init[[i]] = modelo.tmp
  modelo_lm[[i]]   = lm.tmp
}

names(modelo_init) = lista_principio
names(modelo_lm)   = lista_principio

layout(1)

############################################################
################ MODELAMIENTO NEWEY-WEST ###################
############################################################

# verificacion: gls y lm deben dar los mismos coeficientes
# (si se agrega corARMA al gls esto deja de cumplirse y NW sobre el lm ya no aplica al gls)
chequeo_gls_lm = sapply(lista_principio, function(p) {
  max(abs(coef(modelo_init[[p]]) - coef(modelo_lm[[p]])))
})
cat("Diferencia maxima de coeficientes gls vs lm:", max(chequeo_gls_lm), "\n")
stopifnot(max(chequeo_gls_lm) < 1e-8)

# lista de data frames: una fila por coeficiente y columnas 2.5%, estimacion, 97.5%
coef_nw = lapply(modelo_lm, coef_newey_west, L = L_nw)

# lista de tablas con error estandar, t y p-valor bajo Newey-West (escala log)
prueba_nw = lapply(modelo_lm, function(m) lmtest::coeftest(m, vcov. = vcov_nw(m, L_nw)))

############################################################
############ COEFICIENTES EXPONENCIADOS (RAZONES) ##########
############################################################

# el modelo es log(DHDt): exp(b) es la razon multiplicativa sobre DHDt
# exp() es monotona, por eso exp(IC log) es el IC de la razon
coef_nw_exp = lapply(coef_nw, exp)

# un data frame por covariable: una fila por principio activo
ic_covariables = lapply(covariables, function(v) {
  
  filas = lapply(lista_principio, function(p) {
    tb = coef_nw_exp[[p]]
    if (v %in% rownames(tb)) {
      data.frame(principio = p, li = tb[v, 1], estimacion = tb[v, 2], ls = tb[v, 3])
    } else {
      # la covariable no entro al modelo de ese principio
      data.frame(principio = p, li = NA_real_, estimacion = NA_real_, ls = NA_real_)
    }
  })
  
  out = do.call(rbind, filas)
  rownames(out) = NULL
  
  # significativo si el IC de la razon excluye 1 (equivale a excluir 0 en log)
  out$significativo = factor(ifelse(out$li > 1 | out$ls < 1, "Sí", "No"),
                             levels = c("No", "Sí"))
  
  # orden alfabetico de arriba hacia abajo en el grafico
  out$principio = factor(out$principio, levels = rev(lista_principio))
  
  out
})
names(ic_covariables) = covariables

############################################################
################ GRAFICOS DE INTERVALOS ####################
############################################################

titulos = c(
  "pandemia1TRUE"           = "Pandemia (fase 1)",
  "pandemia2TRUE"           = "Pandemia (fase 2)",
  "desabastecimientoTRUE"   = "Desabastecimiento",
  "I(pmax(indice - 45, 0))" = "Cambio de pendiente desde el mes 45 (razon mensual)"
)

grafico_ic = function(df, titulo, delta = NULL) {
  
  ggplot(df, aes(y = principio, x = estimacion, xmin = li, xmax = ls,
                 colour = significativo)) +
    geom_vline(xintercept = 1, linetype = "dashed") +
    {if (!is.null(delta)) geom_vline(xintercept = exp(c(-delta, delta)), linetype = "dotted")} +
    geom_errorbar(width = 0.3, na.rm = TRUE) +
    geom_point(size = 2, na.rm = TRUE) +
    scale_x_log10() +
    scale_colour_manual(values = c("No" = "grey50", "Sí" = "firebrick"), drop = FALSE) +
    labs(title = titulo,
         x = "Razon exp(b) con IC 95% Newey-West (escala log)",
         y = NULL,
         colour = "Significacion\nestadistica") +
    theme_minimal()
}

graficos_ic = lapply(covariables, function(v) grafico_ic(ic_covariables[[v]], titulos[[v]], delta_eq))
names(graficos_ic) = covariables

for (g in graficos_ic) print(g)

# opcional: las 4 covariables en un solo panel
ic_todas = dplyr::bind_rows(ic_covariables, .id = "covariable")
ic_todas$covariable = factor(titulos[ic_todas$covariable], levels = titulos)

grafico_ic(ic_todas, "Intervalos de confianza por covariable", delta_eq) +
  facet_wrap(~ covariable, scales = "free_x")