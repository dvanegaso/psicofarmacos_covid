# contrafactual con auto.arima

rm(list = ls())

library(dplyr)
library(readr)
library(forecast)
library(ggplot2)

#
# v0.1: contrafactual con auto.arima (candidatos Fourier y SARIMA), validacion por origen movil,
#       resumen del efecto a 6, 12 y 24 meses desde T1 y graficos observado vs contrafactual

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

load("Lista Principios DHD.RData")

lista_principio = read.csv("salidas/base_DHD_final.csv")
lista_principio = names(table(lista_principio$principioactivo))

############################################################
######################## PARAMETROS ########################
############################################################

T_pre      = 38   # ultimo mes del ajuste (febrero de 2020, indice 1 = enero de 2017); marzo de 2020 queda fuera
T1         = 40   # primer mes completo de aislamiento obligatorio (abril de 2020)
T2         = 46   # primer mes de pandemia2 (octubre de 2020), solo para el grafico
T_fin      = 72   # ultimo mes del contrafactual (diciembre de 2022); hasta 108 si se amplia
K_fourier  = 3    # armonicos de fourier para la estacionalidad anual (2K parametros; K <= 6)
h_reporte  = c(6, 12, 24)  # meses desde T1; T1 cuenta como mes 1
origen_min = 24   # primer origen de la validacion por origen movil
h_val      = 6    # horizonte (meses) de cada pronostico en la validacion
nivel_ic   = 0.95
candidatos = c("fourier", "sarima")  # fourier: xreg + errores ARMA; sarima: D = 1

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

# serie mensual en escala log desde enero de 2017
serie_log = function(x) ts(as.numeric(x), start = c(2017, 1), frequency = 12)

# ajusta un candidato a la serie previa y (en escala log)
ajusta_candidato = function(y, indice, tipo) {
  
  if (tipo == "fourier") {
    auto.arima(y, xreg = terminos_fourier(indice, K_fourier), seasonal = FALSE,
               max.p = 2, max.q = 2, stepwise = FALSE, approximation = FALSE)
  } else {
    auto.arima(y, seasonal = TRUE, D = 1,
               max.p = 2, max.q = 2, max.P = 1, max.Q = 1,
               stepwise = FALSE, approximation = FALSE)
  }
}

# pronostico del candidato para los meses indice_nuevo
pronostica_candidato = function(modelo, tipo, indice_nuevo, nivel) {
  
  if (tipo == "fourier") {
    forecast(modelo, h = length(indice_nuevo), level = 100 * nivel,
             xreg = terminos_fourier(indice_nuevo, K_fourier))
  } else {
    forecast(modelo, h = length(indice_nuevo), level = 100 * nivel)
  }
}

# validacion por origen movil: ajusta con 1:o, pronostica h meses y mide el error en escala log
valida_candidato = function(y, tipo, origenes, h) {
  
  errores = lapply(origenes, function(o) {
    y_o = serie_log(y[1:o])
    m   = tryCatch(ajusta_candidato(y_o, 1:o, tipo), error = function(e) NULL)
    if (is.null(m)) return(rep(NA_real_, h))
    f = pronostica_candidato(m, tipo, (o + 1):(o + h), nivel_ic)
    as.numeric(y[(o + 1):(o + h)]) - as.numeric(f$mean)
  })
  
  E = do.call(rbind, errores)
  c(RMSE = sqrt(mean(E^2, na.rm = TRUE)), MAE = mean(abs(E), na.rm = TRUE))
}

# orden ARIMA como texto
orden_arima = function(m) {
  a = m$arma # p, q, P, Q, periodo, d, D
  sprintf("(%d,%d,%d)(%d,%d,%d)[%d]", a[1], a[6], a[2], a[3], a[7], a[4], a[5])
}

# grafico observado vs contrafactual
grafico_cf = function(dat, cf, titulo) {
  ggplot() +
    geom_ribbon(data = cf, aes(x = fecha, ymin = li, ymax = ls), fill = "firebrick", alpha = 0.2) +
    geom_line(data = cf, aes(x = fecha, y = contrafactual), colour = "firebrick", linetype = "dashed") +
    geom_line(data = dat, aes(x = fecha, y = DHDt)) +
    geom_vline(xintercept = dat$fecha[T1], linetype = "dotted") +
    geom_vline(xintercept = dat$fecha[T2], linetype = "dotted") +
    labs(title = titulo, x = NULL, y = "DHD",
         caption = "Negro: observado. Rojo: contrafactual con IC. Lineas: T1 y T2.") +
    theme_minimal()
}

############################################################
######################## MODELAMIENTO ######################
############################################################

modelo_cf  = list()
contrafact = list()
validacion = list()
resumen_h  = list()
info_modelo = list()

for (i in 1:length(lista_principio)) {
  
  cat(i, ":", lista_principio[i], "...\n")
  
  dat = as.data.frame(principio[[i]])
  dat = dat[order(dat$indice), ]
  stopifnot(nrow(dat) >= T_fin)
  dat = dat[1:T_fin, ]
  dat$fecha = as.Date(dat$fecha)
  
  y_pre = serie_log(log(dat$DHDt[1:T_pre]))
  
  # validacion por origen movil dentro del periodo previo
  origenes = origen_min:(T_pre - h_val)
  val = sapply(candidatos, function(tp) valida_candidato(y_pre, tp, origenes, h_val))
  mejor = candidatos[which.min(val["RMSE", ])]
  
  validacion[[i]] = data.frame(principio = lista_principio[i],
                               candidato = candidatos,
                               t(val),
                               elegido   = candidatos == mejor,
                               row.names = NULL)
  
  # ajuste final del candidato elegido con todo el periodo previo
  modelo.tmp = ajusta_candidato(y_pre, 1:T_pre, mejor)
  tmp.res    = as.numeric(residuals(modelo.tmp))
  
  # graficacion
  layout(matrix(c(1, 1, 1, 1, 2, 3, 4, 5), nrow = 2, byrow = TRUE))
  qqnorm(tmp.res, main = paste(lista_principio[i], "-", mejor)); qqline(tmp.res)
  ts.plot(cbind(as.numeric(y_pre), as.numeric(fitted(modelo.tmp))), col = c("black", "firebrick"))
  acf(tmp.res, main = "")
  pacf(tmp.res, main = lista_principio[i])
  plot(tmp.res, type = "l", main = "")
  layout(1)
  
  lb = Box.test(tmp.res, lag = 12, type = "Ljung-Box", fitdf = sum(modelo.tmp$arma[1:4]))
  
  cat("--------------------------------------------------------------\n",
      "Validacion (escala log):\n")
  print(round(val, 4))
  cat("Elegido:", mejor, orden_arima(modelo.tmp), "\n",
      "Lyung-Bx:", lb$p.value, "\n",
      "--------------------------------------------------------------\n")
  
  # contrafactual: pronostico del modelo previo sobre los meses posteriores
  idx_f = (T_pre + 1):T_fin
  f     = pronostica_candidato(modelo.tmp, mejor, idx_f, nivel_ic)
  
  # exp() de la media en log es una mediana, no una media; la diferencia es pequena
  cf = data.frame(indice        = idx_f,
                  fecha         = dat$fecha[idx_f],
                  observado     = dat$DHDt[idx_f],
                  contrafactual = exp(as.numeric(f$mean)),
                  li            = exp(as.numeric(f$lower[, 1])),
                  ls            = exp(as.numeric(f$upper[, 1])))
  
  # el observado se toma como fijo: el IC del efecto viene del IC del contrafactual
  cf$efecto    = cf$observado - cf$contrafactual
  cf$efecto_li = cf$observado - cf$ls
  cf$efecto_ls = cf$observado - cf$li
  cf$razon     = cf$observado / cf$contrafactual
  cf$razon_li  = cf$observado / cf$ls
  cf$razon_ls  = cf$observado / cf$li
  
  contrafact[[i]] = cbind(principio = lista_principio[i], cf)
  
  # efecto a h meses desde T1 (T1 cuenta como mes 1)
  resumen_h[[i]] = bind_rows(lapply(h_reporte, function(h) {
    fila = cf[cf$indice == T1 + h - 1, c("observado", "contrafactual", "efecto", "efecto_li",
                                         "efecto_ls", "razon", "razon_li", "razon_ls")]
    cbind(principio = lista_principio[i], meses_desde_T1 = h, fila)
  }))
  
  info_modelo[[i]] = data.frame(principio = lista_principio[i],
                                candidato = mejor,
                                orden     = orden_arima(modelo.tmp),
                                aicc      = modelo.tmp$aicc,
                                rmse_val  = min(val["RMSE", ]),
                                lb_p      = lb$p.value)
  
  modelo_cf[[i]] = modelo.tmp
}

names(modelo_cf)  = lista_principio
names(contrafact) = lista_principio
names(validacion) = lista_principio
names(resumen_h)  = lista_principio

layout(1)

############################################################
######################## RESUMENES #########################
############################################################

validacion_df  = bind_rows(validacion)
contrafact_df  = bind_rows(contrafact)
resumen_h_df   = bind_rows(resumen_h)
info_modelo_df = bind_rows(info_modelo)

print(info_modelo_df)
print(resumen_h_df)

write.csv(validacion_df,  "salidas/contrafactual_validacion.csv",  row.names = FALSE)
write.csv(contrafact_df,  "salidas/contrafactual_series.csv",      row.names = FALSE)
write.csv(resumen_h_df,   "salidas/contrafactual_resumen_h.csv",   row.names = FALSE)
write.csv(info_modelo_df, "salidas/contrafactual_modelos.csv",     row.names = FALSE)

############################################################
######################## GRAFICOS ##########################
############################################################

dir.create("graficos contrafactual", showWarnings = FALSE)

graficos_cf = lapply(1:length(lista_principio), function(i) {
  
  dat = as.data.frame(principio[[i]])
  dat = dat[order(dat$indice), ][1:T_fin, ]
  dat$fecha = as.Date(dat$fecha)
  
  grafico_cf(dat, contrafact[[i]], lista_principio[i])
})
names(graficos_cf) = lista_principio

for (g in lista_principio) {
  ggsave(filename = paste0("graficos contrafactual/", g, ".png"),
         plot = graficos_cf[[g]], width = 8, height = 4.5)
}
