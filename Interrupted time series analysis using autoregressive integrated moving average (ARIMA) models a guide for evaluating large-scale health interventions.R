#################################################################################
# Project: 	Code for "Interrupted time series analysis using Autoregressive #
#           Integrated Moving Average (ARIMA) models: A guide for evaluating 	#
#	          large-scale health interventions"				#
# Author:		Dr Andrea Schaffer					#
#           Centre for Big Data Research in Health                              #
#           UNSW Sydney								#
# Last updated 2 July 2020			                                #
#################################################################################

library(astsa)
library(forecast)
library(dplyr)
library(zoo)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load data
quet <- read.csv(file = '12874_2021_1235_MOESM1_ESM.csv')

# Convert data to time series object
quet.ts <- ts(quet[,2], frequency=12, start=c(2011,1))

# View data
quet.ts

# Plot data to visualise time series
options(scipen=5)
plot(quet.ts, ylim=c(0,40000), type='l', col="blue", xlab="Month", ylab="Dispensings")
# Add vertical line indicating date of intervention (January 1, 2014)
abline(v=2014, col="gray", lty="dashed", lwd=2)

# View ACF/PACF plots of undifferenced data
acf2(quet.ts, max.lag=24)

# View ACF/PACF plots of differenced/seasonally differenced data
acf2(diff(diff(quet.ts,12)), max.lag=24)

# Create variable representing step change and view
step <- as.numeric(as.yearmon(time(quet.ts))>='ene. 2014')
step

# Create variable representing ramp (change in slope) and view
ramp <- append(rep(0,36), seq(1,12,1))
ramp  

# Use automated algorithm to identify p/q parameters
# Specify first difference = 1 and seasonal difference = 1
model1 <- auto.arima(quet.ts, seasonal=TRUE, xreg=cbind(step,ramp), max.d=1, max.D=1, stepwise=FALSE, trace=TRUE)

# Check residuals
checkresiduals(model1)
Box.test(model1$residuals, lag = 24, type = "Ljung-Box")

# Estimate parameters and confidence intervals
summary(model1)
confint(model1)

# To forecast the counterfactual, model data excluding post-intervention time period
model2 <- Arima(window(quet.ts, end=c(2013,12)), order=c(2,1,0), seasonal=list(order=c(0,1,1), period=12))

# Forecast 12 months post-intervention and convert to time series object
fc <- forecast(model2, h=12)
fc.ts <- ts(as.numeric(fc$mean), start=c(2014,1), frequency=12)

# Combine with observed data
quet.ts.2 <- ts.union(quet.ts, fc.ts)
quet.ts.2

# Plot
plot(quet.ts.2, type="l", plot.type="s", col=c('blue','red'), xlab="Month", ylab="Dispensings", linetype=c("solid","dashed"), ylim=c(0,40000))
abline(v=2014, lty="dashed", col="gray")
