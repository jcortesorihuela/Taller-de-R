# install.packages("tseries")
# install.packages("forecast")
# install.packages("PerformanceAnalytics")

library(tseries)
library(forecast)
library(PerformanceAnalytics)

#setwd("D:/Diploma_Pronostico_Demanda/clase5")
setwd("C:/Users/gmrad/Dropbox/FEN/Seminarios/Pronostico de demanda/R")

###################################
########### DATA TURISMO ##########
###################################

data_turismo = read.csv(file="turismo.csv", header=TRUE, sep=",")

T=dim(data_turismo)[1]

####Total######

turistas=ts(data_turismo[,1],frequency = 12, start = c(2008,1))
plot(turistas,xlab="Tiempo",ylab="Turistas")

log_turistas=log(turistas)
plot(log_turistas,xlab="Tiempo",ylab="log(Turistas)")

diff12=diff(log_turistas,lag=12)
plot(diff12)
ndiffs(diff12)

plot(diff12,xlab="tiempo",ylab="d12log(turistas)")

plot(diff(diff12,lag=1),xlab="tiempo",ylab="Diferencia de d12log(turistas)")

################### Mejor modelo en base a BIC ##################
modelo_turistas_bic=auto.arima(log_turistas, 
                               d=1,D=1, max.p = 6, max.q = 6, max.P = 6,
                               max.Q = 6, ic="bic", allowdrift=TRUE, approximation=FALSE, stepwise=FALSE)
summary(modelo_turistas_bic)
plot(forecast(modelo_turistas_bic, h=100))

################### Mejor modelo en base a AIC ##################
modelo_turistas_aic=auto.arima(log_turistas, 
                               d=1,D=1, max.p = 6, max.q = 6, max.P = 6,
                               max.Q = 6, ic="aic", allowdrift=TRUE, approximation=FALSE, stepwise=FALSE)
summary(modelo_turistas_aic)
plot(forecast(modelo_turistas_aic, h=50))


################### Datos de entrenamiento y datos de prueba ##################

data_train=window(log_turistas,start=c(2008,01),end=c(2018,12))
data_test=window(log_turistas,start=c(2019,01),end=c(2019,09))

modelo_sarima=auto.arima(data_train, d=1,D=1, max.p = 6, max.q = 6, max.P = 6,
                         max.Q = 6, ic="bic", allowdrift=TRUE, approximation=FALSE, stepwise=FALSE)

prediccion_sarima=forecast(modelo_sarima, h=9)$mean

modelo_arima=auto.arima(data_train, d=1,D=1, max.p = 6, max.q =6, max.P = 0,
                        max.Q = 0, ic="bic", allowdrift=TRUE, approximation=FALSE, stepwise=FALSE)

prediccion_arima=forecast(modelo_arima, h=9)$mean

modelo_ar=auto.arima(data_train, d=1,D=1, max.p = 6, max.q =0, max.P = 0,
                     max.Q = 0, ic="bic", allowdrift=TRUE, approximation=FALSE, stepwise=FALSE)

prediccion_ar=forecast(modelo_ar, h=9)$mean

modelo_ma=auto.arima(data_train, d=1,D=1, max.p = 0, max.q =6, max.P = 0,
                     max.Q = 0, ic="bic", allowdrift=TRUE, approximation=FALSE, stepwise=FALSE)

prediccion_ma=forecast(modelo_ma, h=9)$mean

modelo_sarma=auto.arima(data_train, d=0,D=1, max.p = 6, max.q = 6, max.P = 6,
                        max.Q = 6, ic="bic", allowdrift=TRUE, approximation=FALSE, stepwise=FALSE)

prediccion_sarma=forecast(modelo_sarma, h=9)$mean

cbind("Real"=data_test, 
      "SARIMA"=prediccion_sarima, 
      "ARIMA"=prediccion_arima, 
      "SAR"=prediccion_ar, 
      "SMA"=prediccion_ma, 
      "SARMA"=prediccion_sarma)

ecmp_sarima=mean((prediccion_sarima-data_test)^2)
ecmp_arima=mean((prediccion_arima-data_test)^2)
ecmp_ar=mean((prediccion_ar-data_test)^2)
ecmp_ma=mean((prediccion_ma-data_test)^2)
ecmp_sarma=mean((prediccion_sarma-data_test)^2)

c("SARIMA"=ecmp_sarima,
  "ARIMA"=ecmp_arima,
  "SAR"=ecmp_ar,
  "SMA"=ecmp_ma,
  "SARMA"=ecmp_sarma)












