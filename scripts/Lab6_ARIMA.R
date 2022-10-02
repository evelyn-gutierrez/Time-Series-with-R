#:::::::::::::::::::::::::::::::::::::
# ARIMA
#:::::::::::::::::::::::::::::::::::::
library(fpp2) # Cargar la librería necesaria


# Preparación de la serie ----
y <- uschange[,"Consumption"]  # Indices de consumo Trimestrales


# Exploración:

y %>% autoplot()

y %>% ggAcf()
y %>% ggPacf()



#=================================
# CASO II:  COMPARANDO VARIOS ARIMA
#=================================


fit <- auto.arima(SerieY, seasonal=FALSE)
summary(fit)  # ARIMA(1,0,3) - No diferencia, AR(1), MA(3)
fit$aic  #AIC=343.3296

fit %>% forecast(h=10) %>% autoplot()

fit2 <- fit.arima(SerieY, order=c(1,0,2) )
summary(fit2)  # ARIMA(1,0,2) 
fit2$aic  # AIC=343.3296

