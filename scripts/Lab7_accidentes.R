#:::::::::::::::::::::::::::::::::::::::::::::::::::::
# Cargar DATOS:
#:::::::::::::::::::::::::::::::::::::::::::::::::::::
dt <- read.csv("https://www.datosabiertos.gob.pe/sites/default/files/Accidentes%20de%20tr%C3%A1nsito%20en%20carreteras-2020-2021-Sutran.csv", sep=";")
DTaccidentes <- data.frame(table(dt$FECHA))
names(DTaccidentes) <- c("fecha","accidentes")

#:::::::::::::::::::::::::::::::::::::::::::::::::::::
# Convertir fecha 
#:::::::::::::::::::::::::::::::::::::::::::::::::::::
DTaccidentes$fecha <- as.Date(DTaccidentes$fecha,"%Y%m%d")

# Indice del día:
DTaccidentes$t <- DTaccidentes$fecha - min(DTaccidentes$fecha)
DTaccidentes$t <- as.numeric(DTaccidentes$t) + 1
head(DTaccidentes)


#:::::::::::::::::::::::::::::::::::::::::::::::::::::
# Crear serie temporal.
#:::::::::::::::::::::::::::::::::::::::::::::::::::::
y <- ts(DTaccidentes$accidentes,
        frequency = 7)
 
y %>% autoplot()

y %>% ggAcf()
 


#::::::::::::::::::::::::::
# Modelo ARIMA
#::::::::::::::::::::::::::

# Modelo automático ARIMA:
modelo.arima1 <- auto.arima(y)
modelo.arima1
#AICc=3736.24

modelo.arima1 %>% checkresiduals()

modelo.arima1 %>% forecast(h=14) %>% autoplot()





# Ajustando manualmente un modelo ARIMA(2,1,1)
modelo.arima2 <- Arima(y, order=c(2,1,1))
modelo.arima2
# AICc=3746.29

modelo.arima2 %>% checkresiduals()
# Los residuales no pueden ser considerados ruido blanco. 
# No usar este modelo.




# Ajustando manualmente un modelo ARIMA(1,1,1)(0,0,2)[7] 
modelo.arima3 <- Arima(y, order=c(1,1,1), seasonal = c(0,0,2))
modelo.arima3
# AICc=3735.33

modelo.arima3 %>% checkresiduals()
 



# Ajustando manualmente un modelo ARIMA(1,1,7)
modelo.arima4 <- Arima(y, order=c(1,1,7))
modelo.arima4
# AICc=3747.35

modelo.arima4 %>% checkresiduals()
# Dos líneas caen fuera de los límites y además, 
# El test Ljung-Box muestra p-valor<0.05, por lo tanto al 5% de significancia, 
# concluimos que hay alguna autocorrelación entre las observaciones.


# ¿Modelo 3 o modelo 1?  Según AICc, elegiría el modelo 3.


#::::::::::::::::::::::::::
# Otros modelos:
#::::::::::::::::::::::::::
fit.hw <- hw(y, h=14)
fit.hw %>% checkresiduals()

fit.ets <- ets(y)
fit.ets %>% checkresiduals()

# Para ambos modelos, el test de Lung-Box muestra p-valor<0.05.
# Hay autocorrelacion en los residuales. NO usamos estos modelos.



#:::::::::::::::::::::::::::::
# Evaluando las predicciones en los últimos 14 días.
#::::::::::::::::::::::::::::::
train <- window(y, end=c(90,1))
length(train)  # 624 observaciones.
length(y)      # La serie original contien 638. Por tanto, 14 están fuera de train.

# Verificar con gráfico:
y %>% autoplot(series="Todo") +
  autolayer(train, series="Entrenamiento")


# Usando ARIMA - MODELO 1
modelo.arima1 <- auto.arima(train)
modelo.arima1 %>% checkresiduals()
modelo.arima1

# Usando ARIMA - MODELO 3
modelo.arima3 <- Arima(train, order=c(1,1,1), seasonal = c(0,0,2))
modelo.arima3 %>% checkresiduals()
modelo.arima3

#:::::::::::::::::::::::::::::::
# Predicción: 
#:::::::::::::::::::::::::::::::
modelo.arima1 %>% forecast(h=14) %>% accuracy(y)

modelo.arima3 %>% forecast(h=14) %>% accuracy(y)
  
# Ambos 