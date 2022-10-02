
#::::::::::::::::::::::::::::::::::::::::
# Configuración ----
#::::::::::::::::::::::::::::::::::::::::
# Instalamos librerías:
# install.packages("fpp2", dependencies = TRUE)

# Cargar librería
library(fpp2) 


# Básicos:

# Definir una serie temporal:
y <- ts(c(100,123,39,78,52,110), 
        start = c(2016,1), frequency = 4)
y

# Definir una serie temporal - v2:
z <- c(100,123,39,78,52,110) # crear objeto z
y <- ts(z, start = 2016) # Pasar el vector z a ts()
y

  
#::::::::::::::::::::::::::
# 1. Exploración ----
#::::::::::::::::::::::::::
  
# Serie: melsyd
summary(melsyd) # Usar ?melsyd para ver detalles de los datos
head(melsyd)
tail(melsyd)

# Grafico de la serie.
autoplot(melsyd[, "Economy.Class"])

# Añade titulos y más etiquetas
autoplot(melsyd[, "Economy.Class"]) +  
  ggtitle("Pasajeros Clase Económica: Melbourne-Sydney") +
  xlab("Años") +
  ylab("Miles")


# Serie: a10 (Ventas de medicinas).
autoplot(a10) +
  ggtitle("Medicinas contra la diabetes") +
  ylab("Millones $") +
  xlab("Años")
 

#::::::::::::::::::::::::::::::::::::::
# Gráfico de autocorrelación
#::::::::::::::::::::::::::::::::::::::

# Serie: producción de cerveza
beer2 <- window(ausbeer, start = 1992) # 
summary(beer2)
head(beer2)
tail(beer2)

# gráfico autocorrelación +
ggAcf(beer2, lag.max=10)

acf <- ggAcf(beer2, lag.max=10)
acf$data

# gráfico de autocorrelación
beer2 %>%
  ggAcf(lag.max = 50)


#:::::::::::::::::::::::::::
# Ruido blanco
#::::::::::::::::::::::::::::
set.seed(33)        # Para que sea replicable
y <- ts(rnorm(50))  # Serie de tiempo de 50 obs.
summary(y)
head(y)             # muestra los primeros valores
tail(y)


# Gráfico de esta serie de ruido blanco:
y %>%                     # Series to be plotted
  autoplot() +            # Plot it
  ggtitle("White Noise")  # Title of the graph

# Autocorrelación
y %>%                     # Series to be plotted
  ggAcf() +               # ACF of the abvoe series
  ggtitle("White Noise (ACF)")


# Prueba - Hasta la autocorrelación de orden p
# H0: p_1 = p_2 = ... = p_p = 0
y %>% 
  Box.test(lag = 10, fitdf = 0, type = "Lj")
# Ok



#::::::::::::::::::::::::::::::::::
# 2. Predicciones simples ----
#::::::::::::::::::::::::::::::::::
beer2 <- window(ausbeer, start = 1992, end = c(2007,4))  # Training data
beer3 <- window(ausbeer, start = 2008)                  # Test data

# Métodos simples de predicción
# Cada método retorna la predicción en h elementos siguientes.
# Los IC son IC para las predicciones.


# Usando el valor medio.
beer2 %>%
  meanf(h = 11) %>% autoplot()

# Metodo Naive
beer2 %>%
  naive(h = 11) %>% autoplot()

# Seasonal Naive
beer2 %>%
  snaive(h = 11) %>% autoplot() 

# Metodo de Deriva. (Drift)
beer2 %>%
  rwf(h = 11, drift=TRUE) %>% autoplot() 


#::::::::::::::::::::::::::::::
# Varias predicciones simples
#::::::::::::::::::::::::::::::
# Plot forecasts
autoplot(beer2) +
  autolayer(meanf(beer2, h = 11),
            series = "Media", PI = FALSE) + #PI: Intervalo de predicción (Prediction Interval)
  autolayer(naive(beer2, h = 11),
            series = "Naive", PI = FALSE) +
  autolayer(snaive(beer2, h = 11),
            series = "Seasonal naive", PI=FALSE) +
  autolayer(rwf(beer2, h = 11, drift=TRUE),
            series = "Drift", PI=FALSE) +
  ggtitle("Predicciones para producción de cervezas") +
  xlab("Años") + ylab("Megalitros") +
  guides(colour = guide_legend(title = "Métodos"))


# Revisar las predicciones en el test set (ultimas 10 obs)
naive_pred <- beer2 %>%      # Serie de tiempo hasta el 2007
              snaive(h = 10)
accuracy(naive_pred, beer3)
# RMSE: Root Mean Squared Error. 

#::::::::::::::::::::::::::::
# 3. Explorando residuales ----
#::::::::::::::::::::::::::::

# Residuales usando método Naive:
#.....................................................
res_naive <- beer2 %>%  
        naive() %>%   # Aplicar predicción Naive
        residuals()   # calcular residuales

res_naive %>% autoplot() #Grafica residuales.
# Los residuales tienen un patrón estacional. 
# Esto muestra que este método no es bueno para la predicción.
# Necesitamos un modelo/método de predicción que ayude 
# a capturar los patrones observados en los residuales. 


# Residuales usando método Naive Estacional - Seasonal Naive:
#.....................................................
res_naive <- beer2 %>%  
            snaive() %>%   # Aplicar predicción Seasonal Naive
            residuals()   # calcular residuales

res_naive %>% 
  gghistogram() + # Crear hisograma
  ggtitle("Histograma para los residuales")

res_naive %>% 
  ggAcf() + 
  ggtitle("Autocorrelograma")


# Todo en un solo gráfico y prueba
# Ho: p1=p2=...=p8=0
beer2 %>% 
  snaive() %>%      
  checkresiduals()


# Para una buena predicción, los residuos deben ser ruido blanco:
  # No correlacionados
  # Media cero.
# Si no se cumple, querrá decir que debo buscar mejores modelos.

# Otras características deseables (no estrictamente necesarias)
 # Normalidad
 # Varianza constante.
# Si no se cumple esto, los intervalos de predicción (PI) 
# obtenidos en R no serán correctos.  

