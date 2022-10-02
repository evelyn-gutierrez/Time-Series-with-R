#:::::::::::::::::::::::::::::::::::::
# Suavizado Holts 
#:::::::::::::::::::::::::::::::::::::
library(fpp2) # Cargar la librería necesaria

# Preparación ----

# Utilizaremos la serie: goog200 del paquete fpp2 
# Los precios de las acciones de google en 200 días.
goog200_v2 <- ts(data.frame(goog200), frequency=7)  # Cada 7 días puede haber un patrón de estacionalidad
goog200_v2 %>% autoplot(series="Precio por acción")
 

# Creamos la serie cambios en precios
serieY <- diff(goog200_v2)


# Examinar autocorrelación en los 10 primeros lags.
Box.test(serieY, lag=10, type="Ljung-Box")


#::::::::::::::::::::::::::::::::::::
# Doble diferencia
#::::::::::::::::::::::::::::::::::::
usmelec %>% autoplot()

# logaritmo de la serie:
log(usmelec) %>% autoplot()

# Diferencia estacional de la serie:
diff(log(usmelec),12) %>% autoplot()

# Diferencia de la diferencia estacional:
diff(diff(log(usmelec),12),1) %>% autoplot()
 
diff(diff(log(usmelec),12),1) %>% Box.test(type="Ljung-Box")