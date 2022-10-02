# Veamos una serie con estacionalidad y ciclos. 

library(fpp2)

# La serie hsales de la librería fpp2
# muestra una serie de las ventas mensuales de viviendas 
# unifamiliares nuevas vendidas en Estados Unidos entre 1973 y 1995

# Explorar serie:
hsales %>% autoplot()
# Observamos ciclos - Tendencia que cambia sin frecuencia definida.

# Explorar gráfico por estaciones
hsales %>% ggseasonplot(year.labels = TRUE, 
                        year.labels.left = TRUE)
                        
# Observamos un patron repetitivo en cada año.
# Existe una componente estacional.

# El gráfico de autocorrelación nos confirma ambos: 
# Tendencia: bastones iniciales fuera de los límites.
# Estacionalidad cada 12 meses: Bastones que sobresalen los límites cada 12 meses aproximadamente.
hsales %>% ggAcf()


# La decomposición aditiva nos muestra también rápidamente los patrones:
hsales %>% decompose() %>% autoplot()


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Utilizaremos suavizado exponencial HoltWinters 
# para est serie con estaciones y ciclos. 
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

SuavizadoHoltWinters3 <- hw(hsales)
summary(SuavizadoHoltWinters3)  
# Smoothing parameters:
# alpha = 0.8824       
# beta  = 1e-04 
# gamma = 1e-04 

# 2. Ver modelo y predicciones gráficamente.
SuavizadoHoltWinters3 %>% 
  autoplot() +                                         
  autolayer(fitted(SuavizadoHoltWinters3), series="HoltWinters")  


# 3. Evaluación de las predicciones:
accuracy(SuavizadoHoltWinters3)


#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Usando un efecto multiplicativo en las estaciones:
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
SuavizadoHoltWinters3_multip <- hw(hsales, seasonal = "multiplicative")
summary(SuavizadoHoltWinters3_multip)  

# Indices estacionales:
#s = 0.7481 0.8183 0.9578 0.9864 1.0772 1.0434
#    1.0959 1.1454 1.1532 1.1609 0.9619 0.8516

# Estos indices muestran como varía el nivel 
# de la serie por cada mes del año. 
# Ejemplo 1: En Enero, las ventas se reducen en 25.19% (1 - 0.7481=0.2519)
#          (Observe que en Enero, las ventas promedio del año se multiplican por 0.7481 (se reducen))
# Ejemplo 2: En Mayo, las ventas aumentan en 7.72%     (1.0772 - 1=0.0772)
#          (Observe que en Mayo, las ventas promedio del año se multiplican por 1.0772 (aumentan))


# Comparando predicciones con efecto aditivo y multiplicativo:
accuracy(SuavizadoHoltWinters3)
accuracy(SuavizadoHoltWinters3_multip)