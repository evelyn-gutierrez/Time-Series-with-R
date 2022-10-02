
#::::::::::::::::::::::::::::::::::::::::
# Regresión en las series de tiempo ----
#::::::::::::::::::::::::::::::::::::::::
 
# Cargar librería
library(fpp2) 
 
#::::::::::::::::::::::::::
# 1. Exploración ----
#::::::::::::::::::::::::::
summary(uschange) # Los datos tienen 5 variables

# Gráfico de series de tiempo 
uschange[, c("Consumption","Income")] %>%     # Se extraen las columas: "Consumption" and "Income"
  autoplot() +                                # Se grafican
  ylab("% cambio") + xlab("Años")  


# Gráfico de dispersión:
uschange %>%                                  # Usamos 'uschange'
  as.data.frame() %>%                         # Se convierte a data.frame
  ggplot(aes(x = Income, y = Consumption)) +  # Se grafica 'Income' en el eje x y  'Consumption' en el eje y ("+" significa que añadiremos más elementos al gráfico)
  ylab("Consumo (Cambio trimestral %)") +     # Etiqueta del eje y
  xlab("Ingreso (Cambio trimestral %)") +     # Etiqueta del eje x
  geom_point() +                            # Graficar puntos para esas variable
  geom_smooth(method = "lm", se = FALSE)      # Añadir la recta de regresión para esas dos variables.



#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# 2. Ajustar modelo de regresión lineal ----
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Usamos la función: tslm
# tslm(formula, data); formula especifica el modelo de regresión. Ejemplo: tslm(y ~ x, data = mydata)


# RL Simple
#........................................................................
ajuste_simple <- tslm(Consumption ~ Income, data = uschange)   # Regresión lineal simple

summary(ajuste_simple) # R2-aj =15.45%

# RL Múltiple
#........................................................................
ajuste_multiple <- tslm(Consumption ~ Income + Production + Unemployment + Savings,
                   data = uschange) 

summary(ajuste_multiple) # R2-aj =74.86%

# Podemos comparar sus métricas de bondad de ajuste
performance::compare_performance(ajuste_multiple,ajuste_simple)


# Revisión de residuales
#.................................
# Ho: r1=r2=r3=r4...r8=0
# HA: Alguna de las autocorrelaciones 
# de hasta orden 8 es diferente de 0.
checkresiduals(ajuste_multiple)

# P-valor: 0.06163
# No rechazo Ho.  
# Puedo asumir que mis residuales no 
# tienen correlación serial de hasta orden 8




#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# 3. Regresión con tendencia y estacionalidad -----
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
beer2 <- window(ausbeer, start = 1992, end = c(2007,4))     # Training data
beer_test <- window(ausbeer, start = 2008)                  # Test data

beer2 %>%
  autoplot() + 
  xlab("Años") + 
  ylab("Megalitros")

ajuste_cerveza <- tslm(beer2 ~ trend + season) # Fit a linear model with trend and seasonality
summary(ajuste_cerveza) 

ajuste_cerveza %>%      
  checkresiduals()

# Ho: Hay correlación serial de hasta orden 8.
# p-valor=0.2285: No rechazo H0, con un nivel de significancia del 0.05

#:::::::::::::::::::::::::::::::::::::::::::::::::
# Predicciones:
#::::::::::::::::::::::::::::::::::::::::::::::
# Usando el modelo con tendencia y estaciones como variables del modelo.
pred_cerveza <- forecast(ajuste_cerveza, h=10) # Predicción del modelo "ajuste_cerveza".
pred_cerveza
# Los intervalos de predicción calculados aqui
# asumen ruido blanco Gaussiano (Con distribución normal) 
# para los residuales: 
# - No correlación entre ellos. 
# - Media cero.
# - Distribución normal con varianza constante.


# graficar predicciones e intervalos de predicción:
autoplot(pred_cerveza)



# Revisar las predicciones en el test set (ultimas 10 obs)
# accuracy(pred_cerveza, beer_test) 