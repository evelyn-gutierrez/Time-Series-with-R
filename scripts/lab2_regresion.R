
#::::::::::::::::::::::::::::::::::::::::
# Regresión en series de tiempo ----
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
  geom_boxplot() +                            # Graficar puntos para esas variable
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

summary(ajuste_multiple) # R2-aj =15.45%

# Claramente el modelo de RL múltiple se ajusta mejor.
performance::compare_performance(ajuste_multiple,ajuste_simple)


# Revisión de residuales
#.................................
checkresiduals(ajuste_multiple)




#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# 3. Variables de tendencia y estancionalidad en el modelo -----
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

#:::::::::::::::::::::::::::::::::::::::::::::::::
# Predicciones:
#::::::::::::::::::::::::::::::::::::::::::::::
# Caso: Producción de Cerveza 
# Usando el modelo RL con tendencia y estaciones como variables del modelo.
pred_cerveza <- forecast(ajuste_cerveza, h=10) # Predicción del modelo "ajuste_cerveza".
pred_cerveza
autoplot(pred_cerveza)

# Revisar las predicciones en el test set (ultimas 10 obs)
accuracy(pred_cerveza, beer_test)


# Predecir usando escenarios para las variables explicativas.
#..........................................................
h <- 4 # Periodos de predicción.

# Escenario 1:
newdata <-  cbind(Income = c(1, 1, 1, 1) ,           # En ingreso incrementa en 1%
                  Savings = c(0.5, 0.5, 0.5, 0.5),      # Los ahorros incrementan cada trimestre en 0.5%
                  Unemployment = c(0, 0, 0, 0)) %>%     # desempleo no cambia (indice de cambio es 0)
            as.data.frame()                             # Convierte a data-frame

prediccion1 <- forecast(ajuste_multiple, newdata = newdata) # Forecast the model based on new data.
prediccion1



# Escenario 2:
newdata <-  cbind(Income =  c(0.5, 1, 1, 0.5) ,               # En ingreso aumenta por 0.5%
                  Savings =  c(0.5, 0.5, 0.5, 1),                # Los ahorros incrementan cada trimestre por 1%
                  Unemployment = c(0, 0, 0, 0)) %>%   # Desempleo no cambia (indice de cambio es 0)
  as.data.frame()                             # Convierte a data-frame

prediccion2 <- forecast(fit.consBest, newdata = newdata) # Forecast the model based on new data.
prediccion2


autoplot(uschange[, "Consumption"]) +
  ylab("% cambio en el consumo") + #
  autolayer(prediccion1, PI = TRUE, series = "Escenario 1") +
  autolayer(prediccion2, PI = TRUE, series = "Escenario 2") +
  guides(colour = guide_legend(title = " "))



