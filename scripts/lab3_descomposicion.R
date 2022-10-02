 
#:::::::::::::::::::::::::::::::::::::::::::::::::
# Serie 1 ----
#:::::::::::::::::::::::::::::::::::::::::::::::::
library(fpp2) 

# Descomposición aditiva clásica ----
# .........................................

desc_aditiva <- beer %>% 
              decompose(type = "additive")   # Realiza la descomposición


desc_aditiva %>% 
              autoplot() +                  # Grafica
              xlab("Años") +                # Años.
              ggtitle("Descomposición aditiva clásica
                      para la producción de cerveza")    # "\n": da un salto de línea en el título.
             


# Indices estacionales:
desc_aditiva$seasonal

# Componente Tendencia-Ciclos
desc_aditiva$trend


# Serie desestacionalizada (removiendo la estacionalidad)
serie_desestacionalizada_beer <- beer - desc_aditiva$seasonal

# Grafico de la serie desestacionalizada.
serie_desestacionalizada_beer %>% autoplot()


# Descomposición con STL ----
# .........................................
desc_stl <- beer %>%
            stl(t.window = 9,          # usara 13 observaciones para
                s.window = "periodic",  # la estacionalidad es la misma entre años
                robust = TRUE)

desc_stl %>% autoplot


serie_desestacionalizada <- desc_stl$time.series[,"trend"]+desc_stl$time.series[,"remainder"]

beer %>% 
  autoplot(series="Original") + 
  autolayer(serie_desestacionalizada, series="Desestacionalizada") +
  scale_colour_manual(values=c("Original"="grey40","Desestacionalizada"="red"),
                      breaks=c("Original","Desestacionalizada"))


serie_desestacionalizada %>% 
  ggseasonplot(year.labels = TRUE, 
               year.labels.left = TRUE)

#:::::::::::::::::::::::::::::::::::::::::::::::::
# Serie 2 ----
#:::::::::::::::::::::::::::::::::::::::::::::::::
library(astsa) # install.packages("astsa")
jj %>% autoplot() + 
  ggtitle("Rentabilidad por acción en la empresa J&J")

# Descomposición multiplicativa clásica ----
# .........................................
des_multi <- jj %>% 
            decompose(type = "multiplicative") 


des_multi %>% 
          autoplot() + 
          xlab("Años") +
          ggtitle("Descomposición aditiva clásica
                  para la producción de cerveza")    # "\n": da un salto de línea en el título.


# Si probamos con la descomposición aditiva: 
jj %>% 
  decompose(type = "additive") %>% 
  autoplot() + 
  xlab("Años") +
  ggtitle("Descomposición aditiva clásica
          para la producción de cerveza")    # "\n": da un salto de línea en el título.
 


#:::::::::::::::::::::::::::::::::::::::::::::
# Serie desestacionalizada
#:::::::::::::::::::::::::::::::::::::::::::::
serie_noestacional <- jj/des_multi$seasonal

serie_noestacional  %>% autoplot()
  
# Indices estacionales
des_multi$seasonal

# En el primer trimestre, la rentabilidad de las acciones
# son 99.3% la rentabilidad del promedio anual.

# En el primer trimestre, la rentabilidad de las acciones
# son 103.29% la rentabilidad del promedio anual.
# En el 2do trimestre, aumenta por 3% la rentabilidad promedio anual

# En el 3r trimestre, aumenta por 11.4% la rentabilidad promedio anual

# En el 4to trimestre, disminuye por (100-85.99)%=14.01% la rentabilidad promedio anual



#serie_desestacionalizada %>% ggseasonplot(year.labels = TRUE, 
#               year.labels.left = TRUE)
