# en el caso de publicidad y ventas cual es la prediccion de las ventas con valo de 
# x = comerciales de 3.5 y 4.5??
#
#como determinarlo en r?
#
library(ggplot2) # Para gráficas
library(stats) # Para regresion lineal

semanas <- c(1:10)
comerciales <- c(2,5,1,3,4,1,5,3,4,2)
ventas <- c(50,57,41,54,54,38,63,48,59,46)

datos <- data.frame(semanas,comerciales,ventas)
datos

plot(datos$comerciales, datos$ventas,
     xlab = "Comerciales",
     ylab = "Ventas $",
     main = "Diagrama de dispersión")

ggplot(datos, aes(comerciales, ventas))   +   geom_point()


#ggplot(data = datos) + 
#  geom_point(mapping = aes(x = comerciales, y = ventas))

modelo <- lm(ventas ~ comerciales, data = datos)
modelo

y_predict <- predict(modelo, datos)

ggplot() + geom_point(data = datos, aes(x = comerciales, y = ventas), size = 0.9) +
  geom_line(aes( x = datos$comerciales, y = y_predict), color = "red") +
  xlab("Comerciales") + 
  ylab("Ventas") + 
  ggtitle("Linea de tendencia sobre Conjunto de Datos")

cr <- cor(datos$comerciales, datos$ventas)
cr

predict.ventas <- predict (modelo,data.frame(comerciales=c(3.5,4.5)))

print(predict.ventas)







