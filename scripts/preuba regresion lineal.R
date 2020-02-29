library(ggplot2) # Para gráficas
library(stats) # Para regresion lineal


restaurantes <- c(1:10)
estudiantes <- c(2,6,8,8,12,16,20,20,22,26)
ventas <- c(58,105,88,118,117,137,157,169,149,202)

datos <- data.frame(restaurantes,estudiantes,ventas)
datos

plot(datos$estudiantes, datos$ventas,
     xlab = "Estudiantes",
     ylab = "Ventas $",
     main = "Diagrama de dispersión")

ggplot(datos, aes(estudiantes, ventas))   +   geom_point()

#ggplot(data = datos) + 
#  geom_point(mapping = aes(x = estudiantes, y = ventas))
modelo<-lm(ventas ~ estudiantes, datos) 
modelo


y_predict <- predict(modelo, datos)

ggplot() + geom_point(data = datos, aes(x = estudiantes, y = ventas), size = 0.9) +
  geom_line(aes( x = datos$estudiantes, y = y_predict), color = "red") +
  xlab("Estudiantes") + 
  ylab("Ventas") + 
  ggtitle("Linea de tendencia sobre Conjunto de Datos")


a.coef.intercepcion = modelo$coefficients[1]
b.pendiente = modelo$coefficients[2]
y.prediccion <- a.coef.intercepcion + b.pendiente * c(16, 32, 64, 96)
# Las predicciones son:
y.prediccion

cor(datos$estudiantes, datos$ventas)
