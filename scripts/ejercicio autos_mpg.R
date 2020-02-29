#cuantas millas por galon ejercicio
library(readr)
# install.packages("corrplot") # Nuevo
library(corrplot) # Para correlación
library(caret)  # Para dividir conjunto de datos

# install.packages("MASS") # NUEVO
library(MASS)

datos <- read.csv("F:/028 - Diplomado/Modulo V/data/auto-mpg.csv")
# datos <- read.csv("~\data\auto-mpg.csv")

head(datos) # Los primeros seis

tail(datos) # Los últimos seis

summary(datos) # Antes de categorizar o factor

datos$cylinders <- factor(datos$cylinders, levels = c(3,4,5,6,8),
                          labels = c('3c', '4c','5c','6c','8c'))

summary(datos) # Después de categorizar o factor

cor(x=datos[,-c(1,3, 8,9)], method = "pearson")

pairs(x=datos[,-c(1,3,8,9)], lower.panel = NULL)

corrplot(corr = cor(x=datos[,-c(1,3,8,9)], method = "pearson"), method = "number")

nrow(entrena) # Cuantos datos de entrenamiento

datosentrenamiento <- datos[entrena, -c(1,8,9)]

datosvalidacion <- datos[-entrena, -c(1,8,9)]

# Veremos que no son los mis datos los de entrenamiento y los datos de validación
head(datos)
tail(datos)


head(datosentrenamiento)
head(datosvalidacion)

modelo <- lm(mpg ~ ., data = datosentrenamiento)
modelo

summary(modelo)

sqrt(mean((modelo$fitted.values - datosentrenamiento$mpg) ^ 2))
mpg_prediccion <- predict(modelo, datosvalidacion) 
sqrt(mean((mpg_prediccion - datosvalidacion$mpg) ^ 2))

par(mfrow=c(2,2))
plot(modelo)


cyl = c(4,5)
dis = c(80,100)
hp = c(70,90)
wei = c(2000, 2100)
ace = c(14, 16)

nuevosdatos <- data.frame(cylinders=cyl, displacement=dis,
                          horsepower=hp, weight=wei, acceleration = ace)
nuevosdatos

modelo$coefficients

mpg.predict4 = modelo$coefficients[1] + 
  modelo$coefficients[2] * 1 +
  modelo$coefficients[6] * dis[1] + 
  modelo$coefficients[7] * hp[1] + 
  modelo$coefficients[8] * wei[1] + 
  modelo$coefficients[9] * ace[1]
mpg.predict4

modelo$coefficients

mpg.predict5 = modelo$coefficients[1] + 
  modelo$coefficients[3] * 1 +
  modelo$coefficients[6] * dis[2] + 
  modelo$coefficients[7] * hp[2] + 
  modelo$coefficients[8] * wei[2] + 
  modelo$coefficients[9] * ace[2]
mpg.predict5
