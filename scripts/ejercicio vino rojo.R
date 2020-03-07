# Objetivo - Desarrollar regresión para evaluar la calidad del vino
# Descripción - Analizar y aplicar la técnica de regresión lineal y lo árboles de regresión en el conjunto de datos de vinos; realizar interpretaciones y de los modelos lineal y árbol de regresión para elaborar predicciones , comparaciones y establecer resultados de la calidad de los vinos.


# Cargar librerías adecuadas
library(reshape)    # Para renombrar columnas
library(caret)      # Para particiones
library(corrplot)   # Para correlaciones visuales
library(rpart)      # Arboles
library(rpart.plot) # Visualizar y represenar árboles
library(caret)      # Para llevar a cabo particiones de conjuntos de datos en caso de...
library(dplyr)      # Para select, filter, mutate, arange ....
library(readr)      # Para leer datos
library(ggplot2)    # Para grafica mas vistosas
library(reshape)    # Para renombrar columnas


#Cargar datos
#Explorar los datos indicando las características generales de los mismos
#Identificar el significado de cada variable e identiicar variables dependientes y variable independiente
datos <- read_csv("F:/028 - Diplomado/Modulo V/data/winequality-red.csv")

head(datos)
tail(datos)

#Describir los datos str() y summary()
str(datos)
summary(datos)

# Identificar el significado de cada variable e identiicar variables dependientes y variable independiente
# 1 fixed acidity
# 2 volatile acidity - acidez?
# 3 citric acid - agridez?
# 4 residual sugar  - azucar (que tan dulce es)
# 5 chlorides
# 6 free sulfur dioxide
# 7 total sulfur dioxide
# 8 density - densidad
# 9 pH  - ph
# 10 sulphates - sulfatos contenidos 
# 11 alcohol - grados alcohol
# 12 quality - calidad

nrow(datos)
ncol(datos)

ggplot(datos, aes(x=quality)) + geom_histogram()

#comproba que las variables son numéricas
sapply(datos, is.numeric)


#Correlaciones
correlaciones <- data.frame(cor(datos))
datosCol.Correl.calidad <- data.frame(cbind("variable" = rownames(correlaciones),"correlacion"= correlaciones$quality)) 
datosCol.Correl.calidad

datosCol.Correl.calidad <- arrange(datosCol.Correl.calidad, desc(correlacion))
datosCol.Correl.calidad

head(datosCol.Correl.calidad)

ggplot(datos, aes(x=pH, y=quality)) +
  geom_point(color="darkred") + geom_smooth(method = "lm")
ggplot(datos, aes(x=density, y=quality)) +
  geom_point(color="darkred") + geom_smooth(method = "lm")
ggplot(datos, aes(x="citric acid", y=quality)) +
  geom_point(color="darkred") + geom_smooth(method = "lm")



#correlaciones <- cor(datos)
#correlaciones
#corrplot(correlaciones, method = "number")
#no es tan significativa, pero se pueden usar las sig variables
#pH, density, citric acid
#cor(x=datos[,-c(3,8,9)], method = "pearson")
#pairs(x=datos[,-c(3,8,9)], lower.panel = NULL)
#corrplot(corr = cor(x=datos[,-c(3,8,9)], method = "pearson"), method = "number")

#Modelo de regresión lineal mútiple
modelo1 <- lm(formula = quality ~ ., datos)
summary(modelo1)
# resultado este metodo parece no ser el adecuado para estos datos


# Arboles 
head(datos)
tail(datos)
summary(datos)

# Ahora a determinar conjuntos de datos de prueba y luego head()
datos.prueba <- select(datos,'citric acid','pH','density')
head(datos.prueba)
tail(datos.prueba)
summary(datos.prueba)

str(datos.prueba)


# depurar datos??
mediana.pH <- median(datos.prueba$pH, na.rm = TRUE)
mediana.density <- median(datos.prueba$density, na.rm = TRUE)
mediana.citric <- median('datos.prueba$citric acid', na.rm = TRUE)

head(datos.prueba, 10) # Los primeros 10,
tail(datos.prueba, 10) # Los ultimos 10,

set.seed(2020) # Semilla
entrena <- createDataPartition(datos$quality, p=0.7, list = FALSE)
head(entrena)
nrow(datos[-entrena,])

summary(datos.prueba)

#datos del 70% para entrenar
datos.Entrena <- datos.prueba[entrena,]
head(datos.Entrena)
summary(datos.Entrena)
# y conjunto de datos de validación del 30%
datos.Valida <- datos.prueba[-entrena,]
head(datos.Valida)
summary(datos.Valida)

set.seed(2020) # Semilla
arbol <- rpart(formula = pH ~ ., data = datos.Entrena)
arbol

prp(arbol, type = 2, nn = TRUE,
    fallen.leaves = TRUE, faclen = 4,
    varlen = 8, shadow.col = "gray")

print("the end")
