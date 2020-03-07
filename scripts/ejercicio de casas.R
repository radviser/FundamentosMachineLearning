#Regresión lineal múltiple para predecir precios de casa Melbourne

#Las librerías
library(readr)
library(dplyr)
library(ggplot2)
#install.packages("reshape")
library(reshape) # Para renombrar columnas
library(caret) # Para particiones
library(corrplot) # Para correlaciones visuales



#Los datos originales
#datos <- read.csv("https://raw.githubusercontent.com/rpizarrog/FundamentosMachineLearning/master/datos/melb_data.csv")
# Cargarlos de manera local porque se tarda desde Web 
datos <- read.csv("F:/028 - Diplomado/Modulo V/data/melb_data.csv")

head(datos)
tail(datos)


#Describir los datos str() y summary()
str(datos)
summary(datos)


#Price Vs Rooms + Distance
modelo <- lm(Price ~ Rooms + Distance, datos)
modelo

summary(modelo)


pairs(datos[,c('Price','Rooms')])

#Un conjunto de datos únicamente con las variables numéricas del conjunto de datos original
datos.Num <- select(datos, Price, Rooms, Distance, Bedroom2, Bathroom, Car, Landsize, BuildingArea, YearBuilt, Propertycount) 
head(datos.Num)

str(datos.Num)


#Depurar, limpiar los datos
#Hay algunos NA que úeden afectar al modelo?

mediana.BA <- median(datos.Num$BuildingArea, na.rm = TRUE) # summary(datos.Num$BuildingArea)[3], como otra alternativa
mediana.YB <- median(datos.Num$YearBuilt, na.rm = TRUE)    # summary(datos.Num$YearBuilt)[3], , como otra alternativa
mediana.C <- median(datos.Num$Car, na.rm = TRUE)    # summary(datos.Num$Car)[3], , como otra alternativa

#Actualizar mutate() los Na por la medianas
head(datos.Num, 10) # Los primeros 10, se observan NAs
datos.Num<- datos.Num %>%
  mutate (BuildingArea = ifelse(is.na(BuildingArea), mediana.BA, BuildingArea))

datos.Num <- datos.Num %>%
  mutate (YearBuilt = ifelse(is.na(YearBuilt), mediana.YB, YearBuilt)) 

datos.Num <- datos.Num %>%
  mutate (Car = ifelse(is.na(Car), mediana.C, Car)) 


head(datos.Num, 10) # # Los primeros 10, YA NO se observan NAs


#Correlaciones
correlaciones <- cor(datos.Num)
correlaciones

corrplot(correlaciones, method = "number")


#Crear conjuntos de entrenamiento y conjuntos de validación
set.seed(2020) # Semilla
entrena <- createDataPartition(datos.Num$Price, p=0.7, list = FALSE)
head(entrena)

nrow(entrena)

# Los registros que no estén en entrena
head(datos.Num[-entrena,])

nrow(datos.Num[-entrena,])

head(datos.Num)


# Ahora a determinar conjuntos de datos de entrenamiento y luego head()
datos.Entrena <- datos.Num[entrena,]
head(datos.Entrena)

summary(datos.Entrena)

# y conjunto de datos de validación y luego head()
datos.Valida <- datos.Num[-entrena,]
head(datos.Valida)

summary(datos.Valida)


#Modelo de regresión lineal múltiple
#Precio en función de todas las variables numéricas del conjunto de datos de entrenamiento
modelo <- lm(Price ~ ., datos.Entrena)
modelo

# Interpretación --No es un buen modelo


#Probar con los datos de validación
modelo <- lm(Price ~ ., datos.Valida)
modelo

summary(modelo)


#Vamos a eliminar las observaciones que tienen NA
#Nuevamente obtenemos los datos, sólo las variables numéricas
datos.Num <- select(datos, Price, Rooms, Distance, Bedroom2, Bathroom, Car, Landsize, BuildingArea, YearBuilt, Propertycount) 

head(datos.Num)
str(datos.Num)
summary(datos.Num)


#Nuevamente datos de entrenamiento y datos de validación
set.seed(2020) # Semilla
entrena <- createDataPartition(datos.Num$Price, p=0.7, list = FALSE)

head(entrena)
nrow(entrena)
nrow(datos.Num[-entrena,])


# Ver los primeros seis datos con sólo variables numéricas
head(datos.Num)

# Ahora a determinar conjuntos de datos de entrenamiento y luego head()
datos.Entrena <- datos.Num[entrena,]
head(datos.Entrena)

# y conjunto de datos de validación y luego head()
datos.Valida <- datos.Num[-entrena,]
head(datos.Valida)
summary(datos.Valida)

#Nuevamente modelo de regresión lineal
#El modelo elimina las observaciones que tienen NA
modelo <- lm(Price ~ ., datos.Entrena)
modelo
summary(modelo)

