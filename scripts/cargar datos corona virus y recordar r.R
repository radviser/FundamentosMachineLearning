#verificar en que directorio estamos trabajando
#cargar los datos de coronavirus
#ver cuantos registros tenemos
#sumary (datos)

print ("hola mundo.")

library(readr)
#esto es una prueba

print ("cargar datos")

print ("probando")

# Cargar los datos
datos <- read.csv("datos/covid_19_data.csv")
datos

summary(datos)
str(datos)

unique(datos$Country.Region)


# ¿Cuángos casos confirmados ?
sum(datos$Confirmed)

# ¿cántos desscesos?
sum(datos$Deaths)

# ¿Porcentaje, descesos confirmados
sum(datos$Confirmed / sum(datos$Deaths))

paste(round(sum(datos$Confirmed / sum(datos$Deaths)),2), " % ")


print("Cambios")