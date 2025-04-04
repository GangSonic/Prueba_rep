#-----------------------------------
# SCRIPT ACTIVIDAD TRANSVERSAL 4
#-----------------------------------


#//////////////////////////////////
# ACTIVIDAD 2
#/////////////////////////////////

#__________________________________
#Cargar datos
#__________________________________

library(readr) 
library(dplyr)

archivo_csv <- "salarios_mujeres.csv"
tabla_datos <- read_csv(archivo_csv)

#__________________________________
#Visualizar las primeras lineas 
#__________________________________
head(tabla_datos)

#__________________________________
#Informacion tecnica de cada columna
#__________________________________
str(tabla_datos)

#__________________________________
#Resumen estadistico 
#__________________________________
summary(tabla_datos)

#__________________________________
# Verificar presencia de na en columnas 
#__________________________________
colSums(is.na(tabla_datos))

#__________________________________
# Revisar existencia de valores duplicados 
#__________________________________
valores_duplicados <- duplicated(tabla_datos)
print (sum(valores_duplicados)) 

#__________________________________
# Limpiar datos na 
#__________________________________
datos_limpios <- na.omit(tabla_datos)

#__________________________________
#Filtrado categoria: Edad 
#__________________________________
filtrado_edad <- tabla_datos[tabla_datos$Edad %in% c(60), ]
filtrado_edad

#__________________________________
#Filtrado categoria: Salario
#__________________________________
filtrado_salrio <- tabla_datos[tabla_datos$Salario %in% c(6137), ]
filtrado_salrio

#__________________________________
#Filtrado categoria: Genero 
#__________________________________
filtrado_genero <- tabla_datos[tabla_datos$Genero %in% c("Hombre"), ]
filtrado_genero

colSums(filtrado_genero)



#//////////////////////////////////
# ACTIVIDAD 3
#/////////////////////////////////

#MEDIDAS DE TENDENCIA CENTRAL 

#__________________________________
#Mediana: Edad 
#__________________________________
mediana_edad <- median(tabla_datos$Edad, na.rm= TRUE)
mediana_edad

#__________________________________
#Mediana: Salario  
#__________________________________
mediana_salario <- median(tabla_datos$Salario, na.rm= TRUE)
mediana_salario

#__________________________________
#Media: Edad 
#__________________________________
media_edad <- mean(tabla_datos$Edad, na.rm= TRUE)
media_edad

#__________________________________
#Media: Salario 
#__________________________________
media_salario <- mean(tabla_datos$Salario, na.rm= TRUE) 
media_salario

#__________________________________
#Moda: Edad 
#__________________________________
moda_edad <- as.numeric(names(sort(table(tabla_datos$Edad), decreasing = TRUE))[1])
moda_edad

#__________________________________
#Moda: Salario 
#__________________________________
moda_salario<- as.numeric(names(sort(table(tabla_datos$Salario), decreasing = TRUE))[1])
moda_salario

#__________________________________
#Moda: Salario 
#__________________________________
moda_genero<- as.character(names(sort(table(tabla_datos$Genero), decreasing = TRUE))[1])
moda_genero


#VISUALIZACION DE DATOS CON "ggplot2" 
install.packages("ggplot2")
library(ggplot2)


#__________________________________
# GRAFICO DE BARRAS  
#__________________________________
ggplot(tabla_datos, aes(x = Salario)) + 
  geom_bar(fill = "lightgoldenrodyellow", color = "black") +
  labs(title = "Frecuencia de salario", x = "Salario", y = "Cantidad") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#__________________________________
# LINK DE REPOSITORIO
#__________________________________
## https://github.com/GangSonic/Prueba_rep.git

