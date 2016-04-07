if (!"package:ggplot2" %in% search()) { 
  install.packages("ggplot2")
}

library(ggplot2)
adult <- read.csv("adult.prac1.csv")



#1. Genere las gráficas necesarias para visualizar las diferencias de ingresos en función de las demás
#variables, tanto categóricas como numéricas. (3.5 punto)

#Indica los posibles valores que puede tomar cualquier fila en esa determinada columna
valClass <- levels(adult$class)

#Genera una tabla de dos columnas, "la de edad y la de dinero"Age" y "Class"
tablaAgeClass <- adult[, c(1,14)]


#Genera un histograma de adult age vs frequency
hist(adult$Age, breaks = 1:100)

#Genera un array guardando el numero de veces que aparece cada numero en la calumna de edades
edades <- table(adult$Age)

#Imprime solo el numero de veces que aparece el numero de la casilla numero 2 del array.
as.integer(edades[2])

#Imprime el numero que aparece en la tabla y el numero de veces que aparece en la tabla.
print(edades[2])
#Bucle que recorre edades e imprime el numero de veces que aparecen sus numeros
for(i in edades){
  print(i)
}


#2. Obtenga las medidas de tendencia central de cada una de las variables numéricas para cada una de las dos
#posibles clases. ¿Existe diferencia? (1 punto)




#3. Obtenga los cuartiles y los percentiles de cada una de las variables numéricas para cada una de las dos
#posisbles clases. ¿Existe diferencia? (1 punto)




#4. Para las variables numéricas y para cada unas de las clases, calcule las medidas de dispersión siguientes:
#  (a) recorrido, (b) recorrido inter-cuantílico y (c) varianza. ¿Existe diferencia? (1 punto)



#5. Con respecto a las medidas de asimetría, ¿que podríamos indicar de la distribución de cada una de las
#variables númericas de las dos clases? (1 punto)




#6. Realice un ajuste de mínimos cuadrados entre la variable horas por semana y nivel de educación. Indique
#la precisión del ajuste. (2.5 punto)


