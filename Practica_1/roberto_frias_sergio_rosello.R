if (!"package:ggplot2" %in% search()) { 
  install.packages("ggplot2")
}

library(ggplot2)

adult <- read.csv("adult.prac1.csv")

nrow(adult)
ncol(adult)
adult[1,1]
adult[150,5]

#1. Genere las gráficas necesarias para visualizar las diferencias de ingresos en función de las demás
#variables, tanto categóricas como numéricas. (3.5 punto)

valClass <- levels(adult$class)



tablaAgeClass <- adult[, c(1,14)]



hist(adult$Age, breaks = 1:100)


edades <- table(adult$Age)

as.integer(edades[2])

numeroedadesmenor <- c()
numeroedadesmayor <- c()


for(j in 1:nrow(adult)){
  valormayor <- 0
  valormenor <- 0
  for(i in 1:100){

    if(adult[j,1] == i)
    { 
      print("hola")
      valormenor <- sum(valormenor + 1)
    }
    if(adult[j, 1]) {
      valormayor <- sum(valormayor + 1)
    }
  }
  valormayorVector <- c(valormayor)
  valormenorVector <- c(valormenor)
  
  append(numeroedadesmayor, valormayor)
  append(numeroedadesmenor, valormenor)
}


dat1 <- data.frame(
  sex = factor(c("Female","Female","Male","Male")),
  time = factor(c("Lunch","Dinner","Lunch","Dinner"), levels=c("Lunch","Dinner")),
  total_bill = c(13.53, 16.81, 16.24, 17.42)
)
dat1

ggplot(data=tablaAgeClass, aes(x=age, y=number_people, fill=class) +
  geom_bar(stat="identity", position=position_dodge())



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


