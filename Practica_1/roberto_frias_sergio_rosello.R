if (!"package:ggplot2" %in% search()) { 
  install.packages("ggplot2")
}

library(ggplot2)
adult <- read.csv("adult.prac1.csv")

#http://www.cookbook-r.com/Graphs/Bar_and_line_graphs_(ggplot2)/

#1. Genere las gráficas necesarias para visualizar las diferencias de ingresos en función de las demás
#variables, tanto categóricas como numéricas. (3.5 punto)

ClassMayor <- adult[grep(">50K.",adult$class), ]
ClassMenor <- adult[grep("<=50K.",adult$class), ]

#//////////////////////////////////Age y class//////////////////////////////////

#Generamos unas tablas 
auxMayor <- table(ClassMayor$Age)
auxMenor <- table(ClassMenor$Age)

finalMayor <- data.frame(
  number_people = c(auxMayor),
  Age = c(rownames(auxMayor)),
  class = c(">50K.")
)

finalMenor <- data.frame(
  number_people = c(auxMenor),
  Age = c(rownames(auxMenor)),
  class = c("<=50K.")
)

final <- merge(finalMenor,finalMayor,all=TRUE)

#Histograma sencillo
ggplot(adult, aes(x=Age, fill=class)) +
  geom_histogram(binwidth=.5, position="identity") 

#Grafico de barras en detalle
ggplot(data=final, aes(x=Age, y=number_people, fill=class)) +
  geom_bar(stat="identity")

#Diagrma de densidad
ggplot(adult, aes(x=Age, fill=class)) + geom_density(alpha=.4, size = 0.4)

#//////////////////////////////////workclass y class//////////////////////////////////

#Generamos unas tablas 
auxMayor <- table(ClassMayor$workclass)
auxMenor <- table(ClassMenor$workclass)

finalMayor <- data.frame(
  number_people = c(auxMayor),
  workclass = c(rownames(auxMayor)),
  class = c(">50K.")
)

finalMenor <- data.frame(
  number_people = c(auxMenor),
  workclass = c(rownames(auxMenor)),
  class = c("<=50K.")
)

final <- merge(finalMenor,finalMayor,all=TRUE)

#Histograma sencillo
ggplot(adult, aes(x=workclass, fill=class)) +
  geom_histogram(binwidth=.5, position="identity") 

#Grafico de barras en detalle
ggplot(data=final, aes(x=workclass, y=number_people, fill=class)) +
  geom_bar(stat="identity")

#Diagrma de densidad
ggplot(adult, aes(x=workclass, fill=class)) + geom_density(alpha=.4, size = 0.4)



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


