
if (!"package:ggplot2" %in% search() ||!"package:e1071" %in% search()) { 
  install.packages("ggplot2")
  install.packages("e1071")
}

library(ggplot2)
library(e1071)
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

#Conclusion de los datos:
#Estos datos muestran que hay mas gente que gana menos de 50.000
#y que a partir de los 25 incrementa el numero de gente que gana mas de 50.000
#y decrementa el numero de gente que gana menos de 50.000, pero siempre hay mas gente
#que gana menos de 50.000. 
#La encuesta tambien se ha preguntado a mucha mas gente joven (<50)que a gente mayor. 

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

#Grafico de barras en detalle
ggplot(data=final, aes(x=workclass, y=number_people, fill=class)) +
  geom_bar(stat="identity")

#Conclusion de los datos:
#Estos datos, representados en un diagramama de barras, muestran que
#excepto en los "self-employed", en todos los demas tipos de trabajos predominan los salarios
#inferiores a los 50.000. Excepto los que no han trabajado nunca, que no tienen ingresos.

#//////////////////////////////////education y class//////////////////////////////////

#Generamos unas tablas 
auxMayor <- table(ClassMayor$education)
auxMenor <- table(ClassMenor$education)

finalMayor <- data.frame(
  number_people = c(auxMayor),
  education = c(rownames(auxMayor)),
  class = c(">50K.")
)

finalMenor <- data.frame(
  number_people = c(auxMenor),
  education = c(rownames(auxMenor)),
  class = c("<=50K.")
)

final <- merge(finalMenor,finalMayor,all=TRUE)

#Grafico de barras en detalle
ggplot(data=final, aes(x=education, y=number_people, fill=class)) +
  geom_bar(stat="identity")

#Conclusion de los datos:
#Estos datos, representados en un diagramama de barras, muestran que
#excepto la gente que tiene estudios masters, doctorados y prof-school, que son una minoria,
#es mucho mas comun que el resto de gente con estudios gane menos de 50.000 siendo "hs-grad"
#el tipo de estudion mas comun entre la poblacion.

#//////////////////////////////////education.num y class//////////////////////////////////

#Generamos unas tablas 
auxMayor <- table(ClassMayor$education.num)
auxMenor <- table(ClassMenor$education.num)

finalMayor <- data.frame(
  number_people = c(auxMayor),
  education.num = c(rownames(auxMayor)),
  class = c(">50K.")
)

finalMenor <- data.frame(
  number_people = c(auxMenor),
  education.num = c(rownames(auxMenor)),
  class = c("<=50K.")
)

final <- merge(finalMenor,finalMayor,all=TRUE)

#Histograma sencillo
ggplot(adult, aes(x=education.num, fill=class)) +
  geom_histogram(binwidth=.5, position="identity") 

#Grafico de barras en detalle
ggplot(data=final, aes(x=education.num, y=number_people, fill=class)) +
  geom_bar(stat="identity")

#Diagrma de densidad
ggplot(adult, aes(x=education.num, fill=class)) + geom_density(alpha=.4, size = 0.4)

#Conclusion de los datos:
#A medida que aumentan los estudios de la gente, aumenta el numero de gente que 
#gana mas de 50.000. 

#//////////////////////////////////marital.status y class//////////////////////////////////

#Generamos unas tablas 
auxMayor <- table(ClassMayor$marital.status)
auxMenor <- table(ClassMenor$marital.status)

finalMayor <- data.frame(
  number_people = c(auxMayor),
  marital.status = c(rownames(auxMayor)),
  class = c(">50K.")
)

finalMenor <- data.frame(
  number_people = c(auxMenor),
  marital.status = c(rownames(auxMenor)),
  class = c("<=50K.")
)

final <- merge(finalMenor,finalMayor,all=TRUE)

#Grafico de barras en detalle
ggplot(data=final, aes(x=marital.status, y=number_people, fill=class)) +
  geom_bar(stat="identity")

#Conclusion de los datos:
#Independientemente del tipo de matrimonio, hay mas matrimonios que ganan menos de 50.000 
#que matrimonios que ganan mas de 50.000 y los matrimonios mas
#comunes son: gente que se casa con civiles y gente que no se casa.

#//////////////////////////////////occupation y class//////////////////////////////////

#Generamos unas tablas 
auxMayor <- table(ClassMayor$occupation)
auxMenor <- table(ClassMenor$occupation)

finalMayor <- data.frame(
  number_people = c(auxMayor),
  occupation = c(rownames(auxMayor)),
  class = c(">50K.")
)

finalMenor <- data.frame(
  number_people = c(auxMenor),
  occupation = c(rownames(auxMenor)),
  class = c("<=50K.")
)

final <- merge(finalMenor,finalMayor,all=TRUE)

#Grafico de barras en detalle
ggplot(data=final, aes(x=occupation, y=number_people, fill=class)) +
  geom_bar(stat="identity")

#Conclusion de los datos:
#Hay mucha mas gente que gana menos de 50.000 a gente que gana mas de 50.000 en
#cualquier disciplina de trabajo, siendo "other-service" la que mas diferencia tiene.

#//////////////////////////////////relationship y class//////////////////////////////////

#Generamos unas tablas 
auxMayor <- table(ClassMayor$relationship)
auxMenor <- table(ClassMenor$relationship)

finalMayor <- data.frame(
  number_people = c(auxMayor),
  relationship = c(rownames(auxMayor)),
  class = c(">50K.")
)

finalMenor <- data.frame(
  number_people = c(auxMenor),
  relationship = c(rownames(auxMenor)),
  class = c("<=50K.")
)

final <- merge(finalMenor,finalMayor,all=TRUE)

#Grafico de barras en detalle
ggplot(data=final, aes(x=relationship, y=number_people, fill=class)) +
  geom_bar(stat="identity")

#Conclusion de los datos:
#Por lo general, hay mas gente que gana menos de 50.000 que gente que gana mas de 50.000.
#Son particularmente interesantes las columnas de "Husband" y "wife" ya que uno asumiria
#que la mayoria de los matrimonios estan compuestos por matido y mujer pero esta grafica
#no sugiere eso. Esta grafica sugiere que hay muchos mas maridos que mujeres en un matrimonio,
#por tanto, me hace sospechar que o se ha entrevistado a mas hombres que a mujeres o que
#hay mas matrimonios homosexuales que heterosexuales.

#//////////////////////////////////race y class//////////////////////////////////

#Generamos unas tablas 
auxMayor <- table(ClassMayor$race)
auxMenor <- table(ClassMenor$race)

finalMayor <- data.frame(
  number_people = c(auxMayor),
  race = c(rownames(auxMayor)),
  class = c(">50K.")
)

finalMenor <- data.frame(
  number_people = c(auxMenor),
  race = c(rownames(auxMenor)),
  class = c("<=50K.")
)

final <- merge(finalMenor,finalMayor,all=TRUE)

#Grafico de barras en detalle
ggplot(data=final, aes(x=race, y=number_people, fill=class)) +
  geom_bar(stat="identity")

#Conclusion de los datos:
#Estos datos, representado en un histograma, indican que la raza mas comun, es la blanca.
#Tambien indican que en ninguna de las razas es mas comun ganar mas de 50.000 

#//////////////////////////////////sex y class//////////////////////////////////

#Generamos unas tablas 
auxMayor <- table(ClassMayor$sex)
auxMenor <- table(ClassMenor$sex)

finalMayor <- data.frame(
  number_people = c(auxMayor),
  sex = c(rownames(auxMayor)),
  class = c(">50K.")
)

finalMenor <- data.frame(
  number_people = c(auxMenor),
  sex = c(rownames(auxMenor)),
  class = c("<=50K.")
)

final <- merge(finalMenor,finalMayor,all=TRUE)

#Grafico de barras en detalle
ggplot(data=final, aes(x=sex, y=number_people, fill=class)) +
  geom_bar(stat="identity")

#Conclusion de los datos:
#Esta grafica indica que se ha entrevistado a mas hombres que a mujeres, por tanto responde
#a mi suposicion anterior. Tambien indica que la mayoria de hombres y mujeres ganan menos de 50.000
#pero que en relacion, hay mas hombres que ganan mas de 50.000 que mujeres que ganan mas de 50.000

#//////////////////////////////////capital.gain y class//////////////////////////////////

#Generamos unas tablas 
auxMayor <- table(ClassMayor$capital.gain)
auxMenor <- table(ClassMenor$capital.gain)

finalMayor <- data.frame(
  number_people = c(auxMayor),
  capital.gain = c(rownames(auxMayor)),
  class = c(">50K.")
)

finalMenor <- data.frame(
  number_people = c(auxMenor),
  capital.gain = c(rownames(auxMenor)),
  class = c("<=50K.")
)

final <- merge(finalMenor,finalMayor,all=TRUE)

#Grafico de barras en detalle
ggplot(data=final, aes(x=capital.gain, y=number_people, fill=class)) +
  geom_bar(stat="identity")

#Diagrma de densidad
ggplot(adult, aes(x=capital.gain, fill=class)) + geom_density(alpha=.4, size = 0.4)

#Conclusion de los datos:
#Esta grafica indica que hay mucha gente que no ganan nada, simplemente se mantiene
#economicamente con lo que cobra. Hay mucha mas gente que gana menos de 50.000 que 
#gente que gana mas de 50.000.

#//////////////////////////////////capital.loss y class//////////////////////////////////

#Generamos unas tablas 
auxMayor <- table(ClassMayor$capital.loss)
auxMenor <- table(ClassMenor$capital.loss)

finalMayor <- data.frame(
  number_people = c(auxMayor),
  capital.loss = c(rownames(auxMayor)),
  class = c(">50K.")
)

finalMenor <- data.frame(
  number_people = c(auxMenor),
  capital.loss = c(rownames(auxMenor)),
  class = c("<=50K.")
)

final <- merge(finalMenor,finalMayor,all=TRUE)

#Grafico de barras en detalle
ggplot(data=final, aes(x=capital.loss, y=number_people, fill=class)) +
  geom_bar(stat="identity")

#Diagrma de densidad
ggplot(adult, aes(x=capital.loss, fill=class)) + geom_density(alpha=.4, size = 0.4)

#Conclusion de los datos:
#Esta grafica concuerda con la anterior ya que hay mucha mas gente que no pierde nada,
#que simplenente se sustienta con lo que gana. 
#Esta grafica tambien indica que la gente que pierde mas de 1590 dolares, tiene mas de 50.000

#//////////////////////////////////hours.per.week y class//////////////////////////////////

#Generamos unas tablas 
auxMayor <- table(ClassMayor$hours.per.week)
auxMenor <- table(ClassMenor$hours.per.week)

finalMayor <- data.frame(
  number_people = c(auxMayor),
  hours.per.week = c(rownames(auxMayor)),
  class = c(">50K.")
)

finalMenor <- data.frame(
  number_people = c(auxMenor),
  hours.per.week = c(rownames(auxMenor)),
  class = c("<=50K.")
)

final <- merge(finalMenor,finalMayor,all=TRUE)

#Histograma sencillo
ggplot(adult, aes(x=hours.per.week, fill=class)) +
  geom_histogram(binwidth=.5, position="identity") 

#Grafico de barras en detalle
ggplot(data=final, aes(x=hours.per.week, y=number_people, fill=class)) +
  geom_bar(stat="identity")

#Diagrma de densidad
ggplot(adult, aes(x=hours.per.week, fill=class)) + geom_density(alpha=.4, size = 0.4)

#Conclusion de los datos:
#Esta grafica indica que el numero de horas mas comunes de trabajo/semana es 40, con 6000
#personas. 
#La gente normalmente gana menos de 50.000, pero el numero de gente que gana mas de 50.000 aumenta 
#a la vez que aumentan las horas trabajadas por semana, hasta las 59. 


#//////////////////////////////////native.country y class//////////////////////////////////

#Generamos unas tablas 
auxMayor <- table(ClassMayor$native.country)
auxMenor <- table(ClassMenor$native.country)

finalMayor <- data.frame(
  number_people = c(auxMayor),
  native.country = c(rownames(auxMayor)),
  class = c(">50K.")
)

finalMenor <- data.frame(
  number_people = c(auxMenor),
  native.country = c(rownames(auxMenor)),
  class = c("<=50K.")
)

final <- merge(finalMenor,finalMayor,all=TRUE)

#Grafico de barras en detalle
ggplot(data=final, aes(x=native.country, y=number_people, fill=class)) +
  geom_bar(stat="identity")

#Conclusion de los datos:
#El pais natal mas comun es America
#El numero de gente que tiene mas de 50.000 es mucho menor a el numero de gente que tiene menos de 50.000.


#2. Obtenga las medidas de tendencia central de cada una de las variables numéricas para cada una de las dos
#posibles clases. ¿Existe diferencia? (1 punto)

#Clases numericas: Age, education.num, capital.gain, capital.loss y hours.per.week

#Utilizo esta funcion para calcular la moda
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#Mean(), median() and getmode()

ClassMayor <- adult[grep(">50K.",adult$class), ]
ClassMenor <- adult[grep("<=50K.",adult$class), ]

#//////////////////////////////////Age//////////////////////////////////

#Mean
meanMayor <- mean(ClassMayor$Age)
meanMenor <- mean(ClassMenor$Age)

#Median
medianMayor <- median(ClassMayor$Age)
medianMenor <- median(ClassMenor$Age)

#Mode
modeMayor <- getmode(ClassMayor$Age)
modeMenor <- getmode(ClassMenor$Age)

cat(" Media de >50K.:",  meanMayor,   "| Media de <=50K.:",   meanMenor,   "\n",
    "Mediana de >50K.:", medianMayor, "    | Mediana de <=50K.:", medianMenor, "\n",
    "Moda de >50K.:",    modeMayor,   "       | Moda de <=50K.:", modeMenor, "\n",
    "Los valores son diferentes en todos los casos")

#Conclusion de los datos:
#La media, mediana y moda de los mayores de 50.000 estan muy cerca mientras 
#que la moda de menos de 50.000 esta muy separada de su mediana y su media. 

#//////////////////////////////////education.num//////////////////////////////////

#Mean
meanMayor <- mean(ClassMayor$education.num)
meanMenor <- mean(ClassMenor$education.num)

#Median
medianMayor <- median(ClassMayor$education.num)
medianMenor <- median(ClassMenor$education.num)

#Mode
modeMayor <- getmode(ClassMayor$education.num)
modeMenor <- getmode(ClassMenor$education.num)

cat(" Media de >50K.:",  meanMayor,   "| Media de <=50K.:",   meanMenor,   "\n",
    "Mediana de >50K.:", medianMayor, "    | Mediana de <=50K.:", medianMenor, "\n",
    "Moda de >50K.:",    modeMayor,   "       | Moda de <=50K.:", modeMenor, "\n",
    "Los valores son diferentes en todos los casos")

#Conclusion de los datos:
#La media, mediana y moda se relacionan en ambos casos.

#//////////////////////////////////capital.gain//////////////////////////////////

#Mean
meanMayor <- mean(ClassMayor$capital.gain)
meanMenor <- mean(ClassMenor$capital.gain)

#Median
medianMayor <- median(ClassMayor$capital.gain)
medianMenor <- median(ClassMenor$capital.gain)

#Mode
modeMayor <- getmode(ClassMayor$capital.gain)
modeMenor <- getmode(ClassMenor$capital.gain)

cat(" Media de >50K.:",  meanMayor,   "| Media de <=50K.:",   meanMenor,   "\n",
    "Mediana de >50K.:", medianMayor, "    | Mediana de <=50K.:", medianMenor, "\n",
    "Moda de >50K.:",    modeMayor,   "       | Moda de <=50K.:", modeMenor, "\n",
    "Los valores son diferentes en la media, pero iguales en la mediana y la moda")


#//////////////////////////////////capital.loss//////////////////////////////////

#Mean
meanMayor <- mean(ClassMayor$capital.loss)
meanMenor <- mean(ClassMenor$capital.loss)

#Median
medianMayor <- median(ClassMayor$capital.loss)
medianMenor <- median(ClassMenor$capital.loss)

#Mode
modeMayor <- getmode(ClassMayor$capital.loss)
modeMenor <- getmode(ClassMenor$capital.loss)

cat(" Media de >50K.:",  meanMayor,   "| Media de <=50K.:",   meanMenor,   "\n",
    "Mediana de >50K.:", medianMayor, "    | Mediana de <=50K.:", medianMenor, "\n",
    "Moda de >50K.:",    modeMayor,   "       | Moda de <=50K.:", modeMenor, "\n",
    "Los valores son diferentes en la media, pero iguales en la mediana y la moda")

#//////////////////////////////////hours.per.week//////////////////////////////////

#Mean
meanMayor <- mean(ClassMayor$hours.per.week)
meanMenor <- mean(ClassMenor$hours.per.week)

#Median
medianMayor <- median(ClassMayor$hours.per.week)
medianMenor <- median(ClassMenor$hours.per.week)

#Mode
modeMayor <- getmode(ClassMayor$hours.per.week)
modeMenor <- getmode(ClassMenor$hours.per.week)

cat(" Media de >50K.:",  meanMayor,   "| Media de <=50K.:",   meanMenor,   "\n",
    "Mediana de >50K.:", medianMayor, "    | Mediana de <=50K.:", medianMenor, "\n",
    "Moda de >50K.:",    modeMayor,   "       | Moda de <=50K.:", modeMenor, "\n",
    "Los valores son diferentes en la media, pero iguales en la mediana y la moda")

#Conclusion de los datos:
#Que la mediana y la moda sean iguales en los que sacan mas de 50.000 y menos de 50.000
#nos indica que el numero de horas no esta directamente relacionada con los ingresos.

#3. Obtenga los cuartiles y los percentiles de cada una de las variables numéricas para cada una de las dos
#posisbles clases. ¿Existe diferencia? (1 punto)

#Clases numericas: Age, education.num, capital.gain, capital.loss y hours.per.week

#quantile()

ClassMayor <- adult[grep(">50K.",adult$class), ]
ClassMenor <- adult[grep("<=50K.",adult$class), ]

#//////////////////////////////////Age//////////////////////////////////

#Quantile
quantileMayor <- quantile(ClassMayor$Age, c(.25,.50,.75))
quantileMenor <- quantile(ClassMenor$Age, c(.25,.50,.75))

#Percentile
percentileMayor <- quantile(ClassMayor$Age, c(.33,.66))
percentileMenor <- quantile(ClassMenor$Age, c(.33,.66))

cat("                    25% 50% 75%                    25% 50% 75%",                          "\n",
    "Cuartiles de >50K.:",  quantileMayor,   "| Cuartiles de <=50K.:",       quantileMenor,    "\n",
    "                     33% 66%                            33% 66%",                         "\n",
    "Percentiles de >50K.:", percentileMayor, "    | Percentiles de <=50K.:", percentileMenor, "\n",
    "Los valores son diferentes en todos los casos")

#//////////////////////////////////education.num//////////////////////////////////

#Quantile
quantileMayor <- quantile(ClassMayor$education.num, c(.25,.50,.75))
quantileMenor <- quantile(ClassMenor$education.num, c(.25,.50,.75))

#Percentile
percentileMayor <- quantile(ClassMayor$education.num, c(.33,.66))
percentileMenor <- quantile(ClassMenor$education.num, c(.33,.66))

cat("                    25% 50% 75%                    25% 50% 75%",                          "\n",
    "Cuartiles de >50K.:",  quantileMayor,   "| Cuartiles de <=50K.:",       quantileMenor,    "\n",
    "                     33% 66%                            33% 66%",                         "\n",
    "Percentiles de >50K.:", percentileMayor, "    | Percentiles de <=50K.:", percentileMenor, "\n",
    "Los valores son iguales en el 1� y 2� cuartil de <=50K.")

#//////////////////////////////////capital.gain//////////////////////////////////

#Quantile
quantileMayor <- quantile(ClassMayor$capital.gain, c(.25,.50,.75))
quantileMenor <- quantile(ClassMenor$capital.gain, c(.25,.50,.75))

#Percentile
percentileMayor <- quantile(ClassMayor$capital.gain, c(.33,.66))
percentileMenor <- quantile(ClassMenor$capital.gain, c(.33,.66))

cat("                   25% 50% 75%                 25% 50% 75%",                          "\n",
    "Cuartiles de >50K.:",  quantileMayor,   "| Cuartiles de <=50K.:",       quantileMenor,    "\n",
    "                     33% 66%                         33% 66%",                         "\n",
    "Percentiles de >50K.:", percentileMayor, "    | Percentiles de <=50K.:", percentileMenor, "\n",
    "Los valores son iguales en todos los casos")

#//////////////////////////////////capital.loss//////////////////////////////////

#Quantile
quantileMayor <- quantile(ClassMayor$capital.loss, c(.25,.50,.75))
quantileMenor <- quantile(ClassMenor$capital.loss, c(.25,.50,.75))

#Percentile
percentileMayor <- quantile(ClassMayor$capital.loss, c(.33,.66))
percentileMenor <- quantile(ClassMenor$capital.loss, c(.33,.66))

cat("                   25% 50% 75%                 25% 50% 75%",                          "\n",
    "Cuartiles de >50K.:",  quantileMayor,   "| Cuartiles de <=50K.:",       quantileMenor,    "\n",
    "                     33% 66%                         33% 66%",                         "\n",
    "Percentiles de >50K.:", percentileMayor, "    | Percentiles de <=50K.:", percentileMenor, "\n",
    "Los valores son iguales en todos los casos")

#//////////////////////////////////hours.per.week//////////////////////////////////

#Quantile
quantileMayor <- quantile(ClassMayor$hours.per.week, c(.25,.50,.75))
quantileMenor <- quantile(ClassMenor$hours.per.week, c(.25,.50,.75))

#Percentile
percentileMayor <- quantile(ClassMayor$hours.per.week, c(.33,.66))
percentileMenor <- quantile(ClassMenor$hours.per.week, c(.33,.66))

cat("                    25% 50% 75%                    25% 50% 75%",                          "\n",
    "Cuartiles de >50K.:",  quantileMayor,   "| Cuartiles de <=50K.:",       quantileMenor,    "\n",
    "                     33% 66%                            33% 66%",                         "\n",
    "Percentiles de >50K.:", percentileMayor, "    | Percentiles de <=50K.:", percentileMenor, "\n",
    "Los valores son iguales en el 1� y 2� cuartil de >50K., en el 2� y 3� cuartil de <=50K. y los percentiles de <=50K.")


#4. Para las variables numéricas y para cada unas de las clases, calcule las medidas de dispersión siguientes:
#  (a) recorrido, (b) recorrido inter-cuantílico y (c) varianza. ¿Existe diferencia? (1 punto)

#Clases numericas: Age, education.num, capital.gain, capital.loss y hours.per.week

#range(), IQR() and var()

ClassMayor <- adult[grep(">50K.",adult$class), ]
ClassMenor <- adult[grep("<=50K.",adult$class), ]

#//////////////////////////////////Age//////////////////////////////////

#Range
rangeMayor <- range(ClassMayor$Age)
rangeMenor <- range(ClassMenor$Age)

#IQR
IQRMayor <- IQR(ClassMayor$Age)
IQRMenor <- IQR(ClassMenor$Age)

#Var
varMayor <- var(ClassMayor$Age)
varMenor <- var(ClassMenor$Age)

cat(" Recorrido de >50K.:",                 rangeMayor,"              | Recorrido de <=50K.:",       rangeMenor, "\n",
    "Recorrido inter-cuantilico de >50K.:", IQRMayor,     "| Recorrido inter-cuantilico de <=50K.:", IQRMenor,   "\n",
    "Varianza de >50K.:",                   varMayor,     "            | Varianza de <=50K.:",       varMenor,   "\n",
    "Los valores son iguales en recorrido entre >50K. y <=50K.") 

#//////////////////////////////////education.num//////////////////////////////////

#Range
rangeMayor <- range(ClassMayor$education.num)
rangeMenor <- range(ClassMenor$education.num)

#IQR
IQRMayor <- IQR(ClassMayor$education.num)
IQRMenor <- IQR(ClassMenor$education.num)

#Var
varMayor <- var(ClassMayor$education.num)
varMenor <- var(ClassMenor$education.num)

cat(" Recorrido de >50K.:",                 rangeMayor,"              | Recorrido de <=50K.:",       rangeMenor, "\n",
    "Recorrido inter-cuantilico de >50K.:", IQRMayor,     "| Recorrido inter-cuantilico de <=50K.:", IQRMenor,   "\n",
    "Varianza de >50K.:",                   varMayor,     "           | Varianza de <=50K.:",       varMenor,   "\n",
    "Los valores son iguales en recorrido entre >50K. y <=50K.") 

#//////////////////////////////////capital.gain//////////////////////////////////

#Range
rangeMayor <- range(ClassMayor$capital.gain)
rangeMenor <- range(ClassMenor$capital.gain)

#IQR
IQRMayor <- IQR(ClassMayor$capital.gain)
IQRMenor <- IQR(ClassMenor$capital.gain)

#Var
varMayor <- var(ClassMayor$capital.gain)
varMenor <- var(ClassMenor$capital.gain)

cat(" Recorrido de >50K.:",                 rangeMayor,"           | Recorrido de <=50K.:",       rangeMenor, "\n",
    "Recorrido inter-cuantilico de >50K.:", IQRMayor,     "| Recorrido inter-cuantilico de <=50K.:", IQRMenor,   "\n",
    "Varianza de >50K.:",                   varMayor,     "          | Varianza de <=50K.:",       varMenor,   "\n",
    "Los valores son diferentes en todos los casos") 

#//////////////////////////////////capital.loss//////////////////////////////////

#Range
rangeMayor <- range(ClassMayor$capital.loss)
rangeMenor <- range(ClassMenor$capital.loss)

#IQR
IQRMayor <- IQR(ClassMayor$capital.loss)
IQRMenor <- IQR(ClassMenor$capital.loss)

#Var
varMayor <- var(ClassMayor$capital.loss)
varMenor <- var(ClassMenor$capital.loss)

cat(" Recorrido de >50K.:",                 rangeMayor,"            | Recorrido de <=50K.:",       rangeMenor, "\n",
    "Recorrido inter-cuantilico de >50K.:", IQRMayor,     "| Recorrido inter-cuantilico de <=50K.:", IQRMenor,   "\n",
    "Varianza de >50K.:",                   varMayor,     "           | Varianza de <=50K.:",       varMenor,   "\n",
    "Los valores son diferentes en todos los casos") 

#//////////////////////////////////hours.per.week//////////////////////////////////

#Range
rangeMayor <- range(ClassMayor$hours.per.week)
rangeMenor <- range(ClassMenor$hours.per.week)

#IQR
IQRMayor <- IQR(ClassMayor$hours.per.week)
IQRMenor <- IQR(ClassMenor$hours.per.week)

#Var
varMayor <- var(ClassMayor$hours.per.week)
varMenor <- var(ClassMenor$hours.per.week)

cat(" Recorrido de >50K.:",                 rangeMayor,"               | Recorrido de <=50K.:",       rangeMenor, "\n",
    "Recorrido inter-cuantilico de >50K.:", IQRMayor,     "| Recorrido inter-cuantilico de <=50K.:", IQRMenor,   "\n",
    "Varianza de >50K.:",                   varMayor,     "            | Varianza de <=50K.:",       varMenor,   "\n",
    "Los valores son iguales en recorrido entre >50K. y <=50K.") 


#5. Con respecto a las medidas de asimetría, ¿que podríamos indicar de la distribución de cada una de las
#variables númericas de las dos clases? (1 punto)

#Clases numericas: Age, education.num, capital.gain, capital.loss y hours.per.week

#skewness() del paquete "e1071"

ClassMayor <- adult[grep(">50K.",adult$class), ]
ClassMenor <- adult[grep("<=50K.",adult$class), ]

#//////////////////////////////////Age//////////////////////////////////

#Skewness
skewnessMayor <- skewness(ClassMayor$Age)
skewnessMenor <- skewness(ClassMenor$Age)

cat(" Distribucion de >50K.:",  skewnessMayor,   "| Distribucion de <=50K.:",       skewnessMenor,    "\n",
    "Los valores nos permiten saber que >50K. tiene asimetria a la derecha y <=50K. tambien")

#//////////////////////////////////education.num//////////////////////////////////

#Skewness
skewnessMayor <- skewness(ClassMayor$education.num)
skewnessMenor <- skewness(ClassMenor$education.num)

cat(" Distribucion de >50K.:",  skewnessMayor,   "| Distribucion de <=50K.:",       skewnessMenor,    "\n",
    "Los valores nos permiten saber que >50K. tiene asimetria a la izquierda y <=50K. tambien")

#//////////////////////////////////capital.gain//////////////////////////////////

#Skewness
skewnessMayor <- skewness(ClassMayor$capital.gain)
skewnessMenor <- skewness(ClassMenor$capital.gain)

cat(" Distribucion de >50K.:",  skewnessMenor,   "| Distribucion de <=50K.:",       skewnessMenor,    "\n",
    "Los valores nos permiten saber que >50K. tiene asimetria a la derecha y <=50K. tambien")

#//////////////////////////////////capital.loss//////////////////////////////////

#Skewness
skewnessMayor <- skewness(ClassMayor$capital.loss)
skewnessMenor <- skewness(ClassMenor$capital.loss)

cat(" Distribucion de >50K.:",  skewnessMenor,   "| Distribucion de <=50K.:",       skewnessMenor,    "\n",
    "Los valores nos permiten saber que >50K. tiene asimetria a la derecha y <=50K. tambien")

#//////////////////////////////////hours.per.week//////////////////////////////////

#Skewness
skewnessMayor <- skewness(ClassMayor$hours.per.week)
skewnessMenor <- skewness(ClassMenor$hours.per.week)

cat(" Distribucion de >50K.:",  skewnessMenor,   "| Distribucion de <=50K.:",       skewnessMenor,    "\n",
    "Los valores nos permiten saber que >50K. tiene asimetria a la derecha y <=50K. tambien")

#6. Realice un ajuste de mínimos cuadrados entre la variable horas por semana y nivel de educación. Indique
#la precisión del ajuste. (2.5 punto)

#lm(), cor()

ClassMayor <- adult[grep(">50K.",adult$class), ]
ClassMenor <- adult[grep("<=50K.",adult$class), ]

#Fitting Linear Models
hours.per.week <- c(adult$hours.per.week)
education.num <- c(adult$education.num)
plot(hours.per.week, education.num)

ajus <- lm(hours.per.week ~ education.num)
ajus
abline(ajus, col = 2)
abline(15.06, -0.048, lty=2, col = 4)
legend(40, 14.5, c("l�nea de regresion"), lty = c(1))

#Correlation
precision <- cor(hours.per.week, education.num)
cat(" La precision del ajuste es:",  precision, ", lo que nos indica que si hay una correlacion 
 entre los valores de hours.per.week y education.num, aunque esta bastante cerca de 0 por 
 lo que hay una correlacion minima.")
    
    