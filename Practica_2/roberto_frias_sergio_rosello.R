
if (!"package:ggplot2" %in% search() ||!"package:grid" %in% search()) { 
  install.packages("ggplot2")
  install.packages("grid")
}

require(ggplot2)
require(grid)



#-----Modelos unidimensionales discretos-----:
#1. Distribución Binomial
#Si un estudiante responde al azar a un examen de 8 preguntas de verdadero o falso.

#Representacion grafica
x1  <- 0:8
df <- data.frame(x = x1, y = dbinom(x1, size=8, prob=0.5))

plot1 <- ggplot(df, aes(x = x, y = y)) + geom_bar(stat = "identity", col = "blue", fill = "blue", alpha = 0.5) + 
  scale_y_continuous(expand = c(0.01, 0)) + xlab("Preguntas acertadas") + ylab("Probabilidad") + 
  labs(title = "Probabilidad de acertar") + theme_bw(16, "serif") + 
  theme(plot.title = element_text(size = rel(1.2), vjust = 1.5))

print(plot1)

#   1. ¿Cuál es la probabilidad de que acierte 4? (1 punto)

#Formula
dbinom(4, size=8, prob=0.5)

#   2. ¿Cuál es la probabilidad de que acierte 2 o menos? (1 punto)

#Formula
pbinom(2, size=8, prob=0.5)

#   3. ¿Cuál es la probabilidad de que acierte 5 o más? (1 punto)

#Formula
pbinom(4, size=8, prob=0.5, lower.tail = FALSE)

#2. Distribución de Poisson
#Una determinada región de Estados Unidos es afectada, en promedio, por 6 huracanes al año. Si se
#define la variable X = "número de huracanes por año" y se sabe que ésta se distribuye mediante una
#Poisson, debido a que describe el número de éxitos por unidad de tiempo y porque son
#independientes del tiempo desde el último evento.
#Encuentre la probabilidad de que en un determinado año esta área sea afectada por:

#Representacion grafica
x1  <- 0:15
df <- data.frame(x = x1, y = dpois(x1,6))

plot1 <- ggplot(df, aes(x = x, y = y)) + geom_bar(stat = "identity", col = "blue", fill = "blue", alpha = 0.5) + 
  scale_y_continuous(expand = c(0.01, 0)) + xlab("Nº huracanes") + ylab("Probabilidad") + 
  labs(title = "Probabilidad de huracan en region") + theme_bw(16, "serif") + 
  theme(plot.title = element_text(size = rel(1.2), vjust = 1.5))

print(plot1)

#   1. Menos de 4 huracanes. (1 punto)

#Formula
ppois(3, lambda = 6)

#   2. Entre 6 y 8 huracanes. (1 punto)

#Formula
result1 <- ppois(8, 6) 
result2 <- ppois(5, 6)
result <- result1 - result2
result

#3. Distribución Geométrica
#Un vendedor de alarmas de hogar tiene éxito en una de cada diez casas que visita. Calcule:
#   1. La probabilidad de que en día determinado consiga vender la primera alarma en la sexta casa que
#      visita. (Nota: Habrá que calcular la probabilidad de que tenga 5 fracasos antes del primer éxito).(1 punto)

#Representacion grafica
x1  <- 1:10
df <- data.frame(x = x1, y = pgeom(x1,1/10))

plot1 <- ggplot(df, aes(x = x, y = y)) + geom_bar(stat = "identity", col = "blue", fill = "blue", alpha = 0.5) + 
  scale_y_continuous(expand = c(0.01, 0)) + xlab("Nº casas visitadas") + ylab("Probabilidad") + 
  labs(title = "Probabilidad de venta") + theme_bw(16, "serif") + 
  theme(plot.title = element_text(size = rel(1.2), vjust = 1.5))

print(plot1)

#Formula
dgeom(5, prob = 1/10)


#-----Modelos unidimensionales continuos-----:
#6. Distribución Normal
#7. Distribución T de Student
#Trabajando directamente en R, para calcular los cuantiles normales se usaría la función qnorm y
#sus parámetros necesarios. Por ejemplo para hallar el valor de la abscisa que en una N(0,1)
#deje en la cola izquierda una probabilidad de 0,25:
#  qnorm(0.25, mean = 0, sd = 1, lower.tail = TRUE)
#Nota: lower.tail = TRUE utiliza la cola de la izquierda, mientras que lower.tail = FALSE usa la
#derecha. Los parámetros mean = 0 y sd = 1 pueden ser omitidos, pues son los valores por defecto
#en esta función.
#Una empresa está buscando personal para su departamento de innovación. Se solicitan perfiles
#extrovertidos y creativos. Se han presentado 50 candidatos y el criterio de selección es que superen
#el percentil 80 en creatividad y extroversión. Sabiendo que la variable extroversión (X) se distribuye
#según una Normal de media 5 y desviación típica 1, que la variable creatividad (Y) sigue una TStudent
#de 10 grados de libertad y que las puntuaciones de creatividad y extroversión son
#independientes:
#   1. ¿Cuantos candidatos serán seleccionados? (1 punto)

porcentaje1 <- 1 - 0.8
porcentaje2 <- 1 - 0.8

porcentaje <- porcentaje1 * porcentaje2

result <- porcentaje * 50
result

#   2. Dibuje las gráficas de densidad de las variables extroversión y creatividad. (1 punto)

#Densidad extroversion
dnorm(.8, mean=5, sd=1)

dat <- data.frame(rating = c(dnorm(.8, mean=5, sd=1)))

plot1 <- ggplot(dat, aes(x=rating)) + geom_density(col = "blue", fill = "blue", alpha = 0.5) +
  xlab("") + 
  labs(title = "Densidad extroversion") + theme_bw(16, "serif") + 
  theme(plot.title = element_text(size = rel(1.2), vjust = 1.5))

print(plot1)

#Densidad creatividad
dt(.8, df=10)

dat <- data.frame(variables = factor(rep(c("Creatividad"), each=50)), 
                  rating = c(dt(.8, df=10)))

plot1 <- ggplot(dat, aes(x=rating)) + geom_density(col = "blue", fill = "blue", alpha = 0.5) +
  xlab("") + 
  labs(title = "Densidad creatividad") + theme_bw(16, "serif") + 
  theme(plot.title = element_text(size = rel(1.2), vjust = 1.5))

print(plot1)


#8. Distribución Chi-cuadrado
#La variable X sigue una distribución Chi-cuadrado con 28 grados de libertad.
#   1. Calcule la probabilidad de que X sea mayor de 7,5. (1 punto)
#      NOTA: Tenga especial cuidado en ver en que cola se debe calcular.

pchisq(c(7.5), df=28, lower.tail=FALSE)

#   2. Dibuje la gráfica de la función de densidad. (1 punto)

dat <- data.frame(variables = factor(rep(c("Creatividad"), each=200)), 
                  rating = c(dchisq(c(7.5), df=28)))

plot1 <- ggplot(dat, aes(x=rating)) + geom_density(col = "blue", fill = "blue", alpha = 0.5) +
  xlab("") + 
  labs(title = "Densidad chi-cuadrado") + theme_bw(16, "serif") + 
  theme(plot.title = element_text(size = rel(1.2), vjust = 1.5))

print(plot1)




