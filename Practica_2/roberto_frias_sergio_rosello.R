
if (!"package:ggplot2" %in% search() ||!"package:grid" %in% search()) { 
  install.packages("ggplot2")
  install.packages("grid")
}

require(ggplot2)
require(grid)



#-----Modelos unidimensionales discretos-----:
#1. Distribuci�n Binomial
#Si un estudiante responde al azar a un examen de 8 preguntas de verdadero o falso.

#Representacion grafica
x1  <- 0:8
df <- data.frame(x = x1, y = dbinom(x1, size=8, prob=0.5))

plot1 <- ggplot(df, aes(x = x, y = y)) + geom_bar(stat = "identity", col = "blue", fill = "blue", alpha = 0.5) + 
  scale_y_continuous(expand = c(0.01, 0)) + xlab("Preguntas acertadas") + ylab("Probabilidad") + 
  labs(title = "Probabilidad de acertar") + theme_bw(16, "serif") + 
  theme(plot.title = element_text(size = rel(1.2), vjust = 1.5))

print(plot1)

#   1. �Cu�l es la probabilidad de que acierte 4? (1 punto)

#Formula
dbinom(4, size=8, prob=0.5)

#   2. �Cu�l es la probabilidad de que acierte 2 o menos? (1 punto)

#Formula
pbinom(2, size=8, prob=0.5)

#   3. �Cu�l es la probabilidad de que acierte 5 o m�s? (1 punto)

#Formula
pbinom(4, size=8, prob=0.5, lower.tail = FALSE)

#2. Distribuci�n de Poisson
#Una determinada regi�n de Estados Unidos es afectada, en promedio, por 6 huracanes al a�o. Si se
#define la variable X = "n�mero de huracanes por a�o" y se sabe que �sta se distribuye mediante una
#Poisson, debido a que describe el n�mero de �xitos por unidad de tiempo y porque son
#independientes del tiempo desde el �ltimo evento.
#Encuentre la probabilidad de que en un determinado a�o esta �rea sea afectada por:

#Representacion grafica
x1  <- 0:15
df <- data.frame(x = x1, y = dpois(x1,6))

plot1 <- ggplot(df, aes(x = x, y = y)) + geom_bar(stat = "identity", col = "blue", fill = "blue", alpha = 0.5) + 
  scale_y_continuous(expand = c(0.01, 0)) + xlab("N� huracanes") + ylab("Probabilidad") + 
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

#3. Distribuci�n Geom�trica
#Un vendedor de alarmas de hogar tiene �xito en una de cada diez casas que visita. Calcule:
#   1. La probabilidad de que en d�a determinado consiga vender la primera alarma en la sexta casa que
#      visita. (Nota: Habr� que calcular la probabilidad de que tenga 5 fracasos antes del primer �xito).(1 punto)

#Representacion grafica
x1  <- 1:10
df <- data.frame(x = x1, y = pgeom(x1,1/10))

plot1 <- ggplot(df, aes(x = x, y = y)) + geom_bar(stat = "identity", col = "blue", fill = "blue", alpha = 0.5) + 
  scale_y_continuous(expand = c(0.01, 0)) + xlab("N� casas visitadas") + ylab("Probabilidad") + 
  labs(title = "Probabilidad de venta") + theme_bw(16, "serif") + 
  theme(plot.title = element_text(size = rel(1.2), vjust = 1.5))

print(plot1)

#Formula
dgeom(5, prob = 1/10)


#-----Modelos unidimensionales continuos-----:
#6. Distribuci�n Normal
#7. Distribuci�n T de Student
#Trabajando directamente en R, para calcular los cuantiles normales se usar�a la funci�n qnorm y
#sus par�metros necesarios. Por ejemplo para hallar el valor de la abscisa que en una N(0,1)
#deje en la cola izquierda una probabilidad de 0,25:
#  qnorm(0.25, mean = 0, sd = 1, lower.tail = TRUE)
#Nota: lower.tail = TRUE utiliza la cola de la izquierda, mientras que lower.tail = FALSE usa la
#derecha. Los par�metros mean = 0 y sd = 1 pueden ser omitidos, pues son los valores por defecto
#en esta funci�n.
#Una empresa est� buscando personal para su departamento de innovaci�n. Se solicitan perfiles
#extrovertidos y creativos. Se han presentado 50 candidatos y el criterio de selecci�n es que superen
#el percentil 80 en creatividad y extroversi�n. Sabiendo que la variable extroversi�n (X) se distribuye
#seg�n una Normal de media 5 y desviaci�n t�pica 1, que la variable creatividad (Y) sigue una TStudent
#de 10 grados de libertad y que las puntuaciones de creatividad y extroversi�n son
#independientes:
#   1. �Cuantos candidatos ser�n seleccionados? (1 punto)

porcentaje1 <- 1 - 0.8
porcentaje2 <- 1 - 0.8

porcentaje <- porcentaje1 * porcentaje2

result <- porcentaje * 50
result

#   2. Dibuje las gr�ficas de densidad de las variables extroversi�n y creatividad. (1 punto)

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


#8. Distribuci�n Chi-cuadrado
#La variable X sigue una distribuci�n Chi-cuadrado con 28 grados de libertad.
#   1. Calcule la probabilidad de que X sea mayor de 7,5. (1 punto)
#      NOTA: Tenga especial cuidado en ver en que cola se debe calcular.

pchisq(c(7.5), df=28, lower.tail=FALSE)

#   2. Dibuje la gr�fica de la funci�n de densidad. (1 punto)

dat <- data.frame(variables = factor(rep(c("Creatividad"), each=200)), 
                  rating = c(dchisq(c(7.5), df=28)))

plot1 <- ggplot(dat, aes(x=rating)) + geom_density(col = "blue", fill = "blue", alpha = 0.5) +
  xlab("") + 
  labs(title = "Densidad chi-cuadrado") + theme_bw(16, "serif") + 
  theme(plot.title = element_text(size = rel(1.2), vjust = 1.5))

print(plot1)




