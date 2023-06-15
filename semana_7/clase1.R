#########################
#### Variables aleatorias
#########################




# Las variables aleatorias son los resultados numéricos de procesos aleatorios. 
# Podemos generar fácilmente variables aleatorias 


# Por ejemplo, definir X a ser 1 si la cuenta (bead) es azul y 0 de lo contrario:

beads <- rep( c("red", "blue"), times = c(2,3))
X <- ifelse(sample(beads, 1) == "blue", 1, 0)


# Aquí X es una variable aleatoria: cada vez que seleccionamos una nueva cuenta,
# el resultado cambia aleatoriamente.








################################################################################


#############  Modelos de muestreo

# Supongan que un casino  los contratan para consultar si deben incluir 
# las ruedas de la ruleta entre sus juegos. 
# Suponemos que jugarán 1,000 personas y que la única apuesta que puedan hacer en 
# la ruleta es apostar en rojo o negro. El casino quiere que predigan cuánto dinero 
# ganarán o perderán. Quieren una gama de valores y, en particular, quieren saber 
# cuál es la probabilidad de perder dinero. Si esta probabilidad es demasiado alta,
# no instalaran ruedas de ruleta.

# Definimos una variable aleatoria S que represente las ganancias totales del casino.
# Comencemos por construir la urna. Una rueda de ruleta tiene 18 casillas rojas,
# 18 casillas negras y 2 verdes. Entonces, jugar un color en un juego de ruleta 
# es equivalente a escoger de la siguiente urna:

color <- rep(c("Black", "Red", "Green"), c(18, 18, 2))

# Los 1,000 resultados de 1,000 personas jugando son sorteos independientes de esta urna. 
# Si aparece el rojo, el jugador gana y el casino pierde un dólar, por lo que sacamos un -$1.
# De otra manera, el casino gana un dólar y sacamos un $1.
# Para construir nuestra variable aleatoria S, podemos usar este código:

n <- 1000
X <- sample(ifelse(color == "Red", -1, 1), n, replace = TRUE)
X[1:10]




# Como sabemos las proporciones de 1s y -1s, podemos generar las elecciones 
# con una línea de código, sin definir color:


X <- sample(c(-1,1), n, replace = TRUE, prob=c(9/19, 10/19))


# Llamamos a esto un modelo de muestreo ya que estamos modelando el comportamiento 
# aleatorio de la ruleta con el muestreo de elecciones de una urna. 
# Las ganancias totales S son simplemente la suma de estos 1,000 sorteos independientes:


X <- sample(c(-1,1), n, replace = TRUE, prob=c(9/19, 10/19))
S <- sum(X)
S




# La distribución de probabilidad de una variable aleatoria nos dice la probabilidad 
# de que el valor observado caiga en cualquier intervalo dado. 
# Entonces, por ejemplo, si queremos saber la probabilidad de que perdamos dinero, 
# estamos preguntando la probabilidad de que S<0.




# si podemos definir una función de distribución acumulativa F(a) = P(S<=a)
# entonces podremos responder a cualquier pregunta relacionada con 
# la probabilidad de eventos definidos por nuestra variable aleatoria S






# Podemos estimar la función de distribución para la variable aleatoria S 
# mediante el uso de una simulación Monte Carlo para generar muchas realizaciones
# de la variable aleatoria.



n <- 1000
B <- 10000
roulette_winnings <- function(n){
  X <- sample(c(-1,1), n, replace = TRUE, prob=c(9/19, 10/19))
  sum(X)
}
S <- replicate(B, roulette_winnings(n))


# ¿En nuestras simulaciones, con qué frecuencia obtuvimos sumas menores o iguales a a?
# mean(S <= a)
# ¿cuán probable es que perdamos dinero?
mean(S<0)







# El teorema del límite central (CLT) nos dice que cuando el número de sorteos, 
# también llamado tamaño de muestra, es grande, 
# la distribución de probabilidad de la suma de los sorteos independientes es 
# aproximadamente normal. 


# Usando el TLC, podemos omitir la simulación Monte Carlo y en su lugar calcular
# la probabilidad de que el casino pierda dinero usando esta aproximación:

mu <- n * (20-18)/38
se <- sqrt(n) * 2 * sqrt(90)/19
pnorm(0, mu, se)




################################################################################







########################################################
###### Regresión
########################################################


# Estos datos contienen alturas de varias docenas de familias: madres, padres, hijas e hijos.
# Para imitar el análisis de Galton, crearemos un set de datos con las alturas 
# de los padres y un hijo de cada familia seleccionado al azar:

# install.packages("HistData")


library(tidyverse)
library(HistData)
data("GaltonFamilies")



set.seed(1983)

galton_heights <- GaltonFamilies |>
  filter(gender == "male") |>
  group_by(family) |>
  sample_n(1) |>
  ungroup() |>
  select(father, childHeight) |>
  rename(son = childHeight)



galton_heights %>% ggplot(aes(father)) + geom_histogram(colour = 4, fill = "white",bins = 50)


# Supongan que nos piden que resumamos los datos de padre e hijo. 
# Dado que ambas distribuciones están  aproximadas por la distribución normal,
# podríamos usar los dos promedios y las dos desviaciones estándar como resúmenes:


galton_heights |>
  summarize(mean(father), sd(father), mean(son), sd(son))



# Sin embargo, este resumen no describe una característica importante de los datos: 
#  la tendencia de que entre más alto es el padre, más alto es el hijo.



galton_heights |> ggplot(aes(father, son)) +
  geom_point(alpha = 0.5)




#########################################################












