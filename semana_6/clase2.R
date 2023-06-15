library(tidyverse)
library(dslabs)


data(heights)
x <- heights |> filter(sex=="Male") |> pull(height)







# Definimos la función de distribución acumulada como:

F <- function(a) mean(x<=a)
F




# para cualquier valor "a", F la proporción de valores en la lista x 
# que son más pequeños o iguales que a.

 


# ¿Si elijo a uno de los estudiantes varones al azar, cuál es la probabilidad 
# de que sea más alto que 70.5 pulgadas? 



sort(x)

1-F(70.48)




# la probabilidad de que un estudiante esté entre altura a y altura b es:

F(b)-F(a)


F(67)-F(60)





# La distribución acumulada para la distribución normal se define 
# mediante la función pnorm en R 

# F(a) = pnorm(a, m, s)





# Esto es útil porque si estamos dispuestos a usar la aproximación normal para,
# por ejemplo, la altura, no necesitamos todo el set de datos para responder a preguntas como:
# ¿cuál es la probabilidad de que un estudiante seleccionado al azar sea más alto que 70 pulgadas?
# Solo necesitamos la altura promedio y la desviación estándar:



m <- mean(x)
s <- sd(x)
1-pnorm(70.5, m, s)

1-F(70.48)





# la distribución normal es útil para aproximar la proporción de estudiantes que 
# indican valores en intervalos como los tres siguientes:


mean(x <= 68.5) - mean(x <= 67.5)

mean(x <= 69.5) - mean(x <= 68.5)

mean(x <= 70.5) - mean(x <= 69.5)


pnorm(68.5, m, s) - pnorm(67.5, m, s)

pnorm(69.5, m, s) - pnorm(68.5, m, s)

pnorm(70.5, m, s) - pnorm(69.5, m, s)




mean(x <= 70.9) - mean(x<=70.1)
pnorm(70.9, m, s) - pnorm(70.1, m, s)






heights |>
  filter(sex == "Male") |>
  ggplot(aes(height)) +
  geom_histogram(binwidth = 1, fill = "blue", col = "black") +
  xlab("Male heights in inches") +
  ggtitle("Histogram")













##### Simulaciones Monte Carlo para variables continuas


# R provee funciones para generar resultados normalmente distribuidos. 
# Específicamente, la función rnorm toma tres argumentos: tamaño,
# media  y desviación estándar  y produce números aleatorios.



n <- length(x)
m <- mean(x)
s <- sd(x)
simulated_heights <- rnorm(n, m, s)



hist(simulated_heights)




# Esta es una de las funciones más útiles en R, 
# ya que nos permite generar datos que imitan eventos naturales y responder 
# a preguntas relacionadas con lo que podría suceder por casualidad 
# al ejecutar simulaciones Monte Carlo.






# Por ejemplo, elegimos 800 hombres al azar, 
# ¿Cuál es la distribución de la persona más alta? 
# ¿Cuán raro es un hombre de 7 pies (un seven footer) en un grupo de 800 hombres?
# La siguiente simulación Monte Carlo nos ayuda a responder esa pregunta:


B <- 10000
tallest <- replicate(B, {
  simulated_data <- rnorm(800, m, s)
  max(simulated_data)
})





# Tener un seven footer es bastante raro:
mean(tallest >= 7*12)


hist(tallest)




