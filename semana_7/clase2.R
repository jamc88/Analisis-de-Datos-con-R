


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




#----------------------------------------------------------------------------


#### Valor esperado condicional


# Queremos calcular la altura promedio del hijo condicionado en que el 
# padre mida 72 pulgadas. 

# Queremos estimar E(Y|X=72) utilizando la muestra recopilada por Galton. 




# El promedio de la muestra es el enfoque preferido para estimar el promedio de la población.
# Sin embargo, un reto al usar este enfoque para estimar los valores esperados condicionales
# es que para los datos continuos no tenemos muchos puntos de datos que coincidan 
# exactamente con un valor en nuestra muestra


sum(galton_heights$father == 72)








# Una forma práctica de mejorar estos estimadores de los valores esperados condicionales
# es definir estratos con valores similares de x. En nuestro ejemplo, podemos redondear 
# las alturas del padre a la pulgada más cercana y suponer que todas son 72 pulgadas

# predicción para el hijo de un padre que mide 72 pulgadas:


conditional_avg <- galton_heights |>
  filter(round(father) == 72) |>
  summarize(avg = mean(son)) |>
  pull(avg)

conditional_avg




# Si queremos hacer una predicción de cualquier altura, no solo 72 pulgadas, 
# podríamos aplicar el mismo enfoque a cada estrato. 

# La estratificación seguida por diagramas de caja nos permite ver la distribución
# de cada grupo:



galton_heights |> mutate(father_strata = factor(round(father))) |>
  ggplot(aes(father_strata, son)) +
  geom_boxplot() +
  geom_point()



################################################################################



# Aquí añadimos la línea de regresión a los datos originales:



mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)

galton_heights |>
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = r * s_y/s_x, intercept = mu_y - r * s_y/s_x * mu_x)



# La fórmula de regresión implica que si primero estandarizamos las variables, es decir,
# restamos el promedio y dividimos por la desviación estándar, entonces la línea de regresión 
# tiene un intercepto 0 y una pendiente igual a la correlación ρ. 
# Pueden hacer el mismo gráfico, pero usando unidades estándar así:


galton_heights |>
  ggplot(aes(scale(father), scale(son))) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = r)





## La regresión mejora la precisión



# Comparemos los dos enfoques de predicción que hemos presentado:
  
# * Redondear las alturas de los padres a la pulgada más cercana, estratificar y 
#  luego tomar el promedio.

# * Calcular la línea de regresión y usarla para predecir.


# Utilizamos una simulación Monte Carlo que muestrea N=50
# familias:

B <- 1000
N <- 50

set.seed(1983)
conditional_avg <- replicate(B, {
  dat <- sample_n(galton_heights, N)
  dat |> filter(round(father) == 72) |>
    summarize(avg = mean(son)) |>
    pull(avg)
})

regression_prediction <- replicate(B, {
  dat <- sample_n(galton_heights, N)
  mu_x <- mean(dat$father)
  mu_y <- mean(dat$son)
  s_x <- sd(dat$father)
  s_y <- sd(dat$son)
  r <- cor(dat$father, dat$son)
  mu_y + r*(72 - mu_x)/s_x*s_y
})

# Aunque el valor esperado de estas dos variables aleatorias es casi el mismo:


mean(conditional_avg, na.rm = TRUE)

mean(regression_prediction)



# El error estándar para la predicción usando la regresión es sustancialmente más pequeño:


sd(conditional_avg, na.rm = TRUE)

sd(regression_prediction)




# La línea de regresión es, por lo tanto, mucho más estable que la media condicional.




#####################################################################################


# Si creemos que los datos de altura están bien aproximados por la distribución normal 
# de dos variables, entonces deberíamos ver que la aproximación normal aplica a cada estrato.
#Aquí estratificamos las alturas de los hijos por las alturas estandarizadas de los padres 



galton_heights |>
  mutate(z_father = round((father - mean(father))/ sd(father))) |>
  filter(z_father %in% -2:2) |>
  ggplot() +
  stat_qq(aes(sample = son)) +
  facet_wrap( ~ z_father)



# Si la distribución de nuestros datos se puede aproximar con una distribución
# normal de dos variables, la línea de regresión da el valor esperado condicional. 
# Por lo tanto, podemos obtener un estimador mucho más estable del valor esperado 
# condicional al encontrar la línea de regresión y usarla para predecir.


# En resumen, si nuestros datos se pueden aproximar con una distribución normal 
# de dos variables, entonces el valor esperado condicional, la mejor predicción de Y
# cuando sabemos el valor de X, lo da la línea de regresión.

















################################################################################

# ¿Qué pasa si queremos predecir la altura del padre basada en la del hijo? 


# Necesitamos calcular E(X∣Y=y). Dado que los datos se aproximan mediante 
# una distribución normal de dos variables, la teoría descrita anteriormente nos dice 
# que este valor esperado condicional seguirá una línea con pendiente e intercepto:


mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)
m_1 <- r * s_y/ s_x
b_1 <- mu_y - m_1*mu_x

# Necesitamos calcular E(X∣Y=y). 

m_2 <- r * s_x/ s_y
b_2 <- mu_x - m_2 * mu_y

# E(X∣Y=y)=40.9+0.41y
# Aquí tenemos un gráfico que muestra las dos líneas de regresión. 
# La azul predice las alturas de los hijos según las alturas de los padres
# y la roja predice las alturas de los padres según las alturas de los hijos:

galton_heights |>
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = b_1, slope = m_1, col = "blue") +
  geom_abline(intercept = -b_2/m_2, slope = 1/m_2, col = "red")


















