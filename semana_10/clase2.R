##############################
# Suavización
##############################

library(tidyverse)
library(dslabs)
library(lubridate)


contam <- read_csv("https://raw.githubusercontent.com/jamc88/Analisis-de-datos/main/AD_R_UAM/ozono23.csv")

contam







colnames(contam) <- c('Fecha','ozono')

n <- tibble(Fecha = ymd("2023-01-01"),
       ozono = 20.525)

contam <- bind_rows(n, contam)












contam |> ggplot(aes(Fecha, ozono)) +
  geom_line()



qplot(Fecha, ozono, data = contam)






# Queremos entender la forma de la tendencia



# línea de regresión

mu_x <- mean(1:120)
mu_y <- mean(contam$ozono)
s_x <- sd(1:120)
s_y <- sd(contam$ozono)
r <- cor(1:120, contam$ozono)

contam |>
  ggplot(aes(1:120, ozono)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = r * s_y/s_x, intercept = mu_y - r * s_y/s_x * mu_x)




# La idea general de la suavización es agrupar los puntos de datos x en estratos 
# en los que el valor de f(x) se puede suponer que es constante. 



# Podemos hacer esta suposición porque pensamos que f(x) cambia lentamente y, 
# como resultado, f(x) es casi constante en pequeñas ventanas de tiempo.





span <- 7
fit <- with(contam,
            ksmooth(Fecha, ozono, kernel = "box", bandwidth = span))

contam |> mutate(smooth = fit$y) |>
  ggplot(aes(Fecha, ozono)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(Fecha, smooth), color="red")











# Podemos atenuar esto algo tomando promedios ponderados que le dan al 
# punto central más peso que a los puntos lejanos

span <- 7
fit <- with(contam,
            ksmooth(Fecha, ozono, kernel = "normal", bandwidth = span))

contam |> mutate(smooth = fit$y) |>
  ggplot(aes(Fecha, ozono)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(Fecha, smooth), color="red")














# ggplot2 utiliza loess en su función geom_smooth:

contam |> ggplot(aes(Fecha, ozono)) +
  geom_point() +
  geom_smooth()


# Tengamos cuidado con los parámetros predeterminados 
# ya que rara vez son óptimos. 
# Afortunadamente, pueden cambiarlos fácilmente:


contam |> ggplot(aes(Fecha, ozono)) +
  geom_point() +
  geom_smooth(method = "loess", span = 0.2, method.args = list(degree=1))






## k-vecinos más cercanos

# utilizaremos los dos datos de dígitos predictores 





library(tidyverse)
library(dslabs)
library(caret)

data("mnist_27")



mnist_27$test|> ggplot(aes(x_1, x_2, color = y)) + geom_point()





# Con kNN estimamos p(x1,x2) de manera similar a la suavización 
# de compartimientos. 



# Primero, definimos la distancia entre todas las observaciones según 
# los atributos. Entonces, para cualquier punto (x1,x2) para el cual
# queremos un estimador de p(x1,x2),
# buscamos los k puntos más cercanos a (x1,x2) y calculamos el promedio 
# de los 0s y 1s asociados con estos puntos



# Nos referimos al conjunto de puntos utilizados para calcular el promedio
# como el vecindario

# esto nos da un estimador ^p(x1,x2), al igual que el suavizador de compartimiento 
# nos dio un estimador de una tendencia. 

# Podemos controlar la flexibilidad de nuestro estimador, en este caso a 
# través del parámetro k: 
# k's más grandes resultan en estimadores más suaves,
# k's más pequeñas resultan en estimadores más flexibles y más ondulados.



# podemos usar la función knn3 del paquete caret.
# Podemos llamarlo de una de dos maneras. Utilizaremos el primero en el que 
# especificamos una formula y un data frame. 
# El data frame contiene todos los datos que se utilizarán.
# La fórmula tiene la forma 
# outcome ~ predictor_1 + predictor_2 + predictor_3 y así sucesivamente.
# Por lo tanto, escribiríamos y ~ x_1 + x_2. 
# Si vamos a usar todos los predictores, podemos usar el . así y ~ ..



knn_fit <- knn3(y ~ ., data = mnist_27$train)



# Para esta función, también debemos elegir un parámetro: 
# el número de vecinos para incluir. 
# Comencemos con el valor predeterminado k=5.

knn_fit <- knn3(y ~ ., data = mnist_27$train, k = 5)

# La función predict para knn produce una probabilidad para cada clase.
# Mantenemos la probabilidad de ser un 7 como el estimador ^p(x1,x2):
  
y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")

confusionMatrix(y_hat_knn, mnist_27$test$y)$overall["Accuracy"]




# Anteriormente utilizamos la regresión lineal para generar un estimador.


fit_lm <- mnist_27$train |>
  mutate(y = ifelse(y == 7, 1, 0)) |>
  lm(y ~ x_1 + x_2, data = _)
p_hat_lm <- predict(fit_lm, mnist_27$test)
y_hat_lm <- factor(ifelse(p_hat_lm > 0.5, 7, 2))

confusionMatrix(y_hat_lm, mnist_27$test$y)$overall["Accuracy"]


# Y vemos que kNN, con el parámetro predeterminado, ya supera la regresión. 

























