library(tidyverse)
library(dslabs)



data(murders)


murders %>%
  ggplot(aes(population,total,label = abb, color = region)) + 
  geom_label()





######### Objetos
a <- 2
b <- -4
c <- 1


#### El espacio de trabajo
ls() ## ver todas las variables guardadas en el espacio de trabajo


sqrt(9)



(-b + sqrt(b^2 - 4*a*c) )/ ( 2*a )

(-b - sqrt(b^2 - 4*a*c) )/ ( 2*a )















### Funciones

log(8)
log(a)

help("log") #ayuda con la función
?log
args(log)

log( 8 , base = 2)




















### Objetos Predefinidos

data()

co2

pi

Inf +3











### Nombres de Variables


sol_1 <- (-b + sqrt(b^2 - 4*a*c) )/ ( 2*a )
sol_2 <- (-b - sqrt(b^2 - 4*a*c) )/ ( 2*a )

sol_1
sol_2






### Scripts +

a <- 3
b <- 2
c <- -1

sol_1 <- (-b + sqrt(b^2 - 4*a*c) )/ ( 2*a )
sol_2 <- (-b - sqrt(b^2 - 4*a*c) )/ ( 2*a )

sol_1
sol_2




### Ejercicios

(1000*(1001))/2

seq(100,200)


n <- 1000
x <- seq(1, n)
sum(x)

class(n)

a <- "naranja"

class(a)







#### Dataframes

class(murders)

str(murders) ###  obtener más información sobre la estructura de un objeto

head(murders) ### mostrar las primeras seis líneas

names(murders) ### acceso rápido a los nombres de las variables usando:

murders$population ## orden de las filas en nuestra tabla de datos

murders$abb


