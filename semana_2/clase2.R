library(dslabs)
library(dplyr)
library(tidyverse)


co2 ## es tidy?                                                         
## no es tidy: para ser tidy tendríamos que cambiarle la forma 
# para tener tres columnas (año, mes y valor), y entonces cada
# observación de CO2 tendría una fila.


ChickWeight ## es tidy?          
## es tidy: cada observación (un peso) está representada por una fila.











## El paquete dplyr del tidyverse ofrece funciones que realizan algunas 
## de las operaciones más comunes cuando se trabaja con data frames








## Añadir columnas con "mutate"



## La función mutate toma el data frame como primer argumento y el nombre y 
## los valores de la variable como segundo argumento usando la convención 
## name = values.


data(murders)

murders

murders <- mutate(murders, rate = total/ population * 100000)

murders






## Crear subconjuntos con "filter"

## Si queremos filtrar la tabla de datos para mostrar solo las entradas 
## para las cuales la tasa de asesinatos es inferior a 0.71.

filter(murders, rate <= 0.71)









filter(murders, region == "West")








##  Seleccionar columnas con "select"

new_table <- select(murders, state, region, rate)

filter(new_table, rate <= 0.71)


murders

## Podemos eliminar filas usando el operador "!="

no_florida <- filter(murders, state != "Florida")

no_florida







## También podemos usar %in% para filtrar con dplyr.

filter(murders, state %in% c("New York", "Texas"))






## filtramos para mantener solo estados pequeños 
## en la región noreste.

filter(murders, population < 5000000 & region == "Northeast")







########################################################################################################

########### El pipe



##El pipe envía el resultado que se encuentra a su lado izquierdo

16 %>% sqrt()





16 %>% sqrt() %>% log2() #### es equivalente a log2(sqrt(16))









## Al usar el pipe con data frames y dplyr, ya no necesitamos especificar 
#el primer argumento requerido 
## puesto que las funciones dplyr que hemos descrito toman todos
#los datos como el primer argumento.







murders %>% select(state, region, rate) %>% filter(rate <= 0.71)










murders %>% select(state, region, rate) %>% filter(region == "North Central")  %>% filter(rate <= 1)  







############################# ######################################
data(murders)
murders

murders <- mutate(murders, rate = total/ population * 100000, rank = rank(-rate))
murders








mutate(murders, rate = total/ population * 100000,
       rank = rank(-rate))  %>%
  select(state, rate, rank)




## Note que select ya no tiene un data frame como primer argumento. Se supone que el primer argumento 
## sea el resultado de la operación realizada justo antes de  %>%




######################## EJERCICIO ###############################


my_states <- murders %>%
  mutate (murders, rate ) %>%
  filter (rate <= 1, region == "Northeast") %>%
  select (state, region, rate, rank) 

my_states




######### TAREA ##################


# 1
data("murders")

new_names <- murders %>% select(abb) %>% filter(nchar(murders$state)>8)
new_names

# 2

compute_sn <- function(n){
  x <- 1:n
  y <- x^2
  sum(y)
}

# a)

s_n <- vector("numeric",25)
for(i in 1:25){
  s_n[i] <- compute_sn(i)
}

s_n

# b)

s_n <- sapply(1:25, compute_sn)
s_n

# c)
s_n <- map_dbl(1:25, compute_sn)
s_n

# d)

n <- 1:25
plot(n,s_n)


# 4
# a)
murders <- mutate(murders, rate =  total/ population * 100000)

murders

 
# b)

murders <- mutate(murders, rank = rank (-rate))
murders

# c) 

murders %>% filter(rank %in% 1:5) %>% select(state,rank)

# d)

no_south <- murders %>% filter(region != "South")
no_south


# 5


my_states <- murders %>%
  mutate ( rate =   total/ population * 100000,  rank = rank (-rate)) %>%
  filter (rate <= 1.7 & (region == "Northeast"| region == "West") ) %>%
  select (state, rate, rank) 

my_states







