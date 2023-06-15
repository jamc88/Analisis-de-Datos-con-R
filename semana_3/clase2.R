library(dplyr)
library(dslabs)



library(NHANES)
data(NHANES)
NHANES




View(NHANES)


dim(NHANES)




# datos de la encuesta recopilada por el Centro Nacional de Estadísticas de Salud
# de Estados Unidos 





# Los datos NHANES tienen muchos valores faltantes.












#################### EJERCICIOS #####################

### 1
ref <- NHANES %>% 
  filter(Gender == "female" , AgeDecade == " 20-29") %>%
  summarize(average = mean(BPSysAve,na.rm = TRUE), standard_deviation = sd(BPSysAve, na.rm = TRUE))
ref




### 2
ref_avg <- ref %>% pull(average)

class(ref_avg)


### 3
NHANES %>% 
  filter(Gender == "female" , AgeDecade == " 20-29") %>%
  summarize(max = max(BPSysAve, na.rm=TRUE), min = min(BPSysAve, na.rm=TRUE))



### 4 
NHANES %>% 
  filter(Gender == "female") %>%
  group_by(AgeDecade) %>% 
  summarize(mean = mean(BPSysAve, na.rm=TRUE), sd=sd(BPSysAve, na.rm=TRUE))



############################################################

## Tibbles

data(murders)

murders %>% group_by(region) %>% class()





# El tbl es un tipo especial de data frame. Las funciones group_by 
# y summarize siempre devuelven este tipo de data frame.



# los tibbles son el formato preferido por el tidyverse y
# Los tibbles son muy similares a los data frames. De hecho, 
# pueden pensar en ellos como una versión moderna de data frames.

 

murders



as_tibble(murders)









# subconjuntos de tibbles son tibbles


# Si creamos subconjuntos de las columnas de un data frame,
# le pueden devolver un objeto que no es un data frame


class(murders[,4])

# Con tibbles, esto no sucede:
class(as_tibble(murders)[,4])







# Con tibbles, si desean acceder al vector que define una columna 
# y no recuperar un data frame, deben usar el operador de acceso $

class(as_tibble(murders)$population)




# tibbles les dará una advertencia si intentan acceder a una 
# columna que no existe.


murders$Population



as_tibble(murders)$Population



# Los tibbles pueden tener entradas complejas

# Si bien las columnas del data frame deben ser vectores de números,
# cadenas o valores lógicos, los tibbles pueden tener objetos más complejos, 
# como listas o funciones. 


tibble(id = c(1, 2, 3), func = c(mean, median, sd))






# La función group_by devuelve un tipo especial de tibble: un tibble agrupado. 
# Esta clase almacena información que les permite saber qué filas están en qué grupos. 



######### Cómo crear un tibble usando tibble en lugar de data.frame ##########


# Para crear un data frame en formato tibble, 
# pueden utilizar la función tibble.



grades <- tibble(names = c("John", "Juan", "Jean", "Yao"),
                 exam_1 = c(95, 80, 90, 85),
                 exam_2 = c(90, 85, 85, 90))
grades



# para un dataframe

grades <- data.frame(names = c("John", "Juan", "Jean", "Yao"),
                     exam_1 = c(95, 80, 90, 85),
                     exam_2 = c(90, 85, 85, 90))

grades






#para convertir un data frame en un tibble, usamos la función as_tibble


as_tibble(grades) %>% class()








# El paquete purrr


# El paquete purrr incluye funciones similares a sapply, 
# pero que interactúan mejor con otras funciones del tidyverse.



compute_s_n <- function(n){
  x <- 1:n
  sum(x)
}

n <- 1:25
s_n <- sapply(n, compute_s_n)
s_n




class(s_n)

# La primera función de purrr que aprenderemos es map, 
# que funciona muy similar a sapply pero siempre, sin excepción, devuelve una lista:

library(purrr)

s_n <- map(n, compute_s_n)
s_n



class(s_n)


# Si queremos un vector numérico, podemos usar map_dbl que siempre devuelve
# un vector de valores numéricos.

s_n <- map_dbl(n, compute_s_n)
s_n
class(s_n)






# Una función de purrr particularmente útil para interactuar con el resto del 
# tidyverse es map_df, que siempre devuelve un tibble data frame. 

s_n <- map_df(n, compute_s_n)




# Sin embargo, la función que llamamos debe devolver un vector o una lista con nombres.
# Necesitamos cambiar la función para arreglar esto:

compute_s_n <- function(n){
  x <- 1:n
  tibble(sum = sum(x))
}
s_n <- map_df(n, compute_s_n)
s_n





## Los condicionales de tidyverse


## La función case_when es útil para vectorizar declaraciones condicionales. 
## Esto es similar a ifelse, pero puede generar cualquier cantidad de valores, 
## en lugar de solo TRUE o FALSE. 


x <- c(-2, -1, 0, 1, 2)

case_when(x < 0 ~ "Negative",
          x > 0 ~ "Positive",
          TRUE ~ "Zero")




# Un uso común de esta función es definir unas variables categóricas 
# basadas en variables existentes.


# supongan que queremos comparar las tasas de asesinatos en cuatro
# grupos de estados: New England, West Coast, South y Other. 

murders %>%
  mutate(group = case_when(
    abb %in% c("ME", "NH", "VT", "MA", "RI", "CT") ~ "New England",
    abb %in% c("WA", "OR", "CA") ~ "West Coast",
    region == "South" ~ "South",
    TRUE ~ "Other")) %>%
  group_by(group) %>%
  summarize(rate = sum(total)/ sum(population) * 10^5)








##################### between #########################

# Una operación común en el análisis de datos es determinar si un valor 
# cae dentro de un intervalo. 

x

x >= 1 & x <= 2





# Dentro del enfoque tidyverse. La función between realiza la misma operación:

between(x, 1, 2)
















