library(tidyverse)
library(dslabs)


# El paquete tidyr incluye varias funciones útiles para poner los datos en formato tidy.

path <- system.file("extdata", package="dslabs") #busca archivos de sistema con base R 
filename <- file.path(path, "fertility-two-countries-example.csv")
wide_data <- read_csv(filename)

View(wide_data)


################
# pivot_longer
###############

# Una de las funciones más usadas del paquete tidyr es pivot_longer,
# que nos permite convertir datos anchos (wide data) en datos tidy.


# Aquí queremos cambiar la forma del set de datos wide_data para que cada fila 
# represente una observación de fertilidad. El primer argumento de la función pivot_longer
# es el data frame que será procesado

new_tidy_data <- pivot_longer(wide_data, `1960`:`2015`, 
                              names_to = "year", values_to = "fertility")

# A través de los argumentos names_to y values_to, le daremos a pivot_longer 
# los nombres de columna que le queremos asignar a las columnas que contienen 
# los nombres de columna y las observaciones actuales, respectivamente. 




# También podemos usar el pipe de esta manera:


new_tidy_data <- wide_data |> 
  pivot_longer(`1960`:`2015`, names_to = "year", values_to = "fertility")




# Una forma un poco más rápida de escribir este código es especificar 
# qué columna no se recopilará

new_tidy_data <- wide_data |>
  pivot_longer(-country, names_to = "year", values_to = "fertility")








# El objeto new_tidy_data se parece al original tidy_data que definimos a continuación:


data("gapminder")

tidy_data <- gapminder |>
  filter(country %in% c("South Korea", "Germany") & !is.na(fertility)) |>
  select(country, year, fertility)




class(tidy_data$year)
class(new_tidy_data$year)



# La función pivot_longer supone que los nombres de columna son caracteres.

# Necesitamos convertir la columna con los años en números. 
# La función pivot_longer incluye el argumento convert para este propósito:

new_tidy_data <- wide_data |>
  pivot_longer(-country, names_to = "year", values_to = "fertility") |>
  mutate(year = as.integer(year))





new_tidy_data |> ggplot(aes(year, fertility, color = country)) + 
  geom_line()



################
# pivot_wider
###############

# La función pivot_wider es  la inversa de pivot_longer

# El argumento names_from le dice a pivot_longer qué variable usar como nombre de columna. 
# El argumento names_to especifica qué variable usar para completar las celdas:

new_wide_data <- new_tidy_data |> 
  pivot_wider(names_from = year, values_from = fertility)
select(new_wide_data, country, `1960`:`1967`)




################
# separate
###############


path <- system.file("extdata", package = "dslabs")

filename <- "life-expectancy-and-fertility-two-countries-example.csv"
filename <- file.path(path, filename)

raw_dat <- read_csv(filename)
select(raw_dat, 1:5)

View(raw_dat)

# observen que esta tabla incluye valores para dos variables, fertilidad y esperanza de vida,
# con nombre de la columna codificando qué columna representa qué variable. 


dat <- raw_dat |> pivot_longer(-country)
head(dat)



# El resultado no es exactamente lo que llamamos tidy ya que cada observación está 
# asociada con dos filas en vez de una.


# Queremos tener los valores de las dos variables, fertility y life_expectancy, 
# en dos columnas separadas. El primer reto para lograr esto es separar 
# la columna name en año y tipo de variable. 



dat$name[1:5] # las entradas en esta columna separan el año del nombre de la variable con una barra baja



# el paquete readr incluye una función para separar estas columnas
# la función separate toma tres argumentos: el nombre de la columna que se separará, 
# los nombres que se utilizarán para las nuevas columnas y 
# el carácter que separa las variables.
library(readr)


dat |> separate(name, c("year", "name"), "_")


# separate supone por defecto que "_" es el separador

dat |> separate(name, c("year", "name"))


# notemos que la variable life_expectancy se corta a life


# Podríamos añadir una tercera columna para guardar esto y dejar que la función 
# separate sepa cual columna llenar con los valores faltantes, NA,
# cuando no hay un tercer valor.


var_names <- c("year", "first_variable_name", "second_variable_name")
dat |> separate(name, var_names, fill = "right")  #  llene la columna de la derecha


# fusionamos las dos últimas variables cuando hay una separación adicional:


dat |> separate(name, c("year", "name"), extra = "merge")



# Esto logra la separación que queríamos. Sin embargo, aún no hemos terminado. 
# Necesitamos crear una columna para cada variable con la función pivot_wider

dat |>
  separate(name, c("year", "name"), extra = "merge") |>
  pivot_wider()




# Los datos ahora están en formato tidy con una fila para cada observación 
# con tres variables: año, fertilidad y esperanza de vida.








################
# unite
###############


# A veces es útil hacer el inverso de separate, es decir, unir dos columnas en una.



var_names <- c("year", "first_variable_name", "second_variable_name")
dat |>
  separate(name, var_names, fill = "right")


# Podemos lograr el mismo resultado final uniendo las segunda y tercera columnas, 
# luego esparciendo las columnas usando pivot_wider y renombrando fertility_NA a fertility:

dat |>
  separate(name, var_names, fill = "right") |>
  unite(name, first_variable_name, second_variable_name) |>
  pivot_wider() |>
  rename(fertility = fertility_NA)






################
# unir tablas
###############


# Es posible que la información que necesitamos para un análisis no esté en solo 
# en una tabla. Por ejemplo, cuando pronosticamos elecciones usamos 
# la función left_join para combinar la información de dos tablas. 


# Supongan que queremos explorar la relación entre el tamaño de la población 
# de los estados de EE. UU. y los votos electorales. 



# Tenemos el tamaño de la población en esta tabla:
library(tidyverse)
library(dslabs)
data(murders)
head(murders)


# los votos electorales están en esta tabla:

data(polls_us_election_2016)
head(results_us_election_2016)


# concatenar estas dos tablas no funcionará ya que el orden de los estados no es el mismo.

identical(results_us_election_2016$state, murders$state)



# Las funciones para unir del paquete dplyr aseguran que las tablas se combinen de tal forma 
# que las filas equivalentes estén juntas

# La idea general es que uno necesita identificar una o más columnas que servirán para 
# emparejar las dos tablas. Entonces se devuelve una nueva tabla con la información 
# combinada. 

# unimos las dos tablas anteriores por estado usando left_join 
# eliminaremos la columna others y renombraremos electoral_votes

tab <- left_join(murders, results_us_election_2016, by = "state") |>
  select(-others) |> rename(ev = electoral_votes)
head(tab)

# Los datos se han unido exitosamente y ahora podemos,
# hacer un diagrama para explorar la relación:
library(ggrepel)

tab |> ggplot(aes(population/10^6, ev, label = abb)) +
  geom_point() +
  geom_text_repel() +
  scale_x_continuous(trans = "log2") +
  scale_y_continuous(trans = "log2") +
  geom_smooth(method = "lm", se = FALSE)



# Vemos que la relación es casi lineal con aproximadamente dos votos electorales 
# por cada millón de personas, 
# pero con estados muy pequeños obteniendo proporciones más altas.




# En la práctica, no siempre ocurre que cada fila de una tabla tiene una fila 
# correspondiente en la otra.

# Creamos las tablas tab1 y tab2 para que tengan algunos estados en común pero no todos

tab_1 <- slice(murders, 1:6) |> select(state, population)
tab_1


tab_2 <- results_us_election_2016 |>
  filter(state%in%c("Alabama", "Alaska", "Arizona",
                    "California", "Connecticut", "Delaware")) |>
  select(state, electoral_votes) |> rename(ev = electoral_votes)
tab_2





# Supongan que queremos una tabla como tab_1, pero agregando votos electorales a
# cualquier estado que tengamos disponible. 
# Para esto, usamos left_join con tab_1 como el primer argumento.
# Especificamos qué columna usar para que coincida con el argumento by.

left_join(tab_1, tab_2, by = "state")




# con pipe
tab_1 |> left_join(tab_2, by = "state")




# Si en lugar de una tabla con las mismas filas que la primera tabla, 
# queremos una con las mismas filas que la segunda tabla, podemos usar right_join:


tab_1 |> right_join(tab_2, by = "state")

# Ahora los NAs están en la columna de tab_1.



# Si queremos mantener solo las filas que tienen información en ambas tablas,
# usamos inner_join. Pueden pensar en esto como una intersección:

inner_join(tab_1, tab_2, by = "state")



# Si queremos mantener todas las filas y llenar las partes faltantes con NAs,
# podemos usar full_join. Pueden pensar en esto como una unión:
  

full_join(tab_1, tab_2, by = "state")




# La función semi_join nos permite mantener la parte de la primera tabla
# para la cual tenemos información en la segunda. No agrega las columnas de la segunda:
  
  
semi_join(tab_1, tab_2, by = "state")



# La función anti_join es la opuesta de semi_join. Mantiene los elementos de la primera
# tabla para los que no hay información en la segunda:


anti_join(tab_1, tab_2, by = "state")




##################
# Binding
#################


# Otra forma común en la que se combinan los sets de datos es pegándolos (binding).
# A diferencia de las funciones join, las funciones binding no intentan coincidir 
# con una variable, sino que simplemente combinan sets de datos. 
# Si los sets de datos no coinciden con las dimensiones apropiadas, se obtiene un error.




# La función bind_cols de dplyr pega dos objetos convirtiéndolos en columnas en un tibble. 
# Por ejemplo, queremos crear rápidamente un data frame que consiste de números 
# que podemos usar.

bind_cols(a = 1:3, b = 4:6)


# bind_cols también puede pegar dos data frames diferentes. 
# Por ejemplo, aquí separamos el data frame tab en tres data frames 
# y luego volvemos a pegarlos:


tab_1 <- tab[, 1:3]
tab_2 <- tab[, 4:6]
tab_3 <- tab[, 7:8]

new_tab <- bind_cols(tab_1, tab_2, tab_3)



# La función bind_rows es similar a bind_cols, pero pega filas en lugar de columnas:

tab_1 <- tab[1:2,]
tab_2 <- tab[3:4,]
bind_rows(tab_1, tab_2)



# Operadores de sets


# Cuando se aplican a los vectores, estos se comportan como lo sugieren sus nombres.
# Ejemplos son intersect, union, setdiff y setequal. 
# Sin embargo, si se carga el tidyverse, o más específicamente dplyr,
# estas funciones se pueden usar en data frames en lugar de solo en vectores.



# Pueden tomar intersecciones de vectores de cualquier tipo, como numéricos:
  
  
intersect(1:10, 6:15)


intersect(c("a","b","c"), c("b","c","d"))


# El paquete dplyr incluye una función intersect que se puede aplicar a tablas 
# con los mismos nombres de columna. Esta función devuelve las filas en común
# entre dos tablas. 


# Para asegurarnos de que usamos la versión de dplyr de intersect en lugar 
# de la versión del paquete base, podemos usar dplyr::intersect así:


tab_1 <- tab[1:5,]
tab_2 <- tab[3:7,]
dplyr::intersect(tab_1, tab_2)



# Del mismo modo, union toma la unión de vectores. Por ejemplo:


union(1:10, 6:15)

union(c("a","b","c"), c("b","c","d"))


# El paquete dplyr incluye una versión de union que combina todas las filas de dos
# tablas con los mismos nombres de columna.


tab_1 <- tab[1:5,]
tab_2 <- tab[3:7,]
dplyr::union(tab_1, tab_2)



# La diferencia establecida entre un primer y un segundo argumento se puede 
# obtener con setdiff. A diferencia de intersect y union, esta función no es simétrica:

setdiff(1:10, 6:15)
setdiff(6:15, 1:10)



# Al igual que con las funciones que se muestran arriba, dplyr tiene una versión 
# para data frames:

tab_1 <- tab[1:5,]
tab_2 <- tab[3:7,]
dplyr::setdiff(tab_1, tab_2)




# la función setequal nos dice si dos sets son iguales, independientemente del orden. 


setequal(1:5, 1:6)

setequal(1:5, 5:1)



# Cuando se aplica a data frames que no son iguales, independientemente del orden, 
# la versión dplyr ofrece un mensaje útil que nos permite saber cómo los sets son diferentes:

dplyr::setequal(tab_1, tab_2)

