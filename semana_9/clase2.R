######################################
# División de cadenas
######################################
library(tidyverse)
library(dslabs)

# Supongan que no tenemos la función read_csv o read.csv disponible.
# En cambio, tenemos que leer un archivo csv usando la función de base R readLines:

filename <- system.file("extdata/murders.csv", package = "dslabs")
lines <- readLines(filename)

# Esta función lee los datos línea por línea para crear un vector de cadenas. 
# En este caso, una cadena para cada fila en la hoja de cálculo.

lines |> head()

# Queremos extraer los valores que están separados por una coma para cada cadena 
# en el vector. El comando str_split hace exactamente esto:

x <- str_split(lines, ",")
x |> head(2)



# la primera entrada representa el nombre de las columnas, por lo que podemos separarlas:


col_names <- x[[1]]
x <- x[-1]


# Para convertir nuestra lista en un data frame, podemos usar un atajo ofrecido por 
# la función map en el paquete purrr.

# si queremos extraer la primera entrada de cada elemento en x, podemos escribir:

library(purrr)

map(x, function(y) y[1]) |> head(5)




# Si el segundo argumento recibe un número entero en lugar de una función, 
# supondrá que queremos esa entrada. 
map(x, 1)

# Para forzar map a devolver vectores en lugar de una lista, 
# podemos usar map_chr. 


dat <- tibble(map_chr(x, 1),
              map_chr(x, 2),
              map_chr(x, 3),
              map_chr(x, 4),
              map_chr(x, 5)) |>
  mutate_all(parse_guess) |>
  setNames(col_names)

dat |> head()

########################################################################################

# si sabemos que los datos que estamos extrayendo se pueden representar como una tabla,
# podemos usar el argumento simplify=TRUE y str_split devuelve una matriz en lugar
# de una lista:
library(purrr)
library(tidyverse)
library(dslabs)

filename <- system.file("extdata/murders.csv", package = "dslabs")
lines <- readLines(filename)

x <- str_split(lines, ",", simplify = TRUE)
col_names <- x[1,]
x <- x[-1,]
colnames(x) <- col_names
x |> as_tibble() |>
  mutate_all(parse_guess) |>
  head(5)




#######################################################################
# Extracción de tablas de un PDF
#######################################################################

# Uno de los sets de datos de dslabs muestra las tasas de financiación científica 
# por género en los Países Bajos:

library(tidyverse)
library(dslabs)

library("pdftools")


temp_file <- tempfile()
url <- paste0("https://www.pnas.org/action/downloadSupplement?doi=10.1073%2Fpnas.1510159112&file=pnas.201510159SI.pdf")
download.file(url, temp_file)
txt <- pdf_text(temp_file)
file.remove(temp_file)



raw_data_research_funding_rates <- txt[2]



#data("raw_data_research_funding_rates")

# vemos que es una cadena larga y cada línea de la página, 
# incluyendo las filas de la tabla, están separadas por el símbolo de nueva línea: \n
# por lo que creamos una lista con las líneas del texto como elementos 

tab <- str_split(raw_data_research_funding_rates, "\n")



# Debido a que comenzamos con solo un elemento en la cadena, 
# terminamos con una lista con solo una entrada:

tab <- tab[[1]]

# vemos que la información para los nombres de columna son la tercera y cuarta entrada:

the_names_1 <- tab[3]
the_names_2 <- tab[5]


# Queremos crear un vector con un nombre para cada columna.
# Queremos eliminar el espacio inicial y todo lo que sigue a la coma.
# Entonces podemos obtener los elementos dividiendo cadenas separadas por espacio.
# Queremos dividir solo cuando hay 2 o más espacios para no dividir Success rates. 
# Para esto usamos la expresión regular \\s{2,}:

the_names_1 <- the_names_1 |>
  str_trim() |>
  str_replace_all(",\\s.", "") |>
  str_split("\\s{2,}", simplify = TRUE)
the_names_1


the_names_2
# Aquí queremos podar el espacio inicial y buscar usar espacios para dividir las cadenas
# como lo hicimos para la primera línea:


the_names_2 <- the_names_2 |>
  str_trim() |>
  str_split("\\s+", simplify = TRUE)
the_names_2

# los unimos para generar un nombre para cada columna:

tmp_names <- str_c(rep(the_names_1, each = 3), the_names_2[-1], sep = "_")
the_names <- c(the_names_2[1], tmp_names) |>
  str_to_lower() |>
  str_replace_all("\\s", "_")
the_names


# Al examinar el objeto tab, notamos que la información está en las líneas 7 a 16. 
# Podemos usar str_split de nuevo para lograr nuestro objetivo:

new_research_funding_rates <- tab[7:16] |>
  str_trim() |>
  str_split("\\s{2,}", simplify = TRUE) |>
  data.frame() |>
  setNames(the_names) |>
  mutate_at(-1, parse_number)
new_research_funding_rates |> as_tibble()



#############################################################
# Recodificación
#############################################################

# Supongan que tienen nombres largos para sus niveles y los mostrarán en gráficos.
# Es posible que quieran utilizar versiones más cortas de estos nombres
# Podemos hacer esto con la función recode de tidyverse.

library(dslabs)
data("gapminder")

# Imaginen que queremos mostrar series temporales de esperanza de vida por país para el Caribe:

gapminder |>
  filter(region == "Caribbean") |>
  ggplot(aes(year, life_expectancy, color = country)) +
  geom_line()


# Una vez que elegimos los nombres cortos, debemos cambiarlos todos.
# La función recode se puede utilizar para hacer esto:

gapminder |> filter(region=="Caribbean") |>
  mutate(country = recode(country,
                          `Antigua and Barbuda` = "Barbuda",
                          `Dominican Republic` = "DR",
                          `St. Vincent and the Grenadines` = "St. Vincent",
                          `Trinidad and Tobago` = "Trinidad")) |>
  ggplot(aes(year, life_expectancy, color = country)) +
  geom_line()



##############################################################
# Procesamiento de fechas y horas
##############################################################

# R define un tipo de datos solo para fechas y horas


library(tidyverse)
library(dslabs)
data("polls_us_election_2016")
polls_us_election_2016$startdate |> head()


class(polls_us_election_2016$startdate)



# Las funciones de ggplot2 que grafican, conocen el formato de fecha. 

polls_us_election_2016 |> filter(pollster == "Ipsos" & state == "U.S.") |>
  ggplot(aes(startdate, rawpoll_trump)) +
  geom_line()



# tidyverse incluye funcionalidad para manejar fechas a través del paquete lubridate.

library(lubridate)

set.seed(2002)
dates <- sample(polls_us_election_2016$startdate, 10) |> sort()
dates



# Las funciones year, month y day extraen esos valores:

tibble(date = dates,
       month = month(dates),
       day = day(dates),
       year = year(dates))



# También podemos extraer las etiquetas del mes:

month(dates, label = TRUE)




#  funciones son las que convierten cadenas en fechas. 
# La función ymd supone que las fechas están en el formato AAAA-MM-DD 
# e intenta leer 
# y procesar (parse) lo mejor posible.

x <- c(20090101, "2009-01-02", "2009 01 03", "2009-1-4",
       "2009-1, 5", "Created on 2009 1 6", "200901 !!! 07")
ymd(x)

#  ¿qué pasa si encuentran fechas como “01/09/02”?

x <- "09/01/02"

# El paquete lubridate ofrece una función para cada posibilidad de orden:

ydm(x)

myd(x)

dmy(x)

dym(x)



# El paquete lubridate también es útil para lidiar con los tiempos. 
# En R, pueden obtener la hora actual escribiendo Sys.time(). 
# "now" permite definir la zona horaria:

now()

now("hongkong")

OlsonNames()




# También podemos extraer horas, minutos y segundos:

now() |> hour()

now() |> minute()

now() |> second()




# El paquete también incluye una función para convertir cadenas 
# en horas del día

x <- c("12:34:56")

hms(x)


x <- "Nov/2/2012 12:34:56"
mdy_hms(x)


# make_date se puede utilizar para rápidamente crear un objeto de fecha

make_date(2019, 7, 6)



# Para hacer un vector del 1 de enero para los años 80 escribimos:

make_date(1980:1989)




####################################
# Minería de textos
####################################

library(tidyverse)
library(lubridate)
library(scales)




# En general, podemos extraer datos directamente de Twitter usando 
# el paquete rtweet. 
# Para facilitar el análisis, incluimos el resultado del código 
# anterior en el paquete dslabs:

library(dslabs)
data("trump_tweets")



# variables incluidas:
names(trump_tweets)



# Los tuits están representados por la variable text:
  
trump_tweets$text[16413] |> str_wrap(width = options()$width) |> cat()

# la variable source nos dice qué dispositivo se usó para escribir cada tuit:

trump_tweets |> count(source) |> arrange(desc(n)) |> head(5)

# Estamos interesados en lo que sucedió durante la campaña, por lo que para este
# análisis nos enfocaremos en lo que se tuiteó entre el día en que Trump anunció su campaña 
# y el día de las elecciones. 

campaign_tweets <- trump_tweets |>
  extract(source, "source", "Twitter for (.*)") |> # usamos extract para eliminar "Twitter for"
  filter(source %in% c("Android", "iPhone") &
           created_at >= ymd("2015-06-17") &
           created_at < ymd("2016-11-08")) |>
  filter(!is_retweet) |>
  arrange(created_at)  |>
  as_tibble()

# exploraremos la posibilidad de que dos grupos diferentes hayan escrito los mensajes 
# desde estos dispositivos. Para cada tuit, extraeremos la hora en que se
# publicó (hora de la costa este de EE.UU. o EST), y luego calcularemos la proporción 
# de tuits tuiteada a cada hora para cada dispositivo:

campaign_tweets |>
  mutate(hour = hour(with_tz(created_at, "EST"))) |>
  count(source, hour) |>
  group_by(source) |>
  mutate(percent = n/ sum(n)) |>
  ungroup() |>
  ggplot(aes(hour, percent, color = source)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Hour of day (EST)", y = "% of tweets", color = "")

# Notamos un gran pico para Android en las primeras horas de la mañana, entre las
# 6 y las 8
# de la mañana. Parece haber una clara diferencia en estos patrones.
# Por lo tanto, suponemos que dos entidades diferentes están utilizando estos dos 
# dispositivos.


#-------------------------------------------------------------------------------

# Ahora, estudiaremos cómo difieren los tuits cuando comparamos Android con iPhone.
# Para hacer esto, utilizaremos el paquete tidytext.


# El paquete tidytext nos ayuda a convertir texto de forma libre en una tabla ordenada. 

library(tidytext)


# Un token se refiere a una unidad que consideramos como un punto de datos.
# Los tokens más comunes son las palabras, pero también pueden ser caracteres individuales,
# oraciones, líneas o un patrón definido por una expresión regular. 
 

poem <- c("Roses are red,", "Violets are blue,",
          "Sugar is sweet,", "And so are you.")
example <- tibble(line = c(1, 2, 3, 4),
                  text = poem)
example

# Las función unnest_tokens tomarán un vector de cadenas y extraerán los tokens para que cada uno
# obtenga una fila en la nueva tabla.

example |> unnest_tokens(word, text)


# miremos el tuit número 3008 

i <- 3008
campaign_tweets$text[i] |> str_wrap(width = 65) |> cat()

campaign_tweets[i,] |>
  unnest_tokens(word, text) |>
  pull(word)

# la función elimina los caracteres que son importantes en el contexto de Twitter (# y @). 


campaign_tweets[i,] |>
  unnest_tokens(word, text, token = "tweets") |>
  pull(word)







