##################
# web scraping
#################

url <- paste0("https://en.wikipedia.org/w/index.php?title=",
              "Gun_violence_in_the_United_States_by_state",
              "&direction=prev&oldid=810166167")

###### Paquete rvest #######

# El tidyverse provee un paquete de extracción de la web llamado rvest. 
# El primer paso para usar este paquete es importar la página web a R. 

library(tidyverse)
library(rvest)

h <- read_html(url)

# Ahora, la página entera de Wikipedia Gun violence in the United States 
# está contenida en h.


class(h)


# El paquete rvest es más general; maneja documentos XML. 
# XML es un lenguaje de marcado general (ML,markup language) que se puede usar 
# para representar cualquier tipo de datos. 
# HTML es un tipo específico de XML desarrollado específicamente
# para representar páginas web. 

h

# Podemos ver todo el código que define la página web descargada usando la función html_text

html_text(h)



# los datos que buscamos se almacenan en una tabla HTML
# Las diferentes partes de un documento HTML, a menudo definidas con un 
# mensaje entre "<" y ">" (nodos)

# El paquete rvest incluye funciones para extraer nodos de un documento HTML:
# html_nodes extrae todos los nodos de diferentes tipos y html_node extrae el primero.


tab <- h |> html_nodes("table")


# Ahora, en lugar de toda la página web, solo tenemos el código HTML 
# para las tablas de la página:

tab

# La tabla que nos interesa es la primera:

tab[[1]]


# rvest incluye una función solo para convertir tablas HTML en data frames:

tab <- tab[[1]] |> html_table()

class(tab)

tab <- tab |> setNames(c("state", "population", "total", "murder_rate"))


################################################################################

library(jsonlite)

superheroes <- fromJSON("https://raw.githubusercontent.com/jamc88/Analisis-de-datos/main/AD_R_UAM/sh_example.json")




superheroes$members

superheroes$members |> as_tibble()



################################################################################

############################
# Procesamiento de cadenas
############################

library(tidyverse)
library(stringr)

# En general, las tareas de procesamiento de cadenas se pueden dividir en detectar, 
# localizar, extraer o reemplazar patrones en cadenas. 







############# Estudio de caso 1: datos de asesinatos en EE. UU. ################

library(rvest)
url <- paste0("https://en.wikipedia.org/w/index.php?title=",
              "Gun_violence_in_the_United_States_by_state",
              "&direction=prev&oldid=810166167")
murders_raw <- read_html(url) |>
  html_node("table") |>
  html_table() |>
  setNames(c("state", "population", "total", "murder_rate"))


# En general, el procesamiento de cadenas implica una cadena y un patrón. 
# En R, generalmente almacenamos cadenas en un vector de caracteres como murders$population. 
# Las primeras tres cadenas en este vector definidas por la variable de población son:

murders_raw$population[1:3]


# Forzar una conversión usando as.numeric no funciona aquí:

as.numeric(murders_raw$population[1:3])

# Esto se debe a las comas ,. El procesamiento de cadenas que queremos hacer aquí 
#es eliminar el patrón ” ,” de las cadenas en murders_raw$population 
# y luego forzar una conversión de los números.



# Podemos usar la función str_detect para ver que dos de las tres columnas tienen 
# comas en las entradas:


commas <- function(x) any(str_detect(x, ","))
murders_raw |> summarize_all(commas)


# Entonces podemos usar la función str_replace_all para eliminarlas:


test_1 <- str_replace_all(murders_raw$population, ",", "")
test_1 <- as.numeric(test_1)





# Resulta que esta operación es tan común que readr incluye la función 
# parse_number específicamente para eliminar caracteres no numéricos antes
# de forzar una conversión:

test_2 <- parse_number(murders_raw$population)
identical(test_1, test_2)


# Podemos obtener nuestra tabla deseada usando:

murders_new <- murders_raw |> mutate_at(2:3, parse_number)



############# Estudio de caso 2: alturas autoreportadas ################


# El paquete dslabs incluye el set de datos sin procesar del cual se obtuvo 
# el set de datos de alturas. 

library(dslabs)

data(reported_heights)

# Estas alturas se obtuvieron mediante un formulario web en el que se les pidió 
# a estudiantes que ingresaran sus alturas. Podían ingresar cualquier cosa, 
# pero las instrucciones pedían altura en pulgadas, un número. 
# Recopilamos 1,095 respuestas, pero desafortunadamente el vector de columna con
# las alturas reportadas tenía varias entradas no numéricas y como resultado 
# se convirtió en un vector de caracteres:


class(reported_heights$height)


# Si intentamos analizarlo en números, recibimos una advertencia:
  
x <- as.numeric(reported_heights$height)


# Aunque la mayoría de los valores parecen ser la altura en pulgadas según lo solicitado

head(x)

# terminamos con muchos NAs:

sum(is.na(x))

# Podemos ver algunas de las entradas que no se convierten correctamente utilizando 
# filter para mantener solo las entradas que resultan en NAs:

reported_heights |>
  mutate(new_height = as.numeric(height)) |>
  filter(is.na(new_height)) |>
  head(n=10)


# Inmediatamente vemos lo que está sucediendo. Algunos de los estudiantes no reportaron 
# sus alturas en pulgadas según lo solicitado. Podríamos descartar estos datos y continuar. 
# Sin embargo, muchas de las entradas siguen patrones que, en principio,
# podemos convertir fácilmente a pulgadas. 

# Queremos encontrar patrones que puedan describirse con precisión con una regla, 
# como “un dígito, seguido por un símbolo de pie, seguido por uno o dos dígitos, 
# seguido por un símbolo de pulgadas”


# es útil eliminar las entradas que son consistentes con estar en pulgadas y ver 
# solo las entradas problemáticas. 

not_inches <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- is.na(inches) | inches < smallest | inches > tallest
  ind
}

# Aplicamos esta función y encontramos el número de entradas problemáticas:

problems <- reported_heights |>
  filter(not_inches(height)) |>
  pull(height)
length(problems)
# Ahora podemos ver todos los casos simplemente imprimiéndolos. 

# Problemas encontrados:

# 1. Un patrón de la forma x'y o x' y'' o x'y" con x e y representando pies y pulgadas,
# respectivamente.
  
# 2. Un patrón de la forma x.y o x,y con x pies y y pulgadas.

# Entradas en centímetros en vez de pulgadas






