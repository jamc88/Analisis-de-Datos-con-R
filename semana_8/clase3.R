############# Estudio de caso 2: alturas autoreportadas ################


# El paquete dslabs incluye el set de datos sin procesar del cual se obtuvo 
# el set de datos de alturas. 
library(tidyverse)
library(stringr)
library(rvest)
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


# Podríamos descartar estos datos y continuar. 
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

#-------------------------------------------------------------------------------


########### Cómo escapar al definir cadenas ##############


# Para definir cadenas en R, podemos usar comillas dobles:


s <- "Hello!"
s <- 'Hello!'


# ¿Qué sucede si la cadena que queremos definir incluye comillas dobles? 
# Por ejemplo, si queremos escribir 10 pulgadas así: 10"
# En este caso no pueden usar:

#s <- "10""

s <- '10"' 

# En R, la función cat nos permite ver como se ve la cadena:

cat(s)


# ¿qué pasa si queremos que nuestra cadena sea de 5 pies escrita así: 5'?
# En ese caso, podemos usar las comillas dobles:


s <- "5'"
cat(s)


# De esta forma podemos escribir 5 pies y 10 pulgadas por separado, 
# pero ¿qué pasa si queremos escribirlos juntos para representar 5 pies y 10 pulgadas 
# así: 5'10"? 

# s <- '5'10"'

# s <- "5'10""

s <- '5\'10"'
cat(s)

# ó así

s <- "5'10\""
cat(s)


################### Expresiones regulares #######################


# Las expresiones regulares (regex) son una forma de describir 
# patrones específicos de caracteres de texto. 
# Se pueden usar para determinar si una cadena dada corresponde con el patrón.

library(rvest)
url <- paste0("https://en.wikipedia.org/w/index.php?title=",
              "Gun_violence_in_the_United_States_by_state",
              "&direction=prev&oldid=810166167")
murders_raw <- read_html(url) |>
  html_node("table") |>
  html_table() |>
  setNames(c("state", "population", "total", "murder_rate"))


# cualquier cadena es una expresión regular, quizás el ejemplo más simple es un 
# solo carácter.
# Entonces la coma , usada en el siguiente ejemplo de código, es un ejemplo sencillo 
# de una búsqueda con expresiones regulares.


pattern <- ","
str_detect(murders_raw$total, pattern)


# Podemos mostrar todas las entradas que usaron cm así:

library(dslabs)

data(reported_heights)

reported_heights |>
  mutate(new_height = as.numeric(height)) |>
  filter(is.na(new_height)) |>
  head(n=10)


str_subset(reported_heights$height, "cm")


# ¿Cuál de las siguientes cadenas contiene el patrón cm o inches?

yes <- c("180 cm", "70 inches")
no <- c("180", "70''")
s <- c(yes, no)

str_detect(s, "cm") | str_detect(s, "inches")

# La característica principal que distingue el lenguaje de las 
# expresiones regulares de cadenas es que podemos usar caracteres especiales.

str_detect(s, "cm|inches")


# Otro carácter especial que será útil para identificar valores de pies y pulgadas 
# es \d que significa cualquier dígito: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9.
# usamos \\d para representar dígitos.


yes <- c("5", "6", "5'10", "5 feet", "4'11")
no <- c("", ".", "Five", "six")
s <- c(yes, no)
pattern <- "\\d"
str_detect(s, pattern)



# la función str_view es útil para la solución de problemas ya que nos muestra
# la primera ocurrencia que corresponde exactamente para cada cadena:

str_view(s, pattern)

# str_view_all nos muestra todos los patrones que corresponden

str_view_all(s, pattern)


#################### clases de caracteres ########################

# Las clases de caracteres se utilizan para definir una serie de caracteres 
# que pueden corresponder. Definimos clases de caracteres entre corchetes [ ]. 
# Entonces, por ejemplo, si queremos que el patrón corresponda solo si tenemos 
# un 5 o un 6, usamos la expresión regular [56]:

str_view(s, "[56]")

# Supongan que queremos unir valores entre 4 y 7. Una forma común de definir
# clases de caracteres es con rangos. Así por ejemplo, [0-9] es equivalente a \\d.
# El patrón que queremos entonces es [4-7].

yes <- as.character(4:7)
no <- as.character(1:3)
s <- c(yes, no)
str_detect(s, "[4-7]")

# ¿Y si queremos una correspondencia cuando tenemos exactamente 1 dígito? 
# Esto será útil en nuestro estudio de caso, ya que los pies nunca tienen 
# más de 1 dígito, por lo que esta restricción nos ayudará. 

# Una forma de hacer esto con regex es usando anclas (anchors en inglés),
# que nos permiten definir patrones que deben comenzar o terminar en un lugar
# específico. Las dos anclas más comunes son ^ y $ que representan el comienzo 
# y el final de una cadena, respectivamente. 
# Entonces el patrón ^\\d$ se lee como “inicio de la cadena seguido por un 
# dígito seguido por el final de la cadena”.


# Este patrón ahora solo detecta las cadenas con exactamente un dígito:

pattern <- "^\\d$"
yes <- c("1", "5", "9")
no <- c("12", "123", " 1", "a4", "b")
s <- c(yes, no)
str_view_all(s, pattern)

# Para la parte de pulgadas, podemos tener uno o dos dígitos. 
# Esto se puede especificar con cuantificadores (quantifiers). 
# Esto se hace siguiendo el patrón con la cantidad de veces que se puede repetir
# cerrada por llaves.

pattern <- "^\\d{1,2}$"
yes <- c("1", "5", "9", "12")
no <- c("123", "a4", "b")
str_view(c(yes, no), pattern)

# En este caso, 123 no corresponde, pero 12 sí. 
# Entonces, para buscar nuestro patrón de pies y pulgadas, 
# podemos agregar los símbolos para pies ' y pulgadas " después de los dígitos.

# Ahora podemos construir un ejemplo para el patrón x'y\" con x pies y y pulgadas.

pattern <- "^[4-7]'\\d{1,2}\"$"

# El patrón ahora se está volviendo complejo, pero pueden mirarlo cuidadosamente y desglosarlo:

# ^ = inicio de la cadena
# [4-7] = un dígito, ya sea 4, 5, 6 o 7
# ' = símbolo de pies
# \\d{1,2} = uno o dos dígitos
# \" = símbolo de pulgadas
# $ = final de la cadena

# Prueba:

yes <- c("5'7\"", "6'2\"", "5'12\"")
no <- c("6,2\"", "6.2\"","I am 5'11\"", "3'2\"", "64")
str_detect(yes, pattern)
str_detect(no, pattern)

# Notemos que ahora estamos permitiendo que las pulgadas sean 12 o más grandes

################# Espacio en blanco \s ###################

# Otro problema que tenemos son los espacios. 
# Por ejemplo, nuestro patrón no corresponde con 5' 4" porque hay un espacio entre ' y 4, 
# que nuestro patrón no permite. 
# Los espacios son caracteres y R no los ignora:

identical("Hi", "Hi ")


# \s representa el espacio en blanco. Para encontrar patrones como 5' 4, 
# podemos cambiar nuestro patrón a:

pattern_2 <- "^[4-7]'\\s\\d{1,2}\"$"
str_subset(problems, pattern_2)

# Sin embargo, esto no encontrará equivalencia con los patrones sin espacio.
# Resulta que también podemos usar un cuantificador para esto.

# Queremos que el patrón permita espacios pero que no los requiera. 
# Incluso si hay varios espacios, como en este ejemplo 5' 4, todavía 
# queremos que corresponda. 
#Hay un cuantificador para exactamente este propósito. 
# * significa cero o más instancias del carácter anterior.

yes <- c("AB", "A1B", "A11B", "A111B", "A1111B")
no <- c("A2B", "A21B")

str_detect(yes, "A1*B")
str_detect(no, "A1*B")

# El patrón anterior encuentra correspondencia con la primera cadena, 
# que tiene cero 1s, y todas las cadenas con un o más 1. 
# Entonces podemos mejorar nuestro patrón agregando el * después del carácter
# espacial \s.

# Hay otros dos cuantificadores similares. Para ninguno o una vez, podemos usar ?,
# y para uno o más, podemos usar +

data.frame(string = c("AB", "A1B", "A11B", "A111B", "A1111B"),
           none_or_more = str_detect(yes, "A1*B"),
           nore_or_once = str_detect(yes, "A1?B"),
           once_or_more = str_detect(yes, "A1+B"))



# Para especificar patrones que no queremos detectar, podemos usar el símbolo ^ 
# pero solo dentro de corchetes.
# Entonces, por ejemplo, si queremos detectar dígitos precedidos por algo 
# que no sea una letra, podemos hacer lo siguiente:

pattern <- "[^a-zA-Z]\\d"
yes <- c(".3", "+2", "-0","*4")
no <- c("A3", "B2", "C0", "E4")

str_detect(yes, pattern)
str_detect(no, pattern)

# Otra forma de generar un patrón que busca todo excepto es usar la mayúscula
# del carácter especial. 
# Por ejemplo \\D significa cualquier cosa que no sea un dígito, 
# \\S significa cualquier cosa excepto un espacio.

################ Grupos ############################

# Los grupos son un elemento muy útil de la expresiones regulares que permiten 
# la extracción de valores. Los grupos se definen usando paréntesis. 
# No afectan las equivalencias de patrones. En cambio, permiten que las
# herramientas identifiquen partes específicas del patrón para que podamos 
# extraerlas.

#Queremos cambiar las alturas escritas como 5.6 a 5'6.

# Para evitar cambiar patrones como 70.2, requeriremos que el primer dígito esté 
#entre 4 y 7 [4-7] y que el segundo sea ninguno o uno o más dígitos \\d*

pattern_without_groups <- "^[4-7],\\d*$"


# Queremos extraer los dígitos para poder formar la nueva versión usando un punto.
# Estos son nuestros dos grupos, por lo que los encapsulamos con paréntesis:

pattern_with_groups <- "^([4-7]),(\\d*)$"


# Encapsulamos la parte del patrón que corresponde con las partes que queremos conservar 
# para su uso posterior. Agregar grupos no afecta la detección, ya que solo indica
# que queremos guardar lo que capturan los grupos. 
# Tengan en cuenta que ambos patrones devuelven el mismo resultado cuando se usa 
# str_detect:

yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s <- c(yes, no)
str_detect(s, pattern_without_groups)

str_detect(s, pattern_with_groups)

# Una vez que hayamos definido los grupos, podemos usar la función str_match
# para extraer los valores que definen estos grupos:

str_match(s, pattern_with_groups)


# Observen que las segunda y tercera columnas contienen pies y pulgadas, 
# respectivamente. La primera columna es la parte de la cadena que corresponde 
# con el patrón. Si no se encuentra una equivalencia, vemos un NA.

# Ahora podemos entender la diferencia entre las funciones str_extract y str_match.
# str_extract extrae solo cadenas que corresponden con un patrón,
# no los valores definidos por grupos:

str_extract(s, pattern_with_groups)

