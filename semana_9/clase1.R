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
# Hay un cuantificador para exactamente este propósito. 
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


# Si queremos detectar dígitos precedidos por algo 
# que no sea una letra, podemos usar el símbolo "^" 

pattern <- "[^a-zA-Z]\\d"
yes <- c(".3", "+2", "-0","*4")
no <- c("A3", "B2", "C0", "E4")

str_detect(yes, pattern)
str_detect(no, pattern)




##########################################################################
############################### Grupos ###################################

# Los grupos permiten que las herramientas identifiquen 
# partes específicas del patrón para que podamos extraerlas.

# Queremos cambiar las alturas escritas como 5.6 a 5'6.

# Para evitar cambiar patrones como 70.2, requeriremos que el primer dígito esté 
# entre 4 y 7 [4-7] y que el segundo sea ninguno o uno o más dígitos \\d*

pattern_without_groups <- "^[4-7],\\d*$"


# Queremos extraer los dígitos para poder formar la nueva versión usando un punto.
# Estos son nuestros dos grupos, por lo que los encapsulamos con paréntesis:

pattern_with_groups <- "^([4-7]),(\\d*)$"


# Encapsulamos la parte del patrón que corresponde con las partes que queremos conservar 
# para su uso posterior. 
# Ambos patrones devuelven el mismo resultado cuando se usa "str_detect":

yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s <- c(yes, no)
str_detect(s, pattern_without_groups)

str_detect(s, pattern_with_groups)

# Una vez que hayamos definido los grupos, podemos usar la función str_match
# para extraer los valores que definen estos grupos:

str_match(s, pattern_with_groups)


# La diferencia entre las funciones str_extract y str_match es que
# str_extract extrae solo cadenas que corresponden con un patrón,
# no los valores definidos por grupos:

str_extract(s, pattern_with_groups)




####################
# Buscar y reemplazar con expresiones regulares
###################

# Recordemos que problems contiene las cadenas que no parecen estar en pulgadas. 

pattern <- "^[4-7]'\\d{1,2}\"$"
sum(str_detect(problems, pattern))


# algunos estudiantes escribieron las palabras “feet” e “inches”. 
# Podemos ver las entradas que hicieron esto con la función str_subset:

str_subset(problems, "inches")



# También vemos que algunas entradas usan dos comillas simples '' en lugar de 
# una comilla doble "

str_subset(problems, "''")




pattern <- "^[4-7]'\\d{1,2}$"

problems |>
  str_replace("feet|ft|foot", "'") |> # replace feet, ft, foot with '
  str_replace("inches|in|''|\"", "") |> # remove all inches symbols
  str_detect(pattern) |>
  sum()

# mejoramos nuestro patrón agregando \\s* delante y después del símbolo de los pies '
# para permitir espacio entre el símbolo de los pies y los números. 

pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
problems |>
  str_replace("feet|ft|foot", "'") |> # replace feet, ft, foot with '
  str_replace("inches|in|''|\"", "") |> # remove all inches symbols
  str_detect(pattern) |>
  sum()


# El segundo grupo grande de entradas problemáticas tienen las formas x.y, x,y o x y.
# Queremos cambiar todo esto a nuestro formato común x'y utilizando grupos

pattern_with_groups <-"^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$"


str_subset(problems, pattern_with_groups) 

# Ahora usamos esto para buscar y reemplazar:

str_subset(problems, pattern_with_groups) |>
  str_replace(pattern_with_groups, "\\1'\\2") 

# Escribamos una función que identifique todas las entradas que no se pueden convertir
# en números

not_inches_or_cm <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- !is.na(inches) &
    ((inches >= smallest & inches <= tallest) |
       (inches/2.54 >= smallest & inches/2.54 <= tallest))
  !ind
}

problems <- reported_heights |>
  filter(not_inches_or_cm(height)) |>
  pull(height)
length(problems)


# Veamos qué proporción de estas se ajusta a nuestro patrón después de los pasos 
# de procesamiento que desarrollamos anteriormente:

converted <- problems |>
  str_replace("feet|foot|ft", "'") |> # convert feet symbols to '
  str_replace("inches|in|''|\"", "") |> # remove inches symbols
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")# change format

pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
index <- str_detect(converted, pattern)
mean(index)

# Este último fragmento de código muestra que encontramos casos en más de la 
# mitad de las cadenas. Examinemos los casos restantes:

converted[!index]



# Surgen cuatro patrones claros:
  
#  1. Muchos estudiantes que miden exactamente 5 o 6 pies no ingresaron ninguna pulgada,
#     por ejemplo 6', y nuestro patrón requiere que se incluyan pulgadas.
#  2. Algunos estudiantes que miden exactamente 5 o 6 pies ingresaron solo ese número.
#  3. Algunas de las pulgadas se ingresaron con puntos decimales. Por ejemplo 5'7.5''. 
#     Nuestro patrón solo busca dos dígitos.
#  4. Algunas entradas tienen espacios al final, por ejemplo 5 ' 9 .


# Aunque no es tan común, también vemos los siguientes problemas:

#  5. Algunas entradas están en metros y algunas usan formatos europeos: 1.6, 1,70.
#  6. Dos estudiantes añadieron cm.
#  7. Un estudiante deletreó los números: Five foot eight inches.


# No está claro que valga la pena escribir código para manejar estos últimos tres casos, 
# ya que son bastante raros.



# Para el caso 1, si agregamos un '0 después del primer dígito, 

yes <- c("5", "6", "5")
no <- c("5'", "5''", "5'4")
s <- c(yes, no)
str_replace(s, "^([4-7])$", "\\1'0")


# Podemos adaptar este código ligeramente para manejar también el caso 2, 
# que cubre la entrada 5'. 
# podemos usar el carácter especial, ?, que significa ninguna o una vez

str_replace(s, "^([56])'?$", "\\1'0")


# Podemos usar cuantificadores para tratar el caso 3. 
# Necesitamos permitir que el segundo grupo incluya decimales, no solo dígitos.
# Usaremos ? y *

pattern <- "^[4-7]\\s*'\\s*(\\d+\\.?\\d*)$"


# Podemos tratar el caso 4, metros usando comas, requerimos que el primer dígito sea 1 o 2.


yes <- c("1,7", "1, 8", "2, " )
no <- c("5,8", "5,3,2", "1.7")
s <- c(yes, no)
str_replace(s, "^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2")


#----------------------------
# los espacios al principio o al final de la cadena no son informativos. 
s <- "Hi "
cat(s)


identical(s, "Hi")

# Existe una función dedicada a eliminarlos:

str_trim("5 ' 9 ")

# podemos usar la función str_to_lower para convertir todas las
# letras mayusculas a minúsculas:


s <- c("Five feet eight Inches")
str_to_lower(s)

#-------------------------------------------------------------------

# Escribimos una función que reúne lo que hemos hecho anteriormente:

convert_format <- function(s){
  s |>
    str_replace("feet|foot|ft", "'") |>
    str_replace_all("inches|in|''|\"|cm|and", "") |>
    str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") |>
    str_replace("^([56])'?$", "\\1'0") |>
    str_replace("^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2") |>
    str_trim()
}


# También escribimos una función que convierta palabras en números:

library(english)


words_to_numbers <- function(s){
  s <- str_to_lower(s)
  for(i in 0:11)
    s <- str_replace_all(s, words(i), as.character(i))
  s
}

# Ahora podemos ver qué entradas problemáticas permanecen:

converted <- problems |> words_to_numbers() |> convert_format()
remaining_problems <- converted[not_inches_or_cm(converted)]
pattern <- "^[4-7]\\s*'\\s*\\d+\\.?\\d*$"
index <- str_detect(remaining_problems, pattern)
remaining_problems[!index]



#-------------------------------------------------------------------------------

# La función extract de tidyverse es util para el procesamiento de cadenas 
# que usaremos en nuestra solución final

# Queremos extraer y guardar los pies y los valores numéricos para poder 
# convertirlos en pulgadas cuando sea apropiado

library(tidyr)

s <- c("5'10", "6'1")
tab <- data.frame(x = s)

tab |> extract(x, c("feet", "inches"), regex = "(\\d)'(\\d{1,2})")

# diferencia con separate

s <- c("5'10", "6'1\"","5'8inches")
tab <- data.frame(x = s)

tab |> separate(x, c("feet","inches"), sep = "'", fill = "right")

tab |> extract(x, c("feet", "inches"), regex = "(\\d)'(\\d{1,2})")

#-------------------------------------------------------------------------------


# juntamos todas las piezas y discutir nuestros datos de alturas reportados para tratar 
# de recuperar todas las alturas posibles.


# Comenzamos limpiando la columna height para que las alturas estén más cerca 
# de un formato de pulgadas y pies.
# Agregamos una columna con las alturas originales para luego poder comparar.

pattern <- "^([4-7])\\s*'\\s*(\\d+\\.?\\d*)$"

smallest <- 50
tallest <- 84
new_heights <- reported_heights |>
  mutate(original = height,
         height = words_to_numbers(height) |> convert_format()) |>
  extract(height, c("feet", "inches"), regex = pattern, remove = FALSE) |>
  mutate_at(c("height", "feet", "inches"), as.numeric) |>
  mutate(guess = 12 * feet + inches) |>
  mutate(height = case_when(
    is.na(height) ~ as.numeric(NA),
    between(height, smallest, tallest) ~ height, #inches
    between(height/2.54, smallest, tallest) ~ height/2.54, #cm
    between(height*100/2.54, smallest, tallest) ~ height*100/2.54, #meters
    TRUE ~ as.numeric(NA))) |>
  mutate(height = ifelse(is.na(height) &
                           inches < 12 & between(guess, smallest, tallest),
                         guess, height)) |> select(-guess)


new_heights

# Podemos verificar todas las entradas que convertimos al escribir:

new_heights |>
  filter(not_inches(original)) |>
  select(original, height) |>
  arrange(height) |>
  View()








