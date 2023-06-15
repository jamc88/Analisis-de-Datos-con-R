#################################################
# Probabilidad
#################################################


# Los generadores de números aleatorios nos permiten imitar el proceso de escoger
# al azar.  La función rep genera la urna:

beads <- rep(c("red", "blue"), times = c(2,3))
beads








# usamos sample para escoger una canica al azar:

sample(beads, 1)








# Queremos repetir este experimento un número infinito de veces

# Podemos repetir el experimento un número suficientemente grande de veces 
# para que los resultados sean prácticamente equivalentes a repetirlo para siempre.
# Este es un ejemplo de una simulación Monte Carlo.




#########################################################


# Para realizar nuestra primera simulación Monte Carlo, utilizamos la función replicate,
# que nos permite repetir la misma tarea varias veces (B= 10,000 veces)


B <- 10000
events <- replicate(B, sample(beads, 1))

events



# Usamos table para ver la distribución:


tab <- table(events)
tab


# prop.table nos da las proporciones:

prop.table(tab)





# Los números anteriores son probabilidades estimadas proveídas 
# por una simulación Monte Carlo. 
# La teoría estadística, nos dice que en lo que B se hace más grande, 
# las estimaciones se acercan a 3/5 = .6 y 2/5 = .4.








# Con y sin reemplazo

# La función sample tiene un argumento que nos permite elegir 
# más de un elemento de la urna.
# Sin embargo, por defecto, esta selección ocurre sin reemplazo; 

sample(beads, 5)
sample(beads, 5)



sample(beads, 6)



# muestreamos con reemplazo

sample(beads, 5, replace = TRUE)




events <- sample(beads, B, replace = TRUE)
prop.table(table(events))







######################################################################








# Construimos una baraja de cartas utilizando las 
# funciones expand.grid y paste. 
# Usamos paste para crear cadenas uniendo cadenas más pequeñas.

# Tomamos el número y el palo de una carta y creamos el nombre 
# de la carta de esta manera:


number <- "Three"
suit <- "Hearts"
paste(number, suit)




#---------------------------------------------------------------------------------------------

# paste también funciona en pares de vectores que realizan la operación 
# elemento por elemento:

paste(letters[1:5], as.character(1:5))

# La función expand.grid nos da todas las combinaciones de entradas de dos vectores. 
# Por ejemplo, si tienen pantalones azules y negros y camisas blancas, grises 
# y a cuadros todas sus combinaciones son:


expand.grid(pantalones = c("azul", "negro"), camisas = c("blanco", "gris", "cuadros"))

#---------------------------------------------------------------------------------------------






# Ahora, generamos una baraja de cartas:

suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven",
             "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(number=numbers, suit=suits)
deck <- paste(deck$number, deck$suit)

deck







# Con la baraja construida, podemos verificar que la probabilidad de que una 
# K sea la primera carta es 1/13  calculando la proporción de posibles resultados 
# que satisfagan nuestra condición:


kings <- paste("King", suits)
mean(deck %in% kings)





# ¿Cuál es la probabilidad de que la segunda carta sea una K dado que 
# la primera era una K? Anteriormente, dedujimos que si una K ya está 
# fuera de la baraja 
# y quedan 51 cartas, entonces la probabilidad es 3/51. Confirmemos 
# enumerando todos los resultados posibles.






#---------------------------------------------------------------------------------------------

# Para cualquier lista de tamaño n, la función permutations del paquete gtools 
# calcula todas las diferentes combinaciones que podemos obtener cuando 
# seleccionamos r artículos.


library(gtools)

# Aquí están todas las formas en que podemos elegir dos números de una lista 
# que consiste en 1,2,3:

permutations(3, 2)




# También, podemos añadir un vector. Si desean ver cinco números de teléfono aleatorios 
# (de siete dígitos) de todos los números de teléfono posibles (sin repeticiones), pueden escribir:

all_phone_numbers <- permutations(10, 7, v = 0:9)
n <- nrow(all_phone_numbers)
index <- sample(n, 5)
all_phone_numbers[index,]




# En lugar de usar los números del 1 al 10, el valor por defecto, 
# R usa lo que proveemos a través de v: los dígitos de 0 a 9.


#---------------------------------------------------------------------------------------------








# Para calcular todas las formas posibles en que podemos elegir dos cartas 
# cuando el orden importa, escribimos:

hands <- permutations(52, 2, v = deck)





# Esta es una matriz con dos columnas y 2652 filas. 
# Con una matriz podemos obtener la primera y segunda carta así:


first_card <- hands[,1]
second_card <- hands[,2]




# Ahora los casos para los cuales la primera carta es una K se pueden calcular así:

kings <- paste("King", suits)
sum(first_card %in% kings)



# Para obtener la probabilidad condicional, calculamos qué fracción de estos
# tiene una K como la segunda carta:


sum(first_card%in%kings & second_card%in%kings)/ sum(first_card%in%kings)



# que es exactamente 3/51, como ya habíamos deducido.


# Tengan en cuenta que el código anterior es equivalente a:

mean(first_card%in%kings & second_card%in%kings)/ mean(first_card%in%kings)

# que usa mean en lugar de sum y es una versión R de P(B|A)











# ¿Y qué tal si el orden no importa? 
# Por ejemplo, en Blackjack, si le dan un As 
# y una carta de figura como su primera mano, se llama un Natural 21 y ganan automáticamente. 

# Si quisiéramos calcular la probabilidad de que esto suceda, enumeraríamos las combinaciones, 
# no las permutaciones, ya que el orden no importa.


#---------------------------------------------------------------------------------------------

combinations(3,2)


#---------------------------------------------------------------------------------------------



# Para calcular la probabilidad de un Natural 21, podemos hacer esto:


aces <- paste("Ace", suits)

facecard <- c("King", "Queen", "Jack", "Ten")
facecard <- expand.grid(number = facecard, suit = suits)
facecard <- paste(facecard$number, facecard$suit)


hands <- combinations(52, 2, v = deck)
mean(hands[,1] %in% aces & hands[,2] %in% facecard)









# podríamos haber producido la misma respuesta al escribir lo siguiente:
mean((hands[,1] %in% aces & hands[,2] %in% facecard) |
       (hands[,2] %in% aces & hands[,1] %in% facecard))









