library(tidyverse)
library(dslabs)

data(murders)



#### Dataframes

class(murders)

str(murders) ###  obtener más información sobre la estructura de un objeto

head(murders) ### mostrar las primeras seis líneas

names(murders) ### acceso rápido a los nombres de las variables usando:

murders$population ## orden de las filas en nuestra tabla de datos

murders$state

murders$region

print(murders)


print(murders)


####################################################


### 
pop <- murders$population
length(pop)


class(pop) #En un vector numérico cada entrada debe ser un número.

class(murders$state)







#vectores lógicos

z <- 3 == 3   # es un operador relacional que pregunta si 3 es igual a 4
z

class(z)





#factores

class(murders$region) #Los factores son útiles para almacenar datos categóricos

levels(murders$region) # hay cuatro regiones 












###########################################################
# LISTAS

registro <- list(nombre = "Alberto",
               matricula = 2121800844,
               calificaciones = c(9.5, 10, 8.5),
               cal_final = "MB")











registro


class(registro)






registro$nombre #pueden extraer los componentes de una lista con el operador de acceso $







registro[["matricula"]] # También podemos usar corchetes dobles 






registro2 <- list("Juan", 2121800844)




registro2








#########################################################





#### MATRICES

mat <- matrix(1:12, 6, 4)
mat












## Pueden acceder a entradas específicas en una matriz usando corchetes


mat[2,3]





#toda la segunda fila
mat[2, ]

# la tercera columna completa
mat[, 3]

# acceder a más de una columna o más de una fila 
mat[, 2:3]

# subconjuntos basados tanto en las filas como en las columnas
mat[1:2, 2:3]








# Podemos convertir las matrices en data frames usando la función as.data.frame

as.data.frame(mat)



# podemos usar corchetes individuales ([) para acceder a las filas y las columnas de un data frame


murders[25, 1]


murders[2:3, ]







#####################################################

## VECTORES

codes <- c(380, 124, 818)
codes



# También podemos crear vectores de caracteres.


country <- c("italy", "canada", "egypt")
country



# es útil nombrar las entradas de un vector

codes <- c('italy' = 380, 'canada' = 124, 'egypt' = 818)
codes






class(codes)

names(codes)









# También podemos asignar nombres usando la función 'names'

codes <- c(380, 124, 818)
country <- c("italy","canada","egypt")
names(codes) <- country

codes









# Sucesiones

seq(1,20)




# un tercer argumento nos permite determinar cuánto saltar



seq(1, 20, 3)


seq(1, 20, 0.5)





#  abreviación

1:20


##### crear un subconjunto


codes



##tener acceso al segundo elemento
codes[2]




# obtener más de una entrada utilizando un vector de entradas múltiples 

codes[c(1,3)]




# si necesitamos acceso a los dos primeros elementos:
codes[1:2]




# Si los elementos tienen nombres, también podemos acceder a las entradas
#utilizando estos nombres.

codes["canada"]

codes[c("egypt","italy")]







##### La conversión forzada


x <- c(1, "canada", 3)
x

class(x)







#### R también ofrece funciones para cambiar de un tipo a otro

x <- 1:5
x
y <- as.character(x)
y

class(y)


# revertir a lo anterior con as.numeric

z <- as.numeric(y)

class(z)






# Not available (NA)

x <- c("1", "b", "3")
as.numeric(x)



############################################################


temp <- c(35, 88, 42, 84, 81, 30)
city <- c('Beijing', 'Lagos', 'Paris', 'R_Janeiro', 'San_Juan', 'Toronto')

names(temp) <- city
temp


temp[c(1:3)]

temp[1]



seq(12, 73)

a <- c(seq(1,100,2))
a

b <- c(seq(6,55, 4/7))
b

length(b)


