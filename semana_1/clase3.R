library(dslabs)

data(murders)





    




##### SORTING ############

# La función sort ordena un vector en orden creciente.


data(murders)

murders

sort(murders$populatio)








# La función order toma un vector como entrada y devuelve
# el vector de índices que clasifica el vector de entrada.


x <- c(31, 4, 15, 92, 65)
x

order(x)








## la función order devuelve el índice que ordena el vector de entrada


order(x)



index <- order(x)
index

x[index] 






## ¿Cómo ordenar los estados por asesinatos?



murders$state[1:20]




murders$abb[1:30]

murders


## Primero obtenemos el índice que ordena los vectores por el total de asesinatos
## luego ponemos el vector de nombres de estado en un índice

ind <- order(murders$total)



murders$state[ind]

murders$abb[ind]








# Si solo estamos interesados en la entrada con el mayor valor, podemos usar max:

max(murders$total)



## which.max para el índice del valor mayor:

i_max <- which.max(murders$total)
murders$state[i_max]



## podemos usar min y which.min del mismo modo.









# suma de vectores 
x <- c(1, 2, 3)
y <- c(10, 20, 30)
x+y





x <- c(1, 2, 3)
y <- c(10, 20, 30, 40, 50, 60, 70)
x+y


# otras operaciones

x <- c(1, 2, 3, 4, 5)

x*3

x-9


## Así como con la suma aplica para otras operaciones matemáticas, como -, * y /



murders

## Esto implica que para calcular las tasas de asesinatos lo hacemos así


murder_rate <- murders$total/ murders$population * 100000
murder_rate








# para poner a los estados en orden por tasa de asesinatos:

murders$state[order(murder_rate)]




murders$abb[order(murder_rate)]




# la tasa promedio de asesinatos para EE. UU.

mean(murder_rate)





################# EJERCICIOS ####################

temp <- c(35, 88, 42, 84, 81, 30)
city <- c("Beijing", "Lagos", "Paris", "Rio de Janeiro",
          "San Juan", "Toronto")
city_temps <- data.frame(name = city, temperature = temp)

city_temps












###### Indexación


##  crear subconjuntos con lógicos








ind <- murder_rate < 0.71
ind




ind <- murder_rate <= 0.71
ind




### Para ver qué estados son estos, podemos aprovechar el hecho de que
### los vectores se pueden indexar con lógicos.


murders$state[ind]






## Para contar cuántos son TRUE, la función sum devuelve la suma de las entradas de un vector y fuerza una conversión 
## de los vectores lógicos a numéricos con TRUE codificado como 1 y FALSE como 0. 


sum(ind)










## un estado seguro en la región occidental del país y que la tasa de asesinatos sea como máximo 1.


## Operadores lógicos

#  operador lógico and (&) 
# Esta operación da como resultado TRUE solo cuando ambos lógicos son TRUE

TRUE & TRUE





murders



west <- murders$region == "West"
west


safe <- murder_rate <= 1
safe


ind <- safe & west
ind


murders$state[ind]






###### which

## Cual es la tasa de asesinatos de California?





## La función which nos dice qué entradas de un vector lógico son TRUE. 


ind <- which(murders$state == "California")
murder_rate[ind]








### match

## tasas de asesinatos de varios estados


ind <- match(c("Hawaii" , "Idaho"   ,"Oregon" , "Utah"  ,  "Wyoming"), murders$state)
ind


murder_rate[ind]




## %in%

## Boston, Dakota y Washington son estados?



c("Boston", "Dakota", "Washington") %in% murders$state








### Gráficos básicos

## plot

x <- murders$population/ 10^6
y <- murders$total
plot(x, y)








### TAREA



# 1
murder_rate <- murders$total/ murders$population * 100000
murder_rate

low <- murder_rate <1
low

# 2
ind <- which(low)
ind

# 3
murders$state[ind]

# 4 
states <- murders$region == "Northeast"
ind1 <- states & low
murders$state[ind1]

# 5
mean <- murder_rate < mean(murder_rate)
sum(mean)

# 6
ind <- match(c("AK", "MI", "IA"), murders$abb)
murders$state[ind]

#7 
x <- c("MA", "ME", "MI", "MO", "MU") %in% murders$abb
x




