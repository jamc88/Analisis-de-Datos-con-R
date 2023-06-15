library(dslabs)

data(murders)
murders

x <- murders$population/ 10^6
y <- murders$total
plot(x, y)










### Para crear un gráfico rápido

# La función with nos permite usar los nombres de la columna murders en la función plot.

with(murders, plot(population, total))







### función hist()




### los histogramas son un resumen gráfico eficaz de una lista de números que nos ofrece 
### una descripción general de los tipos de valores que tenemos

x <- with(murders, total/ population * 100000)
hist(x)





## Podemos ver que hay una amplia gama de valores con la mayoría de ellos entre 2 y 3 
##y un caso muy extremo con una tasa de asesinatos de más de 15:

murders$state[which.max(x)]




## Boxplot


# Los diagramas de caja (boxplots en inglés) proveen un resumen más conciso que los histogramas,
# y son más fáciles de apilar con otros diagramas de caja. 


murders$rate <- with(murders, total/ population * 100000)
boxplot(rate~region, data = murders)





### image

## La función image muestra los valores en una matriz usando color. 


x <- matrix(1:120, 10, 10)
image(x)








## CONCEPTOS BÁSICOS DE PROGRAMACIÓN


# Las expresiones condicionales son una de las características básicas de la programación. 
# Se utilizan para lo que se denomina flow control. La expresión condicional más común 
# es la declaración if-else.





a <- 0

if(a!=0){
  print(1/a)
} else{
  print("No existe el inverso multiplicativo.")
}





# estados, si los hay, que tienen una tasa de homicidios inferior a 0.5 por 100,000


murder_rate <- murders$total/ murders$population*100000


ind <- which.min(murder_rate)

if(murder_rate[ind] < 0.5){
  print(murders$state[ind])
} else{
  print("Ningún estado tiene una tasa de homicidios tan baja")
}









## ifelse es una función que toma tres argumentos: un lógico y dos posibles respuestas. 
# Si el lógico es TRUE, devuelve el valor en el segundo argumento y, si es FALSE, 
# devuelve el valor en el tercer argumento.


a <- 0
ifelse(a > 0, 1/a, NA)




# Esta función es particularmente útil porque sirve para vectores. 
# Examina cada entrada del vector lógico y devuelve elementos del 
# vector proporcionado en el segundo argumento, si la entrada es TRUE, 
# o elementos del vector proporcionado en el tercer argumento, si la entrada es FALSE




a <- c(0, 1, 2, -4, 5)

result <- ifelse(a > 0, 1/a, NA)

result




# Otras dos funciones útiles son any y all. La función any toma un vector 
# de lógicos y devuelve TRUE si alguna de las entradas es TRUE. La función all toma 
# un vector de lógicos y devuelve TRUE si todas las entradas son TRUE

z <- c(TRUE, TRUE, FALSE)
any(z)

all(z)








#### FUNCIONES

prom <- function(x){
  s <- sum(x)
  n <- length(x)
  s/n
}


prom()
mean()



x <- 1:100
identical(mean(x), prom(x))








s <- 3   #las variables definidas dentro de una función no se guardan en el espacio de trabajo.
prom(x)









# Las funciones son objetos, por lo que les asignamos nombres de variables con <-. La función function le dice a R 
# que están a punto de definir una función. La forma general de la definición de una función es así:


my_function <- function(NOMBRE_VARIABLE){
  # realizar operaciones en NOMBRE_VARIABLE y calculamos el VALUE
  VALUE
}







### BUCLES-for


sumas_s_n <- function(n){
  x <- 1:n
  sum(x)
}


sumas_s_n(10)



# ¿Cómo podemos calcular Sn para varios valores de n, digamos n=1,…,25?



for(i in 1:25){
  print(i)
}











m <- 25
s_n <- vector(length = m) # crear un vector vacío

for(n in 1:m){
  s_n[n] <- sumas_s_n(n)
}


s_n





# En cada iteración n=1, n=2, etc …, calculamos Sn 
# y lo guardamos en la entrada n de s_n.


n <- 1:m
plot(n, s_n)






### Vectorización y funcionales

# Una función vectorizada es una función que aplicará 
# la misma operación en cada uno de los vectores.






x <- 1:10
sqrt(x)


y <- 1:10
x*y






n <- 1:25 
sumas_s_n(n) #Este fragmento de código no ejecuta la función en cada entrada de n



## Los funcionales son funciones que nos ayudan a aplicar 
## la misma función a cada entrada en un vector, matriz, data frame o lista. 


## La función sapply nos permite realizar operaciones basadas en elementos 
## en cualquier función. 


x <- 1:10
sapply(x, sqrt)



# el bucle-for anterior puede escribirse de la siguiente manera:

n <- 1:25
s_n <- sapply(n, sumas_s_n)
s_n


graf <- function(x,y){
  
}



### Ejercicio



graf_sr <- function(n,m){
  plot(n+m,n-m)
}

graf_sr(a,1)



######################

cuadr_s_n <- function(n){
  x <- 1:n
  y <- 1:n
  sum(x*y)
}

cuadr_s_n(10)







###########

cuadrado <- function(n){
  f_n <- vector(length = n) # crear un vector vacío
  for(m in 1:n){
    f_n[m] <- m^2
  }
  f_n
}

cuadrado(25)

