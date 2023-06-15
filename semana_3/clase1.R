library(dplyr)
library(dslabs)

data(heights)





heights






# La función summarize de dplyr ofrece una forma de calcular estadísticas 
# de resumen con código intuitivo y legible.











# calcula el promedio y la desviación estándar para las mujeres

s <- heights %>%
  filter(sex == "Female") %>%
  summarize(average = mean(height), standard_deviation = sd(height))
s

heights








# Como la tabla resultante almacenada en s es un data frame, 
# podemos acceder a los componentes con el operador de acceso $


s$average

s$standard_deviation












# La tasa de asesinatos de Estados Unidos es el número total de
# asesinatos en Estados Unidos dividido por la población total.

murders <- murders %>% mutate(rate = total/population*100000)

us_murder_rate <- murders %>%
  summarize(rate = sum(total)/ sum(population) * 100000)
us_murder_rate








###########################################################################



## Resúmenes múltiples


# Supongamos que queremos tres resúmenes de la misma variable,
# como las alturas mediana, mínima y máxima.










# La función quantile: quantile(x, c(0.5, 0, 1)) devuelve la mediana (percentil 50),
# el mínimo (percentil 0) y el máximo (percentil 100) del vector x. 
# Podemos usarlo con summarize así:







heights %>% 
  filter(sex == "Female") %>%
  summarize(median_min_max = quantile(height, c(0.5, 0, 1)))





# Sin embargo, observe que los resúmenes se devuelven en una fila cada uno. 
# Para obtener los resultados en diferentes columnas, tenemos que definir 
# una función que devuelva un marco de datos como este:


median_min_max <- function(x){
  qs <- quantile(x, c(0.5, 0, 1))
  data.frame(median = qs[1], minimum = qs[2], maximum = qs[3])
}





heights %>% 
  filter(sex == "Female") %>%
  summarize(median_min_max(height))








###  Agrupar y luego resumir con group_by



## Una operación común en la exploración de datos es dividir primero los datos
## en grupos y luego calcular resúmenes para cada grupo. 






# calcular el promedio y la desviación estándar para las alturas de hombres 
# y mujeres por separado. La función group_by nos ayuda a hacer esto.



heights  %>%
  group_by(sex)  %>%
  summarize(average = mean(height), standard_deviation = sd(height))







# calculemos la mediana, el mínimo y máximo de la tasa de asesinatos 
# en las cuatro regiones del país usando la función median_min_max definida 
# anteriormente:



murders %>%
  group_by(region) %>%
  summarize(median_min_max(rate))




## pull



## El objeto us_murder_rate definido anteriormente representa solo un número.
## Sin embargo, lo estamos almacenando en un data frame:




class(us_murder_rate)



# ya que, como la mayoría de las funciones de dplyr, summarize siempre 
# devuelve un data frame.
# Esto podría ser problemático si queremos usar este resultado
# con funciones que requieren un valor numérico.



# para acceder a ese objeto y sus columnas se pueden acceder usando
# la función pull

us_murder_rate %>% pull(rate)






# Para obtener un número de la tabla de datos original 
# con una línea de código, podemos escribir:


us_murder_rate <- murders  %>%
  summarize(rate = sum(total)/ sum(population) * 100000)  %>%
  pull(rate)

us_murder_rate


class(us_murder_rate)






### Ordenar los data frames

# Conocemos las funciones order y sort para ordenar, numérica o alfabéticamente 
# las columnas de un dataframe, pero para ordenar tablas enteras, 
# la función arrange de dplyr es útil.


murders %>%
  arrange(population) %>%
  head()




# En dplyr, la función desc transforma un vector para que esté 
# en orden descendente. 





murders %>%
  arrange(desc(rate))






### ordenar anidadamente


# Si estamos ordenando una columna cuando hay empates, 
# podemos usar una segunda columna para romper el empate. 



# ordenamos por region y entonces, dentro de la región,
# ordenamos por tasa de asesinatos:


murders %>%
  arrange(region, total) 







# Del mismo modo, se puede usar una tercera columna 
# para romper empates entre la primera y la segunda, y así sucesivamente.






#  Los primeros n

# Si queremos ver una mayor proporción, podemos usar la función top_n. 
# Esta función toma un data frame como primer argumento, el número de
# filas para mostrar en el segundo y la variable para filtrar en el tercero. 


murders  %>% top_n(10, rate)














murders  %>% top_n(10, rate) %>% arrange(desc(rate))







## Ejemplo--------------------------------------------------------
data(na_example)


na_example





mean(na_example)
sd(na_example)

## Para ignorar los NAs, podemos usar el argumento na.rm


mean(na_example, na.rm = TRUE)

sd(na_example, na.rm = TRUE)



#----------------------------------------------------------------




############################# EJERCICIOS #######################################
library(NHANES)
data(NHANES)



# datos de la encuesta recopilada por el Centro Nacional de Estadísticas de Salud de Estados Unidos (NCHS


# Los datos NHANES tienen muchos valores faltantes.



