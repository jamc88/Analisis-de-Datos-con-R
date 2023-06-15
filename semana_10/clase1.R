####################################
# Métricas de evaluación
####################################

# Para nuestra primera introducción a los conceptos de machine learning, 
# comenzaremos con un ejemplo: cómo predecir sexo basado en altura.

# Utilizamos el paquete caret, que tiene varias funciones útiles para construir 
# y evaluar métodos de machine learning. 

#install.packages("caret")
#install.packages("lattice")



library(tidyverse)
library(caret)
library(dslabs)
data(heights)


# Comenzamos definiendo el resultado y los predictores.

y <- heights$sex
x <- heights$height


# En este caso, solo tenemos un predictor, altura, mientras que y es claramente 
# un resultado categórico ya que los valores observados son Male o Female. 
# Sabemos que no podremos predecir Y de forma precisa basado en X porque las 
# alturas promedio masculinas y femeninas no son tan diferentes en relación 
# con la variabilidad dentro del grupo. 



################### Sets de entrenamiento y de evaluación ########################

# Un algoritmo de machine learning se evalúa basado en cómo funciona en el mundo real
# con sets de datos completamente nuevos. Sin embargo, cuando desarrollamos un algoritmo, 
# generalmente tenemos un set de datos para el cual conocemos los resultados, 
# como lo hacemos con las alturas: sabemos el sexo de cada estudiante en nuestro set de datos.
# Por lo tanto, para imitar el proceso de evaluación final, generalmente dividimos los datos
# en dos partes y actuamos como si no supiéramos el resultado de una de estas.

# Dejamos de fingir que no conocemos el resultado para evaluar el algoritmo, 
# pero solo después de haber terminado de construirlo. 

# Nos referimos al grupo para el que conocemos el resultado y que usamos para desarrollar
# el algoritmo como el set de entrenamiento (training set). 

# Nos referimos al grupo para el que aparentamos no conocer el resultado como el set de 
# evaluación (test set).


# Una forma estándar de generar los sets de entrenamiento y de evaluación es dividiendo 
# aleatoriamente los datos. 


# El paquete caret incluye la función createDataPartition que nos ayuda a generar 
# índices para dividir aleatoriamente los datos en sets de entrenamiento y de evaluación:



set.seed(2007)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)



# El argumento "times" se usa para definir cuántas muestras aleatorias de índices devolver, 
# el argumento "p" se utiliza para definir qué proporción de los datos está representada por
# el índice "y" el argumento list se usa para decidir si queremos que los índices 
# se devuelvan como una lista o no. 



# Podemos usar el resultado de la llamada a la función createDataPartition para definir
# los sets de entrenamiento y de evaluación.

test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]



# Ahora desarrollaremos un algoritmo usando solo el set de entrenamiento. 
# Una vez que hayamos terminado de desarrollar el algoritmo, lo congelaremos 
# y lo evaluaremos utilizando el set de evaluación.

# La forma más sencilla de evaluar el algoritmo cuando los resultados son
# categóricos es simplemente informar la proporción de casos que se predijeron
# correctamente en el set de evaluación.
# Esta métrica generalmente se conoce como exactitud general




# Para demostrar el uso de la exactidud general, crearemos dos algoritmos diferentes
# y los compararemos.

# Comencemos desarrollando el algoritmo más sencillo posible: adivinar el resultado.

y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE)


# Tengan en cuenta que estamos ignorando completamente el predictor y simplemente 
# adivinando el sexo.


# En las aplicaciones de machine learning, es útil usar factores para representar
# los resultados categóricos porque las funciones de R desarrolladas para machine learning,
# como las del paquete caret, requieren o recomiendan que los resultados categóricos 
# se codifiquen como factores. 


y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE) |>
  factor(levels = levels(test_set$sex))


# La exactidud general se define simplemente como la proporción general que se
# predice correctamente:


mean(y_hat == test_set$sex)


# No es sorprendente que nuestra exactidud sea 50%, ya que estamos adivinando.


# ¿Podemos mejorarla? El análisis de datos exploratorios sugiere que sí porque, 
# en promedio, los hombres son un poco más altos que las mujeres:  
  

heights |> group_by(sex) |> summarize(mean(height), sd(height))



# Pero, ¿cómo usamos esta información? Probemos con otro enfoque sencillo: 
# predecir Male si la altura está dentro de dos desviaciones estándar del hombre promedio.


y_hat <- ifelse(x > 62, "Male", "Female") |>
  factor(levels = levels(test_set$sex))



# La exactidud aumenta de 0.50 a aproximadamente 0.80:

mean(y == y_hat)


# ¿Pero podemos mejorarla aún más? En el ejemplo anterior, utilizamos un umbral de 62, 
# pero podemos examinar la exactidud obtenida para otros umbrales y luego elegir el valor 
# que provee los mejores resultados. 


# Sin embargo, es importante que optimicemos el umbral utilizando solo 
# el set de entrenamiento. 

# Aunque para este ejemplo sencillo no es un problema, más adelante veremos 
# que evaluar un algoritmo en el set de entrenamiento puede resultar en un 
# sobreajuste (overfitting), que a menudo resulta en evaluaciones peligrosamente
# sobre optimistas.


# Aquí examinamos la exactidud de 10 umbrales diferentes y elegimos el que
# produce el mejor resultado:


cutoff <- seq(61, 70)

accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") |>
    factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex)
})

cutoff 
accuracy






library(ggplot2)

ggplot(df,aes(x=cutoff,y=accuracy)) +
  geom_line() + geom_point()






# Vemos que el valor máximo es:


max(accuracy)


# que es mucho más grande que 0.5. El umbral que resulta en esta exactitud es:


best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff


# Ahora podemos evaluar el uso de este umbral en nuestro set de evaluaciones 
# para asegurarnos de que nuestra exactitud no sea demasiado optimista:


y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") |>
  factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat)

mean(y_hat == test_set$sex)


# Vemos que es un poco más baja que la exactitud observada para el set de entrenamiento, 
# pero aún es mejor que adivinar. Y al probar en un set de datos en el que no entrenamos,
# sabemos que nuestro resultado no se debe a que se haya elegido para dar un buen resultado
# en el set de evaluación.



########################## Matriz de confusión ##############################

# La regla de predicción que desarrollamos anteriormente predice Male 
# si el alumno es más alto que 64 pulgadas. Dado que la mujer promedio es aproximadamente 
# 64 pulgadas, esta regla de predicción parece incorrecta.
# ¿Que pasó? Si la altura de un estudiante es la de la mujer promedio, 



# En términos generales, la exactitud general puede ser una medida engañosa.
# Para ver esto, comenzaremos construyendo lo que se conoce como matriz de confusión
# (confusion matrix), que tabula cada combinación de predicción y valor real.
# Podemos hacer esto en R usando la función table:

table(predicted = y_hat, actual = test_set$sex)



# Si estudiamos esta tabla detenidamente, revela un problema. 
# Si calculamos la exactitud por separado para cada sexo, obtenemos:

test_set |>
  mutate(y_hat = y_hat) |>
  group_by(sex) |>
  summarize(accuracy = mean(y_hat == sex))



# Hay un desequilibrio en la exactitud para hombres y mujeres: se predice que demasiadas
# mujeres son hombres. Estamos prediciendo que casi la mitad de las mujeres son hombres.
# ¿Cómo es que nuestra exactitud general sea tan alta? 
# Esto se debe a que la prevalencia de los hombres en este set de datos es alta. 

# Estas alturas se obtuvieron de tres cursos, dos de los cuales tenían más
# hombres matriculados:
  
  
prev <- mean(y == "Male")
prev

# Entonces, al calcular la exactitud general, el alto porcentaje de errores cometidos 
# prediciendo cuales son mujeres se ve superado por las ganancias en las predicciones 
# acertadas para los hombres. 

# Esto puede ser un gran problema en machine learning. 
# Si sus datos de entrenamiento están sesgados de alguna manera, es probable 
# que también desarrolle algoritmos sesgados. El hecho de que hayamos utilizado un set
# de evaluación no importa porque también se deriva del set de datos sesgado original. 
















