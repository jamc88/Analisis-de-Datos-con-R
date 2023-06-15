####################### 
# ggplot 2
######################
library(dslabs)
library(tidyverse)
library(dplyr)
library(ggplot2)









# ggplot2 divide los gráficos en componentes de una manera que 
# permite  crear gráficos relativamente complejos y estéticamente 
# agradables utilizando una sintaxis intuitiva y relativamente 
# fácil de recordar.



# El primer paso para crear un gráfico ggplot2 es definir un objeto
# ggplot. Hacemos esto con la función ggplot

data(murders)

ggplot(data = murders) # ó murders %>% ggplot()



p <- ggplot(data = murders) # podemos asignar el gráfico a un objeto
p







################# GEOMETRÍAS ##################

# En ggplot2 creamos gráficos agregando capas (layers).
# Las capas pueden definir geometrías, calcular estadísticas de resumen,
# definir qué escalas (scales) usar o incluso cambiar estilos. 
# Para añadir capas, usamos el símbolo +. 

# En general, una línea de código se verá así:


#  DATOS %>% ggplot() + CAPA 1 + CAPA 2 + … + CAPA N

# Usualmente, la primera capa que agregamos define la geometría. 



# Los nombres de las funciones de geometría siguen el patrón: geom_X 
# donde X es el nombre de la geometría. 
# Algunos ejemplos incluyen geom_point, geom_bar y geom_histogram.









################# MAPEOS ESTÉTICOS ##################

# Los mapeos estéticos (aesthetic) describen 
# cómo las propiedades de los datos se conectan con las características 
# del gráfico, como la distancia a lo largo de un eje, 
# el tamaño o el color. 

# La función aes conecta los datos 
# con lo que vemos en el gráfico mediante la definición de 
# asignaciones estéticas 


murders %>% ggplot() +
  geom_point(aes(x = population/10^6, y = total))



# Podemos quitar el x  e y  ya que estos son el primer 
# y el segundo argumento esperado

p <- ggplot(data = murders)
p + geom_point(aes(population/10^6, total))

# La escala y las etiquetas se definen por defecto al agregar esta capa










################# CAPAS ##################

# Una segunda capa en el gráfico que queremos hacer implica añadir
# una etiqueta a cada punto para identificar el estado. 
# Las funciones geom_label y geom_text nos permiten añadir texto al gráfico
# con o sin un rectángulo detrás del texto, respectivamente.

p + geom_point(aes(population/10^6, total)) +
  geom_text(aes(population/10^6, total, label = abb))









################# Probar varios argumentos ##################


# Cada función de geometría tiene más argumentos además de aes
# y data. Estos suelen ser específicos de la función. 
# Por ejemplo, en el gráfico, los puntos son más grandes
# que el tamaño predeterminado. 
# La estética size cambia este tamaño:


p + geom_point(aes(population/10^6, total), size = 3) + 
  geom_text(aes(population/10^6, total, label = abb))






# el argumento nudge_x mueve el texto ligeramente hacia la derecha 
# o hacia la izquierda:

p + geom_point(aes(population/10^6, total), size = 3) +
  geom_text(aes(population/10^6, total, label = abb), nudge_x = 1.5)









# Si definimos un mapeo en ggplot, todas las geometrías que se agregan 
# como capas se asignarán por defecto a este mapeo. Redefinimos p:


p <- murders %>% ggplot(aes(population/10^6, total, label = abb))



# y entonces podemos simplemente escribir el siguiente código 
# para producir el gráfico anterior:

p + geom_point(size = 3) +
  geom_text(nudge_x = 1.5)











######################### ESCALAS ############################


# las escalas que queremos están en escala logarítmica. Este no es el valor 
# predeterminado, por lo que este cambio debe añadirse a través de una capa de 
# escalas.  

# La función scale_x_continuous nos permite controlar el comportamiento de 
# las escalas.

p + geom_point(size = 3) +
  geom_text(nudge_x = 0.05) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10")





# Esta transformación particular es tan común que ggplot2 ofrece dos funciones 
# especializadas scale_x_log10 y scale_y_log10


p + geom_point(size = 3) +
  geom_text(nudge_x = 0.05) +
  scale_x_log10() +
  scale_y_log10()







######################### ETIQUETAS Y TÍTULOS ###########################

# para cambiar las etiquetas y añadir un título,
# utilizamos las siguientes funciones:

p + geom_point(size = 3) +
  geom_text(nudge_x = 0.05) +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Populations in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010")









######################## CATEGORÍAS COMO COLORES #########################


# Podemos cambiar el color de los puntos usando el argumento 
# col en la función geom_point


# Para facilitar la demostración de características nuevas, 
# redefiniremos p para ser todo 
# excepto la capa de puntos:



p <- murders  %>% ggplot(aes(population/10^6, total, label = abb)) +
  geom_text(nudge_x = 0.05) +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Populations in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010")



# Agregamos diferentes llamadas a geom_point. Por ejemplo, podemos hacer que todos
# los puntos sean azules agregando el argumento color:


p + geom_point(size = 3, color ="blue")


# Buscamos asignar color según la región geográfica.  
# Si asignamos una variable categórica al color, automáticamente  ggplot2
# asigna un color diferente a cada categoría, además de una leyenda.


p + geom_point(aes(col=region), size = 3)   # show.legend = FALSE







######################## ANOTACIÓN, FORMAS Y AJUSTES #########################


# Queremos añadir una línea que represente la tasa promedio de asesinatos en todo el país.

# Una vez que determinemos la tasa por millón  (r), esta línea se define por la fórmula:

# y = rx, con y y x asesinatos totales y población en millones, respectivamente.

# En la escala logarítmica, esta línea se convierte en: 

# log(y) = log(r) + log(x)

# Calculamos el valor de r




r <- murders  %>%
  summarize(rate = sum(total)/ sum(population) * 10^6)  %>%
  pull(rate)


# Para añadir una línea y = ax+b (pendiente (a) intercepto (b)), usamos la función geom_abline.
# En nuestro gráfico, es una línea con pendiente 1 e intercepto log(r).


p + geom_point(aes(col=region), size = 3) +
  geom_abline(slope = 1, intercept = log10(r))



# Podemos cambiar el tipo de línea y el color de las líneas usando argumentos. 
# Además, la dibujamos primero para que no tape nuestros puntos.



p <- p + geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
  geom_point(aes(col=region), size = 3)

p



# Podemos hacer cambios a la leyenda a través de la función scale_color_discrete. 
# En nuestro gráfico original, la palabra region está en mayúscula y podemos cambiarla así:


p <- p + scale_color_discrete(name = "Region")
p




#################### PAQUETES COMPLEMENTARIOS #####################



# El paquete ggthemes añade muchos otros temas:
  
library(ggthemes)



p + theme_economist()




# El paquete de complementos ggrepel incluye una geometría que
# añade etiquetas a la vez que garantiza que no se superpongan entre sí. 
# Para utilizarla, simplemente cambiamos geom_text a geom_text_repel.

library(ggrepel)









#################### COMBINANDO TODO ###########################


library(ggthemes)
library(ggrepel)

r <- murders %>%
  summarize(rate = sum(total)/ sum(population) * 10^6) %>%
  pull(rate)

murders %>% ggplot(aes(population/10^6, total, label = abb)) +
  geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
  geom_point(aes(col=region), size = 3) +
  geom_text_repel() +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Populations in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010") +
  scale_color_discrete(name = "Region") +
  theme_economist()








###################### Gráficos rápidos con qplot #############################

# Hay casos en que sólo necesitamos un gráfico rápido


data(murders)
x <- log10(murders$population)
y <- murders$total




# SI queremos hacer un diagrama de dispersión con ggplot2, 
# tendríamos que escribir algo como:

data.frame(x = x, y = y) %>%
  ggplot(aes(x, y)) +
  geom_point()



# La función qplot nos permite rápidamente generar un gráfico.

qplot(x, y)




################## Cuadrículas de gráficos ######################


# A menudo tenemos que poner gráficos uno al lado del de otro.
# El paquete gridExtra nos permite hacer eso:



library(gridExtra)







p1 <- qplot(x)
p2 <- qplot(x,y)
grid.arrange(p1, p2, ncol = 2)








corredores <- read_csv("corredores.csv")

x <- corredores$Edad
y <- corredores$Tiempo

data.frame(x = x, y = y) %>%
  ggplot(aes(x, y)) +
  geom_point()


corredores





