##########################################
# visualización de distribuciones de datos
##########################################

library(tidyverse)
library(dslabs)
data(heights)
data(murders)


########### Diagramas de barras ##############  

# tenemos la siguiente tabla

tab <- murders %>%
  count(region) %>%
  mutate(proportion = n/sum(n))
tab




# Para generar un diagrama de barras, podemos usar la geometría geom_bar. 
# Por defecto, R cuenta los casos en cada categoría y dibuja una barra. 

murders %>% ggplot(aes(region)) + geom_bar()
                   
                   
# Si no queremos que geom_bar cuente, sino que simplemente grafique 
# una barra a la altura proporcionada por la variable proportion. 
# Para esto necesitamos proveer x (las categorías) e y (los valores) 
# y usar la opción stat="identity".

tab %>% ggplot(aes(region, proportion)) + geom_bar(stat = "identity") #+ coord_flip()



# colorear cada region (fill)

tab %>% ggplot(aes(region, proportion, fill=region)) + geom_bar(stat = "identity") 


##########################



hom21 <- read_csv("homicidios_2021.csv")


hom21 <- mutate(hom21, tasa = Total/Poblacion * 100000, rango = rank(-tasa))

tab <- hom21 %>%
  count(Regiones) %>%
  mutate(proportion = n/sum(n))
tab

tab  %>% ggplot(aes(Regiones, proportion, fill=Regiones)) + geom_bar(stat = "identity") 



a

########### Histogramas ##############

# Para generar histogramas, utilizamos geom_histogram

heights |>
  filter(sex == "Female") |>
  ggplot(aes(height)) +
  geom_histogram()




heights %>%
  filter(sex == "Female") %>%
  ggplot(aes(height)) +
  geom_histogram(binwidth = 1) # ancho de cada categoría



heights |>
  filter(sex == "Female") |>
  ggplot(aes(height)) +
  geom_histogram(binwidth = 1, fill = "yellow", col = "black") + # añadir color
  xlab("Female heights in inches") +
  ggtitle("Histogram")







########### Gráficos de densidad ##############

# Para hacer un gráfico de densidad suave con los datos que anteriormente visualizamos 
# como un histograma, podemos usar este código:

heights |>
  filter(sex == "Male") |>
  ggplot(aes(height)) +
  geom_histogram(binwidth = 1, fill = "blue", col = "black") + # añadir color
  xlab("Female heights in inches") +
  ggtitle("Histogram") 





heights |>
  filter(sex == "Male") |>
  ggplot(aes(height)) +
  geom_density()


heights |>
  filter(sex == "Male") |>
  ggplot(aes(height)) +
  geom_density(fill="yellow")


heights |>
  filter(sex == "Male") |>
  ggplot(aes(height)) +
  geom_density(fill="yellow", adjust = 1/2) # ajuste
  
  



########### Diagramas de caja ##############


boxplot(height~sex, data = heights, col='blue')




murders$rate <- with(murders, total/population * 100000)

boxplot(rate~region, data = murders, col='blue')





# geom_boxplot() da una mejor visualización de los diagramas de caja

ggplot(murders,aes(x=region,y=rate,fill=region)) +
  geom_boxplot()





# Si queremos solo colores en los bordes
# y líneas de los cuadros de nuestro diagrama, utilizamos:

ggplot(murders,aes(x=region,y=rate,col=region)) +
  geom_boxplot()











########### Distribución normal ##############



index <- heights$sex == "Male"
x <- heights$height[index]

m <- mean(x)
s <- sd(x)
c(average = m, sd = s)



# En R, podemos obtener unidades estándar usando la función scale:

z <- scale(x)

z


# Ahora para ver cuántos hombres hay dentro de 2 SD del promedio, simplemente escribimos:

  
mean(abs(z) < 2)
  


# Calculamos los valores para la altura de los varones que almacenaremos en el objeto x:


heights %>%
  filter(sex == "Male") %>%
  ggplot(aes(height)) +
  geom_density(fill="yellow") + # rellenar con color
  geom_density(adjust = 3)




  
  
  



  
########### Gráficos Q-Q ##############

# Construyamos un gráfico Q-Q usando el código R. Comiencen definiendo el vector de proporciones.

p <- seq(0.05, 0.95, 0.05)

p




# Para obtener los cuantiles de los datos, podemos usar la función quantile así:

sample_quantiles <- quantile(x, p)





# Para obtener los cuantiles teóricos de distribución normal con promedio y 
# desviación estándar (SD) correspondiente, utilizamos la función qnorm:


theoretical_quantiles <- qnorm(p, mean = mean(x), sd = sd(x))





# Para ver si coinciden o no, los graficamos uno contra el otro y dibujamos la línea de identidad:

qplot(theoretical_quantiles, sample_quantiles) + geom_abline()




# Para gráficos Q-Q, usamos la geometría geom_qq.
# Aquí tenemos el gráfico Q-Q para alturas de varones:


heights |> filter(sex=="Male") |>
  ggplot(aes(sample = height)) +
  geom_qq()







# La variable muestral se compara con una distribución normal con 
# media de 0 y una desviación estándar de 1. Para cambiar esto,
# utilizamos el argumento dparams

# Para líneas rectas, usamos la función geom_abline. 
# La línea por defecto es la línea de identidad (pendiente = 1, intercepto = 0).

params <- heights |> filter(sex=="Male") |>
  summarize(mean = mean(height), sd = sd(height))

heights |> filter(sex=="Male") |>
  ggplot(aes(sample = height)) +
  geom_qq(dparams = params) +
  geom_abline()







########### Imágenes ##############

# Existen dos geometrías utilizadas para crear imágenes: geom_tile y geom_raster. 

# Para crear una imagen en ggplot2, necesitamos un data frame con las coordenadas x e y,
# así como los valores asociados con cada uno de estos

x <- expand.grid(x = 1:12, y = 1:10) |>
  mutate(z = 1:120)

# Para graficar la imagen, usamos el siguiente código:
  
x |> ggplot(aes(x, y, fill = z)) +
  geom_raster()


# Con estas imágenes, a menudo querrán cambiar la escala de color. 
# Esto se puede hacer a través de la capa scale_fill_gradientn.

x |> ggplot(aes(x, y, fill = z)) +
  geom_raster() +
  scale_fill_gradientn(colors = terrain.colors(10))





