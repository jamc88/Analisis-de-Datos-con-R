#################################################
# Visualización de datos en la práctica
#################################################



library(tidyverse)
library(dslabs)




data(gapminder)



gapminder %>% as_tibble()


gapminder

# ¿qué países creen que tuvieron las tasas de mortalidad 
# infantil más altas en 2015? ¿Qué pares creen que son más similares?
  



# Sri Lanka o Turquía
# Polonia o Corea del Sur
# Malasia o Rusia
# Pakistán o Vietnam
# Tailandia o Sudáfrica



gapminder |>
  filter(year == 2015 & country %in% c("Poland","South Korea")) |>
  select(country, infant_mortality)




# Existe la noción de que el mundo está dividido en dos grupos: 
# el mundo occidental (Europa occidental y América del Norte), caracterizado 
# por una larga vida y familias pequeñas, versus el mundo en desarrollo
# (África, Asia y América Latina),


# Para analizar esta visión del mundo, hacemos un diagrama de dispersión 
# de la esperanza de vida vs las tasas de fertilidad 
# (número promedio de hijos por mujer)

# Comenzamos mirando los datos de hace unos 50 años:

filter(gapminder, year == 1962) |>
  ggplot(aes(fertility, life_expectancy)) +
  geom_point()





# La mayoría de puntos se dividen en dos categorías distintas:

# Esperanza de vida alrededor de 70 años y 3 o menos hijos por familia.
# Esperanza de vida inferior a 65 años y más de 5 niños por familia.







# Para confirmar que estos países son de las regiones que esperamos, 
# podemos usar un color para representar un continente.

filter(gapminder, year == 1962) |>
  ggplot( aes(fertility, life_expectancy, color = continent)) +
  geom_point()










# Para hacer comparaciones es preferible graficar lado a lado.

#Para separar en facetas, añadimos una capa con la función facet_grid:


filter(gapminder, year%in%c(1962, 2012)) |>
  ggplot(aes(fertility, life_expectancy, col = continent)) +
  geom_point() +
  facet_grid(continent~year)






# Para comparar todas en un solo gráfico  usamos "." 
# para que facet_grid sepa que no estamos usando 
# una de las variables:


filter(gapminder, year%in%c(1962, 2012)) |>
  ggplot(aes(fertility, life_expectancy, col = continent)) +
  geom_point() +
  facet_grid(. ~ year)




# En 2012, la visión del mundo occidental versus el mundo 
# en desarrollo ya no tiene sentido.



# Podemos hacer el gráfico para varios años. Por ejemplo,
# podemos añadir los años 1970, 1980, 1990 y 2000.

# La función facet_wrap nos permite hacer esto automáticamente
# acomodando la serie de gráficos para que cada imagen tenga 
# dimensiones visibles:



years <- c(1962, 1980, 1990, 2000, 2012)
continents <- c("Europe", "Asia")
gapminder |>
  filter(year %in% years & continent %in% continents) |>
  ggplot( aes(fertility, life_expectancy, col = continent)) +
  geom_point() +
  facet_wrap(~year)



# Este gráfico muestra claramente cómo la mayoría de los países 
# asiáticos han mejorado a un ritmo mucho más rápido 
# que los europeos.



# Si queremos ajustar las escalas:

filter(gapminder, year%in%c(1962, 2012)) |>
  ggplot(aes(fertility, life_expectancy, col = continent)) +
  geom_point() +
  facet_wrap(. ~ year, scales = "free")






########### Gráficos de series de tiempo ############


# Los gráficos de series de tiempo tienen tiempo en el eje-x 
# y un resultado o medida de interés en el eje-y.

# Por ejemplo, aquí vemos un gráfico de la tendencia de 
# las tasas de fertilidad de Estados Unidos:





gapminder |>
  filter(country == "United States") |>
  ggplot(aes(year, fertility)) +
  geom_point()





# Con geom_line, creamos una curva que une los puntos con líneas,
# para transmitir que estos datos provienen de una sola serie

gapminder |>
  filter(country == "United States") |>
  ggplot(aes(year, fertility)) +
  geom_line()


# Si creamos un subconjunto de los datos para incluir dos países,
# uno de Europa y uno de Asia. Para que ggplot entienda que hay dos 
# curvas que se deben hacer por separado, asignamos cada punto a un group, 
# uno para cada país:


countries <- c("South Korea","Germany")

gapminder |> filter(country %in% countries & !is.na(fertility)) |>
  ggplot(aes(year, fertility, group = country)) +
  geom_line()


# para asignar diferentes colores a los diferentes países utilizamos:


countries <- c("South Korea","Germany")

gapminder |> filter(country %in% countries & !is.na(fertility)) |>
  ggplot(aes(year,fertility, col = country)) +
  geom_line()




# Etiquetas en lugar de leyendas

# usamos los datos de esperanza de vida.


labels <- data.frame(country = countries, x = c(1975,1965), y = c(60,72))

gapminder |>
  filter(country %in% countries) |>
  ggplot(aes(year, life_expectancy, col = country)) +
  geom_line() +
  geom_text(data = labels, aes(x, y, label = country), size = 5) +
  theme(legend.position = "none")




# ¿Los países pobres se han vuelto más pobres y los países ricos?


# El GDP mide el valor de mercado de los bienes y servicios producidos por un país en un año. 
# El GDP por persona a menudo se usa como un resumen aproximado de la riqueza de un país. 


# Dividimos el GDP por 365 para obtener la medida más interpretable de dólares por día. 
# Una persona que sobrevive con un ingreso de menos de $2 por día se define 
# como viviendo en la “pobreza absoluta”





gapminder <- gapminder |> mutate(dollars_per_day = gdp/population/365)



# Histograma de ingresos diarios de 1970:

past_year <- 1970
gapminder |>
  filter(year == past_year & !is.na(gdp)) |>
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black", fill='green')







# Sería más informativo poder ver rápidamente cuántos países 
# tienen ingresos diarios promedio de aproximadamente 
# $1 (extremadamente pobre), $2 (muy pobre), $4 (pobre), $8 (promedio),
# $16 (acomodado), $32 (rico) y $64 (muy rico) 

# Estos cambios son multiplicativos y las transformaciones logarítmicas
# convierten los cambios multiplicativos en aditivos: cuando se usa la base 2, 
# la duplicación de un valor se convierte en un aumento de 1.



gapminder |>
  filter(year == past_year & !is.na(gdp)) |>
  ggplot(aes(log2(dollars_per_day))) +
  geom_histogram(binwidth = 1, color = "black")




# Hay dos formas en que podemos usar las transformaciones logarítmicas en los gráficos. 
# Podemos tomar el logaritmo de los valores antes de graficarlos
# o usar escalas logarítmicas en los ejes.


gapminder |>
  filter(year == past_year & !is.na(gdp)) |>
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2")







######## Comparación de múltiples distribuciones con diagramas de caja y gráficos ridge ##########

# Comencemos examinando rápidamente los datos por región. Reordenamos las regiones
# por la mediana y usamos una escala logarítmica.




gapminder |>
  filter(year == past_year & !is.na(gdp)) |>
  mutate(region = reorder(region, dollars_per_day, FUN = median)) |>
  ggplot(aes(dollars_per_day, region)) +
  geom_point() +
  scale_x_continuous(trans = "log2")



# Podemos ver que existen dos grupos claros, con el grupo rico compuesto por Norteamérica, 
# Europa del Norte y Occidental, Nueva Zelanda y Australia 
# y el grupo conformado por el resto de regiones.



# Definimos grupos basados en esta observación:

gapminder <- gapminder |>
  mutate(group = case_when(
    region %in% c("Western Europe", "Northern Europe","Southern Europe",
                  "Northern America",
                  "Australia and New Zealand") ~ "West",
    region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    region %in% c("Caribbean", "Central America",
                  "South America") ~ "Latin America",
    continent == "Africa" &
      region != "Northern Africa" ~ "Sub-Saharan",
    TRUE ~ "Others"))


# Convertimos esta variable group en un factor para controlar el orden de los niveles:
  
gapminder <- gapminder |>
  mutate(group = factor(group, levels = c("Others", "Latin America",
                                          "East Asia", "Sub-Saharan",
                                          "West")))





p <- gapminder |>
  filter(year == past_year & !is.na(gdp)) |>
  ggplot(aes(group, dollars_per_day)) +
  geom_boxplot() +
  scale_y_continuous(trans = "log2") +
  xlab("") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
p




# Los diagramas de caja tienen la limitación de que al resumir los datos en cinco números, 
# se pueden perder características importantes de los datos. Una forma de evitar esto 
# es mostrando los datos.



p + geom_point(alpha = 0.5)






# Los diagramas de caja ayudan con esto al proveer un resumen de cinco números, pero esto también 
# tiene limitaciones. Por ejemplo, los diagramas de caja no revelan distribuciones bimodales.


# Podemos mostrar densidades suaves o histogramas apilados utilizando gráficos ridge. 


library(ggridges)


p <- gapminder |>
  filter(year == past_year & !is.na(dollars_per_day)) |>
  ggplot(aes(dollars_per_day, group)) +
  scale_x_continuous(trans = "log2")

p + geom_density_ridges()




# Si el número de puntos de datos es lo suficientemente pequeño, 
# podemos añadirlos al gráfico ridge:


p + geom_density_ridges(jittered_points = TRUE)
  
  
  
# podemos usar el siguiente código para agregar lo que se conoce 
# como una representación rug de los datos.
  
  
  
p + geom_density_ridges(jittered_points = TRUE,
                        position = position_points_jitter(height = 0),
                        point_shape = '|', point_size = 3, 
                        point_alpha = 1, alpha = 0.7)  




# La exploración de datos muestra claramente que en 1970 hubo una dicotomía 
# del “Oeste versus el Resto”. ¿Pero persiste esta dicotomía?


# Vamos a usar facet_grid para ver cómo han cambiado las distribuciones.

past_year <- 1970
present_year <- 2010
years <- c(past_year, present_year)
gapminder |>
  filter(year %in% years & !is.na(gdp)) |>
  mutate(west = ifelse(group == "West", "West", "Developing")) |>
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(year ~ west)


# notamos que hay más países representados en los histogramas de 2010 que en 1970.
# Una razón para esto es que varios países
# se fundaron después de 1970. 
  

# Rehacemos los gráficos utilizando solo países con datos disponibles para ambos años.

country_list_1 <- gapminder |>
  filter(year == past_year & !is.na(dollars_per_day)) |>
  pull(country)

country_list_2 <- gapminder |>
  filter(year == present_year & !is.na(dollars_per_day)) |>
  pull(country)

country_list <- intersect(country_list_1, country_list_2)



# Estos 108 constituyen 86% de la población mundial,
# por lo que este subconjunto debe ser representativo.
# Vamos a rehacer el gráfico, pero solo para este subconjunto simplemente agregando
# country %in% country_list a la función filter


past_year <- 1970
present_year <- 2010
years <- c(past_year, present_year)
gapminder |>
  filter(year %in% years & country %in% country_list & !is.na(gdp)) |>
  mutate(west = ifelse(group == "West", "West", "Developing")) |>
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(year ~ west)


# vemos que la proporción de países en desarrollo que ganan más de $16 por 
# día aumentó sustancialmente.



# Para ver qué regiones específicas mejoraron más, podemos rehacer los diagramas de caja 



gapminder |>
  filter(year %in% years & country %in% country_list) |>
  ggplot(aes(group, dollars_per_day)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(trans = "log2") +
  xlab("") +
  facet_grid(. ~ year)


# Como queremos comparar cada región antes y después, sería conveniente tener el diagrama de 
# caja de 1970 al lado del de 2010 para cada región.

# Como el año es un número, lo convertimos en un factor ya que ggplot2 asigna automáticamente 
# un color a cada categoría de un factor. Recuerden que tenemos que convertir la columnas year
# de numérica a factor.
gapminder |>
  filter(year %in% years & country %in% country_list) |>
  mutate(year = factor(year)) |>
  ggplot(aes(group, dollars_per_day, fill = year)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(trans = "log2") +
  xlab("")



# La exploración de datos previa sugiere que la brecha de ingresos entre países ricos y pobres 
# se ha reducido considerablemente durante los últimos 40 años. Debemos transmitir este mensaje 
# con solo un gráfico.


# Observemos que los gráficos de densidad para la distribución del ingreso en 1970 y 2010 
# transmiten el mensaje

gapminder |>
  filter(year %in% years & country %in% country_list) |>
  ggplot(aes(dollars_per_day)) +
  geom_density(fill = "grey") +
  scale_x_continuous(trans = "log2") +
  facet_grid(. ~ year)




# En el gráfico de 1970, vemos dos modas claras: países pobres y ricos. En 2010, parece que 
# algunos de los países pobres se han desplazado hacia la derecha, cerrando la brecha.

# El próximo mensaje que debemos transmitir es que varios países pobres se hicieron más ricos, 
# en lugar de que algunos países ricos se hicieron más pobres. 
# Para hacer esto, podemos asignar un color a los grupos que identificamos durante la exploración 
# de datos: Developing (87) y West (21).


# Hacemos densidades suaves de una manera que conserve la información sobre el número
# de países en cada grupo.

gapminder |>
  filter(year %in% years & country %in% country_list) |>
  mutate(group = ifelse(group == "West", "West", "Developing")) |>
  ggplot(aes(dollars_per_day, fill = group)) +
  scale_x_continuous(trans = "log2") +
  geom_density(alpha = 0.2) +
  facet_grid(year ~ .)


# En el gráfico, parece que hay el mismo número de países en cada grupo.
# Para cambiar esto, necesitaremos aprender a acceder a las variables calculadas
# con la función geom_density
# Para que las áreas de estas densidades sean proporcionales al tamaño de los grupos, 
# simplemente multiplicamos los valores del eje-y por el tamaño del grupo.

p <- gapminder |>
  filter(year %in% years & country %in% country_list) |>
  mutate(group = ifelse(group == "West", "West", "Developing")) |>
  ggplot(aes(dollars_per_day, y = ..count.., fill = group)) +
  scale_x_continuous(trans = "log2", limit = c(0.125, 300))

p + geom_density(alpha = 0.2) +
  facet_grid(year ~ .)




# Si queremos que las densidades sean más suaves, usamos el argumento bw
# para que se use el mismo parámetro de suavizado en cada densidad.



p + geom_density(alpha = 0.2, bw = 0.75) + facet_grid(year ~ .)



 
# La distribución del mundo en desarrollo está cambiando.
# Para visualizar si alguno de los grupos definidos anteriormente 
# son la causa principal de estos cambios, rápidamente podemos hacer un gráfico ridge:



gapminder |>
  filter(year %in% years & !is.na(dollars_per_day)) |>
  ggplot(aes(dollars_per_day, group)) +
  scale_x_continuous(trans = "log2") +
  geom_density_ridges(adjust = 1.5) +
  facet_grid(. ~ year)





# Otra forma de lograr esto es apilando las densidades una encima de otra:


gapminder |>
  filter(year %in% years & country %in% country_list) |>
  group_by(year) |>
  mutate(weight = population/sum(population)*2) |>
  ungroup() |>
  ggplot(aes(dollars_per_day, fill = group)) +
  scale_x_continuous(trans = "log2", limit = c(0.125, 300)) +
  geom_density(alpha = 0.2, bw = 0.75, position = "stack") +
  facet_grid(year ~ .)


# Aquí podemos ver claramente cómo las distribuciones para Asia Oriental, América Latina
# y otros se desplazan notablemente hacia la derecha. Mientras que África
# subsahariana permanece estancada.




###########  TAREA 4




dat <- read_csv("nat_mort_mex.csv")
dat


View(dat)

dat %>%
  ggplot(aes(Indice_fecundidad, Esperanza_de_vida)) +
  geom_point()



dat |>
  ggplot(aes(Fecha, Tasa_natalidad)) +
  geom_point()

dat |>
  ggplot(aes(Fecha, Tasa_mortalidad )) +
  geom_line()




alturas <- read_csv("alturas.csv")
alturas


alturas |>
  ggplot(aes(Altura_media_mujeres)) +
  geom_histogram( fill = "blue", col = "black") +
  xlab("hom") +
  ggtitle("Histogram")



alturas |>
  ggplot(aes(Altura_media_mujeres)) +
  geom_density(fill="blue")


alturas |> 
  ggplot(aes(sample = Altura_media_hombres)) +
  geom_qq()


x <- alturas$Altura_media_hombres
   
m <- mean(x)
s <- sd(x)
c(average = m, sd = s)

z <- scale(x)

z


mean(abs(z) < 2)

p <- seq(0.05, 0.95, 0.05)

p
sample_quantiles <- quantile(x, p)
theoretical_quantiles <- qnorm(p, mean = mean(x), sd = sd(x))
qplot(theoretical_quantiles, sample_quantiles) + geom_abline()

alturas |> 
  ggplot(aes(sample = Altura_media_hombres)) +
  geom_qq()




params <- alturas |> 
  summarize(mean = mean(Altura_media_hombres), sd = sd(Altura_media_hombres))

alturas |> 
  ggplot(aes(sample = Altura_media_hombres)) +
  geom_qq(dparams = params) +
  geom_abline()








natalidad <- read_csv("natalidad_2015.csv")
natalidad











