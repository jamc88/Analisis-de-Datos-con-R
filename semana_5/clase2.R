#################################################
# Visualización de datos en la práctica
#################################################

library(tidyverse)
library(dslabs)
library(gridExtra)


data(murders)
murders |> mutate(murder_rate = total/ population * 100000) |>
  mutate(state = reorder(state, murder_rate)) |>
  ggplot(aes(state, murder_rate)) +
  geom_bar(stat="identity") +
  coord_flip() +
  theme(axis.text.y = element_text(size = 6)) +
  xlab("")





heights |>
  ggplot(aes(sex, height)) +
  geom_point()


# El gráfico anterior nos da una idea del rango de los datos. 
# Sin embargo, este gráfico también tiene limitaciones, ya que realmente no podemos 
# ver todos los 238 y 812 puntos graficados para mujeres y varones

# jitter añade un pequeño desplazamiento aleatorio a cada punto.
# alpha blending hace que los puntos sean algo transparentes.

heights |>
  ggplot(aes(sex, height)) +
  geom_jitter(width = 0.1, alpha = 0.2)

########################################################

# Si queremos obtener el resumen compacto que ofrecen los diagramas de caja,
# tenemos que alinearlos horizontalmente ya que, por defecto, los diagramas 
# de caja se mueven hacia arriba y hacia abajo según los cambios de altura.


heights |>
  ggplot(aes(sex, height)) +
  geom_boxplot(coef=3) +
  geom_jitter(width = 0.1, alpha = 0.2) +
  ylab("Height in inches")



########################################################


############# EJERCICIOS ###############



data(us_contagious_diseases)

us_contagious_diseases


# reordenamos los niveles de las variables de los estados.

dat <- us_contagious_diseases |>
  filter(year == 1967 & disease=="Measles" & !is.na(population)) |>
  mutate(rate = count/ population * 10000 * 52/ weeks_reporting) 

dat



dat |> ggplot(aes(state, rate)) +  
  geom_bar(stat="identity") +
  coord_flip()




#### ordenar



dat <- us_contagious_diseases |>
  filter(year == 1967 & disease=="Measles" & !is.na(population)) |>
  mutate(rate = count/ population * 10000 * 52/ weeks_reporting, range = rank(rate)) %>%
  arrange(range)

dat


dat |> ggplot(aes(x=reorder(state,rate), y=rate)) + 
  geom_bar(stat="identity") +
  coord_flip()






dat |> ggplot(aes(x=reorder(state,rate), y=rate,  fill= state)) +
  geom_bar(stat="identity") +
  theme(legend.position='none') +
  coord_flip()









########################################################################

library(tidyverse)
library(dslabs)
library(gridExtra)
library(RColorBrewer)

data(us_contagious_diseases)
names(us_contagious_diseases)


# Creamos un objeto dat que almacena solo los datos de sarampión, 
# incluye la tasa por 100,000, ordena a los estados según el valor promedio
# de enfermedad y elimina Alaska y Hawai ya que estos dos se convirtieron 
# en estados a fines de la década de 1950.


the_disease <- "Measles"
dat <- us_contagious_diseases |>
  filter(!state%in%c("Hawaii","Alaska") & disease == the_disease) |>
  mutate(rate = count/ population * 10000 * 52/ weeks_reporting) |>
  mutate(state = reorder(state, rate))


# Graficamos las tasas de enfermedad por año. 
# Aquí están los datos de sarampión de California:
# Añadimos una línea vertical en 1963, 
# ya que es cuando se introdujo la vacuna

dat |> filter(state == "California" & !is.na(rate)) |>
  ggplot(aes(year, rate)) +
  geom_line() +
  ylab("Cases per 10,000") +
  geom_vline(xintercept=1963, col = "blue")



# ¿Ahora podemos mostrar datos para todos los estados en un gráfico? 
#  Tenemos tres variables para incluir: año, estado y tasa.




# Usamos la geometría geom_tile para tejar la región con colores que representan las tasas de enfermedad. 
# Usamos una transformación de raíz cuadrada para evitar que los conteos particularmente altos dominen el gráfico. 



dat |> ggplot(aes(year, state, fill = rate)) +
  geom_tile(color = "grey50") +
  scale_x_continuous(expand=c(0,0)) +
  scale_fill_gradientn(colors = brewer.pal(9, "Reds"), trans = "sqrt") +
  geom_vline(xintercept=1963, col = "blue") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position="bottom",
        text = element_text(size = 8)) +
  ggtitle(the_disease) +
  ylab("") + xlab("")



# Observemos que los valores faltantes se muestran en gris. Además, noten que tan pronto una enfermedad
# fue prácticamente erradicada, algunos estados dejaron de informar casos por completo.
# Es por esa razón que vemos tanto gris después de 1980.


# Este gráfico ofrece evidencia preponderante a favor de la contribución de las vacunas. 
# Sin embargo, una limitación es que usa el color para representar la cantidad, que, como explicamos anteriormente, 
# dificulta saber exactamente cuán altos llegan los valores. 

# podemos mostrar el promedio de EE. UU

avg <- us_contagious_diseases |>
  filter(disease==the_disease) |> group_by(year) |>
  summarize(us_rate = sum(count, na.rm = TRUE)/
              sum(population, na.rm = TRUE) * 10000)


avg


# Para hacer el gráfico simplemente usamos la geometría geom_line:

dat |>
  filter(!is.na(rate)) |>
  ggplot() +
  geom_line(aes(year, rate, group = state), color = "grey50",
            show.legend = FALSE, alpha = 0.2, size = 1) +
  geom_line(mapping = aes(year, us_rate), data = avg, size = 1) +
  scale_y_continuous(trans = "sqrt", breaks = c(5, 25, 125, 300)) +
  ggtitle("Cases per 10,000 by state") +
  xlab("") + ylab("") +
  geom_text(data = data.frame(x = 1955, y = 50),
            mapping = aes(x, y, label="US average"),
            color="black") +
  geom_vline(xintercept=1963, col = "blue")










