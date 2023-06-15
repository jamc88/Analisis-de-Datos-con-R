
# ¿Un jugador con un alto total de SB ayuda a producir carreras? 
# ¿Podemos usar el análisis de datos para determinar si es mejor pagar por
# jugadores con totales altos de BB o de SB?


# ¿Los equipos que tienen más cuadrangulares anotan más carreras? 
# Examinamos los datos de 1961 a 2001. La visualización de las opciones 
# al explorar la relación entre dos variables, como HR y triunfos, 
# es un diagrama de dispersión:

library(Lahman)

#install.packages("Lahman")

Teams |> filter(yearID %in% 1961:2001) |>
  mutate(HR_per_game = HR/ G, R_per_game = R/ G) |>
  ggplot(aes(HR_per_game, R_per_game)) +
  geom_point(alpha = 0.5)






# El gráfico muestra una fuerte asociación: los equipos con más HR tienden 
# a anotar más carreras. 


# Ahora examinemos la relación entre bases robadas y carreras:


Teams |> filter(yearID %in% 1961:2001) |>
  mutate(SB_per_game = SB/ G, R_per_game = R/ G) |>
  ggplot(aes(SB_per_game, R_per_game)) +
  geom_point(alpha = 0.5)

# Aquí la relación no es tan clara. Finalmente, examinemos la relación entre BB y carreras:
Teams |> filter(yearID %in% 1961:2001) |>
  mutate(BB_per_game = BB/G, R_per_game = R/G) |>
  ggplot(aes(BB_per_game, R_per_game)) +
  geom_point(alpha = 0.5)

# Aquí nuevamente vemos una asociación clara. Pero, 
# ¿esto significa que aumentar las BB de un equipo causa un aumento en las carreras? 
# Una de las lecciones más importantes que aprenderemos en este libro es que
# la asociación no implica causalidad.
# De hecho, parece que los BB y HR también están asociados:
  
Teams |> filter(yearID %in% 1961:2001 ) |>
  mutate(HR_per_game = HR/G, BB_per_game = BB/G) |>
  ggplot(aes(HR_per_game, BB_per_game)) +
  geom_point(alpha = 0.5)
  



# Sabemos que los HR causan carreras porque, como su nombre sugiere, 
# cuando un jugador logra un “home run”, se le garantiza al menos una carrera. 
# ¿Podría ser que los HR también causen BB y esto hace que parezca que los BB 
# también causen carreras? Cuando esto sucede, decimos que hay "CONFUSIÓN"

# La regresión lineal nos ayudará a analizar todo esto y cuantificar las asociaciones
# para determinar qué jugadores reclutar. 
# Específicamente, trataremos de predecir cosas como cuántas carreras más anotará 
# un equipo si aumentamos el número de BB, pero mantenemos los HR fijos. 
# La regresión nos ayudará a responder preguntas como esta.







###################################################
#  Regresión aplicada a las estadísticas de béisbol
###################################################


# observen que los datos de HR y carreras parecen seguir una distribución normal de dos
# variables. Guardamos el gráfico en el objeto p



library(Lahman)

p <- Teams |> filter(yearID %in% 1961:2001 ) |>
  mutate(HR_per_game = HR/G, R_per_game = R/G) |>
  ggplot(aes(HR_per_game, R_per_game)) +
  geom_point(alpha = 0.5)
p




# Los gráficos Q-Q confirman que la aproximación normal es útil aquí:


Teams |> filter(yearID %in% 1961:2001 ) |>
  mutate(z_HR = round((HR - mean(HR))/sd(HR)),
         R_per_game = R/G) |>
  filter(z_HR %in% -2:3) |>
  ggplot() +
  stat_qq(aes(sample=R_per_game)) +
  facet_wrap(~z_HR)




# Usaremos la regresión lineal para predecir el número de carreras que anotará 
# un equipo si sabemos cuántos cuadrangulares logrará el equipo. 
# Lo único que necesitamos hacer es calcular los resúmenes estadísticos:


summary_stats <- Teams |>
  filter(yearID %in% 1961:2001 ) |>
  mutate(HR_per_game = HR/G, R_per_game = R/G) |>
  summarize(avg_HR = mean(HR_per_game),
            s_HR = sd(HR_per_game),
            avg_R = mean(R_per_game),
            s_R = sd(R_per_game),
            r = cor(HR_per_game, R_per_game))
summary_stats






# creamos las líneas de regresión:

reg_line <- summary_stats |> summarize(slope = r*s_R/s_HR,
                                       intercept = avg_R - slope*avg_HR)

p + geom_abline(intercept = reg_line$intercept, slope = reg_line$slope)




# La función geom_smooth de ggplot2 que calcula y agrega una línea de regresión
# junto con intervalos de confianza al gráfico. Usamos el argumento method = "lm" 
# que significa modelo lineal.
# Entonces podemos simplificar el código anterior así:

p + geom_smooth(method = "lm")


# En el ejemplo anterior, la pendiente es 1.845.
# Esto nos dice que los equipos que logran 1 HR más por juego que el equipo promedio, 
# anotan más carreras por juego que el equipo promedio. 
# Dado que la puntuación final más común es la diferencia de una carrera, 
# esto ciertamente puede conducir a un gran aumento en victorias. 










##################################
# Confusión
##################################

# Si encontramos la línea de regresión para predecir carreras desde bases por bolas, 
# obtendremos una pendiente de:

library(tidyverse)

get_slope <- function(x, y) cor(x, y) * sd(y)/ sd(x)

bb_slope <- Teams |>
  filter(yearID %in% 1961:2001 ) |>
  mutate(BB_per_game = BB/G, R_per_game = R/G) |>
  summarize(slope = get_slope(BB_per_game, R_per_game))

bb_slope

# Entonces, ¿esto significa que si contratamos jugadores de bajo salario con muchos BB y 
# así aumentamos por 2 el número de BB por juego, nuestro equipo marcará 1.5 más carreras 
# por juego?
  
# Nuevamente debemos recordar que la asociación no implica la causalidad. 
# Los datos ofrecen evidencia sólida de que un equipo con dos BB más por juego que el equipo
# promedio, anota 1.5 carreras por juego. Pero esto no significa que los BB sean la causa.

# Noten que si calculamos la pendiente de la línea de regresión para sencillos obtenemos:

singles_slope <- Teams |>
  filter(yearID %in% 1961:2001 ) |>
  mutate(Singles_per_game = (H-HR-X2B-X3B)/G, R_per_game = R/G) |>
  summarize(slope = get_slope(Singles_per_game, R_per_game))

singles_slope

# que es un valor más bajo que el que obtenemos para BB.
# Además, observen que un sencillo lleva a un jugador a primera base igual que un BB. 
# Con un sencillo, los corredores en base 
# tienen una mejor oportunidad de anotar que con un BB. 
# Entonces, ¿cómo puede un BB ser más predictivo de las carreras? 
# La razón por la que esto sucede es por confusión. 
# Aquí mostramos la correlación entre HR, BB y sencillos:

Teams |>
  filter(yearID %in% 1961:2001 ) |>
  mutate(Singles = (H-HR-X2B-X3B)/G, BB = BB/G, HR = HR/G) |>
  summarize(cor(BB, HR), cor(Singles, HR), cor(BB, Singles))


# Resulta que los lanzadores, temerosos de los HR, a veces evitarán lanzar strikes 
# a los bateadores de HR. Como resultado, los bateadores de HR tienden a tener más BB 
# y un equipo con muchos bateadores de HR también tendrá más BB. 
# Aunque puede parecer que BB causan carreras, realmente son HR los que causan
# la mayoría de estas carreras. Decimos que BB están "confundidos" con HR. 
# Sin embargo, ¿es posible que las BB todavía ayuden? Para averiguar, tenemos que 
# ajustar para el efecto de HR. La regresión también puede ayudar con esto.





########################################################
# confusión a través de la estratificación
########################################################


# Un primer acercamiento es mantener los HR fijos a un valor determinado y 
# luego examinar la relación entre BB y las carreras. 
# Podemos estratificar HR por juego a los diez más cercanos.
# Filtramos los estratos con pocos puntos para evitar estimadores muy variables:


dat <- Teams |> filter(yearID %in% 1961:2001) |>
  mutate(HR_strata = round(HR/G, 1),
         BB_per_game = BB/ G,
         R_per_game = R/ G) |>
  filter(HR_strata >= 0.4 & HR_strata <=1.2)



# y hacemos un diagrama de dispersión para cada estrato:


dat |>
  ggplot(aes(BB_per_game, R_per_game)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ HR_strata)


# Recuerden que la pendiente de regresión para predecir carreras con BB era 0.7. 
# Una vez que estratificamos por HR, estas pendientes se reducen sustancialmente:

dat |>
  group_by(HR_strata) |>
  summarize(slope = get_slope(BB_per_game, R_per_game))


# Las pendientes se reducen, pero no son 0, lo que indica que las BB son útiles 
# para producir carreras, pero no tanto como se pensaba anteriormente. 
# De hecho, los valores anteriores están más cerca de la pendiente que obtuvimos 
# de sencillos, 0.45, que es más consistente con nuestra intuición. 
# Dado que tanto los sencillos como los BB nos llevan a primera base, 
# deberían tener aproximadamente el mismo poder predictivo.



