####################### 
# Importando datos
######################

library(dslabs)
library(tidyverse)




# El paquete dslabs incluye una hoja de cálculo que contiene 
# los datos de los asesinatos de EE. UU. 




# las siguientes líneas de código copian el archivo a la carpeta 
# en la que R busca por defecto. Este código no lee los datos en R, 
# solo copia un archivo.



filename <- "murders.csv"
dir <- system.file("extdata", package = "dslabs")
fullpath <- file.path(dir, filename)
file.copy(fullpath, "murders.csv")









# una vez copiado el archivo, podemos importar los datos con
# solo una línea de código. Aquí usamos la función read_csv 
# del paquete readr, que forma parte del tidyverse.


dat <- read_csv(filename)

dat







ase21 <- read_csv("INEGI_2021.csv")
ase21







sum(ase21$Total)






ase21 %>% 
  summarize(max = max(Mujeres, na.rm=TRUE), min = min(Mujeres, na.rm=TRUE))




i_max <- which.max(ase21$Mujeres)
ase21$Estado[i_max]








############################################################



system.file(package = "dslabs")



# Las cadenas separadas por barras son los nombres de los directorios. 
# La primera barra diagonal representa el directorio raíz 




# Si el primer nombre del directorio aparece sin una barra diagonal en el comienzo, 
# entonces R supone que la ruta es relativa





# Podemos usar la función list.files para ver ejemplos de rutas relativas:

dir <- system.file(package = "dslabs")
list.files(path = dir)

# Estas rutas relativas nos dan la localización de los archivos o directorios si
# comenzamos en el directorio con la ruta completa.
# Por ejemplo, la ruta completa al directorio help en el ejemplo anterior es:
#  /Library/Frameworks/R.framework/Versions/3.5/Resources/library/dslabs/help.



# Pueden obtener la ruta completa de su directorio de trabajo sin escribirla explícitamente 
# utilizando la función getwd:

wd <- getwd()
wd





##### Cómo copiar los archivos usando rutas #####

# Ejemplo anterior:

file.copy(fullpath, "murders.csv")

# Si un archivo se copia exitosamente, la función file.copy devuelve TRUE. 


# Para poder ver el archivo en su directorio de trabajo usamos:


list.files()


####################################################################################

# Podemos abrir el archivo para echar un vistazo o
# usar la función read_lines para ver algunas líneas:
read_lines("murders.csv", n_max = 3)



# Esto también muestra que hay un encabezado. Ahora estamos listos para leer los datos 
# en R. 





# Del sufijo .csv y del vistazo al archivo, sabemos que tenemos que usar read_csv:

dat <- read_csv("murders.csv")



# Como read_csv es un lector del tidyverse,  dat es un tibble (no solo un data frame). 
# Podemos confirmar que los datos se han leído de la siguiente manera:


View(dat)
  
  



  
# También podemos usar la ruta completa para el archivo:  
  

  
dat <- read_csv(fullpath)

dat


###################################################################### 

# EJERCICIO

# Utilice la función read_csv ó read_excel  para leer dos de los archivos 
# que el siguiente código guarda en el objeto files:
  
  
path <- system.file("extdata", package = "dslabs")
files <- list.files(path)
files













########################### DESCARGA DE ARCHIVOS ##################################

# Cuando estos datos están en archivos, podemos descargarlos y luego importarlos, 
# o incluso leerlos directamente de la web.


url <- "https://raw.githubusercontent.com/jamc88/Analisis-de-datos/main/Categorizacion_variables/Corredores.csv"

dat <- read_csv(url)

dat







# Si quieremos tener una copia local del archivo, pueden usar la función download.file:
  
download.file(url, "corredores.csv")




















