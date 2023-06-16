
library(dslabs)
library(lubridate)
library(caret)
library(tidyverse)


contam <- read_csv("https://raw.githubusercontent.com/jamc88/Analisis-de-datos/main/AD_R_UAM/PM25.csv",col_names = FALSE)

contam


# 'Fecha','Noroeste_PM25','Noreste_PM25','Centro_PM25','Suroeste_PM25','Sureste_PM25'





## colocar columna de nombres

colnames(contam) <- c('Fecha','Noroeste_PM25','Noreste_PM25','Centro_PM25','Suroeste_PM25','Sureste_PM25')

contam





## contar NA en cada columna

sapply(contam, function(x) sum(is.na(x)))






## reemplazar valores nulos en cada columna con la mediana 

contam$Noroeste_PM25[is.na(contam$Noroeste_PM25)] <- median(contam$Noroeste_PM25,na.rm = TRUE)

contam$Noreste_PM25[is.na(contam$Noreste_PM25)] <- median(contam$Noreste_PM25,na.rm = TRUE)

contam$Centro_PM25[is.na(contam$Centro_PM25)] <- median(contam$Centro_PM25,na.rm = TRUE)

contam$Suroeste_PM25[is.na(contam$Suroeste_PM25)] <- median(contam$Suroeste_PM25,na.rm = TRUE)

contam$Sureste_PM25[is.na(contam$Sureste_PM25)] <- median(contam$Sureste_PM25,na.rm = TRUE)






## verificamos que ya no existan NA's

sapply(contam, function(x) sum(is.na(x)))














boxplot(contam$Noroeste_PM25, contam$Noreste_PM25, contam$Centro_PM25, contam$Suroeste_PM25, contam$Sureste_PM25,
        names = c("Noroeste_PM25","Noreste_PM25",'Centro_PM25','Suroeste_PM25','Sureste_PM25'),
        horizontal = TRUE, main="PM 2.5 por región", col=c("orange","orange","orange","yellow","red"))





# creamos una columna con el promedio de cada entrada

contam$prom <- rowMeans(contam[,2:6], na.rm = TRUE)


# creamos tibble para la serie temporal

PM25 <- tibble(Fecha = contam$Fecha,
             pm_25 = contam$prom)










# gráfica de serie temporal y diagrama de dispersión

PM25 |> ggplot(aes(Fecha, pm_25)) +
  geom_line(size=0.5,color="blue") +
  geom_point() +
  ggtitle("PM 2.5 durante 2022") +
  ylab("PM 2.5")+
  xlab("Fecha") +
  theme(title=element_text(size=18,face="bold"))



qplot(Fecha, pm_25, data = PM25) +
  ggtitle("PM 2.5 durante 2022") +
  ylab("PM 2.5")+
  xlab("Fecha") +
  theme(title=element_text(size=18,face="bold"))






# Línea de regresión

mu_x <- mean(1:365)
mu_y <- mean(PM25$pm_25)
s_x <- sd(1:365)
s_y <- sd(PM25$pm_25)
r <- cor(1:365, PM25$pm_25)

PM25 |>
  ggplot(aes(1:365, pm_25)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = r * s_y/s_x, intercept = mu_y - r * s_y/s_x * mu_x, color="red")


# suavización 
span <- 7
fit <- with(PM25,
            ksmooth(Fecha, pm_25, kernel = "box", bandwidth = span))

PM25 |> mutate(smooth = fit$y) |>
  ggplot(aes(Fecha, pm_25)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(Fecha, smooth), color="red")


span <- 7
fit <- with(PM25,
            ksmooth(Fecha, pm_25, kernel = "normal", bandwidth = span))

PM25 |> mutate(smooth = fit$y) |>
  ggplot(aes(Fecha, pm_25)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(size=1, aes(Fecha, smooth), color="red") +
  ylab("PM 2.5")+
  xlab("Fecha") +
  theme(title=element_text(size=18,face="bold"))











#####################################################################################

library(caret)
library(tidyverse)
library(dslabs)



data("iris")
dataset <- na.omit(iris)


dataset[,-5] <- scale(dataset[,-5])



summary(dataset[,-5])



# Dividimos el conjunto de datos en entrenamiento y prueba

validationIndex <- createDataPartition(dataset$Species, p=0.70, list=FALSE)

train <- dataset[validationIndex,] # 70% de los datos para entrenamiento
test <- dataset[-validationIndex,] # 30% restante para la prueba
# Ejecutamos algoritmos usando validación cruzada de 10 veces

trainControl <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "Accuracy"

set.seed(7)
fit.knn <- train(Species~., data=train, method="knn",
                 metric=metric ,trControl=trainControl)
knn.k1 <- fit.knn$bestTune 
print(fit.knn)

plot(fit.knn)



# Este gráfico muestra el Codo k = 9 con una precisión del 96,07 % para el conjunto de datos de entrenamiento

# Ejecute la predicción con el conjunto de datos de prueba e imprima la matriz de confusión:


set.seed(7)
prediction <- predict(fit.knn, newdata = test)
cf <- confusionMatrix(prediction, test$Species)
print(cf)


# Con k inicial = 9, el modelo predice correctamente el 97,78 % de la variable objetivo 
# en el conjunto de datos de prueba


#######################################################################################





























