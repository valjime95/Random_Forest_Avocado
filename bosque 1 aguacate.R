install.packages("RColorBrewer")
library(RColorBrewer)
library(dplyr)
library(ggplot2)
library(corrplot)
setwd("~/Desktop/AGUACATE")
mercados <- read.csv("~/Desktop/AGUACATE/avo_mercados.csv")
head(mercados)
rattle()
## Bosques ##
# ***Target: REGION*** #
set.seed(101)
library(randomForest)
dim(mercados)

# Tomamos una muestra del 70% de los datos observados #
nobs <- nrow(mercados)
train <- sample(nobs, 0.7*nobs)
 
str(mercados)
                     
#modelo

bosque <- randomForest(region~ ., data= mercados, subset=train, ntree= 500, mtry= 2, importance = TRUE)
#---------------------------OOB-------------------------------------------#
#OOB: Tasa de error, estimada por las observaciones que no estan en la muestra #
#OOB estimate of  error rate: 15.58%
# esto nos dice que cuando el modelo resultado es aplicado a nuevas observaciones, la respuesta tendra
#una respuesta del 15.58%, esto quiere decir que el modelo es 89.34% preciso, por lo que podemos decir que
#es un buen  modelo.
#---------------------------Confusion matrix-------------------------------------------#
#En ella podemos ver la matriz de falsos positivos y falsos negativos
#las columnas representan las predicciones del modelo, mientras que los filas representan las observaciones.
#Por ejemplo de nuestra primera columna

str(bosque)
#---------------------------Predicted-------------------------------------------#
#predicted: El componente predicted contiene los valores estimados para cada
#observación en el conjunto de datos "train" basado en las muestras fuera de bolsa.
#en nuestro caso a cada observacion se le asigno uno de los 45 mercados disponibles.
#En el summary podemos ver el numero de observaciones estimadas en cada mercado.
head (bosque$predicted, 10)
summary(bosque$predicted)
## ---------------------------------------------------------------------------##
# nos dice que para la observacion 13426 el valor predicho es Louisville
#13426         5843        10786         7518         9703         9607 
#Louisville   Louisville      Houston SanFrancisco    Nashville      NewYork 

#---------------------------Importance-------------------------------------------#

head()
## Cross Validation ###
install.packages("glmnet")
require(glmnet)
library(rattle)
rattle()
