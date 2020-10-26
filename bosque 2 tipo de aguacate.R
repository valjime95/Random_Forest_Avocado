## Bosque 2 ##
# ***Target: Type*** #

library(randomForest)
dim(mercados)

# Tomamos una muestra del 70% de los datos observados #
nobs <- nrow(mercados)
train <- sample(nobs, 0.7*nobs)

str(mercados)
#date tiene 169 niveles, es por eso que la quitaremos de la base
mer_sin_fecha <- subset(mercados, select = -2)
#modelo#
head(mer_sin_fecha)

bosque2 <- randomForest(type~ ., data= mer_sin_fecha, subset=train, ntree= 500, mtry= , importance = TRUE)
#---------------------------OOB-------------------------------------------#
#OOB: Tasa de error, estimada por las observaciones que no estan en la muestra #
#OOB estimate of  error rate: 0.05%
# esto nos dice que cuando el modelo es aplicado a nuevas observaciones, la respuesta tendra
#un error del 0.05%, esto quiere decir que el modelo es 99.95% preciso, por lo que podemos decir que
#es un buen  modelo.
#---------------------------Confusion matrix-------------------------------------------#
#En ella podemos ver la matriz de falsos positivos y falsos negativos
#las columnas representan las predicciones del modelo, mientras que los filas representan las observaciones.
# >Confusion matrix:
# >              conventional organic  class.error
# >conventional         5301       2 0.0003771450
# >organic                 3    5338 0.0005616926
#Esto nos dice el que modelo y los datos observados concuerdan que se venderan 5301
#aguacates convencionales y concuerdan que se venderan 5338 aguacates organicos.
#El modelo esta mal en predecir que va a

str(bosque)
#---------------------------Predicted-------------------------------------------#
#predicted: El componente predicted contiene los valores estimados para cada
#observación en el conjunto de datos "train" basado en las muestras fuera de bolsa.
#en nuestro caso a cada observacion se le asigno uno de los 45 mercados disponibles.
#En el summary podemos ver el numero de observaciones estimadas en cada mercado.
head (bosque$predicted, 54)
summary(bosque$predicted)
## ---------------------------------------------------------------------------##
# nos dice que para la observacion 13426 el valor predicho es Louisville
#13426         5843        10786         7518         9703         9607 
#Louisville   Louisville      Houston SanFrancisco    Nashville      NewYork 