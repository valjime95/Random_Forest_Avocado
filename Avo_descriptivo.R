library(dplyr)
library(ggplot2)
setwd("/Users/valeriajimeno/Downloads/")
mercados <- read.csv("avo_mercados.csv")
avocado <- read.csv("avocado.csv")
# ** Analisis descriptivo ** #

##ver si tiene datos faltantes##
table(is.na(mercados))

##numero de observaciones##
nrow(mercados)

summary(mercados)
attach(mercados)

## Boxplot de las variables ##

boxplot(AveragePrice, col = "aliceblue",main= "Average Price", ylim=c(0.3,3.5) )
boxplot(Total.Volume,main= "Precio promedio")
boxplot(X4046,main= "Aguacate hassel pequeño")
boxplot(X4225,main= "Aguacate hassel GRANDE")
boxplot(X4770,main= "Aguacate hassel X-GRANDE")

boxplot(Total.Bags,main= "Número total de bolsas de aguacates")
boxplot(Small.Bags,main= "Número de bolsas de aguacates pequeños")
boxplot(Large.Bags,main= "Número de bolsas de aguacates grandes")

##.....  .....###

ggplot(mercados) +
  geom_point(aes(AveragePrice, region))

sanfrancisco <- mercados %>% filter(region=="SanFrancisco")

View(max(sanfrancisco$AveragePrice)) #3.25


sanfrancisco_17 <-mercados %>% filter(region=="SanFrancisco",year==2017)
sanf_org_3.25 <-sanfrancisco_17 %>% filter(type=="organic",AveragePrice==3.25)
sanf_conv <-sanfrancisco_17 %>% filter(type=="conventional")

View(sanf_org_3)
View(sanfrancisco_17)
summary(sanf_org)

ggplot(sanf_org) +
  geom_point(aes(AveragePrice, Total.Volume)) 


#volumen

mayor_precio <- avocado %>% filter(AveragePrice== 3.25)
mayor_produccion_mercados <- avo_mercados %>% filter(Total.Volume == max(Total.Volume))

View(mayor_produccion_mercados)


#····· Vamos a quitar las variables categoricas para seguir con nuestro analisis·····##

mer_catego=mercados[,c(3:10)]

##......Correlacion entre las variables.....###
corr_mercados<-cor(mer_catego, method = "pearson")
corrplot(corr_mercados, tl.srt = 45,tl.col = "black")

##filtramos la base para que sacar el volumen total y el precio promedio de cada año ##

##primero para todo estados unidos ##

avo <- avocado %>% filter(region == "TotalUS")
avo_organic <- avo %>% filter(type == "organic")
avo_conventional <- avo %>% filter(type == "conventional")
x <- avo_organic$AveragePrice
summary(x)

y <- avo_conventional$AveragePrice
summary(y)


##vemos el volumen total de ventas y el precio promedio por cada año en todo estados unidos##

#####BASES POR AÑO####
avo_15 <- avocado %>% filter(year== 2015) 
avo_16 <- avocado %>% filter(year== 2016) 
avo_17 <- avocado %>% filter(year== 2017) 
avo_18 <- avocado %>% filter(year== 2018) 


######################### Análisis por tipo y año ############## 
colnames(avocado)
unique(avocado$type)
#notemos que exiten dos tipo de aguacate, el orgánico y el convencional 
#analizando por año 
avo_15 %>% group_by(type) %>% summarise(conteo = n())
avo_16 %>% group_by(type) %>% summarise(conteo = n())
avo_17 %>% group_by(type) %>% summarise(conteo = n())
avo_18 %>% group_by(type) %>% summarise(conteo = n())


##2015##
Volumen_total_2015 <-avo_15$Total.Volume
Precio_promedio_2015 <- avo_15$AveragePrice
summary(Volumen_total_2015 )
summary(Precio_promedio_2015)

##2016##
Volumen_total_2016  <- avo_16$Total.Volume
Precio_promedio_2016 <- avo_16$AveragePrice
summary(Volumen_total_2016)
summary(Precio_promedio_2016)

##2017##
Volumen_total_2017 <- avo_17$Total.Volume
Precio_promedio_2017 <- avo_17$AveragePrice
summary(Volumen_total_2017)
summary(Precio_promedio_2017)

##Por otro lado podemos ver el cambio en los precios promedio de aguacate en diferentes regiones de los estados unidos##

precio_avo_region <- aggregate(avocado[,3], list(avocado$year, avocado$region), mean)
precio_avo_region <- precio_avo_region %>% rename(Año = Group.1) %>% rename(Region = Group.2) %>% rename(Precio_prom = x)

View(precio_avo_region)
View(avocado)
## ahara lo veremos el cambio en el volumen total de agucate en las diferentes regiones##

vol_avo_region <- aggregate(avocado[,4], list(avocado$year, avocado$region), mean)
vol_avo_region <- vol_avo_region %>% rename(Año = Group.1) %>% rename(Region = Group.2) %>% rename(Vol_total = x)

View(vol_avo_region)

#Nos agarramos 3 regiones, Atlanta, NY y California en el 2017##
colnames(avocado)
mean(avocado$AveragePrice)
mean(avocado$Total.Bags)
unique(avocado$region)

#Atlanta 2017
atlanta<-avocado %>% filter(region=="Atlanta",year==2017)
mean(atlanta$AveragePrice)
max(atlanta$AveragePrice)
min(atlanta$AveragePrice)
max(atlanta$Total.Bags)
#New York  2017
ny<-avocado %>% filter(region=="NewYork",year==2017)
mean(ny$AveragePrice)
max(ny$AveragePrice)
min(ny$AveragePrice)
max(ny$Total.Bags)
#California 2017
calif<-avocado %>% filter(region=="California",year==2017)
mean(calif$AveragePrice)
max(calif$AveragePrice)
min(calif$AveragePrice)
max(calif$Total.Bags)


##Graficos del precio promedio por cada año##
plot(avo_15$AveragePrice, col="red", main = "Precio promedio del aguacate en 2015", ylab = "Precio en USD") 
plot(avo_16$AveragePrice, col="blue", main = "Precio promedio del aguacate en 2016", ylab = "Precio en USD")
plot(avo_17$AveragePrice, col="pink", main = "Precio promedio del aguacate en 2017", ylab = "Precio en USD")
plot(avo_18$AveragePrice, col="green", main = "Precio promedio del aguacate en 2018", ylab = "Precio en USD")


prueba <- read.csv("volumen.csv")
library(timeDate)
library(timeSeries)
library(lubridate)
View(prueba)

time<-ts(prueba, 
   freq=365.25/7, 
   start=decimal_date(ymd("2015-01-04")))
fechas = seq(as.Date("2015/01/04"), length.out = length(time), by = "weeks")

ts.plot(time, gpars = list(xlab="Años", ylab="Volumen de ventas de aguacate")) 

de <- decompose(time, type = "additive")
plot(de)