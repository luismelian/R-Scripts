# Trabajo Modelos no supervisados: Analisis de los golpes de Estado
# Luis Melian Pena y Carla de Parias Mateos

# Librerias y datos: ------------------------------------------------------
library(dplyr)
library(corrgram)
library(ggplot2)
library(corrr)
library(tidyverse)
library(factoextra)
library(cluster)
library(dendextend)
library(ggpubr)


d2 <- read.table("Documents/UFV/3 UFV/Métodos Estadísticos/data/Coup_Data_v2.0.0-1.csv", sep = "," , header = TRUE)


# Ejercicio 1 -------------------------------------------------------------
# Realiza un estudio y analisis descriptivo del conjunto de datos. Apoyate 
# de los graficos que consideres necesarios
#Vemos de que tipo es nuestro conjunto de datos
class(d2)
#Sus dimensiones
dim(d2)
#Los tipos de las variables y valores de primeras observaciones
str(d2)
#Resumen de las variables (min, max, media, mediana, primer y tercer cuartil)
summary(d2)

#Vamos a convertir la columna coup_id en rowname y despues la eliminamos
row.names(d2) <- d2$coup_id
d2$coup_id <- NULL


#Vamos a ver cuantos paises distintos hay, y vamos a graficar aquellos en los que haya habido 15 golpes de estado o mas
unique(d2$country)
table(d2$country)
y = table((d2$country), exclude=c('Bulgaria', 'Cameroon','Guyana','Ivory Coast','Kyrgyz Republic', 
                                  'Montenegro','Senegal','Ukraine','South Sudan','Serbia', 'Malawi',
                                  'Kyrgyzstan','Jamaica','East Timor', 'Armenia', 'Djibouti', 'Mexico',
                                  'United Arab Emirates', 'Bahrain', 'Cyprus', 'Dominicana', 'Ecuador', 
                                  'Eritrea','Kenya', 'Luxembourg','Turkmenistan', 'United States of America', 
                                  'China','Costa Rica', 'Fiji','German Democratic Republic', 'Lebanon', 
                                  'Papua New Guinea','Poland', 'Sao Tome and Principe', 'Spain', 'Swaziland', 
                                  'Zambia', ' Afghanistan' ,'Albania', 'Angola','Azerbaijan', 'Colombia', 
                                  'Dominica', 'Gabon', 'Italy', 'Malaysia', 'Mongolia', 'Nepal','Oman', 
                                  'Rumania', 'Seychelles','Sri Lanka','Yemen','Zimbabwe','Equatorial Guinea',
                                  'Gambia', 'Jordan', 'Maldives', 'Morocco', 'Qatar', 'Rusia', 'Tajikistan',
                                  'Trinidad and Tobago','Uruguay','Chile','Ecuador','Georgia','Grenada',
                                  'Mozambique', 'Rwanda', 'Tanzania', 'Tunisia', 'USSR','Algeria','Bangladesh', 
                                  'Benin','Brazil','Burkina Faso', 'Cambodia', 'Central African Republic', 
                                  'Comoros', "Cote d'Ivoire",'Cuba','Czechoslovakia Democratic', 'Republic of the Congo',
                                  'Dominican Republic', 'Ecudaor','El Salvador', 'Ethiopia', 'Greece', 
                                  'Guinea', 'Honduras', 'Hungary', 'Indonesia', 'Iran', 'Lesotho', 'Libya', 
                                  'Madagascar', 'Mali', 'Myanmar', 'Nicaragua', 'Niger', 'Pakistan', 'Portugal',
                                  'Republic of Korea','Somalia', 'Russia', 'Republic of Vietnam', 'Surinam', 
                                  'Turkey', 'Yemen Arab Republic','Yemen PDR', 'Czechoslovakia', 
                                  'Democratic Republic of the Congo','Afghanistan', 'Chad' ,'Egypt', 'Ghana',
                                  'Guinea-Bissau', 'Paraguay'  , 'Peru','Philippines' ,'Uganda', 'Sierra Leone', 
                                  'Laos', 'Liberia' , 'Mauritania'))
y
barplot(y, main="Paises con 15 o mas golpes de estado", col = "orange")

#Eliminamos la columna "Country", ya que nos da la misma informacion 
#que "cowcode" (cada pais tiene un cowcode distinto)
d2$country <- NULL
#Creamos intervalos de los anos y graficamos
y2 <- cut(d2$year, breaks = 10)
y2 <- table(y2)
barplot(y2, col = rainbow(11), ylab="Numero de golpes de estado", xlab="Intervalos", main="Numero de golpes de estado por intervalos", ylim = c(0, 200))

#Eliminamos las columnas "year", "month" y "day" ya que no nos 
#aportan informacion relevante en nuestro estudio
d2$year <- NULL
d2$month <- NULL
d2$day <- NULL

#Estudiamos la relacion entre realized y unrealized y graficamos
x1 <- d2$realized
x2 <- d2$unrealized
tabla <- table(x1, x2)
tabla
par(mfrow = c(1, 2))
etiquetas <- c("Realized", "Unrealized") # vector con etiquetas
pie(tabla, cex = 1.1, col = 3:5, labels = etiquetas, main= "Golpes de estado realized Vs Golpes de estado unrealized")
#Eliminamos esta variable puesto que tenemos la variable realized que es justo la contraria
d2$unrealized <- NULL

#Vamos a asignar un valor en funcion del tipo de golpe de estado que estamos hablando,
#para asi transformar la columna "event_type" a int
#Primero vamos a ver que valores puede tomar esta variable:
unique(d2$event_type)
#Numero de consmiracy
sum(d2$conspiracy)
#Numero de attempt
sum(d2$attempt)
#Numero de coup
dim(d2)[1] - sum(d2$conspiracy) - sum(d2$attempt)
#Ahora vamos a asignar: 
# Si es "coup" -> 1
# Si es "attempted" -> 2
# Si es "conspiracy" -> 3
d2$event_type <- as.numeric(factor(d2$event_type,
                                   levels = c("coup", "attempted", "conspiracy")))
#Vamos a eliminar la columna "conspiracy" y "attempt" puesto que en "event_type" 
#ya hemos contemplado el caso en el que el golpe de estado sea de tipo "conspiracy" y de tipo "attempt"
d2$conspiracy <- NULL
d2$attempt <- NULL

#Tambien eliminamos el cowcode puesto que no lo necesitaremos
d2$cowcode <- NULL

#vemos como ha quedado nuestro conjunto de datos
view(d2)


#Calculamos varianza
y_1 = apply(d2, 2, var)
y_1

#Calculamos media de las columnas
y_2 = apply(d2, 2, mean)
y_2

#calculamos correlacion
cor <- correlate(d2)
View(cor)


# Ejercicio 2 -------------------------------------------------------------
# Calcula las componentes principales. Selecciona las componentes 
# necesarias, que identifican al menos un 75% de la varianza acumulada 
# explicativa. Explica cada una de las decisiones que tomes

#Ahora ya tenemos todas nuestras columnas de tipo numericas
#Diferencia entre matriz y dataframe: Matriz tiene datos del mismo tipo y 
#df no tienen que ser del mismo tipo
#Convertimos nuestro dataframe d2 a matriz 
d2 <- as.matrix(d2)
dim(d2)
str(d2)


#Calculamos componentes principales:
colMeans(d2)
apply(d2, 2, sd)
d22 <- prcomp(d2, scale = T) 
summary(d22)
biplot(d22)

#Varianza explicada:
par(mfrow = c(1, 2))

#Calculamos la variabilidad de cada componente:
pr.var <- d22$sdev^2

#Varianza explicada por CP:
pve <- pr.var / sum(pr.var)

#Graficos de la varianza explicada:
plot(pve, xlab = "Componentes principales",
     ylab = "Proporci?n de varianza explicada",
     ylim = c(0,1), type = "b")

plot(cumsum(pve), xlab = "Componentes principales",
     ylab = "Proporci?n de varianza explicada acumulada",
     ylim = c(0,1), type = "b")


# Ejercicio 3 -------------------------------------------------------------
#Clustering: Realiza un cluster jerarquico utilizando al menos 4 formas 
#de agrupacion diferentes, ?con cuantos cluster te quedarias y por que? 
#Compara si las observaciones se agrupan de un modo similar entre los 
#distintos metodos, y si no es asi, sus particularidades.

#Eliminamos filas con valores missing;
d2 <- na.omit(d2)

#Escalamos los datos con media 0 y sd 1
d2 <- scale(d2)

#Utilizamos la fucnion Agnes (aglomerative function)
#cuanto mas cercano sea este valor a 1, mas fuertes seran los grupos:
metodos <- c("average", "single", "complete")
names(metodos) <- c("average", "single", "complete")

#Calculamos la funcion de agloremacion : Agnes
ac <- function(x){
  agnes(d2, method = x)$ac
}

sapply(metodos, ac)

#En base a los criterios, complete va a ser el metodo de clusterizacion seleccionado

clust <- agnes(d2 , method = "complete")

#Ajustamos plot
par(mar = c(0.1, 0.1, 0.1, 0.1))
# Obtenemos grafico del dendrograma:
pltree(clust, cex = 0.0006, hang = 0.001, main = "Dendrograma", col = "aquamarine4")


#Comparamos los dendrogramas
hc_single <- agnes(d2, method = "single")
hc_complete <- agnes(d2, method="complete")

hc_single <- as.dendrogram(hc_single)
hc_complete <- as.dendrogram(hc_complete)
tanglegram(hc_single, hc_complete)

#Numero optimo de clusters:
p1 <- fviz_nbclust(d2, FUN = hcut, method = "wss", k.max = 15)+
  ggtitle("Metodo Elbow")

p2 <- fviz_nbclust(d2, FUN = hcut, method = "silhouette", k.max = 15)+
  ggtitle("Metodo Silhouette")

p3 <- fviz_nbclust(d2, FUN = hcut, method = "gap_stat", k.max = 15)+
  ggtitle("Gap")

#Graficamos los tres metodos:
gridExtra::grid.arrange(p1, p2, p3, nrow = 1)

#En base a los tres criterios, seleccionamos 3 clusters

# Distancia de la matriz:
d <- dist(d2, method = "euclidean")

#Cluster jerarquico:
final_clust <- hclust(d, method = "complete")

#Plot del dendrograma con k=3 clusters  
plot(final_clust, cex = 0.006, hang=0.001, col = "coral1")  
rect.hclust(final_clust, k=3, border=2:8)  

#Cortamos el dendrogama en 3 clusters:
groups <- cutree(final_clust, k=3)

#Numero de observaciones:
table(groups)

#Realizamos la visualizcion final de los clusters:
fviz_cluster(list(data=d2, cluster=groups), repel = F, show.clust.cent = FALSE, ggtheme = theme_minimal())

