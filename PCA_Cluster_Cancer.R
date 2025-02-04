# Set de datos: Cancer
## url: https://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Wisconsin+%28Diagnostic%29
library(factoextra)
library(cluster)
library(dendextend)

url <- "http://s3.amazonaws.com/assets.datacamp.com/production/course_1903/datasets/WisconsinCancer.csv"

# Descarga de datos: wisc.df
wisc.df <- read.csv(url)
summary(wisc.df)
class(wisc.df)
dim(wisc.df)

wisc.data <- as.matrix(wisc.df[,3:32])
dim(wisc.data)

# Establecemos el nombre de filas:
row.names(wisc.data) <- wisc.df$id

# Creamos la variable diagnosis:
diagnosis <- as.numeric(wisc.df$diagnosis == "M", 1, 0)
table(diagnosis)

# Calculamos primero componentes principales:
## Comprobamos la media y desviacion tipica de las columnas:
colMeans(wisc.data)
apply(wisc.data, 2, sd)

## Ejecutamos PCA, escalando variables:
wisc.pr <- prcomp(wisc.data, scale = TRUE)

# Analizamos los resultados:
summary(wisc.pr)

## Interpretacion de los datos:
biplot(wisc.pr)

## Grafico de puntos:
plot(wisc.pr$x[, c(1, 2)], col = (diagnosis + 1),
     xlab = "PC1", ylab = "PC2")

## Grafico de puntos:
plot(wisc.pr$x[, c(1, 3)], col = (diagnosis +1),
     xlab = "PC1", ylab = "PC3")

## Varianza explicada:
par(mfrow = c(1, 2))

## Calculamos la variabilidad de cada componente:
pr.var <- wisc.pr$sdev^2

## Varianza explicada por cada CP: pve
pve <- pr.var/sum(pr.var)

## Grafico de la varianza explicada
plot(pve, xlab = "Componentes principales",
     ylab="Proporcion varianza explicada",
     ylim = c(0, 1), type = "b")

## Grafico de la varianza explicada acumulada:
plot(cumsum(pve), xlab = "Componentes principales",
     ylab="Proporcion varianza explicada acumulada",
     ylim = c(0, 1), type = "b")

# Clustering jerarquico:
## Definimos los metodos de union:
m <- c("average", "single", "complete")
names(m) <- c("average", "single", "complete")

# Calculamos el coeficiente de aglomeración:
ac <- function(x){
  agnes(wisc.pr$x[,1:4], method = x)$ac
}
sapply(m, ac)

# Seleccionamos el metodo "complete"
clust <- agnes(wisc.pr$x[,1:4], method = "complete")

par(mfrow=c(1,1))
pltree(clust, cex = 0.6, hang = -1, "Dendrograma")


# Cantidad optima de clusters: --------------------------------------------
p1 <- fviz_nbclust(wisc.pr$x[,1:4], FUN = hcut, method = "wss",
                   k.max= 15)+
  ggtitle("Metodo Elbow")

p2 <- fviz_nbclust(wisc.pr$x[,1:4], FUN = hcut, method = "silhouette",
                   k.max= 15)+
  ggtitle("Metodo silhouette")

p3 <- fviz_nbclust(wisc.pr$x[,1:4], FUN = hcut, method = "gap_stat",
                   k.max= 15)+
  ggtitle("Metodo Gap")

# Graficamos los tres metodos:
gridExtra::grid.arrange(p1, p2, p3, nrow = 1)

# Distancia de la matriz:
d <- dist(wisc.pr$x[,1:4] ,method = "euclidean")

# Creamos el cluster
final_clust <- hclust(dist(wisc.pr$x[, 1:4]), method = "complete")

final_clust_clust <- cutree(final_clust, k=4)
table(diagnosis, final_clust_clust)

final_clust_clust <- cutree(final_clust, k=2)
table(diagnosis, final_clust_clust)


# Visualizción final:
fviz_cluster(list(data = wisc.pr$x[,1:4], cluster = final_clust_clust))
