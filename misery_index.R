library(readxl)
misery <- read_excel("3º BA/2º cuatri/ECONOMETRÍA/econometría/Misery_index_1977.xlsx")


# 1. Obtención de la tendencia con distintos métodos y predicción con el más adecuado -------------------- 
library(tseries)

## Burkina Faso ----------------------------------------------------------------
burkina <- ts(misery$`Burkina Faso`, start = c(1977, 1), end = c(2014, 1), frequency = 1)

ts.plot(burkina)

## Medias móviles
require(TTR)
MM_burkina <- SMA(burkina)
plot(MM_burkina, main = "Gráfico de Burkina de MA")


## Minimos cuadrados ordinarios
años <- c(1977:2014)
MCO_burkina <- lm(burkina~años)
# para calcular la tendencia sería: Y = 483.1540 - 0.2374*x


## South Africa --------------------------------------------------------------
southafrica <- ts(misery$`South Africa`, start = c(1977, 1), end = c(2014,1), frequency = 1)
ts.plot(southafrica)

## Medias móviles
require(TTR)
MM_southAfrica <- SMA(southafrica)
plot(MM_southAfrica, main = "Gráfico de Sudáfrica de MA")


## Minimos cuadrados ordinarios
años <- c(1977:2014)
MCO_southafrica <- lm(southafrica~años)
# para calcular la tendencia sería: Y = -191.4588 + 0.1114*x



# 2. Metodología Box_Jenkins ---------------------------------------------------
## Burkina Faso ----------------------------------------------------------------

### 2. Estacionariedad

# grafico
ts.plot(burkina, main = "Gráfico índice misearia Burkina Faso")

# Dickey Fuller
adf.test(burkina)


### 3. Transformacion previa de la serie
burkina_log <- log(burkina)


### 4. Eliminación de la tendencia

## diferencias
burkina_diff <- diff(burkina)

## grafico
ts.plot(burkina_diff, main = "Gráfico de la serie Burkina con diferencias")

## Dickey Fuller
adf.test(burkina_diff)


### 5. Identificación del modelo

## correlograma
par(mfrow=c(1,2))
acf(burkina_diff, main = "FAC burkina")
pacf(burkina_diff, main = "FACP burkina")


# 6. Estimación de los coeficientes de los modelos

library(forecast)

burkina1 <- auto.arima(burkina)
# toma un ARIMA (0,1,1)
# AIC = 264.24
# con este modelo se nos quedaría: ∇Yt = -0.8028𝜺t-1 + 𝜺t

burkina2 <- arima(burkina, order = c(1,1,2))
# AIC = 266.51
# con este modelo se nos quedaría: 
#  ∇Yt = -0.0974Yt-1 + 𝜺t - 0.8456𝜺t-1 + 0.1459𝜺t-2

burkina3 <- arima(burkina, order = c(1,1,0))
# AIC = 270.77
# con este modelo se nos quedaría: 
#  ∇Yt = -0.5725Yt-1 + 𝜺t

burkina4 <- arima(burkina, order = c(2,1,0))
# AIC = 264.73
# con este modelo se nos quedaría: 
#  ∇Yt = -0.8327Yt-1 - 0.4566Yt-2 + 𝜺t


### 7. Contraste de validez del modelo

# modelo 1 (ARIMA(0,1,1))
burkina1_sigma1 <- (-0.8051 - 0)/0.1104
## criterio de significatividad de parámetros
coeftest(burkina1)

# modelo 2 (ARIMA(2,1,1))
burkina2_fi1 <- (-0.0997 - 0)/0.4753
burkia2_fi2 <- (-0.8460 - 0)/0.4475
burkina2_sigma1 <- (0.1147 - 0)/0.3725
## criterio de significatividad de parámetros
coeftest(burkina2)
library(Metrics)

# modelo 3 (ARIMA(1,1,0))
burkina3_fi1 <- (-0.5740 - 0)/0.1357
## criterio de significatividad de parámetros
coeftest(burkina3)

# modelo 4 (ARIMA(2,1,0))
burkina4_fi1 <- (-0.8344 - 0)/0.1471
burkina4_fi2 <- (-0.4581 - 0)/0.1443
## criterio de significatividad de parámetros
coeftest(burkina4)


### 8.  Análisis detallado de los errores

# MODELO 1
residuos_burkina1 <- residuals(burkina1)
  ### graficos
par(mfrow = c(1,1))
plot(residuos_burkina1, main = "Gráfico de los residuos de Burkina1")
  ### media = 0
mean(residuos_burkina1)
  ### test de normalidad
jarque.bera.test(residuos_burkina1)
  ### test de asimetría
library(e1071)
  ### asimetría
skewness(residuos_burkina1)
  ### kurtosis
kurtosis(residuos_burkina1)
  ### test de no autocorrelación
Box.test(residuos_burkina1)
  ### varianza
length(residuos_burkina1)
sd(residuos_burkina1[1:9])
sd(residuos_burkina1[10:19])
sd(residuos_burkina1[20:29])
sd(residuos_burkina1[30:38])
  ### test de heterocedasticidad
bptest(MCO_burkina)
  ### correlograma
par(mfrow = c(1,2))
acf(residuos_burkina1, main = "FAC residuos de Burkina1")
pacf(residuos_burkina1, main = "FACP residuos de Burkina1")

# MODELO 2
residuos_burkina2 <- residuals(burkina2)
  ### graficos
plot(residuos_burkina2)
  ### media = 0
mean(residuos_burkina2)
  ### test de normalidad
jarque.bera.test(residuos_burkina2)
  ### test de no autocorrelación
Box.test(residuos_burkina2)
  ### varianza
length(residuos_burkina2)
sd(residuos_burkina2[1:9])
sd(residuos_burkina2[10:19])
sd(residuos_burkina2[20:29])
sd(residuos_burkina2[30:37])

# MODELO 3
residuos_burkina3 <- residuals(burkina3)
  ### graficos
par(mfrow = c(1,1))
plot(residuos_burkina3, main = "Gráfico de los residuos de Burkina3")
  ### media = 0
mean(residuos_burkina3)
  ### test de normalidad
jarque.bera.test(residuos_burkina3)
  ### test de asimetría
library(e1071)
  ### asimetría
skewness(residuos_burkina3)
  ### kurtosis
kurtosis(residuos_burkina3)
  ### test de no autocorrelación
Box.test(residuos_burkina3)
  ### varianza
length(residuos_burkina3)
sd(residuos_burkina3[1:9])
sd(residuos_burkina3[10:19])
sd(residuos_burkina3[20:29])
sd(residuos_burkina3[30:38])
  ### correlograma
par(mfrow = c(1,2))
acf(residuos_burkina3, main = "FAC de los residuos de Burkina3")
pacf(residuos_burkina3, main = "FACP de los residuos de Burkina3")

# MODELO 4
residuos_burkina4 <- residuals(burkina4)
  ### graficos
plot(residuos_burkina4, main = "Gráfico de los residuos de Burkina4")
  ### media = 0
mean(residuos_burkina4)
  ### test de normalidad
jarque.bera.test(residuos_burkina4)
  ### test de asimetría
library(e1071)
  ### asimetría
skewness(residuos_burkina4)
  ### kurtosis
kurtosis(residuos_burkina4)
  ### test de no autocorrelación
Box.test(residuos_burkina4)
  ### varianza
length(residuos_burkina4)
sd(residuos_burkina4[1:9])
sd(residuos_burkina4[10:19])
sd(residuos_burkina4[20:29])
sd(residuos_burkina4[30:38])
  ### correlograma
par(mfrow = c(1,2))
acf(residuos_burkina4, main = "FAC de los residuos de Burkina4")
pacf(residuos_burkina4, main = "FACP de los residuos de Burkina4")


### 8. Selección del modelo
# analizamos los parámetros, residuos y el criterio akaike, sacamos como conclusión
# que el mejor modelo seria burkina 1, que es un ARIMA(0,1,1).

### 9. Predicción

prediction_burkina <- forecast(burkina1, h = 3)
prediction_burkina$mean

burkina_total <- ts(misery$`Burkina Faso`, start = 1977, frequency = 1)
burkina_test <- tail(burkina_total, 3)

### MSE
mean((prediction_burkina$mean - burkina_test)^2)


## Sudáfrica -------------------------------------------------------------------

### 2. Estacionariedad

## grafico
ts.plot(southafrica, main = "Gráfico índice miseria Sudáfrica")

## Dickey Fuller
adf.test(southafrica)


### 3. Transformacion previa de la serie

## logaritmos
southafrica_log <- log(southafrica)
plot(southafrica_log, main = "Gráfico de la serie Sudáfrica con logaritmos")

### 4. Eliminación de la tendencia

## diferencias
dlog_southafrica <- diff(southafrica_log)

## grafico
ts.plot(dlog_southafrica, main = "Gráfico de la serie de Sudáfrica con diferencias y logaritmos")

## Dickey Fuller
adf.test(dlog_southafrica)


### 5. Identificación del modelo

## correlograma
par(mfrow=c(1,2))
acf(dlog_southafrica, main = "FAC Sudáfrica")
pacf(dlog_southafrica, main = "FACP Sudáfrica")


### 6. Estimación de los coeficientes de los modelos

# Modelo 1 (ARIMA(0,1,0))
southafrica1 <- auto.arima(southafrica_log)


### 7. Contraste de validez del modelo


### 8.  Análisis detallado de los errores

# MODELO 1
residuos_southafrica1 <- residuals(southafrica1)
  ### graficos
par(mfrow = c(1,1))
plot(residuos_southafrica1, main = "Gráfico de los residuos de Sudáfrica1")
  ### media = 0
mean(residuos_southafrica1)
  ### test de normalidad
jarque.bera.test(residuos_southafrica1)
  ### test de asimetría
library(e1071)
  ### asimetría
skewness(residuos_southafrica1)
  ### kurtosis
kurtosis(residuos_southafrica1)
  ### test de no autocorrelación
Box.test(residuos_southafrica1)
  ### varianza
length(residuos_southafrica1)
sd(residuos_southafrica1[1:9])
sd(residuos_southafrica1[10:19])
sd(residuos_southafrica1[20:29])
sd(residuos_southafrica1[30:38])
  ### correlograma
par(mfrow = c(1,2))
acf(residuos_southafrica1, main = "FAC de los residuos de Sudáfrica1")
pacf(residuos_southafrica1, main = "FACP de los residuos de Sudáfrica1")

  
### 8. Selección del modelo


### 9. Predicción
prediction_southafrica <- forecast(southafrica1, h = 3)





