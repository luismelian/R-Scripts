#Ejercicio_1
library(readr)
library(psych)
library(stringr)
library(ggplot2)
library(dplyr)

#Lectura de datos
df <- read.table("C:/Users/LuisMeliánPeña/OneDrive - SEOPAN/MASTER EOI/Modulo 8/datos_telco.csv", 
                 header = TRUE,       
                 sep = ",",           
                 stringsAsFactors = FALSE, 
                 na.strings = c("", "NA"))

#Estadísticas descriptivas
summary(df)
describe(df)
View(df)

#Selección de variables numéricas
numericas <- df[sapply(df, is.numeric)]
#Cálculo de la matriz de correlaciones
cor_matrix <- cor(numericas)
#Matriz de correlaciones
print(cor_matrix)



#Tabla de frecuencias y proporciones
table_international <- table(df$International.plan, df$Churn)
prop_international <- prop.table(table_international, margin = 1) * 100

#Conversión en DataFrame
df_international <- as.data.frame(table_international)
colnames(df_international) <- c("International Plan", "Churn", "Count")

#Columna de porcentajes
df_international$Percentage <- round(prop_international[cbind(as.character(df_international$`International Plan`), 
                                                              as.character(df_international$Churn))], 1)

#Gráfico de barras agrupadas con porcentajes
ggplot(df_international, aes(x = `International Plan`, y = Count, fill = Churn)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") + 
  geom_text(aes(label = paste0(Percentage, "%")), 
            position = position_dodge(0.9), 
            vjust = -0.5, 
            size = 4) + 
  labs(title = "Comparación de Churn según el Plan Internacional",
       x = "Plan Internacional", 
       y = "Cantidad de Usuarios") +
  scale_fill_manual(values = c("blue", "orange")) +
  theme_minimal() + 
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5))  




#Tabla de frecuencias
table_voicemail <- table(df$Voice.mail.plan, df$Churn)

#Conversión en dataframe
df_voicemail <- as.data.frame(table_voicemail)
colnames(df_voicemail) <- c("VoiceMailPlan", "Churn", "Count")

#Porcentajes de churn por grupo
df_voicemail <- df_voicemail %>%
  group_by(VoiceMailPlan) %>%
  mutate(Percentage = Count / sum(Count) * 100)

#Gráfico de barras agrupadas con porcentajes
ggplot(df_voicemail, aes(x = VoiceMailPlan, y = Count, fill = Churn)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 4) +
  labs(title = "Churn según el Plan de Correo de Voz",
       x = "Plan de Correo de Voz",
       y = "Cantidad de Usuarios") +
  scale_fill_manual(values = c("cornflowerblue", "deeppink")) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),  
        panel.background = element_blank())


--------------------------------------------------------------------------------------------------------------------------------------


#Ejercicio_2

#PASO 1: Parámetros del enunciado
n <- 30
alpha <- 2
beta <- 6

#Crear vector para almacenar las medias muestrales
medias_muestrales <- c()

for (i in 1:1000) {
  muestra <- rbeta(n, shape1 = alpha, shape2 = beta)  
  media_muestral <- mean(muestra)  
  medias_muestrales <- c(medias_muestrales, media_muestral)
}

#PASO 2: Calcular la media poblacional y desviación típica poblacional
media_poblacional <- alpha / (alpha + beta)
sd_poblacional <- sqrt((alpha * beta) / ((alpha + beta)^2 * (alpha + beta + 1)))

cat("La media poblacional es igual a", media_poblacional, "y la desviación típica poblacional es igual a", sd_poblacional, "\n")

#PASO 3: Visualización gráfica de la distribución de medias muestrales
hist(medias_muestrales, breaks = 30, probability = TRUE, col = "darkseagreen",
     main = "Histograma de las medias muestrales",
     xlab = "Medias muestrales", ylab = "Densidad")

# Superponer la distribución normal esperada
curve(dnorm(x, mean = media_poblacional, sd = sd_poblacional / sqrt(n)), 
      col = "darkslategrey", lwd = 2, add = TRUE)

#PASO 4: Calcular la media y desviación
media_muestrales <- mean(medias_muestrales)
sd_muestrales <- sd(medias_muestrales)

cat("La media de esta distribución normal es", media_muestrales, "\n")

cat("La desviación típica es igual a", sd_muestrales, "\n")

#PASO 5: Comprobación de que la distribución de las medias muestrales es aproximadamente N(μ, σ/√n)
if (abs(media_muestrales - media_poblacional) < 0.01) {
  cat("La media de las medias muestrales coincide con la media poblacional\n")
} else {
  cat("La media de las medias muestrales NO coincide exactamente con la media poblacional\n")
}

if (abs(sd_muestrales - (sd_poblacional / sqrt(n))) < 0.01) {
  cat("La desviación de las medias muestrales coincide con la desviación poblacional dividida por raíz de n\n")
} else {
  cat("La desviación de las medias muestrales NO coincide exactamente con la esperada\n")
}


-------------------------------------------------------------------------------------------------------------------------------------

#Ejercicio_3

#Cálculo de la media y desviación estándar de la variable Total.day.minutes
media <- mean(df$Total.day.minutes)
desv_est <- sd(df$Total.day.minutes)
n <- length(df$Total.day.minutes)  
alpha <- 0.05  
z <- qnorm(1 - alpha/2)

#Cálculo del margen de error
margen_error <- z * (desv_est / sqrt(n))

#Intervalo de confianza
lim_inf <- media - margen_error
lim_sup <- media + margen_error

#Mostrar los resultados
cat("Intervalo de confianza al 95% para la media de Total.day.minutes: (", lim_inf, ",", lim_sup, ")\n")

#Test de normalidad
shapiro_test <- shapiro.test(df$Total.day.minutes)
cat("Test de Shapiro-Wilk para normalidad: p-valor =", shapiro_test$p.value, "\n")

#Histograma con curva de densidad y normal teórica
ggplot(df, aes(x = Total.day.minutes)) +
  geom_histogram(aes(y = ..density..), bins = 40, fill = "aquamarine3", color = "black", alpha = 0.6) +
  geom_density(color = "brown1", linewidth = 1.2) +
  stat_function(fun = dnorm, args = list(mean = media, sd = desv_est), color = "brown", linewidth = 1.2, linetype = "dashed") +
  labs(title = "Distribución de Total.day.minutes", x = "Total.day.minutes", y = "Densidad") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank())


-------------------------------------------------------------------------------------------------------------------------------------


#Ejercicio_4

#Parámetros del problema
p_0 <- 0.13 
n <- length(df$Churn)  
x <- sum(df$Churn == "True")  
p_muestra <- x / n  

#Test de hipótesis
z_obs <- (p_muestra - p_0) / sqrt((p_0 * (1 - p_0)) / n)

#Valores críticos para los niveles de significancia
alpha_1 <- 0.01
alpha_2 <- 0.001
z_crit_1 <- qnorm(1 - alpha_1)  # Valor crítico para α = 0.01
z_crit_2 <- qnorm(1 - alpha_2)  # Valor crítico para α = 0.001

#P-valor del test
p_valor <- 1 - pnorm(z_obs)

#Resultados
cat("Proporción muestral:", p_muestra, "\n")
cat("Estadístico Z observado:", z_obs, "\n")
cat("P-valor:", p_valor, "\n")
cat("Valor crítico para α = 0.01:", z_crit_1, "\n")
cat("Valor crítico para α = 0.001:", z_crit_2, "\n")

#Decisión para α = 0.01
if (z_obs > z_crit_1) {
  cat("Rechazamos H0 al nivel de significancia 0.01: La tasa de bajas es significativamente mayor al 13%\n")
} else {
  cat("No podemos rechazar H0 al nivel 0.01: No hay evidencia suficiente para decir que la tasa de bajas es mayor al 13%\n")
}

#Decisión para α = 0.001
if (z_obs > z_crit_2) {
  cat("Rechazamos H0 al nivel de significancia 0.001: La tasa de bajas es significativamente mayor al 13%\n")
} else {
  cat("No podemos rechazar H0 al nivel 0.001: No hay evidencia suficiente para decir que la tasa de bajas es mayor al 13%\n")
}