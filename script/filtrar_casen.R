library(corrplot)
ruta_rds = "data/casen_rm.rds"
casen_rm = readRDS(ruta_rds)

# Histograma de ingreso per capita original
#hist(casen_rm$ypc, xlab = "Ingreso percapita", col = "lightblue")
#boxplot(casen_rm$ytotcor)

# Reducción del umbral
umbral = quantile(casen_rm$ytotcor, 0.9, na.rm = TRUE)
clean = casen_rm[casen_rm$ytotcor <= umbral & (casen_rm$edad < 65), ]
hist(clean$ypc, xlab = "Ingreso percapita", col = "green")
boxplot(clean$ytotcor)

# Creación de data frame y matriz de correlación
tabla <- data.frame(clean$ytotcor, clean$edad, clean$e6a, clean$p9)
correlation_matrix <- cor(tabla, use = "complete.obs", method = "pearson")
#Gráfica
corrplot(correlation_matrix, method = "color", tl.cex = 0.8, number.cex = 0.7)

# Crear el modelo de regresión multiple
modelo <- lm(clean$ytotcor ~ clean$edad + clean$e6a + clean$p9, data = clean)
summary(modelo)