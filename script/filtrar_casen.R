# Abrir CASEN
# Entradas
ruta_rds = "data/casen_rm.rds"
casen_rm = readRDS(ruta_rds)

# Histograma de ingreso per capita original
hist(casen_rm$ypc, xlab = "Ingreso percapita", col = "lightblue")
boxplot(casen_rm$ypc)

# Reducción del umbral
umbral = quantile(casen_rm$ypc, 0.85, na.rm = TRUE)
casen_clean = casen_rm[casen_rm$ypc <= umbral & (casen_rm$edad > 15) & (casen_rm$edad < 65), ]
boxplot(casen_clean$ypc)
hist(casen_clean$ypc, xlab = "Ingreso percapita", col = "green")

# Crear el modelo de regresión multiple y resumir o25
modelo <- lm(casen_clean$ypc ~ casen_clean$edad + casen_clean$e6a , data = casen_clean)
summary(modelo)

# Intepretar beta, signo, significancia