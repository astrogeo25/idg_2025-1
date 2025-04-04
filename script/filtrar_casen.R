# Abrir CASEN
# Entradas
ruta_rds = "data/casen_rm.rds"
casen_rm = readRDS(ruta_rds)

# Tarea: realizar regresión Ingreso = N.edu + sexo + edad + trabajo

# Variable dependiente
# ytotcor, yautcor, ypc
# limpieza dataframe, analisis correlacion, regresion

hist(casen_rm$ypc, xlab = "Ingreso percapita", col = "lightblue")
boxplot(casen_rm$ypc)

umbral = quantile(casen_rm$ypc, 0.85, na.rm = TRUE)
casen_clean = casen_rm[casen_rm$ypc <= umbral & (casen_rm$edad > 15), ]
boxplot(casen_clean$ypc)
hist(casen_clean$ypc, xlab = "Ingreso percapita", col = "blue")