# --- CARGA DE LIBRERÍAS ---
library(haven)
library(pROC)
library(mgcv)
library(ggplot2)
library(corrplot)
library(data.table)

# --- CARGA DE DATOS EPF ---
personas   <- read_dta("data/datos_epf/base-personas-ix-epf-stata.dta")
gastos     <- read_dta("data/datos_epf/base-gastos-ix-epf-stata.dta")
cantidades <- read_dta("data/datos_epf/base-cantidades-ix-epf-stata.dta")
ccif       <- read_dta("data/datos_epf/ccif-ix-epf-stata.dta")

# --- FILTRADO: Gran Santiago y datos válidos ---
valores_invalidos <- c(-99, -88, -77)

personas_gs <- subset(
  personas,
  macrozona == 2 &
    !(edad %in% valores_invalidos) &
    !(edue %in% valores_invalidos) &
    ing_disp_hog_hd_ai >= 0
)

# --- VARIABLES DERIVADAS ---
personas_gs$ing_pc <- personas_gs$ing_disp_hog_hd_ai / personas_gs$npersonas
personas_gs$id_persona <- paste(personas_gs$folio, personas_gs$n_linea, sep = "_")
cantidades$id_persona  <- paste(cantidades$folio, cantidades$n_linea, sep = "_")

# --- FILTRO GASTO EN VINO (Gran Santiago) ---
cantidades_vino <- subset(cantidades, (ccif == "02.1.2.01.01") & macrozona == 2)

# --- SUMA GASTO TOTAL EN VINO POR PERSONA ---
gasto_vino_por_persona <- aggregate(gasto ~ id_persona, data = cantidades_vino, sum)
names(gasto_vino_por_persona)[2] <- "gasto_vino"

# --- MERGE: Gasto con personas ---
personas_gs <- merge(personas_gs, gasto_vino_por_persona, by = "id_persona", all.x = TRUE)
personas_gs$gasto_vino[is.na(personas_gs$gasto_vino)] <- 0

# --- VARIABLE BINARIA DE GASTO ---
personas_gs$incurre_gasto <- ifelse(personas_gs$gasto_vino > 0, 1, 0)

# --- AGRUPACIÓN ESCOLARIDAD ---
personas_gs$grupo_escolaridad <- cut(
  personas_gs$edue,
  breaks = c(-Inf, 12, 14, 16, Inf),
  labels = c("Escolar", "Tecnico", "Universitaria", "Postgrado"),
  right = TRUE
)

# --- BASE PARA MODELO CONTINUO (solo quienes gastan) ---
tabla_gasto <- subset(personas_gs, gasto_vino > 0)
tabla_gasto <- tabla_gasto[, c("sexo", "edad", "edue", "ing_pc", "gasto_vino", "grupo_escolaridad")]

# --- TRANSFORMACIONES DE VARIABLES ---
tabla_gasto$sexo <- factor(tabla_gasto$sexo, labels = c("Hombre", "Mujer"))
tabla_gasto$log_ing_pc <- log(tabla_gasto$ing_pc)
tabla_gasto$log_gasto_vino <- log(tabla_gasto$gasto_vino + 1)
tabla_gasto$rango_edad <- cut(tabla_gasto$edad,
                              breaks = c(0, 29, 44, 64, Inf),
                              labels = c("jovenes", "adultos_jovenes", "adultos", "adultos_mayores")
)



# --- FILTRO DE OUTLIERS (percentil 1 y 99) ---
q_ing <- quantile(tabla_gasto$ing_pc, probs = c(0.01, 0.99))
q_gasto <- quantile(tabla_gasto$gasto_vino, probs = c(0.01, 0.99))

tabla_gasto <- subset(tabla_gasto,
                      ing_pc >= q_ing[1] & ing_pc <= q_ing[2] &
                        gasto_vino >= q_gasto[1] & gasto_vino <= q_gasto[2]
)

# --- GRAFICOS EXPLORATORIOS ---
# DISTRIBUCIÓN DEL INGRESO
hist(tabla_gasto$ing_pc, breaks = 30, col = "lightblue",
     main = "Distribución del Ingreso", xlab = "Ingreso per cápita")

# DISTRIBUCIÓN DEL GASTO EN VINO
hist(tabla_gasto$gasto_vino, breaks = 30, col = "lightblue",
     main = "Distribución del Gasto en Vino", xlab = "Gasto en vino")

# GASTO EN VINO SEGÚN SEXO
boxplot(gasto_vino ~ factor(sexo), data = tabla_gasto,
        main = "Gasto en Vino según Sexo", xlab = "Sexo",
        col = c("tomato", "lightgreen"))

# GASTO EN FUNCIÓN DE LA EDAD
plot(tabla_gasto$edad, tabla_gasto$gasto_vino,
     main = "Edad vs Gasto en Vino", xlab = "Edad", ylab = "Gasto",
     pch = 20, col = rgb(0, 0, 0, 0.3))
lines(lowess(tabla_gasto$edad, tabla_gasto$gasto_vino), col = "red", lwd = 2)

# GASTO EN FUNCIÓN DEL INGRESO
plot(tabla_gasto$ing_pc, tabla_gasto$gasto_vino,
     main = "Ingreso vs Gasto en Vino", xlab = "Ingreso per cápita", ylab = "Gasto",
     pch = 20, col = rgb(0, 0, 0, 0.3))
lines(lowess(tabla_gasto$ing_pc, tabla_gasto$gasto_vino), col = "blue", lwd = 2)

# BOXPLOT GASTO SEGÚN ESCOLARIDAD
boxplot(gasto_vino ~ grupo_escolaridad, data = tabla_gasto,
        main = "Gasto en Vino según Escolaridad", xlab = "Escolaridad",
        col = "skyblue")

# --- MODELO LINEAL: Quienes incurren en gasto ---
modelo_lineal <- lm(log_gasto_vino ~ grupo_escolaridad + ing_pc + rango_edad + factor(sexo), data = tabla_gasto)
summary(modelo_lineal)


# --- MODELO LOGÍSTICO: Probabilidad de incurrir en gasto ---

# Filtramos la base para asegurar que no haya NA en las variables relevantes
modelo_data <- subset(personas_gs,
                      !is.na(edad) & !is.na(grupo_escolaridad) & !is.na(sexo))

# Entrenamos el modelo logístico para predecir si una persona incurre en gasto
modelo_logit <- glm(
  incurre_gasto ~ factor(sexo) + edad + grupo_escolaridad + ing_pc,
  data = modelo_data,
  family = binomial
)

# --- PREDICCIONES DE PROBABILIDAD ---
# Calculamos la probabilidad predicha de incurrir en gasto según el modelo
modelo_data$prob_predicha <- predict(modelo_logit, type = "response")

# --- EVALUACIÓN INICIAL CON UMBRAL POR DEFECTO (0.5) ---
# Clasificamos: si la probabilidad es ≥ 0.5 → predice que incurre en gasto
modelo_data$clasificacion_05 <- ifelse(modelo_data$prob_predicha >= 0.5, 1, 0)

cat("---- Evaluación con umbral 0.5 ----\n")
conf_05 <- table(Real = modelo_data$incurre_gasto,
                 Predicha = modelo_data$clasificacion_05)
print(conf_05)

# - Se observa que el modelo predice muy bien la clase 0 (no gasto): 16.395 casos correctos.
# - Pero casi no detecta compradores reales: sólo 6 verdaderos positivos vs 933 falsos negativos.
# - Esto indica que el umbral 0.5 no es adecuado para una variable tan desbalanceada.

# Calculamos la precisión total (accuracy)
cat("Accuracy:", mean(modelo_data$incurre_gasto == modelo_data$clasificacion_05), "\n")

# Aunque el accuracy es alto, el modelo no es útil en la práctica porque solo predice bien la clase mayoritaria (no gasto) 
# y falla casi siempre con los compradores reales. Esto es un ejemplo clásico de por qué accuracy no debe usarse 
# solo cuando las clases están desbalanceadas. En su lugar, debemos mirar sensibilidad, especificidad y AUC.

# --- CURVA ROC Y ÁREA BAJO LA CURVA (AUC) ---
# Evaluamos la capacidad discriminativa del modelo
library(pROC)
roc_obj <- roc(modelo_data$incurre_gasto, modelo_data$prob_predicha)
plot(roc_obj, col = "blue", main = "Curva ROC")
cat("AUC:", auc(roc_obj), "\n")

TN <- conf_05["0", "0"]
FP <- conf_05["0", "1"]
especificidad_05 <- TN / (TN + FP)
cat("Especificidad (umbral 0.5):", especificidad_05, "\n")

# El modelo predice muy bien a quienes NO compran vino.

TP <- conf_05["1", "1"]
FN <- conf_05["1", "0"]
sensibilidad_05 <- TP / (TP + FN)
cat("Sensibilidad (umbral 0.5):", sensibilidad_05, "\n")

# El modelo casi nunca detecta a quienes SÍ compran vino.

# --- CÁLCULO DEL UMBRAL ÓPTIMO (CRITERIO DE YOUDEN) ---
# El umbral óptimo se define como aquel que maximiza la suma
# de sensibilidad y especificidad menos uno (índice de Youden).
# Este punto busca el mejor equilibrio entre verdaderos positivos y negativos.

coords_opt <- coords(roc_obj, "best", ret = c("threshold", "sensitivity", "specificity"))

# Extraemos el umbral y las métricas de desempeño asociadas
umbral_optimo <- as.numeric(coords_opt["threshold"])
cat("Umbral óptimo:", umbral_optimo, "\n")
cat("Sensibilidad óptima (Youden):", coords_opt["sensitivity"][[1]], "\n")
cat("Especificidad óptima (Youden):", coords_opt["specificity"][[1]], "\n")

# --- EVALUACIÓN CON UMBRAL ÓPTIMO ---
# Clasificamos nuevamente, esta vez usando el umbral óptimo hallado
modelo_data$clasificacion_optima <- ifelse(modelo_data$prob_predicha >= umbral_optimo, 1, 0)

cat("\n---- Evaluación con umbral óptimo ----\n")
conf_opt <- table(Real = modelo_data$incurre_gasto,
                  Predicha = modelo_data$clasificacion_optima)
print(conf_opt)

# Calculamos la precisión total (accuracy) con este nuevo corte
accuracy_opt <- mean(modelo_data$incurre_gasto == modelo_data$clasificacion_optima)
cat("Accuracy (óptimo):", accuracy_opt, "\n")

# --- CÁLCULO EXPLÍCITO DE SENSIBILIDAD Y ESPECIFICIDAD CON UMBRAL ÓPTIMO ---
TN_opt <- conf_opt["0", "0"]
FP_opt <- conf_opt["0", "1"]
TP_opt <- conf_opt["1", "1"]
FN_opt <- conf_opt["1", "0"]

especificidad_opt <- TN_opt / (TN_opt + FP_opt)
sensibilidad_opt <- TP_opt / (TP_opt + FN_opt)

cat("Especificidad (umbral óptimo):", especificidad_opt, "\n")
cat("Sensibilidad (umbral óptimo):", sensibilidad_opt, "\n")

# --- COMENTARIO FINAL ---
# A diferencia del umbral estándar (0.5), este nuevo corte mejora
# considerablemente la sensibilidad: ahora el modelo logra detectar
# una proporción significativa de compradores reales de vino.
# Aunque se pierde algo de especificidad, el balance general del modelo
# es mucho más útil en contextos donde identificar correctamente
# a quienes sí incurren en gasto es prioritario.

#### CASEN ####

# Carga base CASEN Región Metropolitana
casen <- readRDS("trabajos/t2_microsim/data/casen_base_preprocesado.rds")

# Crear grupo de escolaridad
casen$grupo_escolaridad <- cut(
  casen$esc,
  breaks = c(-Inf, 12, 14, 16, Inf),
  labels = c("Escolar", "Tecnico", "Universitaria", "Postgrado"),
  right = TRUE
)

# Rango de edad (como en EPF)
casen$rango_edad <- cut(casen$edad,
                        breaks = c(0, 29, 44, 64, Inf),
                        labels = c("jovenes", "adultos_jovenes", "adultos", "adultos_mayores"))

casen$ing_pc <- casen$ypc

casen <- casen[!is.na(casen$ing_pc), ]

# Predecir probabilidad de incurrir en gasto
casen$prob_predicha <- predict(modelo_logit, newdata = casen, type = "response")

# Clasificar según umbral óptimo
casen$clasificacion <- ifelse(casen$prob_predicha >= umbral_optimo, 1, 0)

# Asegurarse que sexo tiene los mismos niveles ("Hombre", "Mujer") que en el modelo lineal
casen$sexo <- factor(as.character(casen$sexo), levels = c("1", "2"), labels = c("Hombre", "Mujer"))

# Filtrar quienes incurren en gasto
casen_pred <- casen[casen$clasificacion == 1, ]

# Predecir en escala log
casen_pred$log_gasto_estimado <- predict(modelo_lineal, newdata = casen_pred)

# Volver a escala natural (como usaste log(gasto + 1))
casen_pred$gasto_estimado <- exp(casen_pred$log_gasto_estimado) - 1

# Controlar outliers (Winzorización)
casen_pred$gasto_estimado_wins <- pmin(casen_pred$gasto_estimado, quantile(casen_pred$gasto_estimado, 0.999))

summary(tabla_gasto$gasto_vino)
summary(casen_pred$gasto_estimado_wins)

sd(tabla_gasto$gasto_vino)
sd(casen_pred$gasto_estimado_wins)

plot(density(tabla_gasto$gasto_vino), col = "blue", lwd = 2, main = "Densidad: EPF vs CASEN imputado")
lines(density(casen_pred$gasto_estimado_wins), col = "red", lwd = 2)
legend("topright", legend = c("EPF", "CASEN imputado"), col = c("blue", "red"), lwd = 2)