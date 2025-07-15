# --- CARGA DE LIBRERÍAS ---
library(haven)
library(pROC)
library(mgcv)
library(ggplot2)
library(corrplot)
library(data.table)
library(RPostgres)
library(DBI)

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

# --- FILTRO GASTO EN chocolate (Gran Santiago) ---
cantidades_chocolate <- subset(cantidades, (ccif == "02.1.2.01.01") & macrozona == 2)

# --- SUMA GASTO TOTAL EN chocolate POR PERSONA ---
gasto_chocolate_por_persona <- aggregate(gasto ~ id_persona, data = cantidades_chocolate, sum)
names(gasto_chocolate_por_persona)[2] <- "gasto_chocolate"

# --- MERGE: Gasto con personas ---
personas_gs <- merge(personas_gs, gasto_chocolate_por_persona, by = "id_persona", all.x = TRUE)
personas_gs$gasto_chocolate[is.na(personas_gs$gasto_chocolate)] <- 0

# --- VARIABLE BINARIA DE GASTO ---
personas_gs$incurre_gasto <- ifelse(personas_gs$gasto_chocolate > 0, 1, 0)

# --- AGRUPACIÓN ESCOLARIDAD ---
personas_gs$grupo_escolaridad <- cut(
  personas_gs$edue,
  breaks = c(-Inf, 12, 14, 16, Inf),
  labels = c("Escolar", "Tecnico", "Universitaria", "Postgrado"),
  right = TRUE
)

# --- BASE PARA MODELO CONTINUO (solo quienes gastan) ---
tabla_gasto <- subset(personas_gs, gasto_chocolate > 0)
tabla_gasto <- tabla_gasto[, c("sexo", "edad", "edue", "ing_pc", "gasto_chocolate", "grupo_escolaridad")]

# --- TRANSFORMACIONES DE VARIABLES ---
tabla_gasto$sexo <- factor(tabla_gasto$sexo, labels = c("Hombre", "Mujer"))
tabla_gasto$log_ing_pc <- log(tabla_gasto$ing_pc)
tabla_gasto$log_gasto_chocolate <- log(tabla_gasto$gasto_chocolate + 1)
tabla_gasto$rango_edad <- cut(tabla_gasto$edad,
                              breaks = c(0, 29, 44, 64, Inf),
                              labels = c("jovenes", "adultos_jovenes", "adultos", "adultos_mayores")
)

# --- MODELO LINEAL: Quienes incurren en gasto ---
modelo_lineal <- lm(log_gasto_chocolate ~ grupo_escolaridad + ing_pc + rango_edad + factor(sexo), data = tabla_gasto)
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

# --- CURVA ROC Y ÁREA BAJO LA CURVA (AUC) ---
# Evaluamos la capacidad discriminativa del modelo
library(pROC)
roc_obj <- roc(modelo_data$incurre_gasto, modelo_data$prob_predicha)
#plot(roc_obj, col = "blue", main = "Curva ROC")
# cat("AUC:", auc(roc_obj), "\n")

# --- CÁLCULO DEL UMBRAL ÓPTIMO (CRITERIO DE YOUDEN) ---

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

# --- LECTURA DEL CENSO/CASEN ---
# Entradas

## df del censo ya procesado
cons_censo_df <- readRDS("data/cons_censo_df.rds")
casen_raw = readRDS("data/casen_rm.rds") 

# Preprocesamiento

## 3.1 Extracción de variables del Censo
col_cons   = sort(setdiff(names(cons_censo_df), c("GEOCODIGO","COMUNA")))
age_levels  = grep("^edad", col_cons, value = TRUE)
esc_levels  = grep("^esco", col_cons, value = TRUE)
sexo_levels = grep("^sexo_",col_cons, value = TRUE)

## Extracción de variables de la CASEN
vars_base = c("estrato", "esc", "edad", "sexo", "e6a", "h7a", "ypc")
casen = casen_raw[ , vars_base, drop = FALSE]
rm(casen_raw)

casen$Comuna = substr(as.character(casen$estrato), 1, 5)
casen$estrato = NULL

# Definir según tipo la variable. Se usa numericas para calculos en R
casen$esc = as.integer(unclass(casen$esc))
casen$edad = as.numeric(unclass(casen$edad))
casen$e6a = as.numeric(unclass(casen$e6a))
casen$sexo = as.integer(unclass(casen$sexo))
casen$ypc = as.numeric(unclass(casen$ypc))
casen$h7a = as.numeric(unclass(casen$h7a))
casen$h7a <- ifelse(casen$h7a %in% c(1, 2), 1, 0)  # 1 para sano o levemente afectado, 0 para otros

idx_na = which(is.na(casen$esc))
fit = lm(esc ~ e6a, data = casen[-idx_na,])
pred = predict(fit, newdata = casen[idx_na, ,drop = FALSE])
casen$esc[idx_na] = as.integer(round(pmax(0, pmin(29, pred))))
casen$ID = as.character(seq_len(nrow(casen)))

casen$edad_cat <- cut(
  casen$edad,
  breaks = c(0,30,40,50,60,70,80,Inf),
  labels = age_levels,
  right = FALSE, include.lowest = TRUE
)

casen$esc_cat <- factor(
  with(casen,
       ifelse(esc == 0,           esc_levels[1],
              ifelse(esc <= 8,    esc_levels[2],
                     ifelse(esc <= 12, esc_levels[3],
                            esc_levels[4])))),
  levels = esc_levels
)

casen$sexo_cat <- factor(
  ifelse(casen$sexo == 2, sexo_levels[1],  
         ifelse(casen$sexo == 1, sexo_levels[2], NA)), 
  levels = sexo_levels
)

# --- MICROSIMULACION ---
cons_censo_comunas = split(cons_censo_df, cons_censo_df$COMUNA)
inds_list = split(casen, casen$Comuna)
sim_list = lapply(names(cons_censo_comunas), function(zona) {
  cons_i    = cons_censo_comunas[[zona]]
  col_order = sort(setdiff(names(cons_i), c("COMUNA","GEOCODIGO")))
  cons_i    = cons_i[, c("GEOCODIGO", col_order), drop = FALSE]
  
  tmp    = inds_list[[zona]]
  inds_i = tmp[, c("ID","edad_cat","esc_cat","sexo_cat"), drop = FALSE]
  names(inds_i) = c("ID","Edad","Escolaridad","Sexo")
  
  w_frac  = weight(cons = cons_i, inds = inds_i,
                   vars = c("Edad","Escolaridad","Sexo"))
  sim_i   = integerise(weights = w_frac, inds = inds_i, seed = 123)
  
  merge(sim_i,
        tmp[, c("ID","h7a","edad", "ypc")],  # ahora incluye edad e ypc
        by = "ID", all.x = TRUE)
})

sim_df = data.table::rbindlist(sim_list, idcol = "COMUNA")

# Acomodar para epf
casen$sexo_cat <- factor(
  ifelse(casen$sexo == 2, sexo_levels[1],  
         ifelse(casen$sexo == 1, sexo_levels[2], NA)), 
  levels = sexo_levels
)

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

summary(tabla_gasto$gasto_chocolate)
summary(casen_pred$gasto_estimado_wins)

sd(tabla_gasto$gasto_chocolate)
sd(casen_pred$gasto_estimado_wins)

plot(density(tabla_gasto$gasto_chocolate), col = "blue", lwd = 2, main = "Densidad: EPF vs CASEN imputado")
lines(density(casen_pred$gasto_estimado_wins), col = "red", lwd = 2)
legend("topright", legend = c("EPF", "CASEN imputado"), col = c("blue", "red"), lwd = 2)

# --- UNIR PREDICCIONES ---

# Seleccionamos solo columnas necesarias de casen_pred
casen_pred_reduc <- casen_pred[, c("ID", "gasto_estimado")]

# Unir con sim_df
sim_df <- merge(sim_df, casen_pred_reduc, by = "ID", all.x = TRUE)

# Si se desea, reemplazar NA por 0 para quienes no incurren en gasto
sim_df$gasto_estimado[is.na(sim_df$gasto_estimado)] <- 0

# --- CONEXION POSTGRES Y MAPA ---

con <- dbConnect(
  Postgres(),
  dbname = "censo_rm_2017",
  host = "localhost",
  port = 5432,
  user = "postgres",
  password = "postgres"
)

# Escribir tabla temporal con audición y geocodigo
dbWriteTable(
  conn = con,
  name = Id(schema = "dpa", table = "tmp_chocolate_rm"),
  value = sim_df,
  overwrite = TRUE,
  row.names = FALSE
)

# Crear índice para acelerar joins
dbExecute(con, "CREATE INDEX ON dpa.tmp_chocolate_rm(zone)")
dbExecute(con, "ANALYZE dpa.tmp_chocolate_rm")

dbExecute(con, "
  CREATE TABLE IF NOT EXISTS dpa.zonas_chocolate AS
  SELECT
    z.*,
    t.gasto_estimado
  FROM dpa.zonas_censales_rm AS z
  LEFT JOIN dpa.tmp_chocolate_rm AS t
    ON z.geocodigo::text = t.zone
WHERE urbano = 1 AND (nom_provin = 'SANTIAGO' OR nom_comuna = 'SAN BERNARDO' OR nom_comuna = 'PUENTE ALTO')
")
library(sf)
zonas_chocolate_sf <- st_read(con, query = "
  SELECT * FROM dpa.zonas_chocolate
")

ggplot(zonas_chocolate_sf) +
  geom_sf(aes(fill = gasto_estimado), color = "black", size = 0.2) +
  scale_fill_gradient(low = "lightyellow", high = "red", na.value = "grey90",
                      name = "Problemas de Audición (%)") +
  theme_minimal() +
  labs(title = "Mapa de gasto en chocolatelate en la Región Metropolitana",
       subtitle = "Gasto en chocolate") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())


# --- GRAFICOS EXPLORATORIOS ---
# DISTRIBUCIÓN DEL INGRESO
hist(tabla_gasto$ing_pc, breaks = 30, col = "lightblue",
     main = "Distribución del Ingreso", xlab = "Ingreso per cápita")

# DISTRIBUCIÓN DEL GASTO EN chocolate
hist(tabla_gasto$gasto_chocolate, breaks = 30, col = "lightblue",
     main = "Distribución del Gasto en chocolate", xlab = "Gasto en chocolate")

# GASTO EN chocolate SEGÚN SEXO
boxplot(gasto_chocolate ~ factor(sexo), data = tabla_gasto,
        main = "Gasto en chocolate según Sexo", xlab = "Sexo",
        col = c("tomato", "lightgreen"))

# GASTO EN FUNCIÓN DE LA EDAD
plot(tabla_gasto$edad, tabla_gasto$gasto_chocolate,
     main = "Edad vs Gasto en chocolate", xlab = "Edad", ylab = "Gasto",
     pch = 20, col = rgb(0, 0, 0, 0.3))
lines(lowess(tabla_gasto$edad, tabla_gasto$gasto_chocolate), col = "red", lwd = 2)

# GASTO EN FUNCIÓN DEL INGRESO
plot(tabla_gasto$ing_pc, tabla_gasto$gasto_chocolate,
     main = "Ingreso vs Gasto en chocolate", xlab = "Ingreso per cápita", ylab = "Gasto",
     pch = 20, col = rgb(0, 0, 0, 0.3))
lines(lowess(tabla_gasto$ing_pc, tabla_gasto$gasto_chocolate), col = "blue", lwd = 2)

# BOXPLOT GASTO SEGÚN ESCOLARIDAD
boxplot(gasto_chocolate ~ grupo_escolaridad, data = tabla_gasto,
        main = "Gasto en chocolate según Escolaridad", xlab = "Escolaridad",
        col = "skyblue")