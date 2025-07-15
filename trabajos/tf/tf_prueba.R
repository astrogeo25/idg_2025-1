# ==============================
# CARGA DE LIBRERÍAS
# ==============================
library(haven)
library(pROC)
library(mgcv)
library(ggplot2)
library(corrplot)
library(data.table)
library(RPostgres)
library(DBI)
library(sf)

# ==============================
# CARGA DE DATOS EPF
# ==============================
personas   <- read_dta("data/datos_epf/base-personas-ix-epf-stata.dta")
gastos     <- read_dta("data/datos_epf/base-gastos-ix-epf-stata.dta")
cantidades <- read_dta("data/datos_epf/base-cantidades-ix-epf-stata.dta")
ccif       <- read_dta("data/datos_epf/ccif-ix-epf-stata.dta")
rm(gastos, ccif)  # no se utilizan en el resto del script

# ==============================
# FILTRADO A GRAN SANTIAGO Y VALORES VÁLIDOS
# ==============================
valores_invalidos <- c(-99, -88, -77)
personas_gs <- subset(personas,
                      macrozona == 2 &
                        !(edad %in% valores_invalidos) &
                        !(edue %in% valores_invalidos) &
                        ing_disp_hog_hd_ai >= 0)
rm(personas)

# ==============================
# CREACIÓN DE VARIABLES DERIVADAS
# ==============================
personas_gs$ing_pc <- personas_gs$ing_disp_hog_hd_ai / personas_gs$npersonas
personas_gs$id_persona <- paste(personas_gs$folio, personas_gs$n_linea, sep = "_")
cantidades$id_persona  <- paste(cantidades$folio, cantidades$n_linea, sep = "_")

# ==============================
# FILTRADO DE GASTO EN CHOCOLATE
# ==============================
cantidades_chocolate <- subset(cantidades, ccif == "02.1.2.01.01" & macrozona == 2)
rm(cantidades)

# ==============================
# CÁLCULO DEL GASTO TOTAL EN CHOCOLATE POR PERSONA
# ==============================
gasto_chocolate_por_persona <- aggregate(gasto ~ id_persona, data = cantidades_chocolate, sum)
names(gasto_chocolate_por_persona)[2] <- "gasto_chocolate"
rm(cantidades_chocolate)

# ==============================
# UNIÓN DE GASTO A LA BASE PERSONAS
# ==============================
personas_gs <- merge(personas_gs, gasto_chocolate_por_persona, by = "id_persona", all.x = TRUE)
personas_gs$gasto_chocolate[is.na(personas_gs$gasto_chocolate)] <- 0
rm(gasto_chocolate_por_persona)

# ==============================
# CREACIÓN DE VARIABLES ADICIONALES
# ==============================
personas_gs$incurre_gasto <- ifelse(personas_gs$gasto_chocolate > 0, 1, 0)

personas_gs$grupo_escolaridad <- cut(personas_gs$edue,
                                     breaks = c(-Inf, 12, 14, 16, Inf),
                                     labels = c("Escolar", "Tecnico", "Universitaria", "Postgrado"))

# ==============================
# BASE PARA MODELO CONTINUO (SOLO QUIENES GASTAN)
# ==============================
tabla_gasto <- subset(personas_gs, gasto_chocolate > 0)
tabla_gasto <- tabla_gasto[, c("sexo", "edad", "edue", "ing_pc", "gasto_chocolate", "grupo_escolaridad")]

# ==============================
# TRANSFORMACIONES DE VARIABLES
# ==============================
tabla_gasto$sexo <- factor(tabla_gasto$sexo, labels = c("Hombre", "Mujer"))
tabla_gasto$log_ing_pc <- log(tabla_gasto$ing_pc)
tabla_gasto$log_gasto_chocolate <- log(tabla_gasto$gasto_chocolate + 1)

tabla_gasto$rango_edad <- cut(tabla_gasto$edad,
                              breaks = c(0, 29, 44, 64, Inf),
                              labels = c("jovenes", "adultos_jovenes", "adultos", "adultos_mayores"))

# ==============================
# MODELO LINEAL DEL GASTO (SOLO QUIENES GASTAN)
# ==============================
modelo_lineal <- lm(log_gasto_chocolate ~ grupo_escolaridad + ing_pc + rango_edad + factor(sexo),
                    data = tabla_gasto)
summary(modelo_lineal)

# ==============================
# MODELO LOGÍSTICO: INCURRIR EN GASTO
# ==============================
modelo_data <- subset(personas_gs,
                      !is.na(edad) & !is.na(grupo_escolaridad) & !is.na(sexo))

modelo_logit <- glm(incurre_gasto ~ factor(sexo) + edad + grupo_escolaridad + ing_pc,
                    data = modelo_data,
                    family = binomial)

modelo_data$prob_predicha <- predict(modelo_logit, type = "response")

# ==============================
# CURVA ROC Y UMBRAL ÓPTIMO
# ==============================
roc_obj <- roc(modelo_data$incurre_gasto, modelo_data$prob_predicha)
coords_opt <- coords(roc_obj, "best", ret = c("threshold", "sensitivity", "specificity"))
umbral_optimo <- as.numeric(coords_opt["threshold"])

cat("Umbral óptimo:", umbral_optimo, "\n")
cat("Sensibilidad óptima:", coords_opt["sensitivity"][[1]], "\n")
cat("Especificidad óptima:", coords_opt["specificity"][[1]], "\n")

# ==============================
# EVALUACIÓN DEL MODELO LOGÍSTICO CON UMBRAL ÓPTIMO
# ==============================
modelo_data$clasificacion_optima <- ifelse(modelo_data$prob_predicha >= umbral_optimo, 1, 0)

conf_opt <- table(Real = modelo_data$incurre_gasto, Predicha = modelo_data$clasificacion_optima)
print(conf_opt)

accuracy_opt <- mean(modelo_data$incurre_gasto == modelo_data$clasificacion_optima)
cat("Accuracy (óptimo):", accuracy_opt, "\n")

TN_opt <- conf_opt["0", "0"]
FP_opt <- conf_opt["0", "1"]
TP_opt <- conf_opt["1", "1"]
FN_opt <- conf_opt["1", "0"]

especificidad_opt <- TN_opt / (TN_opt + FP_opt)
sensibilidad_opt <- TP_opt / (TP_opt + FN_opt)

cat("Especificidad (umbral óptimo):", especificidad_opt, "\n")
cat("Sensibilidad (umbral óptimo):", sensibilidad_opt, "\n")

# ==============================
# GRAFICOS EXPLORATORIOS
# ==============================
hist(tabla_gasto$ing_pc, breaks = 30, col = "lightblue",
     main = "Distribución del Ingreso", xlab = "Ingreso per cápita")

hist(tabla_gasto$gasto_chocolate, breaks = 30, col = "lightblue",
     main = "Distribución del Gasto en chocolate", xlab = "Gasto en chocolate")

boxplot(gasto_chocolate ~ factor(sexo), data = tabla_gasto,
        main = "Gasto en chocolate según Sexo", xlab = "Sexo",
        col = c("tomato", "lightgreen"))

plot(tabla_gasto$edad, tabla_gasto$gasto_chocolate,
     main = "Edad vs Gasto en chocolate", xlab = "Edad", ylab = "Gasto",
     pch = 20, col = rgb(0, 0, 0, 0.3))
lines(lowess(tabla_gasto$edad, tabla_gasto$gasto_chocolate), col = "red", lwd = 2)

plot(tabla_gasto$ing_pc, tabla_gasto$gasto_chocolate,
     main = "Ingreso vs Gasto en chocolate", xlab = "Ingreso per cápita", ylab = "Gasto",
     pch = 20, col = rgb(0, 0, 0, 0.3))
lines(lowess(tabla_gasto$ing_pc, tabla_gasto$gasto_chocolate), col = "blue", lwd = 2)

boxplot(gasto_chocolate ~ grupo_escolaridad, data = tabla_gasto,
        main = "Gasto en chocolate según Escolaridad", xlab = "Escolaridad",
        col = "skyblue")
# ==============================
# CARGA Y PREPROCESAMIENTO: CENSO Y CASEN
# ==============================

# --- carga de archivos procesados ---
cons_censo_df <- readRDS("data/cons_censo_df.rds")
casen_raw     <- readRDS("data/casen_rm.rds")

# --- columnas del censo relevantes ---
col_cons     <- sort(setdiff(names(cons_censo_df), c("GEOCODIGO", "COMUNA")))
age_levels   <- grep("^edad", col_cons, value = TRUE)
esc_levels   <- grep("^esco", col_cons, value = TRUE)
sexo_levels  <- grep("^sexo_", col_cons, value = TRUE)

# --- selección de variables de interés en casen ---
vars_base <- c("estrato", "esc", "edad", "sexo", "e6a", "ypc")
casen <- casen_raw[, vars_base, drop = FALSE]
rm(casen_raw)

# ==============================
# TRATAMIENTO DE VARIABLES CASEN
# ==============================

casen$Comuna <- substr(as.character(casen$estrato), 1, 5)
casen$estrato <- NULL

# convertir a numérico o factor según corresponda
casen$esc   <- as.integer(unclass(casen$esc))
casen$edad  <- as.numeric(unclass(casen$edad))
casen$e6a   <- as.numeric(unclass(casen$e6a))
casen$sexo  <- as.integer(unclass(casen$sexo))
casen$ypc   <- as.numeric(unclass(casen$ypc))

# ==============================
# IMPUTACIÓN DE ESCOLARIDAD
# ==============================
idx_na <- which(is.na(casen$esc))
fit <- lm(esc ~ e6a, data = casen[-idx_na, ])
pred <- predict(fit, newdata = casen[idx_na, , drop = FALSE])
casen$esc[idx_na] <- as.integer(round(pmax(0, pmin(29, pred))))
rm(fit, pred, idx_na)

# ==============================
# CATEGORIZACIÓN DE VARIABLES
# ==============================

# id único
casen$ID <- as.character(seq_len(nrow(casen)))

# edad categórica
casen$edad_cat <- cut(
  casen$edad,
  breaks = c(0,30,40,50,60,70,80,Inf),
  labels = age_levels,
  right = FALSE, include.lowest = TRUE
)

# escolaridad categórica
casen$esc_cat <- factor(
  with(casen,
       ifelse(esc == 0,           esc_levels[1],
              ifelse(esc <= 8,           esc_levels[2],
                     ifelse(esc <= 12,          esc_levels[3],
                            esc_levels[4])))),
  levels = esc_levels
)

# sexo categórico
casen$sexo_cat <- factor(
  ifelse(casen$sexo == 2, sexo_levels[1],
         ifelse(casen$sexo == 1, sexo_levels[2], NA)),
  levels = sexo_levels
)

# ==============================
# MICROSIMULACIÓN POR COMUNA
# ==============================

cons_censo_comunas <- split(cons_censo_df, cons_censo_df$COMUNA)
inds_list <- split(casen, casen$Comuna)

sim_list <- lapply(names(cons_censo_comunas), function(zona) {
  cons_i <- cons_censo_comunas[[zona]]
  col_order <- sort(setdiff(names(cons_i), c("COMUNA", "GEOCODIGO")))
  cons_i <- cons_i[, c("GEOCODIGO", col_order), drop = FALSE]
  
  tmp <- inds_list[[zona]]
  inds_i <- tmp[, c("ID", "edad_cat", "esc_cat", "sexo_cat"), drop = FALSE]
  names(inds_i) <- c("ID", "Edad", "Escolaridad", "Sexo")
  
  w_frac <- weight(cons = cons_i, inds = inds_i,
                   vars = c("Edad", "Escolaridad", "Sexo"))
  sim_i <- integerise(weights = w_frac, inds = inds_i, seed = 123)
  
  merge(sim_i,
        tmp[, c("ID", "ypc")],
        by = "ID", all.x = TRUE)
})

sim_df <- data.table::rbindlist(sim_list, idcol = "COMUNA")
rm(sim_list, cons_censo_comunas, inds_list)

# ==============================
# PREDICCIÓN DE GASTO EN CASEN
# ==============================

# asegurar que niveles coincidan con entrenamiento
casen$sexo <- factor(as.character(casen$sexo), levels = c("1", "2"),
                     labels = c("Hombre", "Mujer"))

casen$grupo_escolaridad <- cut(
  casen$esc,
  breaks = c(-Inf, 12, 14, 16, Inf),
  labels = c("Escolar", "Tecnico", "Universitaria", "Postgrado")
)

casen$rango_edad <- cut(casen$edad,
                        breaks = c(0, 29, 44, 64, Inf),
                        labels = c("jovenes", "adultos_jovenes", "adultos", "adultos_mayores"))

casen$ing_pc <- casen$ypc
casen <- casen[!is.na(casen$ing_pc), ]

# clasificación binaria según modelo logístico
casen$prob_predicha <- predict(modelo_logit, newdata = casen, type = "response")
casen$clasificacion <- ifelse(casen$prob_predicha >= umbral_optimo, 1, 0)

# predicción continua del gasto (solo quienes clasifican con gasto)
casen_pred <- casen[casen$clasificacion == 1, ]
casen_pred$log_gasto_estimado <- predict(modelo_lineal, newdata = casen_pred)
casen_pred$gasto_estimado <- exp(casen_pred$log_gasto_estimado) - 1

# winzorización para controlar outliers
casen_pred$gasto_estimado_wins <- pmin(casen_pred$gasto_estimado,
                                       quantile(casen_pred$gasto_estimado, 0.999))

# resumen
summary(tabla_gasto$gasto_chocolate)
summary(casen_pred$gasto_estimado_wins)

# comparación de desviaciones estándar
sd(tabla_gasto$gasto_chocolate)
sd(casen_pred$gasto_estimado_wins)

# comparación visual
plot(density(tabla_gasto$gasto_chocolate), col = "blue", lwd = 2, main = "Densidad: EPF vs CASEN imputado")
lines(density(casen_pred$gasto_estimado_wins), col = "red", lwd = 2)
legend("topright", legend = c("EPF", "CASEN imputado"), col = c("blue", "red"), lwd = 2)

# ==============================
# UNIÓN DE PREDICCIONES A sim_df
# ==============================
casen_pred_reduc <- casen_pred[, c("ID", "gasto_estimado")]
sim_df <- merge(sim_df, casen_pred_reduc, by = "ID", all.x = TRUE)
sim_df$gasto_estimado[is.na(sim_df$gasto_estimado)] <- 0

# ==============================
# EXPORTACIÓN A BASE DE DATOS POSTGRESQL
# ==============================
con <- dbConnect(
  Postgres(),
  dbname = "censo_rm_2017",
  host = "localhost",
  port = 5432,
  user = "postgres",
  password = "postgres"
)

dbWriteTable(
  conn = con,
  name = Id(schema = "dpa", table = "tmp_chocolate_rm"),
  value = sim_df,
  overwrite = TRUE,
  row.names = FALSE
)

dbExecute(con, "CREATE INDEX ON dpa.tmp_chocolate_rm(zone)")
dbExecute(con, "ANALYZE dpa.tmp_chocolate_rm")

# ==============================
# CREACIÓN DE TABLA CON JOIN A ZONAS
# ==============================
dbExecute(con, "
  CREATE TABLE IF NOT EXISTS dpa.zonas_chocolate AS
  SELECT
    z.*,
    t.gasto_estimado
  FROM dpa.zonas_censales_rm AS z
  LEFT JOIN dpa.tmp_chocolate_rm AS t
    ON z.geocodigo::text = t.zone
  WHERE urbano = 1 AND (nom_provin = 'SANTIAGO' OR nom_comuna IN ('SAN BERNARDO', 'PUENTE ALTO'))
")

# ==============================
# MAPA DE GASTO EN CHOCOLATE
# ==============================
zonas_chocolate_sf <- st_read(con, query = "
  SELECT * FROM dpa.zonas_chocolate
  WHERE gasto_estimado > 0
")

ggplot(zonas_chocolate_sf) +
  geom_sf(aes(fill = gasto_estimado), color = NA) +
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "Gasto estimado en chocolate - Gran Santiago",
       fill = "Gasto estimado") +
  theme_minimal()
