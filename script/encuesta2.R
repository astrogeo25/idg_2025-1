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
  macrozona == 2 & sprincipal == 1 &
    !(edad %in% valores_invalidos) &
    !(edue %in% valores_invalidos) &
    ing_disp_hog_hd_ai >= 0
)

# --- VARIABLES DERIVADAS ---
personas_gs$ing_pc <- personas_gs$ing_disp_hog_hd_ai / personas_gs$npersonas
personas_gs$id_persona <- paste(personas_gs$folio, personas_gs$n_linea, sep = "_")
cantidades$id_persona  <- paste(cantidades$folio, cantidades$n_linea, sep = "_")

# --- FILTRO GASTO EN CHOCOLATES (Gran Santiago) ---
cantidades_choco <- subset(cantidades, ccif == "01.1.8.05.01" & macrozona == 2)

# --- SUMA GASTO TOTAL EN CHOCOLATES POR PERSONA ---
gasto_ch_por_persona <- aggregate(gasto ~ id_persona, data = cantidades_choco, sum)
names(gasto_ch_por_persona)[2] <- "gasto_ch"

# --- MERGE: Gasto con personas ---
personas_gs <- merge(personas_gs, gasto_ch_por_persona, by = "id_persona", all.x = TRUE)
personas_gs$gasto_ch[is.na(personas_gs$gasto_ch)] <- 0

# --- VARIABLE BINARIA DE GASTO ---
personas_gs$incurre_gasto <- ifelse(personas_gs$gasto_ch > 0, 1, 0)

# --- AGRUPACIÓN ESCOLARIDAD ---
personas_gs$grupo_escolaridad <- cut(
  personas_gs$edue,
  breaks = c(-Inf, 8, 12, 16, Inf),
  labels = c("Básica o menos", "Media-baja", "Media-alta", "Alta"),
  right = TRUE
)

# --- BASE PARA MODELO CONTINUO (solo quienes gastan) ---
tabla_gasto <- subset(personas_gs, gasto_ch > 0)
tabla_gasto <- tabla_gasto[, c("sexo", "edad", "edue", "ing_pc", "gasto_ch", "grupo_escolaridad")]

# --- GRAFICOS EXPLORATORIOS ---
# DISTRIBUCIÓN DEL INGRESO
hist(tabla_gasto$ing_pc, breaks = 30, col = "lightblue",
     main = "Distribución del Ingreso", xlab = "Ingreso per cápita")

# DISTRIBUCIÓN DEL GASTO EN EL PRODUCTO
hist(tabla_gasto$gasto_ch, breaks = 30, col = "lightblue",
     main = "Distribución del Gasto en Chocolates", xlab = "Gasto en chocolates")

# GASTO SEGÚN SEXO
boxplot(gasto_ch ~ factor(sexo), data = tabla_gasto,
        main = "Gasto en Chocolates según Sexo", xlab = "Sexo",
        col = c("tomato", "lightgreen"))

# GASTO EN FUNCIÓN DE LA EDAD
plot(tabla_gasto$edad, tabla_gasto$gasto_ch,
     main = "Edad vs Gasto", xlab = "Edad", ylab = "Gasto",
     pch = 20, col = rgb(0, 0, 0, 0.3))
lines(lowess(tabla_gasto$edad, tabla_gasto$gasto_ch), col = "red", lwd = 2)

# GASTO EN FUNCIÓN DEL INGRESO
plot(tabla_gasto$ing_pc, tabla_gasto$gasto_ch,
     main = "Ingreso vs Gasto", xlab = "Ingreso per cápita", ylab = "Gasto",
     pch = 20, col = rgb(0, 0, 0, 0.3))
lines(lowess(tabla_gasto$ing_pc, tabla_gasto$gasto_ch), col = "blue", lwd = 2)

# BOPLOT GASTO SEGÚN ESCOLARIDAD
boxplot(gasto_ch ~ grupo_escolaridad, data = tabla_gasto,
        main = "Gasto según Escolaridad", xlab = "Escolaridad",
        col = "skyblue")

# --- MODELO LINEAL: Quienes incurren en gasto ---
modelo_lineal <- lm(gasto_ch ~ edue + ing_pc + sexo, data = tabla_gasto)
summary(modelo_lineal)

# --- MODELO LOGÍSTICO: Probabilidad de incurrir en gasto ---
modelo_data <- subset(personas_gs, !is.na(edad) & !is.na(grupo_escolaridad) & !is.na(sexo))
modelo_logit <- glm(incurre_gasto ~ factor(sexo) + edad + grupo_escolaridad,
                    data = modelo_data, family = binomial)
summary(modelo_logit)

# --- MODELO GAM
library(mgcv)
modelo_gam <- gam(log_gasto ~ s(ing_pc) + s(edad) + sexo + grupo_escolaridad, data = tabla_gasto)
summary(modelo_gam)
plot(modelo_gam, se = TRUE)

# --- SEGMENTAR GASTO
modelo_hombres <- lm(log_gasto ~ edue + ing_pc, data = subset(tabla_gasto, sexo == 1))
modelo_mujeres <- lm(log_gasto ~ edue + ing_pc, data = subset(tabla_gasto, sexo == 2))

summary(modelo_hombres)
summary(modelo_mujeres)