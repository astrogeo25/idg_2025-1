# --- CARGA DE LIBRERÍAS ---
library(haven)
library(pROC)
library(mgcv)
library(ggplot2)
library(corrplot)

# --- CARGA DE DATOS ---
personas <- read_dta("data/datos_epf/base-personas-ix-epf-stata.dta")
gastos   <- read_dta("data/datos_epf/base-gastos-ix-epf-stata.dta")
cantidades <- read_dta("data/datos_epf/base-cantidades-ix-epf-stata.dta")
ccif     <- read_dta("data/datos_epf/ccif-ix-epf-stata.dta")

# --- FILTRO GRAN SANTIAGO ---
personas_gs = subset(personas, macrozona == 2 & sprincipal == 1)

valores_invalidos <- c(-99, -88, -77)

personas_gs = subset(personas_gs, !(edad %in% valores_invalidos) &
                       !(edue %in% valores_invalidos) &
                       ing_disp_hog_hd_ai >= 0)

personas_gs$ing_pc = personas_gs$ing_disp_hog_hd_ai / personas_gs$npersonas


# --- CREAR ID DE PERSONA: folio + n_linea ---
personas_gs$id_persona <- paste(personas_gs$folio, personas_gs$n_linea, sep = "_")
cantidades$id_persona <- paste(cantidades$folio, cantidades$n_linea, sep = "_")

cantidades_choco <- subset(cantidades,
                           ccif == "01.1.8.05.01" &
                             macrozona == 2)

# --- SUMAR GASTO TOTAL POR PERSONA ---
gasto_ch_por_persona <- aggregate(gasto ~ id_persona, data = cantidades_choco, sum)
names(gasto_ch_por_persona)[2] <- "gasto_ch"

# --- UNIR CON PERSONAS ---
personas_gs <- merge(personas_gs, gasto_ch_por_persona, by = "id_persona", all.x = TRUE)

# --- RELLENAR CON 0 QUIENES NO GASTARON ---
personas_gs$gasto_ch[is.na(personas_gs$gasto_ch)] <- 0

# --- SELECCIÓN DE VARIABLES FINALES ---
tabla_og <- personas_gs[, c("sexo", "edad", "edue", "ing_pc", "gasto_ch")]
tabla_gasto <- tabla_og[tabla_og$gasto_ch > 0, ] # Quitar gastos 0
rm(tabla_og)

# --- GRAFICOS EXPLORATORIOS ---
# Variables: ing_pc, gasto, sexo
hist(tabla_gasto$ing_pc, breaks = 30, col = "lightblue", main = "Distribución del Ingreso", xlab = "Ingreso per cápita")
hist(tabla_gasto$gasto, breaks = 30, col = "lightblue", main = "Distribución del Gasto en producto", xlab = "Gasto en chocolates")

boxplot(gasto_ch ~ factor(sexo), data = tabla_gasto, main = "Gasto en Servicio según Sexo", xlab = "Sexo", col = c("tomato", "lightgreen"))

plot(tabla_gasto$edad, tabla_gasto$gasto_ch, main = "Edad vs Gasto", xlab = "Edad", ylab = "Gasto", pch = 20, col = rgb(0,0,0,0.3))
lines(lowess(tabla_gasto$edad, tabla_gasto$gasto_ch), col = "red", lwd = 2)

plot(tabla_gasto$ing_pc, tabla_gasto$gasto_ch, main = "Ingreso vs Gasto", xlab = "Ingreso per cápita", ylab = "Gasto", pch = 20, col = rgb(0,0,0,0.3))
lines(lowess(tabla_gasto$ing_pc, tabla_gasto$gasto_ch), col = "blue", lwd = 2)

# Escolaridad agrupada
tabla_gasto$grupo_escolaridad <- cut(tabla_gasto$edue, breaks = c(-Inf, 8, 12, 16, Inf), labels = c("Básica o menos", "Media-baja", "Media-alta", "Alta"), right = TRUE)

# Boxplot según grupo de escolaridad
boxplot(gasto_ch ~ grupo_escolaridad, data = tabla_gasto, main = "Gasto según Escolaridad", xlab = "Escolaridad", col = "skyblue")


# --- DETECCIÓN Y ELIMINACIÓN DE OUTLIERS (basado en IQR) ---
limpiar_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lim_inf <- Q1 - 1.5 * IQR
  lim_sup <- Q3 + 1.5 * IQR
  return(x >= lim_inf & x <= lim_sup)
}

# Aplicar limpieza a cada variable numérica
filtros <- with(tabla_gasto, 
                limpiar_outliers(edad) &
                  limpiar_outliers(edue) &
                  limpiar_outliers(ing_pc) &
                  limpiar_outliers(gasto_ch)
)

# Crear nuevo dataframe limpio
tabla_limpia <- tabla_gasto[filtros, ]

# Matriz correlación
# Creación de data frame y matriz de correlación
corr <- data.frame(df$gasto_ch, df$edad, df$edue, df$ing_pc, df$sexo)
correlation_matrix <- cor(corr, use = "complete.obs", method = "pearson")
#Gráfica
corrplot(correlation_matrix, method = "color", tl.cex = 0.8, number.cex = 0.7)

# Regresión lineal de quienes si gastan

modelo_lineal <- lm(df$gasto_ch ~ df$edue + df$ing_pc + df$sexo, data = df)
summary(modelo_lineal) # Resumen de metricas

# --- VARIABLE BINARIA: incurre o no en gasto en chocolates ---
personas_gs$incurre_gasto <- ifelse(personas_gs$gasto_ch > 0, 1, 0)

# --- CREAR ESCOLARIDAD AGRUPADA ---
personas_gs$grupo_escolaridad <- cut(
  personas_gs$edue,
  breaks = c(-Inf, 8, 12, 16, Inf),
  labels = c("Básica o menos", "Media-baja", "Media-alta", "Alta"),
  right = TRUE
)

# --- ELIMINAR VALORES NA DE VARIABLES RELEVANTES ---
modelo_data <- subset(personas_gs,
                      !is.na(edad) & !is.na(grupo_escolaridad) & !is.na(sexo))

# --- AJUSTAR MODELO LOGIT ---
modelo_logit <- glm(incurre_gasto ~ factor(sexo) + edad + grupo_escolaridad,
                    data = modelo_data,
                    family = binomial)
summary(modelo_logit)
