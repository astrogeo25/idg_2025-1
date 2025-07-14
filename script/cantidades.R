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

# --- FILTRO CANTIDADES DE CHOCOLATES (Gran Santiago) ---
cantidades_choco <- subset(cantidades, ccif == "01.1.8.05.01" & macrozona == 2)

# --- SUMA CANTIDAD TOTAL DE CHOCOLATES POR PERSONA ---
cantidad_ch_por_persona <- aggregate(cantidad ~ id_persona, data = cantidades_choco, sum)
names(cantidad_ch_por_persona)[2] <- "cantidad_ch"

# --- MERGE: Cantidades con personas ---
personas_gs <- merge(personas_gs, cantidad_ch_por_persona, by = "id_persona", all.x = TRUE)
personas_gs$cantidad_ch[is.na(personas_gs$cantidad_ch)] <- 0

# --- VARIABLE BINARIA DE CANTIDAD ---
personas_gs$incurre_cantidad <- ifelse(personas_gs$cantidad_ch > 0, 1, 0)

# --- AGRUPACIÓN ESCOLARIDAD ---
personas_gs$grupo_escolaridad <- cut(
  personas_gs$edue,
  breaks = c(-Inf, 8, 12, 16, Inf),
  labels = c("Básica o menos", "Media-baja", "Media-alta", "Alta"),
  right = TRUE
)

# --- BASE PARA MODELO CONTINUO (solo quienes tienen cantidades) ---
tabla_cantidad <- subset(personas_gs, cantidad_ch > 0)
tabla_cantidad <- tabla_cantidad[, c("sexo", "edad", "edue", "ing_pc", "cantidad_ch", "grupo_escolaridad")]

# --- MODELO LINEAL: Quienes incurren en cantidad ---
modelo_lineal <- lm(log(cantidad_ch) ~ edue + ing_pc + sexo, data = tabla_cantidad)
summary(modelo_lineal)

# --- FILTRAR 99% CANTIDADES
q99 <- quantile(tabla_cantidad$cantidad_ch, 0.99)
tabla_filtrada <- subset(tabla_cantidad, cantidad_ch <= q99)

modelo_lineal_f <- lm(log(cantidad_ch) ~ edue + ing_pc, data = tabla_filtrada)
summary(modelo_lineal_f)

# --- ANALISIS ANOVA
# --- ANÁLISIS ANOVA Y GRÁFICOS DE RESIDUOS ---

# 1. Modelo lineal original (quienes incurren en cantidad)
cat("\n--- ANOVA: Modelo Lineal original ---\n")
anova(modelo_lineal)
par(mfrow = c(1, 2))
plot(modelo_lineal)

# 2. Modelo lineal con filtrado del 99% superior de la cantidad
cat("\n--- ANOVA: Modelo Lineal filtrado 99% ---\n")
anova(modelo_lineal_f)
par(mfrow = c(1, 2))
plot(modelo_lineal_f)