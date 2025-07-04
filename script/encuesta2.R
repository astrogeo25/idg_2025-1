# --- CARGA DE LIBRERÍAS ---
library(haven)
library(ggplot2)
library(pROC)
library(mgcv)

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
gasto_choco_por_persona <- aggregate(gasto ~ id_persona, data = cantidades_choco, sum)
names(gasto_choco_por_persona)[2] <- "gasto_choco"

# --- UNIR CON PERSONAS ---
personas_gs <- merge(personas_gs, gasto_choco_por_persona, by = "id_persona", all.x = TRUE)

# --- RELLENAR CON 0 QUIENES NO GASTARON ---
personas_gs$gasto_choco[is.na(personas_gs$gasto_choco)] <- 0

# --- SELECCIÓN DE VARIABLES FINALES ---
tabla_gastos <- personas_gs[, c("sexo", "edad", "edue", "ing_pc", "gasto_choco")]

tabla_gastos_con_consumo <- tabla_gastos[tabla_gastos$gasto_choco > 0, ]


# --- GRAFICOS EXPLORATORIOS ---
hist(tabla_gastos_con_consumo$ing_pc, breaks = 30, col = "lightblue", main = "Distribución del Ingreso", xlab = "Ingreso per cápita")
hist(tabla_gastos_con_consumo$gasto, breaks = 30, col = "lightblue", main = "Distribución del Gasto en producto", xlab = "Gasto en chocolates")

boxplot(gasto_choco ~ factor(sexo), data = tabla_gastos_con_consumo, main = "Gasto en Servicio según Sexo", xlab = "Sexo", col = c("tomato", "lightgreen"))

plot(tabla_gastos_con_consumo$edad, tabla_gastos_con_consumo$gasto_choco, main = "Edad vs Gasto", xlab = "Edad", ylab = "Gasto", pch = 20, col = rgb(0,0,0,0.3))
lines(lowess(tabla_gastos_con_consumo$edad, tabla_gastos_con_consumo$gasto_choco), col = "red", lwd = 2)

plot(tabla_gastos_con_consumo$ing_pc, tabla_gastos_con_consumo$gasto_choco, main = "Ingreso vs Gasto", xlab = "Ingreso per cápita", ylab = "Gasto", pch = 20, col = rgb(0,0,0,0.3))
lines(lowess(tabla_gastos_con_consumo$ing_pc, tabla_gastos_con_consumo$gasto_choco), col = "blue", lwd = 2)

# Escolaridad agrupada
tabla_gastos_con_consumo$grupo_escolaridad <- cut(tabla_gastos_con_consumo$edue, breaks = c(-Inf, 8, 12, 16, Inf), labels = c("Básica o menos", "Media-baja", "Media-alta", "Alta"), right = TRUE)
boxplot(gasto_choco ~ grupo_escolaridad, data = tabla_gastos_con_consumo, main = "Gasto según Escolaridad", xlab = "Escolaridad", col = "skyblue")