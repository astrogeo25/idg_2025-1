library(haven)
library(dplyr)
# Leer archivos Stata
personas <- read_dta("data/datos_epf/base-personas-ix-epf-stata.dta")
gastos   <- read_dta("data/datos_epf/base-gastos-ix-epf-stata.dta")
cantidades <- read_dta("data/datos_epf/base-cantidades-ix-epf-stata.dta")
ccif     <- read_dta("data/datos_epf/ccif-ix-epf-stata.dta")

# Filtro para trabajar solo el Gran Santiago
personas_gs = personas[personas$macrozona == 2, ]

# Filtro para valores inválidos.
valores_invalidos <- c(-99, -88, -77)

# Edad y escolaridad
personas_gs = personas_gs[!(personas_gs$edad %in% valores_invalidos), ]
personas_gs = personas_gs[!(personas_gs$edue %in% valores_invalidos), ]
personas_gs = personas_gs[!(personas_gs$ing_disp_hog_hd_ai < 0), ]

# Se calcula el ingreso per cápita
personas_gs$ing_pc = personas_gs$ing_disp_hog_hd_ai / personas_gs$npersonas

# Filtrar en base de cantidades en función del servicio
cantidades_servicio = cantidades[
  cantidades$g == "1" &
    cantidades$c == "8" &
    cantidades$sc == "05" &
    cantidades$p == "01",] #NOTA: La tabla será nula, pués el servicio no se mide en unidades como el producto

# calculo de gasto
gastos_servicio = gastos[gastos$ccif == "01.1.8.05.01" & macrozona == 2] #folio


#Agrupa persona repetidas en un mismo folio en la variable personas_gs. pOSTERIOR agerga edad, edue e ing_pc a cantidades_servicio conectando por una elda union de folio + n_linea