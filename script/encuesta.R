library(haven)
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
  cantidades$g == "3" &
    cantidades$c == "2" &
    cantidades$sc == "01" &
    cantidades$p == "01",] #NOTA: La tabla será nula, pués el servicio no se mide en unidades como el producto

# calculo de gasto
gastos_servicio = gastos[gastos$ccif == "04.3.2.01.01",]

# Sumar gasto total en el servicio/producto por hogar
