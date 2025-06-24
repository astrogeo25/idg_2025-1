library(dplyr)
library(haven)

# Leer archivos Stata
personas <- read_dta("data/datos_epf/base-personas-ix-epf-stata.dta")
gastos   <- read_dta("data/datos_epf/base-gastos-ix-epf-stata.dta")
cantidades <- read_dta("data/datos_epf/base-cantidades-ix-epf-stata.dta")
ccif     <- read_dta("data/datos_epf/ccif-ix-epf-stata.dta")

# Filtro para trabajar solo el Gran Santiago
personas_gs <- personas[personas$macrozona == 2, ]

# Filtro para valores inválidos
valores_invalidos <- c(-99, -88, -77)

# Filtro de datos inválidos
personas_gs <- personas_gs[!(personas_gs$edad %in% valores_invalidos), ]
personas_gs <- personas_gs[!(personas_gs$edue %in% valores_invalidos), ]
personas_gs <- personas_gs[!(personas_gs$ing_disp_hog_hd_ai < 0), ]

# Se calcula el ingreso per cápita
personas_gs$ing_pc <- personas_gs$ing_disp_hog_hd_ai / personas_gs$npersonas

# Agrupar por folio y n_linea, y agregar las variables edad, edue e ing_pc
personas_gs_grouped <- personas_gs %>%
  group_by(folio, n_linea) %>%
  summarize(
    edad = mean(edad, na.rm = TRUE),        # Promedio de edad
    edue = mean(edue, na.rm = TRUE),        # Promedio de escolaridad
    ing_pc = mean(ing_pc, na.rm = TRUE),    # Promedio de ingreso per cápita
    .groups = 'drop'                        # Desagrupa después de la operación
  )

# Filtro en base de cantidades en función del servicio
cantidades_servicio <- cantidades %>%
  filter(
    g == "1" &
      c == "8" &
      sc == "05" &
      p == "01"
  )

# Realizar la unión entre las tablas
resultado_final <- cantidades_servicio %>%
  left_join(personas_gs_grouped, by = c("folio", "n_linea"))

# Ver los primeros registros del resultado
head(resultado_final)
