install.packages("rakeR")
library(rakeR)

# Leer datos
cons_censo_df <- readRDS("data/cons_censo_df.rds")
casen_raw = readRDS("data/casen_rm.rds")

# Cada registro de la case representa 1 perosna. ID, escolaridad, edad, sexo

# Ordenar y extraer una sola vez los nombres de las columnas de constraints
col_cons   = sort(setdiff(names(cons_censo_df), c("GEOCODIGO","COMUNA")))

# De ahí generar dinámicamente los niveles que luego deben coincidir con los factor levels
age_levels  <- grep("^edad", col_cons, value = TRUE)    # p.ej. "edad_menor_30", "edad_30_40", …
esc_levels  <- grep("^esco", col_cons, value = TRUE)    # p.ej. "esco_0","esco_1_8",…
sexo_levels <- grep("^sexo_",col_cons, value = TRUE)    # p.ej. "sexo_f","sexo_m"
age_levels = c(
  "edad_menor_30",
  setdiff(age_levels, "edad_menor_30")
)

# Sleccionar variables.. Se deben elimnar N.A

vars_base = c("estrato", # para extraer ID de comuna
              "esc", # Escolaridad
              "edad",
              "sexo",
              "e6a", # Imputar escolaridad
              "ypc") # Var a micro simular

# Flitrar CASEN
casen = casen_raw[ , vars_base, drop = FALSE]
rm(casen_raw) # Eliminar data sin utilizar

# Extraer comuna
casen$Comuna = substr(as.character(casen$estrato), 1, 5)
casen$estrato = NULL

# Se quitan etiquetas (transformar de heyven a dtaframe normal)
casen$e6a = as.integer(unclass(casen$e6a))
casen$ypc = as.integer(unclass(casen$ypc))
casen$Comuna = as.integer(unclass(casen$Comuna))
casen$sexo = as.integer(unclass(casen$sexo))
casen$edad = as.integer(unclass(casen$edad))
casen$esc = as.integer(unclass(casen$esc))

# Imputación lineal de esc en base a e6a (deben tener alta correlación)
cor(casen$esc, casen$e6a, use = "complete.obs")
idx_na = which(is.na(casen$esc))# Reconocer en que puntos hay N.A

# Ajustar modelo con casos donde no hay NA
fit = lm(esc ~ e6a, data = casen[-idx_na, ])
summary(fit) #R2 0,84; aceptable

# Prediccion
pred = predict(fit, newdata = casen[idx_na, ,drop = FALSE])

# Imput acotada
casen$esc[idx_na] = as.integer(round(pmax(0, pmin(29, pred))))

# Añadir ID fijo
casen$ID = nrow(casen)

## Recodificamos 

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

