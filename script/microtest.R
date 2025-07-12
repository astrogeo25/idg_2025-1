# 1. Librerías
library(RPostgres)
library(DBI)
library(ggplot2)
library(rakeR)
library(data.table) 

# 2. Entradas
cons_censo_df <- readRDS("data/cons_censo_df.rds")
casen <- as.data.table(readRDS("data/casen_rm.rds")[, c("estrato", "esc", "edad", "sexo", "e6a", "ypc")])

# 3. Preprocesamiento
## 3.1 Variables del Censo
col_cons <- sort(setdiff(names(cons_censo_df), c("GEOCODIGO", "COMUNA")))
age_levels  <- grep("^edad", col_cons, value = TRUE)
esc_levels  <- grep("^esco", col_cons, value = TRUE)
sexo_levels <- grep("^sexo_", col_cons, value = TRUE)

## 3.2 Procesamiento de CASEN
casen[, Comuna := substr(as.character(estrato), 1, 5)][, estrato := NULL]

# Conversión eficiente de tipos
casen[, `:=`(
  esc = as.integer(esc),
  edad = as.numeric(edad),
  e6a = as.numeric(e6a),
  sexo = as.integer(sexo),
  ypc = as.numeric(ypc)
)]

# Imputación de ESC
idx_na <- which(is.na(casen$esc))
if (length(idx_na) > 0) {
  fit <- lm(esc ~ e6a, data = casen[-idx_na])
  casen$esc[idx_na] <- pmin(29, pmax(0, round(predict(fit, newdata = casen[idx_na]))))
}
casen[, ID := as.character(.I)]

# Categorías
casen[, edad_cat := cut(edad, breaks = c(0,30,40,50,60,70,80,Inf), labels = age_levels, right = FALSE)]
casen[, esc_cat := factor(fcase(
  esc == 0, esc_levels[1],
  esc <= 8, esc_levels[2],
  esc <= 12, esc_levels[3],
  default = esc_levels[4]
), levels = esc_levels)]

casen[, sexo_cat := factor(fcase(
  sexo == 2, sexo_levels[1],
  sexo == 1, sexo_levels[2]
), levels = sexo_levels)]

# 4. Microsimulación
cons_list <- split(cons_censo_df, cons_censo_df$COMUNA)
inds_list <- split(casen, casen$Comuna)

sim_list <- lapply(names(cons_list), function(zona) {
  cons_i <- cons_list[[zona]]
  tmp <- inds_list[[zona]]
  if (is.null(tmp) || nrow(tmp) == 0) return(NULL)
  
  cons_i <- cons_i[, c("GEOCODIGO", sort(setdiff(names(cons_i), c("COMUNA", "GEOCODIGO")))), drop = FALSE]
  inds_i <- tmp[, .(ID, Edad = edad_cat, Escolaridad = esc_cat, Sexo = sexo_cat)]
  
  w_frac <- weight(cons = cons_i, inds = inds_i, vars = c("Edad", "Escolaridad", "Sexo"))
  sim_i <- integerise(weights = w_frac, inds = inds_i, seed = 123)
  
  merge(sim_i, tmp[, .(ID, ypc)], by = "ID", all.x = TRUE) # Agregar variables simuladas
})

sim_df <- rbindlist(sim_list, idcol = "COMUNA", use.names = TRUE, fill = TRUE)