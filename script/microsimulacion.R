# 1. LIBRERÍAS
library(rakeR)
library(RPostgres)
library(DBI)
library(ggplot2)

# 2. ENTRADAS

## df del censo ya procesado
cons_censo_df <- readRDS("data/cons_censo_df.rds")
casen_raw = readRDS("data/casen_rm.rds") 

# 3. PREPROCESAMIENTO

## 3.1 CENSO
# Ordenar y extraer una sola vez los nombres de las columnas de constraints
col_cons   = sort(setdiff(names(cons_censo_df), c("GEOCODIGO","COMUNA")))

# De ahí generar dinámicamente los niveles que luego deben coincidir con los factor levels
age_levels  = grep("^edad", col_cons, value = TRUE)    # p.ej. "edad_menor_30", "edad_30_40", …
esc_levels  = grep("^esco", col_cons, value = TRUE)    # p.ej. "esco_0","esco_1_8",…
sexo_levels = grep("^sexo_",col_cons, value = TRUE)    # p.ej. "sexo_f","sexo_m"

## 3.2 CASEN
# Se seleccionan variables de interés

vars_base = c("estrato", # Para extraer la comuna
              "esc", # Para escolaridad
              "edad", 
              "sexo",
              "e6a",
              "h7b") # Variable a microsimular


# Se filtra la CASEN con las variables de interés
casen = casen_raw[ , vars_base, drop = FALSE]

# Se limpia memoria
rm(casen_raw)

# Extraemos la comuna
casen$Comuna = substr(as.character(casen$estrato), 1, 5)
casen$estrato = NULL

# Se quitan etiquetas haven y se cambian tipos de datos
casen$esc = as.integer(unclass(casen$esc))
casen$edad = as.integer(unclass(casen$edad))
casen$e6a = as.numeric(unclass(casen$e6a))
casen$sexo = as.integer(unclass(casen$sexo))
casen$h7b = as.numeric(unclass(casen$h7b)) # Se modifica para el trabajo
casen$h7b <- ifelse(casen$h7b == 1, 1, 0) # 1 Oyente saludable (h7b=1), 0 oyente con dificultades (h7b=2,3 o 4)

# Imputación lineal de esc en base a e6a
idx_na = which(is.na(casen$esc))

# Ajustar modelo con casos en donde no hay na's
fit = lm(esc ~ e6a, data = casen[-idx_na,])

# Predicción para los casos con NA
pred = predict(fit, newdata = casen[idx_na, ,drop = FALSE])

# Imputar acotada
casen$esc[idx_na] = as.integer(round(pmax(0, pmin(29, pred))))

# Le añadimos un ID fijo
casen$ID = as.character(seq_len(nrow(casen)))

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

## Microsimulación
# crear la lista de constraints POR COMUNA
cons_censo_comunas = split(cons_censo_df, cons_censo_df$COMUNA)

# Lista de INDS 
inds_list = split(casen, casen$Comuna)


sim_list = lapply(names(cons_censo_comunas), function(zona) {
  cons_i    = cons_censo_comunas[[zona]]
  col_order = sort(setdiff(names(cons_i), c("COMUNA","GEOCODIGO")))
  cons_i    = cons_i[, c("GEOCODIGO", col_order), drop = FALSE]
  
  tmp    = inds_list[[zona]]
  inds_i = tmp[, c("ID","edad_cat","esc_cat","sexo_cat"), drop = FALSE]
  names(inds_i) = c("ID","Edad","Escolaridad","Sexo")
  
  
  
  w_frac  = weight(cons = cons_i, inds = inds_i,
                   vars = c("Edad","Escolaridad","Sexo"))
  sim_i   = integerise(weights = w_frac, inds = inds_i, seed = 123)
  merge(sim_i,
        tmp[, c("ID","h7b")],
        by = "ID", all.x = TRUE)
})

# Data Frame de toda la población
sim_df = data.table::rbindlist(sim_list, idcol = "COMUNA")

zonas_h7b <- aggregate(
  h7b ~ zone,
  data = sim_df,
  FUN = function(x) {
    total = sum(!is.na(x))                # Total con datos válidos
    con_problema = sum(x == 0, na.rm = TRUE)  # Problemas de audición
    porcentaje = 100 * con_problema / total
    round(porcentaje, 2)                  # Redondear a 2 decimales
  }
)

# Renombrar columnas
names(zonas_h7b) <- c("geocodigo", "pct_problemas_audicion")

# source("trabajos/t2_microsim/R/conexion_db.R")

con <- dbConnect(
  Postgres(),
  dbname   = "censo_rm",
  host     = "localhost",
  port     = 5432,
  user     = "postgres",
  password = "postgres"
)

dbWriteTable(
  conn      = con,
  name      = Id(schema = "dpa", table = "tmp_audicion_rm"),
  value     = zonas_h7b,
  overwrite = TRUE,
  row.names = FALSE
)

dbExecute(con, "CREATE INDEX ON dpa.tmp_audicion_rm(geocodigo)")
dbExecute(con, "ANALYZE dpa.tmp_audicion_rm")

# 1) Crea la nueva capa directamente con un SELECT … LEFT JOIN
dbExecute(con, "
  CREATE TABLE dpa.zonas_censales_gs_income AS
  SELECT
    z.*,
    t.pct_problemas_audicion
  FROM dpa.zonas_censales_rm AS z
  LEFT JOIN dpa.tmp_audicion_rm AS t
    ON z.geocodigo::text = t.geocodigo
WHERE urbano = 1 AND (nom_provin = 'SANTIAGO' OR nom_comuna = 'SAN BERNARDO' OR nom_comuna = 'PUENTE ALTO')
")
