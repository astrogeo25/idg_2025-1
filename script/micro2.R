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
col_cons   = sort(setdiff(names(cons_censo_df), c("GEOCODIGO","COMUNA")))
age_levels  = grep("^edad", col_cons, value = TRUE)
esc_levels  = grep("^esco", col_cons, value = TRUE)
sexo_levels = grep("^sexo_",col_cons, value = TRUE)

## 3.2 CASEN
vars_base = c("estrato", "esc", "edad", "sexo", "e6a", "h7a") # Cambiado h7b → h7a
casen = casen_raw[ , vars_base, drop = FALSE]
rm(casen_raw)

casen$Comuna = substr(as.character(casen$estrato), 1, 5)
casen$estrato = NULL

casen$esc = as.integer(unclass(casen$esc))
casen$edad = as.integer(unclass(casen$edad))
casen$e6a = as.numeric(unclass(casen$e6a))
casen$sexo = as.integer(unclass(casen$sexo))
casen$h7a = as.numeric(unclass(casen$h7a)) # Nuevo: visión
casen$h7a <- ifelse(casen$h7a == 1, 1, 0)  # 1 sano, 0 con problemas

idx_na = which(is.na(casen$esc))
fit = lm(esc ~ e6a, data = casen[-idx_na,])
pred = predict(fit, newdata = casen[idx_na, ,drop = FALSE])
casen$esc[idx_na] = as.integer(round(pmax(0, pmin(29, pred))))
casen$ID = as.character(seq_len(nrow(casen)))

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

# Microsimulación
cons_censo_comunas = split(cons_censo_df, cons_censo_df$COMUNA)
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
        tmp[, c("ID","h7a")],  # h7a en vez de h7b
        by = "ID", all.x = TRUE)
})

sim_df = data.table::rbindlist(sim_list, idcol = "COMUNA")

zonas_h7a <- aggregate(
  h7a ~ zone,
  data = sim_df,
  FUN = function(x) {
    total = sum(!is.na(x))
    con_problema = sum(x == 0, na.rm = TRUE)
    porcentaje = 100 * con_problema / total
    round(porcentaje, 2)
  }
)

names(zonas_h7a) <- c("geocodigo", "pct_problemas_vision")

# 5. Conexión con Postgres
con <- dbConnect(
  Postgres(),
  dbname = "censo_rm_2017",
  host = "localhost",
  port = 5432,
  user = "postgres",
  password = "postgres"
)

dbWriteTable(
  conn = con,
  name = Id(schema = "dpa", table = "tmp_vision_rm"),
  value = zonas_h7a,
  overwrite = TRUE,
  row.names = FALSE
)

dbExecute(con, "CREATE INDEX ON dpa.tmp_vision_rm(geocodigo)")
dbExecute(con, "ANALYZE dpa.tmp_vision_rm")

dbExecute(con, "
  CREATE TABLE dpa.zonas_vision AS
  SELECT
    z.*,
    t.pct_problemas_vision AS vision
  FROM dpa.zonas_censales_rm AS z
  LEFT JOIN dpa.tmp_vision_rm AS t
    ON z.geocodigo::text = t.geocodigo
  WHERE urbano = 1 AND (nom_provin = 'SANTIAGO' OR nom_comuna = 'SAN BERNARDO' OR nom_comuna = 'PUENTE ALTO')
")

zonas_vision_sf <- st_read(con, query = "
  SELECT * FROM dpa.zonas_vision
")

# 6. Generar mapa
ggplot(zonas_vision_sf) +
  geom_sf(aes(fill = vision), color = "black", size = 0.2) +
  scale_fill_gradient(low = "lightyellow", high = "darkblue", na.value = "grey90",
                      name = "Problemas de Visión (%)") +
  theme_minimal() +
  labs(title = "Mapa de Problemas de Visión en la Región Metropolitana",
       subtitle = "Porcentaje de personas con dificultades visuales por zona censal") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())
