# 1. Librerías
# Para simular
library(rakeR)
library(RPostgres)
library(DBI)
library(ggplot2)
# Para generar mapas bivariados
library(biscale)
library(cowplot)
library(sf)

# 2. Entradas

## df del censo ya procesado
cons_censo_df <- readRDS("data/cons_censo_df.rds")
casen_raw = readRDS("data/casen_rm.rds") 

# 3. Preprocesamiento

## 3.1 CENSO
col_cons   = sort(setdiff(names(cons_censo_df), c("GEOCODIGO","COMUNA")))
age_levels  = grep("^edad", col_cons, value = TRUE)
esc_levels  = grep("^esco", col_cons, value = TRUE)
sexo_levels = grep("^sexo_",col_cons, value = TRUE)

## 3.2 CASEN
vars_base = c("estrato", "esc", "edad", "sexo", "e6a", "h7a", "ypc")
casen = casen_raw[ , vars_base, drop = FALSE]
rm(casen_raw)

casen$Comuna = substr(as.character(casen$estrato), 1, 5)
casen$estrato = NULL

casen$esc = as.integer(unclass(casen$esc))
casen$edad = as.integer(unclass(casen$edad))
casen$e6a = as.numeric(unclass(casen$e6a))
casen$sexo = as.integer(unclass(casen$sexo))
casen$ypc = as.integer(unclass(casen$ypc))
casen$h7a = as.numeric(unclass(casen$h7a))
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

# 4. Microsimulación
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
        tmp[, c("ID","h7a","edad")],  # ahora incluye edad
        by = "ID", all.x = TRUE)
})

sim_df = data.table::rbindlist(sim_list, idcol = "COMUNA")

# 4.1 Cálculo de métricas

zonas_stats <- aggregate(
  cbind(h7a, edad) ~ zone,
  data = sim_df,
  FUN = function(x) x
)

zonas_stats <- within(zonas_stats, {
  pct_problemas_vision = round(100 * sapply(h7a, function(x) sum(x == 0, na.rm = TRUE) / length(x)), 2)
  pct_mayores_60 = round(100 * sapply(edad, function(x) sum(x >= 60, na.rm = TRUE) / length(x)), 2)
})

zonas_stats_df <- data.frame(
  geocodigo = zonas_stats$zone,
  pct_problemas_vision = zonas_stats$pct_problemas_vision,
  pct_mayores_60 = zonas_stats$pct_mayores_60
)

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
  name = Id(schema = "dpa", table = "tmp_vision_vejez_rm"),
  value = zonas_stats_df,
  overwrite = TRUE,
  row.names = FALSE
)

dbExecute(con, "CREATE INDEX ON dpa.tmp_vision_vejez_rm(geocodigo)")
dbExecute(con, "ANALYZE dpa.tmp_vision_vejez_rm")

dbExecute(con, "
  CREATE TABLE dpa.zonas_vision_vejez AS
  SELECT
    z.*,
    t.pct_problemas_vision AS vision,
    t.pct_mayores_60 AS vejez
  FROM dpa.zonas_censales_rm AS z
  LEFT JOIN dpa.tmp_vision_vejez_rm AS t
    ON z.geocodigo::text = t.geocodigo
  WHERE urbano = 1 AND (nom_provin = 'SANTIAGO' OR nom_comuna = 'SAN BERNARDO' OR nom_comuna = 'PUENTE ALTO')
")

zonas_vision_sf <- st_read(con, query = "
  SELECT * FROM dpa.zonas_vision_vejez
")

# 6. Generar mapa bivariado

# 6.1 Preparar los datos con biscale para crear clases bivariadas
# Usamos las variables pct_mayores_60 (vejez) y pct_problemas_vision (visión)
zonas_vision_sf <- bi_class(
  zonas_vision_sf,
  x = vejez,
  y = vision,
  style = "quantile", # o "equal" según prefieras
  dim = 3             # número de clases por variable (3x3 = 9 clases)
)

# 6.2 Crear el mapa con ggplot2 usando la clasificación bivariada
map <- ggplot() +
  geom_sf(data = zonas_vision_sf, aes(fill = bi_class), color = "white", size = 0.1) +
  bi_scale_fill(pal = "DkBlue", dim = 3) +  # paleta de colores bivariados
  labs(
    title = "Mapa bivariado: Vejez y Problemas de Visión",
    subtitle = "Región Metropolitana",
    fill = "Vejez / Problemas de Visión"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

# 6.3 Crear la leyenda bivariada con biscale
legend <- bi_legend(pal = "DkBlue",
                    dim = 3,
                    xlab = "Vejez",
                    ylab = "Problemas de visión",
                    size = 8)

# 6.4 Combinar mapa y leyenda con cowplot
final_plot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.75, 0.1, 0.2, 0.2)

# Mostrar el gráfico final
print(final_plot)