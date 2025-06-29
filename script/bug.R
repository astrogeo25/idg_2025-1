# 1. Librerías
# Para simular
library(RPostgres) # Conectar postgres
library(DBI)
library(ggplot2)
library(sf) # Leer selecciones postgres
# Kmeans
library(factoextra)
library(GGally) # Multiples gráficos
library(rakeR)

# 2. Entradas

## df del censo ya procesado
cons_censo_df <- readRDS("data/cons_censo_df.rds")
casen_raw = readRDS("data/casen_rm.rds") 

# 3. Preprocesamiento

## 3.1 Extracción de variables del Censo
col_cons   = sort(setdiff(names(cons_censo_df), c("GEOCODIGO","COMUNA")))
age_levels  = grep("^edad", col_cons, value = TRUE)
esc_levels  = grep("^esco", col_cons, value = TRUE)
sexo_levels = grep("^sexo_",col_cons, value = TRUE)

## 3.2 Extracción de variables de la CASEN
vars_base = c("estrato", "esc", "edad", "sexo", "e6a", "h7a", "ypc")
casen = casen_raw[ , vars_base, drop = FALSE]
rm(casen_raw)

casen$Comuna = substr(as.character(casen$estrato), 1, 5)
casen$estrato = NULL

# Definir según tipo la variable. Se usa numericas para calculos en R
casen$esc = as.integer(unclass(casen$esc))
casen$edad = as.numeric(unclass(casen$edad))
casen$e6a = as.numeric(unclass(casen$e6a))
casen$sexo = as.integer(unclass(casen$sexo))
casen$ypc = as.numeric(unclass(casen$ypc))
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
        tmp[, c("ID","h7a","edad", "ypc")],  # ahora incluye edad e ypc
        by = "ID", all.x = TRUE)
})

sim_df = data.table::rbindlist(sim_list, idcol = "COMUNA")

# 4.1 Cálculo de métricas

zonas_stats <- aggregate(
  cbind(h7a, edad, ypc) ~ zone, # Incluir variables micro simular
  data = sim_df,
  FUN = function(x) x
)

# Calcular indices mediante function(x) y elimnar valores vacíos
zonas_stats <- within(zonas_stats, {
  tasa_vision = round(100 * sapply(h7a, function(x) sum(x == 0, na.rm = TRUE) / length(x)), 2)
  tasa_mayores = round(100 * sapply(edad, function(x) sum(x >= 60, na.rm = TRUE) / length(x)), 2)
  ingreso = round(sapply(ypc, function(x) mean(x, na.rm = TRUE)), 2)
})
# Agregar a un data frame
zonas_stats_df <- data.frame(
  geocodigo = zonas_stats$zone,
  tasa_vision = zonas_stats$tasa_vision,
  tasa_mayores = zonas_stats$tasa_mayores,
  ingreso = zonas_stats$ingreso
)

# 5. Kmeans
# Escalar variables de estudio
vars_scaled <- scale(zonas_stats_df[, c("tasa_vision", "tasa_mayores", "ingreso")])

# 5.1 Método del codo: Visualiza la suma de cuadrados dentro del cluster (WSS) para varios K
fviz_nbclust(vars_scaled, kmeans, method = "wss") + # Datos, algoritmo, método de selección
  labs(title = "Método del codo", x = "Número de clusters (K)", y = "WSS")

# 5.2 Calcular kmeans
set.seed(123)  # Para reproducibilidad
km = kmeans(vars_scaled, centers = 3, nstart = 25) # Introducir k determinado (3)
zonas_stats_df$cluster = as.factor(km$cluster) # Agregar número de cluster a la tabla

# 6. Mapa de correlaciones
library(GGally)
# Eliminar columna 'geocodigo' (si está presente en el data frame)
df_plot <- zonas_stats_df[, c("tasa_vision", "tasa_mayores", "ingreso", "cluster")]
# Asegurarse de que 'cluster' sea un factor
df_plot$cluster <- as.factor(df_plot$cluster)

# Crear el gráfico de correlaciones
p = ggpairs(
  df_plot,
  columns = 1:3,  # Variables numéricas (sin 'cluster')
  mapping = aes(color = cluster),  # Colorear por cluster
  upper = list(continuous = "points"),  # Gráfico de puntos en la parte superior
  lower = list(continuous = "points"),  # Gráfico de puntos en la parte inferior
  diag  = list(continuous = "densityDiag")  # Gráfico de densidad en la diagonal
)

# 7. Conexión con Postgres
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

dbExecute(con, "DROP TABLE IF EXISTS dpa.zonas_vision_vejez")

dbExecute(con, "
  CREATE TABLE dpa.zonas_vision_vejez AS
  SELECT
    z.*,
    t.tasa_vision AS vision,
    t.tasa_mayores AS vejez,
    t.ingreso AS ingreso,
    t.cluster
  FROM dpa.zonas_censales_rm AS z
  LEFT JOIN dpa.tmp_vision_vejez_rm AS t
    ON z.geocodigo::text = t.geocodigo
  WHERE urbano = 1
    AND (nom_provin = 'SANTIAGO' OR nom_comuna = 'SAN BERNARDO' OR nom_comuna = 'PUENTE ALTO')
")

zonas_vision_sf <- st_read(con, query = "
  SELECT * FROM dpa.zonas_vision_vejez
")

# 8. Visualizar mapa de clusters

library(sf)
library(ggplot2)
library(RPostgres)
library(DBI)
library(RColorBrewer)

# Leer geometría + atributos directamente desde la tabla final
sql_mapa <- "
SELECT *
FROM dpa.zonas_vision_vejez
WHERE urbano = 1
  AND (nom_provin = 'SANTIAGO' OR nom_comuna IN ('SAN BERNARDO', 'PUENTE ALTO'));
"

sf_mapa <- st_read(con, query = sql_mapa)

# Leer geometría de comunas para contexto
sql_comunas <- "
SELECT cut, nom_comuna, geom
FROM dpa.comunas_rm_shp
WHERE nom_provin = 'SANTIAGO';
"
sf_comunas <- st_read(con, query = sql_comunas)

# Bounding box para acotar visualización
bbox <- st_bbox(sf_mapa)

# Crear mapa de clusters
mapa_clusters <- ggplot() +
  geom_sf(data = sf_mapa, aes(fill = as.factor(cluster)), color = NA) +
  geom_sf(data = sf_comunas, fill = NA, color = "black", size = 0.4) +
  geom_sf_text(data = st_centroid(sf_comunas), aes(label = nom_comuna), size = 2, fontface = "bold") +
  scale_fill_brewer(palette = "Set2", name = "Cluster") +
  labs(
    title = "Mapa de Clusters de Zonas Censales",
    subtitle = "Provincia de Santiago, Región Metropolitana"
  ) +
  coord_sf(
    xlim = c(bbox["xmin"], bbox["xmax"]),
    ylim = c(bbox["ymin"], bbox["ymax"]),
    expand = FALSE
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

print(mapa_clusters)

# (Opcional) Exportar como GeoJSON
# st_write(sf_mapa, "zonas_clusters.geojson", driver = "GeoJSON", delete_dsn = TRUE)

# Mostrar el gráfico
print(p)

# 9 Generar indices de Shannon

library(tibble)
library(dplyr)
library(tidyr)
library(vegan)
library(ggplot2)

# Crear la tabla con el conteo de clusters por comuna
tabla_shannon <- sf_mapa |>
  st_drop_geometry() |>
  count(nom_comuna, cluster) |>
  filter(!is.na(nom_comuna)) |>
  tidyr::pivot_wider(names_from = cluster, values_from = n, values_fill = 0) |>
  column_to_rownames("nom_comuna")

# Calcular el índice de Shannon para cada comuna
shannon <- diversity(tabla_shannon, index = "shannon")

# Crear un data frame con el índice de Shannon por comuna
df_shannon <- data.frame(
  nom_comuna = names(shannon),
  shannon_index = shannon
)

# Unir los resultados con la geometría de las comunas
sf_comunas_shannon <- merge(sf_comunas, df_shannon, by = "nom_comuna")

# Crear el gráfico del índice de Shannon por comuna
ggplot(sf_comunas_shannon) +
  geom_sf(aes(fill = shannon_index), color = "white") +
  scale_fill_gradient(name = "Índice de Shannon") +
  labs(
    title = "Variabilidad interna de clusters por comuna",
    subtitle = "Índice de diversidad (mayor = más heterogénea)"
  ) +
  theme_minimal()