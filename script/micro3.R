# 1. LIBRERÍAS
library(rakeR)
library(RPostgres)
library(DBI)
library(ggplot2)
library(dplyr)
library(sf)
library(data.table)

# 2. ENTRADAS

## df del censo ya procesado
cons_censo_df <- readRDS("data/cons_censo_df.rds")
casen_raw <- readRDS("data/casen_rm.rds") 

# 3. PREPROCESAMIENTO

## 3.1 CENSO
col_cons   <- sort(setdiff(names(cons_censo_df), c("GEOCODIGO","COMUNA")))
age_levels  <- grep("^edad", col_cons, value = TRUE)
esc_levels  <- grep("^esco", col_cons, value = TRUE)
sexo_levels <- grep("^sexo_",col_cons, value = TRUE)

## 3.2 CASEN
vars_base <- c("estrato", "esc", "edad", "sexo", "e6a", "h7a", "ypc")
casen <- casen_raw[, vars_base, drop = FALSE]
rm(casen_raw)

casen$Comuna <- substr(as.character(casen$estrato), 1, 5)
casen$estrato <- NULL

casen$esc <- as.integer(unclass(casen$esc))
casen$edad <- as.integer(unclass(casen$edad))
casen$e6a <- as.numeric(unclass(casen$e6a))
casen$sexo <- as.integer(unclass(casen$sexo))
casen$ypc <- as.integer(unclass(casen$ypc))
casen$h7a <- as.numeric(unclass(casen$h7a))
casen$h7a <- ifelse(casen$h7a == 1, 1, 0)  # 1 sano, 0 con problemas

idx_na <- which(is.na(casen$esc))
fit <- lm(esc ~ e6a, data = casen[-idx_na,])
pred <- predict(fit, newdata = casen[idx_na, ,drop = FALSE])
casen$esc[idx_na] <- as.integer(round(pmax(0, pmin(29, pred))))
casen$ID <- as.character(seq_len(nrow(casen)))

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
cons_censo_comunas <- split(cons_censo_df, cons_censo_df$COMUNA)
inds_list <- split(casen, casen$Comuna)

sim_list <- lapply(names(cons_censo_comunas), function(zona) {
  cons_i    <- cons_censo_comunas[[zona]]
  col_order <- sort(setdiff(names(cons_i), c("COMUNA","GEOCODIGO")))
  cons_i    <- cons_i[, c("GEOCODIGO", col_order), drop = FALSE]
  
  tmp    <- inds_list[[zona]]
  inds_i <- tmp[, c("ID","edad_cat","esc_cat","sexo_cat"), drop = FALSE]
  names(inds_i) <- c("ID","Edad","Escolaridad","Sexo")
  
  w_frac  <- weight(cons = cons_i, inds = inds_i,
                    vars = c("Edad","Escolaridad","Sexo"))
  sim_i   <- integerise(weights = w_frac, inds = inds_i, seed = 123)
  
  merge(
    sim_i,
    tmp[, c("ID", "h7a", "ypc")],  # Se agrega ypc aquí para ingreso
    by = "ID", all.x = TRUE
  )
})

sim_df <- data.table::rbindlist(sim_list, idcol = "COMUNA")

# Calcular porcentaje con problemas de visión (h7a == 0) por zona
z_h7a <- aggregate(
  h7a ~ COMUNA,
  data = sim_df,
  FUN = function(x) {
    total <- sum(!is.na(x))
    con_problema <- sum(x == 0, na.rm = TRUE)
    porcentaje <- 100 * con_problema / total
    round(porcentaje, 2)
  }
)

# Calcular promedio de ingreso (ypc) por zona
z_ypc <- aggregate(
  ypc ~ COMUNA,
  data = sim_df,
  FUN = function(x) round(mean(x, na.rm = TRUE), 2)
)

# Unir ambos resultados en una sola tabla
zonas_h7a <- merge(z_ypc, z_h7a, by = "COMUNA")

# Renombrar columnas para mayor claridad y consistencia con geocodigo
names(zonas_h7a) <- c("geocodigo", "mediana_ingreso", "pct_problemas_vision")

# 5. Conexión con Postgres
con <- dbConnect(
  Postgres(),
  dbname = "censo_rm_2017",
  host = "localhost",
  port = 5432,
  user = "postgres",
  password = "postgres"
)

# Escribir tabla temporal con resultados
dbWriteTable(
  conn = con,
  name = Id(schema = "dpa", table = "tmp_vision_rm"),
  value = zonas_h7a,
  overwrite = TRUE,
  row.names = FALSE
)

# Crear índice y analizar tabla para optimizar consultas
dbExecute(con, "CREATE INDEX ON dpa.tmp_vision_rm(geocodigo)")
dbExecute(con, "ANALYZE dpa.tmp_vision_rm")

# Crear tabla final uniendo con zonas censales y filtrando zonas urbanas relevantes
dbExecute(con, "
  DROP TABLE IF EXISTS dpa.zonas_vision;
  
  CREATE TABLE dpa.zonas_vision AS
  SELECT
    z.*,
    t.mediana_ingreso,
    t.pct_problemas_vision AS vision
  FROM dpa.zonas_censales_rm AS z
  LEFT JOIN dpa.tmp_vision_rm AS t
    ON z.geocodigo::text = t.geocodigo
  WHERE urbano = 1 
    AND (nom_provin = 'SANTIAGO' OR nom_comuna = 'SAN BERNARDO' OR nom_comuna = 'PUENTE ALTO')
")

# Leer tabla espacial desde Postgres
zonas_vision_sf <- st_read(con, query = "SELECT * FROM dpa.zonas_vision")

# Cerrar conexión
dbDisconnect(con)

# 6. Crear mapas bivariados con ggplot2 y sf

# Normalizar variables para bivariado
zonas_vision_sf <- zonas_vision_sf %>%
  mutate(
    ingreso_norm = (mediana_ingreso - min(mediana_ingreso, na.rm=TRUE)) / 
      (max(mediana_ingreso, na.rm=TRUE) - min(mediana_ingreso, na.rm=TRUE)),
    vision_norm = (vision - min(vision, na.rm=TRUE)) / 
      (max(vision, na.rm=TRUE) - min(vision, na.rm=TRUE))
  )

# Crear índice bivariado simple (suma de normalizados)
zonas_vision_sf <- zonas_vision_sf %>%
  mutate(bivar_index = ingreso_norm + vision_norm)

# Visualización del mapa bivariado
ggplot(zonas_vision_sf) +
  geom_sf(aes(fill = bivar_index), color = NA) +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red", 
    midpoint = median(zonas_vision_sf$bivar_index, na.rm=TRUE),
    name = "Ingreso y Problemas visión"
  ) +
  theme_minimal() +
  labs(
    title = "Mapa bivariado: Promedio de ingreso y porcentaje con problemas de visión",
    subtitle = "Zonas urbanas de Santiago, San Bernardo y Puente Alto"
  )
