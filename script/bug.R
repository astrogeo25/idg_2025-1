# 1. LIBRERÍAS
library(rakeR)
library(RPostgres)
library(DBI)
library(ggplot2)
library(sf)
library(data.table)

# 2. ENTRADAS
cons_censo_df <- readRDS("data/cons_censo_df.rds")
casen_raw = readRDS("data/casen_rm.rds")

# 3. PREPROCESAMIENTO

## 3.1 CENSO
col_cons = sort(setdiff(names(cons_censo_df), c("GEOCODIGO","COMUNA")))
age_levels = grep("^edad", col_cons, value = TRUE)
esc_levels = grep("^esco", col_cons, value = TRUE)
sexo_levels = grep("^sexo_",col_cons, value = TRUE)

## 3.2 CASEN
vars_base = c("estrato", "esc", "edad", "sexo", "e6a", "h7b", "h11")
casen = casen_raw[ , vars_base, drop = FALSE]
rm(casen_raw)

casen$Comuna = substr(as.character(casen$estrato), 1, 5)
casen$estrato = NULL

casen$esc = as.integer(unclass(casen$esc))
casen$edad = as.integer(unclass(casen$edad))
casen$e6a = as.numeric(unclass(casen$e6a))
casen$sexo = as.integer(unclass(casen$sexo))
casen$h7b = ifelse(as.numeric(unclass(casen$h7b)) == 1, 1, 0)
casen$h11 = as.numeric(unclass(casen$h11))

# Ingreso per cápita
casen$ingreso_ypc = with(casen, ifelse(h11 > 0, e6a / h11, NA))

# Imputación lineal de esc
idx_na = which(is.na(casen$esc))
fit = lm(esc ~ e6a, data = casen[-idx_na,])
pred = predict(fit, newdata = casen[idx_na, ,drop = FALSE])
casen$esc[idx_na] = as.integer(round(pmax(0, pmin(29, pred))))
casen$ID = as.character(seq_len(nrow(casen)))

# Recodificación
casen$edad_cat <- cut(
  casen$edad,
  breaks = c(0,30,40,50,60,70,80,Inf),
  labels = age_levels,
  right = FALSE, include.lowest = TRUE
)
casen$esc_cat <- factor(
  with(casen,
       ifelse(esc == 0, esc_levels[1],
              ifelse(esc <= 8, esc_levels[2],
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
  cons_i = cons_censo_comunas[[zona]]
  col_order = sort(setdiff(names(cons_i), c("COMUNA","GEOCODIGO")))
  cons_i = cons_i[, c("GEOCODIGO", col_order), drop = FALSE]
  
  tmp = inds_list[[zona]]
  inds_i = tmp[, c("ID","edad_cat","esc_cat","sexo_cat"), drop = FALSE]
  names(inds_i) = c("ID","Edad","Escolaridad","Sexo")
  
  w_frac = weight(cons = cons_i, inds = inds_i, vars = c("Edad","Escolaridad","Sexo"))
  sim_i = integerise(weights = w_frac, inds = inds_i, seed = 123)
  merge(sim_i,
        tmp[, c("ID","h7b","ingreso_ypc")],
        by = "ID", all.x = TRUE)
})

sim_df = data.table::rbindlist(sim_list, idcol = "COMUNA")

# Agregados zonales
zonas_h7b <- aggregate(h7b ~ zone, data = sim_df, FUN = function(x) {
  total = sum(!is.na(x))
  con_problema = sum(x == 0, na.rm = TRUE)
  round(100 * con_problema / total, 2)
})
names(zonas_h7b) <- c("geocodigo", "audicion")

zonas_ypc <- aggregate(ingreso_ypc ~ zone, data = sim_df, FUN = function(x) round(mean(x, na.rm = TRUE), 0))
names(zonas_ypc) <- c("geocodigo", "ypc")

zonas_comb <- merge(zonas_h7b, zonas_ypc, by = "geocodigo")

# Conexión a PostgreSQL
con <- dbConnect(
  Postgres(),
  dbname = "censo_rm_2017",
  host = "localhost",
  port = 5432,
  user = "postgres",
  password = "postgres"
)

# Subida y combinación
dbWriteTable(conn = con,
             name = Id(schema = "dpa", table = "tmp_audicion_ingreso"),
             value = zonas_comb,
             overwrite = TRUE,
             row.names = FALSE)
dbExecute(con, "CREATE INDEX ON dpa.tmp_audicion_ingreso(geocodigo)")
dbExecute(con, "ANALYZE dpa.tmp_audicion_ingreso")

dbExecute(con, "
CREATE TABLE dpa.zonas_bivariado AS
SELECT
z.*,
t.audicion,
t.ypc
FROM dpa.zonas_censales_rm AS z
LEFT JOIN dpa.tmp_audicion_ingreso AS t
ON z.geocodigo::text = t.geocodigo
WHERE urbano = 1 AND (nom_provin = 'SANTIAGO' OR nom_comuna = 'SAN BERNARDO' OR nom_comuna = 'PUENTE ALTO')
")

# Mapa Bivariado
zonas_bivariado_sf <- st_read(con, query = "SELECT * FROM dpa.zonas_bivariado")

zonas_bivariado_sf$aud_cat <- cut(zonas_bivariado_sf$audicion,
                                  breaks = quantile(zonas_bivariado_sf$audicion, probs = seq(0, 1, length.out = 4), na.rm = TRUE),
                                  include.lowest = TRUE, labels = 1:3)

zonas_bivariado_sf$ypc_cat <- cut(zonas_bivariado_sf$ypc,
                                  breaks = quantile(zonas_bivariado_sf$ypc, probs = seq(0, 1, length.out = 4), na.rm = TRUE),
                                  include.lowest = TRUE, labels = 1:3)

zonas_bivariado_sf$bivar_class <- interaction(zonas_bivariado_sf$aud_cat, zonas_bivariado_sf$ypc_cat)

biv_colors <- c(
  "1.1" = "#e8e8e8", "2.1" = "#ace4e4", "3.1" = "#5ac8c8",
  "1.2" = "#dfb0d6", "2.2" = "#a5add3", "3.2" = "#5698b9",
  "1.3" = "#be64ac", "2.3" = "#8c62aa", "3.3" = "#3b4994"
)

ggplot(zonas_bivariado_sf) +
  geom_sf(aes(fill = bivar_class), color = "white", size = 0.2) +
  scale_fill_manual(values = biv_colors, na.value = "grey80", name = "Audición / Ingreso") +
  theme_minimal() +
  labs(
    title = "Mapa Bivariado: Problemas de Audición vs Ingreso per cápita",
    subtitle = "Zonas Censales, Región Metropolitana",
    caption = "Clasificación bivariada por terciles"
  ) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())
