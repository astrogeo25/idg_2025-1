# 1. Librerías
library(RPostgres)
library(DBI)
library(ggplot2)
library(rakeR)
library(data.table) 

# Leer Censo
cons_censo_df <- readRDS("data/cons_censo_df.rds")

# 1) Ordenar y extraer una sola vez los nombres de las columnas de constraints
col_cons   <- sort(setdiff(names(cons_censo_df), c("GEOCODIGO","COMUNA")))

# 2) De ahí generar dinámicamente los niveles que luego deben coincidir con los factor levels
age_levels  <- grep("^edad", col_cons, value = TRUE)    # p.ej. "edad_menor_30", "edad_30_40", …
esc_levels  <- grep("^esco", col_cons, value = TRUE)    # p.ej. "esco_0","esco_1_8",…
sexo_levels <- grep("^sexo_",col_cons, value = TRUE)    # p.ej. "sexo_f","sexo_m"

# crear la lista de constraints POR COMUNA
cons_censo_comunas <- split(cons_censo_df, cons_censo_df$COMUNA)

# Leer CASEN y variables base
casen_raw <- readRDS("data/casen_rm.rds")
vars_base <- c("estrato",
               "esc",
               "edad",
               "educ",
               "sexo",
               "e6a",
               "ypc")

casen <- casen_raw[ , vars_base, drop = FALSE]
rm(casen_raw)  # liberamos memoria

# 3) Extraer COMUNA y descartar 'estrato'
casen$Comuna  <- substr(as.character(casen$estrato), 1, 5)
casen$estrato <- NULL

# 4) Quitar etiquetas haven_labelled y forzar atómicos
casen$esc  <- as.integer(unclass(casen$esc))
casen$edad  <- as.integer(unclass(casen$edad))
casen$educ <- as.integer(unclass(casen$educ))
casen$sexo <- as.integer(unclass(casen$sexo))
casen$e6a  <- as.numeric(unclass(casen$e6a))
casen$ypc <- as.numeric(unclass(casen$ypc))

# 5) Limpiar e imputar 'esc' (Años de escolaridad)
#    – Fuera de rango → NA
casen$esc[casen$esc < 0 | casen$esc > 29] <- NA

#    – Imputación lineal por e6a
imputar_escolaridad <- function(df) {
  idx_na <- which(is.na(df$esc))
  if (length(idx_na) == 0) return(df)
  fit   <- lm(esc ~ e6a, df[-idx_na, ], na.action = na.omit)
  pred  <- predict(fit, df[idx_na, , drop = FALSE])
  pred  <- pmax(0, pmin(29, pred))
  df$esc[idx_na] <- as.integer(round(pred))
  df
}
casen <- imputar_escolaridad(casen)

# 6) Añadir ID fijo
casen$ID <- as.character(seq_len(nrow(casen)))

# 7) Guardar snapshot base
casen_pob <- casen[ , c("ID","Comuna","edad","esc","sexo","e6a","ypc")]

# 3) Recodificar para rakeR
casen_pob$edad_cat <- cut(
  casen_pob$edad,
  breaks = c(0,30,40,50,60,70,80,Inf),
  labels = age_levels,
  right = FALSE, include.lowest = TRUE
)

casen_pob$esc_cat <- factor(
  with(casen_pob,
       ifelse(esc == 0,           esc_levels[1],
              ifelse(esc <= 8,    esc_levels[2],
                     ifelse(esc <= 12, esc_levels[3],
                            esc_levels[4])))),
  levels = esc_levels
)

casen_pob$sexo_cat <- factor(
  ifelse(casen_pob$sexo == 2, sexo_levels[1],  # 2→"sexo_f"
         ifelse(casen_pob$sexo == 1, sexo_levels[2], NA)), # 1→"sexo_m"
  levels = sexo_levels
)

inds_list <- split(casen_pob, casen_pob$Comuna)

# 4) Preparar lista de inds por comuna

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
  merge(sim_i,
        tmp[, c("ID","ypc")],
        by = "ID", all.x = TRUE)
})

sim_df <- data.table::rbindlist(sim_list, idcol = "COMUNA")