# 1. Cargar librerías necesarias
library(ggplot2)
library(dplyr)
library(rakeR)

# 2. Cargar los datos CASEN procesados previamente
cons_censo_df <- readRDS("data/cons_censo_df.rds")
casen_raw <- readRDS("data/casen_rm.rds")

# 3. Variables necesarias
vars_base <- c("estrato", "esc", "edad", "sexo", "e6a", "ypc")
casen <- casen_raw[, vars_base, drop = FALSE]
rm(casen_raw)

# 4. Procesamiento básico
casen$Comuna <- substr(as.character(casen$estrato), 1, 5)
casen$estrato <- NULL

casen$esc <- as.integer(unclass(casen$esc))
casen$edad <- as.integer(unclass(casen$edad))
casen$e6a <- as.numeric(unclass(casen$e6a))
casen$sexo <- as.integer(unclass(casen$sexo))
casen$ypc <- as.integer(unclass(casen$ypc))

# 6. Categorías auxiliares
sexo_levels <- c("Mujer", "Hombre")
esc_levels <- c("Sin instrucción", "Básica", "Media", "Superior")
age_levels <- c("0-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

casen$edad_cat <- cut(
  casen$edad,
  breaks = c(0, 30, 40, 50, 60, 70, 80, Inf),
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

# 7. Filtrar comuna de Lampa (código 13302)
lampa <- casen %>% filter(Comuna == "13302")

total_habitantes <- nrow(lampa)
cat("Total de habitantes en Lampa:", total_habitantes, "\n")

# 8. Gráfico de dona: Distribución por sexo
sexo_count <- lampa %>%
  count(sexo_cat) %>%
  mutate(pct = n / sum(n) * 100,
         label = paste0(sexo_cat, ": ", round(pct, 1), "%"))

ggplot(sexo_count, aes(x = "", y = pct, fill = sexo_cat)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
  labs(title = "Distribución por sexo en la comuna de Lampa", fill = "Sexo") +
  theme_void()

# 9. Histograma de edades
ggplot(lampa, aes(x = edad)) +
  geom_histogram(binwidth = 5, fill = "#EFC000FF", color = "black") +
  labs(
    title = "Distribución de edades en la comuna de Lampa",
    x = "Edad",
    y = "Frecuencia"
  ) +
  theme_minimal()

# 10. Edad por nivel educativo
ggplot(lampa, aes(x = edad, fill = esc_cat)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribución de edad por nivel educativo",
       x = "Edad",
       fill = "Nivel educativo") +
  theme_minimal()

# 11. Dispersión YPC vs edad por sexo
ggplot(lampa, aes(x = edad, y = ypc, color = sexo_cat)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE) +
  scale_color_manual(values = c("Mujer" = "#EFC000FF", "Hombre" = "#0073C2FF")) +
  labs(
    title = "Relación entre edad e ingreso per cápita (YPC) en Lampa",
    x = "Edad",
    y = "Ingreso per cápita (YPC)",
    color = "Sexo"
  ) +
  theme_minimal()