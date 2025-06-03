# install.packages(c("factoextra", "ggfortify"))
# Cargar librerías necesarias
if (!require("plotly")) install.packages("plotly", dependencies = TRUE)
library(plotly)
library(factoextra)
library(ggfortify)

# Leer el dataset
# Proviene de https://www.kaggle.com/datasets/vjchoudhary7/customer-segmentation-tutorial-in-python?resource=download
mall = read.csv("data/Mall_Customers.csv")

# Renombrar columnas para simplificar
colnames(mall)[c(3,4,5)] = c("Age", "Income", "Score")

# Extraer las columnas relevantes
mall_data = mall[, c("Age", "Income", "Score")]

# Escalamos las variables numéricas para evitar que alguna domine por su magnitud
mall_scaled = scale(mall_data)

# Limpiar memoria
rm(mall_data)

# Visualiza la suma de cuadrados dentro del cluster (WSS) para varios K
fviz_nbclust(mall_scaled, kmeans, method = "wss") + # datos, algoritmo, metodo de selección
  labs(title = "Método del codo", x = "Número de clusters (K)", y = "WSS")

set.seed(123)  # para reproducibilidad
km = kmeans(mall_scaled, centers = 4, nstart = 25) # 4 es el K optimo

# Añadir los clusters al dataframe original
mall$cluster = as.factor(km$cluster)

# Mostrar resumen estadístico para cada cluster
for (k in levels(mall$cluster)) {
  cat("\nCluster", k, "\n")
  grupo = mall[mall$cluster == k, ]
  print(summary(grupo[, c("Age", "Income", "Score")]))
  cat("n =", nrow(grupo), "\n")
}

# Crear gráfico 3D
fig <- plot_ly(
  data = mall,
  x = ~Age,
  y = ~Income,
  z = ~Score,
  color = ~as.factor(cluster),  # Asegurarse de que sea un factor para colorear por grupo
  colors = c("red", "green", "blue", "purple"), # puedes ajustar esto
  type = "scatter3d",
  mode = "markers",
  marker = list(size = 4)
)

# Mostrar el gráfico
fig

# Ingreso vs Score
ggplot(mall, aes(x = Income, y = Score, color = cluster)) +
  geom_point(size = 2) +
  labs(title = "¿Qué tipo de clientes hay aquí?",
       x = "Ingreso Anual (k$)",
       y = "Nivel de Gasto (Spending Score)") +
  theme_minimal()

# Edad vs Score
ggplot(mall, aes(x = Age, y = Score, color = cluster)) +
  geom_point(size = 2) +
  labs(title = "¿Cómo varía el gasto según la edad?",
       x = "Edad",
       y = "Nivel de Gasto") +
  theme_minimal()

# Edad vs Ingreso
ggplot(mall, aes(x = Age, y = Income, color = cluster)) +
  geom_point(size = 2) +
  labs(title = "¿Cómo se relacionan edad e ingreso?",
       x = "Edad",
       y = "Ingreso Anual (k$)") +
  theme_minimal()