library(FactoMineR)
library(factoextra)

# Cargar los datos
Mental.Health.Dataset <- read.csv("C:/Users/juans/Downloads/archive (2)/Mental Health Dataset.csv")


# Selección de variables importantes ------------------------------------
df_p1 <- Mental.Health.Dataset[, c("Social_Weakness", "Mental_Health_History", "family_history")]

# Confirmar valores NA
sum(is.na(df_p1))  # Si hay NA, pueden ser imputados o eliminados

# Eliminar valores faltantes (opcional, si no deseas imputarlos)
df_p1 <- na.omit(df_p1)

# Librerías necesarias ----------------------------------------------------
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(FactoMineR)) install.packages("FactoMineR")
if (!require(factoextra)) install.packages("factoextra")
if (!require(janitor)) install.packages("janitor")

library(tidyverse)
library(FactoMineR)
library(factoextra)
library(janitor)

# Convertir las variables en factores -------------------------------------
df_p1 <- df_p1 %>%
  mutate(across(everything(), as.factor))

# Análisis de Correspondencia Múltiple (MCA) ------------------------------

# Realizar el MCA para el conjunto de datos
mca_result <- MCA(df_p1, graph = FALSE, ncp = 5)

# Visualización y colores personalizados ----------------------------------

# Colores para las categorías (puedes personalizar)
category_colors <- c(
  "Yes" = "green",
  "No" = "red",
  "Maybe" = "blue"
)

# Determinar colores según las categorías
var_colors <- ifelse(rownames(mca_result$var$coord) %in% names(category_colors),
                     category_colors[rownames(mca_result$var$coord)],
                     "gray") # Otras variables tendrán color gris

# Visualización del MCA
fviz_mca_var(
  mca_result,
  repel = TRUE,
  col.var = var_colors,  # Color para las variables
  labelsize = 4,         # Tamaño de las etiquetas
  pointsize = 3,         # Tamaño de los puntos
  title = "MCA - Social Weakness, Mental Health History, and Family History"
)

# Varianza explicada ------------------------------------------------------

# Varianza acumulada
varianza_acumulada <- cumsum(mca_result$eig[, 2])
print("Varianza acumulada:")
print(varianza_acumulada)

# Screeplot --------------------------------------------------------------

# Screeplot para el conjunto completo
fviz_screeplot(
  mca_result,
  addlabels = TRUE,
  ylim = c(0, 50),
  main = "Screeplot - Social Weakness, Mental Health History, and Family History"
)

# Tabulación cruzada para entender relaciones -----------------------------

# Relación entre Social_Weakness y Mental_Health_History
print("Tabulación cruzada para Social Weakness y Mental Health History:")
Mental.Health.Dataset %>%
  tabyl(Social_Weakness, Mental_Health_History)

# Relación entre Mental_Health_History y family_history
print("Tabulación cruzada para Mental Health History y Family History:")
Mental.Health.Dataset %>%
  tabyl(Mental_Health_History, family_history)

# Relación entre Social_Weakness y family_history
print("Tabulación cruzada para Social Weakness y Family History:")
Mental.Health.Dataset %>%
  tabyl(Social_Weakness, family_history)
