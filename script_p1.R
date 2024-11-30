# Importación de datos iniciales ------------------------------------------

df <- Mental_Health_Dataset

# Creación de nueva variable Continent ------------------------------------
df$Continent <- ifelse(df$Country %in% c("United States", "Canada", "Mexico"), "North America",
                       ifelse(df$Country %in% c("Costa Rica", "Brazil", "Colombia"), "South America",
                              ifelse(df$Country %in% c("Poland", "United Kingdom", "France", "Germany", 
                                                       "Belgium", "Ireland", "Portugal", "Sweden", 
                                                       "Netherlands", "Finland", "Italy", "Russia", 
                                                       "Bosnia and Herzegovina", "Switzerland", 
                                                       "Greece", "Denmark", "Croatia", "Moldova", 
                                                       "Georgia", "Czech Republic"), "Europe",
                                     ifelse(df$Country %in% c("India", "Israel", "Singapore", "Philippines", 
                                                              "Thailand"), "Asia",
                                            ifelse(df$Country %in% c("Australia", "New Zealand"), "Oceania",
                                                   ifelse(df$Country %in% c("South Africa", "Nigeria"), "Africa", NA))))))

# Se seleccionan variables importantes ------------------------------------
df_p1 <- df[, c("Continent", "Occupation", "family_history", "treatment", "Gender")]

# Confirmar valores NA
sum(is.na(df_p1))  # Si hay NA, pueden ser imputados o eliminados

# Librerías necesarias ----------------------------------------------------
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(FactoMineR)) install.packages("FactoMineR")
if (!require(factoextra)) install.packages("factoextra")
if (!require(janitor)) install.packages("janitor")

library(tidyverse)
library(FactoMineR)
library(factoextra)
library(janitor)

# Filtrar datos por género ------------------------------------------------

# Subconjunto para Gender = Female
df_female <- df_p1 %>%
  filter(Gender == "Female") %>%
  select(-Gender) %>%  # Excluye la columna Gender
  mutate(across(everything(), as.factor))

df_male <- df_p1 %>%
  filter(Gender == "Male") %>%
  select(-Gender) %>%  # Excluye la columna Gender
  mutate(across(everything(), as.factor))


# Análisis de Correspondencia Múltiple (MCA) ------------------------------

# MCA para Gender = Female
mca_female <- MCA(df_female, graph = FALSE, ncp = 5)

# MCA para Gender = Male
mca_male <- MCA(df_male, graph = FALSE, ncp = 5)

# Visualización y colores personalizados ----------------------------------

# Colores para continentes
continent_colors <- c(
  "North America" = "blue",
  "South America" = "green",
  "Europe" = "orange",
  "Africa" = "purple",
  "Asia" = "red",
  "Oceania" = "brown"
)

# Determinar colores según las categorías
var_colors_f <- ifelse(rownames(mca_female$var$coord) %in% names(continent_colors),
                     continent_colors[rownames(mca_female$var$coord)],
                     "gray") # Otras variables tendrán color gris
var_colors_m <- ifelse(rownames(mca_male$var$coord) %in% names(continent_colors),
                       continent_colors[rownames(mca_male$var$coord)],
                       "gray") # Otras variables tendrán color gris

# Visualización para Female
fviz_mca_var(
  mca_female,
  repel = TRUE,
  col.var = var_colors_f,  # Color para las variables (Female)
  labelsize = 4,          # Tamaño de las etiquetas
  pointsize = 3,          # Tamaño de los puntos
  title = "MCA - Gender: Female"
)

# Visualización para Male
fviz_mca_var(
  mca_male,
  repel = TRUE,
  col.var = var_colors_m,     # Color para las variables (Male)
  labelsize = 4,          # Tamaño de las etiquetas
  pointsize = 3,          # Tamaño de los puntos
  title = "MCA - Gender: Male"
)

# Varianza explicada ------------------------------------------------------

# Varianza acumulada para Female
varianza_acumulada_female <- cumsum(mca_female$eig[, 2])
print("Varianza acumulada para Female:")
print(varianza_acumulada_female)

# Varianza acumulada para Male
varianza_acumulada_male <- cumsum(mca_male$eig[, 2])
print("Varianza acumulada para Male:")
print(varianza_acumulada_male)

# Screeplots --------------------------------------------------------------

# Screeplot para Female
fviz_screeplot(
  mca_female,
  addlabels = TRUE,
  ylim = c(0, 50),
  main = "Screeplot - Gender: Female"
)

# Screeplot para Male
fviz_screeplot(
  mca_male,
  addlabels = TRUE,
  ylim = c(0, 50),
  main = "Screeplot - Gender: Male"
)

# Tabulación cruzada para entender relaciones -----------------------------

# Female: Relación entre Continent y Treatment
print("Tabulación cruzada para Female:")
df_female %>% tabyl(Continent, treatment)

# Male: Relación entre Continent y Treatment
print("Tabulación cruzada para Male:")
df_male %>% tabyl(Continent, treatment)

# Relación entre Occupation y Family History
print("Tabulación cruzada por Occupation y Family History (Female):")
df_female %>% tabyl(Occupation, family_history)

print("Tabulación cruzada por Occupation y Family History (Male):")
df_male %>% tabyl(Occupation, family_history)


