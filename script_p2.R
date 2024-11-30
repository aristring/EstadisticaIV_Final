install.packages("tidyverse")
library(tidyverse)
install.packages("FactoMineR")
library(FactoMineR)
library(factoextra)
install.packages("skimr")
library(skimr)


# Pregunta2 ---------------------------------------------------------------


# Datos -------------------------------------------------------------------

#Nos aseguramos que las variables se encuentren como factores 
data_mca <- Base2 %>% 
  select(Gender, Days_Indoors, Growing_Stress) %>%
  mutate(
    Gender = as.factor(Gender),
    Days_Indoors = as.factor(Days_Indoors),
    Growing_Stress = as.factor(Growing_Stress)
  )

# Se realiza el MCA 
mca_result <- MCA(data_mca, graph = FALSE, ncp = 5)

# Acceder a la matriz de Burt
burt_matrix <- mca_result$call$Xtot

# Mostrar la matriz de Burt
print(burt_matrix)

#Resumen del analisis
summary(mca_result)

#Para observar la contribución de las variables a las dos primeras dimensiones
fviz_contrib(mca_result, choice = "var", axes = 1) 
fviz_contrib(mca_result, choice = "var", axes = 2) 

# Visualización del Screeplot para examinar la varianza explicada por cada dimensión
fviz_screeplot(mca_result, addlabels = TRUE, ylim = c(0,20))


#Biplot MCA
fviz_mca_var(mca_result,  
             col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, 
             max.overlaps = "ggrepel.max.overlaps", 
             ggtheme = theme_minimal()
)


