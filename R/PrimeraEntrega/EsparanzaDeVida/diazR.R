## Parte de Diaz

library(dplyr)
library(readr)


ruta_archivo <- "../baseDeDatos/lifeExpectancy.csv"
life_expectancy <- read_csv2(ruta_archivo)

# Convertir Year y Corruption en factores
life_expectancy$Year <- as.factor(life_expectancy$Year)
life_expectancy$Corruption <- as.factor(life_expectancy$Corruption)

View(life_expectancy)

regions <- c("East Asia & Pacific", "Europe & Central Asia", "Middle East & North Africa", "Sub-Saharan Africa")

# Crear un nuevo dataframe solo con las poblaciones seleccionadas
df_filtrado <- life_expectancy %>%
  filter(Region %in% regions)

# Calcular la estimación de la media para todas las variables en las poblaciones seleccionadas
medias <- df_filtrado %>%
  group_by(Region) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

# Ver los resultados
View(medias)
