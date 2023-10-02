library(dplyr)
library(readr)


ruta_archivo <- "../baseDeDatos/lifeExpectancy.csv"

life_expectancy <- read_csv2(ruta_archivo)

View(life_expectancy)

# Estimacion de muestra

life_expectancy$Year <- as.factor(life_expectancy$Year)
life_expectancy$Corruption <- as.factor(life_expectancy$Corruption)

regions <- c("East Asia & Pacific", "Europe & Central Asia", "Middle East & North Africa", "Sub-Saharan Africa")

df_filtrado <- life_expectancy %>%
  filter(Region %in% regions)

medias <- df_filtrado %>%
  group_by(Region) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

View(medias)
