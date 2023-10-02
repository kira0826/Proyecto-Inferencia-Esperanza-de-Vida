## Parte de Mendo

library(readxl)
library(tidyverse)
library(ggplot2)
library(plotly)
library(pastecs)
library (fdth)
library(PASWR2)
library(descr)
library(lmtest)
library(dplyr)
library(readr)

# Leer los datos y convertir la variable IncomeGroup en factor
life_expectancy <- read.csv2("../baseDeDatos/lifeExpectancy.csv", header = TRUE, sep = ";")
life_expectancy$IncomeGroup <- factor(life_expectancy$IncomeGroup, levels = c("Low income", "Lower middle income", "Upper middle income", "High income"))

# Filtrar los datos para incluir solo el año 2019
datos_2019 <- subset(life_expectancy, Year == 2019)

# Calcular la relación entre IncomeGroup y expectativa de vida por regiones
relacion_ingresos_expectativa_vida <- datos_2019 %>%
  group_by(Region, IncomeGroup) %>%
  summarise(Media_Expectativa_Vida = mean(`Life.Expectancy.World.Bank`, na.rm = TRUE))

print(relacion_ingresos_expectativa_vida)

# Crear el gráfico de barras apiladas con colores distintos para cada grupo de ingresos
ggplot(relacion_ingresos_expectativa_vida, aes(x = Region, y = Media_Expectativa_Vida, fill = IncomeGroup)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#33a02c","#1f78b4" , "#e31a1c", "#ff7f00"), name = "Income Group") +
  labs(x = "Región", y = "Expectativa de Vida Media", title = "Relación entre el Ingreso y la expectativa de vida por regiones en el año 2019") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Resumen de cantidad de países por ingreso y por región
resumen_paises_por_ingreso <- datos_2019 %>%
  group_by(Region, IncomeGroup) %>%
  summarise(Cantidad_Paises = n())

# Mostrar el resumen
resumen_paises_por_ingreso

## Calcular la media de expectativa de vida por año
media_expectativa_vida_por_año <- life_expectancy %>%
  group_by(Year) %>%
  summarise(Media_Expectativa_Vida = mean(Life.Expectancy.World.Bank, na.rm = TRUE))

# Mostrar el resultado
media_expectativa_vida_por_año


# Calcular la cantidad de países por cada nivel de corrupción
cantidad_paises_por_corrupcion <- datos_2019 %>%
  group_by(Corruption) %>%
  summarise(Cantidad_Paises = n())

# Mostrar el resultado
ggplot(cantidad_paises_por_corrupcion, aes(x = factor(Corruption), y = Cantidad_Paises)) +
  geom_bar(stat = "identity", fill = "#1f78b4") +
  labs(x = "Nivel de Corrupción", y = "Cantidad de Países", 
       title = "Cantidad de Países por Nivel de Corrupción en el Año 2019") +
  theme_minimal()


