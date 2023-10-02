# Parte de Richie

library(readxl)
library(tidyverse)
library(ggplot2)
library(pastecs)
library (fdth)
library(PASWR2)
library(descr)
library(lmtest)
library(dplyr)
library(readr)

life_expectancy <- read.csv2("../baseDeDatos/lifeExpectancy.csv", header = TRUE, sep = ";") 
# Filtrar los datos para incluir solo el año 2019
life_expectancy <- subset(life_expectancy, Year == 2019)



#Análisis exploratorio de datos 
eda(life_expectancy$Health.Expenditure..)
eda(life_expectancy$Education.Expenditure..)
eda(life_expectancy$Unemployment)

#Pruebas de hipótesis

##Análisis exploratorio de datos Life expectancy
eda(life_expectancy$Life.Expectancy.World.Bank)

curve_area <- function(mean = 0, sd = 1, lb, ub, acolor = "lightgray", ...,z) {
  x <- seq(mean - 3 * sd, mean + 3 * sd, length = 100) 
  
  if (missing(lb)) {
    lb <- min(x)
  }
  if (missing(ub)) {
    ub <- max(x)
  }
  
  x2 <- seq(lb, ub, length = 100)    
  plot(x, dnorm(x, mean, sd), type = "l", ylab = "")
  
  y <- dnorm(x2, mean, sd)
  polygon(c(lb, x2, ub), c(0, y, 0), col = acolor)
  lines(x, dnorm(x, mean, sd), type = "l", ...)
  
  arrows(0, 0.1, z, 0, lwd = 2, length = 0.2)
  text(0, 0.13, z, cex = 1.5)
  
  
}

##Hipotesis para la media
####Comprobar que en promedio los paises con bajos ingresos tienen menos de 65 años de esperanza de vida
####H0:μ\ge65
####H1:μ<65
grupos <- split(life_expectancy, life_expectancy$IncomeGroup)
grupo_low_income <- grupos[["Low income"]]
t.test(grupo_low_income$Life.Expectancy.World.Bank, mu=65,  alternative = "less")
qt (p = 0.95, df = 22, lower.tail = FALSE)
####Si buscamos el valor crítico de t con un α de 0,05 tenemos que tα=-1.717
####Al comparar con el t calculado vemos que este es menor al valor crítico, por lo que decimos que se ubica en la zona de rechazo. Es decir, rechazamos H0
####Si utilizamos el valor P para comparar y validar el error de tipo I, vemos que el valor p es de 0.0005817, este es menor al $alpha $ de 0,05, por lo que rechazamos H0
####Se concluye que en promedio los paises con bajos ingresos tienen menos de 65 años de esperanza de vida.

##Hipotesis para la media
####Comprobar que en promedio los paises con altos ingresos tienen más de 70 años de esperanza de vida
####H0:μ\le70
####H1:μ>70
grupo_high_income <- grupos[["High income"]]
t.test(grupo_high_income$Life.Expectancy.World.Bank, mu=70,  alternative = "greater")
qt (p = 0.95, df = 56, lower.tail = TRUE)
curve_area(mean = 0, sd = 1, lb = qt (p = 0.95, df = 56, lower.tail = TRUE), lwd = 2, acolor = rgb(0, 0, 1, alpha = 0.5), z=12.77)
####Si buscamos el valor crítico de t con un α de 0,05 tenemos que tα=1.67
####Al comparar con el t calculado vemos que este es mayor al valor crítico, por lo que decimos que se ubica en la zona de rechazo. Es decir, rechazamos H0
####Si utilizamos el valor P para comparar y validar el error de tipo I, vemos que el valor p es de 2.2e-16, este es menor al $alpha $ de 0,05, por lo que rechazamos H0
####Se concluye que en promedio los paises con altos ingresos tienen más de 70 años de esperanza de vida

##Hipotesis para la proporción
####Comprobar que más del 50% de los paises tienen un grado de corrupción 3.
####H0:π\le0.5
####H1:π>0.5
conteoX <- sum(life_expectancy$Corruption == "3")
conteoN <- nrow(life_expectancy)
prop.test(x=conteoX, n=conteoN, p=0.5, alternative = "greater", conf.level=0.95, correct=FALSE)
#Valor crítico
qnorm(0.95)
curve_area(mean = 0, sd = 1, lb = qnorm(0.95), lwd = 2, acolor = rgb(0, 0, 1, alpha = 0.5), z=6.36)
####Al comparar con el valor p vemos se debe rechazar H0, ya que p es menor al valor de alpha. 
####Al rechazar la hipótesis nula concluimos que más del 50% de de los paises tienen un grado de corrupción 3.

