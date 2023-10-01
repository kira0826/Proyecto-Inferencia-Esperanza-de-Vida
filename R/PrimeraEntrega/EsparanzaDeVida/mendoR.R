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

life_expectancy <- read.csv("../baseDeDatos/lifeExpectancy.csv", header = TRUE, sep = ";") 
problems(life_expectancy)
spec()
