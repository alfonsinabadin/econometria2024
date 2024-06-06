library(readxl)
library(ggplot2)
library(dplyr)

gini_sucia <- read_excel("Bases/Indice de GINI por país.xls", 
                         sheet = "Data", skip = 3)

names(gini_sucia)[names(gini_sucia) == "Country Name"] <- "Pais"
names(gini_sucia)[names(gini_sucia) == "Country Code"] <- "Codigo"


#grafico de series por pais para demostrar por que tomar el valor de 2019 o mas reciente
#hasta 2010

