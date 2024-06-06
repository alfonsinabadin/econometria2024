library(readxl)
library(ggplot2)
library(dplyr)

gini_sucia <- read_excel("Bases/Indice de GINI por país.xls", 
                         sheet = "Data", skip = 3)

names(gini_sucia)[names(gini_sucia) == "Country Name"] <- "Pais"
names(gini_sucia)[names(gini_sucia) == "Country Code"] <- "Codigo"


sum(esperanza_de_vida$`Location type`!= "Country")
codigo_paises <- esperanza_de_vida$SpatialDimValueCode

#grafico de series por pais para demostrar por que tomar el valor de 2019 o mas reciente
#hasta 2010

#filtro_paisesifelse(gini_sucia$Codigo codigo_paises,1,0)

datos_mersheados <- merge(gini_sucia, esperanza_de_vida, by.x="Codigo", by.y = "SpatialDimValueCode")
