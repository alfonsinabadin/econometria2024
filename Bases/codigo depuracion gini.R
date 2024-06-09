library(readxl)
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)

gini_sucia <- read_excel("Bases/Indice de GINI por país.xls", 
                         sheet = "Data", skip = 3)

names(gini_sucia)[names(gini_sucia) == "Country Name"] <- "Pais"
names(gini_sucia)[names(gini_sucia) == "Country Code"] <- "Codigo"

# Creo tabla de paises y codigos y le agrego una indicadora q vale 1
paises <- read_csv("Bases/HDR23-24_Composite_indices_complete_time_series.csv")

paises <- paises %>% select(1)

paises <- paises %>% slice(1:195)

names(paises)[names(paises) == "iso3"] <- "Codigo"

paises <- paises %>% mutate(indicadora = 1)

# Junto ambas tablas para dejar solo los paises

gini <- left_join(gini_sucia, paises, by = "Codigo")

gini <- gini[!is.na(gini$indicadora), ]

sum(esperanza_de_vida$`Location type`!= "Country")
codigo_paises <- esperanza_de_vida$SpatialDimValueCode

#grafico de series por pais para demostrar por que tomar el valor de 2019 o mas reciente
#hasta 2010

<<<<<<< HEAD
# Transformar los datos de formato ancho a formato largo
gini_long <- gini_sucia %>% pivot_longer(
  cols = starts_with("20"), # Selecciona las columnas que representan los años
  names_to = "Año",         # Nuevo nombre para las columnas
  values_to = "Gini"        # Nuevo nombre para los valores
)

ggplot(gini_long, aes(x = Año, y = Gini, group = Pais, color = Pais)) +
  geom_line(size = 0.5) +  # Línea para cada serie temporal de cada país
  geom_point(size = 1) + # Puntos en cada año para cada país
  theme_minimal() +  # Estilo de gráfico limpio y minimalista
  labs(
    title = "Índice de Gini por País (2010-2020)",
    x = "Año",
    y = "Índice de Gini"
  ) +
  theme(legend.position = "none")


# Eliminar las columnas 2020, 2021, 2022, 2023 e "indicadora"
gini <- gini[, -c(13:ncol(gini))]

# Encontrar el ultimo dato actualizado y guardarlo en la nueva variable gini

# Paso 1: Encuentra el año más reciente con datos disponibles para cada país
gini$last_year <- apply(gini[, -1], 1, function(x) max(which(!is.na(x))+1))

# Paso 2: Selecciona los valores de Gini de acuerdo al año encontrado
gini$gini <- apply(gini[, -c(1, ncol(gini))], 1, function(x) {
  ifelse(all(is.na(x)), NA, ifelse(!is.na(x[gini$last_year]), x[gini$last_year], x[10]))
})

# Elimina las columnas auxiliares
gini <- gini[, -(ncol(gini)-1):ncol(gini)]

# Crear data que tenga al ultimo indice de gini y el codigo del pais solamente
indicegini <- subset(gini, select = c(Codigo, gini))
=======
#filtro_paisesifelse(gini_sucia$Codigo codigo_paises,1,0)

datos_mersheados <- merge(gini_sucia, esperanza_de_vida, by.x="Codigo", by.y = "SpatialDimValueCode")
>>>>>>> 5957b9a60771108dc4e4fec85eb48f25228872fa
