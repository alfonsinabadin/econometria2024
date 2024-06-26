library(readxl)
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)

gini_sucia <- read_excel("Bases/Indice de GINI por país.xls", 
                         sheet = "Data", skip = 3)
esperanza_de_vida <- read_csv("Bases/Esperanza de vida.csv")

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

for (i in 1:nrow(gini)){
  if(gini[i, 13] < 3){
    gini[i, 13] <- NA
  }
}

# Paso 2: Selecciona los valores de Gini de acuerdo al año encontrado
gini$gini <- apply(gini[], 1, function(row) {
  columna <- as.numeric(row["last_year"])
  return(row[columna])
})

gini <- gini[!is.na(gini$gini),]

# Crear data que tenga al ultimo indice de gini y el codigo del pais solamente
indicegini <- subset(gini, select = c(Codigo, gini))

write.csv(indicegini, "Bases/indicegini.csv")
