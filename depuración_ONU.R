library(tidyverse)
database <- read.csv("Bases/HDR23-24_Composite_indices_complete_time_series.csv", stringsAsFactors=TRUE)
datos_filtrados_2019 <- database |> select(c(iso3, country, region, hdicode, hdi_2019, le_2019, eys_2019, mys_2019, gnipc_2019 ))
rm(database)

datos_filtrados_2019 <- datos_filtrados_2019 |> 
  mutate(le2_2019 = case_when(
    le_2019 <= 20 ~ 20,
    le_2019 > 20 & le_2019 <= 85 ~ le_2019,
    le_2019 > 85 ~ 85
  ),
  eys2_2019 = case_when(
    eys_2019 <= 0 ~ 0,
    eys_2019 > 0 & eys_2019 <= 18 ~ eys_2019,
    eys_2019 > 18 ~ 18
  ),
  mys2_2019 = case_when(
    mys_2019 <= 0 ~ 0,
    mys_2019 > 0 & mys_2019 <= 15 ~ mys_2019,
    mys_2019 > 15 ~ 15
  ),
  gnipc2_2019 = case_when(
    gnipc_2019 <= 100 ~ 100,
    gnipc_2019 > 100 & gnipc_2019 <= 75000 ~ gnipc_2019,
    gnipc_2019 > 75000 ~ 75000
  ))

datos_filtrados_2019 <- datos_filtrados_2019 |> 
  mutate(
    dim_salud = (le2_2019-20)/(85-20),
    dim_educ = (eys2_2019/18+mys2_2019/15)/2,
    dim_econ = (log(gnipc2_2019)-log(100))/(log(75000)-log(100))
  )

datos_filtrados_2019 <- datos_filtrados_2019 |> 
  mutate(
    IDH = (dim_salud*dim_educ*dim_econ)^(1/3)
  )

datos_filtrados_2019 <- datos_filtrados_2019 |> 
  mutate(
    DIF = case_when(abs(IDH-hdi_2019) <= 0.0005 ~ 0,
                      abs(IDH-hdi_2019) > 0.0005 ~ 1))

cat("Hay", sum(datos_filtrados_2019$DIF, na.rm = TRUE), 
    "paises que dieron diferencias entre los IDH calculados y",
    sum(is.na(datos_filtrados_2019$DIF)),
    "en los que no se pudo calcular el IDH.")

datos_filtrados_2019 <- datos_filtrados_2019 |> 
  select(-DIF) |> mutate(codigo = iso3)

datos_salud <- read.csv("Bases/Datos Salud Completa.csv", stringsAsFactors=TRUE)

datos_juntos_sinNA <- merge(datos_filtrados_2019, datos_salud, by = "codigo")
datos_juntos <- merge(datos_filtrados_2019, datos_salud, by = "codigo", all.x = TRUE)

datos_gini <- read.csv("Bases/indicegini.csv", stringsAsFactors=TRUE)
datos_gini <- datos_gini[,-1]
colnames(datos_gini) <- c("codigo", "gini")

datos_juntos_sinNA <- merge(datos_juntos_sinNA, datos_gini, by = "codigo")
datos_juntos <- merge(datos_juntos, datos_gini, by = "codigo", all.x = TRUE)

### DATOS DE LIBERTAD
library(readxl)
libertadOG <- read_excel("All_data_FIW_2013-2024.xlsx", sheet = "FIW13-24")
libertad <- libertadOG |> filter(Edition == 2019)
libertad <- libertad |> select("Country/Territory", "Total", "PR", "CL")
colnames(libertad) <- c("country", "lpts", "political_rights", "civil_liberties")

library(countrycode)
libertad <- libertad |> 
  mutate(ISO3 = countrycode(country, "country.name", "iso3c"))

# Verificar los primeros registros para asegurarse de que la columna se ha añadido correctamente
head(libertad)

libertad <- libertad |> 
  filter(!is.na(ISO3)) |> 
  mutate(codigo = ISO3) |> 
  select(codigo, lpts, political_rights, civil_liberties)

datos_juntos_sinNA <- merge(datos_juntos_sinNA, libertad, by = "codigo")
datos_juntos <- merge(datos_juntos, libertad, by = "codigo", all.x = TRUE)

