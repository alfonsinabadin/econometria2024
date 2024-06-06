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
