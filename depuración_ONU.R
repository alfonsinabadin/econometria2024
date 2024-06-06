library(tidyverse)
database <- read.csv("Bases/HDR23-24_Composite_indices_complete_time_series.csv", stringsAsFactors=TRUE)
datos_filtrados_2019 <- database |> select(c(iso3, country, region, hdicode, hdi_2019, le_2019, eys_2019, mys_2019, gnipc_2019 ))
rm(database)
