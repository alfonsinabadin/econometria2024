# Limpieza de base -------------------------------------------------------------
library(readxl)
library(dplyr)

# Esperanza de salud -----------------------------------------------------------
salud <- read_excel("Bases/Esperanza de salud de vida.xlsx", 
                    sheet = "Hoja1")

# al nacer ---------------------------------------------------------------------
nacimiento <- salud[3:nrow(salud),1:5]
colnames(nacimiento) <- c("Pais","A_2019","A_2015","A_2010","A_2000")
nacimiento$Pais[which(nacimiento$Pais=="Turkiye")] <- "Türkiye"
sum(is.na(nacimiento$A_2019))
# Ningun valor falta en el 2019

# a los 60 ---------------------------------------------------------------------
viejo <- salud[3:nrow(salud),c(1,14:17)]
colnames(viejo) <- c("Pais","A_2019","A_2015","A_2010","A_2000")
viejo$Pais[which(viejo$Pais=="Turkiye")] <- "Türkiye"
sum(is.na(viejo$A_2019))
# Ningun valor falta en el 2019

# Esperanza de vida ------------------------------------------------------------
vida <- read.csv("Bases/Esperanza de vida.csv")

# al nacer ---------------------------------------------------------------------
nacer <- subset(vida,vida$Indicator=="Life expectancy at birth (years)")
# filtro datos de pais
nacer <- subset(nacer, nacer$Location.type=="Country")
nacer <- nacer[,7:ncol(nacer)]
nacer <- subset(nacer, nacer$Period == 2019) # filtro para 2019
nacer <- subset(nacer, nacer$Dim1 == "Both sexes") # filtro para ambos sexos
sum(is.na(nacer$FactValueNumeric))
# Ningun valor falta en el 2019
nacer <- nacer[,c(1,2,4,18)]

# a los 60 ---------------------------------------------------------------------
vieji <- subset(vida,vida$Indicator=="Life expectancy at age 60 (years)")
# filtro datos de pais
vieji <- subset(vieji, vieji$Location.type=="Country")
vieji <- vieji[,7:ncol(vieji)]
vieji <- subset(vieji, vieji$Period == 2019) # filtro para 2019
vieji <- subset(vieji, vieji$Dim1 == "Both sexes") # filtro para ambos sexos
sum(is.na(vieji$FactValueNumeric))
# Ningun valor falta en el 2019
vieji <- vieji[,c(1,2,4,18)]

# Base completa salud ----------------------------------------------------------
salud <- data.frame(
  codigo = nacer$SpatialDimValueCode,
  Pais = nacer$Location,
  Año = 2019
)

salud$vida_nacer = numeric(nrow(salud))
salud$vida_60 = numeric(nrow(salud))
salud$salud_nacer = numeric(nrow(salud))
salud$salud_60 = numeric(nrow(salud))

for(i in 1:nrow(salud)){
  pais = salud$Pais[i]
  salud$vida_nacer[i] = nacer$FactValueNumeric[which(nacer$Location == pais)]
  salud$vida_60[i] = vieji$FactValueNumeric[which(vieji$Location == pais)]
  salud$salud_nacer[i] = nacimiento$A_2019[which(nacimiento$Pais == pais)]
  salud$salud_60[i] = viejo$A_2019[which(viejo$Pais == pais)]
}

write.csv(salud,"Bases/Datos Salud Completa.csv")