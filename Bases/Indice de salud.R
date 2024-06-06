# Limpieza de base -------------------------------------------------------------
library(readxl)
library(dplyr)

# Esperanza de salud -----------------------------------------------------------
salud <- read_excel("Bases/Espeanza de salud de vida.xlsx", 
                    sheet = "Hoja1")

# al nacer ---------------------------------------------------------------------
nacimiento <- salud[3:nrow(salud),1:5]
colnames(nacimiento) <- c("Pais","A_2019","A_2015","A_2010","A_2000")
sum(is.na(nacimiento$A_2019))
# Ningun valor falta en el 2019

# a los 60 ---------------------------------------------------------------------
viejo <- salud[3:nrow(salud),c(1,14:17)]
colnames(viejo) <- c("Pais","A_2019","A_2015","A_2010","A_2000")
sum(is.na(viejo$A_2019))
# Ningun valor falta en el 2019

# Esperanza de vida ------------------------------------------------------------
vida <- read.csv("Bases/esperanza de vida.csv")

# al nacer ---------------------------------------------------------------------
nacer <- subset(vida,vida$Indicator=="Life expectancy at birth (years)")
# filtro datos de pais
nacer <- subset(nacer, nacer$Location.type=="Country")
nacer <- nacer[,7:ncol(nacer)]
nacer <- subset(nacer, nacer$Period == 2019) # filtro para 2019
nacer <- subset(nacer, nacer$Dim1 == "Both sexes") # filtro para ambos sexos
sum(is.na(nacer$FactValueNumeric))

# a los 60 ---------------------------------------------------------------------
vieji <- subset(vida,vida$Indicator=="Life expectancy at age 60 (years)")
