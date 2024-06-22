#Carga de librerias
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(tidyr)
library(openxlsx)

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
libertad <- libertad |> filter(`C/T` == "c")
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

par(family = "serif")

#Carga data frame para analisis descriptivo
bbdd_descrip <- datos_juntos_sinNA %>%
  select(codigo, Pais,dim_econ,gnipc2_2019,gini,vida_nacer,salud_nacer,dim_salud,dim_educ)

colnames(bbdd_descrip) <- c("Codigo","Pais","Dim_Econ","GNIPC","G","Vida_nacer","Salud_nacer","Dim_Salud","Dim_Educ")

# OPCION 1

# SALUD

# calculo min y max

Datos_Salud_Completa <- read_csv("Bases/Datos Salud Completa.csv")
dif <- Datos_Salud_Completa$vida_nacer - Datos_Salud_Completa$salud_nacer
db <- cbind(Datos_Salud_Completa,dif)

df2 <- data.frame(indice=rep(c("vida", "salud"), each=183),
                  valor = c(Datos_Salud_Completa$vida_nacer,Datos_Salud_Completa$salud_nacer),
                  pais= Datos_Salud_Completa$Pais)

promedio <- c()
min_salud_nacer <- 20*(min(Datos_Salud_Completa$salud_nacer)/min(Datos_Salud_Completa$vida_nacer))
max_salud_nacer <- 85*(max(Datos_Salud_Completa$salud_nacer)/max(Datos_Salud_Completa$vida_nacer))

bbdd_descrip$Dim_Salud2 <- (bbdd_descrip$Salud_nacer-min_salud_nacer)/(max_salud_nacer-min_salud_nacer)

#Boxplot

#Formato largo
bbddescrip_nuevo <- data.frame(
  idpais = rep(bbdd_descrip$Codigo, 2),
  valor = c(bbdd_descrip$Dim_Salud, bbdd_descrip$Dim_Salud2),
  tipo_variable = factor(rep(c("Indice original", "Indice opcion 1"), each = nrow(bbdd_descrip)),
                         levels = c("Indice original", "Indice opcion 1"), ordered = TRUE)
)

saveRDS(bbddescrip_nuevo,"Figuras Informe/salud_base_box_1.rds")

ggplot(bbddescrip_nuevo) +
  geom_boxplot(aes(x = tipo_variable, y = valor, fill = tipo_variable), alpha = 0.85) +
  labs(x = "Indice", y = "Valor") + 
  scale_fill_manual(values = c("#d4dff0", "#543786")) +
  theme_minimal() +
  theme(
    text = element_text(family = "serif"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "none"
  ) 

# OPCIÓN 2

bbdd_descrip$SaludVida_nacer <- (bbdd_descrip$Vida_nacer+bbdd_descrip$Salud_nacer)/2
min_saludvida_nacer <- 20*(min(bbdd_descrip$SaludVida_nacer)/min(bbdd_descrip$Vida_nacer))
max_saludvida_nacer <- 85*(max(bbdd_descrip$SaludVida_nacer)/max(bbdd_descrip$Vida_nacer))
bbdd_descrip$Dim_Salud3 <- (bbdd_descrip$SaludVida_nacer-min_saludvida_nacer)/(max_saludvida_nacer-min_saludvida_nacer)

#Boxplot

#Formato largo
bbddescrip_nuevo <- data.frame(
  idpais = rep(bbdd_descrip$Codigo, 2),
  valor = c(bbdd_descrip$Dim_Salud, bbdd_descrip$Dim_Salud3),
  tipo_variable = factor(rep(c("Indice original", "Indice opcion 2"), each = nrow(bbdd_descrip)),
                         levels=c("Indice original", "Indice opcion 2"), ordered = TRUE)
)

saveRDS(bbddescrip_nuevo,"Figuras Informe/salud_base_box_2.rds")

ggplot(bbddescrip_nuevo) +
  geom_boxplot(aes(x = tipo_variable, y = valor, fill = tipo_variable)) +
  labs(x = "Indice", y = "Valor") +
  scale_fill_manual(values = c("#d4dff0", "#543786")) +
  theme_minimal() +
  theme(
    text = element_text(family = "serif"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "none"
  ) 

# OPCIÓN 3

bbdd_descrip$Ind_Salud1 <- (bbdd_descrip$Salud_nacer-17.41872)/(74.75077-17.41872)
bbdd_descrip$Dim_Salud1 <- bbdd_descrip$Ind_Salud1*bbdd_descrip$Dim_Salud
#Boxplot

#Formato largo
bbddescrip_nuevo <- data.frame(
  idpais = rep(bbdd_descrip$Codigo, 2),
  valor = c(bbdd_descrip$Dim_Salud, bbdd_descrip$Dim_Salud1),
  tipo_variable = factor(rep(c("Indice original", "Indice opcion 3"), each = nrow(bbdd_descrip)),
                         levels = c("Indice original", "Indice opcion 3"), ordered = TRUE)
)

saveRDS(bbddescrip_nuevo,"Figuras Informe/salud_base_box_3.rds")

ggplot(bbddescrip_nuevo) +
  geom_boxplot(aes(x = tipo_variable, y = valor, fill = tipo_variable)) +
  labs(x = "Indice", y = "Valor") +
  scale_fill_manual(values = c("#d4dff0", "#543786")) +
  theme_minimal() +
  theme(
    text = element_text(family = "serif"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "none"
  ) 

# OPCIÓN 4

bbdd_descrip$Dim_Salud4 <- (bbdd_descrip$Dim_Salud+bbdd_descrip$Dim_Salud2)/2

#Boxplot

#Formato largo
bbddescrip_nuevo <- data.frame(
  idpais = rep(bbdd_descrip$Codigo, 2),
  valor = c(bbdd_descrip$Dim_Salud, bbdd_descrip$Dim_Salud4),
  tipo_variable = factor(rep(c("Indice original", "Indice opcion 4"), each = nrow(bbdd_descrip)),
                         levels = c("Indice original", "Indice opcion 4"), ordered = TRUE)
)

saveRDS(bbddescrip_nuevo,"Figuras Informe/salud_base_box_4.rds")

ggplot(bbddescrip_nuevo) +
  geom_boxplot(aes(x = tipo_variable, y = valor, fill = tipo_variable)) +
  labs(x = "Indice", y = "Valor") +
  scale_fill_manual(values = c("#d4dff0", "#543786")) +
  theme_minimal() +
  theme(
    text = element_text(family = "serif"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "none"
  ) 

saveRDS(bbdd_descrip, "Figuras Informe/Base_tabla.rds")

bbdd_descrip <- readRDS("Figuras Informe/Base_tabla.rds")

tabla_resultado <- bbdd_descrip %>%
  arrange(Salud_nacer) %>%
  select(Pais, Salud_nacer, Vida_nacer, Dim_Salud, Dim_Salud2) %>%
  head(5)

tabla_resultado <- tabla_resultado %>%
  select(Pais, Vida_nacer, Salud_nacer, Dim_Salud, Dim_Salud2) %>%
  rename(
    `País` = Pais,
    `Esperanza de vida` = Vida_nacer,
    `Esperanza de vida saludable` = Salud_nacer,
    `Índice de Salud original` = Dim_Salud,
    `Índice de Salud modificado` = Dim_Salud2
  )


# DIMENSION ECONÓMICA ----------------------------------------------------------

# OPCIÓN 1

bbdd_descrip$GNIPCA <- bbdd_descrip$GNIPC * (1- bbdd_descrip$G / 100)
bbdd_descrip$Dim_Econ1 <- (log(bbdd_descrip$GNIPCA)-log(100))/(log(53377.0398)-log(100))

bbddescrip_nuevo <- data.frame(
  idpais = rep(bbdd_descrip$Codigo, 2),
  valor = c(bbdd_descrip$Dim_Econ, bbdd_descrip$Dim_Econ1),
  tipo_variable = factor(rep(c("Indice Original", "Indice Opcion 1"), each = nrow(bbdd_descrip)),
                         levels = c("Indice Original", "Indice Opcion 1"), ordered = TRUE)
)

saveRDS(bbddescrip_nuevo, "Figuras Informe/econ_base_box_1.rds")

ggplot(bbddescrip_nuevo) +
  geom_boxplot(aes(x = tipo_variable, y = valor, fill = tipo_variable)) +
  labs(x = "Indice", y = "Valor") + 
  scale_fill_manual(values = c("#d4dff0", "#543786")) +
  theme_minimal() +
  theme(
    text = element_text(family = "serif"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "none"
  ) +
  coord_flip()

# OPCION 2

# opcion 2 sin raiz
bbdd_descrip$Dim_Econ2_1 <- bbdd_descrip$Dim_Econ*(1-bbdd_descrip$G/100)
# opcion 2 con raiz
bbdd_descrip$Dim_Econ2 <- bbdd_descrip$Dim_Econ*sqrt(1-bbdd_descrip$G/100)


#Boxplot

#Formato largo
bbddescrip_nuevo <- data.frame(
  idpais = rep(bbdd_descrip$Codigo, 3),
  valor = c(bbdd_descrip$Dim_Econ, bbdd_descrip$Dim_Econ2_1, bbdd_descrip$Dim_Econ2),
  tipo_variable = factor(rep(c("Indice Original", "Indice Opcion 2 sin raiz", "Indice Opcion 2 con raiz"),each = nrow(bbdd_descrip)),
                      levels =c("Indice Original", "Indice Opcion 2 sin raiz", "Indice Opcion 2 con raiz"),
                      ordered = TRUE))

saveRDS(bbddescrip_nuevo, "Figuras Informe/econ_base_box_2.rds")

ggplot(bbddescrip_nuevo) +
  geom_boxplot(aes(x = tipo_variable, y = valor, fill = tipo_variable)) +
  labs(title = "Comparación indice económico original y opción 2", x = "Indice", y = "Valor") +
  scale_fill_manual(values = c("#d4dff0", "#543786", "#543786")) +
  theme_minimal() +
  theme(
    text = element_text(family = "serif"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "none"
  ) +
  coord_flip()
