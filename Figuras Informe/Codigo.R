#Carga de librerias
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(tidyr)
library(openxlsx)

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






bbdd_descrip <- bbdd_descrip %>%
  arrange(desc(Dim_Salud)) %>%
  mutate(Original = row_number()) %>%
  arrange(desc(row_number()))

bbdd_descrip <- bbdd_descrip %>%
  arrange(desc(Dim_Salud2)) %>%
  mutate(`Opcion 1` = row_number()) %>%
  arrange(desc(row_number()))

bbdd_descrip <- bbdd_descrip %>%
  arrange(desc(Dim_Salud3)) %>%
  mutate(`Opcion 2` = row_number()) %>%
  arrange(desc(row_number()))

bbdd_descrip <- bbdd_descrip %>%
  arrange(desc(Dim_Salud1)) %>%
  mutate(`Opcion 3` = row_number()) %>%
  arrange(desc(row_number()))

bbdd_descrip <- bbdd_descrip %>%
  arrange(desc(Dim_Salud4)) %>%
  mutate(`Opcion 4` = row_number()) %>%
  arrange(desc(row_number()))

