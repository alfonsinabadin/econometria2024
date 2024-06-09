library(ggplot2)
library(dplyr)
ggplot(data = db, aes(y = dif, x= Pais)) +
  geom_bar(stat="identity")

dif <- Datos_Salud_Completa$vida_nacer - Datos_Salud_Completa$salud_nacer
db <- cbind(Datos_Salud_Completa,dif)

df2 <- data.frame(indice=rep(c("vida", "salud"), each=183),
                  valor = c(Datos_Salud_Completa$vida_nacer,Datos_Salud_Completa$salud_nacer),
                  pais= Datos_Salud_Completa$Pais)

promedio <- c()
min_salud_nacer <- 20*(min(Datos_Salud_Completa$salud_nacer)/min(Datos_Salud_Completa$vida_nacer))
max_salud_nacer <- 85*(max(Datos_Salud_Completa$salud_nacer)/max(Datos_Salud_Completa$vida_nacer))

for(i in 1:nrow(Datos_Salud_Completa)){
  promedio[i] = mean(Datos_Salud_Completa$vida_nacer[i],Datos_Salud_Completa$salud_nacer[i])
}
Datos_Salud_Completa$promedio <- promedio
Datos_Salud_Completa$indice_vida <- (Datos_Salud_Completa$vida_nacer - 20 )/65
Datos_Salud_Completa$indice_salud <- (Datos_Salud_Completa$salud_nacer - min_salud_nacer )/(max_salud_nacer-min_salud_nacer)
Datos_Salud_Completa$indice_promedio <- (Datos_Salud_Completa$promedio - 17.4 )/65

indice <- c()
for(i in 1:nrow(Datos_Salud_Completa)){
  indice[i] = Datos_Salud_Completa$indice_salud[i]*Datos_Salud_Completa$indice_vida[i]
}

Datos_Salud_Completa$indice <- indice

# esto es una mierda!! no lo hacemos asi
##----------------##

min_salud_nacer <- 20*(min(Datos_Salud_Completa$salud_nacer)/min(Datos_Salud_Completa$vida_nacer))
max_salud_nacer <- 85*(max(Datos_Salud_Completa$salud_nacer)/max(Datos_Salud_Completa$vida_nacer))
Datos_Salud_Completa$indice_vida <- (Datos_Salud_Completa$vida_nacer - 20 )/65
Datos_Salud_Completa$indice_salud <- (Datos_Salud_Completa$salud_nacer - min_salud_nacer )/(max_salud_nacer-min_salud_nacer)

promedio_indices <- c()
for(i in 1:nrow(Datos_Salud_Completa)){
  promedio_indices[i] = (Datos_Salud_Completa$indice_salud[i]+Datos_Salud_Completa$indice_vida[i])/2
}

Datos_Salud_Completa$promedio_indices <- promedio_indices

indice <- c()
for(i in 1:nrow(Datos_Salud_Completa)){
  indice[i] = (Datos_Salud_Completa$indice_salud[i]*Datos_Salud_Completa$indice_vida[i])
}

Datos_Salud_Completa$indice<- indice

 #elegimos el promedio porque:
# 1) si tiene ambos valores bajos o ambos altos no cambia mucho, pero si hay diferencia si lo baja
# 2) tenemos en cuenta el promedio de las dos y no solo el de salud pq:
# quizas las grandes diferencias pueden darse

plot(Datos_Salud_Completa$dif, Datos_Salud_Completa$vida_nacer)
plot(Datos_Salud_Completa$dif, Datos_Salud_Completa$salud_nacer)
plot(Datos_Salud_Completa$salud_nacer, Datos_Salud_Completa$vida_nacer)
