### Creacion de bases para los resultados-------------

source("depuración_ONU.R")

res <- datos_juntos_sinNA

res <- res |> mutate(
  dim_lib = lpts/100
)

res$GNI_modif <- res$gnipc2_2019 * (1 - res$gini/100)

res$dim_salud2 <- (res$salud_nacer-17.41872)/(74.75077-17.41872)

res$dim_econ2 <- (log(res$GNI_modif)-log(100))/(log(53377.0398)-log(100))

res <- res |> select(codigo, Pais, hdicode, IDH, le2_2019, eys2_2019, mys2_2019, 
              gnipc2_2019, salud_nacer, gini, lpts, GNI_modif, dim_salud2,
              dim_educ, dim_econ2, dim_lib)
res <- res |> mutate(IDH2 = (dim_salud2*dim_educ*dim_econ2)^(1/3))
res <- res |> mutate(IDH3 = (dim_salud2*dim_educ*dim_econ2*dim_lib)^(1/4))


library(ggplot2)
library(rworldmap)
library(dplyr)
library(readr)

#cargar la base
  # Convertir los códigos de país a nombres de país
  res <- res %>%
  mutate(region = countrycode(codigo, origin = 'iso3c', destination = 'country.name'))

# Unir los datos del IDH con el mapa mundial
world_map <- getMap()
world_map_data <- fortify(world_map)

world_map_data <- world_map_data |> 
  mutate(codigo = countrycode(id, "country.name", "iso3c"))

res <- res %>%
  mutate(region = tolower(region))



# Unir los datos del mapa con los datos del IDH
map_data <- left_join(world_map_data, res, by = "codigo")

options(device = "jpeg")  # Establecer el dispositivo gráfico a JPEG

# Crear el gráfico
graf_IDH <- ggplot(map_data, aes(x = long, y = lat, group = group, fill = IDH)) +
  geom_polygon(color = "black") +
  scale_fill_gradientn(
    colours = c("darkred", "#EA1111", "#FBFF27", "#3FE447", "#002506"),
    values = c(0, 0.25, 0.5, 0.75, 1),
    na.value = "grey50",
    limits = c(0, 1), 
  ) +
  labs(title = "Índice de Desarrollo Humano (IDH) por país",
       fill = "IDH") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

graf_IDH2 <- ggplot(map_data, aes(x = long, y = lat, group = group, fill = IDH2)) +
  geom_polygon(color = "black") +
  scale_fill_gradientn(
    colours = c("darkred", "#EA1111", "#FBFF27", "#3FE447", "#002506"),
    values = c(0, 0.25, 0.5, 0.75, 1),
    na.value = "grey50",
    limits = c(0, 1), 
  ) +
  labs(title = "IDH con modificaciones sin Libertad por país",
       fill = "IDH") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

graf_IDH3 <- ggplot(map_data, aes(x = long, y = lat, group = group, fill = IDH3)) +
  geom_polygon(color = "black") +
  scale_fill_gradientn(
    colours = c("darkred", "#EA1111", "#FBFF27", "#3FE447", "#002506"),
    values = c(0, 0.25, 0.5, 0.75, 1),
    na.value = "grey50",
    limits = c(0, 1), 
  ) +
  labs(title = "IDH con modificaciones con Libertad por país",
       fill = "IDH") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

ggsave("Resultados/mapa_IDH1.jpg", plot = graf_IDH, width = 3840, height = 2160, units = "px")
ggsave("Resultados/mapa_IDH2.jpg", plot = graf_IDH2, width = 3840, height = 2160, units = "px")
ggsave("Resultados/mapa_IDH3.jpg", plot = graf_IDH3, width = 3840, height = 2160, units = "px")

# Revertir el dispositivo gráfico a su configuración predeterminada
options(device = NULL)

library(magick)
# Leer las imágenes usando magick
img1 <- image_read("Resultados/mapa_IDH1.jpg")
img2 <- image_read("Resultados/mapa_IDH2.jpg")
img3 <- image_read("Resultados/mapa_IDH3.jpg")

# Crear el GIF
gif <- image_animate(image_join(img1, img2, img3), fps = 0.5)

# Guardar el GIF
image_write(gif, "Resultados/mapas.gif")





### Establecer jerarquias---------
res <- res %>%
  arrange(desc(IDH)) %>%
  mutate(rank_IDH1 = row_number()) %>%
  arrange(desc(IDH2)) %>%
  mutate(rank_IDH2 = row_number())%>%
  arrange(desc(IDH3)) %>%
  mutate(rank_IDH3 = row_number())%>%
  arrange(desc(row_number()))

library("openxlsx")
write.xlsx(res, "Resultados/resultados.xlsx")


#00770E

### Correlacion de los IDH ------------
cor(res$IDH, res$IDH2)
cor(res$IDH, res$IDH3)

ggplot(res)+
  geom_point(aes(IDH,IDH2), color="blue")+
  geom_abline(slope=1, intercept = 0)

ggplot(res)+
  geom_point(aes(IDH,IDH3), color="blue")+
  geom_abline(slope=1, intercept = 0)

ggplot(res)+
  geom_point(aes(IDH2,IDH3), color="blue")+
  geom_abline(slope=1, intercept = 0)
