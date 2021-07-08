library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(auk)
library(dplyr)
library(ggspatial)

world <- ne_countries(scale = "medium", returnclass = "sf")

#Inverno
f_out <- "data/ebd_prinv.txt"

auk_ebd("data/ebd_verfly_relMar-2021.txt") %>%
  # filtar pela data
  auk_date(c("*-05-01", "*-07-31")) %>% 
  # rodar os filtros
  auk_filter(f_out, overwrite = TRUE)

inverno <- read_ebd("data/ebd_prinv.txt")

#Verao - Parte 1
f_out <- "data/ebd_prver.txt"

auk_ebd("data/ebd_verfly_relMar-2021.txt") %>%
  # filtar pela data
  auk_date(c("*-10-01", "*-02-29")) %>%
  # rodar os filtros
  auk_filter(f_out, overwrite = TRUE)

verao <- read_ebd("data/ebd_prver.txt")

#Plot
ggplot() +
  geom_sf(data = world, color = "gray3", fill = "transparent") +
#Coordenadas da America do Sul  
  coord_sf(xlim = c(-109.4, -26.2), ylim = c(-58.5, 12.6), expand = FALSE)+
#Escala e norte  
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         height = unit(2.0, "cm"),
                         width = unit(2.0, "cm"),
                         pad_x = unit(0.2, "cm"),
                         pad_y = unit(0.3, "cm"),
                         style = north_arrow_fancy_orienteering) +
#Pontos, título da legenda e cores respectivas aos pontos
  geom_point(data = verao, aes(x = longitude, y = latitude, color = "Verão (Out.- Fev.)"), size = 1.2, shape = 19) +
  geom_point(data = inverno, aes(x = longitude, y = latitude, color = "Inverno (Mai.- Jul.)"), size = 1.2, shape = 19)+
  scale_colour_manual(name = "Estação",
                      breaks = c("Verão (Out.- Fev.)", "Inverno (Mai.- Jul.)"),
                      values = c("Verão (Out.- Fev.)" = "firebrick3", "Inverno (Mai.- Jul.)" = "darkturquoise") )+
 #Nome do plot
   ggtitle("Pyrocephalus Rubinus")+
  xlab("Longitude") + ylab("Latitude") +
  #Configuracoes da grade, da legenda e cor de fundo 
  theme(panel.grid.major = element_line(colour = gray(0.6), 
                                        size = 0.5, linetype = "solid"),
        panel.background = element_rect(fill = "gray100"),
        panel.border = element_rect(fill = "NA"),
        legend.title=element_text(size=18),
        legend.text=element_text(size=15),
        text=element_text(family="serif", size=17),
        legend.background = element_rect(fill="white", 
                                         size=0.5, linetype="solid", color = "black"),
        legend.position = "right")

#Salvar plot como png 827x1169 ou pdf tamanho A4 
  
  
  







