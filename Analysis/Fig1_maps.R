# Maps------------

## Libraries -----
library(ggplot2)
library(sf)
library(geosphere)
library(viridis)
library(tidyverse)
library(lwgeom)
library(ggspatial)

## Shape files ----------------- 
# These files should be located on your own computer. Replace directories/files as needed
### LML-----
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/AFRP/shape_files/LML_shape/")
LML_shape <- st_read("World_Lakes.shp")
### FBL ----------
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/AFRP/shape_files/FBL_shape/")
FBL_shape <- st_read("World_Lakes.shp")
### ETL -----------
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/AFRP/shape_files/kml/")
east_shape = st_read("east.kml")
### GNL ----------
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/AFRP/shape_files//ALC/GNL/")
GNL_shape = st_read("World_Lakes.shp")
### PRL ------
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/AFRP/shape_files/ALC/PRL/")
PRL_shape = st_read("World_Lakes.shp")
### POL ------
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/AFRP/shape_files/ALC/POL/")
POL_shape = st_read("World_Lakes.shp")
### CAL ----------
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/AFRP/shape_files/ALC/CAL/")
CAL_shape = st_read("World_Lakes.shp")
### FBL----------
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/AFRP/shape_files/ALC/FBL/")
FBL_shape = st_read("World_Lakes.shp")
### SBL ---------
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/AFRP/shape_files/ALC/SBL/")
SBL_shape = st_read("World_Lakes.shp")
### TBL ---------
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/AFRP/shape_files/ALC/TBL/")
TBL_shape = st_read("World_Lakes.shp")
### FOB --------
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/AFRP/shape_files/ALC/FOB/")
FOB_shape = st_read("World_Lakes.shp")
### CSL ------
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/AFRP/shape_files/ALC/CSL/")
CSL_shape = st_read("World_Lakes.shp")
### USP ----
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/AFRP/shape_files/ALC/USP/")
USP_shape = st_read("Upper Sylvan.kml")
### LSP ----
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/AFRP/shape_files/ALC/LSP/")
LSP_shape = st_read("Lower Sylvan.kml")
### COM ----- 
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/AFRP/shape_files/ALC/COM/")
COM_shape = st_read("Combs.kml")
### TRP----- 
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/AFRP/shape_files/ALC/TRP/")
TRP_shape = st_read("taylor.kml")
### MNP ----- 
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/AFRP/shape_files/ALC/MNP/")
MNP_shape = st_read("Mountain.kml")
### PEP ----- 
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/AFRP/shape_files/ALC/PEP/")
PEP_shape = st_read("Pinchnose.kml")
### WLL ----- 
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/AFRP/shape_files/ALC/WLL/")
WLL_shape = st_read("World_Lakes.shp")
### SDL ----- 
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/AFRP/shape_files/ALC/SDL/")
SDL_shape = st_read("World_Lakes.shp")
### RKP ----- 
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/AFRP/shape_files/ALC/RKP/")
RKP_shape = st_read("Rock Pond.kml")
### HAL ----- 
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/AFRP/shape_files/ALC/HAL/")
HAL_shape = st_read("World_Lakes.shp")
### GEL ----- 
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/AFRP/shape_files/ALC/GEL/")
GEL_shape = st_read("GEL.kml")
### ORL ----- 
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/AFRP/shape_files/ALC/ORL/")
ORL_shape = st_read("World_Lakes.shp")
### JSL ----- 
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/AFRP/shape_files/ALC/JEL/")
JSL_shape = st_read("World_Lakes.shp")


## ALC Map
ggplot() +
  theme_minimal(base_size = 18) +
  # Add North arrow
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
                         style = north_arrow_fancy_orienteering) +
  
  geom_sf(data = east_shape, fill = NA) +
  geom_sf(data = PRL_shape, fill = NA ) +
  geom_sf(data = GNL_shape, fill = NA ) + 
  geom_sf(data = LML_shape, fill = "#154c79") +
  geom_sf(data = POL_shape, fill = NA ) + 
  geom_sf(data = CAL_shape, fill = NA ) + 
  geom_sf(data = FBL_shape, fill = "#154c79") + 
  geom_sf(data = SBL_shape, fill = NA ) + 
  geom_sf(data = TBL_shape, fill = NA ) + 
  geom_sf(data = FOB_shape, fill = NA ) + 
  geom_sf(data = CSL_shape, fill = NA ) + 
  geom_sf(data = MNP_shape, fill = NA ) +
  geom_sf(data = TRP_shape, fill = NA ) +
  geom_sf(data = PEP_shape, fill = NA ) +
  geom_sf(data = COM_shape, fill = NA ) +
  geom_sf(data = USP_shape, fill = NA ) + 
  geom_sf(data = LSP_shape, fill = NA ) +
  geom_sf(data = WLL_shape, fill = NA ) + 
  geom_sf(data = SDL_shape, fill = NA ) + 
  geom_sf(data = RKP_shape, fill = NA ) +
  scale_x_continuous(labels = scales::number_format(accuracy = 0.001)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.001))

## Col mapping to match temporal diversity figures 
col_join = data.frame(labels = c("Rock", "Wood + Rock",
                                "Fine Sediment","Wood + Fine Sediment"),
          colors = pal[1:4],
          Habitat = c("R", "RW","S", "SW"))

## Individual lake and habitat maps -------------
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/crispy-bassoon")


FBL.shape = read.csv("Data/CCA_data/FBL_shape.csv") %>%
  select(-X) %>% rownames_to_column(var = "row")
FBL.shape[dim(FBL.shape)+1,] =FBL.shape[1,] 

LML.shape = read.csv("Data/CCA_data/LML.shape.csv") %>%
  select(-X) %>% 
  rownames_to_column(var = "row") %>% 
  unique()
LML.shape[dim(LML.shape) +1, ] = LML.shape[1,]



### FBL Individual Map -----------

fbl.sites = read.csv("Data/CCA_data/FBL_SiteLengths.csv")



fbl.graph = fbl.sites %>% left_join(FBL.shape)%>%
  left_join(FBL.shape, by = c("ID2" ="ID1")) %>% filter(water == "FBL")


habs = read.csv("Data/habs.csv") 

fbl.colors = habs %>% filter(WATER == "FBL")

#colors = c("#254f5c","#e1b83c","#de7e43","#2b4155") 

#colors = pal[1:4]
#col_join = data.frame(Habitat = unique(fbl.colors$Habitat), colors)
fbl.legend = (left_join(fbl.colors, col_join ))$colors


## For loop for map


fbl.graph.geom = ggplot() +
  geom_polygon(data = FBL.shape, aes(y = lat1, x = lon1), fill = "darkgray", alpha = .3)+
  # Add North arrow
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
                         style = north_arrow_fancy_orienteering)   + 
  theme_minimal()



sites = unique(fbl.sites$site)[1:15]

for(i in 1:length(sites)){
  
  fbl.graph.geom = fbl.graph.geom + 
    geom_path(data = FBL.shape[fbl.graph[i,"row.x"] : fbl.graph[i,"row.y"],], 
              aes(y = lat1, x = lon1),
              col = fbl.legend[i],
              size = 1) 
  
}
point.dat = fbl.sites %>% 
  left_join(FBL.shape) %>%
  filter(water == "FBL") %>%
  left_join(habs)

fbl.graph.geom  +
  geom_path(data = rbind(FBL.shape[271:277,], FBL.shape[1:9,])  , 
            aes(y = lat1, x = lon1),
            col = fbl.legend[1],
            size = 1) +
 # geom_point(data = point.dat, aes(x = lon1,y = lat1),   size = 8, shape = "+") +
  scale_color_manual(values = col_join$colors) +
  guides(color = guide_legend(override.aes = list(shape = 15, size = 8))) +
  #geom_point(data = point.dat, aes(x = lon1,y = lat1),col = "black",  size = 3, shape = "+") +
  xlab("") +
  ylab("")




### LML Individual Map -----------

## Get site boundaries
lml.sites = read.csv("Data/CCA_data/LML_SiteLenghts.csv") %>%
  filter(water == "LML")
## Get points
lml.graph = lml.sites %>% left_join(LML.shape) %>%
  left_join(LML.shape, by = c("ID2" ="ID1")) %>%
  unique()



habs = read.csv("Data/habs.csv") 

lml.colors = habs %>% filter(WATER == "LML")
colors = c("#254f5c","#e1b83c","#de7e43","red") 
colors = pal[1:4]
col_join = data.frame(Habitat = unique(fbl.colors$Habitat), colors)
lml.legend = (left_join(lml.colors, col_join ))$colors

## For loop for map


lml.graph.geom = ggplot() +
  geom_polygon(data = LML.shape, aes(y = lat1, x = lon1), fill = "darkgray", alpha = .2) +
  # Add North arrow
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
                         style = north_arrow_fancy_orienteering)   + 
  theme_minimal()




sites = (1:length(unique(lml.sites$site)[1:32]))[-23]



for(i in sites){
  

    lml.graph.geom = lml.graph.geom + 
    geom_path(data = LML.shape[lml.graph[i,"row.x"] : lml.graph[i,"row.y"],], 
              aes(y = lat1, x = lon1),
              col = lml.legend[i],
              size = 1, col = "gray") 
  
}

## Need to check out site 18 and change the color of red 
lml.graph.geom + 
  geom_path(data = rbind(LML.shape[lml.graph[23,"row.y"] : dim(LML.shape)[1],], LML.shape[1,]), 
            aes(y = lat1, x = lon1),
            col = lml.legend[i],
            size = 1) +
  #geom_point(data = point.dat, aes(x = lon1,y = lat1),col = "black", shape = "+", size = 8)  +
  scale_color_manual(values = col_join$colors) +
 # guides(color = guide_legend(override.aes = list(shape = 15, size = 4))) +
  xlab("") +
  ylab("")



LML.shape[lml.graph[23,"row.x"] : lml.graph[23,"row.y"],]



point.dat = lml.sites %>% 
  left_join(LML.shape) %>%
  filter(water == "LML") %>%
  left_join(habs)



## Trying to add recapture data to the LML map

lml.graph %>% 
  left_join(circle_points, by= c("site" = "site_capture"))
circle_points

point.dat = lml.sites %>% 
  left_join(LML.shape) %>%
  filter(water == "LML") %>%
  left_join(habs, by = c("site"="SITE_N"))  %>% 
  left_join(circle_points %>%
              select(-x, -y), by= c("site" = "site_capture"))


start_end = same_site %>%
  filter(is.na(TAG2_R) == F) %>%
  filter(same.site == F) %>%
  group_by(site_capture, site_release) %>%
  summarize(total_moves = n()) %>%
  left_join(point.dat, 
            by = c("site_release" = "site" )) %>%
  rename(x.release = lon1, 
         y.release = lat1) %>%
  left_join(point.dat, 
            by = c("site_capture" = "site")) %>%
  rename(x.capture = lon1, y.capture = lat1) 


## Need to check out site 18 and change the color of red 
lml.graph.geom + 
  geom_path(data = rbind(LML.shape[lml.graph[23,"row.y"] : dim(LML.shape)[1],], LML.shape[1,]), 
            aes(y = lat1, x = lon1),
            col = "gray",
            size = 1) +
  geom_point(data = point.dat, aes(x = lon1,y = lat1, col = prop_recaptured, ), size = 10) +
  #geom_point(data = point.dat, aes(x = lon1,y = lat1, size = prop_recaptured),col = "black")  +
  xlab("") +
  ylab("") +
  scale_color_viridis_c() + 
 geom_segment(data = start_end, aes(x = x.release, y = y.release, xend = x.capture, yend = y.capture, lwd = total_moves), arrow = grid::arrow(length = unit(0.2, "cm"))) + 
  theme_minimal()







