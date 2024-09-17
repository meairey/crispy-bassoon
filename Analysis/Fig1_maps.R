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
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/LML_SMB_removal/Data/LML_shape")
LML_shape <- st_read("World_Lakes.shp")
### FBL ----------
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/LML_SMB_removal/Data/FBL_shape")
FBL_shape <- st_read("World_Lakes.shp")
### ETL -----------
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/LML_SMB_removal/Data/kml/")
east_shape = st_read("east.kml")
### GNL ----------
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/LML_SMB_removal/Data/ALC/GNL/")
GNL_shape = st_read("World_Lakes.shp")
### PRL ------
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/LML_SMB_removal/Data/ALC/PRL/")
PRL_shape = st_read("World_Lakes.shp")
### POL ------
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/LML_SMB_removal/Data/ALC/POL/")
POL_shape = st_read("World_Lakes.shp")
### CAL ----------
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/LML_SMB_removal/Data/ALC/CAL/")
CAL_shape = st_read("World_Lakes.shp")
### FBL----------
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/LML_SMB_removal/Data/ALC/FBL/")
FBL_shape = st_read("World_Lakes.shp")
### SBL ---------
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/LML_SMB_removal/Data/ALC/SBL/")
SBL_shape = st_read("World_Lakes.shp")
### TBL ---------
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/LML_SMB_removal/Data/ALC/TBL/")
TBL_shape = st_read("World_Lakes.shp")
### FOB --------
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/LML_SMB_removal/Data/ALC/FOB/")
FOB_shape = st_read("World_Lakes.shp")
### CSL ------
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/LML_SMB_removal/Data/ALC/CSL/")
CSL_shape = st_read("World_Lakes.shp")
### USP ----
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/LML_SMB_removal/Data/ALC/USP/")
USP_shape = st_read("Upper Sylvan.kml")
### LSP ----
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/LML_SMB_removal/Data/ALC/LSP/")
LSP_shape = st_read("Lower Sylvan.kml")
### COM ----- 
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/LML_SMB_removal/Data/ALC/COM/")
COM_shape = st_read("Combs.kml")
### TRP----- 
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/LML_SMB_removal/Data/ALC/TRP/")
TRP_shape = st_read("taylor.kml")
### MNP ----- 
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/LML_SMB_removal/Data/ALC/MNP/")
MNP_shape = st_read("Mountain.kml")
### PEP ----- 
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/LML_SMB_removal/Data/ALC/PEP/")
PEP_shape = st_read("Pinchnose.kml")
### WLL ----- 
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/LML_SMB_removal/Data/ALC/WLL/")
WLL_shape = st_read("World_Lakes.shp")
### SDL ----- 
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/LML_SMB_removal/Data/ALC/SDL/")
SDL_shape = st_read("World_Lakes.shp")
### RKP ----- 
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/LML_SMB_removal/Data/ALC/RKP/")
RKP_shape = st_read("Rock Pond.kml")
### HAL ----- 
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/LML_SMB_removal/Data/ALC/HAL/")
HAL_shape = st_read("World_Lakes.shp")
### GEL ----- 
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/LML_SMB_removal/Data/ALC/GEL/")
GEL_shape = st_read("GEL.kml")
### ORL ----- 
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/LML_SMB_removal/Data/ALC/ORL/")
ORL_shape = st_read("World_Lakes.shp")
### JSL ----- 
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/LML_SMB_removal/Data/ALC/JEL/")
JSL_shape = st_read("World_Lakes.shp")


## ALC Map
ggplot() +
  theme_minimal() +
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
  geom_sf(data = RKP_shape, fill = NA ) 

## Individual lake and habitat maps -------------
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/crispy-bassoon")
FBL.shape = read.csv("Data/FBL_shape.csv") %>%
  select(-X) %>% rownames_to_column(var = "row")
FBL.shape[dim(FBL.shape)+1,] =FBL.shape[1,] 

LML.shape = read.csv("Data/LML.shape.csv") %>%
  select(-X) %>% 
  rownames_to_column(var = "row") %>% 
  unique()
LML.shape[dim(LML.shape) +1, ] = LML.shape[1,]

### FBL Individual Map -----------

fbl.sites = read.csv("Data/FBL_SiteLengths.csv")



fbl.graph = fbl.sites %>% left_join(FBL.shape)%>%
  left_join(FBL.shape, by = c("ID2" ="ID1")) %>% filter(water == "FBL")


habs = read.csv("Data/habs.csv") 

fbl.colors = habs %>% filter(WATER == "FBL")
colors = c("#254f5c","#e1b83c","#de7e43","#2b4155") 
col_join = data.frame(Habitat = unique(fbl.colors$Habitat), colors)
fbl.legend = (left_join(fbl.colors, col_join ))$colors





## For loop for map


fbl.graph.geom = ggplot() +
  geom_polygon(data = FBL.shape, aes(y = lat1, x = lon1), fill = "#154c79", alpha = .1)+
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
  geom_point(data = point.dat, aes(x = lon1,y = lat1, color = Habitat),   size = 3) +
  scale_color_manual(values = col_join$colors) +
  guides(color = guide_legend(override.aes = list(shape = 15, size = 4))) +
  geom_point(data = point.dat, aes(x = lon1,y = lat1),col = "black",  size = 3) +
  xlab("") +
  ylab("")




### LML Individual Map -----------

## Get site boundaries
lml.sites = read.csv("Data/LML_SiteLenghts.csv") %>%
  filter(water == "LML")
## Get points
lml.graph = lml.sites %>% left_join(LML.shape) %>%
  left_join(LML.shape, by = c("ID2" ="ID1")) %>%
  unique()



habs = read.csv("Data/habs.csv") 

lml.colors = habs %>% filter(WATER == "LML")
colors = c("#254f5c","#e1b83c","#de7e43","red") 
col_join = data.frame(Habitat = unique(fbl.colors$Habitat), colors)
lml.legend = (left_join(lml.colors, col_join ))$colors

## For loop for map


lml.graph.geom = ggplot() +
  geom_polygon(data = LML.shape, aes(y = lat1, x = lon1), fill = "#154c79", alpha = .1)+
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
              size = 1) 
  
}

## Need to check out site 18 and change the color of red 
lml.graph.geom + 
  geom_path(data = rbind(LML.shape[lml.graph[23,"row.y"] : dim(LML.shape)[1],], LML.shape[1,]), 
            aes(y = lat1, x = lon1),
            col = lml.legend[i],
            size = 1) +
  geom_point(data = point.dat, aes(x = lon1,y = lat1),col = "black",  size = 3)  +
  xlab("") +
  ylab("")



LML.shape[lml.graph[23,"row.x"] : lml.graph[23,"row.y"],]



point.dat = lml.sites %>% 
  left_join(LML.shape) %>%
  filter(water == "LML") %>%
  left_join(habs)

lml.graph.geom  +
  geom_point(data = point.dat, aes(x = lon1,y = lat1, color = Habitat),   size = 3) +
  scale_color_manual(values = col_join$colors) +
  guides(color = guide_legend(override.aes = list(shape = 15, size = 4))) +
  geom_point(data = point.dat, aes(x = lon1,y = lat1),col = "black",  size = 3) +
  xlab("") +
  ylab("")

