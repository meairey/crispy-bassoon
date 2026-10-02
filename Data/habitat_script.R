`%nin%` = Negate(`%in%`)
# Replace "your_shapefile.shp" with the path to your shapefile
library(ggplot2)
library(sf)
library(geosphere)
library(viridis)
library(tidyverse)

library(lwgeom)

habs = read.csv("Data/habs.csv") %>% 
  select(-X)


## Set up LML shape file ---------------
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/LML_SMB_removal/Data/LML_shape")
LML_shape <- st_read("World_Lakes.shp")

## Set up FBL shape file ------------------
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/LML_SMB_removal/Data/FBL_shape")

FBL_shape <- st_read("World_Lakes.shp")


## Hab data classifications -----------------------


### Functions ---------------------
# Function to calculate distance matrix
dist_matrix <- function(df) {
  as.matrix(dist(df[, c("lat1", "lon1")]))
}

# Reorder points based on nearest neighbor
reorder_points <- function(df) {
  dists <- dist_matrix(df)
  order <- numeric(nrow(df))
  visited <- logical(nrow(df))
  
  # Start with the first point
  current <- 1
  order[1] <- current
  visited[current] <- TRUE
  
  for (i in 2:nrow(df)) {
    # Find nearest unvisited point
    nearest <- which.min(dists[current, !visited])
    current <- which(!visited)[nearest]
    order[i] <- current
    visited[current] <- TRUE
  }
  
  df[order, ]
}


### Dataset

# Set working directry
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/crispy-bassoon/")

## gps points from habitat features
gps = read.csv("Data/CCA_data/garmin_lml_hab_day2.csv")

# set of points to create pairs for joining gps and features together
points = data.frame(ID1 = rep(1:1100, each = 1100), 
                    ID2 = rep(1:1100, by = 1100))

# Calcualting distance between all pairs of points 
distance_pairs = points %>% 
  left_join(gps, by = c("ID1" = "ID")) %>%
  rename("lat1" = "lat", 
         "lon1" = "lon", 
         "ele1" = "ele",
         "name1" = "name") %>%
  left_join(gps, by = c("ID2" = "ID")) %>% 
  rename("lat2" = "lat", 
         "lon2" = "lon", 
         "ele2" = "ele",
         "name2" = "name") %>%
  mutate(dist = distHaversine(cbind(lon1, lat1), cbind(lon2, lat2))) 

## Read in the data sheets and replace any codes that need changing
substrate = read.csv("Data/CCA_data/habitat_class1.csv") %>% 
  filter(is.na(start) == F) %>%
  filter(MEA == "MEA") %>%
  #filter(density > 2) %>%
  mutate(feature = str_replace(feature, "A", "SV")) %>%
  mutate(feature = str_replace(feature, "SBR","B")) %>%
  mutate(feature = str_replace(feature, "SCS", "SC")) %>%
  mutate(feature = str_replace(feature, "SB", "B")) %>%
  mutate(feature = str_replace(feature, "SCD", "SC"))  %>%
  mutate(feature = str_replace(feature, "GRSVVEL", "G")) %>%
  mutate(feature = str_replace(feature, "SC", "C")) %>%
  mutate(feature = str_replace(feature, "GRSVVEL", "G")) %>%
  mutate(feature = str_replace(feature, "SW", "SV")) %>%
  mutate(feature = str_replace(feature, "SE", "SV"))

## Now join the distances with the substrate assignments to calculate total habitat lengths
whole = substrate %>%
  mutate(end= as.numeric(end), 
         start = as.numeric(start)) %>%
  left_join(distance_pairs, by = c("start" = "ID1", "end" = "ID2")) 



## Site_length for FBL
site_lengths = substrate %>% group_by(water, site) %>%
  mutate(end = as.numeric(end)) %>% 
  summarize(ID1 = min(start),
            ID2= max(end, na.rm = T)) %>%
  mutate(ID2.1 = case_when(site == "BEF.FBL.009" ~ 1022, ## End points
                           site == "BEF.FBL.010" ~ 1023,
                           site == "BEF.FBL.011" ~ 1024,
                           site == "BEF.FBL.012" ~ 1025,
                           site == "BEF.FBL.013" ~ 1026, 
                           site == "BEF.FBL.014" ~ 1027, 
                           site == "BEF.FBL.015" ~ 1028,
                           site == "BEF.FBL.001" ~ 246), 
         ID1.1 = case_when(site == "BEF.FBL.010" ~ 1022, ## Start points
                           site == "BEF.FBL.011" ~ 1023,
                           site == "BEF.FBL.012" ~ 1024,
                           site == "BEF.FBL.013" ~ 1025, 
                           site == "BEF.FBL.014" ~ 1026, 
                           site == "BEF.FBL.015" ~ 1027,
                           site == "BEF.FBL.001" ~ 1028)) %>%
  mutate(ID1 = case_when(ID1.1 > 1000 ~ ID1.1, TRUE ~ ID1),
         ID2 = case_when(ID2.1 > 1005 ~ ID2.1, ID2.1 == 246 ~ ID2.1, TRUE ~ ID2)) %>%
  
  left_join(distance_pairs, by = c("ID1", "ID2")) %>%
  select(site, ID1, ID2, dist) %>%
  rename("shoreline" = "dist") 



### FBL ----------------------------
FBL_ids = substrate %>% 
  filter(water == "FBL", MEA == "MEA") %>%
  
  mutate(end = as.numeric(end)) %>% 
  pivot_longer(c(start, end),
               names_to = "class", 
               values_to = "name") %>%
  select(water, name) %>% 
  unique() %>%
  na.omit()


# Create dataframe for reordering
df= data.frame(ID1 = c(1:1100)) %>%
  mutate(ID2 = lag(ID1)) %>% 
  left_join(gps, by = c("ID1" = "ID")) %>%
  rename("lat1" = "lat", 
         "lon1" = "lon", 
         "ele1" = "ele",
         "name1" = "name") %>%
  left_join(gps, by = c("ID2" = "ID")) %>% 
  rename("lat2" = "lat", 
         "lon2" = "lon", 
         "ele2" = "ele",
         "name2" = "name") %>%
  mutate(dist = distHaversine(cbind(lon1, lat1), cbind(lon2, lat2))) %>%
  select(ID1, ID2, dist, lat1, lon1) %>%
  filter(ID1 %in% FBL_ids$name | ID2 %in% FBL_ids$name, 
         lat1 < 43.625) %>% 
  select(lat1, lon1, ID1) %>%
  na.omit()

# Apply reordering
df_ordered <- reorder_points(df)


## Write a csv with the ordered points for a shape file
#write.csv(df_ordered, "Data/FBL_shape.csv")

# Plot with geom_path
ggplot(df_ordered, aes(y = lat1, x = lon1)) +
  geom_path() +
  labs(title = "Ordered Path")

site_lengths = site_lengths %>% na.omit()

(site_lengths %>% filter(water == "FBL"))$shoreline %>% sum()

## Write a csv for site lengths for FBL shape file 
#write.csv(site_lengths, "Data/FBL_SiteLengths.csv")


for(i in 1:length(site_lengths$ID1)){
  i = 13
  if(i == 1){
    start = which(df_ordered$ID1 == site_lengths$ID1[i])
    end = which(df_ordered$ID1 == site_lengths$ID2[i])
    l = dim(df_ordered)[1]
    frame  = rbind(df_ordered[start:l, ],df_ordered[1:end,]) %>%
      as.data.frame() %>%
      mutate(dist = distHaversine(cbind(lon1, lat1), cbind(lag(lon1), lag(lat1)))) %>%
      na.omit()
    site_lengths$shoreline[i] = sum(frame$dist)
  }else{
    
    start = which(df_ordered$ID1 == site_lengths$ID1[i]) -1
    end = which(df_ordered$ID1 == site_lengths$ID2[i])
    
    frame = df_ordered[start:end,]   %>%
      mutate(dist = distHaversine(cbind(lon1, lat1), cbind(lag(lon1), lag(lat1)))) %>%
      na.omit()
    
    site_lengths$shoreline[i] = sum(frame$dist)
    
  }
} 

whole %>%
  filter(water == "FBL") %>%
  left_join(site_lengths %>% filter(water == "FBL"), by = "site")%>%
  group_by(site, feature, shoreline) %>%
  select(dist, everything()) %>% 
  summarize(total_hab = sum(dist)) %>%
  mutate(percent_shoreline = total_hab / shoreline * 100)  %>%
  separate(site, into = c("gear","water","site")) %>%
  mutate(percent_shoreline = case_when(percent_shoreline > 100 ~ 100, 
                                       TRUE ~ percent_shoreline)) %>%
  ggplot(aes(x = site, y = percent_shoreline, fill =  feature)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~feature) 



## Creating some variable for coarse woody debris 

tree_density_habitat = whole %>% left_join(site_lengths, by = "site") %>%
  filter(feature == "CW") %>%
  group_by(site, feature, shoreline) %>%
  summarize(total_hab = sum(dist)) %>%
  mutate(total.tree.count = 10 * (total_hab)/50) %>%  ## If I assume there are 10 trees in 50 m of dense tree habitat
  mutate(MEA = "MEA")


wood_counts = read.csv("Data/CCA_data/habitat_class1.csv") %>% 
  group_by(water, site, feature)%>%
  filter(feature == "CW")  %>%
  
  #filter(is.na(start) == T) %>%
  filter(MEA == "MEA") %>%
  left_join(tree_density_habitat) %>%
  summarize(sum_feature = n()) %>%
  ungroup() %>%
  group_by(water) %>%
  complete(site, feature) %>%    
  mutate(across(everything(), ~ replace_na(.x, 0))) %>%
  filter(feature == "CW") %>%
  ungroup() %>%
  left_join(site_lengths %>% ## Calculate the number of trees per 
              select(site, shoreline)) %>%
  mutate(density = (sum_feature / shoreline )* 100) %>%
   mutate(CW = case_when(
    density == 0 ~ 0,  # zeros are rank 0
    TRUE ~ as.numeric(cut(
      density,
      breaks = c(-Inf,
                 quantile(density[density > 0], probs = c(0.2,0.4,0.6,0.8)),
                 Inf),
      labels = 1:5,
      right = TRUE
    )) - 1  # subtract 1 so non-zero ranks are 1–4
  )) %>%
  select(water, site, CW)
  
  
  


  





## Creating little table for the CCA
env_updated_FBL = whole %>% left_join(site_lengths, by = "site") %>%
  group_by(site, feature, shoreline) %>%
  summarize(total_hab = sum(dist)) %>%
  mutate(percent_shoreline = total_hab / shoreline * 100)  %>%
  #separate(site, into = c("gear", "water", "site_n"), remove = F) %>%
  select(site,  feature, percent_shoreline) %>%
  mutate(f_score = .bincode(percent_shoreline, breaks = c(0,25,50,75,110))) %>%
  select(-percent_shoreline)%>% 
  
  pivot_wider(values_from = f_score, names_from = feature) %>%    
  mutate(across(everything(), ~ replace_na(.x, 0))) %>%
  rename("SITE_N" = "site") %>%
  select(-CW) %>%
  mutate(water = "FBL") %>%
  left_join(wood_counts , by = c("SITE_N" = "site", "water")) %>%
  filter(water == "FBL") %>%
  mutate(CW = ifelse(is.na(CW), 0, CW)) %>%
  select(water, everything())
  

#write.csv(env_updated_FBL, "Data/FBL_habitat.csv") ## Write file

## Trying to come up with more nuanced habitat designations
## Trying to decide on new hab classes save below
FBL_updated_sub =env_updated_FBL  %>%
  left_join(habs)  %>% 
  select(SITE_N,Habitat,  C, B, G, EV, CW) %>%
  mutate(rock_habitat = pmax(C, G)) %>%
    mutate(macrohab = case_when(
    (B <= 1 & C <= 1) ~ "S", 
    (B <= 1 & C >= 2) ~ "RS", ## cobble fields with low structure,
    (B >= 2 | C > 3) ~ "R" ## Rock shoals with higher structure or higher density cobbles 
    
  )) %>%
  mutate(new_hab = case_when(CW >= 1 ~ paste(macrohab, "W", sep = ""), 
                             CW < 1 ~ macrohab) ) %>%
  select(SITE_N, Habitat, new_hab) %>% 
  filter(grepl("FBL", SITE_N)) %>%
  mutate(same.new = case_when(new_hab == Habitat ~ "T",
                              new_hab == "RS" & Habitat == "S" ~ "Mixed sub",
                              new_hab == "RSW" & Habitat == "RW" ~ "MS + WD",
                              new_hab == "R" & Habitat == "S" ~ "F", 
                              new_hab == "S" & Habitat == "R" ~ "F", 
                              new_hab == "RW" & Habitat == "R" ~ "Wood diff", 
                              new_hab == "SW" & Habitat == "S" ~ "Wood diff", 
                              new_hab == "S" & Habitat == "SW"~ "Wood diff",
                              new_hab == "R" & Habitat == "RW" ~ "Wood diff",
                              new_hab == "RS" & Habitat == "SW" ~ "MS + WD",
                              new_hab == "RSW" & Habitat == "SW" ~ "Mixed sub",
                              new_hab == "RSW" & Habitat == "S" ~ "MS + WD", 
                              new_hab == "S" & Habitat == "RW" ~ "F"))
  
#write.csv(FBL_updated_sub, "Data/updated_habs_testing.FBL.csv", row.names = F)

env_updated_long.fbl = env_updated_FBL %>%
  filter(grepl("FBL", SITE_N)) %>%
  pivot_longer(
    cols = C:CW,            # all habitat columns
    names_to = "Habitat",
    values_to = "Rank"
  ) %>% filter(Habitat %nin% c("S", "G", "R")) %>%
  mutate(
    Habitat = factor(Habitat, levels = c("C","B","BED","EV","SV","O","CW","FW")),
    SITE_N = factor(SITE_N, levels = unique(SITE_N))  # keeps original order
  ) %>%
  mutate(SITE_N = factor(SITE_N, levels = rev(levels(SITE_N))))

# Column-wise heatmap
ggplot() +
  geom_tile(data = env_updated_long.fbl, aes(x = Habitat, y = SITE_N, fill = Rank), color = "white") + # tiles with white borders
  scale_fill_gradient(low = "white", high = "steelblue", na.value = "grey90") +
  labs(x = "Habitat Feature", y = "Site", fill = "Rank") +
  geom_text(data = env_updated_long.fbl, 
            aes(x = Habitat, y = SITE_N, label = as.character(Rank)), size = 3, col = "black") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(hjust = .5, size = 8),
    axis.text.y = element_text(size = 9),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "bottom",
    panel.grid.major = element_blank(),  # remove major grid lines
    panel.grid.minor = element_blank()) +
  scale_x_discrete(position = "top", labels = c("C" = "Cobble", "B" = "Boulder", "BED" = "Bedrock", "EV" = "Emergent\nvegetation",
                                                "SV" = "Submerged\nvegetation", "O" = "Organic\nmatter", "CW" = "Coarse woody\ndebris",
                                                "FW" = "Fine\nwoody debris")) +
   geom_text(data = (habs %>% filter(WATER == "FBL"))[1:15,], aes(y = SITE_N, x = "Old\nhabitat", label = Habitat), size = 3) +
  geom_text(data = FBL_updated_sub, aes(y = SITE_N,  x = "New\nhabitat", label = new_hab),  size = 3) +
  geom_text(data = FBL_updated_sub, aes(y = SITE_N, x = "Difference", label = same.new), size = 3)

ggsave(file = "Figures_Tables/Testing Habitat/FBL_habitat_table.jpeg", width = 8, height = 5, dpi = 600) 


## ------------------ LML ------------------- 

# Set working directory
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/crispy-bassoon/")
# Pull together df with substrate information and waypoints
substrate = read.csv("Data/CCA_data/habitat_class1.csv") %>% 
  filter(water == "LML") %>%
  filter(is.na(start) == F)  %>%
  mutate(feature = str_replace(feature, "A", "SV")) %>%
  mutate(feature = str_replace(feature, "SBR","B")) %>%
  mutate(feature = str_replace(feature, "SCS", "SC")) %>%
  mutate(feature = str_replace(feature, "SB", "B")) %>%
  mutate(feature = str_replace(feature, "SCD", "SC"))%>%
  mutate(feature = str_replace(feature, "SC", "C")) %>%
  mutate(feature = str_replace(feature, "GRSVVEL", "G")) %>%
  mutate(feature = str_replace(feature, "SW", "SV")) %>%
  mutate(feature = str_replace(feature, "SE", "SV"))

# get the start and end points for each site
site_lengths = substrate %>% group_by(water, site) %>%
  mutate(end = as.numeric(end)) %>% 
  summarize(ID1 = min(start),
            ID2= max(end, na.rm = T)) %>% filter(water == "LML")
# IDs for each site
LML_ids = substrate %>% 
  filter(water == "LML") %>%
  mutate(end = as.numeric(end)) %>% 
  pivot_longer(c(start, end),
               names_to = "class", 
               values_to = "name") %>%
  select(water, name) %>% 
  unique() %>%
  na.omit()

# Create dataframe for reordering
df= data.frame(ID1 = c(1:1000)) %>%
  mutate(ID2 = lag(ID1)) %>% 
  left_join(gps, by = c("ID1" = "ID")) %>%
  rename("lat1" = "lat", 
         "lon1" = "lon", 
         "ele1" = "ele",
         "name1" = "name") %>%
  left_join(gps, by = c("ID2" = "ID")) %>% 
  rename("lat2" = "lat", 
         "lon2" = "lon", 
         "ele2" = "ele",
         "name2" = "name") %>%
  mutate(dist = distHaversine(cbind(lon1, lat1), cbind(lon2, lat2))) %>%
  select(ID1, ID2, dist, lat1, lon1) %>%
  filter(ID1 %in% LML_ids$name | ID2 %in% LML_ids$name, 
         lat1 > 43.65 & lat1 < 43.705) %>% select(lat1, lon1, ID1)


# Apply reordering
df_ordered <- reorder_points(df)


# Write csv for LML shape
#write.csv(df_ordered, "Data/LML.shape.csv")


site_lengths = site_lengths %>% na.omit()

site_lengths %>% print(n = 100)

site_lengths[21, "ID1"] = 665


gps %>% filter(ID == 678) 
   



# write csv for LML sites
#write.csv(site_lengths, "Data/LML_SiteLenghts.csv")
#write.csv(site_lengths, "Data/LML_SiteLenghts.csv")
for(i in 1:length(site_lengths$ID1)){
  
  frame = df_ordered %>% filter(ID1 <= site_lengths$ID2[i] & ID1 >= site_lengths$ID1[i] ) %>%
    mutate(dist = distHaversine(cbind(lon1, lat1), cbind(lag(lon1), lag(lat1)))) %>%
    na.omit()
  
  site_lengths$shoreline[i] = sum(frame$dist)
  
  
}  



whole = substrate %>%
  mutate(end= as.numeric(end), 
         start = as.numeric(start)) %>%
  left_join(distance_pairs, by = c("start" = "ID1", "end" = "ID2")) 



whole_lml = whole %>%
  #filter(water == "LML") %>%
  left_join(site_lengths %>% 
              filter(water == "LML"),
            by = "site") %>%
  #filter(density > 1) %>% ## Filter out low density habitats
  group_by(site, feature, shoreline) %>%
  summarize(total_hab = sum(dist)) %>%
  mutate(percent_shoreline = total_hab / shoreline * 100)  %>%
  separate(site, into = c("gear","site_num"), remove = F)

whole_lml  %>%
  separate(site, into = c("gear", "wate", "site")) %>%
  mutate(site_num = parse_number(site))  %>%
  ggplot(aes(x = site_num, y = percent_shoreline, fill =  feature)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~feature) 

## Plotting Little Moose
ggplot() +
  geom_path(data = df_ordered, aes(x = lat1, y = lon1)) +
  geom_path(data = df_ordered %>% filter(ID1 <= 404 & ID1 >=382), 
            aes(x = lat1, y = lon1),
            col = "red") +
  scale_x_reverse()

site_lengths$shoreline %>% sum()

 


## Creating little table for the CCA
env_updated_LML = whole_lml %>% left_join(site_lengths, by = "site") %>%
  select(site,  feature, percent_shoreline) %>%
  mutate(f_score = .bincode(percent_shoreline, breaks = c(0,25,50,75,110))) %>%
  select(-percent_shoreline) %>%
  pivot_wider(values_from = f_score, names_from = feature) %>%    
  mutate(across(everything(), ~ replace_na(.x, 0))) %>%
  rename("SITE_N" = "site") %>%
  left_join(wood_counts) %>% ## Join in the woody debris data table
  filter(water == "LML") %>%
  select(-water, -site)

## Saved old criteria as im playing around with this



updated_habs.LML= env_updated_LML %>%
  left_join(habs) %>% 
  select(SITE_N,Habitat,  C, B, G, EV, CW) %>%
  unique() %>%
  mutate(rock_habitat = pmax(C, G)) %>%
    mutate(macrohab = case_when(
    (B <= 1 & C <= 1) ~ "S", 
    (B <= 1 & C >= 2) ~ "RS", ## cobble fields with low structure,
    (B >= 2 | C >= 3) ~ "R" ## Rock shoals with higher structuure or higher density cobbles 
    
  )) %>%
  mutate(new_hab = case_when(CW >= 1 ~ paste(macrohab, "W", sep = ""), 
                             CW < 1 ~ macrohab) ) %>%
  select(SITE_N, Habitat, new_hab) %>% 
  unique() %>%
  mutate(same = (
    (grepl("R", Habitat) & grepl("R", new_hab)) |
    (grepl("S", Habitat) & grepl("S", new_hab))
  ) & new_hab != "RS") %>%
  mutate(same.new = case_when(new_hab == Habitat ~ "T",
                              new_hab == "RS" & Habitat == "S" ~ "Mixed sub",
                              new_hab == "RSW" & Habitat == "RW" ~ "MS + WD",
                              new_hab == "R" & Habitat == "S" ~ "F", 
                              new_hab == "S" & Habitat == "R" ~ "F", 
                              new_hab == "RW" & Habitat == "R" ~ "Wood diff", 
                              new_hab == "SW" & Habitat == "S" ~ "Wood diff", 
                              new_hab == "S" & Habitat == "SW"~ "Wood diff",
                              new_hab == "R" & Habitat == "RW" ~ "Wood diff",
                              new_hab == "RS" & Habitat == "SW" ~ "MS + WD",
                              new_hab == "RSW" & Habitat == "SW" ~ "Mixed sub",
                              new_hab == "RSW" & Habitat == "S" ~ "MS + WD"))


write.csv(updated_habs.LML, "Data/updated_habs_LML.csv", row.names = F) ## New habitat data


write.csv(env_updated_LML, "Data/LML_habitat.csv")


env_updated_long.LML <- env_updated_LML %>%
  select(-BED) %>%
  filter(grepl("LML", SITE_N)) %>%
  pivot_longer(
    cols = R:CW,            # all habitat columns
    names_to = "Habitat",
    values_to = "Rank"
  ) %>% filter(Habitat %nin% c("S", "G", "R")) %>%
  mutate(
    Habitat = factor(Habitat, levels = c("C","B","BED","EV","SV","O","CW","FW")),
    SITE_N = factor(SITE_N, levels = unique(SITE_N))  # keeps original order
  ) %>%
  mutate(SITE_N = factor(SITE_N, levels = rev(levels(SITE_N))))

# Column-wise heatmap
ggplot() +
  geom_tile(data = env_updated_long.LML, aes(x = Habitat, y = SITE_N, fill = Rank),color = "white") + # tiles with white borders
  scale_fill_gradient(low = "white", high = "steelblue", na.value = "grey90") +
  labs(x = "Habitat Feature", y = "Site", fill = "Rank") +
  geom_text(data = env_updated_long.LML, 
            aes(x = Habitat, y = SITE_N, label = as.character(Rank)), size = 3, col = "black") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(hjust = .5, size = 8),
    axis.text.y = element_text(size = 9),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "bottom",
    panel.grid.major = element_blank(),  # remove major grid lines
    panel.grid.minor = element_blank()) +
  scale_x_discrete(position = "top",labels = c("C" = "Cobble", "B" = "Boulder", "BED" = "Bedrock", "EV" = "Emergent\nvegetation",
                                                "SV" = "Submerged\nvegetation", "O" = "Organic\nmatter", "CW" = "Coarse woody\ndebris",
                                                "FW" = "Fine\nwoody debris")) +
  
  geom_text(data = updated_habs.LML, aes(y = SITE_N, x = "Old\nhabitat", label = Habitat), size = 3) +
  geom_text(data = updated_habs.LML, aes(y = SITE_N,  x = "New\nhabitat", label = new_hab),  size = 3) +
  
  geom_text(data = updated_habs.LML, aes(y = SITE_N, x = "Old\nhabitat", label = Habitat), size = 3) + 
  geom_text(data = updated_habs.LML, aes(y = SITE_N, x = "Difference", label = same.new), size = 3)

ggsave(file = "Figures_Tables/Testing Habitat/LML_habitat_table.jpeg", width = 8, height = 6, dpi = 600)

