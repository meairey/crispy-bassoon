# Replace "your_shapefile.shp" with the path to your shapefile
library(ggplot2)
library(sf)
library(geosphere)
library(viridis)
library(tidyverse)

library(lwgeom)



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
gps = read.csv("Data/garmin_lml_hab_day2.csv")

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
substrate = read.csv("Data/habitat_class1.csv") %>% 
  filter(is.na(start) == F) %>%
  filter(MEA == "MEA") %>%
  filter(density > 2) %>%
  mutate(feature = str_replace(feature, "A", "SV")) %>%
  mutate(feature = str_replace(feature, "SBR","B")) %>%
  mutate(feature = str_replace(feature, "SCS", "SC")) %>%
  mutate(feature = str_replace(feature, "SB", "B")) %>%
  mutate(feature = str_replace(feature, "SCD", "SC"))

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
## Fixing site lengths with site boundaries for that one weird continuous stretch

# Plot with geom_path
ggplot(df_ordered, aes(x = lat1, y = lon1)) +
  geom_path() +
  labs(title = "Ordered Path")

site_lengths = site_lengths %>% na.omit()


site_lengths
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


wood_counts = read.csv("Data/habitat_class1.csv") %>% 
  group_by(water, site, feature) %>%
  #filter(is.na(start) == T) %>%
  filter(MEA == "MEA") %>%
  summarize(sum_feature = n()) %>%
  ungroup() %>%
  group_by(water) %>%
  complete(site, feature)%>%    
  mutate(across(everything(), ~ replace_na(.x, 0))) %>%
  filter(feature == "CW") %>%
  ungroup() %>%
  group_by(water) %>%
  mutate(total_logs = sum(sum_feature),
         percent = sum_feature / total_logs * 100, 
         score = percent / 100 * 5) %>%
  rename("SITE_N" = "site") %>%
  select(-total_logs, -sum_feature, -feature)


## Creating little table for the CCA
env_updated_FBL = whole %>% left_join(site_lengths, by = "site") %>%
  group_by(site, feature, shoreline) %>%
  summarize(total_hab = sum(dist)) %>%
  mutate(percent_shoreline = total_hab / shoreline * 100)  %>%
  #separate(site, into = c("gear", "water", "site_n"), remove = F) %>%
  select(site,  feature, percent_shoreline) %>%
  mutate(f_score = .bincode(percent_shoreline, breaks = c(0,25,50,75,110))) %>%
  select(-percent_shoreline) %>%
  pivot_wider(values_from = f_score, names_from = feature) %>%    
  mutate(across(everything(), ~ replace_na(.x, 0))) %>%
  rename("SITE_N" = "site") %>%
  left_join(wood_counts) %>%
  filter(water == "FBL") %>%
  mutate(CW = round(CW + score, digits = 0)) %>%
  select(-water, -percent, -score)

write.csv(env_updated_FBL, "Data/FBL_habitat.csv") ## Write file

## ------------------ LML ------------------- 

# Set working directory
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/crispy-bassoon/")
# Pull together df with substrate information and waypoints
substrate = read.csv("Data/habitat_class1.csv") %>% 
  filter(water == "LML") %>%
  filter(is.na(start) == F)
# get the start and end points for each site
site_lengths = substrate %>% group_by(water, site) %>%
  mutate(end = as.numeric(end)) %>% 
  summarize(ID1 = min(start),
            ID2= max(end, na.rm = T)) 
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


site_lengths = site_lengths %>% na.omit()


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
  group_by(site, feature, shoreline) %>%
  summarize(total_hab = sum(dist)) %>%
  mutate(percent_shoreline = total_hab / shoreline * 100)  %>%
  separate(site, into = c("gear","site_num"), remove = F)

whole_lml  %>%
  mutate(site_num = parse_number(site)*1000)  %>%
  ggplot(aes(x = site_num, y = percent_shoreline, fill =  feature)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~feature) 

## Plotting Little Moose
ggplot() +
  geom_path(data = df_ordered, aes(x = lat1, y = lon1)) +
  geom_path(data = df_ordered %>% filter(ID1 <= 404 & ID1 >=382),  aes(x = lat1, y = lon1), col = "red") +
  scale_x_reverse()


 


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
  mutate(CW = round(CW + score, digits = 0)) %>%
  select(-water, -percent, -score)



write.csv(env_updated_LML, "Data/LML_habitat.csv")

