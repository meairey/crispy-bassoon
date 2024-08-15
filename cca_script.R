# Libraries --
library(tidyverse)
library(gridExtra)

## LML ------
LML.CPUE.w.sec = read.csv("Data/LML_CPUE.csv") %>% 
  column_to_rownames(var = "X")


env_updated.lml = read.csv("Data/LML_habitat.csv")  %>%
  select(X, SITE_N, B, C, EV, FW, G, S, SV, O, BED, CW, everything())#  %>%
  #mutate(across(c(-SITE_N, -X), ~ ifelse(. < 2, 0, 1))) ## Makes this just a presence absence of any habitat feature that is more than 20% of the shoreline



v.lml = LML.CPUE.w.sec  %>%
  mutate(y_s = rownames(LML.CPUE.w.sec)) %>%
  pivot_longer(1:WS_2,
               names_to = "Species") %>%
  separate(y_s, 
           into = c("Year", "SITE_N"), sep = "_") %>% 
  #mutate(value = value * 60 ) %>%
  filter(SITE_N %in% c(env_updated.lml$SITE_N)) %>%
  group_by(Year, SITE_N,  Species) %>%
  summarize(value = median(value))  %>%
  left_join(env_updated.lml) %>%
  mutate(value = value * 60 * 60 ) %>%
  filter(Year != 2002 & Year > 2000)  %>% ## take medians just to try
  
  pivot_wider(names_from = Species, values_from = value)



### Trying to include some of the actual data


data.lml = v.lml 


data_com.lml = data.lml %>% 
  #select(veg_emerg:wood_fine, CC_1, CC_2) %>%
  filter(CC_1 + CC_2 +
           CS_1 + CS_2 +
           PS_1 + PS_2 +
           WS_1 + WS_2 +
           MM_1 + MM_2 > 0) 

#select(BB_1:WS_2) %>%





data_env.lml = data_com.lml %>%
  ungroup() %>%
  select(SMB_1, SMB_2, SITE_N, 
         Year, 
         FW, O,
         SV,B,S,EV, CW, BED, C) %>%
  mutate(Year = as.numeric(Year)) %>%
  ungroup()


data_com.lml = data_com.lml %>% ungroup() %>% select(CC_1, CC_2, CS_1, CS_2, MM_1, MM_2,
                                             WS_1, WS_2,
                                             PS_1, PS_2)




## Maybe slope gradient of the shoreline? Proximity to deep water
## Depth, proximity to tribs? Or known groundwater seeps?
## Proximity to camps?
cca_model.lml = cca(data_com.lml ~ 
                  SMB_2 + 
                  SMB_1 +
                  Year +
                  B +
                  BED +
                  C +
                  CW +
                  EV +
                  FW + 
                  O +
                  #S +
                  SV,
                data = data_env.lml)
print(cca_model.lml)
#summary(cca_model.lml)



# Extract species scores
species_scores.lml <- scores(cca_model.lml, display = "species")
# Extract site scores
site_scores.lml <- scores(cca_model.lml, display = "sites")

# cca_mo
vectors.lml = summary(cca_model.lml)[4]$biplot %>% as.data.frame() %>% 
  mutate(ID = rownames(.)) 

# Plot the biplot
LML.biplot = ggplot() +
  theme_minimal() + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  geom_text(data = species_scores.lml, aes(label = rownames(species_scores.lml), 
                                       x = CCA1, y = CCA2), size = 5) +
  geom_text(data = vectors.lml, aes(label = ID, x = CCA1, y = CCA2), col = "brown") + 
  geom_segment(data = vectors.lml, aes(x = 0, y = 0, xend = CCA1, yend = CCA2),
               col = "brown", alpha = 0.5, arrow = arrow(length = unit(0.1, "inches"))) + 
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") + 
  xlim(-1.2, 1.5)



# FBL -----------------------------


FBL.CPUE.w.sec = read.csv("Data/FBL_CPUE.csv") %>% 
  column_to_rownames(var = "X")
env_updated.fbl = read.csv("Data/FBL_habitat.csv") %>%
  select(X, SITE_N, B, C, EV, FW, G, S, SV, O, BED, CW, everything()) #%>%
  #mutate(across(c(-SITE_N, -X), ~ ifelse(. < 2, 0, 1)))
v.fbl = FBL.CPUE.w.sec %>% 
  mutate(y_s = rownames(FBL.CPUE.w.sec)) %>%
  pivot_longer(1:WS_2,
               names_to = "Species") %>%
  separate(y_s, 
           into = c("Year", "SITE_N"), sep = "_") %>%
  left_join(env_updated.fbl) %>%
  unite("ID", 
        c(SITE_N,Species), 
        sep = "_", 
        remove = F) %>%


  mutate(value = value * 60 * 60 ) %>%
  filter(Year != 2002 & Year > 2004)





## Load in data from CPUE_hab.Rmd file
data.fbl = v.fbl %>% ## Used for changepoints graph
  filter(Year > 2004) %>% ## Filter for after the start of ther emoval
  mutate(value = value ) %>% ## not sure?
  left_join(env_updated.fbl) %>% ## Pull in this table from maps.R where we calculate percent shoreline of all different habitat features
  select(-ID) %>% 
  pivot_wider(names_from = Species, values_from = value)%>% ## Pivot out to match format for CCA
  mutate(across(c(CC_1:WS_2), ~ coalesce(., 0)))%>%
  mutate(Year = as.numeric(Year)) %>%
  na.omit() %>%
  ungroup()

# Create community dataframe where all target taxa (here the CC, WS, and MM) all have populations above 0
data_com.fbl = data.fbl %>% 
  filter(CC_1 + CC_2 +
           WS_1 + WS_2 +
           MM_1 + MM_2 > 0) 

# Create environmental data frame that matches the data_com but contains only the habitat features
data_env.fbl = data_com.fbl %>%
  select(Year, SMB_1,
         SMB_2, SITE_N,
         C, O, FW, B, 
         EV, SV, BED, 
         CW) %>%
  ungroup()

## Now remove all other specieis that we're not interested in to get the community data frame
data_com.fbl = data_com.fbl %>% ungroup() %>% select(CC_1, CC_2, 
                                             MM_1, MM_2,
                                             WS_1, WS_2)

## Maybe slope gradient of the shoreline? Proximity to deep water
## Depth, proximity to tribs? Or known groundwater seeps?
## Proximity to camps?
cca_model.fbl = cca(data_com.fbl ~ 
                  Year +
                  SMB_2 + 
                  SMB_1 +
                  B +
                  BED +
                  C +
                  CW +
                  EV +
                  FW + 
                  O +
                  #S +
                  SV,
                data = data_env.fbl)


#summary(cca_model.fbl)
print(cca_model.fbl)

cca_result.fbl <- cca_model.fbl

# Extract species scores
species_scores.fbl <- scores(cca_result.fbl, display = "species")

# Extract site scores
site_scores.fbl <- scores(cca_result.fbl, display = "sites")

# cca_mo

vectors.fbl = summary(cca_model.fbl)[4]$biplot %>% as.data.frame() %>% 
  mutate(ID = rownames(.)) 


# Plot the biplot
FBL.biplot = ggplot() +
  theme_minimal() + 
  geom_hline( yintercept = 0, linetype = "dashed", color = "gray") +
  geom_vline( xintercept = 0, linetype = "dashed", color = "gray") +
  geom_text(data = species_scores.fbl,
            aes(label = rownames(species_scores.fbl),
                x = CCA1, y = CCA2),
            size = 5) +
  geom_text(data = vectors.fbl, 
            aes(label = ID, 
                x = CCA1,
                y = CCA2),
            col = "brown") + 
  geom_segment(data = vectors.fbl,
               aes(x = 0,
                   y = 0,
                   xend = CCA1,
                   yend = CCA2), 
               col = "brown", 
               alpha = 0.5,
               arrow = arrow(length = unit(0.1, "inches"))) + 
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") + 
  xlim(-1.2, 1.5)


## Plotting the two together ----------------

grid.arrange(FBL.biplot, LML.biplot, ncol = 2)
