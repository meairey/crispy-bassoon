LML.CPUE.w.sec = read.csv("Data/LML_CPUE.csv") %>% 
  column_to_rownames(var = "X")


env_updated.lml = read.csv("Data/LML_habitat.csv")

v = LML.CPUE.w.sec  %>%
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


data = v 


data_com = data %>% 
  #select(veg_emerg:wood_fine, CC_1, CC_2) %>%
  filter(CC_1 + CC_2 +
           CS_1 + CS_2 +
           PS_1 + PS_2 +
           WS_1 + WS_2 +
           MM_1 + MM_2 > 0) 

#select(BB_1:WS_2) %>%





data_env = data_com %>%
  ungroup() %>%
  select(SMB_1, SMB_2, SITE_N, 
         Year, 
         FW, O, SC,
         SV,B,S,EV, CW, BED, C) %>%
  mutate(Year = as.numeric(Year)) %>%
  ungroup()


data_com = data_com %>% ungroup() %>% select(CC_1, CC_2, CS_1, CS_2, MM_1, MM_2,
                                             WS_1, WS_2,
                                             PS_1, PS_2)




## Maybe slope gradient of the shoreline? Proximity to deep water
## Depth, proximity to tribs? Or known groundwater seeps?
## Proximity to camps?
cca_model = cca(data_com ~ 
                  data_env$SMB_2 + 
                  data_env$SMB_1 +
                  data_env$Year +
                  data_env$CW +
                  data_env$BED +

                  data_env$SC +
                  data_env$S +
                  data_env$O +
                  data_env$SV + 
                  data_env$B +
                  data_env$FW,
                data = data_env)

summary(cca_model)
print(cca_model)


# Extract species scores
species_scores <- scores(cca_model, display = "species")
# Extract site scores
site_scores <- scores(cca_model, display = "sites")
# cca_mo
vectors = summary(cca_model)[4]$biplot %>% as.data.frame() %>% 
  mutate(ID = rownames(.)) %>%
  separate(ID,  into = c("d", "env", "ID"))
vectors$ID[1] = "SMB_2"
vectors$ID[2] = "SMB_1"

# Plot the biplot
ggplot() +
  theme_minimal() + 
  geom_hline(, yintercept = 0, linetype = "dashed", color = "gray") +
  geom_vline(, xintercept = 0, linetype = "dashed", color = "gray") +
  geom_text(data = species_scores, aes(label = rownames(species_scores), 
                                       x = CCA1, y = CCA2), size = 5) +
  geom_text(data = vectors, aes(label = ID, x = CCA1, y = CCA2), col = "brown") + 
  geom_segment(data = vectors, aes(x = 0, y = 0, xend = CCA1, yend = CCA2),
               col = "brown", alpha = 0.5, arrow = arrow(length = unit(0.1, "inches"))) + 
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") + 
  xlim(-1.2, 1.5)



# FBL -----------------------------


FBL.CPUE.w.sec = read.csv("Data/FBL_CPUE.csv") %>% 
  column_to_rownames(var = "X")
env_updated.fbl = read.csv("Data/FBL_habitat.csv")
v = FBL.CPUE.w.sec %>% 
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
data = v %>% ## Used for changepoints graph
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
data_com = data %>% 
  filter(CC_1 + CC_2 +
           WS_1 + WS_2 +
           MM_1 + MM_2 > 0) 

# Create environmental data frame that matches the data_com but contains only the habitat features
data_env = data_com %>%
  select(Year, SMB_1,
         SMB_2, SITE_N,
         C, O, FW, B, 
         EV, SV, BED, 
         CW) %>%
  ungroup()

## Now remove all other specieis that we're not interested in to get the community data frame
data_com = data_com %>% ungroup() %>% select(CC_1, CC_2, 
                                             MM_1, MM_2,
                                             WS_1, WS_2)

## Maybe slope gradient of the shoreline? Proximity to deep water
## Depth, proximity to tribs? Or known groundwater seeps?
## Proximity to camps?
cca_model = cca(data_com ~ 
                  #data_env$assigned_sand +
                  data_env$Year + 
                  data_env$SMB_2 + 
                  data_env$SMB_1 +
                  data_env$C +
                  data_env$O + 
                  data_env$SV +
                  data_env$EV +
                  data_env$BED +
                  data_env$FW +
                  data_env$CW,
                data = data_env)


summary(cca_model)
print(cca_model)

cca_result <- cca_model

# Extract species scores
species_scores <- scores(cca_result, display = "species")

# Extract site scores
site_scores <- scores(cca_result, display = "sites")

# cca_mo

vectors = summary(cca_model)[4]$biplot %>% as.data.frame() %>% 
  mutate(ID = rownames(.)) %>%
  separate(ID,  into = c("d", "env", "ID"))

vectors$ID[2] = "SMB_2"; vectors$ID[3] = "SMB_1"


# Plot the biplot
ggplot() +
  theme_minimal() + 
  geom_hline(, yintercept = 0, linetype = "dashed", color = "gray") +
  geom_vline(, xintercept = 0, linetype = "dashed", color = "gray") +
  geom_text(data = species_scores,
            aes(label = rownames(species_scores),
                x = CCA1, y = CCA2),
            size = 5) +
  geom_text(data = vectors, 
            aes(label = ID, 
                x = CCA1,
                y = CCA2),
            col = "brown") + 
  geom_segment(data = vectors,
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


