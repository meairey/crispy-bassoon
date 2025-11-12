# Libraries --
library(tidyverse)
library(gridExtra)
library(vegan)
library(ggrepel)

## LML ------
LML.CPUE.w.sec = read.csv("Data/LML_CPUE.csv") %>% 
  column_to_rownames(var = "X")


env_updated.lml = read.csv("Data/CCA_data/LML_habitat.csv")  %>%
  select(X, SITE_N, B, C, EV, FW, S, SV, O, BED, CW, everything())  %>%
  mutate(across(c(-SITE_N, -X), ~ ifelse(. < 3, 0, 1))) ## Makes this just a presence absence of any habitat feature that is more than 60% of the shoreline




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
  separate(SITE_N, into = c("GEAR", "WATER", "SITE_N")) %>%
  mutate(SITE_N = as.numeric(SITE_N))


data_com.lml = data_com.lml %>% ungroup() %>% select(CC_1, CC_2, CS_1, CS_2, MM_1, MM_2,
                                             WS_1, WS_2,
                                             PS_1, PS_2)


cca_model.lml = cca(data_com.lml ~ 
                  SMB_2 + 
                  SMB_1 +
                  Year  +
                  B +
                  BED +
                  C +
                  CW +
                  EV +
                  FW + 
                  O +
                  #S +
                  #SITE_N +
                  SV,
                data = data_env.lml)

print(cca_model.lml)




# Extract species scores
species_scores.lml <- scores(cca_model.lml, display = "species") %>% 
  as.data.frame() %>%
  rownames_to_column(var = "id") %>% 
  left_join(read.csv("Data/legend.csv")) %>%
  select(common, CCA1, CCA2,age_code) %>%
  mutate(common = tolower(common)) %>%
  rename(ID = common) %>%
  mutate(ID = str_replace(ID, " ","~")) 
# Extract site scores
site_scores.lml <- scores(cca_model.lml, display = "sites")

# cca_mo
vectors.lml = summary(cca_model.lml)[4]$biplot %>% as.data.frame() %>% 
  mutate(ID = rownames(.)) %>%
  mutate(ID = c("bold((J)~SMB)", "bold((A)~SMB)", "bold(Year)","bold(Boulders)", "bold(Cobbles)", "bold(CWD)", "bold(Emergent~veg)", "bold(FWD)", "bold(Organic~debris)","bold(Submerged~veg)")) %>%
  select(ID, CCA1, CCA2) %>%
  mutate(CCA1 = CCA1 * 1.5, 
         CCA2 = CCA2 * 1.5)


cca_graph.LML = vectors.lml %>% 
  mutate(age_code = NA) %>%
  rbind(species_scores.lml) %>%
  mutate(WATER = "LML")

## Make results table


# Extract eigenvalues
eigenvalues.lml <- eigenvals(cca_model.lml) %>% 
  as.data.frame() %>%
  rename("value" = "x")%>%
  rownames_to_column(var = "rowname") 

## Get unconstrained values
CCA.lml = eigenvalues.lml %>% 
  filter(grepl("CCA", rowname))
## Get constrained values
CA.lml = eigenvalues.lml %>% 
  filter(!grepl("CCA", rowname))

## Summary stats for table
total = sum(CCA.lml$value) + sum(CA.lml$value)
CA =  sum(CA.lml$value) / total * 100 # The percent unconstrained variance in the CCA
CCA = sum(CCA.lml$value) / total * 100
CCA1 = CCA.lml$value[1] / total * 100
CCA2 = CCA.lml$value[2] / total * 100
CCA3 = CCA.lml$value[3] / total * 100
CCA4 = CCA.lml$value[4] / total * 100
eig.sum = data.frame(CCA1 = CCA1, CCA2 = CCA2, CCA3 = CCA3, CCA4 = CCA4)

## Create table
sum.table.lml = rbind(eig.sum, scores(cca_model.lml,  choices = 1:4)$biplot,
      scores(cca_model.lml, choices = 1:4)$species)
## Write table - there is also a cleaned excel workbook with table formating in the Tables_Figures folder in crispy_bassoon
write.csv(sum.table.lml,"Data/CCA.lml.csv")



## Scree/Elbow plot


ggplot(mapping = aes( x = c(1:9), y = eigenvalues.lml[1:9,"value"])) + geom_line() +
  xlab("CCA Axis") + ylab("Eigen Value") +
  theme_minimal(base_size = 14)


kmeans_result <- kmeans(data, centers = k, nstart = 25)


silhouette_scores <- silhouette(kmeans_result$cluster, dist(data))

plot(silhouette_scores, col = 1:k)

sil_widths <- numeric()

for (k in 2:10) {
  kmeans_result <- kmeans(data, centers = k, nstart = 25)
  sil <- silhouette(kmeans_result$cluster, dist(data))
  sil_widths[k] <- mean(sil[, 3]) # Average silhouette width
}

plot(2:10, sil_widths[2:10], type = "b", xlab = "Number of clusters", ylab = "Average silhouette width")

# FBL -----------------------------


FBL.CPUE.w.sec = read.csv("Data/FBL_CPUE.csv") %>% 
  column_to_rownames(var = "X")

env_updated.fbl = read.csv("Data/CCA_data/FBL_habitat.csv") %>%
  select(X, SITE_N, B, C, EV, FW,  S, SV, O, BED, CW, everything()) %>%
  mutate(across(c(-SITE_N, -X), ~ ifelse(. < 3, 0, 1)))


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

m = cor(env_updated.fbl %>% select(-SITE_N, -X))

library(corrplot)
m = cor(env_updated.lml %>% select(-SITE_N, -X, -B, -G, -R, -BED,-S))
testRes = cor.mtest(env_updated.fbl %>% select(-SITE_N, -X, -B, -G, -R, -BED,-S), conf.level = 0.95)
corrplot(m)

## Load in data from CPUE_hab.Rmd file
data.fbl = v.fbl %>% ## Used for changepoints graph
  filter(Year > 2004) %>% ## Filter for after the start of the removal
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
species_scores.fbl = scores(cca_result.fbl, display = "species")  %>% 
  as.data.frame() %>%
  rownames_to_column(var = "id") %>% 
  left_join(read.csv("Data/legend.csv")) %>%
  select(common, CCA1, CCA2,age_code) %>%
  mutate(common = tolower(common)) %>%
  rename(ID = common) %>%
  mutate(ID = str_replace(ID, " ","~")) # %>%
 # unite("ID",c(age1, scientific), sep = " " )

# Extract site scores
site_scores.fbl <- scores(cca_result.fbl, display = "sites")

# cca_mo

vectors.fbl = summary(cca_model.fbl)[4]$biplot %>% as.data.frame() %>% 
  mutate(ID = rownames(.)) %>%
  mutate(ID = c( "bold(Year)",  "bold((A)~SMB)", "bold((J)~SMB)",
                "bold(Cobbles)", "bold(CWD)", "bold(Emergent~veg)", 
                "bold(FWD)", "bold(Organic~debris)","bold(Submerged~veg)")) %>%
  select(ID, CCA1, CCA2) %>%
  mutate(CCA1 = CCA1 * 1.5, 
         CCA2 = CCA2 * 1.5)




species_scores.fbl = species_scores.fbl #%>% 
  mutate(ID = paste0("italic(", gsub(" ", "~", scientific), ")")) %>%
  select( ID,CCA1, CCA2, age_code)

cca_graph.fbl = vectors.fbl %>% 
  mutate(age_code = NA) %>%
  rbind(species_scores.fbl) %>%
  mutate(WATER = "FBL") 



# Extract eigenvalues
eigenvalues.fbl <- eigenvals(cca_model.fbl) %>% 
  as.data.frame() %>%
  rename("value" = "x")%>%
  rownames_to_column(var = "rowname") 

## Get unconstrained values
CCA.fbl = eigenvalues.fbl %>% 
  filter(grepl("CCA", rowname))
## Get constrained values
CA.fbl = eigenvalues.fbl %>% 
  filter(!grepl("CCA", rowname))

## Summary stats for table
total = sum(CCA.fbl$value) + sum(CA.fbl$value)
CA =  sum(CA.fbl$value) / total * 100 # The percent unconstrained variance in the CCA
CCA = sum(CCA.fbl$value) / total * 100
CCA1 = CCA.fbl$value[1] / total * 100
CCA2 = CCA.fbl$value[2] / total * 100
CCA3 = CCA.fbl$value[3] / total * 100
CCA4 = CCA.fbl$value[4] / total * 100
eig.sum = data.frame(CCA1 = CCA1, CCA2 = CCA2, CCA3 = CCA3, CCA4 = CCA4)

## Create table
sum.table.fbl = rbind(eig.sum, scores(cca_model.fbl,  choices = 1:4)$biplot,
                      scores(cca_model.fbl, choices = 1:4)$species)
## Write table - there is also a cleaned excel workbook with table formating in the Tables_Figures folder in crispy_bassoon
#write.csv(sum.table.fbl,"Data/CCA.fbl.csv")


## Plotting the two together ----------------

cca_graph = rbind(cca_graph.fbl, cca_graph.LML) ## Bind together the two individual data frames

## Plot the arranged grid - using facet wrap
ggplot() +
    # Vectors (environmental variables)
  geom_segment(
    data = cca_graph %>% 
      filter(is.na(age_code)),
    aes(x = 0, y = 0, xend = CCA1, yend = CCA2),
    arrow = arrow(length = unit(0.1, "inches")),
    color = "black",
    linewidth = 0.7,
    alpha = 0.7
  ) +
  # Background and axes
  theme_minimal(base_size = 15) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray70") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +

  # Species scores (colored by life stage)
  geom_label_repel(
    data = cca_graph,
    aes(x = CCA1, y = CCA2, label = ID, color = as.factor(age_code)),  # assumes a column "LifeStage" exists
    size = 3.5,
    max.overlaps = 15,
    segment.alpha = 0.3,
    parse = T,
    fill = "white"
  ) +
  # Color legend for species life stages
  scale_color_manual(
    values = c("1" = "#1f78b4", "2" = "#873e23"),
    labels = c("1" ="Juvenile", "2" = "Adult")) + # adjust to your dataset
  # Axes and limits
  xlim(-1.2, 1.5) +
  labs(
    x = "CCA1",
    y = "CCA2",
    color = "Life Stage"
  ) +
  theme(
    legend.position = "top",
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 12),
    panel.grid.major = element_line(color = "gray90")
  ) + 
  facet_wrap(~WATER, labeller = labeller(WATER = c( "FBL" ="First Bisby", "LML" = "Little Moose")))

