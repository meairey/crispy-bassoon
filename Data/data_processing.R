## !!!Run this to make sure that you have the right directory!!!

setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/AFRP/")

# Libraries --------------------------

library(corrplot)
library(Hmisc)
library(MASS)
require(pscl) # alternatively can use package ZIM for zero-inflated 
library(lmtest)
library(dplyr)
library(tidyr)
library(tidyverse)
library(vegan)
library(RColorBrewer)
library(ggridges)
library(ecp)
library(dplyr)
library(knitr)
library(wesanderson)
library(broom)




## Functions source -----------
source("AFRP_Master_Code/AFRP_Functions.R")
`%nin%` = Negate(`%in%`) # sets up a way to exclude if in a string

# Boat Electrofishing Data
## Contains the metadata for each sampling event
sample = sample %>% mutate(SITE_N = str_replace(SITE_N, "BEF.FBL.003,4", 
                                                "BEF.FBL.003"))
## Contain the catch data for each sampling event
BEF_data_unfiltered = left_join(fish, sample, by = "YSAMP_N") %>%
  filter(WATER %in% c("LML", "FBL"), 
         GEAR == "BEF",
         GEAR_CODE == "NAF") %>%
  #separate(SITE_N, into = c("year", "water", "SITE"), remove = F) %>%
  #select(-year, -water) %>% 
  filter(MONTH %in% c(5,6,7))


# Length break for size
length_break = 75


# LML --------------------------------------------------
## Removing species ---------------

rare_threashold = 50 ## change this based on preference
rare = BEF_data_unfiltered %>% group_by(SPECIES) %>% 
  summarise(frequency = n()) %>% 
  filter(frequency < rare_threashold)
stocked = c("LLS", "RT") ## Stocked fish in little moose
remove = c(stocked, rare$SPECIES) ## Remove SMB + RWF (targeted 2000s)
# I'm also going to remove LT and RS because I want to focus mostly on native littoral fishes 


BEF_data = BEF_data_unfiltered %>% filter(SPECIES %nin% remove)


habs =read.csv("Data/BEFsites_LengthAndHabitat.csv") %>% 
  filter(Water %in% c("FBL", "LML"))%>%
  select(Water, SITE_N, Habitat) %>%
  na.omit() %>%
  filter(Habitat != "N") %>%
  rename("WATER" = Water)


### LML ---------------


LML_unfiltered = BEF_data_unfiltered %>% filter(WATER == "LML")
## Fixing site issues 


###bef_unfiltered_saved = BEF_data_unfiltered
#BEF_data_unfiltered = bef_unfiltered_saved

site_bin = c(1,4,5,9,13,16,18, 21, 23,27,31) ## for the simulations

post_2002 = LML_unfiltered %>% filter(YEAR == 2005) %>% select(SITE_N) %>% unique() %>% 
  separate(SITE_N, into = c("year", "water", "SITE"), remove = F) %>% 
  mutate(SITE_num = parse_number(SITE)) %>% 
  mutate(site_bin = .bincode(SITE_num, site_bin))  %>%
  select(SITE_N, SITE_num, site_bin)



## Create matrices for each cluster of years that were sampled in one way or another


LML_1998_site = LML_unfiltered %>%
  filter(YEAR == 1998) %>% 
  select(SITE_N) %>% unique() %>% 
  mutate(site_bin = c(1,1,1,1,2,3,3,3,4,4,5,7,8,11,10,10,4,5,1,1)) %>% 
  mutate(SITE_num = c(1:20)) 


LML_1998 = LML_unfiltered %>% filter(YEAR == 1998) %>% 
  left_join(LML_1998_site)


lml_1999 = LML_unfiltered %>%
  filter(YEAR == 1999) %>% 
  dplyr::select(SITE_N) %>% unique() %>% 
  filter(SITE_N !="NA") %>% 
  mutate(site_bin = c(2,3,4,1,5,7,8, 10,11,12)) %>% 
  mutate(SITE_num = c(1:10))

LML_1999 = LML_unfiltered %>% filter(YEAR == 1999) %>% 
  left_join(lml_1999)

site_matrix = rbind(lml_1999, LML_1998_site, post_2002) %>%
  as.data.frame() 


LML_data_2002 = BEF_data_unfiltered %>%
  filter(YEAR > 1999) %>%
  left_join(post_2002)# %>%
# select(-SITE)



LML_data_unfiltered = left_join(LML_unfiltered, site_matrix) %>%
  mutate(SITE = SITE_N) %>%## Make sure to use this BEF_data_unfiltered for final graphs
  mutate(SITE_cat = case_when(YEAR %nin% c(1998, 1999) ~ as.numeric(SITE_num),
                              YEAR %in% c(1998, 1999) ~ as.numeric(SITE_num))) %>%
  #dplyr::select(-SITE) %>% 
  #rename(SITE = SITE_cat) %>%
  filter(SITE != "NA") %>% 
  select(SITE_N, SITE, everything())








#LML.CPUE.w.sec = read.csv("Data/CPUE.w.sec.csv")

LML.habs = read.csv("Data/updated_habitat.csv") %>%
  filter(Water == "LML") %>%
  filter(Habitat != "N")

rare_LML = LML_data_unfiltered %>% group_by(SPECIES) %>% 
  summarise(frequency = n()) %>% 
  filter(frequency < rare_threashold)
stocked = c("LLS", "RT") ## Stocked fish in little moose
remove = c(stocked, rare$SPECIES, "ST") ## Remove SMB + RWF (targeted 2000s)
# I'm also going to remove LT and RS because I want to focus mostly on native littoral fishes 


## Intermediate to go into LML.CPUE.w.sec
LML_data = LML_data_unfiltered %>% filter(SPECIES %nin% remove) %>% 
  mutate(LENGTH = case_when(SPECIES %nin% c("MM","SS") ~ .bincode(LENGTH, breaks = c(0,100,8000)), 
                            SPECIES %in% c("MM","SS") ~.bincode(LENGTH, breaks = c(0,50,8000)) )) %>%
  unite("SPECIES", c(SPECIES, LENGTH), remove = F) %>% 
  filter(!is.na(LENGTH)) %>%
  mutate(SITE = as.numeric(SITE)) %>%
  left_join(LML.habs) %>%
  rename(HAB_1 = Habitat) %>% 
  #mutate(WATER == "FBL") %>%
  mutate(EFFORT = as.numeric(EFFORT)) %>%
  select(-SITE) %>%
  rename(SITE = SITE_N) %>%
  filter(SPECIES %nin% remove) 

## Final dataframe for analysis
LML.CPUE.w.sec = CPUE_wide_seconds(LML_data) %>%
  unite("Group", c(YEAR, SITE)) %>% 
  column_to_rownames(., var = "Group") %>% 
  mutate(sumrow = rowSums(.)) %>%
  filter(sumrow>0) %>%
  select(-sumrow)





LML_cpue.habs = LML.CPUE.w.sec %>% 
  mutate(names = rownames(.)) %>% 
  separate(names, into = c("YEAR", "SITE_N"), sep = "_") %>%
  left_join(LML.habs) %>% 
  select(SITE_N, Habitat, everything())%>%
  select(SITE_N, Habitat) %>%
  unique()

species_names.LML = c("brown bullhead", "creek chub", "common shiner", "lake trout", "central mudminnow", "pumpkinseed", "rainbow smelt", "round whitefish", "smallmouth bass", "slimy sculpin","white sucker")


c.h = unique(LML_cpue.habs) %>% na.omit() %>% mutate(SITE = c(c(1:13), c(1:5), c(1:32)))



LML.CPUE.w.sec_avg = CPUE_wide_seconds_avg(LML_data) %>% 
  pivot_longer(BB_1:WS_2, names_to = "SPECIES", values_to = "CPUE") 



totals = LML.CPUE.w.sec %>% mutate(ID = rownames(.)) %>%
  separate(ID, into = c("YEAR", "SITE_N"), sep = "_") %>%
  left_join(c.h) %>% 
  pivot_longer(BB_1:WS_2, names_to = "SPECIES", values_to = "CPUE") %>%
  group_by(YEAR, Habitat, SPECIES) %>%
  summarize(mean_CPUE = mean(CPUE)) %>%
  ungroup() %>%
  separate(SPECIES, into = c("SP", "AGE"), remove = F)  %>%
  group_by(SP, YEAR) %>%
  filter(Habitat != "NA") %>%
  summarise(n = sum(mean_CPUE)) %>%
  mutate(percentage = n / sum(n))

## -------------------------- First bisby -------------------------

# Removing species ---------------


FBL_data_unfiltered = left_join(fish, sample, by = "YSAMP_N") %>%
  filter(WATER == "FBL", 
         GEAR == "BEF",
         GEAR_CODE == "NAF") %>%
  separate(SITE_N, into = c("year", "water", "SITE"), remove = F) %>%
  select(-year, -water, -SITE) %>% 
  filter(MONTH %in% c(5,6,7))


rare_FBL = FBL_data_unfiltered %>% group_by(SPECIES) %>% 
  summarise(frequency = n()) %>% 
  filter(frequency < rare_threashold)
stocked = c("LLS", "RT") ## Stocked fish in little moose
remove = c(stocked, rare$SPECIES, "RS", "CS") ## Remove SMB + RWF (targeted 2000s)
# I'm also going to remove LT and RS because I want to focus mostly on native littoral fishes 

c.h.FBL = habs%>% filter(WATER == "FBL")



FBL_data = FBL_data_unfiltered %>% 
  filter(SPECIES %nin% remove) %>% 
  mutate(LENGTH = case_when(SPECIES %nin% c("MM","SS") ~ .bincode(LENGTH, breaks = c(0,100,8000)),
                            SPECIES %in% c("MM") ~.bincode(LENGTH, breaks = c(0,50,8000)),
                            SPECIES %in% c("SS") ~.bincode(LENGTH, breaks = c(0,25,8000))))%>%
  unite("SPECIES", c(SPECIES, LENGTH), remove = F) %>% 
  filter(!is.na(LENGTH)) %>%
  #mutate(SITE = as.numeric(SITE)) %>%
  left_join(c.h.FBL) %>%
  rename(HAB_1 = Habitat) %>% 
  mutate(WATER == "FBL") %>%
  mutate(EFFORT = as.numeric(EFFORT)) %>%
  filter(YEAR >= 2001) %>%
  filter(YEAR<2023) %>%
  rename(SITE = SITE_N)


FBL_data_unfiltered %>% filter(SPECIES %nin% remove) %>% 
  
  mutate(LENGTH = case_when(SPECIES %nin% c("MM","SS") ~ .bincode(LENGTH, breaks = c(0,100,8000)),
                            SPECIES %in% c("MM") ~.bincode(LENGTH, breaks = c(0,50,8000)),
                            SPECIES %in% c("SS") ~.bincode(LENGTH, breaks = c(0,25,8000)))) %>%
  unite("SPECIES", c(SPECIES, LENGTH), remove = F) %>% 
  filter(!is.na(LENGTH)) %>%
  mutate(SITE = as.numeric(SITE)) %>%
  filter(YEAR == 2004)




FBL.CPUE.w.sec = ((CPUE_wide_seconds(FBL_data) %>%
                     unite("Group", c(YEAR, SITE)) %>% 
                     column_to_rownames(., var = "Group") %>% 
                     mutate(sumrow = rowSums(.)) %>%
                     filter(sumrow>0) %>%
                     select(-sumrow))) 

FBL_cpue.habs = FBL.CPUE.w.sec %>% 
  mutate(names = rownames(.)) %>% 
  separate(names, into = c("year", "Site")) %>%
  mutate(Site = as.numeric(Site)) %>% 
  left_join(site_hab) %>%
  select(Site, HAB_1) 

site_hab = FBL_data %>% select(SITE, HAB_1) %>%
  rename(Site = SITE) %>%
  unique() %>% arrange(Site) %>% na.omit()


species.FBL = colnames(FBL.CPUE.w.sec) 

species_names.FBL = c("Creek Chub","Lake Trout","Central Mudminnow", "Smallmouth Bass",  "Brook Trout", "White Sucker")

length_graph = rep(c("< 100 mm", "> 100 mm"), length(species_names.FBL))

## Change through time -----------------------------------------------------
BEF_data = BEF_data_unfiltered %>% 
  mutate(EFFORT = as.numeric(EFFORT)) %>% 
  mutate(SITE = SITE_N)

# Data setup
CPUE.w.sec = CPUE_wide_seconds(BEF_data) %>%
                 unite("Group", c(YEAR, SITE)) %>% 
                 column_to_rownames(., var = "Group") %>% 
                 mutate(sumrow = rowSums(.)) %>%
                 filter(sumrow>0) %>%
                 select(-sumrow) %>%
                 rownames_to_column(var = "rownames")

CPUE.w.sec

## Write the files for analysis
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/crispy-bassoon/Data/")
write.csv(LML.CPUE.w.sec, "LML_CPUE.csv")
write.csv( FBL.CPUE.w.sec,"FBL_CPUE.csv")
write.csv(CPUE.w.sec, "CPUE_whole.csv")
write.csv(habs, "habs.csv")





