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
source("../AFRP/AFRP_Master_Code/AFRP_Functions.R")
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
  filter(MONTH %in% c(5,6,7)) ## Filter to just sample the spring period of sampling


rare_threashold = 50 ## To filter out rare taxa

## Habitat info 
habs = read.csv("../AFRP/Data/BEFsites_LengthAndHabitat.csv") %>% 
filter(Water %in% c("FBL", "LML"))%>%
  select(Water, SITE_N, Habitat) %>%
  na.omit() %>%
  filter(Habitat != "N") %>%
  rename("WATER" = Water)


## Removing species ---------------

## Rare taxa are removed below the rare_threshold defined above
rare = BEF_data_unfiltered %>% group_by(WATER, SPECIES) %>% 
  summarise(frequency = n()) %>% 
  filter(frequency < rare_threashold)
stocked = c("LLS", "RT") ## Stocked fish in little moose
remove = c(stocked,  "LT", "RS","RWF") ## Remove stocked taxa, and pelagic taxa that we're not focusing on

# Define Boat electrofishing data with target taxa only
BEF_data = BEF_data_unfiltered %>%
  left_join(rare) %>% ## Join rare data frame
  filter(frequency > rare_threashold, # remove rare
         SPECIES %nin% remove) %>% # remove stocked and non-target taxa
  select(-frequency)
## 


LML_unfiltered = BEF_data_unfiltered %>% filter(WATER == "LML")


## Create matrices for each cluster of years that were sampled uniquely

## Standardized siittets post 2000
post_2000 = LML_unfiltered %>% # Take LML data
  filter(YEAR == 2005) %>% ## Just filtered for any year during the standardized sampling
  select(SITE_N) %>% # select just site numbers
  unique() %>% ## unique sites
  separate(SITE_N, into = c("year", "water", "SITE"), remove = F) %>% ## separate out to get the SITE component of the SITE_N
  mutate(SITE_num = parse_number(SITE)) %>% ## Assign number in chronological order
  select(SITE_N, SITE_num) ## select just the SITE_N and the new assigned name
## 1998 sites -----------
LML_1998_site = LML_unfiltered %>% ## Take LML data
  filter(YEAR == 1998) %>% ## filter for 1998
  select(SITE_N) %>% unique() %>% ## find unique sites
  mutate(SITE_num = c(1:20)) ## assign site a new number in chronological order

LML_1998 = LML_unfiltered %>% filter(YEAR == 1998) %>% ## create a new data frame that joins these new site names with 1998 data
  left_join(LML_1998_site)

## 1999 sites ------------
lml_1999 = LML_unfiltered %>% ## Take LML Data
  filter(YEAR == 1999) %>% ## Filter for 1999
  dplyr::select(SITE_N) %>% unique() %>% ## Select for unique sites
  filter(SITE_N !="NA") %>% ## remove any sites that are not identified
  mutate(SITE_num = c(1:10)) ## assign site a new number in chronological order

LML_1999 = LML_unfiltered %>% filter(YEAR == 1999) %>% ## create a new data frame that joins these new site names with 1999 data
  left_join(lml_1999)

## Combine all the data frames together from 1998, 1999, and 2000+

site_matrix = rbind(lml_1999, LML_1998_site, post_2000) %>%
  as.data.frame() 

LML_data_unfiltered = left_join(LML_unfiltered, site_matrix) %>% ## Join the LML data with the matrix of new site numbers
  mutate(SITE = SITE_N) %>% ## Make sure to use this BEF_data_unfiltered for final graphs
  mutate(SITE_cat = case_when(YEAR %nin% c(1998, 1999) ~ as.numeric(SITE_num),
                              YEAR %in% c(1998, 1999) ~ as.numeric(SITE_num))) %>%
  filter(SITE != "NA") %>% 
  select(SITE_N, SITE, everything())



LML.habs = read.csv("Data/updated_habitat.csv") %>%
  filter(Water == "LML") %>%
  filter(Habitat != "N") %>%
  rename(WATER = Water) %>%
  rename(SITE_num = SITE) %>%
  select(WATER, SITE_N, Habitat) %>%
  unique()

## Intermediate to go into LML.CPUE.w.sec
LML_data = LML_data_unfiltered %>% filter(SPECIES %nin% remove) %>% 
  mutate(LENGTH = case_when(SPECIES %nin% c("MM","SS") ~ .bincode(LENGTH, breaks = c(0,100,8000)), ## Length break is 100 mm
                            SPECIES %in% c("MM","SS") ~.bincode(LENGTH, breaks = c(0,50,8000)))) %>%
  unite("SPECIES", c(SPECIES, LENGTH), remove = F) %>% 
  filter(!is.na(LENGTH)) %>%
  mutate(SITE = as.numeric(SITE)) %>%
  left_join(LML.habs, by =) %>%
  rename(HAB_1 = Habitat) %>% 
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

## Intermediate for c.h. that includes the site names and the habitats associated with them
LML_cpue.habs = LML.CPUE.w.sec %>% 
  unique() %>%
  mutate(names = rownames(.)) %>% 
  separate(names, into = c("YEAR", "SITE_N"), sep = "_") %>%
  left_join(LML.habs) %>% 
  select(SITE_N, Habitat, everything()) %>%
  select(SITE_N, Habitat) %>%
  unique()

## Species names for the species in LML
species_names.LML = c("brown bullhead", "creek chub", "common shiner", "lake trout", 
                      "central mudminnow", "pumpkinseed", "rainbow smelt", 
                      "round whitefish", "smallmouth bass", "slimy sculpin","white sucker")


## Matrix that includes the site_N and the new assigned site numbers
c.h = unique(LML_cpue.habs) %>% na.omit() %>% mutate(SITE = c(c(1:13), c(1:5), c(1:32)))
c.h = site_matrix %>% left_join(LML_cpue.habs) %>% na.omit() 
dim(c.h)


LMLtotals = LML.CPUE.w.sec %>% 
  mutate(ID = rownames(.)) %>%
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


## Create data frame that will go into the changepoint analysis
### Every site in each year is ordered through the total unique sites sampled that year that were assigned a habitat value


LML.v = LML.CPUE.w.sec %>% 
  mutate(y_s = rownames(LML.CPUE.w.sec)) %>%
  pivot_longer(1:length(colnames(LML.CPUE.w.sec)),
               names_to = "Species") %>%
  separate(y_s, 
           into = c("Year", "SITE_N"), sep = "_") %>%
  left_join(c.h) %>%
  unite("ID", 
        c(SITE,Species), 
        sep = "_", 
        remove = F) %>%
  select(-SITE_N) %>%
  rename(HAB_1 = Habitat) %>%
  mutate(value = value * 60 * 60 ) %>%
  filter(Year != 2002)

## Because of site issues, remove the woody habitat descriptor from habitat

LML.v = v %>% 
  mutate(HAB_1 = str_replace(HAB_1, "SW", "S")) %>%
  mutate(HAB_1 = str_replace(HAB_1, "RW", "R")) %>%
  filter(HAB_1 != "NA")

## Write these into data files that can be loaded later
save(file = "Data/LMLTotals.Rdata", LMLtotals)
save(file = "Data/ChangePoint_Data/LMLV.Rdata", LML.v)
## -------------------------- First bisby -------------------------

# Removing species ---------------


FBL_data_unfiltered = left_join(fish, sample, by = "YSAMP_N") %>%
  filter(WATER == "FBL", 
         GEAR == "BEF",
         GEAR_CODE == "NAF") %>%
  separate(SITE_N, into = c("year", "water", "SITE"), remove = F) %>%
  select(-year, -water, -SITE) %>% 
  filter(MONTH %in% c(5,6,7))

FBL_data_unfiltered = BEF_data_unfiltered %>% filter(WATER == "FBL")


# I'm also going to remove LT and RS because I want to focus mostly on native littoral fishes 

c.h.FBL = habs%>% filter(WATER == "FBL")



FBL_data = FBL_data_unfiltered %>% 
  filter(SPECIES %nin% remove) %>% 
  mutate(LENGTH = case_when(SPECIES %nin% c("MM","SS") ~ .bincode(LENGTH, breaks = c(0,100,8000)),
                            SPECIES %in% c("MM") ~.bincode(LENGTH, breaks = c(0,50,8000)),
                            SPECIES %in% c("SS") ~.bincode(LENGTH, breaks = c(0,25,8000))))%>%
  unite("SPECIES", c(SPECIES, LENGTH), remove = F) %>% 
  filter(!is.na(LENGTH)) %>%
  left_join(c.h.FBL) %>%

  rename(HAB_1 = Habitat) %>% 
  mutate(WATER = "FBL") %>%
  mutate(EFFORT = as.numeric(EFFORT)) %>%
  filter(YEAR >= 2001) %>%
  filter(YEAR <2023) %>%
  rename(SITE = SITE_N)




FBL.CPUE.w.sec = ((CPUE_wide_seconds(FBL_data) %>%
                     unite("Group", c(YEAR, SITE)) %>% 
                     column_to_rownames(., var = "Group") %>% 
                     mutate(sumrow = rowSums(.)) %>%
                     filter(sumrow>0) %>%
                     select(-sumrow))) 

site_hab = FBL_data %>% select(SITE, HAB_1) %>%
  rename(Site = SITE) %>%
  unique() %>% arrange(Site) %>% na.omit()

species.FBL = colnames(FBL.CPUE.w.sec) 

species_names.FBL = c("Creek Chub","Lake Trout","Central Mudminnow", "Smallmouth Bass",  "Brook Trout", "White Sucker")

length_graph = rep(c("< 100 mm", "> 100 mm"), length(species_names.FBL))


FBL_v = FBL.CPUE.w.sec %>% 
  mutate(y_s = rownames(FBL.CPUE.w.sec)) %>%
  pivot_longer(1:length(species.FBL),
               names_to = "Species") %>%
  separate(y_s, 
           into = c("Year", "SITE_N"), sep = "_") %>%
  left_join(habs) %>%
  unite("ID", 
        c(SITE_N,Species), 
        sep = "_", 
        remove = F) %>%
  rename(HAB_1 = Habitat) %>%
  mutate(value = value * 60 * 60 ) %>%
  filter(Year != 2002)

save(file = "Data/ChangePoint_Data/FBL_v.RData", FBL_v)




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


## Supplemental 

## Contain the catch data for each sampling event

facet_data = data.frame(WATER = c("FBL", "LML"), 
                        YEAR = c(2003, 2000))
water_labels = c("FBL" = "First Bisby", "LML" = "Little Moose")

BEF_data_unfiltered %>%
  filter(WATER == "FBL" & YEAR > 2000 | WATER == "LML" & YEAR > 1997) %>%
  select(WATER, YEAR, MONTH, SITE_N, EFFORT, DAY_N, GEAR_CODE) %>%
  group_by(WATER, YEAR, MONTH, DAY_N, GEAR_CODE) %>%
  summarize(EFFORT = sum(EFFORT)) %>%
  filter(GEAR_CODE == "NAF", MONTH < 7) %>%
  ggplot(aes(x = YEAR,
             y = DAY_N,
             col = round(EFFORT,digits = 2))) +
  theme_minimal() +
  geom_vline(data = facet_data, aes(xintercept =YEAR), linetype = 2) +
 
  geom_point() + 
  ylab("Day of Year") + 
  labs(color = "Total Effort (sec)") +  

  xlab("") + 
  theme(axis.text = element_text(size = 13), 
        #text = element_text(size = 13), 
        axis.text.x = element_text(angle = 90, vjust = .5)) +
  

  xlim(1998, 2022) + 
  scale_color_viridis_c(trans = "log", breaks = c(2980, 22026, 162754, 1202602)) + 
  facet_wrap(~WATER, labeller = labeller(WATER = water_labels))


