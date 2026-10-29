## !!!Run this to make sure that you have the right directory!!!

setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/crispy-bassoon/")

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
#sample = sample 

## Contain the catch data for each sampling event
BEF_data_unfiltered = left_join(fish, sample, by = "YSAMP_N") %>%
  filter(WATER %in% c("LML", "FBL"), 
         GEAR == "BEF",
         GEAR_CODE == "NAF") %>%
  filter(MONTH %in% c(5,6,7)) %>%  ## Filter to just sample the spring period of sampling
  filter(SITE_N != "BEF.FBL.003,4") ## remove because this sort of confounds habitat assignments


## Checking size histograms
size_cutoff = read.csv("Data/ChangePoint_Data/size_cutoff.csv") %>%
  rename(LENGTH = size_cutoff) %>%
  mutate(WATER = factor(WATER, levels = c("LML", "FBL")))
BEF_data_unfiltered %>%
  mutate(WATER = factor(WATER, levels = c("LML", "FBL"))) %>% 
  filter(SPECIES %in% c("CC", "CS", "MM","WS", "PS", "SMB")) %>%
  filter(!(SPECIES == "CS" & WATER == "FBL")) %>%
  select(WATER, SPECIES, LENGTH) %>% 
  ggplot(aes(x = LENGTH)) + 
  geom_histogram(bins = 50) + 
  facet_wrap(~WATER+SPECIES, scales = "free", 
             labeller = labeller(SPECIES = c("CC" = "creek chub", 
                                             "CS" = "common shiner",
                                             "MM" = "central mudminnow",
                                             "PS" = "pumpkinseed", 
                                             "SMB" = "smallmouth bass",
                                             "WS" = "white sucker"),
                                 WATER = c("LML" = "Little Moose", 
                                           "FBL" = "First Bisby"))) + 
  geom_vline(data = size_cutoff, aes(xintercept = LENGTH), col = "red") + 
  theme_bw() + 
  xlab("Total Length (mm)") + 
  ylab("Count")




rare_threashold = 50 ## To filter out rare taxa


BEF_data_unfiltered %>% #filter(SITE_N == "BEF.FBL.003,4") %>% 
  filter(YEAR == 2011) %>% select(SITE_N) %>% unique()



## Habitat info 
## iMPORTANT TO TAKE OUT OF CRIPSY-BASSOON REPOSITORY
habs = read.csv("../crispy-bassoon/Data/updated_habitat.csv") %>% 
filter(Water %in% c("FBL", "LML"))%>%
  select(Water, SITE_N, Habitat) %>%
  na.omit() %>%
  filter(Habitat != "N") %>%
  rename("WATER" = Water)


## Removing species ---------------

## Rare taxa are removed below the rare_threshold defined above
frequency = BEF_data_unfiltered %>% group_by(WATER, SPECIES) %>% 
  summarise(frequency = n())# %>%  ## This get's filtered out below now maybe delete this
 # filter(frequency < rare_threashold)
stocked = c("LLS", "RT", "ST") ## Stocked fish in little moose and FBL
pelagic = c("LT", "RS","RWF")
remove = c(stocked) ## Remove stocked taxa, and possibly pelagic taxa that we're not focusing on

# Define Boat electrofishing data with target taxa only
BEF_data = BEF_data_unfiltered %>%
  left_join(frequency) %>% ## Join rare data frame
  filter(frequency > rare_threashold, # remove rare
         SPECIES %nin% remove) %>% # remove stocked and non-target taxa
  select(-frequency)
## 

BEF_data$SPECIES %>% unique()
## For this modified change point analysis you have to be after removal initiation
LML_unfiltered = BEF_data_unfiltered %>% 
  filter(WATER == "LML") %>%
  filter(YEAR > 2000)

LML_unfiltered


## Create matrices for each cluster of years that were sampled uniquely

## Standardized sites post 2000
post_2000 = LML_unfiltered %>% # Take LML data
  filter(YEAR == 2005) %>% ## Just filtered for any year during the standardized sampling
  select(SITE_N) %>% # select just site numbers
  unique() %>% ## unique sites
  separate(SITE_N, into = c("year", "water", "SITE"), remove = F) %>% ## separate out to get the SITE component of the SITE_N
  mutate(SITE_num = parse_number(SITE)) %>% ## Assign number in chronological order
  select(SITE_N, SITE_num) ## select just the SITE_N and the new assigned name


## I am trying out if i go in and remove certain sites
LML.habs = read.csv("Data/updated_habs_LML.csv") ## this is the mixed substrate sites
#remove_sites = (updated_hab %>% filter(same == F))$SITE_N
## Intermediate to go into LML.CPUE.w.sec

LML_data = LML_unfiltered %>% filter(SPECIES %nin% remove) %>% 
  mutate(LENGTH = case_when(SPECIES %in% c("BB") ~ .bincode(LENGTH, breaks = c(0,100,5000)),
                            SPECIES %in% c("MM") ~.bincode(LENGTH, breaks = c(0,55.5,5000)),
                            SPECIES %in% c("CS") ~ .bincode(LENGTH, breaks = c(0,70.2,50000)),
                            SPECIES %in% c("PS") ~ .bincode(LENGTH, breaks = c(0,47.7,50000)),
                            SPECIES %in% c("WS") ~ .bincode(LENGTH, breaks = c(0,127.6,50000)),
                            SPECIES %in% c("SMB") ~ .bincode(LENGTH, breaks = c(0,99.2,50000)),
                            SPECIES %in% c("CC") ~ .bincode(LENGTH, breaks = c(0,84.015,50000)),
                            SPECIES %in% c("RS") ~ .bincode(LENGTH, breaks = c(0,100, 5000)),
                            SPECIES %in% c("ST","LLS","RT","LT") ~ .bincode(LENGTH, breaks = c(0,50,50000)))) %>%
  
  unite("SPECIES", c(SPECIES, LENGTH), remove = F) %>% 
  #filter(!is.na(LENGTH)) %>%
 # mutate(SITE = as.numeric(SITE)) %>%
  left_join(LML.habs) %>%
  rename(HAB_1 = new_hab) %>% ## set the updated habitat classification as the HAB_1
  mutate(EFFORT = as.numeric(EFFORT)) %>%
  rename(SITE = SITE_N) %>%
  filter(SPECIES %nin% remove, 
         YEAR != 2002) 

## Double check how many individuals of each species end up excluded from the changepoint analysis
LML_data %>% select(YEAR, SPECIES, LENGTH) %>% 
  filter(grepl("CS", SPECIES)|grepl("CC", SPECIES)|grepl("SMB", SPECIES)|grepl("MM", SPECIES)|grepl("PS", SPECIES)|         grepl("WS", SPECIES)) %>%
  group_by(YEAR, SPECIES, LENGTH) %>%
  summarize(sum = n()) %>%
  filter(!is.na(LENGTH)) %>% 
  print(n = 300) %>% 
  ungroup() %>% 
  group_by(SPECIES) %>% 
  summarize(mean = mean(sum))

## Final data frame for analysis
LML.CPUE.w.sec = CPUE_wide_seconds(LML_data) %>%
  unite("Group", c(YEAR, SITE)) %>% 
  column_to_rownames(., var = "Group") %>% 
  mutate(sumrow = rowSums(.)) %>%
  filter(sumrow>0) %>%
  select(-sumrow) %>% 
  select(!contains("NA"))

## Intermediate for c.h. that includes the site names and the habitats associated with them
LML_cpue.habs = LML.CPUE.w.sec %>% 
  unique() %>%
  mutate(names = rownames(.)) %>% 
  separate(names, into = c("YEAR", "SITE_N"), sep = "_") %>%
  left_join(LML.habs) %>% 
  #select(SITE_N, new_hab, everything()) %>%
  select(SITE_N, new_hab) %>%
  unique()

## Species names for the species in LML
species_names.LML = c("brown bullhead", "creek chub", "common shiner", "lake trout", 
                      "central mudminnow", "pumpkinseed", "rainbow smelt", 
                      "round whitefish", "smallmouth bass", "slimy sculpin","white sucker")


## Matrix that includes the site_N and the new assigned site numbers
c.h = unique(LML_cpue.habs) 
#c.h = site_matrix %>% left_join(LML_cpue.habs) %>% na.omit() 
dim(c.h)





## Create data frame that will go into the changepoint analysis
### Every site in each year is ordered through the total unique sites sampled that year that were assigned a habitat value


LML.v = LML.CPUE.w.sec %>% 
  mutate(y_s = rownames(LML.CPUE.w.sec)) %>%
  pivot_longer(1:length(colnames(LML.CPUE.w.sec)),
               names_to = "Species") %>%
  separate(y_s, 
           into = c("Year", "SITE_N"), sep = "_") %>%
  left_join(c.h) %>%
 # unite("ID", 
   #     c(SITE_N,Species), 
    #    sep = "_", 
     #   remove = F) %>%
  #select(-SITE_N) %>%
  rename(HAB_1 = new_hab) %>%
  mutate(value = value * 60 * 60 ) %>%
  filter(Year != 2002)# %>% ## Remove year 2002 because of boat logistical issues
  ## Remove Site 001 because of weird habitat classiications
  #filter(!grepl("BEF.LML.001", SITE_N))



save(LML.v, file = "Data/LML.v.post2000.RData")




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
## Write these into data files that can be loaded later
#save(file = "../crispy-bassoon/Data/LMLTotals.Rdata", LMLtotals)
#save(file = "../crispy-bassoon/Data/ChangePoint_Data/LMLV.Rdata", LML.v)
## -------------------------- First bisby -------------------------

# Removing species ---------------


FBL_data_unfiltered = left_join(fish, sample, by = "YSAMP_N") %>%
  filter(WATER == "FBL", 
         GEAR == "BEF",
         GEAR_CODE == "NAF") %>%
  separate(SITE_N, into = c("year", "water", "SITE"), remove = F) %>%
  select(-year, -water, -SITE) %>% 
  filter(MONTH %in% c(5,6,7)) %>% 
  filter(SITE_N != "BEF.FBL.003,4", 
         YEAR >= 2004, 
         !(YEAR == 2011 & SITE_N == "BEF.FBL.004")) 


FBL_data_unfiltered %>% filter(WATER == "FBL", GEAR_CODE == "NAF") %>% select(SITE_N) %>% unique()
FBL_data_unfiltered$SITE_N %>% unique()


FBL_data_unfiltered %>%
  select(SITE_N, YEAR) %>%
  filter(YEAR > 2003) %>%
  unique() %>%
  group_by(SITE_N) %>%
  summarize(n())


# I'm also going to remove LT and RS because I want to focus mostly on native littoral fishes 



c.h.FBL =  read.csv("Data/updated_habs_testing.FBL.csv") 

## Remove this line after
remove = c("LLS", "RT","RS","RWF", "ST", "CS")



FBL_data = FBL_data_unfiltered %>% 
  filter(SPECIES %nin% remove) %>% 
  mutate(LENGTH = case_when(SPECIES %in% c("SMB") ~ .bincode(LENGTH, breaks = c(0,99.2,8000)),
                            
                            SPECIES %in% c("MM") ~.bincode(LENGTH, breaks = c(0,55.5,8000)),
                            SPECIES %in% c("CC") ~ .bincode(LENGTH, breaks = c(0,84.01,80000)),
                            SPECIES %in% c("LT") ~ .bincode(LENGTH, breaks = c(0,100,100000)),
                            SPECIES %in% c("WS") ~.bincode(LENGTH, breaks = c(0,127.6,8000))))%>%
  unite("SPECIES", c(SPECIES, LENGTH), remove = F) %>% 
 # filter(!is.na(LENGTH)) %>%
  left_join(c.h.FBL) %>%

  #rename(HAB_1 = Habitat) %>% 
  mutate(WATER = "FBL") %>%
  mutate(EFFORT = as.numeric(EFFORT)) %>%
  filter(YEAR >= 2004) %>%
  filter(YEAR <2023) %>%
  rename(SITE = SITE_N)

## In First Bisby, there are 110 smallmouth bass observations that didnt get lengths
### And 1 landlocked salmon? All these get removed from the resulting dataframes
FBL_data_unfiltered %>% 
  select(YEAR, SPECIES, LENGTH) %>% 
  group_by(SPECIES, LENGTH) %>%
  summarize(n()) %>%
  filter(is.na(LENGTH))

FBL_data %>% 
  select(YEAR, SPECIES, LENGTH) %>% 
  group_by(YEAR, SPECIES, LENGTH) %>%
  summarize(n()) %>%
  filter(is.na(LENGTH))

  
  
FBL_data %>% filter( YEAR == 2011) -> cat
cat$SITE %>% unique()
FBL_data %>% 
  select(SITE, YEAR) %>% unique() %>% 
  arrange(SITE) %>%
  filter(SITE == "BEF.FBL.004")

FBL.CPUE.w.sec = ((CPUE_wide_seconds(FBL_data) %>%
                     unite("Group", c(YEAR, SITE)) %>% 
                     column_to_rownames(., var = "Group") %>% 
                     mutate(sumrow = rowSums(.)) %>%
                     filter(sumrow>0) %>%
                     select(-sumrow)))%>%
  select(!contains("NA"))


species.FBL = colnames(FBL.CPUE.w.sec) 

species_names.FBL = c("Creek Chub","Lake Trout","Central Mudminnow", "Smallmouth Bass",  "Brook Trout", "White Sucker")



FBL_v = FBL.CPUE.w.sec %>% 
  mutate(y_s = rownames(FBL.CPUE.w.sec)) %>%
  select(y_s, everything()) %>%
  pivot_longer(2:(length(species.FBL)+1),
               names_to = "Species") %>%
  separate(y_s, 
           into = c("Year", "SITE_N"), sep = "_") %>%
  left_join(c.h.FBL) %>%
  unite("ID", 
        c(SITE_N,Species), 
        sep = "_", 
        remove = F) %>%
  rename(HAB_1 = new_hab) %>%
  mutate(value = value * 60 * 60 ) %>%
  filter(Year > 2003)


save(file = "Data/ChangePoint_Data/FBL_v2.RData", FBL_v)




## Whole CPUE for Diversity through time analysis -------------------------------
### uses CPUE of all taxa (non-stocked) 
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


## Write the files for analysis ---------------------------

write.csv(LML.CPUE.w.sec, "Data/CSVs/LML_CPUE.csv", row.names = F)
write.csv(FBL.CPUE.w.sec,"Data/CSVs/FBL_CPUE.csv", row.names = F)
write.csv(CPUE.w.sec, "Data/CSVs/CPUE_whole.csv", row.names = F)



## Supplemental 

## Contain the catch data for each sampling event

facet_data = data.frame(WATER = c("FBL", "LML"), 
                        YEAR = c(2003, 2000)) %>%
  mutate(WATER = factor(WATER, levels = c("LML", "FBL")))
water_labels = c("FBL" = "First Bisby", "LML" = "Little Moose")

### Figure S1 - Figure of total effort per sampling date of study ----------
BEF_data_unfiltered %>%
  mutate(WATER = factor(WATER, levels = c("LML", "FBL")))%>%
  filter((WATER == "FBL" & YEAR > 2003) | (WATER == "LML" & YEAR > 2000)) %>%
  filter(YEAR != 2012) %>%
  select(WATER, YEAR, MONTH, SITE_N, EFFORT, DAY_N, GEAR_CODE) %>%
  group_by(WATER, YEAR, MONTH, DAY_N, GEAR_CODE) %>%
  summarize(EFFORT = sum(EFFORT, na.rm =T)) %>%
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
  theme(axis.text = element_text(size = 11), 
       axis.text.x = element_text(angle = 90, vjust = .5)) +
  scale_color_viridis_c(trans = "log", breaks = c(2980, 22026, 162754, 1202602)) + 
  facet_wrap(~WATER, labeller = labeller(WATER = water_labels))



site.hab.table = BEF_data_unfiltered %>%
  filter(WATER == "FBL" & YEAR > 2000 | WATER == "LML" & YEAR > 1997 & YEAR != 2002, 
         GEAR_CODE == "NAF") %>%
  select(WATER, YEAR,  SITE_N, GEAR_CODE) %>%
  left_join(habs) %>%
  unique() %>%
  group_by(WATER, YEAR, Habitat) %>%

  summarize(n = n()) %>%
  na.omit() %>%
  pivot_wider(names_from = Habitat, values_from = n) %>%
  mutate(R = replace_na(R, 0))


write.csv(file = "Figures_Tables/SiteHabTable.csv", site.hab.table, rownames = F)

### Size at age ------------------------------------------------
### SMB Age/Size 
## I used the table below for the cutoffs
read.csv("Data/Supplemental/FISH_AGE_GROWTH_SMB.csv") %>% 
  left_join(fish, by = c("AGE_FISH_N" = "FISH_N")) %>%
  left_join(sample) %>%
  filter(MONTH > 8) %>%
  select(LENGTH, FINAL_AGE, MONTH, YEAR) %>% 
  group_by(FINAL_AGE) %>%
  summarize(mean = mean(LENGTH, na.rm =T),
            min = min(LENGTH, na.rm = T), ## Min SMB size around age 2 = 102
            max = max(LENGTH, na.rm = T),
            med = median(LENGTH, na.rm = T))
## Sample size for bass calculations
read.csv("Data/Supplemental/FISH_AGE_GROWTH_SMB.csv") %>% 
  left_join(fish, by = c("AGE_FISH_N" = "FISH_N")) %>%
  left_join(sample) %>%
  filter(MONTH < 8) %>%
  select(WATER, YEAR) %>%
  group_by(WATER, YEAR) %>%
  summarize(n()) %>% 
  print(n = 100)
### PS size/age
read.csv("Data/Supplemental/FISH_AGE_GROWTH_PS.csv") %>% 
  left_join(fish, by = c("AGE_FISH_N" = "FISH_N")) %>%
  left_join(sample) %>%
  filter(MONTH < 8) %>%
  select(LENGTH, FINAL_AGE, MONTH, YEAR) %>% 
  group_by(FINAL_AGE) %>%
  summarize(mean = mean(LENGTH, na.rm =T),
            min = min(LENGTH, na.rm = T), ## Min SMB size around age 2 = 102
            max = max(LENGTH, na.rm = T),
            med = median(LENGTH, na.rm = T))
## Sample size for pumpkinseed calculations
read.csv("Data/Supplemental/FISH_AGE_GROWTH_PS.csv") %>% 
  left_join(fish, by = c("AGE_FISH_N" = "FISH_N")) %>%
  left_join(sample) %>%
  filter(MONTH < 8) %>% 
  select(WATER, YEAR) %>%
  group_by(YEAR) %>%
  summarize(n())




