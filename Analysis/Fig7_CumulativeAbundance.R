## Species accumulation curves

## Libraries 
library(tidyverse)
library(vegan)


read.csv("Data/LML_CPUE.csv")

load("Data/ChangePoint_Data/LMLV.Rdata") 

load("Data/ChangePoint_Data/FBL_v.RData")

FBL_v = FBL_v %>% select(Year, ID, Species, value, HAB_1, SITE_N, WATER) %>%
  rename(SITE = SITE_N) %>%
  filter(Year > 2003)
LML.v = LML.v %>% mutate(WATER = "LML") %>%
  filter(Year > 2000)

v.combined = rbind(LML.v, FBL_v)

totals = v.combined %>% 
  filter(Year > 2000) %>%
  separate(Species, into = c("SP", "age")) %>%
  group_by(WATER, Year, SP) %>%
  summarize(year_sum = sum(value))

cumulative.abund = v.combined %>% 
  filter(Year > 2000) %>%
  separate(Species, into = c("SP", "age"))%>% 
  group_by(WATER, Year, SP, SITE) %>%
  summarize(sp_total = sum(value)) %>%
  left_join(totals) %>%
  mutate(proportion = sp_total / year_sum * 100) %>%
  ungroup() %>% 
  group_by(WATER, SP, SITE) %>%
  summarize(mean_proportion = mean(proportion,na.rm = T))
 




## Cumulative abundance
labels = c("CC" = "creek chub", "CS" = "common shiner","MM" =  "central mudinnow","PS" = "pumpkinseed","SMB" = "smallmouth bass", "WS" = "white sucker")
cumulative.abund %>% 
  ungroup() %>% 
  group_by(WATER, SP) %>%
  arrange(WATER, SP, -mean_proportion) %>%
  
  mutate(index = 1:length(unique(SITE))) %>%
  
  filter(SP %in% c("CC", "CS","PS","MM","SMB","WS")) %>%
  mutate(proportion_sites = index / max(index) * 100) %>%
  mutate(cum = cumsum(mean_proportion)) %>%
  
  ggplot(aes(x = proportion_sites, y = cum, col = WATER)) + 
  theme_minimal() + 
  geom_line(lwd = 1) + 
  facet_wrap(~SP, labeller = labeller(SP = labels)) + 
  ylab("Cummulative Percent of Abundance") +
  xlab("Percent of Sites Sampled") +
  scale_color_manual("Lake", labels = c("First Bisby","Little Moose"), values = c("#91bab6","#E79805"))



