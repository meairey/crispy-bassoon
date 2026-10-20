set.seed(123)
`%nin%` = Negate(`%in%`)
## Species accumulation curves

## Libraries -----
library(tidyverse)
library(vegan)
library(lmerTest)
library(dplyr)
library(ineq)
library(wesanderson)


## Prep Data ----
## First Bisby data
load("Data/ChangePoint_Data/FBL_v.RData") ## Generated in Fig4_changepoint_analysis.R
FBL_v = FBL_v %>% 
  rename(SITE = SITE_N) %>%
  select(Year, ID, Species, value, HAB_1, SITE, WATER) %>%
  filter(Year > 2003)

## Little Moose data
load("Data/ChangePoint_Data/LMLV.Rdata") ## Generated in Fig4_changepoint_analysis.R
LML.v = LML.v  %>%
  select(Year, ID, Species, value, HAB_1, SITE )%>% 
  mutate(WATER = "LML") %>%
  filter(Year > 2000, 
         !grepl("RT", Species)) 


labels = c("CC" = "creek chub", "CS" = "common shiner","MM" =  "central mudinnow","PS" = "pumpkinseed","SMB" = "smallmouth bass", "WS" = "white sucker", "ST" = "brook trout", "LLS" = "Atlantic salmon", "LT" = "lake trout")


## Combine both lakes for facet wrap
v.combined = rbind(LML.v, FBL_v)

## Geni Coeffficient -----------------------------------------------------------

spatial_evenness =  v.combined %>% 
  filter(Year > 2000) %>%
  separate(Species, into = c("SP", "age"))%>% 
  group_by(WATER, Year, SP, SITE) %>%
  summarize(sp_total = sum(value)) %>%
  ungroup() %>% 
  mutate(WATER = factor(WATER, levels = c("LML", "FBL"))) %>%
  ungroup() %>%
  group_by(WATER, Year, SP) %>%
  summarise(
    occupancy = mean(sp_total > 0),
    gini = Gini(sp_total),
    .groups = "drop"
  ) %>%
 # filter(SP %in% c("CC", "CS","PS","MM","SMB","WS") ) %>%
  mutate(year.scaled = scale(as.numeric(Year)))



## LMs to assess Gini Coefficient
library(emmeans)

## Little Moose Models
lml.gini.mod = lm(gini ~  year.scaled*SP, data = spatial_evenness %>% filter(WATER == "LML")) 
summary(lml.gini.mod)


# Do the species differ in intercept from eachother (yes most do)
emmeans(lml.gini.mod, pairwise ~ SP)
# Do the species slopes differ from 0. in LML only pumpkinseed does
lml.sig = emtrends(lml.gini.mod, ~ SP, var = "year.scaled") %>% 
  as.data.frame() %>% 
  mutate(sig = sign(lower.CL) == sign(upper.CL)) %>%
  mutate(WATER = "LML")

## First Bisby Models
fbl.gini.mod = lm(gini ~  year.scaled*SP, data = spatial_evenness %>% filter(WATER == "FBL")) 
summary(fbl.gini.mod)

# Do the species differ in intercept from eachother (yes most do)
emmeans(fbl.gini.mod, pairwise ~ SP)

# Do the species slopes differ from 0. in LML only pumpkinseed does
fbl.sig = emtrends(fbl.gini.mod, ~ SP, var = "year.scaled") %>% 
  as.data.frame() %>% 
  mutate(sig = sign(lower.CL) == sign(upper.CL)) %>%
  mutate(WATER = "FBL") 

## Join emtrends for both lakes
sig = rbind(lml.sig, fbl.sig) %>%
  as.data.frame()

## Supplemental emmeans + etrends table

Table.Supp.ETrends = sig %>% 
    mutate(SP = str_replace_all(SP, c(
    "CC" = "creek chub",
    "CS" = "common shiner",
    "MM" = "cenral mudminnow",
    "PS" = "pumpkinseed",
    "SMB" = "smallmouth bass",
    "WS" = "white sucker"
  ))) %>%
  ## replace lake names
  mutate(WATER = str_replace_all(WATER, c("LML" = "Little Moose", 
                                          "FBL" = "First Bisby"))) %>%
  rename(Lake = WATER, ## rename columns for supplement table
         Species = SP) %>% 
  select(-sig) %>% ## remove extra significance column
  mutate(across(year.scaled.trend:upper.CL, ~round(.x, digits = 3))) ## round all values

## Write table
write.csv(Table.Supp.ETrends,
          file = "Figures_Tables/GeniCoefficient/SuppTable_GeniCoefficient_Trends.csv",
          row.names = F)


FBL.emmeans = emmeans(fbl.gini.mod, pairwise ~ SP) ## Emmeans for first bisby

LML.emmeans = emmeans(lml.gini.mod, pairwise ~ SP) ## Emmeans for Little Moose

## Combine both emmeans into a joint table for supplement
Table.Supp.Emmeans = rbind(FBL.emmeans$emmeans %>% as.data.frame() %>% mutate(Lake = "First Bisby"),
      LML.emmeans$emmeans %>% as.data.frame() %>% mutate(Lake = "Little Moose")) %>% 
      mutate(SP = str_replace_all(SP, c( ## switch species codes for common names
    "CC" = "creek chub",
    "CS" = "common shiner",
    "MM" = "cenral mudminnow",
    "PS" = "pumpkinseed",
    "SMB" = "smallmouth bass",
    "WS" = "white sucker"
  ))) %>%
  rename(Species = SP) %>% ## rename columns
  mutate(across(emmean:upper.CL, ~round(.x, digits = 3))) ## round all values 

## Write Table
write.csv(Table.Supp.Emmeans,
          file = "Figures_Tables/GeniCoefficient/SuppTable_GeniCoefficient_emmeans.csv",
          row.names = F)

## Contrasts

contrasts = rbind(FBL.emmeans$contrasts %>% as.data.frame() %>% mutate(Lake = "First Bisby"),
      LML.emmeans$contrasts %>% as.data.frame() %>% mutate(Lake = "Little Moose")) %>%
  mutate(p.value = case_when(p.value < .001 ~ "< 0.001", 
                             p.value < .01 ~ "< 0.01", 
                             p.value >= 0.01 ~ as.character(round(p.value, digits = 3)))) %>%
  mutate(across(estimate:t.ratio, ~round(.x, digits = 3)))

## Write table 
write.csv(contrasts,
          file = "Figures_Tables/GeniCoefficient/SuppTable_GeniCoefficient_contrasts.csv", 
          row.names = F)





## Visualize Gini Coefficient
spatial_evenness %>% 
  left_join(sig, by = c("WATER", "SP")) %>%
  mutate(WATER = factor(WATER, levels = c("LML", "FBL"))) %>%
  ggplot(aes(x = as.numeric(Year), y = gini, col = SP)) + 
  geom_point(alpha = .2) +
  facet_wrap(~WATER, labeller = labeller(WATER = c( "LML"="Little Moose", "FBL" = "First Bisby"))) + 
  geom_smooth(aes(lty = sig),method = "lm", se = F) +
  theme_minimal(base_size = 12) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle= 45)) + 
  ylab("Gini Coefficient") + 
  scale_color_manual("Species", values = wes_palette("Darjeeling1",type = "c", n = 10), 
                     labels = labels) +
  scale_linetype_manual("Significance", values = c("dashed", "solid")) + 
  guides(linetype = F)


ggsave(file = "Figures_Tables/Figure2_GeniCoefficient.jpeg", width = , height = 4, dpi = 600)

#






## Cumulative abundances -------------

## Calculate total ind/species/year/water
totals = v.combined %>% 
  filter(Year > 2000) %>%
  separate(Species, into = c("SP", "age")) %>%
  group_by(WATER, Year, SP) %>%
  summarize(year_sum = sum(value))

## Calculate the cumulative abundance
cumulative.abund = v.combined %>% 
  filter(Year > 2000) %>%
  separate(Species, into = c("SP", "age"))%>% 
  group_by(WATER, Year, SP, SITE, HAB_1) %>%
  summarize(sp_total = sum(value)) %>%
  left_join(totals) %>%
  mutate(proportion = sp_total / year_sum * 100) %>%
  ungroup() %>% 
  group_by(WATER, SP, SITE, HAB_1) %>%
  summarize(mean_proportion = mean(proportion,na.rm = T))
 



## Visualization -----




## Cumulative abundance figure
cumabund = cumulative.abund %>% 
  mutate(WATER = factor(WATER, levels = c("LML", "FBL"))) %>%
  ungroup() %>% 
  group_by(WATER, SP) %>%
  arrange(WATER, SP, -mean_proportion) %>%
  mutate(index = 1:length(unique(SITE))) %>%
  filter(SP %in% c("CC", "CS","PS","MM","SMB","WS")) %>% ## Species of interest
  mutate(proportion_sites = index / max(index) * 100) %>% 
  mutate(cumu = cumsum(mean_proportion)) %>%
  ggplot(aes(x = proportion_sites, y = cumu, col = WATER)) + 
  theme_minimal(base_size = 12) + 
  geom_line(lwd = 1) + 
  facet_wrap(~SP, labeller = labeller(SP = labels)) + 
  ylab("Cummulative Percent of Abundance") +
  xlab("Percent of Sites Sampled") +
  scale_color_manual("Lake", labels = c("FBL" = "First Bisby","LML" = "Little Moose"), values = c("#91bab6","#E79805")) 



## Proportional selection of habitat ratios
hab.proportion = habs %>% 
  group_by(WATER, Habitat) %>%
  summarize(prop.habs = n()) %>%
  mutate(total_sites = case_when(WATER == "LML" ~ 32, WATER == "FBL" ~ 15)) %>%
  mutate(prop.habs = prop.habs / total_sites)
prop.selection =  v.combined %>% 
  filter(Year > 2000) %>%
  separate(Species, into = c("SP", "age"))%>% 
  group_by(WATER, Year, SP, HAB_1) %>% 
  summarize(sp_total = sum(value)) %>% ## Total CPUE per habitat
  ungroup() %>% 
  mutate(WATER = factor(WATER, levels = c("LML", "FBL"))) %>%
  ungroup()%>%
  left_join(totals) %>% 
  left_join(hab.proportion, by = c("WATER", "HAB_1" = "Habitat")) %>%
  ungroup() %>%
  mutate(hab.sp.prop = sp_total / year_sum) %>%
  mutate(selection = hab.sp.prop / prop.habs)

prop.selection %>% 
   filter(SP %in% c("CC", "CS","PS","MM","SMB","WS") ) %>%
  ggplot(aes(x = as.numeric(Year), y = selection, col = HAB_1)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  facet_wrap(~WATER + SP, scales = "free")





### Old Cumulative Abundance ----------------

### I'm wondering if cumulative abundances change by year
## Calculate total ind/species/year/water
totals = v.combined %>% 
  filter(Year > 2000) %>%
  separate(Species, into = c("SP", "age")) %>%
  group_by(WATER, Year, SP) %>%
  summarize(year_sum = sum(value))


## Calculate the cumulative abundance
cumulative.abund = v.combined %>% 
  filter(Year > 2000) %>%
  separate(Species, into = c("SP", "age"))%>% 
  group_by(WATER, Year, SP, SITE) %>%
  summarize(sp_total = sum(value)) %>%
  left_join(totals) %>%
  mutate(proportion = sp_total / year_sum * 100) %>%
  ungroup() %>% 
  mutate(WATER = factor(WATER, levels = c("LML", "FBL"))) %>%
  ungroup() %>% 
  group_by(WATER, SP, Year) %>%
  arrange(WATER, Year, SP, -proportion) %>%
  mutate(index = 1:length(unique(SITE))) %>%
  filter(SP %in% c("CC", "CS","PS","MM","SMB","WS")) %>% ## Species of interest
  mutate(proportion_sites = index / max(index) * 100) %>% 
  mutate(cumu = cumsum(proportion))# %>%
  filter(cumu < 70) 

lmer(proportion ~  as.numeric(Year)*SP + (1|SITE), data = cumulative.abund %>% filter(WATER == "LML")) %>% summary()
lmer(proportion ~  as.numeric(Year)*SP + (1|SITE), data = cumulative.abund %>% filter(WATER == "FBL")) %>% summary()


cumulative.abund %>% 
  group_by(SP, Year) %>%
  arrange(-proportion) %>%
  slice(1:3) %>%
 # filter(SP== "CC") %>%
  ggplot(aes(x = as.numeric(Year), y = proportion, col = SP)) + 
  geom_point() +
  geom_smooth(method = "lm") + 
    facet_wrap(~WATER)


## Importance of different habitats in a different way (using cumulative sums)
habitat_importance = cumulative.abund %>% 
  mutate(WATER = factor(WATER, levels = c("LML", "FBL"))) %>%
  mutate(HAB_1 = str_replace_all(HAB_1, c("RW$"="Rock +\nWood",
                                          "R$" = "Rock", 
                                          "SW$" = "Sediment +\nWood",
                                          "S$" = "Sediment"))) %>%
  ungroup() %>% 
  group_by(WATER, SP) %>%
  arrange(WATER, SP, -mean_proportion) %>%
  mutate(index = 1:length(unique(SITE))) %>%
  filter(SP %in% c("CC", "CS","PS","MM","SMB","WS")) %>% ## Species of interest
  mutate(proportion_sites = index / max(index) * 100) %>% 
  mutate(cumu = cumsum(mean_proportion)) %>%
  ungroup() %>% 
  filter(cumu < 70) %>%
  group_by(WATER, HAB_1, SP) %>%
  summarize(n_sites = n(), 
            mean_proportion = sum(mean_proportion)) %>%
  ggplot(aes(x = HAB_1, y = mean_proportion, fill = WATER)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = n_sites),
            position = position_dodge(width = 0.9),
            vjust = -0.2) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  facet_wrap(~SP, labeller = labeller(SP = labels)) +
  theme_minimal(base_size = 12) +
  scale_fill_manual("Lake", labels = c("FBL" = "First Bisby","LML" = "Little Moose"), values = c("#91bab6","#E79805")) +
  geom_vline(xintercept = 2.5, linetype = "dashed", color = "black") + 
  ylab("Percent of Total Abundance") +
  xlab("Habitat Type") + 
  theme(axis.text.x = element_text(angle = 90))
  

library(patchwork)
figure_2 = cumabund + habitat_importance + plot_layout(ncol = 1)

ggsave(figure_2, file = "Figures_Tables/Figure2_CumulativeAbundance.pdf", width = 6, height = 6.5)
