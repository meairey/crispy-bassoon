library(tidyverse)
`%nin%` = Negate(`%in%`)


library(scales)
## FBL_v
#load("Data/ChangePoint_Data/FBL_v.RData")
load(file = "Data/ChangePoint_Data/FBL_v2.RData") 
load(file = "Data/LML.v.post2000.RData") ## updated
## Habit data
env_updated_LML = read.csv("Data/LML_habitat.csv")
env_updated_FBL = read.csv("Data/FBL_habitat.csv")


env_updated_FBL = read.csv("Data/FBL_habitat.csv")

updated.fbl.habs = read.csv("Data/updated_habs_testing.FBL.csv") %>%
  select(SITE_N, new_hab) %>% 
  rename(HAB_1 = new_hab)

updated.lml.habs = read.csv("Data/updated_habs_LML.csv") %>%
  select(SITE_N, new_hab) %>% 
  rename(HAB_1 = new_hab)

updated.habs = rbind(updated.fbl.habs, 
                     updated.lml.habs) %>% 
  ## Testing consolidating the new habitats
  mutate(HAB_1 = case_when(HAB_1 %in% c("S", "RS") ~ "S", 
                           HAB_1 %in% c("RSW", "SW") ~ "SW", 
                           HAB_1 == "RW" ~ "RW", 
                           HAB_1 == "R" ~ "R"))


FBL_v %>% filter(Year == 2013) %>% 
  select(SITE_N) %>%
  unique()



rbind(LML.v %>% select(Year, SITE_N) %>%
        mutate(WATER = "LML"),
      FBL_v %>% select(Year, SITE_N) %>% 
        mutate(WATER = "FBL")) %>%
  unique() %>% left_join(updated.habs) %>%
  na.omit() %>%
  mutate(HAB_1 = case_when(HAB_1 %in% c("S", "RS") ~ "S", 
                           HAB_1 %in% c("RSW", "SW") ~ "SW", 
                           HAB_1 == "RW" ~ "RW", 
                           HAB_1 == "R" ~ "R")) %>%
  group_by(WATER, HAB_1, Year) %>%
  summarize(sites = n()) %>% 
  pivot_wider(names_from = HAB_1, values_from = sites) %>%
  print(n = 100)

## FBL_v
#load("Data/ChangePoint_Data/FBL_v.RData")
load(file = "Data/ChangePoint_Data/FBL_v2.RData") 
load(file = "Data/LML.v.post2000.RData") ## updated
env_updated_LML = read.csv("Data/LML_habitat.csv")
env_updated_FBL = read.csv("Data/FBL_habitat.csv")
LML.v %>% group_by(, Species) %>%
  filter(value == max(value, na.rm = T))

persist.data = rbind(FBL_v %>%
  filter(Year > 2003) %>%
  
  group_by(Species, Year) %>%
  mutate(value = as.numeric(scale(value))) %>%
  mutate(WATER = "FBL") %>%
  select(WATER, Year, Species, SITE_N, value) %>%
    filter(SITE_N != "BEF.FBL.001"),
   LML.v %>%

  filter(Species != "ST") %>%


  na.omit() %>% 
  filter(Year > 2000) %>%
   
  group_by(Species, Year) %>%
  mutate(value = as.numeric(scale(value))) %>%
  ungroup()%>%
  mutate(WATER = "LML") %>%
  select(WATER, Year, Species, SITE_N, value)) %>%
  ungroup() %>%
  #filter(Species %nin% c("SMB_1", "SMB_2"))  %>% 
  
  arrange(WATER, Year, Species, -value) %>%
  group_by(WATER, Year, Species) %>%
  mutate(index = row_number()) %>%
  ungroup() %>%
  group_by(WATER, Species, SITE_N) %>%
  summarize(
    mean_rank = median(index, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(rank_bin = cut(
      mean_rank,
      breaks = (c(0, 5, 10, 15, 20, 25, 33)),
      labels = (c("1–5", "6–10", "11–15", "16–20", "21–25", "26–32"))
    )) %>%
  mutate(WATER = factor(WATER, levels = c("LML", "FBL"))) %>%
  left_join(updated.habs) %>%
  separate(SITE_N, into = c("GEAR", "WA", "SITE")) %>%
  unite("SITE_N" , WA, SITE, sep = ".") %>%
  filter(!grepl("LT", Species),
         !grepl("RS", Species), 
         !grepl("BB", Species)) %>%
  ungroup() %>% 
  group_by(WATER) %>%
  mutate(mean_rank = rescale(mean_rank)) %>% 
  filter(!grepl("LML.029", SITE_N)) 

  

persist.data %>% 
  
  select(SITE_N, HAB_1) %>%
  unique() %>%
  print(n = 100)

persist.data %>%
  ggplot(aes(x = Species, y = SITE_N, 
             #fill = as.factor(rank_bin)
             fill = mean_rank
             )) + 
  geom_tile() +
  #scale_fill_viridis_d(direction = -1) + 
  scale_fill_viridis_c(direction = -1) +
  facet_wrap(~WATER, scales = "free") + 
  theme_bw() + 
  ylab("Site") + 
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5
                                   )) + 
  scale_x_discrete(labels = c("CC_1" = "CC (J)", 
                              "CC_2" = "CC (A)", 
                              "CS_1" = "CS (J)",
                              "CS_2" = "CS (A)", 
                              "MM_1" = "MM (J)", 
                              "MM_2" = "MM (A)",
                              "PS_1" = "PS (J)", 
                              "PS_2" = "PS (A)", 
                              "WS_1" = "WS (J)", 
                              "WS_2" = "WS (A)")) + 
  labs(fill = "Rank\nImportance")


persist.data %>%
  select(WATER, SITE_N, Species, mean_rank) %>%
  pivot_wider(
    names_from = Species,
    values_from = mean_rank
  ) %>%
  select(-SITE_N) %>%
  cor(method = "spearman", use = "pairwise.complete.obs") %>%
  as.data.frame() %>%
  rownames_to_column(var = "S1") %>%
  pivot_longer(2:ncol(.)) %>%
  filter(S1 != name) %>%
  ggplot(aes(x = S1, y = name, fill = value)) + 
  geom_tile() + 
  scale_fill_viridis_c()


### Pick the like top 5 or so most important sites in the lake and look at them


summ.dat = persist.data %>% 
  arrange(WATER, Species, mean_rank) %>%
  group_by(WATER, Species) %>%
  slice(1:2) %>%
  unite("SITE_N", c(GEAR, SITE_N), sep = ".") %>%
  ungroup() 

summ.dat = persist.data %>%
  filter(!grepl("PS", Species)) %>%
  arrange(WATER, Species, mean_rank) %>%
  group_by(WATER, Species) %>%
  filter(mean_rank < .3) %>%
  unite("SITE_N", c(GEAR, SITE_N), sep = ".") %>%
  ungroup() 

summ.dat %>% print(n = 100)


summ.dat %>% select(SITE_N, HAB_1, rank_bin) %>% 
  unique()

united.habs = rbind(env_updated_LML, 
                    env_updated_FBL %>% 
                      select(SITE_N, FW, CW, O, SV, EV, C, B, BED))

summ.dat %>%
  select(WATER, SITE_N, mean_rank, rank_bin) %>%
  unique() %>%
  left_join(united.habs)  %>%

  pivot_longer(FW:ncol(.), names_to = "Habitat", values_to = "hab_val") %>%
  mutate(Habitat = factor(Habitat, levels = c("BED", "B", "C", "CW","FW","EV","SV","O"))) %>%
  filter(Habitat %nin% c("BED","S")) %>%

  ggplot(aes(x = Habitat, y = SITE_N, fill = hab_val)) + 
  geom_tile() +
  scale_fill_viridis_c()

summ.dat %>%
  ungroup() %>% 
  select(WATER, SITE_N, Species) %>%
  mutate(Pres = 1) %>%
  pivot_wider(names_from = Species, values_from = Pres) %>%
  ungroup() %>%
  mutate(sums = rowSums(across(CC_1:WS_2), na.rm =T)) %>%
  left_join(env_updated_LML) %>%
  select(SITE_N, sums, everything()) %>%
  pivot_longer(CC_1:ncol(.), names_to = "Category", values_to = "value") %>%
  mutate(Category = factor(Category, levels = c("CC_1", "CC_2", "CS_1", "CS_2",
                                                "MM_1", "MM_2", "PS_1", "PS_2",
                                                "WS_1", "WS_2",
                                                
                                                "BED", "B", "C", "R","G","CW","FW","EV","SV","O"))) %>%
  na.omit() %>%
  ggplot(aes(x =  Category, y = reorder(SITE_N, sums), fill = value)) + 
  geom_tile() + 
  facet_wrap(~WATER, scales = "free")

  
summ.dat %>%
  ungroup() %>% 
  group_by( SITE_N) %>%
  reframe(unique(Species))





## Thinking of an abundance bar chart

rbind(FBL_v %>%
  filter(Year > 2003) %>%
  
  group_by(Species, Year) %>%
  mutate(value = rescale(value)) %>%
  mutate(WATER = "FBL") %>%
  select(WATER, Year, Species, SITE_N, value) %>%
    filter(SITE_N != "BEF.FBL.001"),
   LML.v %>%

  filter(Species != "ST") %>%


  na.omit() %>% 
  filter(Year > 2000) %>%
   
  group_by(Species, Year) %>%
  mutate(value = rescale((value))) %>%
  summarize(sum(value))
  ungroup()%>%
  mutate(WATER = "LML") %>%
  select(WATER, Year, Species, SITE_N, value)) %>%
  ungroup() %>%
  filter(!grepl("BB", Species), 
         !grepl("ST", Species), 
         !grepl("LT", Species)#,
        # !grepl("SMB", Species)
        )  %>% 
  
  arrange(WATER, Year, Species, -value) %>%
  left_join(updated.habs) %>%
  group_by(WATER, Species, SITE_N, HAB_1) %>%
  summarize(mean_CPUE = mean(value)) %>%
  ungroup() %>%
  group_by(WATER, Species) %>%
  summarize(sum(mean_CPUE))

  ggplot(aes(x = Species, y = mean_CPUE, fill = HAB_1, col = SITE_N)) + 
  geom_bar(stat = "identity") + facet_wrap(~WATER, scales = "free") +
  guides(color = "none")




%>%
  
  mutate(WATER = factor(WATER, levels = c("LML", "FBL"))) %>%
  left_join(updated.habs) %>%
  separate(SITE_N, into = c("GEAR", "WA", "SITE")) %>%
  unite("SITE_N" , WA, SITE, sep = ".") %>%
  filter(!grepl("LT", Species),
         !grepl("RS", Species), 
         !grepl("BB", Species)) %>%
  ungroup() %>% 
  group_by(WATER) %>%
  mutate(mean_rank = rescale(mean_rank)) %>% 
  filter(!grepl("LML.029", SITE_N)) 













