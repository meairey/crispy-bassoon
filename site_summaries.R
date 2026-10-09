
persist.data = rbind(FBL_v %>%
  filter(Year > 2003) %>%
  
  group_by(Species, Year) %>%
  mutate(value = as.numeric(scale(value))) %>%
  select(WATER, Year, Species, SITE_N, value),
   LML.v %>%
  separate(ID, into = c("SITE_N", "Species", "Age"), sep = "_") %>%
  filter(Species != "ST") %>%
  unite("Species", Species, Age, sep = "_") %>% 

  na.omit() %>% 
  filter(Year > 2000) %>%
   
  group_by(Species, Year) %>%
  mutate(value = as.numeric(scale(value))) %>%
  ungroup()%>%
  mutate(WATER = "LML") %>%
  select(WATER, Year, Species, SITE_N, value)) %>%
  ungroup() %>%
  filter(Species %nin% c("SMB_1", "SMB_2"))  %>% 
  
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
  separate(SITE_N, into = c("GEAR", "WA", "SITE")) %>%
  unite("SITE_N" , WA, SITE, sep = ".")

  

persist.data %>%
  ggplot(aes(x = Species, y = SITE_N, fill = as.factor(rank_bin) )) + 
  geom_tile() +
  scale_fill_viridis_d(direction = -1) + 
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
  slice(1:3) 

summ.dat %>%
  left_join(env_updated_LML) %>% 
  pivot_longer(FW:ncol(.), names_to = "Habitat", values_to = "hab_val") %>%
  mutate(Habitat = factor(Habitat, levels = c("BED", "B", "C", "CW","FW","EV","SV","O"))) %>%
  ggplot(aes(x = Habitat, y = SITE_N, fill = hab_val)) + 
  geom_tile() 

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





















