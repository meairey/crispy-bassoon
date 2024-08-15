## LML -------------------------------

#### These are the changepoint analyses of all habitats and species 

## Habitat changepoints ------------------

LML.CPUE.w.sec = read.csv("Data/LML_CPUE.csv") %>% 
  column_to_rownames(var = "X")
species_names = c("brown bullhead", "creek chub", "common shiner", "lake trout", "central mudminnow", "pumpkinseed", "rainbow smelt", "round whitefish", "smallmouth bass", "slimy sculpin","white sucker")
vec = vector()
p.val = vector()
species = colnames(LML.CPUE.w.sec)

change_points_list = list()


v = LML.CPUE.w.sec  %>%
  mutate(y_s = rownames(LML.CPUE.w.sec)) %>%
  pivot_longer(1:WS_2,
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
  filter(Year != 2002)# %>%
# filter(Year > 2000) 
summary_graph_data = list() 


v = v %>% 
  mutate(HAB_1 = str_replace(HAB_1, "SW", "S")) %>%
  mutate(HAB_1 = str_replace(HAB_1, "RW", "R")) %>%
  filter(HAB_1 != "NA")


sandy.98 = c(2,4,5,12,13,11)
rocky.98 = c(1,3,7,8,9,10)

sandy.99 = c(1,4,5)
rocky.99= c(3,2)
color_fixed = data.frame(hex = c("#707173","#56B4E9", "#D55E00","#009E73"), color = c(1:4))



## Fixed change point using multiple sites a year as multivariate --------- 
for(i in 1:length(LML.CPUE.w.sec[1,])){
  list_habitats = list()
  for(h in c("S","R")){
    # Set up data frame
    x = v %>%
      #filter(Year > 1999) %>%
      #rename(HAB_1 = Habitat) %>%
      filter(Species == species[i], HAB_1 == h) %>%
      mutate(value = as.numeric(value)) %>% 
      select(-Species, -HAB_1) %>%
      mutate(value =(value)) %>%
      select(-ID) %>%
      pivot_wider(values_from = value,
                  names_from = SITE) %>%
      replace(is.na(.), 0) %>%
      column_to_rownames(var = "Year") %>%
      as.matrix() %>% as.data.frame()
    
    
    
    #rownames(x) = unique(v$Year)
    
    if(h == "R"){
      x = x %>% select(`1`, `2`, `3`, `5`,`6`,`7`,`8`,`9`,`10`,`12`,`13`,`14`,`15`,`16`,`20`,`24`,`27`,
                       `31`)
      
      
      x[1,which(colnames(x) %nin% as.character(rocky.98) == T)] = "NA"
      x[2, which(colnames(x) %nin% as.character(rocky.99) == T)] = "NA"
      
    } else {
      
      x = x %>% select(`1`, `2`, `4`, `5`,`6`,`11`,`12`,`13`,`17`,`18`,`19`,`21`,`22`,`23`,`25`,`26`,`28`,`29`,`30`,`32`)
      
      x[1,which(colnames(x) %nin% as.character(sandy.98) == T)] = "NA"
      x[2, which(colnames(x) %nin% as.character(sandy.99) == T)] = "NA"
      
    }
    
    # Run changepoint analysis ---------------
    output = e.divisive(as.data.frame(x), 
                        R = 1000, 
                        alpha = 2, 
                        min.size = 2,
                        sig.lvl = .05)
    
    # Format data -------------------------
    dat = data.frame(Year = unique(v$Year), 
                     color = output$cluster)
    v_mod = left_join(v,dat) 
    
    ## Poisson count data regression 
    po_v = v_mod %>% mutate(value_round = round(value*60, digits = 0)) %>% 
      filter(Year > 2000) %>%
      filter(Species == species[i], HAB_1 == h ) %>%
      mutate(Year = as.numeric(Year)) %>% 
      mutate(Year = scale(Year)[,1])
    
    try(M4 <- zeroinfl(value_round ~ (Year + SITE) | (Year) + SITE,
                       dist = 'negbin',
                       data = po_v))
    
    
    
    
    M4_sum = (M4 %>% summary())
    
    try(if(max(M4_sum$coefficients$count[1:2,4]) < .05){
      print(paste(species[i], h))
      print(M4_sum$coefficients$count[2,])
    }else{
      NULL
    })
    # Plots
    
    # Filtering out the data for this species habitat combo
    hab_species_data = v_mod %>%
      filter(Species == species[i], HAB_1 == h)
    # Create a list to put the above data into
    list_habitats[[h]] = hab_species_data
  }
  
  cpoint_dataframe = rbind(list_habitats[[1]],list_habitats[[2]]) ## LML
  
  # Creating the changepoint graphs--------------
  species_graph = rep(species_names, each = 2)[-15]
  length_graph = rep(c("< 100 mm", "> 100 mm"), 12)[-15]
  graph_dat = cpoint_dataframe %>% left_join(color_fixed)
  graph = graph_dat %>% 
    mutate(HAB_1 = replace(HAB_1, HAB_1 == "R", "Rock")) %>%
    mutate(HAB_1 = replace(HAB_1, HAB_1 == "RW", "Wood + Rock")) %>%
    mutate(HAB_1 = replace(HAB_1, HAB_1 == "S", "Fine Sediment")) %>%
    mutate(HAB_1 = replace(HAB_1, HAB_1 == "SW", "Wood + Fine Sediment")) %>%
    ggplot(aes(x = as.numeric(Year), 
               y = value,color = graph_dat$hex)) +
    theme_minimal() + 
    geom_jitter(color = graph_dat$hex, alpha = .5) +
    facet_wrap(~HAB_1) + 
    theme(axis.text.x = element_text(angle = 90),
          legend.position = "none") + 
    xlim(1998, 2023) +
    ylab(paste("CPUE (indv / hour)")) +
    xlab(paste(species_graph[i], " (",length_graph[i],") ")) +
    theme(text = element_text(size = 14)) 
  
  print(graph)
  
  
}




## Final Figure
cp_lines = read.csv("Data/hab_cp.csv") %>% filter(SP %in% c("CC","CS","PS","WS","SMB","MM")) %>% filter(WATER == "LML")

LML.CPUE.w.sec %>% mutate(ID = rownames(.)) %>%
  separate(ID, into = c("YEAR", "SITE_N"), sep = "_") %>%
  left_join(c.h) %>%
  mutate(Habitat = str_replace(Habitat, "SW", "S")) %>%
  mutate(Habitat = str_replace(Habitat, "RW", "R")) %>%
  filter(Habitat != "NA")%>%
  
  pivot_longer(BB_1:WS_2, names_to = "SPECIES", values_to = "CPUE") %>%
  group_by(YEAR, Habitat, SPECIES) %>%
  summarize(mean_CPUE = mean(CPUE)) %>%
  ungroup() %>%
  separate(SPECIES, into = c("SP", "AGE"), remove = F)  %>%
  group_by(SP, YEAR) %>%
  left_join(totals) %>% 
  arrange(SP, YEAR) %>%
  filter(Habitat != "NA") %>%
  mutate(percentage = mean_CPUE / n) %>%
  filter(SP %in% c("CC","CS","PS","WS","SMB","MM")) %>%
  ggplot(aes(x = as.numeric(YEAR), y = ((mean_CPUE)*60)+1, fill = interaction(Habitat, AGE), col = interaction(Habitat, AGE))) + 
  #ggplot(aes(x = as.numeric(YEAR), y = (mean_CPUE*60), fill = interaction(Habitat, AGE))) + 
  theme_classic() +
  theme(
    #panel.border = element_rect(color = "black", fill = NA),
    strip.background = element_blank(),
    
  ) +
  geom_area(position = "identity", alpha = .00001, size = 1)+ 
  guides(fill = guide_legend(override.aes = list(alpha = 1))) +
  scale_y_log10() + 
  facet_wrap(~SP, scales = "free_y") +
  scale_fill_manual(#values = pal_con[c(1,2,4,5,6,7,9,10)],
    values = pal_con[c(1,3,5,7)], 
    labels = c("R-Juvenile",
               #"RW-Juvenile",
               "S-Juvenile",
               #"SW-Juvenile",
               "R-Adult",
               #"RW-Adult",
               "S-Adult"#,
               #"SW-Adult"
    )) + 
  theme(axis.text.x = element_text(angle= 90, vjust = .5)) +
  labs(fill = "Habitat & Age") + 
  xlab("") + 
  ylab("CPUE (ind/min) + 1") +
  
  geom_vline(aes(xintercept = 2000), col = "black", linetype = 1, size = .5) + 
  geom_vline(aes(xintercept = YEAR, col = interaction(Habitat,AGE)),
             data = cp_lines, size = .5, linetype = 2) +
  scale_color_manual(guide = "none", 
                     values = pal_con[c(1,3,5,7)], 
                     
                     labels = c("R-Juvenile",
                                #"RW-Juvenile",
                                "S-Juvenile",
                                #"SW-Juvenile",
                                "R-Adult",
                                #"RW-Adult",
                                "S-Adult"#,
                                #"SW-Adult"
                     )) 



## FBL --------
FBL.CPUE.w.sec = read.csv("Data/FBL_CPUE.csv") %>% 
  column_to_rownames(var = "X")
vec = vector()
p.val = vector()
species = colnames(FBL.CPUE.w.sec)



change_points_list = list()
v = FBL.CPUE.w.sec %>% 
  mutate(y_s = rownames(FBL.CPUE.w.sec)) %>%
  pivot_longer(1:length(species),
               names_to = "Species") %>%
  separate(y_s, 
           into = c("Year", "SITE_N"), sep = "_") %>%
  left_join(habs) %>%
  unite("ID", 
        c(SITE_N,Species), 
        sep = "_", 
        remove = F) %>%
  # select(-SITE_N) %>%
  rename(HAB_1 = Habitat) %>%
  mutate(value = value * 60 * 60 ) %>%
  filter(Year != 2002)# %>%
# filter(Year > 2000) 
summary_graph_data = list()

v = v %>% 
  mutate(HAB_1 = str_replace(HAB_1, "SW", "S")) %>%
  mutate(HAB_1 = str_replace(HAB_1, "RW", "R")) %>%
  filter(HAB_1 != "NA")

#sandy.98 = c(2,4,5,12,13,11)
#rocky.98 = c(1,3,7,8,9,10)

sandy.99 = c(1,4,5)
rocky.99= c(3,2)
color_fixed = data.frame(hex = c("#707173","#56B4E9", "#D55E00","#009E73"), color = c(1:4))

## Fixed change point using multiple sites a year as multivariate --------- 
for(i in 1:length(FBL.CPUE.w.sec[1,])){
  list_habitats = list()
  for(h in c("S","SW","RW")){
    # Set up data frame
    x = v %>%
      select(-X) %>%
      #filter(Year > 1999) %>%
      #rename(HAB_1 = Habitat) %>%
      filter(Species == species[i], HAB_1 == h) %>%
      mutate(value = as.numeric(value)) %>% 
      select(-Species, -HAB_1) %>%
      mutate(value = log10(value+1)) %>%
      select(-ID) %>%
      pivot_wider(values_from = value,
                  names_from = SITE_N) %>%
      replace(is.na(.), 0) %>%
      column_to_rownames(var = "Year") %>%
      as.matrix() %>% as.data.frame() %>%
      select(-WATER)
    
    # Run changepoint analysis ---------------
    output = e.divisive(as.data.frame(x), 
                        R = 1000, 
                        alpha = 2, 
                        min.size = 2,
                        sig.lvl = .05)
    
    # Format data -------------------------
    dat = data.frame(Year = unique(v$Year), 
                     color = output$cluster)
    v_mod = left_join(v,dat)
    
    po_v = v_mod %>% mutate(value_round = round(value, digits = 0)) %>% 
      filter(Year > 2000) %>%
      filter(Species == species[i], HAB_1 == h ) %>%
      mutate(Year = as.numeric(Year)) %>% 
      mutate(Year = scale(Year)[,1])
    
    try(M4 <- zeroinfl(value_round ~ (Year ) | (Year) ,
                       dist = 'negbin',
                       data = po_v))
    
    M4_sum = (M4 %>% summary())
    
    try(if(max(M4_sum$coefficients$count[1:2,4]) < .05){
      print(paste(species[i], h))
      print(M4_sum)
      #print(M4_sum$coefficients$count[2,])
    }else{
      NULL
    })
    
    # Plots
    
    # Filtering out the data for this species habitat combo
    hab_species_data = v_mod %>%
      filter(Species == species[i], HAB_1 == h)
    # Create a list to put the above data into
    list_habitats[[h]] = hab_species_data
  }
  
  
  cpoint_dataframe = rbind(list_habitats[[1]],list_habitats[[2]], list_habitats[[3]]) ## FML
  
  # Creating the changepoint graphs--------------
  species_graph = rep(species_names, each = 2)
  length_graph = rep(c("< 100 mm", "> 100 mm"), 12)
  graph_dat = cpoint_dataframe %>% left_join(color_fixed)
  graph = graph_dat %>% 
    mutate(HAB_1 = replace(HAB_1, HAB_1 == "R", "Rock")) %>%
    mutate(HAB_1 = replace(HAB_1, HAB_1 == "RW", "Wood + Rock")) %>%
    mutate(HAB_1 = replace(HAB_1, HAB_1 == "S", "Fine Sediment")) %>%
    mutate(HAB_1 = replace(HAB_1, HAB_1 == "SW", "Wood + Fine Sediment")) %>%
    ggplot(aes(x = as.numeric(Year), 
               y = value,color = graph_dat$hex)) +
    theme_minimal() + 
    geom_point(color = graph_dat$hex, alpha = .5) +
    facet_wrap(~HAB_1) + 
    theme(axis.text.x = element_text(angle = 90, vjust = .5),
          legend.position = "none") + 
    xlim(1998, 2023) +
    ylab(paste("CPUE (indv / hour)")) +
    xlab(paste(species_graph[i], " (",length_graph[i],") ")) +
    theme(text = element_text(size = 14)) 
  
  print(graph)
  
  
}


## Final Plots ----------------------
cp_lines = read.csv("Data/hab_cp.csv") %>% 
  filter(SP %in% c("CC","CS","PS","WS","SMB","MM")) %>% 
  filter(WATER == "FBL") 


FBL.CPUE.w.sec %>% mutate(ID = rownames(.)) %>%
  separate(ID, into = c("YEAR", "SITE_N"), sep = "_") %>%
  left_join(habs) %>% 
  pivot_longer(CC_1:WS_2, names_to = "SPECIES", values_to = "CPUE") %>%
  group_by(YEAR, Habitat, SPECIES) %>%
  summarize(mean_CPUE = mean(CPUE)) %>%
  ungroup() %>%
  separate(SPECIES, into = c("SP", "AGE"), remove = F)  %>%
  group_by(SP, YEAR) %>%
  left_join(totals) %>% 
  arrange(SP, YEAR) %>%
  filter(Habitat != "NA") %>%
  mutate(percentage = mean_CPUE / n) %>%
  filter(SP %nin% c("LT","ST")) %>%
  ggplot(aes(x = as.numeric(YEAR), y = ((mean_CPUE)*60)+1,fill = interaction(Habitat, AGE),  col= interaction(Habitat, AGE))) + 
  theme_classic() +
  theme(
    #panel.border = element_rect(color = "black", fill = NA),
    strip.background = element_blank(),
    
  ) +
  geom_area(alpha = .00009, position = 'identity' , size = 1)+ scale_y_log10() + 
  guides(fill = guide_legend(override.aes = list(alpha = 1))) +
  
  facet_wrap(~SP, scales = "free_y") +
  scale_fill_manual(values = pal_con[1:8][c(-1, -5)], 
                    labels = c("RW-Juvenile","S-Juvenile",
                               "SW-Juvenile", "RW-Adult","S-Adult",
                               "SW-Adult")) + 
  theme(axis.text.x = element_text(angle= 90, vjust = .5)) +
  labs(fill = "Habitat & Age") + 
  xlab("") + 
  ylab("CPUE (ind/min) + 1") + 
  geom_vline(aes(xintercept = 2003)) + 
  geom_vline(aes(xintercept = (YEAR), color =  interaction(Habitat, AGE)), data = cp_lines, linetype = 2, size = .5)  +
  scale_color_manual(guide = "none", values =  pal_con[1:8][c(-1, -5)], 
                     labels = c("R-Juvenile",
                                "RW-Juvenile",
                                "S-Juvenile",
                                "SW-Juvenile",
                                "R-Adult",
                                "RW-Adult",
                                "S-Adult",
                                "SW-Adult"
                     )) 






