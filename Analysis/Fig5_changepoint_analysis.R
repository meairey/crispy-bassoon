library(ecp)
library(pscl)
library(wesanderson)
library(tidyverse)
`%nin%` = Negate(`%in%`)
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

pal_con = wes_palette("Zissou1", type ="continuous")
cat = wes_palette("Zissou1", type ="discrete")

labels = c("CC" = "creek chub", "CS" = "common shiner","MM" =  "central mudinnow","PS" = "pumpkinseed","SMB" = "smallmouth bass", "WS" = "white sucker")

pal_custom = c("#91bab6","#DCCB4E","#b5ea8c","#194b57","#E79805","#739559")


## Creating data for habitat assignments 

load("Data/ChangePoint_Data/LMLV.Rdata")

LML.habs = LML.v %>% select(Year, SITE, HAB_1) %>%
  unique()


## For little moose remove the woody designations
LML.v = LML.v %>% 
  mutate(HAB_1 = str_replace(HAB_1, "SW", "S")) %>%
  mutate(HAB_1 = str_replace(HAB_1, "RW", "R")) %>%
  filter(HAB_1 != "NA")


sandy.98 = c(2,4,5,12,13,11)
rocky.98 = c(1,3,7,8,9,10)

sandy.99 = c(1,4,5)
rocky.99= c(3,2)
color_fixed = data.frame(hex = c("#707173","#56B4E9", "#D55E00","#009E73"), color = c(1:4))


list_coef.R = list()
list_coef.S = list()
coef_dat = NA
## Fixed change point using multiple sites a year as multivariate --------- 
for(i in 1:length(LML.CPUE.w.sec[1,])){
  list_habitats = list()
  for(h in c("S","R")){
    # Set up data frame
    x = LML.v %>%
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
    dat = data.frame(Year = unique(LML.v$Year), 
                     color = output$cluster)
    v_mod = left_join(LML.v,dat) 
    
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
      coef.dat = c(M4_sum$coefficients$count[2,], colnames(LML.CPUE.w.sec[i]))
    }else{
      coef.dat = NA
    })
    
    if(h == "R"){
      list_coef.R[[i]] = coef.dat
    }else{
      list_coef.S[[i]]= coef.dat
    }
    
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
  
  #print(graph)
  
  
}

colnames(LML.CPUE.w.sec) 
coef_dat

coef.names = c("estimate", "stdError", "z_value", "p-value", "ID","HAB_1")

Rcoefs = list_coef.R %>% unlist() %>% na.omit %>% matrix(., ncol = 5, byrow = T) %>% 
  as.data.frame() %>% mutate(HAB_1 = "R")
Scoefs = list_coef.S %>% unlist() %>% na.omit %>% matrix(., ncol = 5, byrow = T) %>% 
  as.data.frame() %>% mutate(HAB_1 = "S")

colnames(Rcoefs) = coef.names
colnames(Scoefs) = coef.names

coefs.LML = rbind(Rcoefs, Scoefs) %>% as.data.frame()  %>% 
  mutate(WATER = "LML")%>% 
  select(WATER, ID, HAB_1, everything())
coefs.LML


## Final Figure
cp_lines = read.csv("Data/hab_cp.csv") %>% filter(SP %in% c("CC","CS","PS","WS","SMB","MM")) %>% filter(WATER == "LML")

  
LML.v %>%
  group_by(Year, HAB_1, Species) %>%
  summarize(mean_CPUE = mean(value)) %>%
  ungroup() %>%
  separate(Species, into = c("SP", "AGE"), remove = F)  %>%
  group_by(SP, Year) %>%
  arrange(SP, Year) %>%
  filter(HAB_1 != "NA") %>%
  filter(SP %in% c("CC","CS","PS","WS","SMB","MM")) %>%
  ggplot(aes(x = as.numeric(Year), y = ((mean_CPUE))+1, fill = interaction(HAB_1, AGE), col = interaction(HAB_1, AGE))) + 
  theme_classic() +
  theme(strip.background = element_blank()) +
  geom_area(position = "identity", alpha = .00001, size = 1)+ 
  guides(fill = guide_legend(override.aes = list(alpha = 1))) +
  #scale_y_log10() + 
  facet_wrap(~SP, scales = "free_y", labeller = labeller(SP = labels)) +
  scale_fill_manual(values = pal_custom[c(1,2,4,5)],
                    labels = c(expression("R-Juvenile"^"WS"), # 1 ## These numbers represent the values for the pallete
                               expression("S-Juvenile"^ "CC, CS, MM"), #3
                               "R-Adult", #5
                               expression("S-Adult"^ "CC, MM") #7 
                               )) + 
  theme(axis.text.x = element_text(angle= 90, vjust = .5)) +
  labs(fill = "Habitat & Age") + 
  xlab("") + 
  ylab("CPUE (ind/hour)") +
  geom_vline(aes(xintercept = 2000), col = "black", linetype = 1, size = .5) + 
  geom_vline(aes(xintercept = YEAR, col = interaction(Habitat,AGE)),
             data = cp_lines, size = .5, linetype = 2) +
  scale_color_manual(guide = "none", 
                     values =  pal_custom[c(1,2,4,5)],
                     
                     labels = c(expression("R-Juvenile"^"WS"), #1 ## These numbers represent the values for the pallete
                                "S-Juvenile", #2
                                "R-Adult", #3
                                "S-Adult" #5
                     ))


expression(R^Juvenile)


 ## FBL --------

species_names = c( "creek chub",  "lake trout", "central mudminnow",  "smallmouth bass", "brook trout","white sucker")



FBL.CPUE.w.sec = read.csv("Data/FBL_CPUE.csv") %>% 
  column_to_rownames(var = "X")
vec = vector()
p.val = vector()
species = colnames(FBL.CPUE.w.sec)

load("Data/ChangePoint_Data/FBL_v.RData")

change_points_list = list()

summary_graph_data = list()

color_fixed = data.frame(hex = c("#707173","#56B4E9", "#D55E00","#009E73"), color = c(1:4))
coef.dat = NA

list_coef.R = list()
list_coef.S = list()
list_coef.SW = list()
## Fixed change point using multiple sites a year as multivariate --------- 
for(i in 1:length(FBL.CPUE.w.sec[1,])){
  list_habitats = list()
  for(h in c("S","SW","RW")){
    # Set up data frame
    x = FBL_v %>% 
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
    
    # Run change  point analysis ---------------
    output = e.divisive(as.data.frame(x), 
                        R = 1000, 
                        alpha = 2, 
                        min.size = 2,
                        sig.lvl = .05)
    
    # Format data -------------------------
    dat = data.frame(Year = unique(FBL_v$Year), 
                     color = output$cluster)
    v_mod = left_join(FBL_v,dat)
    
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
      coef.dat = c(M4_sum$coefficients$count[2,], colnames(FBL.CPUE.w.sec[i]))
    }else{
      coef.dat = NA
    })
    
    
    if(h == "RW"){
      list_coef.R[[i]] = coef.dat
    } else if(h == "S"){
      list_coef.S[[i]]= coef.dat
    } else {
      list_coef.SW[[i]] = coef.dat
    }
    
    
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



coef.names = c("estimate", "stdError", "z_value", "p-value", "ID","HAB_1")

Rcoefs = list_coef.R %>% unlist() %>% na.omit() %>% matrix(., ncol = 5, byrow = T) %>% 
  as.data.frame() %>% mutate(HAB_1 = "RW")
Scoefs = list_coef.S %>% unlist() %>% na.omit() %>% matrix(., ncol = 5, byrow = T) %>% 
  as.data.frame() %>% mutate(HAB_1 = "S")
SWcoefs = list_coef.SW %>% unlist() %>% na.omit() %>% matrix(., ncol = 5, byrow = T) %>%
  as.data.frame() %>% mutate(HAB_1 = "SW")


colnames(Rcoefs) = coef.names
colnames(Scoefs) = coef.names
colnames(SWcoefs) = coef.names

coefs.FBL = rbind(Rcoefs, Scoefs, SWcoefs) %>% as.data.frame()  %>% 
  mutate(WATER = "FBL")%>% 
  select(WATER, ID, HAB_1, everything()) 
coefs.FBL



## Bind together the coefficient data frames

coefs = rbind(coefs.LML, coefs.FBL) %>%
  left_join(read.csv("Data/legend.csv"), by = c("ID" = "id")) %>%
  na.omit() %>%
  select(WATER, HAB_1, common, age, estimate, stdError, z_value, `p-value`) %>%
  mutate(z_value = round(as.numeric(z_value), digits = 3),
         `p-value` = round(as.numeric(`p-value`), digits = 3),
         stdError = round(as.numeric(stdError), digits = 3),
         estimate = round(as.numeric(estimate), digits = 3)) %>%
  mutate(`p-value` = replace(`p-value`, `p-value` <.001, "<.001")) %>%
  rename(Water = WATER,
         Habitat = HAB_1, 
         `Standard Error` = stdError,
         `Z Value` = z_value, 
         Common = common, 
         Age = age,
         B = estimate) 
coefs
write.csv(coefs, row.names = F, file = "Data/Coef.csv")
## Final Plots ----------------------
cp_lines = read.csv("Data/hab_cp.csv") %>% 
  filter(SP %in% c("CC","CS","PS","WS","SMB","MM")) %>% 
  filter(WATER == "FBL") 

habs = read.csv("Data/habs.csv") %>%
  select(-X)


FBL_v %>%
  group_by(Year, HAB_1, Species) %>%
  summarize(mean_CPUE = mean(value)) %>%
  ungroup() %>%
  separate(Species, into = c("SP", "AGE"), remove = F)  %>%
  group_by(SP, Year) %>%
  arrange(SP, Year) %>%
  filter(HAB_1 != "NA") %>%
  filter(SP %nin% c("LT","ST")) %>%
  ggplot(aes(x = as.numeric(Year), y = ((mean_CPUE))+1,fill = interaction(HAB_1, AGE),  col= interaction(HAB_1, AGE))) + 
  theme_classic() +
  theme(strip.background = element_blank()) +
  geom_area(alpha = .00009, position = 'identity' , size = 1) +
  guides(fill = guide_legend(override.aes = list(alpha = 1))) +
  facet_wrap(~SP, scales = "free_y", labeller = labeller(SP = labels)) +
  scale_fill_manual(values = pal_custom[1:6],
                    labels = c("RW-Juvenile",
                               "S-Juvenile",
                               "SW-Juvenile", 
                               "RW-Adult",
                               "S-Adult",
                               "SW-Adult")) + 
  theme(axis.text.x = element_text(angle= 90, vjust = .5)) +
  labs(fill = "Habitat & Age") + 
  xlab("") + 
  ylab("CPUE (ind/hour)") + 
  geom_vline(aes(xintercept = 2003)) + 
  geom_vline(aes(xintercept = (YEAR), color =  interaction(Habitat, AGE)), data = cp_lines, linetype = 2, size = .5)  +
  scale_color_manual(guide = "none", 
                     values = pal_custom,
                     labels = c("RW-Juvenile",
                                "S-Juvenile",
                                "SW-Juvenile", 
                                "RW-Adult",
                                "S-Adult",
                                "SW-Adult")) 









