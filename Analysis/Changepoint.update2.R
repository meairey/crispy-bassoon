set.seed(123)
## Setup -------------
setwd("C:/Users/monta/OneDrive - Airey Family/GitHub/crispy-bassoon")
library(ecp)
library(pscl)
library(wesanderson)
library(tidyverse)
`%nin%` = Negate(`%in%`)
pal_custom = c("#91bab6","#DCCB4E","#b5ea8c","#194b57","#E79805","#739559")
## LML -------------------------------

### Data  ------

LML.CPUE.w.sec = read.csv("Data/LML_CPUE.csv") %>% 
  column_to_rownames(var = "X")

LML.CPUE.w.sec %>% 
  select(CC_1)

species_names = c("brown bullhead", "creek chub", "common shiner", "lake trout", "central mudminnow", "pumpkinseed", "rainbow smelt", "round whitefish", "smallmouth bass", "slimy sculpin","white sucker") ## This varies by lake, variable gets rewritten below

vec = vector() ## empty vector to fill

p.val = vector() ## empty vector to fill

#species = colnames(LML.CPUE.w.sec) ## included species from data frame ## DELETE LINE???

change_points_list = list() ## empty list to fill

pal_con = wes_palette("Zissou1", type ="continuous") ## pallete for plotting

cat = wes_palette("Zissou1", type ="discrete")


## Creating data for habitat assignments 

#load(file = "Data/LML.v.post2000.RData")
LML.habs = LML.v %>% select(Year, SITE, HAB_1) %>%
  unique()

## Loading in new habitat assignments 
updated_habs_LML=read.csv("Data/updated_habs_LML.csv") %>%
    #separate(SITE_N, into =c("BEF", "LML", "SITE")) %>%
  #mutate(SITE = as.character(as.numeric(SITE))) %>%
  select( -same, -same.new, -Habitat)
## For little moose remove the woody designations
LML.v2 = LML.v  %>% 
  mutate(Year = as.numeric(Year)) %>%
  filter(Year > 1999) %>%
      separate(ID, into = c("SITE", "Sp", "Age"), sep = "_",remove = F)  %>% 
  left_join(updated_habs_LML, by = c("SITE" = "SITE_N"))%>%
  mutate(HAB_1 = new_hab) %>%
  select(-new_hab) %>%
  ## Consolidated habitats - remove if you want to get rid of them
  mutate(HAB_1 = case_when(HAB_1 %in% c("S", "RS") ~ "S", 
                           HAB_1 %in% c("RSW", "SW") ~ "SW", 
                           HAB_1 == "RW" ~ "RW", 
                           HAB_1 == "R" ~ "R"))
  filter(SITE != "BEF.LML.001")


LML.v2 %>% summarize(unique(HAB_1))
LML.v2 %>% filter(HAB_1 == "RS")
LML.v %>% summarize(unique(HAB_1))

color_fixed = data.frame(hex = c("#707173","#56B4E9", "#D55E00","#009E73"), color = c(1:4))



species = c("CC_1", "CC_2", "CS_1", "CS_2", "MM_1", "MM_2", "PS_1", "PS_2", "SMB_1","SMB_2", "WS_1","WS_2")

species_names = c( "creek chub",  "common shiner", "central mudminnow",  "pumpkinseed","smallmouth bass", "white sucker")


list_coef.R = list()
list_coef.S = list()
list_coef.RS = list()
list_coef.RSW = list()
list_coef.SW = list()

coef.dat = NA

CP.frame.LML = data.frame(CP= NA, YEAR = NA) %>% 
    
      mutate(Species = NA, 
             Habitat = NA)

write.table(
      CP.frame.LML,
      file = "Data/CP_update_LML.csv",
      sep = ",",
      row.names = F
    )

## Note that you need to manually create the file of CP lines by 
for(i in 1:length(species)){
  list_habitats = list()
  #for(h in c("S","R", "RS", "RW", "RSW")){
  for(h in c("S", "SW", "R", "RW")){
    # Set up data frame
    x = LML.v2 %>%
      filter(Year > 1999)  %>%
      select(-Sp, -Age) %>%
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
    
  
    
    ### Run changepoint analysis ---------------
    output = e.divisive(as.data.frame(x), 
                        R = 10000, 
                        alpha = 1, 
                        min.size = 2,
                        sig.lvl = .05)
    
    CP.frame = data.frame(CP=output$cluster, year = rownames(x)) %>% 
      group_by(CP) %>% slice(1) %>%
      mutate(Species = species[i], 
             Habitat = h)
    
    write.table(
      CP.frame,
      file = "Data/CP_update_LML.csv",
      append = TRUE,
      col.names = F,
      sep = ",",
      row.names = F
    )
    
    ### Format data 
    dat = data.frame(Year = (unique(LML.v2$Year)), 
                     color = output$cluster)
    v_mod = left_join(LML.v2, dat) 
    
    ## Poisson count data regression 
    po_v = v_mod %>% 
      mutate(value_round = round(value*60, digits = 0)) %>% 
      filter(Year > 2000) %>%
      filter(Species == species[i], HAB_1 == h ) %>%
      mutate(Year = as.numeric(Year)) %>% 
      mutate(Year = scale(Year)[,1])
    

    
    M4 = try( zeroinfl(value_round ~ (Year + SITE) | 1 , ## set up as try so it doesn't break the loop
                        dist = 'negbin',
                       data = po_v))
    
    M4_sum = M4 %>% summary()
    
    
    try(if(max(M4_sum$coefficients$count[1:2,4]) < .05){
      print(paste(species[i], h))
      print(M4_sum$coefficients$count[2,])
      coef.dat = c(M4_sum$coefficients$count[2,], colnames(LML.CPUE.w.sec[i]))
    }else{
      coef.dat = NA
    })
    

    
    # Plots
    
    # Filtering out the data for this species habitat combo
    hab_species_data = v_mod %>%
      filter(Species == species[i], HAB_1 == h)
    # Create a list to put the above data into
    list_habitats[[h]] = hab_species_data
    }

  
    
  cpoint_dataframe = rbind(list_habitats[[1]],list_habitats[[2]], 
                           list_habitats[[3]], 
                           
                           list_habitats[[4]]#,
                           ## take these out for tests
                          # list_habitats[[5]]
                           ) ## FML
  
  ### Creating the changepoint graphs--------------
  species_graph = rep(species_names, each = 2)[-15]
  length_graph = rep(c("< 100 mm", "> 100 mm"), 12)[-15]
  graph_dat = cpoint_dataframe %>% left_join(color_fixed)
  graph = graph_dat %>% 
    #mutate(HAB_1 = replace(HAB_1, HAB_1 == "R", "Rock")) %>% ## remove for consolidated
    mutate(HAB_1 = replace(HAB_1, HAB_1 == "R", "Complex")) %>%
    #mutate(HAB_1 = replace(HAB_1, HAB_1 == "RW", "Wood + Rock")) %>% ## remove for consolidated
    mutate(HAB_1 = replace(HAB_1, HAB_1 == "RW", "Complex + woody")) %>%
    #mutate(HAB_1 = replace(HAB_1, HAB_1 == "S", "Fine Sediment")) %>% ## remove for consolidated
    mutate(HAB_1 = replace(HAB_1, HAB_1 == "S", "Low complexity")) %>%
    #mutate(HAB_1 = replace(HAB_1, HAB_1 == "SW", "Wood + Fine Sediment")) %>%
    mutate(HAB_1 = replace(HAB_1, HAB_1 == "SW", "Low complexity + woody")) %>%
   # mutate(HAB_1 = replace(HAB_1 = replace(HAB_1, HAB_1 == "RS", "Mixed Substrate"))) %>%
    ggplot(aes(x = as.numeric(Year), 
               y = value,color = graph_dat$hex)) +
    theme_minimal() + 
    geom_point(color = graph_dat$hex, alpha = .5) +
    facet_wrap(~HAB_1) + 
    theme(axis.text.x = element_text(angle = 90),
          legend.position = "none") + 
    xlim(1998, 2023) +
    ylab(paste("CPUE (indv / hour)")) +
    xlab(paste(species[i], " (",length_graph[i],") ")) +
    theme(text = element_text(size = 14)) 
  
  print(graph) ## Prints graphs to watch and create CP_lines csv
  
  
}


## Final Figure
cp_lines.LML = read.csv("Data/CP_update_LML.csv") %>% 
  na.omit() %>%
  separate(Species, into = c("SP", "AGE")) %>%
  group_by(SP, AGE, Habitat) %>%
  mutate(unique.sp = length(unique(CP))) %>%
  filter(unique.sp > 1, 
         YEAR > 2000) %>%
  ungroup() %>% 
  group_by(YEAR, SP)  %>%
  mutate(stagger = (row_number() - 1) * 0.3,
         YEAR = YEAR + stagger) %>%
  ungroup()  %>%
  rename(HAB_1 = Habitat) %>%
    #mutate(HAB_1 = replace(HAB_1, HAB_1 == "R", "Rock")) %>% ## remove for consolidated
    mutate(HAB_1 = replace(HAB_1, HAB_1 == "R", "Complex")) %>%
    #mutate(HAB_1 = replace(HAB_1, HAB_1 == "RW", "Wood + Rock")) %>% ## remove for consolidated
    mutate(HAB_1 = replace(HAB_1, HAB_1 == "RW", "Complex + woody")) %>%
    #mutate(HAB_1 = replace(HAB_1, HAB_1 == "S", "Fine Sediment")) %>% ## remove for consolidated
    mutate(HAB_1 = replace(HAB_1, HAB_1 == "S", "Low complexity")) %>%
    #mutate(HAB_1 = replace(HAB_1, HAB_1 == "SW", "Wood + Fine Sediment")) %>%
    mutate(HAB_1 = replace(HAB_1, HAB_1 == "SW", "Low complexity + woody"))  %>%
  mutate(interaction = interaction(HAB_1, AGE))
   
  
cp_lines.LML %>% 
  group_by(YEAR, SP) %>%
  summarize(tot = length(unique(Habitat)))


labels = c("CC" = "creek chub", "CS" = "common shiner","MM" =  "central mudminnow","PS" = "pumpkinseed","SMB" = "smallmouth bass", "WS" = "white sucker") 

LML.v2 %>%
   # mutate(HAB_1 = replace(HAB_1 = replace(HAB_1, HAB_1 == "RS", "Mixed Substrate"))) 
  group_by(Year, HAB_1, Species) %>%
  summarize(mean_CPUE = mean(value)) %>%
  ungroup() %>%
  separate(Species, into = c("SP", "AGE"), remove = F)  %>%
  group_by(SP, Year) %>%
  arrange(SP, Year) %>%
  filter(HAB_1 != "NA") %>%
  filter(SP %in% c("CC","CS","PS","WS","SMB","MM")) %>%
    #mutate(HAB_1 = replace(HAB_1, HAB_1 == "R", "Rock")) %>% ## remove for consolidated
    mutate(HAB_1 = replace(HAB_1, HAB_1 == "R", "Complex")) %>%
    #mutate(HAB_1 = replace(HAB_1, HAB_1 == "RW", "Wood + Rock")) %>% ## remove for consolidated
    mutate(HAB_1 = replace(HAB_1, HAB_1 == "RW", "Complex + woody")) %>%
    #mutate(HAB_1 = replace(HAB_1, HAB_1 == "S", "Fine Sediment")) %>% ## remove for consolidated
    mutate(HAB_1 = replace(HAB_1, HAB_1 == "S", "Low complexity")) %>%
    #mutate(HAB_1 = replace(HAB_1, HAB_1 == "SW", "Wood + Fine Sediment")) %>%
    mutate(HAB_1 = replace(HAB_1, HAB_1 == "SW", "Low complexity + woody")) %>%
  mutate(interaction = interaction(HAB_1, AGE)) %>%
  ungroup() %>%
 # reframe(unique(interaction))
  ggplot(aes(x = as.numeric(Year), y = ((mean_CPUE))+1, fill = interaction, col = interaction)) + 
  theme_classic() +
  theme(strip.background = element_blank()) +
  geom_area(position = "identity", alpha = .00001, size = 1)+ 
  guides(fill = guide_legend(override.aes = list(alpha = 1))) +
  #scale_y_log10() + 
  facet_wrap(~SP, scales = "free_y", labeller = labeller(SP = labels), ncol = 2) +
  
  theme(axis.text.x = element_text(angle= 90, vjust = .5),
        legend.position = "bottom", 
        legend.title = element_blank()) +
  xlab("") + 
  ylab("CPUE (ind/hour)") +
   
  geom_vline(aes(xintercept = YEAR, col = interaction),
             data = cp_lines.LML, size = 1.5, linetype = 2) +
  geom_vline(aes(xintercept = 2000), col = "black", linetype = 1, size = .5) +
  scale_color_manual(guide = "none", 
                     values =  wes_palette("Darjeeling1", type = "c", n = 8)) +
  scale_fill_manual(values =  wes_palette("Darjeeling1", type = "c", n = 8)) 



### Trying in FBL

## Note to self 2/23/26 - I'm close to getting the FBL to work I've reassigned habitat based on my substrate surveys. I think the next plan is to simplify it because i have like 5 new sub groups. I think maybe get back down to three in the habitat script 

### Data setup ---------
species_names = c( "creek chub",  "lake trout", "central mudminnow",  "smallmouth bass", "brook trout","white sucker")

FBL.CPUE.w.sec = read.csv("Data/FBL_CPUE.csv") %>% 
  column_to_rownames(var = "X")

vec = vector() ## empty vector rewrites above
p.val = vector() ## empty vector rewrites above
species = colnames(FBL.CPUE.w.sec)[c(-3,-4, -9,-10) ]
load("Data/ChangePoint_Data/FBL_v.RData")


updated.fbl.habs = read.csv("Data/updated_habs_testing.FBL.csv")
FBL_v2 = FBL_v %>% 
  filter(Year > 2003) %>%
  left_join(updated.fbl.habs) %>% 
  select(-HAB_1,  -Habitat, -same.new) %>% 
  rename(HAB_1 = new_hab) %>%
  ## Testing consolidating the new habitats
  mutate(HAB_1 = case_when(HAB_1 %in% c("S", "RS") ~ "S", 
                           HAB_1 %in% c("RSW", "SW") ~ "SW", 
                           HAB_1 == "RW" ~ "RW"))


### I think the 2003 sites have a different name but do correspond to the post 2003 sites? need to figure out if they can get assined modern equivalents

change_points_list = list()

summary_graph_data = list()

color_fixed = data.frame(hex = c("#707173","#56B4E9", "#D55E00","#009E73"), color = c(1:4))
coef.dat = NA

list_coef.R = list()
list_coef.S = list()
list_coef.SW = list()
list_coef.RS = list()
list_coef.RSW = list()


CP.frame.FBL = data.frame(CP= NA, YEAR = NA) %>% 
    mutate(Species = NA, 
             Habitat = NA)

write.table(
      CP.frame.FBL,
      file = "Data/CP_update_FBL.csv",
      sep = ",",
      row.names = F
    )


## Changepoint for loop 
for(i in 1:length(species)){
  list_habitats = list()
#  for(h in c("S","RW", "RS","SW", "RSW")){
  for(h in c("S", "SW", "RW")){
    ### Set up data frame
    x = FBL_v2 %>% 
      filter(Species == species[i], HAB_1 == h) %>%
      mutate(value = as.numeric(value)) %>% 
      select(-Species, -HAB_1) %>%
      mutate(value = log10(value+1)) %>%
      select(-ID) %>%
      pivot_wider(values_from = value,
                  names_from = SITE_N) %>%
      replace(is.na(.), 0) %>%
      column_to_rownames(var = "Year") %>%
      as.matrix() %>% as.data.frame()# %>%
     # select(-WATER)
    
    ### Run change  point analysis ---------------
    output = e.divisive(as.data.frame(x), 
                        R = 10000, 
                        alpha = 1, 
                        min.size = 2,
                        sig.lvl = .05)
    
      CP.frame.FBL = data.frame(CP=output$cluster, year = rownames(x)) %>% 
      group_by(CP) %>% slice(1) %>%
      mutate(Species = species[i], 
             Habitat = h)
    
    write.table(
      CP.frame.FBL,
      file = "Data/CP_update_FBL.csv",
      append = TRUE,
      col.names = F,
      sep = ",",
      row.names = F
    )
    
    ### Format data 
    dat = data.frame(Year =rownames(x), 
                     color = output$cluster)
    v_mod = left_join(FBL_v2,dat)
    
    po_v = v_mod %>% mutate(value_round = round(value, digits = 0)) %>% 
      filter(Year > 2003) %>%
      filter(Species == species[i], HAB_1 == h ) %>%
      mutate(Year = as.numeric(Year)) %>% 
      mutate(Year = scale(Year)[,1])
    
    M4 = try( zeroinfl(value_round ~ (Year ) | (Year) ,
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
    } else if(h == "SW"){
      list_coef.SW[[i]] = coef.dat
    } else if(h == "RS"){
      list_coef.RS[[i]] = coef.dat
    }  else {
      list_coef.RSW[[i]] = coef.dat
    }
    
    
    # Plots
    
    # Filtering out the data for this species habitat combo
    hab_species_data = v_mod %>%
      filter(Species == species[i], HAB_1 == h)
    # Create a list to put the above data into
    list_habitats[[h]] = hab_species_data
    }

  
  
  
  cpoint_dataframe = rbind(list_habitats[[1]],list_habitats[[2]], 
                           list_habitats[[3]]#, 
                           ## taken out for tests
                           #list_habitats[[4]],
                           #list_habitats[[5]]
                           ) ## FML
  
  ### Creating the changepoint graphs--------------
  species_graph = rep(species_names, each = 2)
  length_graph = rep(c("< 100 mm", "> 100 mm"), 12)
  graph_dat = cpoint_dataframe %>% left_join(color_fixed)
  graph = graph_dat %>% 
    mutate(HAB_1 = replace(HAB_1, HAB_1 == "R", "Rock")) %>%
    mutate(HAB_1 = replace(HAB_1, HAB_1 == "RW", "Wood + Rock")) %>%
    mutate(HAB_1 = replace(HAB_1, HAB_1 == "S", "Low complexity substrate")) %>%
    mutate(HAB_1 = replace(HAB_1, HAB_1 == "SW", "Wood + LCS")) %>%
    ggplot(aes(x = as.numeric(Year), 
               y = value,color = graph_dat$hex)) +
    theme_minimal() + 
    geom_point(color = graph_dat$hex, alpha = .5) +
    facet_wrap(~HAB_1) + 
    theme(axis.text.x = element_text(angle = 90, vjust = .5),
          legend.position = "none") + 
    xlim(1998, 2023) +
    ylab(paste("CPUE (indv / hour)")) +
    xlab(paste(species[i], " (",length_graph[i],") ")) +
    theme(text = element_text(size = 14)) 
  
  print(graph)
  
  
}

## Final Figure
cp_lines.FBL = read.csv("Data/CP_update_FBL.csv") %>% 
  na.omit() %>%
  separate(Species, into = c("SP", "AGE")) %>%
  group_by(SP, AGE, Habitat) %>%
  mutate(unique.sp = length(unique(CP))) %>%
  filter(unique.sp > 1, 
         YEAR > 2004) %>%
  ungroup() %>% 
  group_by(YEAR, SP)  %>%
  mutate(stagger = (row_number() - 1) * 0.3,
         YEAR = YEAR + stagger) %>%
  ungroup()  %>%
  rename(HAB_1 = Habitat) %>%
    #mutate(HAB_1 = replace(HAB_1, HAB_1 == "R", "Rock")) %>% ## remove for consolidated
    mutate(HAB_1 = replace(HAB_1, HAB_1 == "R", "Complex")) %>%
    #mutate(HAB_1 = replace(HAB_1, HAB_1 == "RW", "Wood + Rock")) %>% ## remove for consolidated
    mutate(HAB_1 = replace(HAB_1, HAB_1 == "RW", "Complex + woody")) %>%
    #mutate(HAB_1 = replace(HAB_1, HAB_1 == "S", "Fine Sediment")) %>% ## remove for consolidated
    mutate(HAB_1 = replace(HAB_1, HAB_1 == "S", "Low complexity")) %>%
    #mutate(HAB_1 = replace(HAB_1, HAB_1 == "SW", "Wood + Fine Sediment")) %>%
    mutate(HAB_1 = replace(HAB_1, HAB_1 == "SW", "Low complexity + woody"))  %>%
  mutate(interaction = interaction(HAB_1, AGE))
   

FBL_v2 %>%
   # mutate(HAB_1 = replace(HAB_1 = replace(HAB_1, HAB_1 == "RS", "Mixed Substrate"))) 
  group_by(Year, HAB_1, Species) %>%
  summarize(mean_CPUE = mean(value)) %>%
  ungroup() %>%
  separate(Species, into = c("SP", "AGE"), remove = F)  %>%
  group_by(SP, Year) %>%
  arrange(SP, Year) %>%
  filter(HAB_1 != "NA") %>%
  filter(SP %in% c("CC","CS","PS","WS","SMB","MM")) %>%
    #mutate(HAB_1 = replace(HAB_1, HAB_1 == "R", "Rock")) %>% ## remove for consolidated
    mutate(HAB_1 = replace(HAB_1, HAB_1 == "R", "Complex")) %>%
    #mutate(HAB_1 = replace(HAB_1, HAB_1 == "RW", "Wood + Rock")) %>% ## remove for consolidated
    mutate(HAB_1 = replace(HAB_1, HAB_1 == "RW", "Complex + woody")) %>%
    #mutate(HAB_1 = replace(HAB_1, HAB_1 == "S", "Fine Sediment")) %>% ## remove for consolidated
    mutate(HAB_1 = replace(HAB_1, HAB_1 == "S", "Low complexity")) %>%
    #mutate(HAB_1 = replace(HAB_1, HAB_1 == "SW", "Wood + Fine Sediment")) %>%
    mutate(HAB_1 = replace(HAB_1, HAB_1 == "SW", "Low complexity + woody")) %>%
  mutate(interaction = interaction(HAB_1, AGE)) %>%
  ungroup() %>%
 # reframe(unique(interaction))
  ggplot(aes(x = as.numeric(Year), y = ((mean_CPUE))+1, fill = interaction, col = interaction)) + 
  theme_classic() +
  theme(strip.background = element_blank()) +
  geom_area(position = "identity", alpha = .00001, size = 1)+ 
  guides(fill = guide_legend(override.aes = list(alpha = 1))) +
  #scale_y_log10() + 
  facet_wrap(~SP, scales = "free_y", labeller = labeller(SP = labels), ncol = 2) +
  
  theme(axis.text.x = element_text(angle= 90, vjust = .5),
        legend.position = "bottom", 
        legend.title = element_blank()) +
  xlab("") + 
  ylab("CPUE (ind/hour)") +
   
  geom_vline(aes(xintercept = YEAR, col = interaction),
             data = cp_lines.FBL, size = 1.5, linetype = 2) +
  geom_vline(aes(xintercept = 2003), col = "black", linetype = 1, size = .5) +
  scale_color_manual(guide = "none", 
                     values =  wes_palette("Darjeeling1", type = "c", n = 8)) +
  scale_fill_manual(values =  wes_palette("Darjeeling1", type = "c", n = 8)) 


library(lmerTest)
## Linear mixed effects model
lmer.testFBL = v_mod %>% mutate(value_round = round(value, digits = 0)) %>% 
      filter(Year > 2000) %>%
      filter(Species == "CC_1") %>%
      mutate(Year = as.numeric(Year)) %>% 
      mutate(Year = scale(Year)[,1])
    


cat = lmer(data = lmer.testFBL, value_round ~ Year*HAB_1 + (1|SITE_N))
summary(cat)

## Zero inflated model 

cat = zeroinfl(value_round ~ (Year*HAB_1 ) | 1 ,
                       dist = 'negbin',
                       data = lmer.testFBL)
summary(cat)
