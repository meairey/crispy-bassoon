

LML.habs = LML.v %>% select(Year, SITE, HAB_1) %>%
  unique()


## For little moose remove the woody designations
LML.v2 = LML.v %>% 
  select(-HAB_1) %>%
  rename(HAB_1 = new_hab) %>%
 mutate(HAB_1 = str_replace(HAB_1, "SW", "S")) %>%
 mutate(HAB_1 = str_replace(HAB_1, "RW", "R")) %>%
  filter(HAB_1 != "NA") %>%
  select(-same) 

LML.v2 %>% summarize(unique(HAB_1))
LML.v2 %>% filter(HAB_1 == "RS")
LML.v %>% summarize(unique(new_hab))
## List of habitat designations by site index in LML in 1998
sandy.98 = c(2,4,5,12,13,11)
rocky.98 = c(1,3,7,8,9,10)
## List of habitat designations by site index in LML 1999
sandy.99 = c(1,4,5)
rocky.99= c(3,2)
color_fixed = data.frame(hex = c("#707173","#56B4E9", "#D55E00","#009E73"), color = c(1:4))



species = c("CC_1", "CC_2", "CS_1", "CS_2", "MM_1", "MM_2", "PS_1", "PS_2", "SMB_1","SMB_2", "WS_1","WS_2")
list_coef.R = list()
list_coef.S = list()
coef.dat = NA

sites.to.keep.sand = (c.h.test %>% filter(!grepl("R",new_hab)))$SITE %>% 
  unique() %>% as.character()
sites.to.keep.rock = (c.h.test %>% filter(!grepl("S",new_hab)))$SITE %>% 
  unique() %>% as.character()
sites.to.keep.mixed = (c.h.test %>% filter(grepl("RS",new_hab)))$SITE %>% 
  unique() %>% as.character()


## Note that you need to manually create the file of CP lines by 
for(i in 1:length(species)){
  list_habitats = list()
  for(h in c("S","R")){
    # Set up data frame
    x = LML.v2 %>%
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
    
    ## This sets up the dataframes and deals with habitat assignments
    if(h == "R"){ ## if rock sites
      x = x %>% select(all_of(sites.to.keep.rock))
      x[1,which(colnames(x) %nin% as.character(rocky.98) == T)] = "NA"
      x[2, which(colnames(x) %nin% as.character(rocky.99) == T)] = "NA"
      
    } else { ## if sediment - based sites
      
      x = x %>% select(all_of(sites.to.keep.sand))
      x[1,which(colnames(x) %nin% as.character(sandy.98) == T)] = "NA"
      x[2, which(colnames(x) %nin% as.character(sandy.99) == T)] = "NA"}
      

    ### Run changepoint analysis ---------------
    output = e.divisive(as.data.frame(x), 
                        R = 10000, 
                        alpha = 1, 
                        min.size = 2,
                        sig.lvl = .05)
    print(output)
   
  }
  
   ### Format data 
    dat = data.frame(Year = unique(LML.v$Year), 
                     color = output$cluster)
    v_mod = left_join(LML.v,dat) 
    
    ## Poisson count data regression 
    po_v = v_mod %>% mutate(value_round = round(value*60, digits = 0)) %>% 
      filter(Year > 2000) %>%
      filter(Species == species[i], HAB_1 == h ) %>%
      mutate(Year = as.numeric(Year)) %>% 
      mutate(Year = scale(Year)[,1])
    
    M4 = try( zeroinfl(value_round ~ (Year + SITE) | (Year) + SITE, ## set up as try so it doesn't break the loop
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

  cpoint_dataframe = rbind(list_habitats[['R']],list_habitats[['S']]) ## LML
  
  ### Creating the changepoint graphs--------------
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
  

  
  
  
  

## Code trying to put the mixed habitat sites in this - but I'm thinking they'll have to be treated separately
} else {
      
       x = x %>% select(all_of(sites.to.keep.mixed))
      x[1,which(colnames(x) %nin% as.character(sandy.98) == T)] = "NA"
      x[2, which(colnames(x) %nin% as.character(sandy.99) == T)] = "NA"
    }
    
