library(vegan)
setwd("C:/users/monta/OneDrive - Airey Family/GitHub/crispy-bassoon")
CPUE.w.sec = read.csv("Data/CPUE_whole.csv") %>%
  select(-X) %>%
  column_to_rownames(var = "rownames")
habs = read.csv("Data/habs.csv")

CPUE.w.sec


pal <- wes_palette("Cavalcanti1", 5, "discrete")

pal_con = wes_palette("Zissou1", type ="continuous")

## Alpha diversity of rock habitats 


cpue_alphadiv = CPUE.w.sec %>%
  mutate(across(everything(), ~replace(., . >  0 , 1))) %>%
  mutate(alpha_div = rowSums(.)) %>% 
  rownames_to_column(var = "Site") %>% 
  separate(Site, into = c("Year", "SITE_N"), sep = "_") %>%
  separate(SITE_N, into = c("GEAR", "WATER", "Y", "SI"), remove = F) %>%
  select(-Y, -SI) %>%
  left_join(habs) %>%
  rename(HAB_1 = Habitat) %>%
  group_by(Year, HAB_1) %>%
  filter(WATER == "FBL" & Year > 2003 | WATER == "LML" & Year > 2000) %>%
  filter(HAB_1 != "NA") 

cpue_alphadiv %>%
  ggplot(aes(x = as.numeric(Year), 
             y = as.numeric(alpha_div), col = HAB_1)) +
  geom_jitter(alpha = .3) + 
  facet_wrap(~WATER) + 
  ylim(0,10) + 
  theme_minimal() +
  geom_smooth(method = "lm", se = F, lwd = 1.25) + 
  scale_color_manual(labels = c( "Rock","Wood + Rock","Fine Sediment",
                                 "Wood + Fine Sediment"), values = pal[1:4]) +
  labs(col = "Habitat") + 
  ylab("Alpha Diversity") +
  xlab("Year")


cpue_alphadiv %>%
  filter(Year > 2000 & HAB_1 != "NA") %>%
  filter(HAB_1 != "R") %>% ## Need for FBL since only in 2003 was R sampled
  ggplot(aes(x = as.numeric(Year), y = alpha_div, col = HAB_1)) +
  geom_jitter(alpha = .2) + 
  geom_smooth(method = lm, se = F) + 
  theme_minimal() + 
  ylab("Alpha Diversty") + xlab("Year") +labs(col = "Habitat") + 
  scale_color_manual(labels = c( "Wood + Rock","Fine Sediment","Wood + Fine Sediment*"), values = pal[1:4])  


## Temporal alpha diversity per site 


cpue_alphadiv %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(Year >= 2001 & WATER == "LML" | Year > 2003 & WATER == "FBL") %>%
  group_by(WATER, HAB_1) %>% 
  na.omit() %>%
  do({
    model <- lm(alpha_div ~ Year, data = .)
    data.frame(
      coef = tidy(model)$estimate[2],   # Coefficient for SMB_1
      r_squared = summary(model)$r.squared,  # R-squared value
      p_value = tidy(model)$p.value[2],
      p_value_intercept = tidy(model)$p.value[1]     
      
    )
  }) %>%
  filter(p_value < .05)


shannon = CPUE.w.sec %>% 
  mutate(diversity = diversity(., index = "shannon")) %>%
  rownames_to_column(var = "Site") %>% 
  separate(Site, into = c("Year", "SITE_N"), sep = "_") %>%
  #mutate(Site = as.numeric(Site)) %>%
  left_join(habs) %>%
  separate(SITE_N, into = c("GEAR", "WATER", "Y", "SI"), remove = F) %>%
  select(-Y, -SI) %>%
  rename(HAB_1 = Habitat) %>%
  group_by(WATER, Year, HAB_1) %>% 
  select(WATER, Year, SITE_N, HAB_1, diversity,  everything())

shannon %>%
  filter(Year > 1998, HAB_1 != "NA") %>%
  #filter(HAB_1 != "R") %>%
  ggplot(aes(x = as.numeric(Year),
             y = diversity,
             col = HAB_1),
         key_glyph = "rect") + 
  geom_jitter(alpha = .2) + 
  geom_smooth(method = lm, se = F) + 
  theme_minimal() + 
  ylab("Shannon Diversity Index") + 
  xlab("Year") + 
  labs(col = "Habitat") + 
  scale_color_manual(labels = c("Rock","Wood + Rock","Fine Sediment",
                                "Wood + Fine Sediment"), 
                     values = pal[1:4] ) +
  facet_wrap(~WATER)


shannon %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(Year >= 2001 & WATER == "LML" | Year > 2003 & WATER == "FBL") %>%
  group_by(WATER, HAB_1) %>% 
  na.omit() %>%
  do({
    model <- lm(diversity ~ Year, data = .)
    data.frame(
      coef = tidy(model)$estimate[2],   # Coefficient for SMB_1
      r_squared = summary(model)$r.squared,  # R-squared value
      p_value = tidy(model)$p.value[2],
      p_value_intercept = tidy(model)$p.value[1]     
      
    )
  }) %>%
  filter(p_value < .05)

### Ratios ----------------------------



## Proportion of SMB to all other native species through time 


ratios = CPUE.w.sec %>% 
  mutate(non_bass_sum = BB + CC + CS + LT + MM + PS + RS + SS + ST + WS) %>%   mutate(native_sum = BB + CC + CS + LT + PS + SS + ST + WS) %>% ## LML
  #mutate(non_bass_sum = CC + LT + ST + WS + MM + CS + SS + RS) %>% mutate(native_sum = CC + LT + ST + WS + RS) %>%
  mutate(ratio = non_bass_sum / SMB)  %>% 
  mutate(native_ratio = native_sum / SMB) %>%
  rownames_to_column(var = "Site") %>% 
  separate(Site, into = c("Year", "SITE_N"), sep = "_") %>%
  #mutate(Site = as.numeric(Site)) %>%
  left_join(habs) %>%
  separate(SITE_N, into = c("GEAR", "WATER", "Y", "SI"), remove = F) %>%
  select(-Y, -SI) %>%
  rename(HAB_1 = Habitat) %>%
  filter(ratio < 10000000 | native_ratio < 10000000) 

ratios %>% 
  mutate(Year = as.numeric(Year)) %>%
  filter(HAB_1 != "NA") %>%
  ggplot(aes( x = as.numeric(Year), y = log(native_ratio +1), col = HAB_1)) + 
  theme_minimal() + 
  geom_point() + 
  geom_smooth(method = 'lm', se = F) +
  scale_color_manual(labels = c("Rock", "Wood + Rock*","Fine Sediment*","Wood + Fine Sediment*"), values = pal[1:4] ) + 
  xlab("Year") + 
  ylab("log Ratio Native:Bass") + 
  labs(col = "Habitat") + 
  facet_wrap(~WATER)



ratios %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(Year >= 2001 & WATER == "LML" | Year > 2003 & WATER == "FBL") %>%
  group_by(WATER, HAB_1) %>% 
  na.omit() %>%
  do({
    model <- lm(native_ratio ~ Year, data = .)
    data.frame(
      coef = tidy(model)$estimate[2],   # Coefficient for SMB_1
      r_squared = summary(model)$r.squared,  # R-squared value
      p_value = tidy(model)$p.value[2],
      p_value_intercept = tidy(model)$p.value[1]     
      
    )
  }) %>%
  filter(p_value < .05)

