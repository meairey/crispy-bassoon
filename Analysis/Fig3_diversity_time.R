## Script setup ------------------------
library(vegan)
library(MASS)
#library(lme4)
library(lmerTest)
library(tidyverse)
library(broom)
library(emmeans)
library(wesanderson)
library(patchwork)

setwd("C:/users/monta/OneDrive - Airey Family/GitHub/crispy-bassoon")

## Pull out the CPUE data - contains both FBL and LML
CPUE.w.sec = read.csv("Data/CPUE_whole.csv") %>%
  select(-X) %>%
  column_to_rownames(var = "rownames") %>%
  select(-NF,-RT,-LLS,-ST)


colnames(CPUE.w.sec)
## Habitat assignments for each site
habs = read.csv("Data/habs.csv")
habs = rbind(read.csv("Data/updated_habs_LML.csv") %>% select(SITE_N, new_hab, old_hab ), 
             read.csv("Data/updated_habs_testing.FBL.csv") %>% select(SITE_N, new_hab, old_hab)) %>%
  rename(Habitat = new_hab) %>% ## Change this based on which habitat labels you want to explore
  mutate(Habitat = case_when(Habitat %in% c("S", "RS") ~ "S", 
                           Habitat %in% c("RSW", "SW") ~ "SW", 
                           Habitat == "RW" ~ "RW", 
                           Habitat == "R" ~ "R"))

habs$Habitat == habs$new_hab

## Palettes for plotting 
pal = wes_palette("Cavalcanti1", 5, "discrete")

pal_con = wes_palette("Zissou1", type ="continuous")

# Labels for plotting
water_labels = c("FBL" = "First Bisby", "LML" = "Little Moose")
facet_data = data.frame(WATER = c("LML", "FBL"), 
                        YEAR = c(2000, 2003)) %>%
  mutate(WATER = factor(WATER, levels = c("LML", "FBL")))

new_hab_labels = c( "S" = "Low complexity",
                    "SW" = "LC + Wood",
                     "R" ="High complexity", 
                    "RW" ="HC + Wood" )


old_hab_labels = c("R" = "Rock", "RW" = "Rock + Wood",
                                "S" = "Sediment","SW" = "Sediment + Wood")

habitat_labels = new_hab_labels ## Change this depending on which habitat labels you want to explore

## Alpha diversity ---------------------


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
  filter(HAB_1 != "NA") %>%
  select(WATER, Year, SITE_N, HAB_1,alpha_div) %>%
  ungroup() %>%
  mutate(HAB_1 = as.factor(HAB_1)) %>%
  mutate(HAB_1 = relevel(HAB_1, ref = "SW"))





## Ratios ----------------------------

## Proportion of SMB to all other native species through time 
ratios = CPUE.w.sec %>% 
  mutate(non_bass_sum = BB + CC + CS + LT + MM + PS + RS + SS + WS) %>%
  mutate(native_sum = BB + CC + CS + LT + PS + SS  + WS) %>% 
  mutate(ratio = non_bass_sum / SMB)  %>% 
  mutate(native_ratio = native_sum / SMB) %>%
  rownames_to_column(var = "Site") %>% 
  separate(Site, into = c("Year", "SITE_N"), sep = "_") %>%
  #mutate(Site = as.numeric(Site)) %>%
  left_join(habs) %>%
  separate(SITE_N, into = c("GEAR", "WATER", "Y", "SI"), remove = F) %>%
  select(-Y, -SI) %>%
  rename(HAB_1 = Habitat) %>%
  filter(ratio < 10000000 | native_ratio < 10000000) %>%
  filter(WATER == "FBL" & Year > 2003 | WATER == "LML" & Year > 2000) %>%
 
  mutate(native_ratio = (native_ratio+1)) %>%
  select(WATER, Year, SITE_N, HAB_1,native_ratio) %>%

  mutate(WATER = factor(WATER, levels = c( "LML","FBL"))) %>% 
  mutate(Year = as.numeric(Year)) %>%
  filter(HAB_1 != "NA") %>%
  mutate(HAB_1 = factor(HAB_1, levels = c("S", "SW", "R", "RW"))) %>%
  ## Remove site 1 from all comparisons
  filter(SITE_N %nin% c("BEF.LML.001", "BEF.FBL.001"))




## Are data normally distributed

which.water = "FBL"

ratios %>%
  filter(WATER == "FBL") %>%
  ggplot(aes(x = log(native_ratio), fill = HAB_1))+
  geom_histogram() + 
  facet_wrap(~HAB_1, scales = "free") 


norm_test = ratios %>%
  group_by(WATER) %>%
  mutate(Year = as.numeric(scale(as.numeric(Year))), 
         native_ratio = log(native_ratio)) %>%
  mutate(HAB_1 = as.factor(HAB_1)) %>% 
  mutate(HAB_1 = relevel(HAB_1, ref = "SW"))


models.ratios = norm_test %>%
  group_by(WATER) %>%
  nest() %>%
  mutate(
    model = map(data, ~ lmer(
      native_ratio ~ Year*HAB_1 + (1 | SITE_N),
      data = .x
    ))
  )

## Table of model coefficients for both lakes
ratios.coefficients = rbind(summary(models.ratios$model[[1]])$coefficients %>% 
                             as.data.frame() %>% 
                               mutate(WATER = "LML"),
                             summary(models.ratios$model[[2]])$coefficients %>% 
                             as.data.frame() %>% 
                               mutate(WATER = "FBL"))  %>% 
  mutate(`Pr(>|t|)` = case_when(`Pr(>|t|)` < .001 ~ .001, 
                                `Pr(>|t|)` < .01 ~ .01,
                                `Pr(>|t|)` > .01 ~ round(`Pr(>|t|)`, digits = 3))) %>%
  mutate(sig = case_when(`Pr(>|t|)` <= .001 ~ "***",
                         `Pr(>|t|)` <= .01 ~ "**",
                         `Pr(>|t|)` <= .05 ~ "*",
                         `Pr(>|t|)` > .05 ~ ""))
## Are slopes different from 0
ratios.trends = rbind(emtrends(models.ratios$model[[1]], ~HAB_1, var = "Year", delta.var = 1) %>% 
                 as.data.frame() %>% 
                 mutate(WATER = "LML"),
               emtrends(models.ratios$model[[2]], ~HAB_1, var = "Year") %>%
                 as.data.frame() %>%
                 mutate(WATER = "FBL")) %>%
  rename(estimate = Year.trend) %>% 
  mutate(across(estimate:upper.CL, ~round(.x, digits = 3)),
         name = "ratio.trends")

ratios.slopes = ratios.trends %>%
  mutate(percent_change = round(((Year.trend)) * 100, digits = 2), 
         lower.CL = round(((lower.CL) )* 100, digits = 2), 
         upper.CL = round(((upper.CL))* 100, digits = 2), 
         SE = round(SE, digits = 2)) %>%
  select(-df) %>%
  mutate(cred = paste("[", lower.CL, ", ", upper.CL, "]", sep = ""))



#write.csv(ratios.slopes,"Figures_Tables/TemporalDiversityData/slopes_ratios.csv", row.names = F)


line_encoding.ratios = ratios.slopes %>%
  mutate(sig = sign(lower.CL) == sign(upper.CL)) %>%
  select(WATER, HAB_1, sig)

## differences in slopes between waters
pairs(emtrends(models.ratios$model[[1]], ~HAB_1, var = "Year", delta.var = 1), adjust = "tukey")
pairs(emtrends(models.ratios$model[[2]], ~HAB_1, var = "Year", delta.var = 1), adjust = "tukey")

## Intercepts for each  model
ratios.emmeans = rbind(emmeans(models.ratios$model[[1]], pairwise ~ HAB_1)$emmeans %>% 
                                    as.data.frame() %>% 
                                    mutate(WATER = "LML"),
                                  emmeans(models.ratios$model[[2]], pairwise ~ HAB_1)$emmeans %>% 
                                    as.data.frame() %>% 
                                    mutate(WATER = "FBL")) %>%
  rename(estimate = emmean) %>% 
  mutate(across(estimate:upper.CL, ~round(.x, digits = 3)),
         name = "ratio.emmean")


## Are intercepts different between habitats
ratios.contrasts.emmeans = rbind(emmeans(models.ratios$model[[1]], pairwise ~ HAB_1)$contrasts %>% 
                                    as.data.frame() %>% 
                                    mutate(WATER = "LML"),
                                  emmeans(models.ratios$model[[2]], pairwise ~ HAB_1)$contrasts %>% 
                                    as.data.frame() %>% 
                                    mutate(WATER = "FBL"))   %>% 
  mutate(across(estimate:p.value, ~round(.x, digits = 3)), 
         name = "ratios.emmean.contrast") 

ratios.trends.contrasts = rbind(pairs(emtrends(models.ratios$model[[1]], ~HAB_1, var = "Year", delta.var = 1)) %>% 
                 as.data.frame() %>% 
                 mutate(WATER = "LML") %>% 
                   as.data.frame(),
               pairs(emtrends(models.ratios$model[[2]], ~HAB_1, var = "Year")) %>%
                 as.data.frame() %>%
                 mutate(WATER = "FBL") %>% 
                   as.data.frame()) %>% 
  mutate(across(estimate:p.value, ~round(.x, digits = 3)), 
         name = "ratios.trends.contrast")

## Use log model only if doing Gaussian model family 
#library(lmerTest)

plot(models.ratios$model[[1]])       # Residuals vs. fitted
qqnorm(resid(models.ratios$model[[1]])); qqline(resid(models.ratios$model[[1]]))  # Q-Q plot

AIC((models.ratios$model[[1]]))



## Graph ratios
ratios_graphs = ratios   %>%
  left_join(line_encoding.ratios)%>% 
               mutate(WATER = factor(WATER, levels = c("LML", "FBL"))) %>%
  ggplot(aes( x = as.numeric(Year), y = native_ratio, col = HAB_1)) + 
  theme_minimal(base_size = 12) + 
  geom_point(alpha = .1) + 
  geom_smooth(aes(linetype = sig),method = 'lm', se = F) +
  geom_vline(data = facet_data %>% 
               mutate(WATER = factor(WATER, levels = c("LML", "FBL"))), aes(xintercept =YEAR), linetype = 2) +
  scale_color_manual(labels = habitat_labels,
                     values = pal[1:4] ) + 
  scale_linetype_manual("Significant", labels = c("Sig" = "TRUE", "Insig" = "FALSE"), 
                        values = c("TRUE" = 1, "FALSE" = 2)) +
  xlab("Year") + 
  ylab("log(Ratio Native:Bass)") + 
  labs(col = "Habitat") + 
  facet_wrap(~WATER, labeller = labeller(WATER = c("LML"="Little Moose", "FBL" = "First Bisby"))) +
  scale_y_log10() + 
  guides(linetype = "none") + 
  theme(axis.text.x = element_text(angle = 45)) 








## Double checking shannon again 

shannon = CPUE.w.sec %>%
  mutate(diversity = diversity(., index = "shannon")) %>%
  rownames_to_column(var = "Site") %>% 
  separate(Site, into = c("Year", "SITE_N"), sep = "_") %>%
  separate(SITE_N, into = c("Gear", "WATER", "site"), remove = F) %>%
  na.omit() %>%
  filter(WATER == "FBL" & Year > 2003 | WATER == "LML" & Year > 2000)  %>% 
  select(-site, -Gear) %>%
  left_join(habs) %>%
  ungroup()%>%
  mutate(Habitat = as.factor(Habitat)) %>%
  mutate(WATER = factor(WATER, levels = c( "LML","FBL")),
         Year = as.numeric(Year))  %>%
  # mutate(HAB_1 = relevel(HAB_1, ref = "SW")) %>% ## Only scale for the normality test below. Not for plotting/visualizing
 # mutate(Year = scale(as.numeric(Year), center = TRUE, scale = FALSE)) %>%
  select(WATER, Year, SITE_N, Habitat, diversity) %>%
  mutate(Habitat = factor(Habitat, levels = c("S", "SW", "R", "RW"))) %>%
  filter(SITE_N %nin% c("BEF.LML.001", "BEF.FBL.001"))


## mmodels for shannon diversity 


models.shannon = shannon %>%
  group_by(WATER) %>%
  nest() %>%
  mutate(
    model = map(data, ~ lmer(
      diversity ~ Year*Habitat + (1 | SITE_N),
      data = .x
    ))
  )



shannon.coefficients = rbind(summary(models.shannon$model[[1]])$coefficients %>% 
                             as.data.frame() %>% 
                               mutate(WATER = "LML"),
                             summary(models.shannon$model[[2]])$coefficients %>% 
                             as.data.frame() %>% 
                               mutate(WATER = "FBL"))  %>% 
  mutate(`Pr(>|t|)` = case_when(`Pr(>|t|)` < .001 ~ .001, 
                                `Pr(>|t|)` < .01 ~ .01,
                                `Pr(>|t|)` > .01 ~ round(`Pr(>|t|)`, digits = 3))) %>%
  mutate(sig = case_when(`Pr(>|t|)` <= .001 ~ "***",
                         `Pr(>|t|)` <= .01 ~ "**",
                         `Pr(>|t|)` <= .05 ~ "*",
                         `Pr(>|t|)` > .05 ~ ""))

shannon.trends = rbind(emtrends(models.shannon$model[[1]], ~Habitat, var = "Year") %>% 
                 as.data.frame() %>% 
                 mutate(WATER = "LML"),
               emtrends(models.shannon$model[[2]], ~Habitat, var = "Year") %>%
                 as.data.frame() %>%
                 mutate(WATER = "FBL")) %>% 
  rename(estimate = Year.trend) %>% 
  mutate(across(estimate:upper.CL, ~round(.x, digits = 3)), 
         name = "shannon.trends")

shannon.trends.contrasts = rbind(pairs(emtrends(models.shannon$model[[1]],
                                                ~ Habitat,
                                                var = "Year",
                                                delta.var = 1), 
                                       adjust = "tukey") %>% 
                                   as.data.frame() %>%
                                   mutate(WATER = "LML"),
                                 pairs(emtrends(models.shannon$model[[2]],
                                                ~ Habitat,
                                                var = "Year",
                                                delta.var = 1), 
                                       adjust = "tukey") %>% 
                                   as.data.frame()%>%
                                   mutate(WATER = "FBL")) %>% 
  mutate(across(estimate:p.value, ~round(.x, digits =3)))  %>% 
  mutate(name = "shannon.trends.contrast")

shannon.emmeans = rbind(emmeans(models.shannon$model[[1]], pairwise ~ Habitat)$emmeans %>% 
                                    as.data.frame() %>% 
                                    mutate(WATER = "LML"),
                                  emmeans(models.shannon$model[[2]], pairwise ~ Habitat)$emmeans %>% 
                                    as.data.frame() %>% 
                                    mutate(WATER = "FBL")
                                  ) %>% 
  rename(estimate = emmean) %>% 
  mutate(across(estimate:upper.CL, ~round(.x, digits = 3)),
         name = "shannon.emmeans")


shannon.contrasts.emmeans = rbind(emmeans(models.shannon$model[[1]], pairwise ~ Habitat)$contrasts %>% 
                                    as.data.frame() %>% 
                                    mutate(WATER = "LML"),
                                  emmeans(models.shannon$model[[2]], pairwise ~ Habitat)$contrasts %>% 
                                    as.data.frame() %>% 
                                    mutate(WATER = "FBL")
                                  ) %>% 
  mutate(across(estimate:p.value, ~round(.x, digits =3))) %>%
  mutate(name = "shannon.emmeans.contrast")

## Contrasts table 



slopes = shannon.trends %>%
  mutate(percent_change = round(((Year.trend)) * 100, digits = 2), 
         lower.CL = round(((lower.CL) )* 100, digits = 2), 
         upper.CL = round(((upper.CL))* 100, digits = 2), 
         SE = round(SE, digits = 2)) %>%
  select(-df) %>%
  mutate(cred = paste("[", lower.CL, ", ", upper.CL, "]", sep = ""))

#write.csv(slopes, "Figures_Tables/TemporalDiversityData/slopes_ratios_LML.csv")


line_encoding.shannon = shannon.trends %>%
  mutate(sig = sign(lower.CL) == sign(upper.CL)) %>%
  select(WATER, Habitat, sig)
  


emmeans(log_model, pairwise ~ HAB_1)



## graphing




shannon_graph = shannon %>%
  left_join(line_encoding.shannon) %>%
  mutate(WATER = factor(WATER, levels = c("LML", "FBL"))) %>%
  ggplot(aes(x = (Year),
             y = diversity,
             col = Habitat)) + 
  geom_jitter(alpha = .1) + 
  geom_vline(data = facet_data %>%
               mutate(WATER = factor(WATER, levels = c("LML", "FBL"))), aes(xintercept =YEAR), linetype = 2) +
  geom_smooth(aes(lty = sig), method = lm, se = F) + 
  theme_minimal(base_size = 12) + 
  ylab("Shannon Diversity") + 
  xlab("Year") + 
  labs(col = "Habitat") + 
  scale_color_manual(labels = habitat_labels ,
                     values = pal[1:4] ) +
  scale_linetype_manual("Significant", labels = c("Sig" = "TRUE", "Insig" = "FALSE"), 
                        values = c("TRUE" = 1, "FALSE" = 2)) +
  facet_wrap(~WATER, labeller = labeller(WATER = water_labels)) +
  guides(linetype = "none")


### Figure 3 combination of both graphs --------------------


shannon_graph / ratios_graphs 

ggsave("Figures_Tables/Figure2_TemporalDiversity1.jpeg", width = 6, height = 6, dpi = 600)


### Supplemental Graphs 


contrasts = rbind(shannon.trends.contrasts, shannon.contrasts.emmeans) %>% ## shannon comparisons
  ## ratio comparisons
  rbind(ratios.contrasts.emmeans) %>%
  rbind(ratios.trends.contrasts) %>% 
   mutate(p.value = case_when(p.value < .001 ~ "< 0.001", 
                             p.value >= .001 ~ as.character(p.value)))
  
write.csv(contrasts, 
          file = "Figures_Tables/TemporalDiversityData/contrasts.csv", 
          row.names = F)

estimates = rbind(shannon.trends, shannon.emmeans) %>% 
  ## ratio comparisons
   rbind(ratios.trends %>% 
          rename(Habitat = HAB_1)) %>%
  rbind(ratios.emmeans%>% 
          rename(Habitat = HAB_1)) %>% 
   mutate(p.value = case_when(p.value < .001 ~ "< 0.001", 
                             p.value >= .001 ~ as.character(p.value)))
write.csv(estimates, 
          file = "Figures_Tables/TemporalDiversityData/estimates.csv", 
          row.names = F)

## ------------------------Old Code--------------------------------------------

## Size at age

joined = full_join(cpue_alphadiv, shannon) %>%
  full_join(ratios %>%
              mutate(native_ratio = log(native_ratio))) %>%
  select(sig, everything()) %>%
  mutate(sig = case_when(sig == "sig" ~ sig, is.na(sig) ~ "non")) %>%
  pivot_longer(c(alpha_div:native_ratio), names_to = "metric", values_to = "metric.value") %>%
  mutate(interaction = interaction(metric, WATER))

facet.order = c("alpha_div.LML", "alpha_div.FBL", "diversity.LML", "diversity.FBL", 
                "native_ratio.LML", "native_ratio.FBL")
joined %>%
  left_join(facet_data, by = "WATER") %>%
  mutate(interaction = factor(interaction, levels = facet.order)) %>%
  ggplot(aes(x = as.numeric(Year), y = metric.value, col = HAB_1)) + 
  theme_minimal(base_size = 14) + 
  facet_wrap(~interaction , scales = "free_y", ncol = 2, 
             labeller = labeller(interaction = c(  "alpha_div.LML"= "Little Moose Lake", 
                                                   "alpha_div.FBL" = "First Bisby Lake"))) +

  scale_color_manual("Habitat", labels = c("Rock", "Wood + Rock",
                                "Fine Sediment","Wood + Fine Sediment"),
                     values = pal[1:4] )+
  
  xlab("Year") +  
  
  geom_jitter(alpha = .1) +
  geom_smooth(aes(linetype = sig),method = 'lm', se = F, lwd =1.25) +
  geom_vline( aes(xintercept =YEAR), linetype = 2) +
  theme(axis.title.y = element_blank(),
        legend.position = "bottom") +
  scale_linetype_manual(values = c(4,1)) + 
  guides(linetype = "none")




## Temporal alpha diversity per site 

cpue_alphadiv.dat = cpue_alphadiv %>%
  ungroup() %>%
  mutate(Year = as.vector(scale(as.numeric(Year), center = TRUE, scale = FALSE))) %>% 
  filter(WATER == "FBL") ## Change this per lake


pois_model = glmer(alpha_div ~ as.numeric(Year) * HAB_1 +  (1 | SITE_N), family = poisson, data = cpue_alphadiv.dat )
AIC(pois_model)
sum_alpha = summary(pois_model)

sum_alpha = sum_alpha$coefficients %>% as.data.frame() %>%
  mutate(`Pr(>|z|)` = case_when(`Pr(>|z|)` < .001 ~ .001, 
                                `Pr(>|z|)` < .01 ~ .01,
                                `Pr(>|z|)` > .01 ~ round(`Pr(>|z|)`, digits = 3))) %>%
  mutate(sig = case_when(`Pr(>|z|)` <= .001 ~ "***",
                         `Pr(>|z|)` <= .01 ~ "**",
                         `Pr(>|z|)` <= .05 ~ "*",
                         `Pr(>|z|)` > .05 ~ ""))

## It's annoying but easiest to just write these twice and load in to join later
# write.csv(sum_alpha, "Figures_Tables/TemporalDiversityData/AlphaDiv_summary_FBL.csv")
# write.csv(sum_alpha, "Figures_Tables/TemporalDiversityData/AlphaDiv_summary_LML.csv")

slopes = emtrends(pois_model, ~ HAB_1, var = "Year", delta.var = 1)
pairs(slopes, adjust = "tukey")
slopes = summary(slopes) %>%
  mutate(percent_change = round((exp(Year.trend) - 1) * 100, digits = 2), 
         asymp.LCL = round((exp(asymp.LCL) - 1)* 100, digits = 2), 
         asymp.UCL = round((exp(asymp.UCL) - 1)* 100, digits = 2), 
         SE = round(SE, digits = 2)) %>%
  select(-df) %>%
  mutate(cred = paste("[", asymp.LCL, ", ", asymp.UCL, "]", sep = ""))

# write.csv(slopes, file = "Figures_Tables/TemporalDiversityData/AlphaDiv_slopes_LML.csv", row.names = F)
# write.csv(slopes, file = "Figures_Tables/TemporalDiversityData/AlphaDiv_slopes_FBL.csv", row.names = F)

intercepts = emmeans(pois_model, pairwise ~ HAB_1)


pairs(slopes)
slopes
intercepts


line_encoding.alpha = rbind(read.csv(file = "Figures_Tables/TemporalDiversityData/AlphaDiv_slopes_LML.csv") %>%
                        mutate(WATER = "LML"),
                      read.csv(file = "Figures_Tables/TemporalDiversityData/AlphaDiv_slopes_FBL.csv") %>% 
                        mutate(WATER = "FBL")) %>%

  mutate(sig = sign(asymp.LCL) == sign(asymp.UCL)) %>%
  select(WATER, HAB_1, sig)

##
alpha_graph = cpue_alphadiv %>%
  left_join(line_encoding) %>%
  mutate(WATER = factor(WATER, levels = c("LML", "FBL"))) %>%
  ggplot(aes(x = as.numeric(Year), 
             y = as.numeric(alpha_div), col = HAB_1)) +
  geom_jitter(alpha = .2) + 
  facet_wrap(~WATER,
             labeller = labeller(WATER = c("LML" = "Little Moose", 
                                                   "FBL" = "First Bisby"))) + 
  ylim(0,10) + 
  theme_minimal(base_size = 12) +
  geom_vline(data = facet_data, aes(xintercept =YEAR), linetype = 2) +
  geom_smooth(method = "lm", se = F, lwd = 1.25, aes(lty = sig)) + 
  scale_color_manual(labels = c( "Rock","Wood + Rock","Fine Sediment",
                                 "Wood + Fine Sediment"), values = pal[1:4]) +
  scale_linetype_manual("Significance", 
                        values = c("FALSE" = 2, "TRUE" = 1),
                        labels = c("FALSE" = "Insignificant", 
                                   "TRUE" = "Significant")) +
  labs(col = "Habitat") + 
  ylab("Alpha Diversity") +
  xlab("Year") + 
  theme(axis.text.x = element_text(angle = 45)) + 
  guides(lty = "none")

alpha_graph
