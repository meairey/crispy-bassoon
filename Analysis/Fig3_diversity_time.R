library(vegan)
library(MASS)
library(lme4)
library(tidyverse)

library(broom)
library(wesanderson)
setwd("C:/users/monta/OneDrive - Airey Family/GitHub/crispy-bassoon")
CPUE.w.sec = read.csv("Data/CPUE_whole.csv") %>%
  select(-X) %>%
  column_to_rownames(var = "rownames")
habs = read.csv("Data/habs.csv")

CPUE.w.sec


pal <- wes_palette("Cavalcanti1", 5, "discrete")

pal_con = wes_palette("Zissou1", type ="continuous")
water_labels = c("FBL" = "First Bisby", "LML" = "Little Moose")
facet_data = data.frame(WATER = c("FBL", "LML"), 
                        YEAR = c(2003, 2000))

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
  filter(HAB_1 != "NA") %>%
  select(WATER, Year, SITE_N, HAB_1,alpha_div) %>%
  mutate(sig = case_when(HAB_1 == "RW" & WATER == "FBL" ~ "sig",
                         HAB_1 == "S" & WATER == "FBL" ~ "sig",
                         HAB_1 == "SW" & WATER == "FBL" ~ "sig",
                         HAB_1 == "SW" & WATER == "LML" ~ "sig",
                         HAB_1 == "RW" & WATER == "LML" ~ "sig",
                         HAB_1 == "R" & WATER == "LML" ~ "sig")) %>%
  ungroup() %>%
  mutate(HAB_1 = as.factor(HAB_1)) %>%
  mutate(HAB_1 = relevel(HAB_1, ref = "SW"))

cpue_alphadiv %>%
  ggplot(aes(x = as.numeric(Year), 
             y = as.numeric(alpha_div), col = HAB_1)) +
  geom_jitter(alpha = .2) + 
  facet_wrap(~WATER, labeller = labeller(WATER = water_labels))+ 
  ylim(0,10) + 
  theme_minimal(base_size = 14) +
  geom_vline(data = facet_data, aes(xintercept =YEAR), linetype = 2) +
  geom_smooth(method = "lm", se = F, lwd = 1.25) + 
  scale_color_manual(labels = c( "Rock","Wood + Rock","Fine Sediment",
                                 "Wood + Fine Sediment"), values = pal[1:4]) +
  labs(col = "Habitat") + 
  ylab("Alpha Diversity") +
  xlab("Year")



## Temporal alpha diversity per site 

## Count data like for a species richness curve should be modeled with poisson or negative binomials because theyre discrete integars (count) and not continuous
cpue_alphadiv.dat = cpue_alphadiv %>%
  ungroup() %>%
 # mutate(Year = as.numeric(Year)-2000)
  mutate(Year = as.vector(scale(as.numeric(Year), center = TRUE, scale = FALSE))) %>% 
  filter(WATER == "FBL")


pois_model <- glmer(alpha_div ~ as.numeric(Year) * HAB_1 +  (1 | SITE_N), family = poisson, data = cpue_alphadiv.dat )
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


#write.csv(sum_alpha, "Figures_Tables/TemporalDiversityData/AlphaDiv_summary_FBL.csv")
#write.csv(sum_alpha, "Figures_Tables/TemporalDiversityData/AlphaDiv_summary_LML.csv")

library(emmeans)

slopes = emtrends(pois_model, ~ HAB_1, var = "Year", delta.var = 1)
slopes = summary(slopes) %>%
  mutate(percent_change = round((exp(Year.trend) - 1) * 100, digits = 2), 
         asymp.LCL = round((exp(asymp.LCL) - 1)* 100, digits = 2), 
         asymp.UCL = round((exp(asymp.UCL) - 1)* 100, digits = 2), 
         SE = round(SE, digits = 2)) %>%
  select(-df) %>%
  mutate(cred = paste("[", asymp.LCL, ", ", asymp.UCL, "]", sep = ""))

#write.csv(slopes, file = "Figures_Tables/TemporalDiversityData/AlphaDiv_slopes_LML.csv")

## Shannon Diversity ----------------------
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
  select(WATER, Year, SITE_N, HAB_1, diversity,  everything()) %>%
  filter(WATER == "FBL" & Year > 2003 | WATER == "LML" & Year > 2000) %>%
  filter(WATER == "LML") %>%
  select(WATER, Year, SITE_N, HAB_1,diversity) %>%
  mutate(sig = case_when(HAB_1 == "RW" & WATER == "FBL" ~ "sig",
                         HAB_1 == "S" & WATER == "FBL" ~ "sig",
                         HAB_1 == "SW" & WATER == "FBL" ~ "sig",
                         HAB_1 == "SW" & WATER == "LML" ~ "sig")) %>%
  ungroup() %>%
  mutate(HAB_1 = as.factor(HAB_1)) %>%
  mutate(HAB_1 = relevel(HAB_1, ref = "SW")) %>%
  mutate(Year = scale(as.numeric(Year), center = TRUE, scale = FALSE))

shannon %>%
  filter(Year > 1998, HAB_1 != "NA") %>%
  ggplot(aes(x = as.numeric(Year),
             y = diversity,
             col = HAB_1),
         key_glyph = "rect") + 
  geom_jitter(alpha = .2) + 
  geom_vline(data = facet_data, aes(xintercept =YEAR), linetype = 2) +
  geom_smooth(method = lm, se = F) + 
  theme_minimal(base_size = 14) + 
  ylab("Shannon Diversity Index") + 
  xlab("Year") + 
  labs(col = "Habitat") + 
  scale_color_manual(labels = c("Rock","Wood + Rock","Fine Sediment",
                                "Wood + Fine Sediment"), 
                     values = pal[1:4] ) +
  facet_wrap(~WATER, labeller = labeller(WATER = water_labels))






## Normality tests
cat = lm(shannon$diversity ~ as.numeric(shannon$Year)) %>% summary()
tidy(cat)$estimate

shannon.resid = cpue_alphadiv %>%
  filter(WATER == "FBL", HAB_1 == "S") %>%
  rename(diversity = alpha_div)
dim(shannon.resid
    )
hist(shannon.resid$diversity)
qqnorm(shannon.resid$diversity)
qqline(shannon.resid$diversity)
shapiro.test(shannon.resid$diversity)


library(lmerTest)
shannon_model <- lmer(diversity ~ (Year) * HAB_1  + (1 | SITE_N),
                      data = shannon )


shannon.dat = summary(shannon_model)
AIC(shannon_model)
shannon.dat = shannon.dat$coefficients %>%
  as.data.frame() %>%
  mutate(sig = case_when(`Pr(>|t|)` <= .001 ~ "***",
                         `Pr(>|t|)` <= .01 ~ "**",
                         `Pr(>|t|)` <= .05 ~ "*",
                         `Pr(>|t|)` > .05 ~ ""))

write.csv(shannon.dat, file = "Figures_Tables/TemporalDiversityData/Shannon_Model_Coef_FBL.csv")

shannon.slopes = emtrends(shannon_model, ~ HAB_1, var = "Year", delta.var = 1)

shannon.slopes = summary(shannon.slopes) %>%
  mutate(percent_change = round(((Year.trend)) * 100, digits = 2), 
         lower.CL = round(((lower.CL) )* 100, digits = 2), 
         upper.CL = round(((upper.CL))* 100, digits = 2), 
         SE = round(SE, digits = 2)) %>%
  select(-df) %>%
  mutate(cred = paste("[", lower.CL, ", ", upper.CL, "]", sep = ""))
write.csv(shannon.slopes, file = "Figures_Tables/TemporalDiversityData/shannon_slopes_FBL.csv")

### Ratios ----------------------------



## Proportion of SMB to all other native species through time 


ratios = CPUE.w.sec %>% 
  mutate(non_bass_sum = BB + CC + CS + LT + MM + PS + RS + SS + ST + WS) %>%
  mutate(native_sum = BB + CC + CS + LT + PS + SS + ST + WS) %>% ## LML
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
  filter(ratio < 10000000 | native_ratio < 10000000) %>%
  filter(WATER == "FBL" & Year > 2003 | WATER == "LML" & Year > 2000) %>%
 
  mutate(native_ratio = (native_ratio+1)) %>%
  select(WATER, Year, SITE_N, HAB_1,native_ratio) %>%
  mutate(sig = case_when(HAB_1 == "RW" & WATER == "FBL" ~ "sig",
                         HAB_1 == "S" & WATER == "FBL" ~ "sig",
                         HAB_1 == "SW" & WATER == "FBL" ~ "sig",
                         HAB_1 == "SW" & WATER == "LML" ~ "sig"))



ratios %>% 
  mutate(Year = as.numeric(Year)) %>%
  filter(HAB_1 != "NA") %>%
  ggplot(aes( x = as.numeric(Year), y = native_ratio, col = HAB_1)) + 
  theme_minimal(base_size = 14) + 
  geom_point(alpha = .3) + 
  geom_smooth(aes(linetype = sig),method = 'lm', se = F) +
  geom_vline(data = facet_data, aes(xintercept =YEAR), linetype = 2) +
  scale_color_manual(labels = c("Rock", "Wood + Rock",
                                "Fine Sediment","Wood + Fine Sediment"),
                     values = pal[1:4] ) + 
  xlab("Year") + 
  ylab("Ratio Native:Bass") + 
  labs(col = "Habitat") + 
  facet_wrap(~WATER, labeller = labeller(WATER = water_labels)) +
  scale_y_log10()

## Are data normally distributed 
ratios %>%
  filter(WATER == "FBL") %>%
  ggplot(aes(x = log(native_ratio), fill = HAB_1))+
  geom_histogram() + 
  facet_wrap(~HAB_1, scales = "free") 


norm_test = ratios %>%
  filter(WATER == "FBL") %>%
  ungroup() %>%
  mutate(Year = scale(as.numeric(Year)), 
         native_ratio = log(native_ratio)) %>%
  mutate(HAB_1 = as.factor(HAB_1)) %>% 
  mutate(HAB_1 = relevel(HAB_1, ref = "SW"))


## Use log model only if doing gaussian model family 
#library(lmerTest)
log_model <- lmer(native_ratio ~ Year * HAB_1 + (1 | SITE_N),
                  data = norm_test)
summary(log_model)

plot(log_model)       # Residuals vs. fitted
qqnorm(resid(log_model)); qqline(resid(log_model))  # Q-Q plot

AIC(log_model)

log_model.dat = (summary(log_model))$coefficients %>%
  as.data.frame() %>% 
  mutate(`Pr(>|t|)` = case_when(`Pr(>|t|)` < .001 ~ .001, 
                                `Pr(>|t|)` < .01 ~ .01,
                                `Pr(>|t|)` > .01 ~ round(`Pr(>|t|)`, digits = 3))) %>%
  mutate(sig = case_when(`Pr(>|t|)` <= .001 ~ "***",
                         `Pr(>|t|)` <= .01 ~ "**",
                         `Pr(>|t|)` <= .05 ~ "*",
                         `Pr(>|t|)` > .05 ~ ""))


write.csv(log_model.dat, "Figures_Tables/TemporalDiversityData/ratio_summary_LML.csv")



slopes = emtrends(log_model, ~ HAB_1, var = "Year", delta.var = 1)


slopes = summary(slopes) %>%
  mutate(percent_change = round(((Year.trend)) * 100, digits = 2), 
         lower.CL = round(((lower.CL) )* 100, digits = 2), 
         upper.CL = round(((upper.CL))* 100, digits = 2), 
         SE = round(SE, digits = 2)) %>%
  select(-df) %>%
  mutate(cred = paste("[", lower.CL, ", ", upper.CL, "]", sep = ""))

write.csv(slopes, "Figures_Tables/TemporalDiversityData/slopes_ratios_LML.csv")





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

