`%nin%` = Negate(`%in%`)
library(MASS)
library(lmerTest)
library(performance)
library(broom.mixed)
library(wesanderson)
library(scales)
library(tidyverse)
## First Bisby --------------------------

## Habit data
env_updated_FBL = read.csv("Data/FBL_habitat.csv")

## FBL_v
#load("Data/ChangePoint_Data/FBL_v.RData")
load(file = "Data/ChangePoint_Data/FBL_v2.RData") ## Updated

FBL_model.data = FBL_v %>%
  left_join(env_updated_FBL) %>%
  na.omit() %>% 
  filter(Year > 2003) %>%
  group_by(Year, SITE_N) %>%
  mutate(SMB_1 = value[Species == "SMB_1"], 
         SMB_2 = value[Species == "SMB_2"]) %>%
  ungroup() %>%
  mutate(SMB_1 = scale(SMB_1), 
         SMB_2 = scale(SMB_2)) %>%
  group_by(Species, Year) %>% ## This does something similar to the lmer. but controls year to year variation
  mutate(value = scale(value)) %>%
  ungroup() %>%
  filter(!grepl("SMB", Species), ## Remove bass because theyre a variable not the study here
         !grepl("LT", Species), ## Remove lake trout because not one of target species 
         SITE_N != "BEF.FBL.001") ## Remove site 1 because of habitat classification differences

FBL_models = FBL_model.data %>%
  mutate(Year = scale(as.numeric(Year))) %>%
  nest_by(Species) %>% 
 #mutate(model = list(lmer(value ~   SMB_1 + SMB_2 + C + B + CW + SV + EV + FW + O +(1|Year), data = data)))
  mutate(model = list(lm(value ~   SMB_1 + SMB_2 + C + B + CW + SV + EV + FW + O , data = data))) 
 ## I'm going to leave this out for now because 
  # mutate(model = list(glm.nb(value ~ SMB_1 + SMB_2 + C + B + CW + SV + EV + FW + O ,
  #data = data)))


## Creek chub
FBL_models$model[[1]] %>% summary() ## CC1
r2(FBL_models$model[[1]])

FBL_models$model[[2]] %>% summary() ## CC2
r2(FBL_models$model[[2]])
## Mudminnow
FBL_models$model[[3]] %>% summary() ## MM3
r2(FBL_models$model[[3]])
FBL_models$model[[4]] %>% summary() ## MM4
r2(FBL_models$model[[4]])
## White sucker
FBL_models$model[[5]] %>% summary() ## WS_1
r2(FBL_models$model[[5]])
FBL_models$model[[6]] %>% summary() ## WS_2
r2(FBL_models$model[[6]])

FBL_results <- FBL_models %>%
  mutate(
    results = list(
      broom.mixed::tidy(
        model,
        effects = "fixed",
        conf.int = TRUE,
        conf.level = 0.95
      )
    )
  ) %>%
  select(Species, results) %>%
  unnest(results) %>%
  filter(term != "(Intercept)") %>%
  select(Species, term, estimate, conf.low, conf.high, std.error, statistic, p.value) %>%
  mutate(sig = case_when(p.value <= .05 ~ "T", p.value > .05 ~ "F"))




FBL_results_table = FBL_models %>%
  mutate(
    N = nobs(model),
    R2 = summary(model)$r.squared,
    adj_R2 = summary(model)$adj.r.squared,
    F_statistic = summary(model)$fstatistic[1],
    df1 = summary(model)$fstatistic[2],
    df2 = summary(model)$fstatistic[3],
    model_p = pf(
      F_statistic, df1, df2,
      lower.tail = FALSE
    )
  ) %>%
  select(Species, N, R2, adj_R2, F_statistic, df1, df2, model_p) %>% 
  mutate(model_p = round(model_p, digits = 3))  %>%
  mutate(across(R2:F_statistic, ~round(.x, digits = 3))) %>%
  mutate(model_p = case_when(model_p < 0.001 ~ "< 0.001", 
                             model_p >= 0.001 ~ as.character(model_p))) %>% 
  select(Species, N, df1, df2, R2, adj_R2, model_p)


FBL_results %>% filter(grepl("MM", Species))
## Little Moose ----------------------------------------------


load(file = "Data/LML.v.post2000.RData") ## updated
env_updated_LML = read.csv("Data/LML_habitat.csv")
LML.v





LML_model.data = LML.v %>%
 # separate(ID, into = c("SITE_N", "Species", "Age"), sep = "_") %>%
  filter(!grepl("BB", Species), 
         !grepl("RS", Species), 
         !grepl("LT", Species)) %>%
 # unite("Species", Species, Age, sep = "_") %>% 
  left_join(env_updated_LML) %>%
  na.omit() %>% 
  filter(Year > 2000) %>%
  group_by(Year, SITE_N) %>%
  mutate(SMB_1 = value[Species == "SMB_1"], 
         SMB_2 = value[Species == "SMB_2"]) %>%
  ungroup() %>%
  mutate(SMB_1 = scale(SMB_1), 
         SMB_2 = scale(SMB_2)) %>%
  group_by(Species, Year) %>%
  mutate(value = scale(value)) %>%
  ungroup() %>%
  filter(Species %nin% c("SMB_1", "SMB_2")) %>%
  ## Remove Site 1 Lodge/Dock
  filter(SITE_N != "BEF.LML.001")

LML_models = LML_model.data %>%
  mutate(Year = scale(as.numeric(Year))) %>%
  nest_by(Species) %>% 
  #mutate(model = list(lmer(value ~   SMB_1 + SMB_2 + C + B + CW + SV + EV + FW + O +(1|Year), data = data)))
  mutate(model = list(lm(value ~   SMB_1 + SMB_2 + C + B + CW + SV + EV + FW + O , data = data)))

## Creek chub
LML_models$model[[1]] %>% summary() ## CC1
r2(LML_models$model[[1]])

LML_models$model[[2]] %>% summary() ## CC2
r2(LML_models$model[[2]])

## Common shiner
LML_models$model[[3]] %>% summary() ## CS3
r2(LML_models$model[[3]])
LML_models$model[[4]] %>% summary() ## CS4
r2(LML_models$model[[4]])

## Mudminnow
LML_models$model[[5]] %>% summary() ## WS_1
r2(LML_models$model[[5]])
LML_models$model[[6]] %>% summary() ## WS_2
r2(LML_models$model[[6]])

## Pumpkinseed
LML_models$model[[7]] %>% summary() ## WS_1
r2(LML_models$model[[7]])
LML_models$model[[8]] %>% summary() ## WS_2
r2(LML_models$model[[8]])


## White sucker
LML_models$model[[9]] %>% summary() ## WS_1
r2(LML_models$model[[9]])
LML_models$model[[10]] %>% summary() ## WS_2
r2(LML_models$model[[10]])




LML_results = LML_models %>%
  mutate(
    results = list(
      broom.mixed::tidy(
        model,
        effects = "fixed",
        conf.int = TRUE,
        conf.level = 0.95
      )
    )
  ) %>%
  select(Species, results) %>%
  unnest(results) %>%
  filter(term != "(Intercept)") %>%
  select(Species, term, estimate, conf.low, conf.high, std.error, statistic, p.value) %>%
  mutate(sig = case_when(p.value <= .05 ~ "T", p.value > .05 ~ "F"))




LML_results_table = LML_models %>%
  mutate(
    N = nobs(model),
    R2 = summary(model)$r.squared,
    adj_R2 = summary(model)$adj.r.squared,
    F_statistic = summary(model)$fstatistic[1],
    df1 = summary(model)$fstatistic[2],
    df2 = summary(model)$fstatistic[3],
    model_p = pf(
      F_statistic, df1, df2,
      lower.tail = FALSE
    )
  ) %>%
  select(Species, N, R2, adj_R2, F_statistic, df1, df2, model_p) %>% 
  mutate(model_p = round(model_p, digits = 3))  %>%
  mutate(across(R2:F_statistic, ~round(.x, digits = 3))) %>%
  mutate(model_p = case_when(model_p < 0.001 ~ "< 0.001", 
                             model_p >= 0.001 ~ as.character(model_p))) %>% 
  select(Species, N, df1, df2, R2, adj_R2, model_p)


## Combining both lakes

supplemental_table = rbind(LML_results_table %>% 
                             mutate(Water = "LML"), 
                           FBL_results_table %>%
                             mutate(Water = "FBL"))

write.csv(supplemental_table, file = "Figures_Tables/Model_Coefficients.csv",
          row.names = F)

results = rbind(LML_results %>% 
                  mutate(WATER = "LML"), FBL_results %>%
                  mutate(WATER = "FBL")) %>% 
  mutate(WATER = factor(WATER, levels = c("LML", "FBL"))) %>%
  mutate(term = factor(term, levels = c("SMB_1", "SMB_2", "EV", "SV", "O", "CW","FW", "C", "B"))) 




results %>%
  ggplot(aes(x = estimate, y = term, alpha = sig, color = WATER)) + 
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high)) + 
  facet_wrap(~Species, labeller = labeller(Species = c("CC_1" = "creek chub (J)", 
                              "CC_2" = "creek chub (A)", 
                              "CS_1" = "common shiner (J)",
                              "CS_2" = "common shiner (A)", 
                              "MM_1" = "central\nmudminnow (J)", 
                              "MM_2" = "central\nmudminnow (A)",
                              "PS_1" = "pumpkinseed (J)", 
                              "PS_2" = "pumpkinseed (A)", 
                              "WS_1" = "white sucker (J)", 
                              "WS_2" = "white sucker (A)"))) +
  geom_vline(xintercept = 0) + 
  scale_alpha_manual("Significance", values = c("F" = .2,"T" =  1)) +
  scale_color_manual("Water", labels = c("LML" ="Little Moose",
                                         "FBL" = "First Bisby"),
                     values = wes_palette("Zissou1", n = 2, type = "continuous")) +
  theme_bw(base_size = 11) + 
  guides(alpha = "none") + 
  xlab("Estimate") +
  theme(axis.title.y = element_blank()) + 
  scale_y_discrete(labels = c("B" = "Boulders", 
                              "C" = "Cobbles",
                              "FW" = "Fine wood", 
                              "CW" = "Coarse wood", 
                              "O" = "Organic debris", 
                              "SV" = "Subm. veg", 
                              "EV" = "Emerg. veg",
                              "SMB_1" = "SMB (J)", 
                              "SMB_2" = "SMB (A)"))


ggsave(filename = "Figures_Tables/Figure5_ModelCoefficients.jpeg", width = 7, height = 5)

## Singular figures
FBL_results %>%
  ggplot(aes(x = estimate, y = term, alpha = sig)) + 
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high)) + 
  facet_wrap(~Species) +
  geom_vline(xintercept = 0) + 
  scale_alpha_manual("Significance", values = c(.2, 1)) +
  theme_bw() + 
  guides(alpha = "none") + 
  xlab("Estimate") +
  theme(axis.title.y = element_blank())


LML_results %>%
  ggplot(aes(x = estimate, y = term, alpha = sig)) + 
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high)) + 
  facet_wrap(~Species) +
  geom_vline(xintercept = 0) + 
  scale_alpha_manual("Significance", values = c("F" = .2,"T" =  1)) +
  theme_bw() + 
  guides(alpha = "none") + 
  xlab("Estimate") +
  theme(axis.title.y = element_blank())



