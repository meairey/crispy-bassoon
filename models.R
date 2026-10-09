`%nin%` = Negate(`%in%`)
library(lmerTest)

library(broom.mixed)
library(wesanderson)
## First Bisby --------------------------

## Habit data
env_updated_FBL = read.csv("Data/FBL_habitat.csv")

## FBL_v
load("Data/ChangePoint_Data/FBL_v.RData")

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
  filter(Species %nin% c("SMB_1", "SMB_2"))

FBL_models = FBL_model.data %>%
  mutate(Year = scale(as.numeric(Year))) %>%
  nest_by(Species) %>% 
 # mutate(model = list(lmer(value ~   SMB_1 + SMB_2 + C + B + CW + SV + EV + FW + O +(1|Year), data = data)))
  mutate(model = list(lm(value ~   SMB_1 + SMB_2 + C + B + CW + SV + EV + FW + O , data = data)))
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

library(performance)




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



## Little Moose ----------------------------------------------

load(file = "Data/LML.v.post2000.RData")
env_updated_LML = read.csv("Data/LML_habitat.csv")
LML.v





LML_model.data = LML.v %>%
  separate(ID, into = c("SITE_N", "Species", "Age"), sep = "_") %>%
  filter(Species != "ST") %>%
  unite("Species", Species, Age, sep = "_") %>% 
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
  filter(Species %nin% c("SMB_1", "SMB_2"))

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




LML_results <- LML_models %>%
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



## Combining both lakes

results = rbind(LML_results %>% 
                  mutate(WATER = "LML"), FBL_results %>%
                  mutate(WATER = "FBL")) %>% 
  mutate(WATER = factor(WATER, levels = c("LML", "FBL"))) %>%
  mutate(term = factor(term, levels = c("SMB_1", "SMB_2", "EV", "SV", "O", "CW","FW", "C", "B")))

results %>%
  ggplot(aes(x = estimate, y = term, alpha = sig, color = WATER)) + 
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high)) + 
  facet_wrap(~Species, labeller = labeller(Species = c("CC_1" = "CC (J)", 
                              "CC_2" = "CC (A)", 
                              "CS_1" = "CS (J)",
                              "CS_2" = "CS (A)", 
                              "MM_1" = "MM (J)", 
                              "MM_2" = "MM (A)",
                              "PS_1" = "PS (J)", 
                              "PS_2" = "PS (A)", 
                              "WS_1" = "WS (J)", 
                              "WS_2" = "WS (A)"))) +
  geom_vline(xintercept = 0) + 
  scale_alpha_manual("Significance", values = c("F" = .2,"T" =  1)) +
  scale_color_manual("Water", labels = c("LML" ="Little Moose",
                                         "FBL" = "First Bisby"),
                     values = wes_palette("Zissou1", n = 2, type = "continuous")) +
  theme_bw() + 
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



