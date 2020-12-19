# Calculate scope for growth.
#
# We first calculate the energy from food using the equation:
#
# C_E = (T * C_S) / W_S
#
# where
#
# T   :  Time elapsed in hours - here it is 24 hours
# C_S :  Effective heat energy of calorimeter chambers in kJ/g
# W_S :  Weight of the sample used in the calorimeter in g
#
#
# Then, we calculate SfG using the equation:
#
# SfG = C_E * A - (R_E + U)
#
# where
#
# SfG :  Scope for Growth
# C_E :  Energy available from food 
# A   :  Absorption efficiency in %
# R_E :  Energy lost in respiration
# U   :  Energy lost due to ammonia excretion
# basic unit is J/g/day but we will convert to kJ/h/kg 
# this way we can compare to current literature (usually J/h/kg or kJ/h/kg)

library(tidyverse)
library(readxl)
library(sciplot) # for se() function
library(cowplot)


# Load data ----
load(file = "output/ch_multistressor/feeding.RData") # feeding rate
rm(agar_dt, fd, feeding_plot, fit, hsd, model, mxmodel, c, correction, m)

# C_E: mean energy available from feeding
feed <- 
  feeding_rate %>%
  mutate(feed = rate_dry * 12.674) %>% # conversion: g/h/g to J/h/g
  mutate(feed = feed * 1000) %>%   # J/h/g to kJ/h/g
  mutate(feed = feed/1000) %>%     # kJ/h/g to kJ/h/kg
  group_by(treatment) %>% # group by treatment
  summarise(ce = mean(feed), ce_se = se(feed)) %>%
  print()

# A: mean assimilation efficiency 
assi <- read_excel("input/ch_multistressor/assimilation.xlsx", skip = 2) %>%
  group_by(treatment) %>%
  summarise(a = mean(calc)) %>%
  mutate(a = a/100) %>% # convert to proportion
  print()

# R_E: energy lost due to respiration
resp <- read_excel("input/ch_multistressor/metabolic_rate.xlsx", 2) %>%
  filter(!is.na(rate)) %>% # remove NA values
  filter(week == 12) %>% # grab only week 12 data
  mutate(resp = rate * 14.14) %>% # conversion: mg/h/kg to J/h/kg
  mutate(resp = resp / 1000) %>% # conversion: J/h/kg to kJ/h/kg
  group_by(treatment) %>% # group by treatment
  summarise(re = mean(resp), re_se = se(resp)) %>%
  print()

# U: mean energy lost from ammonia excretion
ammo <- 
  read_excel("input/ch_multistressor/ammonia.xlsx", skip = 3) %>%
  filter(!is.na(ammonia)) %>% # remove NA values (there should be none)
  mutate(ammo = ammonia / 1e6) %>% # conversion: ug/h/kg to g/h/kg
  mutate(ammo = ammo * 0.025) %>% # conversion: g/h/kg to J/h/kg
  mutate(ammo = ammo * 1e3) %>% # conversion: J/h/kg to kJ/h/kg
  group_by(treatment) %>% # group by treatment
  summarise(u = mean(ammo), ammo_se = se(ammo)) %>%
  print()

# generate the sfg data frame
# use urchins in metabolic rate data as units

# SfG = C_E * A - (R_E + U)
sfg <- 
  resp %>%
  inner_join(feed) %>%
  inner_join(ammo) %>%
  inner_join(assi) %>%
  mutate(sfg = (ce * a - (re + u))) %>%
  mutate(sfg_se = sqrt(ce_se^2 + re_se^2 + ammo_se^2)) %>%
  print()

# prepare for plotting - grab factors from original data:
facs <- read_excel("input/ch_multistressor/metabolic_rate.xlsx", 2) %>%
  select(4,6,7) %>%
  mutate(temp_fac = factor(temp_fac), 
         ph_fac = factor(ph_fac, levels = c("pH 8.0", "pH 7.6"))) %>%
  distinct() %>%
  print()

# join factor data with sfg data
sfg <- inner_join(sfg, facs)
sfg

# ok, plot (figure 5 in manuscript)
sfg_plot <- 
  ggplot(sfg, aes(temp_fac, sfg, fill = ph_fac)) +
  geom_bar(stat = "identity", 
           position = "dodge",
           colour = "black", 
           alpha = .25) +
  geom_errorbar(aes(ymin = (sfg - sfg_se), ymax = (sfg + sfg_se)),
                width = 0, 
                position = position_dodge(width = .9)) +
  theme_half_open() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = .5),
    strip.background = element_rect(colour = "black", fill = NA)
  ) +
  scale_fill_manual(name = "", values =c("white", "steelblue3")) +
  scale_x_discrete(labels = c("Ambient\n(16.6 °C)", "+2\n(18.6 °C)", 
                              "+3\n(19.6 °C)")) +
  theme(
    legend.position = c(.95, 1),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  ) +
  xlab("") +
  ylab(expression("SfG ("*kJ~h^{-1}~kg^{-1}*")")) +
  geom_blank()

cowplot::save_plot(
  "figs/ch_multistressor/sfg_plot.pdf", sfg_plot,
  base_height = 6, base_width = 8
)
