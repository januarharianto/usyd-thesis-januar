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
# C_E :  Energy available from food in J/g/day
# A   :  Absorption efficiency in %
# R_E :  Energy lost in respiration in J/g/day
# U   :  Energy lost due to ammonia excretion in J/g/day


# Pre-processing
library(snipR)
snipR::refresh()

# Load libraries

snipR::pkgs(
  tidyverse,
  readxl,
  huxtable
)

# Load data ----
load(file = "output/ch_multistressor/feeding.RData") # feeding data
rm(feed_anova, feed_lsq, feed_model, feed_posthoc, feeding_plot)

# Respiration data ----
# Note that rate unit is in mgO2/h/kg
resp <- read_excel("input/ch_multistressor/metabolic_rate.xlsx", sheet = 2) %>%
  print()

# Convert the rate to energy equivalent units.
# Conversion is 14.14 J/mgO2
sfg_resp_raw <-
  resp %>%
  filter(!is.na(rate)) %>% # remove NA values
  filter(week == 12) %>% # grab only week 12 data
  mutate(resp = rate * 14.14) %>% # conversion: mg/h/kg to J/h/kg
  mutate(resp = resp / 1000) %>% # conversion: J/h/kg to kJ/h/kg
  group_by(treatment) %>% # group by treatment
  print()

sfg_resp <- summarise(sfg_resp_raw,
    Re = mean(resp),
    Re_sd = sd(resp),
    Re_se = Re_sd / sqrt(length(resp))
  ) %>% # grab summary stats
  print() # END


# Ammonia excretion data ----

## Rate of ammonia excretion is in ug/h/kg
## Conversion is 0.025 J/g ammonia
sfg_ammo_raw <-
  read_excel("input/ch_multistressor/ammonia.xlsx", skip = 3) %>% # read data from file
  mutate(ammo = ammonia / 1e6) %>% # conversion: ug/h/kg to g/h/kg
  mutate(ammo = ammo * 0.025) %>% # conversion: g/h/kg to J/h/kg
  mutate(ammo = ammo * 1e3) %>% # conversion: J/h/kg to kJ/h/kg
  group_by(treatment) %>% # group by treatment
  print()

sfg_ammo <- summarise(sfg_ammo_raw,
    Ue = mean(ammo),
    Ue_sd = sd(ammo),
    Ue_se = Ue_sd / sqrt(length(ammo))
  ) %>% # grab summary stats
  print()
# END
##

# Feeding data ----

sfg_feed_raw <-
  feeding_rate %>%
  mutate(feed = rate * 12.674) %>% # conversion: g/day/g to kJ/day/g
  mutate(feed = feed * 1000 / 1000) %>%
  group_by(treatment) %>% # group by treatment
  print()

sfg_feed <- summarise(sfg_feed_raw,
    Ce = mean(feed),
    Ce_sd = sd(feed),
    Ce_se = Ce_sd / sqrt(length(feed))
  ) %>% # grab summary stats
  print() # END
# Here is where I'm not sure if the conversion is right. It's 12.674 g to kJ of
# **AFDW**. However food consumed is in wet weight. I think some conversion is
# needed here.

# Assimilation efficiency data ----

sfg_assi <- read_excel("input/ch_multistressor/assimilation.xlsx", skip = 2) %>%
  group_by(treatment) %>%
  summarise(
    A = mean(calc),
    A_sd = sd(calc),
    A_se = A_sd / sqrt(length(calc))
  ) %>%
  print()

# Calculate scope for growth ----

## individual results


## group results
sfg <- list(sfg_resp, sfg_ammo, sfg_feed, sfg_assi) %>%
  map_df(gather, key, value, -treatment) %>% # gather data from lists
  spread(key, value) %>%
  mutate(sfg = (Ce * A / 100) - (Re + Ue)) %>% # calculate sfg using standard eqn
  mutate(sfg_sd = sqrt(Ce_sd ^ 2 + Re_sd ^ 2 + Ue_sd ^ 2)) %>% # recalculate sd
  mutate(sfg_se = sqrt(Ce_se ^ 2 + Re_se ^ 2 + Ue_se ^ 2)) %>% # recalculate se
  mutate(ph = c("pH 7.6", "pH 8.0", "pH 7.6", "pH 8.0", "pH 7.6", "pH 8.0")) %>%
  mutate(ph = factor(ph, levels = c("pH 8.0","pH 7.6"))) %>%
  mutate(temp = factor(c("Ambient","Ambient","+2 °C","+2 °C","+3 °C","+3 °C"), 
    levels = c("Ambient", "+2 °C","+3 °C"))) %>%
  print()

# Check summary ----

sfg %>%
  select(sfg, sfg_sd, sfg_se) %>%
  print()


## Plot sfg - but don't use this for the manuscript. Just do the table.
sfgplot <- ggplot(sfg, aes(temp, sfg, fill = factor(ph))) +
  # aes(fill = as.factor(ph)) +
  facet_wrap(.~ ph, strip.position = "bottom") +
  # Plots
  geom_bar(stat = "identity", colour = "black", alpha = .25) +
  geom_errorbar(aes(ymin = (sfg - sfg_se), ymax = (sfg + sfg_se)), width = 0) +
  # Theme
  cowplot::theme_cowplot() +
  scale_fill_manual(values = c("white", "steelblue3")) +
  # Axes (scale)
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    strip.background = element_rect(colour = "black", fill = NA)
  ) +
  # make facet labels bold:
  theme(strip.text = element_text(size = 13),
  legend.position = "none") +
  theme(strip.background = element_blank(), strip.placement = "outside") +
  # Labels (title, x & y)
  # annotate("segment",x=-Inf,xend=-Inf,y=-Inf,yend=-Inf,color="black",lwd=1) +
  xlab("") +
  ylab(expression("SfG ("*kJ~h^{-1}~kg^{-1}*")")) +
  # change x axis labels:
  # scale_x_discrete(labels = c("Ambient\n(20.1 °C)", "+2\n(22.1 °C)", "+3 \n(23.1°C)")) +
  # Legend
  geom_blank()
# END ggplot
sfgplot

## save plot ----
cowplot::save_plot(
  "figs/ch_multistressor/sfg_plot.png", sfgplot,
  base_height = 8, base_width = 8
)

save.image(file ="output/ch_multistressor/sfg.RData")

library(knitr)
library(kableExtra)
sfg %>%
  select(treatment, A, Ce, Re, Ue, sfg) %>%
  mutate(Treatment = c("+0 °C; pH 7.6", "+0 °C; pH 8.0", "+2 °C; pH 7.6", 
    "+2 °C; pH 8.0", "+3 °C; pH 7.6", "+3 °C; pH 8.0")) %>%
  mutate(Treatment = factor(Treatment, 
    levels = c("+0 °C; pH 8.0", "+0 °C; pH 7.6",  "+2 °C; pH 8.0",
      "+2 °C; pH 7.6", "+3 °C; pH 8.0", "+3 °C; pH 7.6"))) %>%
  arrange(Treatment) %>%
  rename("AE (%)" = A, Feeding = Ce, Respiration = Re, Excretion = Ue,
    SfG = sfg) %>%
  select(7,2,3,4,5,6) %>%
  kable(booktabs = TRUE, caption = "(ref:sfgtab)")

