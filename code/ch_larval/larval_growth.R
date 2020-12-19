# Script written by Januar Harianto

# Load main packages -----
library(tidyverse)

# Load data -----
dt <- readxl::read_excel("input/ch_larval/larval_growth_2015.xlsx") %>%
  mutate(parent_temp = factor(parent_temp)) %>%
  mutate(parent_ph = factor(parent_ph)) %>%
  print()

# Data wrangling -----

summdt <-
  dt %>%
  group_by(parent, parent_temp, parent_ph, f0_temp, f0_ph) %>%
  summarise(mean = mean(diameter_um),
            sd = sd(diameter_um),
            se = sd/sqrt(30)) %>%
  print()

## Filter out f0_ph

dta <-
  dt %>%
  filter(f0_ph == 8) %>%
  group_by(parent, parent_temp, parent_ph, f0_temp, f0_ph) %>%
  summarise(mean = mean(diameter_um),
            sd = sd(diameter_um),
            se = sd/sqrt(30)) %>%
  print()

dtb <-
  dt %>%
  filter(f0_ph == 7.6) %>%
  group_by(parent, parent_temp, parent_ph, f0_temp, f0_ph) %>%
  summarise(mean = mean(diameter_um),
            sd = sd(diameter_um),
            se = sd/sqrt(30)) %>%
  print()


## get summary

dta %>%
  group_by(f0_ph) %>%
  summarise(average = mean(mean),
    sd = sd(mean), 
    se = sd/sqrt(n())) 

dtb %>%
  group_by(f0_ph) %>%
  summarise(average = mean(mean), 
    sd = sd(mean),
    se = sd/sqrt(n()))

# Data plotting -----

# ggplot(summdt, aes(factor(f0_temp), mean, fill = parent)) +
#   geom_bar(stat = "identity", width = 1, position = position_dodge(1)) +
#   geom_errorbar(aes(ymax = mean + se, ymin = mean - se),
#                 position = position_dodge(1), width = 0) +
#   facet_wrap(~ f0_ph, strip.position = "bottom") +
#   theme(panel.spacing = unit(1, "lines"),
#         strip.background = element_blank(),
#         strip.placement = "outside") +
#   cowplot::theme_cowplot() +
#   scale_fill_brewer() +
#   geom_blank()

p1 <- ggplot(dta, aes(f0_temp, mean, fill = parent)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymax = mean + se, ymin = mean - se),
                position = position_dodge(.9), width = 0) +
  ylim(0, 430) +
  cowplot::theme_cowplot(font_size = 20) +
  scale_fill_brewer(name = "Parental conditions",
    labels = c(
      "Ambient °C | 7.6 pH",
      "Ambient °C | 8.0 pH",
      "+2 °C | 7.6 pH",
      "+2 °C | 8.0 pH")) +
  xlab("pH 8.0") +
  ylab(expression("Test Diameter (" *mu*m * ")")) +
  # fix legend
  geom_blank()
p1

p2 <- ggplot(dtb, aes(f0_temp, mean, fill = parent)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymax = mean + se, ymin = mean - se),
                position = position_dodge(.9), width = 0) +
  ylim(0, 430) +
  cowplot::theme_cowplot(font_size = 20) +
  scale_fill_brewer() +
  theme(axis.title.y=element_blank(), 
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    # axis.ticks.x=element_blank(),
    panel.grid = element_blank()) +
  xlab("pH 7.6") +
  geom_blank()

# Data analysis -----
library(car) # For ANOVA type II and III
library(lme4) # For mixed models

lm(diameter_um ~ parent_temp*parent_ph*f0_temp*f0_ph, dt) %>%
  Anova(type = 3, contrasts = list(topic=contr.sum, sys=contr.sum))

lm(diameter_um ~ parent_temp + parent_ph + f0_temp + f0_ph, dt) %>%
  Anova()


# Refine plots if necessary and save -----
legend <- cowplot::get_legend(p1 + theme(legend.direction = "horizontal",
  legend.justification = "center", legend.box.just = "bottom"))

cp <- 
  cowplot::plot_grid(
    p1 + theme(legend.position = "none"), 
    p2 + theme(legend.position = "none"),
    ncol = 2)

cpl <- cowplot::plot_grid(cp, legend, ncol = 1, rel_heights = c(1, .05))

cowplot::save_plot(
  "figs/ch_larval/larvalgrowth.png", cpl,
  base_height = 12, base_width = 12
)
