# Notes ----

## Description

# Pre-processing ----

library(snipR)  # my personal package
snipR::refresh()

# Load packages ----

snipR::pkgs(
  tidyverse,
  readxl,
  car,
  cowplot
)

# Load data ----

dt <- read_excel("input/ch_warming/gonadindex.xlsx", 2) %>%
  dplyr::select(treatment, gi = gonad.index) %>%
  filter(!is.na(gi)) %>%
  print()

# Manipulate data ----

## Grab summary stats:
summ <- dt %>%
  group_by(treatment) %>%
  summarise(count = length(treatment), mean = mean(gi)) %>%
  print()

# Plotting ----

gi_plot <-
  ggplot(dt, aes(as.character(treatment), gi)) +
  geom_boxplot(fatten = 1) +
  stat_summary(fun.y = median, geom = "errorbar",
    aes(ymax = ..y.., ymin = ..y.., linetype = "median"),
    width = .75) +
  stat_summary(fun.y = mean, geom = "errorbar",
    aes(ymax = ..y.., ymin = ..y.., linetype = "mean"),
    width = .75) +
  scale_linetype_manual(name = "", values = c(3,1)) +
  xlab("Treatment temperature (°C)") +
  ylab("Gonad Index")
gi_plot


# Analysis ----

model <- lm(gi~treatment, data = dt)

analysis <-
  Anova(model) %>% # perform ANOVA (one-way)
  print()
##  Not significant.



# Export data ----

cowplot::save_plot("figs/ch_warming/gindex.png", gi_plot,
  base_height = 6, base_width = 6*1.3)
