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
  geom_boxplot(    
    fill = "lightyellow",
    fatten = 1,
    alpha = .6,
    outlier.size = 2,
    outlier.alpha = .8) +
  stat_summary(fun.y = median, geom = "errorbar",
    aes(ymax = ..y.., ymin = ..y.., linetype = "Median"),
    width = .75) +
  stat_summary(fun.y = mean, geom = "errorbar",
    aes(ymax = ..y.., ymin = ..y.., linetype = "Mean"),
    width = .75) +
  scale_linetype_manual(name = "", values = c(3,1)) +
  theme(legend.position = "bottom", legend.justification = "center") +
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

cowplot::save_plot("figs/ch_warming/c2gindex.png", gi_plot,
  base_height = 8, base_width = 8)
