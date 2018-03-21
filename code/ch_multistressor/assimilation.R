# Pre-processing
rm(list=ls())  # remove all objects from global environment
gc()           # force garbage collection

# load packages
library(tidyverse)
library(readxl)

# prep data
assi <- read_excel("input/ch_multistressor/assimilation.xlsx", skip = 2) %>%
  select(treatment, temp, ph, grp, calc) %>%
  print()

# plot it
ggplot(assi, aes(treatment, calc)) +
  geom_boxplot()

# doesn't seem to have a pattern here, boss

# explore:
assi %>%
  group_by(treatment) %>%
  summarise(mean = mean(calc),
    sd = sd(calc)) %>%
  print()


