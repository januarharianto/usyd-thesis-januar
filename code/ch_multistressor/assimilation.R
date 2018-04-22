# Pre-processing
library(snipR)
snipR::refresh()


# load packages
library(tidyverse)
library(readxl)

# prep data
assi <- read_excel("input/ch_multistressor/assimilation.xlsx", skip = 2) %>%
  select(treatment, temp, ph, grp, calc) %>%
  print()

# explore:
assi %>%
  group_by(treatment) %>%
  summarise(mean = mean(calc),
    sd = sd(calc)) %>%
  print()


# plot it
ggplot(assi, aes(treatment, calc)) +
  geom_boxplot()

# doesn't seem to have a pattern here, boss



