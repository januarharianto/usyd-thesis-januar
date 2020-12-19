library(tidyverse)
library(readxl)

df <- read_excel("input/ch_larval/larval_mR_2015.xlsx", 2, skip = 1)
df %>%
  select(temp, ph, temp_c) %>%
  group_by(temp, ph) %>%
  summarise(mean = mean(temp_c), sd = sd(temp_c), se = sd/sqrt(n()))
