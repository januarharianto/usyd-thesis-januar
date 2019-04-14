# load data
# library(readr)
library(tidyverse)
library(readxl)
dt <- read_excel("assets/data_in/c4_krieseltemps.xlsx") %>%
  group_by(Day, Treatment) %>%
  summarise(mean = mean(Temp), sd = sd(Temp))
  

juvtemp <- ggplot(dt, aes(Day, mean, col = Treatment)) +
  geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), width = 0) +
  geom_line() +
  # geom_boxplot() +
  # cowplot::theme_cowplot() +
  # viridis::scale_colour_viridis(discrete = T) +
  scale_colour_brewer(palette = "Paired") +
  scale_x_continuous(name= "Days", limits=c(0, 17)) +
  scale_y_continuous(name= "Temperature (°C)", 
    limits=c(16, 26), 
    breaks = seq(16, 26, by = 2)) +
  theme(legend.position= "bottom") +
  geom_blank()



cowplot::save_plot(
  "assets/figs/c4juvtemps.png", juvtemp,
  base_height = 8, base_width = 8
)
