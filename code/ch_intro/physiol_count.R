library(tidyverse)
library(readxl)

dt <- read_excel("input/ch_intro/physiol_count.xlsx", skip = 2)
dt

pcounts_plot <- ggplot(dt) +
  geom_bar(aes(year, total), stat = "identity", fill = "black") +
  geom_bar(aes(year, phys), stat = "identity", fill = "lightgoldenrod3") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  xlab("Year") +
  ylab("No. of published studies") +
  cowplot::theme_cowplot() +
  geom_blank()
pcounts_plot

cowplot::save_plot(
  "figs/ch_intro/pcount.png", pcounts_plot,
  base_height = 8, base_width = 8
)
