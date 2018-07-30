library(tidyverse)
library(readxl)

dt <- read_excel("input/ch_intro/physiol_count.xlsx", skip = 2) %>%
  select(-4) %>%
  # gather(type,count, total:phys) %>%
  print


# pcounts_plot <- ggplot(dt) +
#   geom_bar(aes(year, count, fill = type), stat = "identity") +
#   scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
#   xlab("Year") +
#   ylab("No. of published studies") +
#   cowplot::theme_cowplot() +
#   scale_fill_brewer(palette = "Accent") +
#   theme(legend.position = "none") +
#   geom_blank()

pcounts_plot <- ggplot(dt) +
  geom_bar(aes(year, total), stat = "identity", fill = "firebrick2") +
  geom_bar(aes(year, phys), stat = "identity", fill = "goldenrod2") +
  # geom_bar(aes(year, total), stat = "identity", fill = "black") +
  # geom_bar(aes(year, phys), stat = "identity", fill = "grey") +
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
 
