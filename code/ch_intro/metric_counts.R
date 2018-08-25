library(tidyverse)
library(readxl)
library(ggthemes)
library(RColorBrewer)



dt <- read_excel("input/ch_intro/physiol_count.xlsx", sheet = 2) %>%
  # select(-c(2,4)) %>%
  gather(group, no) %>%
  transform(group = reorder(group, -no)) %>%
  mutate(pct = round(prop.table(no) * 100, 1)) %>%
  print
colourCount <- length(unique(dt$group))

g <- ggplot(dt, aes(group, no, fill = group))
counts <- 
  g + 
  geom_bar(stat = "identity") +
  geom_text(aes(y = no + 30, label = paste0(pct, '%')), size = 3.5) +
  cowplot::theme_cowplot() +
  # scale_fill_brewer(palette = "RdGy") +
  # scale_fill_manual(values = colorRampPalette(rev(brewer.pal(8, "RdYlBu")))(colourCount)) +
  scale_fill_manual(values = colorRampPalette(brewer.pal(8, "RdYlBu"))(colourCount)) +
  # scale_fill_tableau() +
  theme(legend.position="none") +
  xlab("") +
  ylab("Reported") +
  coord_flip() +
  geom_blank()
counts

cowplot::save_plot(
  "figs/ch_intro/pgroups.png", counts,
  base_height = 6, base_width = 8
) 
