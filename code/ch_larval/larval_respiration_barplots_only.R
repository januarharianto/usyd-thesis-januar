library(tidyverse)
library(readxl)
library(viridis)
dt <- 
  read_excel("input/ch_larval/larval_mR_2015.xlsx", sheet = 2, skip = 1) %>%
  group_by(parents, parent_temp, parent_ph, temp, ph) %>%
  summarise(mean = mean(mr),
    se = sd(mr)/sqrt(3)) %>%
  print()

# Brewer colours -----
# Generate plots -----
# offspring low pH
p1_bar <- 
  dt %>%
  mutate(parents = factor(parents, levels = c("0C", "0A", "2C", "2A"))) %>%
  filter(ph == "pH 7.6") %>%
  ggplot(aes(factor(temp), mean, fill = parents)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black", size = .5) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .1,
    position = position_dodge(.9)) +
  cowplot::theme_cowplot(font_size = 16) +
  scale_fill_brewer(name = "Parental conditions",
    labels = c(
      "+0 °C; pH 8.0",
      "+0 °C; pH 7.6",
      "+2 °C; pH 8.0",
      "+2 °C; pH 7.6")) +
  xlab("") +
  ylab("") +
  # ggtitle("7.6 pH") +
  ggtitle("pH 7.6") +
  theme(plot.title = element_text(size = 14, face = "bold")) +
  geom_blank()


# offspring ambient pH
p2_bar <- 
  dt %>%
  mutate(parents = factor(parents, levels = c("0C", "0A", "2C", "2A"))) %>%
  filter(ph == "pH 8.0") %>%
  ggplot(aes(factor(temp), mean, fill = parents)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black", size = .5) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .1,
    position = position_dodge(.9)) +
  cowplot::theme_cowplot(font_size = 16) +
  scale_fill_brewer(name = "Parental conditions",
    labels = c(
      "+0 °C; pH 8.0",
      "+0 °C; pH 7.6",
      "+2 °C; pH 8.0",
      "+2 °C; pH 7.6")) +
  xlab("") + # leave this blank so that it doesn't duplicate in plot_grid()
  ylab("") +
  # ggtitle("8.0 pH") +
  ggtitle("pH 8.0") +
  theme(plot.title = element_text(size = 14, face = "bold")) +
  geom_blank()


# Generate legends -----

## for legends on right side -----
legend_right <- cowplot::get_legend(p1_bar + 
    theme(legend.box.margin = margin(0, 0, 0, 12)))


## for legends on bottom side -----
legend_bot <- cowplot::get_legend(p1_bar +
    theme(legend.direction = "horizontal",
      legend.justification = "center" ,
      legend.box.just = "bottom"))

## vertical-arranged plots -----
pp_v <- 
  cowplot::plot_grid(p2_bar + theme(legend.position = "none"), 
    p1_bar + theme(legend.position = "none"),
    ncol = 1,
    labels = c("A   8.0 pH","B   7.6 pH")) +
  cowplot::draw_label((expression("Metabolic rate (" ~ pmol ~ O[2] ~~ h ^ -1 ~~ mu*m ^ -1 * ")")), x = 0, y = 0.5, vjust = 1.1, angle = 90) +
  cowplot::draw_label(expression("Offspring temperature " ( degree*C)), x = .5, y = .02)


pp_vbot <- 
  cowplot::plot_grid(pp_v, legend_bot, ncol = 1, rel_heights = c(1, .1))


pp_vside <- 
  cowplot::plot_grid(pp_v, legend_right, rel_widths = c(1, .4))


# horizontal-arranged plots -----

pp_hz <- 
  cowplot::plot_grid(p2_bar + theme(legend.position = "none"), 
    p1_bar + theme(legend.position = "none"),
    ncol = 2,
    labels = c("(A)", "(B)")) +
  cowplot::draw_label((expression("Metabolic rate (" ~ pmol ~ O[2] ~~ h ^ -1 ~~ mu*m ^ -1 * ")")), x = 0, y = 0.5, vjust = 1.1, angle = 90) +
  cowplot::draw_label(expression("Offspring temperature " ( degree*C)), x = .5, y = 0.02)

# horizontally spaced plots, with legend
pp_hzside <- 
  cowplot::plot_grid(pp_hz, legend_right, rel_widths = c(1, .3))

pp_hzbot <- 
  cowplot::plot_grid(pp_hz, legend_bot, ncol = 1, rel_heights = c(1, .1))

## previews -----

# pp_vbot # vertical-arranged plots with bottom legend
# pp_vside # vertical-arranged plots with side legend
# pp_hzbot # horizontal-arranged plots with bottom legend
pp_hzside # horizontal-arranged plots with side legend SELECTED



# Custom colours -----

# only if asked!


# Export -----
# cowplot::save_plot(
#   "figs/ch_larval/larvalrates_vbot.png", pp_vbot,
#   base_height = 12, base_width = 9
# )
# cowplot::save_plot(
#   "figs/ch_larval/larvalrates_vside.png", pp_vside,
#   base_height = 12, base_width = 8
# )
# cowplot::save_plot(
#   "figs/ch_larval/larvalrates_hzbot.png", pp_hzbot,
#   base_height = 6, base_width = 12
# )
cowplot::save_plot(
  "figs/ch_larval/larvalrates_hzside.png", pp_hzside,
  base_height = 6, base_width = 12
)

