library(tidyverse)
library(readxl)
dt <- 
  read_excel("input/ch_larval/larval_mR_2015.xlsx", sheet = 2, skip = 1) %>%
  group_by(parents, parent_temp, parent_ph, temp, ph) %>%
  summarise(mean = mean(mr),
    se = sd(mr)/sqrt(3)) %>%
  print()


# alternative bar plots -----
# offspring low pH
p1_bar <- 
  dt %>%
  mutate(parents = factor(parents, levels = c("0C", "0A", "2C", "2A"))) %>%
  filter(ph == "pH 7.6") %>%
  ggplot(aes(factor(temp), mean, fill = parents)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .1,
                position = position_dodge(.9)) +
  cowplot::theme_cowplot(font_size = 16) +
  scale_fill_brewer(name = "Parental conditions",
                    labels = c(
                      "+0 °C; 8.0 pH",
                      "+0 °C; 7.6 pH",
                      "+2 °C; 8.0 pH",
                      "+2 °C; 7.6 pH")) +
  xlab("") +
  ylab("") +
  geom_blank()
p1_bar

# offspring ambient pH
p2_bar <- 
  dt %>%
  mutate(parents = factor(parents, levels = c("0C", "0A", "2C", "2A"))) %>%
  filter(ph == "pH 8.0") %>%
  ggplot(aes(factor(temp), mean, fill = parents)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .1,
                position = position_dodge(.9)) +
  cowplot::theme_cowplot(font_size = 16) +
  scale_fill_brewer(name = "Parental conditions",
                    labels = c(
                      "+0 °C; 8.0 pH",
                      "+0 °C; 7.6 pH",
                      "+2 °C; 8.0 pH",
                      "+2 °C; 7.6 pH")) +
  xlab("") + # leave this blank so that it doesn't duplicate in plot_grid()
  ylab("") +
  geom_blank()
p2_bar

# Get legend out
legend_right <- cowplot::get_legend(p1_bar + 
    theme(legend.box.margin = margin(0, 0, 0, 12)))

legend_bot <- cowplot::get_legend(p1_bar +
    theme(legend.direction = "horizontal",
      legend.justification = "center" ,
      legend.box.just = "bottom"))

pp_hz <- 
  cowplot::plot_grid(p2_bar + theme(legend.position = "none"), 
  p1_bar + theme(legend.position = "none"),
  ncol = 1,
  labels = c("A - pH 8.0","B - pH 7.6"),
  label_x = .1) +
  cowplot::draw_label((expression("Metabolic rate (" ~ pmol ~ O[2] ~~ h ^ -1 ~~ mu*m ^ -1 * ")")), x = 0, y = 0.5, vjust = 1.1, angle = 90) +
  cowplot::draw_label(expression("Temperature " ( degree*C)), x = .5, y = .03)


pp_bleg <- cowplot::plot_grid(pp_hz, legend_bot, ncol = 1, rel_heights = c(1, .1))
pp_bleg

pp_sleg <- cowplot::plot_grid(pp_hz, legend_right, rel_widths = c(1, .4))
pp_sleg


pp_vert <- 
  cowplot::plot_grid(p2_bar + theme(legend.position = "none"), 
  p1_bar + theme(legend.position = "none"),
  ncol = 2,
  labels = c("A", "B")) +
  cowplot::draw_label((expression("Metabolic rate (" ~ pmol ~ O[2] ~~ h ^ -1 ~~ mu*m ^ -1 * ")")), x = 0, y = 0.5, vjust = 1.1, angle = 90) +
  cowplot::draw_label(expression("Offspring temperature " ( degree*C)), x = .5, y = .03)

# horizontally spaced plots, with legend
pp_vert_leg <- 
  cowplot::plot_grid(pp_vert, legend_right, rel_widths = c(1, .3))
  



# Plot as cowplot top-down line plots -----

dt_lowph <- dt %>%
  filter(ph == "pH 7.6") %>%
  # group_by(parents, temp, ph) %>%
  print()


# line plot
p1 <-
  ggplot(dt_lowph, aes(temp, mean, colour = factor(parent_temp))) +
  # data:
  geom_line(aes(linetype = parent_ph)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), 
                width = .1) +
  ylim(20,50) +
  # overall theme:
  cowplot::theme_cowplot() +
  scale_colour_manual(values = c("midnightblue", "orangered1")) +
  scale_linetype_manual(name = "Parental pH", 
                        labels = c("Acidic 7.6", "Ambient 8.0"),
                        values = c(1,2)) + 
  # axis labels
  xlab("Temperature (°C)") +
  ylab("") +
  # ylab((expression("Metabolic rate (" ~ pmol ~ O[2] ~~ h ^ -1 ~~ mu*m ^ -1 * ")"))) +
  geom_blank()


dt_ambph <- dt %>% 
  filter(ph == "pH 8.0") %>%
  print()

p2 <-
ggplot(dt_ambph, aes(temp, mean, colour = factor(parent_temp))) +
  # data:
  geom_line(aes(linetype = parent_ph)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), 
                width = .1) + 
  ylim(20,50) +
  # overall theme
  cowplot::theme_cowplot() +
  scale_colour_manual(name = "Parental Temp.", 
                      labels = c("+0 °C", "+2 °C"),
                      values = c("midnightblue", "orangered1")) +
  scale_linetype_manual(name = "Parental pH", 
                        labels = c("Acidic 7.6", "Ambient 8.0"),
                        values = c(1,2)) +
  theme(legend.key.size = unit(1.4,"line")) +
  # axis labels
  # xlab("Temperature (°C)") +
  xlab("") +
  ylab("") +
  # ylab((expression("Metabolic rate (" ~ pmol ~ O[2] ~~ h ^ -1 ~~ mu*m ^ -1 * ")"))) +
  geom_blank()


# Get legend out
# legend <- cowplot::get_legend(p2)
legend <- cowplot::get_legend(p2 + 
    theme(legend.direction = "horizontal",
      legend.justification = "center" ,
      legend.box.just = "bottom"))

p1p2 <- 
  cowplot::plot_grid(p2 + theme(legend.position = "none"), 
                     p1 + theme(legend.position = "none"),
                     ncol = 1,
                     labels = c("A - pH 8.0","B - pH 7.6"),
                     label_x = .1) +
  cowplot::draw_label((expression("Metabolic rate (" ~ pmol ~ O[2] ~~ h ^ -1 ~~ mu*m ^ -1 * ")")),
                      x = 0, y = 0.5, vjust = 1.1, angle = 90)
# to add italics to units  
# cowplot::draw_label((expression("Metabolic rate (" ~ italic(pmol ~ O[2] ~~ h ^ -1 ~~ mu*m ^ -1) * ")")),
  #                   x = 0, y = 0.5, vjust = 1.1, angle = 90)
# p1p2 <- 
# p1p2 <- cowplot::ggdraw(cowplot::add_sub(p1p2, "Larvae in 8.0 pH", vpadding=grid::unit(0, "lines"),
                                 # y = 59.3, x = .1, hjust = 0))

# p1p2 <- cowplot::ggdraw(cowplot::add_sub(p1p2, "Larvae in 7.6 pH", vpadding=grid::unit(0, "lines"),
#                                  y = 119.5, x = .1, hjust = 0))
# p1p2

p1p2 <- cowplot::plot_grid(p1p2, legend, ncol = 1, rel_heights = c(1, .1))
p1p2
# ANALYSE

model <- lm(mean ~ temp * ph * parents, dt)
# summary(model)
car::Anova(model)

newmodel <- lm(mean ~ temp*ph*parents - temp:ph - ph:parents - temp:ph:parents, dt)
car::Anova(newmodel)

lm(mean ~ temp*ph*parents - temp:ph - ph:parents - temp:ph:parents, dt) %>%
  car::Anova(type = 3, contrasts = list(topic=contr.sum, sys=contr.sum))


## separate parental conditions
lm(mean ~ temp * ph * parent_temp * parent_ph, dt) %>%
  car::Anova(type = 3, contrasts = list(topic=contr.sum, sys=contr.sum))

lm(mean ~ temp + ph + parent_temp + parent_ph + temp:parent_temp, dt) %>%
  car::Anova(type = 3, contrasts = list(topic=contr.sum, sys=contr.sum))


lm(mean ~ temp + ph + parent_temp + parent_ph + temp:parent_temp + ph:parent_temp + temp:parent_ph + ph:parent_ph + parent_temp:parent_ph + temp:ph, dt) %>%
  car::Anova(type = 3, contrasts = list(topic=contr.sum, sys=contr.sum))


# fix? did i forget to set parents as factors?
lm(mean ~ temp + ph + factor(parent_temp) + factor(parent_ph) + temp:factor(parent_temp) + ph:factor(parent_temp) + temp:factor(parent_ph) + ph:factor(parent_ph) + factor(parent_temp):factor(parent_ph) + temp:ph, dt) %>%
  car::Anova(type = 3, contrasts = list(topic=contr.sum, sys=contr.sum))
# results exactly the same phew!
# 
# library(emmeans)
# emmeans(model, pairwise ~ temp + parents)

# Export ----
# rm(resp_data)
cowplot::save_plot(
  "figs/ch_larval/larvalrates.png", p1p2,
  base_height = 14, base_width = 8
)


save.image(file = "output/ch_larval/larvalresp.RData")
# RColorBrewer::display.brewer.all()
