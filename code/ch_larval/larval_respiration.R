library(tidyverse)
library(readxl)
dt <- 
  read_excel("input/ch_larval/larval_mR_2015.xlsx", sheet = 2, skip = 1) %>%
  group_by(parents, parent_temp, parent_ph, temp, ph) %>%
  summarise(mean = mean(mr),
    se = sd(mr)/sqrt(3)) %>%
  print()

# Plot as cowplot top-down figures

dt_lowph <- dt %>%
  filter(ph == "pH 7.6") %>%
  # group_by(parents, temp, ph) %>%
  print()

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
