# Pre-processing ----
library(snipR) # my personal package
snipR::refresh()

# Load packages ----
snipR::pkgs(
  tidyverse,
  readxl,
  car,
  emmeans
)

# Load data ----

resp_data <- read_excel("input/ch_multistressor/metabolic_rate.xlsx", sheet = 2) %>%
  filter(!is.na(rate)) %>%
  filter(week %in% c(4, 12)) %>%
  # we convert these to factors so that the model runs an ANOVA, not an ANCOVA:
  mutate(
    Temperature = factor(temp_fac),
    pH = factor(ph_fac),
    Time = factor(week),
    rate
  ) %>%
  select(tank, Temperature, pH, Time, rate) %>%
  print()

# Analyse ----

# Perform a three-way analysis of variance using temperature (+0, +2, +3), pH
# (8.0, 7.6) and time (4 weeks, 12 weeks) as fixed factors.
resp_anova <- lm(rate ~ Temperature * pH * Time, data = resp_data) %>%
  Anova(type = 3, contrasts=list(topic=contr.sum, sys=contr.sum))

lm(rate ~ Temperature*pH*Time - Temperature:pH:Time, data = resp_data) %>%
  Anova(type = 3, contrasts=list(topic=contr.sum, sys=contr.sum))

## Run ANOVA
model <- lm(rate ~ Temperature*pH*Time - Temperature:pH:Time, data = resp_data)
Anova(model, type = 3, contrasts=list(topic=contr.sum, sys=contr.sum))

## Post Hoc
## include all
emmeans(model, ~ Temperature:pH:Time) %>%
  cld(alpha = 0.05, Letters = letters) %>%
  print()

## separate time
emmeans(model, pairwise ~ Temperature:pH | Time) %>%
  cld()


# PLOTS ------------------------------------------------------------------------

# load data
rplot_data <- read_excel("input/ch_multistressor/metabolic_rate.xlsx", sheet = 2) %>%
  filter(!is.na(rate)) %>%
  filter(week %in% c(4, 12)) %>%
  mutate(ph_fac = factor(ph_fac, levels = c("pH 8.0", "pH 7.6"))) %>%
  # mutate(week = factor(week, levels = c("4 weeks", "12 weeks"))) %>%
  print()

resp_plot <- ggplot(rplot_data, aes(factor(temp_fac), rate, fill = factor(ph_fac))) +
  # facet_wrap(~ifelse(week, c("4 weeks","12 weeks"))) +
  facet_wrap(~week,
    labeller = labeller(week = c(`4` = "4 weeks", `12` = "12 weeks"))) +
  cowplot::theme_cowplot() +
  # create boxplot and add a bit of space between objects:
  geom_boxplot(position = position_dodge(0.8), outlier.alpha = .5) +
  stat_summary(
    fun.y = mean, geom = "errorbar", position = position_dodge(width = .8),
    aes(ymax = ..y.., ymin = ..y..), width = .75, linetype = 3, size = .5
  ) +
  # remove borders from facet labels:
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = .5),
    strip.background = element_rect(colour = "black", fill = NA)
  ) +
  # make facet labels bold:
  theme(strip.text = element_text(face = "bold"), legend.position = "bottom") +
  scale_fill_manual(labels = c("pH 8.0", "pH 7.6"), values = c("white", "lightgrey")) +
  # ggsci::scale_fill_jco(labels = c("4 weeks", "12 weeks")) +
  # change x axis labels:
  scale_x_discrete(labels = c(
    "Ambient", expression("+2"~degree~C),
    expression("+3"~degree~C)
  )) +
  # legend title
  labs(fill = "") +
  xlab(" ") +
  ylab((expression("Metabolic Rate (" ~ mg ~ O[2] ~~ h ^ -1 ~~ kg ^ -1 * ")"))) +
  geom_blank()
resp_plot


# plot without facets
## first prep the data - filter to 4 or 12 weeks
wks4 <- rplot_data %>%
  dplyr::filter(week == 4) %>% print()

wks12 <- rplot_data %>%
  dplyr::filter(week == 12) %>% print()


# generate the 4-week plot
p4 <- ggplot(wks4, aes(factor(temp_fac), rate, fill = factor(ph_fac))) +
  geom_boxplot(position = position_dodge(.8), alpha = .25) +
  stat_summary(
    fun.y = mean, geom = "errorbar", position = position_dodge(width = .8),
    aes(ymax = ..y.., ymin = ..y..), width = .75, linetype = 3, size = .5
  ) +
  cowplot::theme_cowplot() +
  # scale_fill_brewer(name = "", palette = "BrBG", direction = -1) +
  scale_fill_manual(name = "", values =c("white", "grey")) +
  # scale_x_discrete(labels = c("Ambient", expression("+2"~degree~C),
  #   expression("+3"~degree~C))) +
  scale_x_discrete(labels = c("Ambient\n(16.6 °C)", "+2\n(18.6 °C)", 
    "+3\n(19.6 °C)")) +
  ggtitle("4 weeks") +
  xlab(" ") +
  ylab(expression("Metabolic Rate (" ~ mg ~ O[2] ~~ h ^ -1 ~~ kg ^ -1 * ")")) +
  geom_blank()
p4

p12 <- ggplot(wks12, aes(factor(temp_fac), rate, fill = factor(ph_fac))) +
  geom_boxplot(position = position_dodge(.8), alpha = .25) +
  stat_summary(
    fun.y = mean, geom = "errorbar", position = position_dodge(width = .8),
    aes(ymax = ..y.., ymin = ..y..), width = .75, linetype = 3, size = .5
  ) +
  cowplot::theme_cowplot() +
  # scale_fill_brewer(name = "", palette = "Greys", direction = -1) +
  scale_fill_manual(name = "", values =c("white", "grey")) +
  # scale_x_discrete(labels = c("Ambient", expression("+2"~degree~C),
  #   expression("+3"~degree~C))) +
  scale_x_discrete(labels = c("Ambient\n(20.7 °C)", "+2\n(22.7 °C)", 
    "+3\n(23.7 °C)")) +
  ggtitle("12 weeks") +
  xlab("") +
  ylab(expression("Metabolic Rate (" ~ mg ~ O[2] ~~ h ^ -1 ~~ kg ^ -1 * ")")) +
  geom_blank()


legend <- cowplot::get_legend(p4 + theme(legend.position="bottom"))
resp_plot <- cowplot::plot_grid(
  p4 + theme(legend.position="none"),
  p12 + theme(legend.position="none"),
  legend,
  rel_heights = c(1, 1, .1),
  labels = c("A", "B"),
  # hjust = -6,
  ncol = 1,
  align = "hv", axis = "l")
resp_plot

# Export ----
# rm(resp_data)
cowplot::save_plot(
  "figs/ch_multistressor/resp_plot.png", resp_plot,
  base_height = 12, base_width = 8
)

save.image(file = "output/ch_multistressor/respiration.RData")
# RColorBrewer::display.brewer.all()
