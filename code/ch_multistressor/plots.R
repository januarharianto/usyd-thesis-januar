

# load packages
library(tidyverse)
library(readxl)
library(cowplot)
library(ggsci)
library(emmeans)
library(gridExtra)
library(grid)
# figure 3 respiration ----------------------------------------------------

# load data and clean
rplot_data <- read_excel("input/ch_multistressor/metabolic_rate.xlsx", sheet = 2) %>%
  filter(!is.na(rate)) %>%
  filter(week %in% c(4, 12)) %>%
  mutate(ph_fac = factor(ph_fac, levels = c("pH 8.0", "pH 7.6"))) %>%
  mutate(temp_fac = factor(temp_fac, levels = c(0, 2, 3))) %>%
  # mutate(week = factor(week, levels = c("4 weeks", "12 weeks"))) %>%
  print()

# first plot

p4 <- 
  rplot_data %>%
  filter(week == 4) %>%
  ggplot(aes(temp_fac, rate, fill = ph_fac)) +
  geom_boxplot(position = position_dodge(.8)) +
  stat_summary(
    fun = mean, geom = "errorbar", position = position_dodge(width = .8),
    aes(ymax = ..y.., ymin = ..y..), width = .75, linetype = 3, size = .5
  ) +
  cowplot::theme_cowplot() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    strip.background = element_rect(colour = "black", fill = NA)
  ) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
  scale_fill_manual(name = "", values =c("white", "steelblue2")) +
  scale_x_discrete(labels = c("Ambient\n(16.6 °C)", "+2\n(18.6 °C)", 
                              "+3\n(19.6 °C)")) +
  ggtitle("4 weeks") +
  xlab(" ") +
  ylab(expression("Metabolic Rate (" ~ mg ~ O[2] ~~ h ^ -1 ~~ kg ^ -1 * ")")) +
  ylim(4.5,12.6) +
  geom_blank()
p4



p12 <- 
  rplot_data %>%
  filter(week == 12) %>%
  ggplot(aes(temp_fac, rate, fill = ph_fac)) +
  geom_boxplot(position = position_dodge(.8)) +
  stat_summary(
    fun = mean, geom = "errorbar", position = position_dodge(width = .8),
    aes(ymax = ..y.., ymin = ..y..), width = .75, linetype = 3, size = .5
  ) +
  cowplot::theme_cowplot() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    strip.background = element_rect(colour = "black", fill = NA)
  ) +
  scale_fill_manual(name = "", values =c("white", "steelblue2")) +
  scale_x_discrete(labels = c("Ambient\n(20.7 °C)", "+2\n(22.7 °C)", 
                              "+3\n(23.7 °C)")) +
  ggtitle("12 weeks") +
  xlab("") +
  ylab("") +
  # ylab(expression("Metabolic Rate (" ~ mg ~ O[2] ~~ h ^ -1 ~~ kg ^ -1 * ")")) +
  ylim(4.5,12.6) +
  theme(
    legend.position = c(.95, 1),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  ) +
  geom_blank()
p12


legend <- get_legend(p4 + theme(
  legend.position = "bottom",
  legend.direction = "horizontal",
  legend.justification = "center",
  legend.box.just = "bottom",
  legend.key.width = unit(2, "line"),
  legend.key.height = unit(2,"line")
))

resp_plot <- cowplot::plot_grid(
  p4 + theme(legend.position="none"),
  p12,
  # legend,
  # rel_heights = c(1, .1),
  labels = c("A", "B"), label_size = 15,
  hjust = -2,
  vjust = 1.65,
  ncol = 2,
  align = "hv", axis = "l")
resp_plot

# Export ----
# rm(resp_data)
cowplot::save_plot(
  "figs/fms/fig3_resp_plot.pdf", resp_plot,
  base_height = 6, base_width = 8
)


# figure 4 feeding --------------------------------------------------------

#load data and clean
agar_dt <- read_excel("input/ch_multistressor/wetdry_correlation.xlsx", skip = 6)
correction <-
  read_excel("input/ch_multistressor/feeding.xlsx", sheet = "controls", skip = 1) %>%
  summarise(mean = mean(rate)) %>% # average the values
  unlist()

m <- fit$coefficients[2] %>% print()
c <- fit$coefficients[1] %>% print()

feeding_rate <-
  read_excel("input/ch_multistressor/feeding.xlsx", sheet = "main", 
             skip = 1) %>%
  select(treatment, temp, ph, tank, grp, specimen, consumed) %>% # select cols
  mutate(corrected = consumed - abs(correction)) %>% # apply correction
  mutate(rate = corrected / specimen) %>% # convert to rate, g/day/g
  mutate(rate_dry = rate * abs(m) + abs(c)) %>% # convert to dry, g/day/g
  arrange(desc(ph)) %>%
  mutate(ph = factor(ph, levels = c("pH 8.0","pH 7.6"))) %>%
  mutate(tank = factor(tank),
         temp = factor(temp, levels = c("amb", "med", "high")),
         ph = factor(ph, levels = c("pH 8.0", "pH 7.6"))) %>%
  group_by(treatment, temp, ph) %>%
  # summarise()
  print()
# Grab the coefficients to convert wet to dry
fit <- lm(dry ~ wet, data = agar_dt) %>%
  print()


# plot
ggplot(feeding_rate, aes(temp, rate_dry, fill = ph)) +
  geom_boxplot(position = position_dodge(0.8), alpha = .25) +
  stat_summary(
    fun.y = mean, geom = "errorbar", position = position_dodge(width = .8),
    aes(ymax = ..y.., ymin = ..y..), width = .75, linetype = 3, size = .5
  ) +
  theme_half_open() +
  panel_border(remove = FALSE, colour = "black", size = .5) +
  scale_fill_manual(values = c("white", "steelblue3")) +
  # change x axis labels:
  scale_x_discrete(labels = c("Ambient", "+2 °C", "+3 °C")) + 
  xlab("Treatment") +
  ylab((expression("Feeding Rate (" ~ g ~~ day ^ -1 ~~ kg ^ -1 * ")"))) +
  geom_blank()
  

feeding_plot <- ggplot(feeding_rate, aes(temp, rate_dry, fill = factor(ph))) +
  # facet_wrap(~ ph, strip.position = "bottom") +
  cowplot::theme_cowplot() +
  # geom_violin() +
  # geom_boxplot(alpha = .25) +
  geom_boxplot(position = position_dodge(0.8)) +
  stat_summary(
    fun.y = mean, geom = "errorbar", position = position_dodge(width = .8),
    aes(ymax = ..y.., ymin = ..y..), width = .75, linetype = 3, size = .5
  ) +
  # remove borders from facet labels:
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    strip.background = element_rect(colour = "black", fill = NA)
  ) +
  # make facet labels bold:
  # theme(strip.text = element_text(size = 13),
  #       legend.position = "none") +
  # theme(strip.background = element_blank(), strip.placement = "outside") +
  # scale_fill_brewer(palette = "Greys", labels = c("+0", "+2", "+3")) +
  scale_fill_manual(values = c("white", "steelblue2")) +
  # change x axis labels:
  scale_x_discrete(labels = c("Ambient\n(20.1 °C)", "+2\n(22.1 °C)", "+3 \n(23.1°C)")) +
  # legend title
  labs(fill = "Temp") +
  guides(fill = FALSE) +
  # legend position:
  # theme(legend.position = c(0, 0.9)) + # https://stackoverflow.com/a/35094476
  xlab(" ") +
  ylab((expression("Rate (" ~ g ~~ day ^ -1 ~~ g ^ -1 * ")"))) +
  ggtitle("Feeding Rate") +
  geom_blank()
feeding_plot

# ammonia
ammo <- read_excel("input/ch_multistressor/ammonia.xlsx", skip = 3) %>%
  print()

plot_dt <- ammo %>%
  select(treatment, tempv, phv, ammonia) %>%
  mutate(treatment = factor(treatment,
    levels = c("0C", "0A", "2C", "2A", "3C", "3A"))) %>%
  mutate(ph = factor(phv, levels = c("pH 8.0","pH 7.6"))) %>%
  mutate(temp = factor(tempv, levels = c("Ambient","+2 °C","+3 °C"))) %>%
  print()

nh_plot <- ggplot(plot_dt, aes(factor(temp), ammonia, fill = ph)) +
  cowplot::theme_cowplot() +
  geom_boxplot(position = position_dodge(0.8)) +
  stat_summary(
    fun.y = mean, geom = "errorbar", position = position_dodge(width = .8),
    aes(ymax = ..y.., ymin = ..y..), width = .75, linetype = 3, size = .5
  ) +
  # add border:
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    strip.background = element_rect(colour = "black", fill = NA)
  ) +
  scale_x_discrete(labels = c("Ambient\n(20.7 °C)", "+2\n(22.7 °C)", 
    "+3\n(23.7 °C)")) +
  scale_fill_manual(name = "", values =c("white", "steelblue2")) +
  ylab((expression("Rate ( "*mu*g ~~ kg ^ -1 ~~ h ^ -1 * ")"))) +
  xlab("") +
  ggtitle("Ammonia Excretion Rate") +
  theme(
    legend.position = c(.95, 1),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  ) +
  geom_blank()
nh_plot


feed_nh_plot <- cowplot::plot_grid(
  feeding_plot + theme(legend.position="none"),
  nh_plot,
  labels = c("A", "B"), label_size = 15,
  hjust = -2,
  vjust = 1.65,
  ncol = 2,
  align = "hv", axis = "l")
feed_nh_plot

# Export ----
# rm(resp_data)
cowplot::save_plot(
  "figs/fms/fig4_feed_nh_plot.pdf", feed_nh_plot,
  base_height = 6, base_width = 8
)

# supplementary figure ANOVA
resp_df <- read_excel("input/ch_multistressor/metabolic_rate.xlsx", sheet = 2) %>%
  filter(!is.na(rate)) %>%
  filter(week %in% c(4, 12)) %>%
  mutate(Temperature = temp_text,
    pH = ph_fac,
    Time = factor(week_text, levels = c("4 weeks", "12 weeks"))) %>%
  select(tank, Temperature, pH, Time, rate) %>%
  print()

# Do some filtering so that week 4 and 12 can be measured separately
resp_w4 <- resp_df %>% 
  filter(Time == "4 weeks") %>%
  print()

# Summarise week 4 data
resp_w4 %>%
  group_by(Temperature, pH) %>%
  summarise(mean = mean(rate))
resp_w12 <- resp_df %>% 
  filter(Time == "12 weeks") %>%
  print()

# Summarise week 12 data
resp_w12 %>%
  group_by(Temperature, pH) %>%
  summarise(mean = mean(rate),
    sd = sd(rate))
# Analyse 
wk4_lm <- lm(rate ~ Temperature + pH, data = resp_w4) 
wk12_lm <- lm(rate ~ Temperature * pH, data = resp_w12) 

#create common x and y labels
# library(grid)
library(gridExtra)
y.grob <- textGrob("Temperature : pH pairs", 
  gp=gpar(fontface="plain", fontsize=15), rot=90)
x.grob <- textGrob("Tukey-adjusted P value", 
  gp=gpar(fontface="plain", fontsize=15))

pw4 <- 
  pwpp(emmeans(wk4_lm,~Temperature:pH)) + 
  geom_vline(aes(xintercept = 0.05), linetype = 2) +
  # theme_linedraw(base_size = 15) +
  theme_half_open() +
  theme(axis.title = element_blank()) +
  scale_colour_jco() +
  ggtitle("4 weeks")

pw12 <- 
  pwpp(emmeans(wk12_lm,~Temperature:pH)) + 
  geom_vline(aes(xintercept = 0.05), linetype = 2) +
  # theme_linedraw(base_size = 15) +
  theme_half_open() +
  theme(axis.title = element_blank()) +
  scale_colour_jco() +
  ggtitle("12 weeks")



ptukey <- grid.arrange(arrangeGrob(
  plot_grid(
    pw4, pw12,
    align = "hv", nrow = 2, labels = c("A", "B"), label_size = 15,
    hjust = -4, vjust = 1.68
  ),
  left = y.grob, bottom = x.grob
))

save_plot(
  "figs/fms/suppfig2_Tukeys.pdf", ptukey,
  base_height = 6, base_width = 8
)

