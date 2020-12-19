library(tidyverse)
library(readxl)
library(cowplot)
library(viridis)


df <- read_excel("input/ch_larval/larval_mR_2015.xlsx", sheet = 2, skip = 1) %>%
  mutate(temp = factor(temp), 
         ph = factor(ph, levels = c("pH 8.0", "pH 7.6")),
         parent_temp = factor(parent_temp),
         parents = factor(parents, levels = c("0C", "0A", "2C", "2A")),
         parent_ph = factor(parent_ph, levels = c("C", "A")))

# plot
# group by parent pH ------------------------------------------------------

# define new labels
facet_label <- c("Offspring pH 8.0", "Offspring pH 7.6")
names(facet_label) <- c("pH 8.0", "pH 7.6")

# create dummy plot to extract legend
dp <- ggplot(df, aes(ph, mr, fill = ph, linetype = parent_ph)) + 
  scale_linetype_discrete(name = "Parent temp.:",
                          labels = c("Ambient +0 °C", "Elevated +2 °C")) +
  scale_fill_manual(name = "Parent pH:",
                    values = c("white", "steelblue2"),
                      labels = c("pH 8.0", "pH 7.6")) +
  geom_boxplot() +
  theme_cowplot() +
  theme(legend.position="bottom") +
  geom_blank() 

legend <- get_legend(dp+ theme(legend.direction = "horizontal",legend.justification="center" ,legend.box.just = "bottom"))

juvplot <- 
  ggplot(df, aes(temp, mr, fill = parents, colour = parent_ph)) +
  facet_wrap(~ ph, labeller = labeller(ph = facet_label)) +
  geom_boxplot(
    position = position_dodge(.8)
  ) +
  stat_summary(
    fun = mean, geom = "errorbar", position = position_dodge(width = .8),
    aes(ymax = ..y.., ymin = ..y..), width = .75, linetype = 3, size = .5
  ) +
  stat_summary(
    fun = median, geom = "errorbar", position = position_dodge(width = .8),
    aes(ymax = ..y.., ymin = ..y..), width = .75, linetype = 1, size = 1
  ) +
  cowplot::theme_cowplot() +
  theme(
    strip.background = element_rect(colour = "black", fill = NA)
  ) +
  scale_fill_manual(name = "", values =c("white", "steelblue2", "white", "steelblue2")) +
  # scale_linetype_manual(values = c(5, 1)) +
  scale_colour_manual(values = c("black","red3")) +
  xlab(" Offspring temp. (°C)") +
  ylab(expression("Metabolic rate (" ~ pmol ~ O[2] ~~ h ^ -1 ~~ mu*m ^ -1 * ")")) +
  geom_blank()
juvplot


jplot <- plot_grid(
  juvplot + theme(legend.position="none"),
  legend,
  rel_heights = c(10,1),
  # hjust = -6,
  ncol = 1,
  # rel_widths = c(5, 1),
  align = "hv", axis = "l")
jplot


save_plot(
  "figs/ch_larval/juvrates.pdf", jplot,
  base_height = 10, base_width = 12
)
