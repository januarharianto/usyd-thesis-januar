dt <- readxl::read_excel("input/ch_larval/larval_growth_2015.xlsx") %>%
  mutate(parent_temp = factor(parent_temp)) %>%
  mutate(parent_ph = factor(parent_ph)) %>%
  print()

# ggplot(dt, aes(f0_temp, diameter_um)) +
#   geom_boxplot()

# preparation ----
# ok, create a dummy plot just for the legend
leg_p <-
  ggplot(dt, aes(x = f0_temp, y = diameter_um)) +
  geom_boxplot(aes(fill = parent_ph)) +
  geom_line(aes(colour = as.factor(parent_temp)), size = 1.5) +
  scale_fill_manual(
    name = "Parental pH",
    values = c("white", "steelblue3"),
    breaks = c("C", "A"),
    labels = c("Control, 8.0", "Low, 7.6")
  ) +
  scale_colour_manual(
    name = "Parental °C",
    values = c("black", "red2"),
    breaks = c("0", "2"),
    labels = c("+ 0 °C", "+ 2 °C")
  ) +
  theme_cowplot() +
  theme(legend.position = "bottom") +
  theme(
    legend.direction = "horizontal",
    legend.justification = "center",
    legend.box.just = "bottom"
  ) +
  geom_blank()

leg <- get_legend(leg_p)

#create common x and y labels
y.grob <- textGrob(
  expression("Test Diameter (" *mu*m * ")"), 
  gp=gpar(fontface="plain", fontsize=15), rot=90)
x.grob <- textGrob("Offspring temperature treatment (°C)", 
  gp=gpar(fontface="plain", fontsize=15))

# make the plot ----
# plot individually
p1 <- 
  dt %>%
  filter(f0_ph == 8) %>%
  ggplot(aes(x = f0_temp, y = diameter_um, fill = parent, colour = as.factor(parent_temp))) +
  geom_boxplot(position = position_dodge(.8), size = .6, fatten = 1) +
  ggtitle("Offspring pH 8.0") +
  theme_half_open() +
  scale_fill_manual(values = c("white", "steelblue3", "white", "steelblue3"),
    guide = FALSE) +
  scale_colour_manual(values = c("black", "red2"), guide = FALSE) +
  theme(axis.title = element_blank()) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    strip.background = element_rect(colour = "black", fill = NA)
  ) +
  geom_blank()

p2 <- 
  dt %>%
  filter(f0_ph == 7.6) %>%
  ggplot(aes(x = f0_temp, y = diameter_um, fill = parent, colour = as.factor(parent_temp))) +
  geom_boxplot(position = position_dodge(.8), size = .6, fatten = 1) +
  ggtitle("Offspring pH 7.6") +
  theme_half_open() +
  scale_fill_manual(values = c("white", "steelblue3", "white", "steelblue3"),
    guide = FALSE) +
  scale_colour_manual(values = c("black", "red2"), guide = FALSE) +
  theme(axis.title = element_blank()) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    strip.background = element_rect(colour = "black", fill = NA)
  ) +
  geom_blank()

p3 <- plot_grid(p1, p2, 
  labels = c("A", "B"),label_size = 15,  
  hjust = -.5,
  vjust = 1.68)


#add to plot
p4 <- grid.arrange(arrangeGrob(p3, left = y.grob, bottom = x.grob))
p5 <- plot_grid(p4, leg,
  ncol = 1,
  rel_heights = c(10, 1), 
  align = "hv", 
  axis = "l")
p5

save_plot(
  "figs/fms/figx_larval_growth.pdf", p5,
  base_height = 6, base_width = 8
)
