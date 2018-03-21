# Notes ----

## Description

# Pre-processing ----
library(snipR) # my personal package
snipR::refresh()

# Load packages ----
snipR::pkgs(
  tidyverse,
  readxl,
  scales,
  cowplot
)

# Load data ----

dt <- read_excel("input/ch_multistressor/acclimation.xlsx", 2) %>%
  select(date, c, a, c1, c2, c3, a1, a2, a3) %>%
  mutate_at(3:8, funs(as.numeric)) %>%
  print()

# Plotting ----

phacclim_p <- ggplot(dt) +
  # Plots
  ## template
  geom_rect(
    data = dt[1, ],
    aes(linetype = "temp adj."),
    alpha = .5,
    xmin = dt$date[1],
    xmax = dt$date[22],
    ymin = -Inf,
    ymax = Inf,
    fill = "rosybrown1"
  ) +
  geom_rect(
    data = dt[1, ],
    aes(linetype = "pH adj."),
    alpha = .5,
    xmin = dt$date[22],
    xmax = dt$date[50],
    ymin = -Inf,
    ymax = Inf,
    fill = "slategray1"
  ) +
  geom_line(aes(date, c)) +
  geom_line(aes(date, a)) +
  ## non-acidified
  geom_point(aes(date, c1, colour = "0", shape = "8.0 (ambient)")) +
  geom_point(aes(date, c2, colour = "2", shape = "8.0 (ambient)")) +
  geom_point(aes(date, c3, colour = "3", shape = "8.0 (ambient)")) +
  ## acidified
  geom_point(aes(date, a1, colour = "0", shape = "7.6 (acidic)")) +
  geom_point(aes(date, a2, colour = "2", shape = "7.6 (acidic)")) +
  geom_point(aes(date, a3, colour = "3", shape = "7.6 (acidic)")) +
  ## extra point for chowder bay (in temp plot, not here)
  # geom_point(aes(colour = "Chowder Bay")) +
  # Theme
  cowplot::theme_cowplot() +
  # Axes (scale)
  # scale_y_continuous(limits = c(7.5, 8.1)) +
  scale_x_datetime(date_breaks = "1 month", labels = date_format("%b")) +
  # Labels (title, x & y)
  xlab("") +
  ylab("pH") +
  # Legend
  scale_colour_brewer(
    palette = "Dark2",
    name = "Temp:",
    labels = c(
      expression("+0"~degree * C),
      expression("+2"~degree * C),
      expression("+3"~degree * C),
      expression("Chowder Bay")
    )
  ) +
  scale_shape_discrete(name = "pH:") +
  scale_linetype_manual(
    values = c("temp adj." = 0, "pH adj." = 0),
    breaks = c("temp adj.", "pH adj."), name = "",
    guide = guide_legend(
      override.aes = list(fill = c(
        "rosybrown1", "slategray1"
      ))
    )
  ) +
  theme(
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    legend.position = "bottom"
  ) +
  geom_blank()
# END ggplot
phacclim_p


## Create AB plot

load("output/ch_multistressor/temp_acclim.RData")
acclim <- plot_grid(tempacclim_p, phacclim_p, ncol = 1, labels = "AUTO")
acclim

# Export ----

save_plot(
  "figs/ch_multistressor/ph_acc.png", phacclim_p,
  base_height = 6, base_width = 6 * 1.3
)

save_plot(
  "figs/ch_multistressor/acclim.png", acclim,
  base_height = 12, base_width = 8
)

rm(acclim)
save.image("output/ch_multistressor/ph_acclim.RData")
