# Notes ----

# Acclimation schedule based on harbour temperature (check harbourtemps.R). pH
# schedule based on setpoints.

# Pre-processing ----
library(snipR) # my personal package
snipR::refresh()

# Load packages ----
snipR::pkgs(
  tidyverse,
  readxl,
  lubridate,
  scales,
  cowplot
)

# Load data ----

dt <- read_excel("input/ch_multistressor/acclimation.xlsx", 2) %>%
  print()

# Plotting ----

tempacclim_p <- ggplot(dt, aes(date, t0)) +
  # Plots
  geom_rect(data = dt[1,],
    aes(linetype = "temp adj."),
    alpha = .5,
    xmin = dt$date[1],
    xmax = dt$date[22],
    ymin = -Inf,
    ymax = Inf,
    fill = "rosybrown1"
  ) +
  geom_rect(data = dt[1,],
    aes(linetype = "pH adj."),
    alpha = .5,
    xmin = dt$date[22],
    xmax = dt$date[50],
    ymin = -Inf,
    ymax = Inf,
    fill = "slategray1"
  ) +
  geom_line(aes(colour = "+0")) +
  geom_line(aes(date, t2, colour = "+2")) +
  geom_line(aes(date, t3, colour = "+3")) +
  # geom_line(aes(colour = "Chowder Bay")) +
  geom_point(aes(date, field, shape = "Chowder Bay")) +
  geom_point(aes(date, log0, colour = "+0"), alpha = .6, size = 1) +
  geom_point(aes(date, log2, colour = "+2"), alpha = .6, size = 1) +
  geom_point(aes(date, log3, colour = "+3"), alpha = .6, size = 1) +
  # Axes (scale)
  scale_x_datetime(date_breaks = "1 month", labels = date_format("%b")) +
  scale_y_continuous(breaks = seq(12, 30, 2)) +
  # Labels (title, x & y)
  xlab("") +
  ylab(expression("Temp. "~degree * C)) +
  # Legend
  theme(
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    legend.position = "bottom"
  ) +
  # theme(legend.position = "bottom") +
  scale_colour_brewer(
    palette = "Set1",
    direction = -1,
    name = "",
    labels = c(
      expression("ambient"),
      expression("+2"~degree * C),
      expression("+3"~degree * C),
      expression("Chowder Bay")
    )
  ) +
  # scale_fill_discrete(name = "Logger", labels = "temp (field)") +
  scale_shape_discrete(name = "") +
  scale_linetype_manual(
    values = c("temp adj." = 0, "pH adj." = 0),
    breaks = c("temp adj.", "pH adj."), name = "",
    guide = guide_legend(
      override.aes = list(fill = c(
        "rosybrown1", "slategray1"
      ))
    )
  ) +
  geom_blank()
# END ggplot
tempacclim_p



# Export ----
save_plot("figs/ch_multistressor/temp_acc.png", tempacclim_p, base_height = 6, base_width = 6)
save.image("output/ch_multistressor/temp_acclim.RData")

