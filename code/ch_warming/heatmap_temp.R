# Notes ----

## Description

# Pre-processing ----

library(snipR)  # my personal package
snipR::refresh()

# Load packages ----

snipR::pkgs(
  tidyverse,
  readxl,
  lubridate,
  viridis,
  scales,
  cowplot
)




# Load data ----

dt <- read_csv("input/ch_warming/sims13-17cal.csv") %>%
  print()

# Manipulate data ----

## summarise data for tile plot:

daily <- dt %>%
mutate(day = yday(time), year = year(time)) %>%
  group_by(year, day) %>%
  summarise(mean = mean(temp)) %>%
  mutate(date = ymd_hms("2013-01-01 00:00:00") + days(day - 1)) %>%
  print()

## Check temperature thresholds
dt %>%
  dplyr::filter(temp > 25.3) %>%
  print()

# Plotting ----

heatmap <-
  ggplot(daily, aes(date, year)) +
  # Plots
  geom_tile(aes(fill = mean), colour = "black", size = .01) +
  # Theme
  scale_fill_viridis(option = "B", name = "", limits = c(12, 26),
    breaks = seq(10, 26, by = 2)) +
  background_grid(major = "xy", minor = "none") +
  # theme(panel.grid.minor = element_line(colour = "black", size = 0.5,
  #   linetype = 3)) +
  # Axes (scale)
  scale_x_datetime(date_breaks = "1 month", labels = date_format("%m"),
    expand = c(0, 0)) +
  # theme(plot.margin=grid::unit(c(0,0,0,0), "mm")) +
  scale_y_continuous(expand = c(0, 0)) +
  # Labels (title, x & y)
  xlab("Month") +
  ylab("") +
  # legend
  theme(legend.position = "bottom") +
  guides(fill = guide_colorbar(barwidth = 20, barheight = 1)) +
  geom_blank()
# END ggplot
heatmap

# Lower resolution:
heatmap <-
  ggplot(daily, aes(date, year)) +
  # Plots
  geom_tile(aes(fill = mean), colour = "black", size = .01) +
  # Theme
  scale_fill_viridis(option = "B", name = "", limits = c(12, 26),
    breaks = seq(10, 26, by = 2)) +
  # background_grid(major = "xy", minor = "none") +
  # theme(panel.grid.minor = element_line(colour = "black", size = 0.5,
  #   linetype = 3)) +
  # Axes (scale)
  scale_x_datetime(date_breaks = "1 month", labels = date_format("%m"),
    expand = c(0, 0)) +
  # theme(plot.margin=grid::unit(c(0,0,0,0), "mm")) +
  scale_y_continuous(expand = c(0, 0)) +
  # Labels (title, x & y)
  xlab("Month") +
  ylab("") +
  # legend
  theme(legend.position = "bottom") +
  guides(fill = guide_colorbar(barwidth = 20, barheight = 1)) +
  geom_blank()
# END ggplot
heatmap



# Analysis ----


# Export ----

cowplot::save_plot("figs/ch_warming/heatmap.png", heatmap,
  base_height = 8, base_width = 6)

