# Notes ----

# Weekly temperature averaged from 2009, 2010, 2013 and 2014. Data for 2011-12
# are not available.

# Pre-processing ----
library(snipR) # my personal package
snipR::refresh()

# Load packages ----
snipR::pkgs(
  tidyverse,
  readxl,
  lubridate,
  scales
)

# Load data ----

dt <- read_csv("input/ch_multistressor/cbtemp_2009-16.csv") %>%
  mutate(date = ymd_hms(time)) %>% # convert to proper datetime
  filter(date < "2015-01-01 00:00:00") %>% # grab only 2014 data and earlier
  filter(!between(date,
                  ymd_hms("2010-01-01 00:00:00"),
                  ymd_hms("2010-03-10 00:00:00"))) %>%
  filter(!is.na(temp)) %>%
  mutate(week = week(date)) %>%
  select(date, temp, week) %>%
  group_by(week) %>%
  summarise(
    mean = mean(temp),
    sd = sd(temp),
    max = max(temp),
    min = min(temp)
  ) %>%
  mutate(date = ymd_hms("2014-01-01 00:00:00") + weeks(week - 1)) %>%
  select(date, week, mean, sd, max, min) %>%
  filter(week < 53) %>%
  print()

# Plotting ----

harbour_plot <- ggplot(dt, aes(date, mean)) +
  # Plots
  geom_ribbon(aes(ymax = max, ymin = min), alpha = .2, fill = "grey") +
  geom_errorbar(aes(ymax = mean + sd, ymin = mean - sd), width = .2) +
  geom_errorbar(
    data = dt[1:2, ],
    width = .2,
    aes(
      ymax = mean + sd,
      ymin = mean - sd,
      colour = "long-term\nacclimation"
    )
  ) +
  geom_errorbar(
    data = dt[26:32, ],
    width = .2,
    aes(
      ymax = mean + sd,
      ymin = mean - sd,
      colour = "temp/pH\nadjustment"
    )
  ) +
  geom_errorbar(
    data = dt[33:52, ],
    width = .2,
    aes(
      ymax = mean + sd,
      ymin = mean - sd,
      colour = "long-term\nacclimation"
    )
  ) +
  geom_point(size = 2.5) +
  geom_point(
    data = dt[1:2, ], aes(colour = "long-term\nacclimation"),
    size = 2.5
  ) +
  geom_point(
    data = dt[26:32, ], aes(colour = "temp/pH\nadjustment"),
    size = 2.5
  ) +
  geom_point(
    data = dt[33:52, ], aes(colour = "long-term\nacclimation"),
    size = 2.5
  ) +
  # Theme
  cowplot::theme_cowplot() +
  # Axes (scale)
  scale_x_datetime(date_breaks = "1 month", labels = date_format("%b")) +
  scale_y_continuous(breaks = seq(12, 30, 1)) +
  # Labels (title, x & y)
  xlab("") +
  ylab(expression("Mean weekly temperature"~degree~C)) +
  # Legend
  scale_colour_brewer(
    palette = "Set1", name = "Exp. period",
    breaks = c("temp/pH adjustment", "long-term acclimation")
  ) +
  theme(legend.position = "bottom") +
  theme(legend.key.size = unit(2, "lines")) +
  geom_blank()
# END ggplot
harbour_plot

# Export ----

cowplot::save_plot(
  "figs/ch_multistressor/harbour.png", harbour_plot,
  base_height = 8, base_width = 8
)

rm(dt)
save.image("output/ch_multistressor/harbourtemps.RData")
