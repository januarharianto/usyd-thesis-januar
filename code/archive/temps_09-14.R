# Pre-processing ----
library(snipR)
snipR::refresh()

# Load packages ----
snipR::pkgs(
  tidyverse, # simplify data manipulation
  readxl, # read ms excel files
  lubridate,
  scales
)


# Load data ----
df <- read_csv("input/ch_multistressor/temps09-14.csv") %>%
  print()

# Manipulate data ----

## I want to filter the data to include only 2009 - 2014
sdf <- df %>%
  filter(date >= "2009-12-31 23:59:59") %>%
  filter(date <= "2015-01-01 00:00:01") %>%
  filter(temp_c > 14) %>% # remove the 2 outliers
  mutate(temp_c = temp_c) %>%
  select(date, temp_c) %>%
  print()

## Summarise by day
# ssdf <- sdf %>%
#   group_by(day = yday(date)) %>%
#   summarise(mean = mean(temp_c)) %>%
#   mutate(date = ymd("2014-01-01") + days(day - 1)) %>%
#   print()


## Summarise by week
summ <- sdf %>%
  mutate(week = week(date)) %>%
  group_by(week) %>%
  summarise(
    mean = mean(temp_c),
    max = max(temp_c),
    min = min(temp_c),
    sd = sd(temp_c)
  ) %>%
  # https://stackoverflow.com/a/46183403
  mutate(date = ymd_hms("2014-01-01 00:00:00") + weeks(week - 1)) %>%
  filter(week <= 52) %>%
  print()



# Plotting ----

## Plot the summary data
harbour_p <- ggplot(summ, aes(date, mean)) +
  # Plots
  geom_errorbar(
    aes(ymin = mean - sd, ymax = mean + sd),
    width = .1
  ) +
  geom_errorbar(
    data = summ[23:29, ],
    aes(ymin = mean - sd, ymax = mean + sd, colour = "temp/pH adjustment"),
    width = .1
  ) +
  geom_errorbar(
    data = summ[30:46, ],
    aes(ymin = mean - sd, ymax = mean + sd, colour = "acclimation"),
    width = .1
  ) +
  geom_ribbon(aes(ymin = min, ymax = max), alpha = .1) +
  geom_point(
    size = 2.5
  ) +
  geom_point(
    data = summ[23:29, ],
    aes(date, mean, colour = "temp/pH adjustment"),
    size = 2.5
  ) +
  geom_point(
    data = summ[30:46, ],
    aes(date, mean, colour = "acclimation"),
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
    breaks = c("temp/pH adjustment", "acclimation")
  ) +
  theme(legend.position = c(0.8, 0.9)) +
  geom_blank()
# END ggplot
harbour_p


# Analysis ----
## No analysis here.

# RColorBrewer::display.brewer.all()

# Export ----
# rm(df,sdf,summ)
# save.image("output/ch_multistressor/harbour_p.RData")
