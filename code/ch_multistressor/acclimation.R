# Notes ----

# Acclimation schedule based on harbour temperature (check harbourtemps.R). pH
# schedule based on setpoints.

# Load packages ----
library(tidyverse)
library(readxl)
library(lubridate)
library(scales)
library(cowplot)
# Load data ----

dt <- read_excel("input/ch_multistressor/acclimation.xlsx", 2) %>%
  print()
dt

# tidy raw data ----
ndt <- dt %>%
  select(week, log0:log3, c1:a3) %>%
  rename_with(.fn = ~paste0("temp_", .x), .cols = 2:4) %>%
  rename_with(.fn = ~paste0("ph_", .x), .cols = c1:a3) %>%
  print()

ndt %>%
  select(1:4) %>%
  pivot_longer(
    !week,
    names_to = c(".value", "treatment"),
    names_sep = "_"
  ) %>%
  ggplot(aes(week, temp, fill = treatment)) +
  geom_violin()


# mtcars %>% rename_with(.fn = ~ paste0("Myprefix_", .x, "_Mypostfix")) 

# summarise data, then  tidy ----
summ_dt <- 
  dt %>%
  select(week, log0, log2, log3, c1, c2, c3, a1, a2, a3) %>%
  group_by(week) %>%
  summarise(
    mean_lg0 = mean(log0, na.rm = TRUE), sd_lg0 = sd(log0, na.rm = TRUE),
    mean_lg2 = mean(log3, na.rm = TRUE), sd_lg2 = sd(log3, na.rm = TRUE),
    mean_lg3 = mean(log3, na.rm = TRUE), sd_lg3 = sd(log3, na.rm = TRUE),
    mean_cph1 = mean(c1, na.rm = TRUE), sd_cph1 = sd(c1, na.rm = TRUE),
    mean_cph2 = mean(c2, na.rm = TRUE), sd_cph2 = sd(c2, na.rm = TRUE),
    mean_cph3 = mean(c3, na.rm = TRUE), sd_cph3 = sd(c3, na.rm = TRUE),
    mean_aph1 = mean(a1, na.rm = TRUE), sd_aph1 = sd(a1, na.rm = TRUE),
    mean_aph2 = mean(a2, na.rm = TRUE), sd_aph2 = sd(a2, na.rm = TRUE),
    mean_aph3 = mean(a3, na.rm = TRUE), sd_aph3 = sd(a3, na.rm = TRUE)
    ) %>% 
  print()

#  field data only
field_dt <- 
  dt %>% 
  select(week, t0) %>%
  group_by(week) %>%
  summarise(mean = mean(t0))


summ_dt %>%
  select(week:sd_lg3) %>%
  rename(
    "+0 °C" = mean_lg0,
    "+2 °C" = mean_lg2,
    "+3 °C" = mean_lg3,
    "sd0" = sd_lg0,
    "sd2" = sd_lg2,
    "sd3" = sd_lg3
    ) %>%
  mutate(across(2:7, round, 2)) %>%
  write_csv("tables/temperatures.csv")

# ph
dt %>% 
  select(week, c1:a3) %>%
  rename(c_1 = c1, c_2 = c2, c_3 = c3,
    a_1 = a1, a_2 = a2, a_3 = a3) %>%
  pivot_longer(
    !week,
    names_to = c(".value", "treatmemt"),
    names_sep = "_"
  ) %>%
  # group_by(week) %>%
  summarise(
    mean_c = mean(c, na.rm = TRUE),
    sd_c = sd(c, na.rm = TRUE),
    mean_a = mean(a, na.rm = TRUE),
    sd_a = sd(a, na.rm = TRUE)
    ) %>%
  mutate(across(1:4, round, 2)) %>%
  write_csv("tables/ph.csv")






# tidy data
# summ_dt %>%
#   select(week:sd_lg3) %>%
#   pivot_longer(
#     !week,
#     names_to = c(".value", "treatment"),
#     names_sep = "_"
#   ) 
# 
# summ_dt %>%
#   select(week, mean_cph1:sd_aph3) %>%
#   pivot_longer(
#     !week,
#     names_to = c(".value", "treatment"),
#     names_sep = "_"
#   ) 


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
  theme_half_open() +
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

