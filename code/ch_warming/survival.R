# Notes ----

## Perform survival analysis on urchin experiments at SIMS in 2013.

# Pre-processing ----

library(snipR) # my personal package
snipR::refresh()

# Load packages ----

snipR::pkgs(
  tidyverse,
  readxl,
  lubridate,
  cowplot,
  viridis,
  survival,
  survminer,
  scales
)

# Load data ----

path <- "input/ch_warming/survival_2013.xlsx"
dt <- lapply(1:4, function(x) read_excel(path, x)) %>%
  bind_rows() %>%
  mutate(treatment = factor(treatment)) %>%
  print()

# Manipulate data ----

model <- survfit(Surv(time, death) ~ treatment, conf.type = "log", data = dt) %>%
  print()

summary(model)

summs <- dt %>%
  dplyr::select(treatment, death) %>%
  group_by(treatment) %>%
  summarise(dead = as.integer(sum(death)), total = length(death)) %>%
  mutate(perc = (total - dead) / total) %>%
  print()

# Plotting ----

xsurvplot <-
  ggsurvplot(
    model,
    palette = "RdBu",
    # linetype = "strata",
    # size = .8,
    pval = T,
    pval.coord = c(0, .7),
    legend.title = "",
    legend.labs = (c("20 °C", "22 °C", "24 °C", "26 °C")),
    legend = "right",
    xlab = "Time elapsed (days)",
    ylab = "Survival Probability (%)",
    xlim = c(0,120),
    ylim = c(0.65,1),
    surv.scale = "percent",
    censor = F
  )
xsurvplot

survplot <- xsurvplot$plot +
  # ggplot2::annotate("text", x = 100, y = .958, label = "a") +
  # ggplot2::annotate("text", x = 100, y = .938, label = "a") +
  # ggplot2::annotate("text", x = 100, y = .771, label = "b") +
  # ggplot2::annotate("text", x = 100, y = .688, label = "b")
    geom_text(
      data = summs,
      aes(x = 100, y = perc, label = c("a", "a", "b", "b")),
      fontface = "bold"
    ) +
  theme(legend.text=element_text(size=rel(1))) +
  guides(color = guide_legend(override.aes = list(size = 5)))
survplot

## This stopped working but I need it to at some point.
# survplot <-
#   ggsurvplot(
#     model,
#     palette = "RdBu",
#     linetype = "strata",
#     size = .7,
#     pval = T,
#     pval.coord = c(0, .7),
#     legend.title = "",
#     legend.labs = (c("20 °C", "22 °C", "24 °C", "26 °C")),
#     legend = "right",
#     # break.x.by = 7,
#     xlab = "Time elapsed (days)",
#     ylab = "Survival Probability (%)"
#   ) +
#   # theme(legend.text=element_text(size = 13)) +
#   geom_text(
#     data = summs,
#     aes(x = 100, y = perc, label = c("a", "a", "b", "b")),
#     fontface = "bold"
#   ) +
#   scale_y_continuous(labels = scales::percent, limits = c(0.65, 1))
# survplo

# Analysis ----

## Perform Kaplan-Meier logrank test
analysis <- survdiff(Surv(time, death) ~ treatment, data = dt) %>%
  print()

## Perform pairwise contrasts
posthoc <- pairwise_survdiff(Surv(time, death) ~ treatment, data = dt,
  p.adjust.method = "bonferroni") %>%
  print()


## I want to perform Cox regression. Let's check if it fits assumptions

cox <- coxph(Surv(time, death) ~ treatment, data = dt) %>%
  print()

summary(cox)


cox_assumption <- cox.zph(cox)
cox_assumption$table[3,3] # this is the overall p value
ggcoxzph(cox.zph(cox))  # plot of the residuals.
# It's "wavy" but relatively straight indicating no time effect on covariates.

# Export ----

cowplot::ggsave(
  filename = "figs/ch_warming/survival.png",
  plot = survplot,
  height = 6, width = 6 * 1.3
)

