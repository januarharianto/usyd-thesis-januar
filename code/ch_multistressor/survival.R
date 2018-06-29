# Survival 2014 data. This is the survival data for urchins in the temp X pH
# experiment in 2014. The data collection started in August and ended in
# December before feeding experiments started.

# Pre-processing
library(snipR)
refresh()


# Load required packages
pkgs(
  tidyverse,
  readxl,
  survival,
  survminer,
  texreg
)


# Load data ----
# Because data are in separate tabls, use lapply to process all of them.
mydata <- lapply(1:6, function(x)
  read_excel("input/ch_multistressor/survival.xlsx", sheet = x))


# Manipulate data ----
# Bind to a single data frame.
survdata <- mydata %>%
  bind_rows() %>%
  print()


# Visualise data ----

# Create model for survival.
s <- survfit(Surv(time, death) ~ temp + pH, data = survdata) %>%
  print()
surv_summary(s)


# Plot the graph (kaplan-meier)
init_plot <- ggsurvplot(s, size = .75,
  surv.scale = "percent",
  # pval = TRUE,
  # pval.coord = c(1,.4),
  ## censoring
  censor.shape = "|",
  censor.size = 4,
  ## legend
  # legend = "bottom",
  legend.title = "",
  legend.labs = c("0A", "0C", "2A", "2C", "3A", "3C"),
  ## x and y axis
  xlab = "Days",
  ylab = "Survival",
  xlim = c(0,180),
  break.time.by = 20
  # surv.median.line = "h"
  ## colours
  # palette = "npg" # "npg", "aaas", "lancet", "jco", "ucscgb", "uchicago", "simpsons" and "rickandmorty"
)
init_plot

s1 <- 4
# Add on to the plot!
surv_plot <- init_plot$plot +
  ggplot2::annotate("text", x = 170, y = .933, label = "A  (0A)", size = s1) +
  ggplot2::annotate("text", x = 170, y = .967, label = "A  (0C)", size = s1) +
  ggplot2::annotate("text", x = 170, y = .833, label = "A  (2A)", size = s1) +
  ggplot2::annotate("text", x = 170, y = .900, label = "A  (2C)", size = s1) +
  ggplot2::annotate("text", x = 170, y = .459, label = "B  (3C)", size = s1) +
  ggplot2::annotate("text", x = 170, y = .376, label = "B  (3A)", size = s1) +
  ggsci::scale_color_npg(
    labels = c(
      expression(bold("0A") ~ "(+0"~degree * C * ", pH 7.6)"),
      expression(bold("0C") ~ "(+0"~degree * C * ", pH 8.0)"),
      expression(bold("2A") ~ "(+2"~degree * C * ", pH 7.6)"),
      expression(bold("2C") ~ "(+2"~degree * C * ", pH 8.0)"),
      expression(bold("3A") ~ "(+3"~degree * C * ", pH 7.6)"),
      expression(bold("3C") ~ "(+3"~degree * C * ", pH 8.0)")
    )
  ) +
  geom_hline(yintercept = .5, linetype = 2) +
  cowplot::theme_cowplot() +
  theme(legend.position = "bottom")
surv_plot



# Analyse data ----
## We hav the model "s" from above
## Check if significant:
logrank <- survdiff(Surv(time, death) ~ temp + pH, data = survdata) %>%
  print()

## Pairwise differences (with BH correction):
pairwise <- pairwise_survdiff(Surv(time, death) ~ temp + pH,
                              data = survdata) %>%
  print()

## Convert into something visual:
symnum(pairwise$p.value, cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 0.1, 1),
  symbols = c("****", "***", "**", "*", "+", " "),
  abbr.colnames = FALSE, na = "")

## use cox to perform stats
# cox <- coxph(Surv(time, death) ~ factor(temp) + pH, data = survdata) %>%
#   print()
# summ <- summary(cox) %>%
#   print()

cox <- coxph(Surv(time, death) ~ temp + pH, data = survdata) %>%
  print()
summ <- summary(cox)

coxtable <- summ$coefficients %>%
  as.data.frame() %>%
  select(
    `$\\beta$` = coef,
    `exp($\\beta$)` = `exp(coef)`,
    `se($\\beta$)` = `se(coef)`,
    z,
    `*p*` = `Pr(>|z|)`)



## significant effects detected
cox_interact <- coxph(Surv(time, death) ~ temp * pH, data = survdata) %>%
  print()
summary(cox_interact)
# model cox_interact is discarded as there are no interactions between temp and pH

## alternative check for interactions
lm(Surv(time, death) ~ temp * pH, data = survdata)

# library(texreg)
screenreg(list(cox_interact, cox))

## check assumptions
cox_assumption <- cox.zph(cox)
cox_assumption$table[3,3] # this is the overall p value

# plot of the residuals. it's "wavy" but relatively straight indicating no time
# effect on the covariates.
ggcoxzph(cox.zph(cox))

# Export ----
cowplot::save_plot(
  "figs/ch_multistressor/surv_plot.png", surv_plot,
  base_height = 8, base_width = 8
)
save.image(file = "output/ch_multistressor/survival.RData")
