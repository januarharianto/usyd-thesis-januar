# Notes ----

## Respiration rate analysis for experiment conducted at SIMS in 2013.

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
  car,
  lme4,
  lmerTest,
  emmeans
)

# Load data ----

dt <- read_excel("input/ch_warming/resp_2013.xlsx") %>%
  dplyr::select(treatment, group, metabolicRate) %>%
  mutate(treatment = as.factor(treatment), group = as.factor(group)) %>%
  print()


# Manipulate data ----

## Check mean values
summ <- dt %>%
  group_by(treatment) %>%
  summarise(mean = mean(metabolicRate), sd = sd(metabolicRate)) %>%
  print()


## Calculate Q10



# Plotting ----

# First prep
## This lists the factors in the dataset.
factors <- levels(factor(dt$treatment)) # list factors
# from boxplot, list stats from 5th row (thats the max whisker value)
wupper <- boxplot(metabolicRate ~ treatment, data = dt)$stats[5, ]
# create table with the 2 lists
(letterPos <- tibble(factors, wupper))
# add the letters to the list
letterPos$group <- c("ab", "a", "a", "b")

resp_plot <- ggplot(dt, aes(factor(treatment), metabolicRate)) +
  geom_boxplot(
    fatten = 1,
    alpha = .6,
    outlier.size = 2,
    outlier.alpha = .8
  ) +
  geom_text(data = letterPos,
    aes(factors, wupper + .05),
    label = letterPos$group,
    fontface = "bold") +
  ## code below adds a mean to the boxplot (dotted line)
  ## from: https://stackoverflow.com/a/40136391
  stat_summary(fun.y = median, geom = "errorbar",
    aes(ymax = ..y.., ymin = ..y.., linetype = "median"),
    width = .75) +
  stat_summary(fun.y = mean, geom = "errorbar",
    aes(ymax = ..y.., ymin = ..y.., linetype = "mean"),
    width = .75) +
  scale_linetype_manual(name = "", values = c(3,1)) +
  xlab("Treatment temperature (°C)") + # change y-axis title
  ylab(expression("Rate "~(mg~O[2]~g^{-1}~h^{-1}))) + # Change x-axis title
  geom_blank()
resp_plot

# Analysis ----

## Nested ANOVA

## Check effect of nesting
nest <-
  lmer(metabolicRate ~ treatment + (1 | group), data = dt, REML = T) %>%
  lmerTest::rand() %>%  # here we randomly drop observations to test the effect of nesting
  print()
## No effect of nesting (within groups) detected.

## Perform ANOVAs
analysis1 <-
  lmer(metabolicRate ~ treatment + (1 | group), data = dt, REML = T) %>%
  print() %>%
  anova() %>%
  print()

# pseudoreplication - pool urchins to tank
sdt <- dt %>%
  group_by(treatment, group) %>%
  summarise(mean = mean(metabolicRate))

# Nested ANOVA on pooled data
model <- lm(mean ~ treatment, data = sdt)
Anova(model)


## (not using this)
analysis2 <- lm(metabolicRate ~ treatment, data = dt) %>%
  print() %>%
  Anova() %>%
  print()

## Using results of analysis 1 (with nesting) since the results will be the same
## anyway, just fewer degrees of freedom but the P-value is pretty much equal.

## Multiple comparisons
model <- lmer(metabolicRate ~ treatment + (1 | group), data = dt, REML = T)

## Pairwise comparisons (for p values)
pairwise <- emmeans(model, "treatment", lmer.df = "satterthwaite") %>%
  pairs() %>%  # this defaults to adjust = "tukey"
  print()

## Groupings based on pairwise comparisons
groups <- emmeans(model, "treatment", lmer.df = "satterthwaite") %>%
  cld() %>%
  print()

# Export ----

cowplot::save_plot("figs/ch_warming/mr.png", resp_plot,
  base_height = 6, base_width = 6*1.3)
