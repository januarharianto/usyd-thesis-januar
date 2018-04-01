# Pre-processing
library(snipR)
snipR::refresh()

# Load packages
library(tidyverse)
library(readxl)

# Generate linear relationship between the weight of wet and dry agar diet ----
# This is used for assimilation efficiency calculations only.

# Load data
agar_dt <- read_excel("input/ch_multistressor/wetdry_correlation.xlsx", skip = 6) %>%
  print()

# Plot (just to visualise)
ggplot(agar_dt, aes(dry, wet)) +
  geom_point() +
  geom_smooth(method = "lm")

# Grab the coefficients to convert wet to dry
fit <- lm(dry ~ wet, data = agar_dt) %>%
  print()
m <- fit$coefficients[2] %>% print()
c <- fit$coefficients[1] %>% print()


# Calculations for feeding rate ----

# Get mean value from controls to correct main experiment
correction <-
  read_excel("input/ch_multistressor/feeding.xlsx", sheet = "controls", skip = 1) %>%
  summarise(mean = mean(rate)) %>% # average the values
  unlist() %>% # then, convert to vector
  print()

# Apply correction to consumed, and then calculate rate. Also, calculate dry
# feeding rate.
feeding_rate <-
  read_excel("input/ch_multistressor/feeding.xlsx", sheet = "main", 
    skip = 1) %>%
  select(treatment, temp, ph, tank, grp, specimen, consumed) %>% # select cols
  mutate(corrected = consumed - abs(correction)) %>% # apply correction
  mutate(rate = corrected / specimen) %>% # convert to rate, g/day/g
  mutate(rate_dry = rate * abs(m) + abs(c)) %>% # convert to dry, g/day/g
  arrange(desc(ph)) %>%
  mutate(ph = factor(ph, levels = c("pH 8.0","pH 7.6"))) %>%
  mutate(tank = factor(tank),
    temp = factor(temp, levels = c("amb", "med", "high")),
    ph = factor(ph, levels = c("pH 8.0", "pH 7.6"))) %>%
  group_by(treatment, temp, ph) %>%
  # summarise()
  print()

# ANALYSE DATA -----------------------------------------------------------------
## Select required columns from dataset for easier ID and analysis.
fd <- feeding_rate %>%
  select(temp, ph, tank, grp, rate = rate_dry) %>%
  filter(grp == 2) %>%
  select(-grp) %>%
  print()

## Generate linear mixed model
mxmodel <- lmer(rate ~ temp*ph + (1|tank) + (temp|tank) + (ph|tank), 
  data = fd) %>% 
  print()
Anova(mxmodel)

## Update to remove interaction since it is not significant
mxmodel <- lmer(rate ~ temp*ph - temp:ph + (1|tank) + (temp|tank) + (ph|tank), 
  data = fd) %>% 
  print()
Anova(mxmodel)
## Test for random effects:
rand(mxmodel)
## No effect of tank, temp|tank or ph|tank
## Remove the random

## Update to switch to usual linear model
model <- lm(rate ~ temp*ph - temp:ph, data = fd) %>% print()
Anova(model)
## Summary:
## Significant effect of temp, but not pH.

## Ok let's do multiple pairwise comparisons
## Tukey HSD
library(emmeans)
emmeans(model, ~temp:ph)

hsd <- emmeans(model, pairwise~temp|ph) # use this since we have no effect of pH
hsd
xtable::xtable(hsd$emmeans)
pander::pander(hsd$emmeans)

grps <- cld(hsd)
grps


## check assumptions
hist(residuals(model))  # looks normal
shapiro.test(feeding_rate$rate_dry) # test for normality
plot(fitted(model), residuals(model)) # residuals look ok
bartlett.test(rate_dry ~ interaction(temp, ph), feeding_rate) # pass
fligner.test(rate_dry ~ interaction(temp, ph), feeding_rate) # pass

# PLOT -------------------------------------------------------------------------
feeding_plot <- ggplot(feeding_rate, aes(temp, rate_dry)) +
  facet_wrap(~ ph) +
  cowplot::theme_cowplot() +
  # geom_violin() +
  geom_boxplot() +
  geom_boxplot(position = position_dodge(0.8)) +
  stat_summary(
    fun.y = mean, geom = "errorbar", position = position_dodge(width = .8),
    aes(ymax = ..y.., ymin = ..y..), width = .75, linetype = 3, size = .5
  ) +
  # remove borders from facet labels:
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    strip.background = element_rect(colour = "black", fill = NA)
  ) +
  # make facet labels bold:
  theme(strip.text = element_text(face = "bold")) +
  scale_fill_brewer(palette = "Greys", labels = c("+0", "+2", "+3")) +
  # change x axis labels:
  scale_x_discrete(labels = c("Ambient\n(20.1 °C)", "+2\n(22.1 °C)", "+3 \n(23.1°C)")) +
  # legend title
  labs(fill = "Temp") +
  guides(fill = FALSE) +
  # legend position:
  # theme(legend.position = c(0, 0.9)) + # https://stackoverflow.com/a/35094476
  xlab(" ") +
  ylab((expression("Feeding Rate (" ~ g ~~ day ^ -1 ~~ kg ^ -1 * ")"))) +
  geom_blank()
feeding_plot

# Export plot
cowplot::save_plot(
  "figs/ch_multistressor/feed_plot.png", feeding_plot,
  base_height = 8, base_width = 8
)
# UNCOMMENT TO SAVE, BUT REMEMBER TO CLEAR ENVIRONMENT
save.image(file ="output/ch_multistressor/feeding.RData")

# Create a new tibble that will be used to insert post-hoc results into boxplot
# fac <- levels(factor(feed.df$treatment))
# pos <- boxplot(rate ~ treatment, feed.df)$stats[5, ]
# # phoc <- tibble(fac = as.factor(fac), pos, grp=posthoc$.group)
# phoc <- tibble(fac = as.factor(fac), pos, grp=c("b","a","a","a","a","a"))
