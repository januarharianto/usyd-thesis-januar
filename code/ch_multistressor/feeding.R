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

# Apply correction to consumed, and then calculate rate
feeding_rate <-
  read_excel("input/ch_multistressor/feeding.xlsx", sheet = "main", skip = 1) %>%
  select(treatment, temp, ph, grp, specimen, consumed) %>% # select columns
  mutate(corrected = consumed - abs(correction)) %>% # apply correction
  mutate(rate = corrected / specimen) %>% # convert to rate, g/day/g
  mutate(rate_dry = rate * abs(m) + abs(c)) %>% # convert to dry, g/day/g
  arrange(desc(ph)) %>%
  mutate(ph = factor(ph, levels = c("pH 8.0","pH 7.6"))) %>%
  print()

# Just checking summary stats
feeding_summary <-
  feeding_rate %>%
  group_by(treatment, temp, ph) %>%
  summarise(
    mean = mean(rate_dry),
    sd = sd(rate_dry),
    se = sd / sqrt(length(temp))
  ) %>%
  mutate(energy_sd = sd * 12.674) %>%
  print()

# ggplot(feeding_summary, aes(treatment, mean)) +
#   geom_bar(stat = "identity") +
#   geom_errorbar(aes(ymin = mean - se, ymax = mean + se))

# Plot
feeding_plot <- ggplot(feeding_rate, aes(temp, rate)) +
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
  scale_x_discrete(labels = c("Ambient", "+ 2 °C", "+ 3 °C")) +
  # legend title
  labs(fill = "Temp") +
  guides(fill = FALSE) +
  # legend position:
  # theme(legend.position = c(0, 0.9)) + # https://stackoverflow.com/a/35094476
  xlab(" ") +
  ylab((expression("Feeding Rate (" ~ g ~~ day ^ -1 ~~ kg ^ -1 * ")"))) +
  geom_blank()
feeding_plot

# statistics
### 2 way anova with repeated measures!!!
library(lme4)
library(lmerTest)
library(car)
model <- lmer(rate ~ temp + ph + temp*ph + (1|grp), data = feeding_rate)
Anova(model)
rand(model)
difflsmeans(model, "temp:ph")  # ONLY IF random effects are significant

# no nesting term
feed_model <- lm(rate ~ temp * ph, data = feeding_rate)
feed_anova <- car::Anova(feed_model) %>% print()


## Tukey HSD
library(emmeans)
emmeans(feed_model, ~temp:ph)

hsd <- emmeans(feed_model, pairwise~temp|ph) # use this since we have no effect of pH
hsd
grps <- cld(hsd)
grps

rm(agar_dt, agar_plot, fit)

# check assumptions
hist(residuals(model))  # not that normal... but should be fine
shapiro.test(feeding_rate$rate_dry) # yup, data is normal

plot(fitted(model), residuals(model)) # residuals look ok
bartlett.test(rate_dry ~ interaction(temp, ph), feeding_rate) # pass
fligner.test(rate_dry ~ interaction(temp, ph), feeding_rate) # pass


# Create a new tibble that will be used to insert post-hoc results into boxplot
# fac <- levels(factor(feed.df$treatment))
# pos <- boxplot(rate ~ treatment, feed.df)$stats[5, ]
# # phoc <- tibble(fac = as.factor(fac), pos, grp=posthoc$.group)
# phoc <- tibble(fac = as.factor(fac), pos, grp=c("b","a","a","a","a","a"))

# Export plot
cowplot::save_plot(
  "figs/ch_multistressor/feed_plot.png", feeding_plot,
  base_height = 8, base_width = 8
)
# UNCOMMENT TO SAVE, BUT REMEMBER TO CLEAR ENVIRONMENT
save.image(file ="output/ch_multistressor/feeding.RData")

