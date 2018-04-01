# Pre-processing
library(snipR)
snipR::refresh()

# load packages
snipR::pkgs(
  tidyverse,
  readxl,
  car
)


# load data frame:
ammo <- read_excel("input/ch_multistressor/ammonia.xlsx", skip = 3) %>%
  print()

# generate data frame for plot:
plot_dt <- ammo %>%
  select(treatment, tempv, phv, ammonia) %>%
  mutate(treatment = factor(treatment,
    levels = c("0C", "0A", "2C", "2A", "3C", "3A"))) %>%
  mutate(ph = factor(phv, levels = c("pH 8.0","pH 7.6"))) %>%
  mutate(temp = factor(tempv, levels = c("Ambient","+2 °C","+3 °C"))) %>%
  print()

# plot ammonia data:
nh_plot <- ggplot(plot_dt, aes(factor(temp), ammonia)) +
  facet_wrap(~ ph) +
  cowplot::theme_cowplot() +
  geom_boxplot() +
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
  xlab("") +
  scale_x_discrete(labels = c("Ambient\n(20.7 °C)", "+2\n(22.7 °C)", 
    "+3\n(23.7 °C)")) +
  ylab((expression("Ammonia excretion rate ("*mu*g ~~ kg ^ -1 ~~ h ^ -1 * ")"))) +
  geom_blank()

# Data analysis
# first, generate model (linear)
model <- lm(ammonia ~ factor(temp) * factor(ph), data = ammo)

# perform Type 2 ANOVA
car::Anova(model) # temp significant, pH not significant, no interactions

# check assumptions
hist(residuals(model))  # not that normal... but should be fine
shapiro.test(ammo$ammonia) # yup, data is normal

plot(fitted(model), residuals(model)) # residuals look ok
bartlett.test(ammonia ~ interaction(temp, ph), ammo) # pass
fligner.test(ammonia ~ interaction(temp, ph), ammo) # pass

# perform posthoc
library(emmeans)
emmeans(model, ~temp:ph)
emmeans(model, pairwise~temp:ph) # view overall results
hsd <- emmeans(model, pairwise~temp|ph) # use this since we have no effect of pH
emmeans(model, pairwise~temp, by = ph) # use this since we have no effect of pH
grps <- cld(hsd)
hsd$contrasts
grps$contrasts


# Generate summary
ammo_summ <- ammo %>%
  select(treatment, temp, ph, ammonia) %>%
  group_by(treatment) %>%
  dplyr::summarise(mean = mean(ammonia),
    sd = sd(ammonia),
    se = sd/sqrt(length(ammonia))) %>%
  print()

ggplot(ammo_summ, aes(treatment, mean)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean +se, ymax = mean - se), width = .1) +
  cowplot::theme_cowplot() +
  geom_blank()

cowplot::save_plot(
  "figs/ch_multistressor/nh_plot.png", nh_plot,
  base_height = 8, base_width = 8
)

save.image(file ="output/ch_multistressor/ammonia.RData")


