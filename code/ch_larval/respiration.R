library(tidyverse)
library(readxl)

# dt <- 
#   readxl::read_excel("input/ch_larval/larval_mR_2015.xlsx",
#     sheet = 2, skip = 1) %>%
#   print()

# ggplot(dt, aes(temp_c, mr)) +
#   geom_point(aes(colour = parents)) +
#   geom_smooth(aes(colour = parents), 
#               method = lm,  
#               # formula = y ~ x + I(x^2),
#               se = F)
# 

dt <- 
  read_excel("input/ch_larval/larval_mR_2015.xlsx", sheet = 2, skip = 1) %>%
  group_by(parents, temp, ph) %>%
  summarise(mean = mean(mr),
    se = sd(mr)/sqrt(3)) %>%
  print()

noph_dt <- dt %>%
  filter(ph == "pH 8.0") %>%
  print()

# ggplot(noph_dt, aes(temp_c, mean, colour = parents)) +
#   geom_point() +
#   geom_line() +
#   geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .1) +
#   cowplot::theme_cowplot() + 
#   geom_blank()

# yesph_dt <- dt %>%
#   filter(ph == "pH 7.6") %>%
#   print()
# 
# ggplot(yesph_dt, aes(temp_c, mean, colour = parents)) +
#   geom_point() +
#   geom_line() +
#   geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .1) +
#   cowplot::theme_cowplot() + 
#   geom_blank()


# # Overall
# ggplot(dt, aes(temp_c, mean, colour = interaction(parents, ph))) +
#   geom_point() +
#   geom_line() +
#   geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .1) +
#   cowplot::theme_cowplot() + 
#   geom_blank()


ggplot(dt, aes(temp, mean, colour = parents)) +
  facet_grid(cols = vars(ph)) +
  # geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .1) +
  cowplot::theme_cowplot() +
  xlab("Temperature °C") +
  ylab((expression("Metabolic rate (" ~ pmol ~ O[2] ~~ h ^ -1 ~~ mu*m ^ -1 * ")"))) +
  geom_blank()

# Difference between ANOVA and ANCOVA? 
# One factor is continuous (and is often the covariate on the x axis)

# analyse warming larvae only
noph_dt
model <- lm(mean ~ temp * ph * parents, dt)
# summary(model)
car::Anova(model)
# 
# library(emmeans)
# emmeans(model, pairwise ~ temp + parents)
