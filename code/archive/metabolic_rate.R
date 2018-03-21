# Analysis on the effect of temperature on metabolic rate
# Experiment was conducted in 2013. Refer to PhD thesis chapter 2.

# Load my usual packages and functions
source("~/Dropbox (Personal)/R/functions/allFunctions.R")
loadPackage("tidyverse")  # better code
loadPackage("lubridate")  # handles datetime
loadPackage("ggplot2")    # grammar of graphics (plots)
loadPackage("cowplot")    # scientific theme for ggplot2
loadPackage("viridis")    # colour scheme for ggplot2


setwd("~/Dropbox (Personal)/R/2013-experiments/")

# Load additional libraries as needed
loadPackage("readxl")
loadPackage("lme4")
loadPackage("lmerTest")
loadPackage("multcomp")
loadPackage("car")

# Load data
dt <- read_excel("respiration2013-vol.xlsx", sheet = 1) %>%
  dplyr::select(treatment,group,metabolicRate) %>%
  mutate(treatment = as.factor(treatment), group = as.factor(group)) %>%
  print()

# data needs to be rechecked
# recheck:
dt <- read_excel("respiration2013-vol.xlsx") %>%
  dplyr::select(1,2,4,8,9,10) %>%
  mutate(vol = volume.L - volUrchin,
    slope = slope.mgl.per.min,
    metabolicRate = slope / (wetWt.g / 1000) * vol) %>%
  dplyr::select(treatment, group, metabolicRate) %>%
  print()

# ------------------------------------------------------------------------------
# Plots
# ------------------------------------------------------------------------------
# First prep
## This lists the factors in the dataset.
factors <- levels(factor(dt$treatment)) # list factors
# from boxplot, list stats from 5th row (thats the max whisker value)
wupper <- boxplot(metabolicRate ~ treatment, data = dt)$stats[5, ]
# create table with the 2 lists
(letterPos <- tibble(factors, wupper))
# add the letters to the list
letterPos$group <- c("ab","a","a","b")

ggplot(dt, aes(factor(treatment), metabolicRate)) +
  geom_boxplot(aes(fill = treatment), alpha = .6,
               outlier.shape = 21, outlier.size = 1.5, outlier.colour = "black", outlier.fill = "red", outlier.alpha = .5) +
  geom_text(data = letterPos, aes(factors,wupper+.05, label = group, fontface = "bold", size = 12)) +
  # geom_text(aes(label=c("bc","c","a","bc"), hjust=offset.h, vjust=offset.v)) +
  # geom_boxplot(outlier.colour = NA) +  # Outliers are duplicated in geom point, take them out here
  # geom_point(position = position_jitter(width = 0.1), size = 0.3) +
  xlab("Treatment temperature (°C)") +  # change y-axis title
  ylab(expression(Rate ~ (mgO[2] ~ g^{-1} ~ h^{-1}))) +  # Change x-axis title
  # scale_fill_viridis(discrete = T) +
  #scale_fill_viridis(discrete = T) +
  theme(legend.position="none")  # removes legend

cowplot::ggsave("c2_mrboxplot.png", path = "~/Documents/thesis_usyd_2017/chapter-2/figs/", width = 8, height = 6)
cowplot::ggsave("c2_mrboxplot.png", path = "~/Downloads/pandocThesis/source/figures/", width = 8, height = 6)

ggplot(dt, aes(factor(treatment), metabolicRate)) +
  geom_violin(colour = "white", fill = "black", alpha = .1) +
  geom_boxplot(alpha = 1, width = .2,
    outlier.shape = 21, outlier.size = 2, outlier.colour = "white", outlier.fill = "black", outlier.alpha = .5) +
  geom_text(data = letterPos, aes(factors,wupper+.05, label = group, fontface = "bold", size = 12)) +
  xlab("Treatment temperature (°C)") +  # change y-axis title
  ylab(expression(Rate ~ (mgO[2] ~ g^{-1} ~ h^{-1}))) +  # Change x-axis title
  cowplot::theme_cowplot(font_size = 16) +
  theme(legend.position="none") +  # removes legend
  geom_blank()
cowplot::ggsave(filename = "metabolicrate2013.png", width = 129, height = 96.75, units = "mm")
cowplot::ggsave(filename = "metabolicrate2013b.png", width = 8, height = 6)

# ------------------------------------------------------------------------------
# STATS
# ------------------------------------------------------------------------------
# Run the nested ANOVA
model <- lmer(metabolicRate ~ treatment + (1|group), data = dt, REML = T)
anova(model)
rand(model)
?rand
difflsmeans(model, test.effs="treatment") # compare treatment groups within model


# Posthoc - multiple comparisons
posthoc <-  glht(model, linfct = mcp(treatment="Tukey"))
multicp <- summary(posthoc, test=adjusted("single-step")) %>% print()
cld(multicp, level=0.05, decreasing=TRUE)

# Alternative method
amodel <- lm(metabolicRate ~ treatment, dt)
Anova(amodel, type = "2")
summary(amodel)
library(rcompanion)
plotNormalHistogram(residuals(amodel))
plot(fitted(amodel), residuals(amodel))
plot(amodel)

# Summary stats and add Q10 values
summ <- dt %>%
  group_by(treatment) %>%
  summarise(average = mean(metabolicRate),
            pctChange = average/0.6563912*100) %>%
  print()

dt %>%
  group_by(treatment) %>%
  summarise(
    mean = mean(metabolicRate),
    se = sqrt(var(metabolicRate)/length(metabolicRate)),
    sd = sd(metabolicRate),
    count = length(metabolicRate),
    q10 = )

# What's the % difference in MO2 between the control treatment and the 24°C treatment?
summ$average[3]/summ$average[1]*100-100

