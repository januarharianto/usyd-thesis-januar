wants <- c("easypackages")
has <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])
setwd("~/Dropbox/R/2013-experiments/")
library(easypackages)    # This allows easy loading of multiple packages

# Load excel data
library(tidyverse)
library(readxl)
(dt <- read_excel("respiration2013-vol.xlsx", sheet = 1))
# Select only the required data (treatment, group and rate)
(rate.dt <- dplyr::select(dt, treatment, group, metabolicRate))

# Define the factors within the data
rate.dt$group <- as.factor(rate.dt$group)
rate.dt$treatment <- as.factor(rate.dt$treatment)
rate.dt # check

# Test only. This lists the factors in the dataset.
factors <- levels(factor(rate.dt$treatment)) # list factors
# from boxplot, list stats from 5th row (thats the max whisker value)
wupper <- boxplot(metabolicRate ~ treatment, data = rate.dt)$stats[5, ]
# create table with the 2 lists
(letterPos <- tibble(factors, wupper))
# add the letters to the list
letterPos$group <- c("ab","b","b","a")


# Let's boxplot that and look at the data
packages("ggplot2", "cowplot")
library(ggplot2)
library(cowplot)
(
  plot.boxplot <-
    ggplot(rate.dt, aes(factor(treatment), metabolicRate)) + 
    geom_boxplot() +
    geom_text(data = letterPos, aes(factors,wupper*1.05, label = group, fontface = "bold")) +
    # geom_text(aes(label=c("bc","c","a","bc"), hjust=offset.h, vjust=offset.v)) +
    # geom_boxplot(outlier.colour = NA) +  # Outliers are duplicated in geom point, take them out here
    # geom_point(position = position_jitter(width = 0.1), size = 0.3) +
    xlab("Treatment temperature (°C)") +  # change y-axis title
    ylab(expression(Rate ~ (mgO[2] ~ L^{-1} ~ kg^{-1}))) +  # Change x-axis title
    #scale_fill_brewer(palette = "YlOrRd") +
    theme(legend.position="none")  # removes legend
)
# Save the file into latex project folder
folder <- "/Users/starlord/Dropbox/phd/thesis/phd-thesis/figures"
ggsave("c2_mo2_boxplot.pdf", path = folder, width = 6, height = 6)

#Summarise the data
(rate.smry <- rate.dt %>%
  dplyr::group_by(treatment) %>%
  dplyr::summarise(
    n = length(metabolicRate),
    rate = mean(metabolicRate, na.rm = T),
    sd = sd(metabolicRate, na.rm = T),
    se = sd / sqrt(n)
  ))


  

# Check effect of tank
model.fixed = gls(metabolicRate ~ treatment, data = rate.dt, method="REML")
anova(model, model.fixed) # p-value 0.9399 - no effect of tank

# Let's do a mixed effects model (aka nested ANOVA)
packages("nlme")
model <- lme(metabolicRate ~ treatment, random = ~ 1 | group, data = rate.dt)
anova.lme(model, type = "sequential", adjustSigma = F)

packages("multcomp")
# Perform Tukey post-hoc, mcp = multicomp
posthoc <- glht(model, linfct = mcp(treatment="Tukey"))
# Summarise the results
(mcs <- summary(posthoc, test = adjusted("single-step")))
# Letter display of posthoc
cld(mcs, level=0.05, decreasing=TRUE)
# Compare least-square means. This is generally thought as more representative of the population
# provided that the experimental design is balanced.
packages("multcompView", "lsmeans")
(leastsquare <- lsmeans(model, pairwise ~ treatment, adjust = "tukey"))  # Tukey-adjusted comparisons
cld(leastsquare, alpha=0.05, Letters=letters, adjust = "tukey") # summarise the comparisons

# Check assumptions of the model
hist(residuals(model), col="darkgray") # visualising normality
plot(fitted(model), residuals(model)) # visualising homoscedasticity
