# Minimal working example (with full data)

# Load packages
library(tidyverse) # data manipulation
library(lme4) # mixed models
library(lmerTest) # this works only for lme4 and tests for fixed/random effects
library(nlme) # mixed models (alternative)
library(car) # analysis package

# ABOUT THE DATA ---------------------------------------------------------------
# The data is a record of metabolic rate in urchins (Heliocidaris
# erythrogramma). Experiment was conducted in 2014/15. Factors are temperature,
# pH and time (week). For the measurement of metabolic rate, urchins are
# selected randomly, but from 3 tanks for each treatment (total 10 urchins per
# treatment). This is done by sequencing tanks repeatedly from 1-3, and then
# randomly picking an urchin from each tank number in the sequence until 10
# urchins are selected. The issue with this method is that it is unbalanced (one
# tank only has 2 urchins).

# temperatures: +0 (ambient), +2 (med) and +3 (high)
#           ph: 8.0 (ambient) and 7.6 (low)
#         time: 4 wks and 12 wks

# Here, urchins are nested to tank, and tank is nested to header tank. Since
# there is only one single header tank per treatment, we don't need to put
# header tank in the model.

# LOAD THE DATA ----------------------------------------------------------------
# Don't want to rely on a file, so let's put the cleaned dataset in here.
# Current original file location is backed up to Dropbox in the folder:
# /phd/usydthesis/code/ch_multistressor/respiration.R
rd <-
  structure(list(tank = structure(c(1L, 2L, 3L, 1L, 2L, 3L, 1L, 
    2L, 3L, 1L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 2L, 1L, 2L, 3L, 
    1L, 2L, 3L, 1L, 2L, 3L, 1L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 
    1L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 2L, 1L, 2L, 3L, 1L, 2L, 
    3L, 1L, 2L, 3L, 1L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 2L, 1L, 
    2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 2L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 
    2L, 3L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 2L, 1L, 2L, 3L, 
    1L, 2L, 3L, 1L, 2L, 3L, 1L, 1L, 2L, 3L, 1L, 2L, 3L), .Label = c("1", 
      "2", "3"), class = "factor"), temp = structure(c(1L, 1L, 1L, 
        1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
        1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
        1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
        2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
        2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 
        3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 
        3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 
        3L), .Label = c("0", "2", "3"), class = "factor"), ph = structure(c(2L, 
          2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
          2L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
          1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
          2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 
          1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 
          2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
          2L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
          1L, 1L, 1L), .Label = c("pH 7.6", "pH 8.0"), class = "factor"), 
    time = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
      2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 
      1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
      1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 
      2L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
      2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 
      1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
      1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 
      2L), .Label = c("4", "12"), class = "factor"), rate = c(4.83, 
        5.62, 7.48, 5.67, 6.38, 6.52, 5.49, 6.77, 5.69, 6.43, 7.02, 
        8.54, 6.35, 8.4, 7.18, 7.09, 8.62, 7.49, 6.45, 7.15, 6.55, 
        7.8, 7.64, 8.6, 8.98, 8.21, 7, 7.15, 7.46, 7.06, 8.17, 9.04, 
        9.24, 7.13, 9.01, 9.56, 9.32, 9.05, 8.89, 8.27, 6.35, 5.58, 
        6.45, 6.54, 7.74, 8.27, 5.43, 6.36, 9.22, 6.42, 7.39, 8.7, 
        7.57, 9.04, 7.27, 7.19, 7.4, 8.36, 8.23, 7.79, 8.33, 7.54, 
        8.75, 9.63, 9.49, 7.21, 7.16, 8.22, 7.26, 9.56, 8.23, 8, 
        8.82, 9.04, 6.73, 7.23, 7.19, 9.01, 8.46, 7.18, 7.89, 8.72, 
        10.23, 6.46, 6.63, 9.37, 9.45, 7.15, 8.54, 8.06, 8.23, 8.53, 
        7.83, 7.92, 7.71, 6.32, 8.1, 8.81, 8.54, 8.84, 8.97, 12.51, 
        7.26, 8.49, 11.67, 8.68, 9.83, 7.8, 9.48, 10.65, 6.82, 6.12, 
        7.73, 7.65, 7.44, 7.36)), class = c("tbl_df", "tbl", "data.frame"
        ), row.names = c(NA, -116L), .Names = c("tank", "temp", "ph", 
          "time", "rate")) %>%
  print()

# So the data contains 116 rows of samples, instead of 120. This is because at
# week 12 there are not enough urchins in the higher temperature treatments (due
# to mortality) - making the experiment unbalanced. Thus we should account for
# unbalanced designs in our post-hoc later, but the initial analysis should be
# fine since it is a regression model.

# ANALYSIS ---------------------------------------------------------------------
## Generate the mixed model. Use Type 3 as it is more accurate for interactions,
## see Langsrud (2003). 

## Account for all interactions first:
model_lmer <- lmer(rate ~ temp*ph*time + (1|tank) + (temp|tank) + (ph|tank), data = rd) %>% print()
summary(model_lmer)
Anova(model_lmer, type = 3, contrasts=list(topic=contr.sum, sys=contr.sum)) 
## Quick anova shows marginal 3-way interaction (ns, 0.09)


## Refine the mixed model to remove 3-way interactions:
model_lmer <- lmer(rate ~ temp*ph*time - temp:ph:time + (1|tank) + (temp|tank) + (ph|tank), 
  data = rd) %>%
  print()
summary(model_lmer)
Anova(model_lmer, type = 3, contrasts=list(topic=contr.sum, sys=contr.sum))
## Since at least one 2-way interaction exists, we keep this model.

## Perform test for random effects. Here we use lme4's `rand` function.
rand(model_lmer)
## Whoa, random effects table showed that adding the random factor did not
## affect the model any one bit. It's not normal. Let's find another method to
## verify.


## ALTERNATIVE METHOD (NLME)
## Use `nlme``, a completely different mixed models package, to analyse the same
## dataset.
model_nlme <- lme(rate ~ temp*ph*time, random = ~1|tank, data = rd) %>% print()
summary(model_nlme)
Anova(model_nlme, type = 3, contrasts=list(topic=contr.sum, sys=contr.sum)) 
## Same as in lme4, 3-way interactions are marginal (ns, 0.09)

## Refine the mixed model and remove 3-way interactions:
model_nlme <- lme(rate ~ temp*ph*time - temp:ph:time, random = ~1|tank, 
  data = rd) %>% print()
summary(model_nlme)
Anova(model_nlme, type = 3, contrasts=list(topic=contr.sum, sys=contr.sum)) 
## Keep this model since at least one 2-way interaction exists.

## TEST FOR RANDOM EFFECTS IN NLME
## Generate model without random effects using nlme's `gls`` function:
model_fx <- gls(rate ~ temp*ph*time - temp:ph:time, data = rd) %>% print()
## Compare the 2 models using quick anova:
anova(model_nlme, model_fx)
## Same p-value as lme4 method! 


