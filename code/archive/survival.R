# Install required packages, if necessary.
wants <- c("easypackages")
has <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])

library(easypackages)    # This allows easy loading of multiple packages
packages("ggplot2",      # Plot better graphs
         "cowplot",      # Instant scientific graphs
         "tidyverse",    # Packages tibble, tidyr, readr, purrr and dplyr
         "readxl",       # Read excel files better
         "survival",     # Statistical analysis of survival datasets
         "survminer")    # Among other things, display survival data better


# Load excel data
survival <- read_excel("survival2013.xlsx", sheet = 1)
survival


# Fit survival curves
fit <- survfit(Surv(time, death) ~ treatment, data = survival)
fit

# plot survival curves
plot.surv <- 
  ggsurvplot(fit,
             size = 1,
             # linetype = "strata", # Automatically adjust line types
             # conf.int = T, # Add confidence interval
             pval = T, # Add p-value
             #risk.table = T, # Add risk table
             #risk.table.y.text.col = T, 
             palette = "Spectral",
             #break.time.by = 10, # Break time axis by 250
             ylim = c(0.6,1), # Limit the y-axis
             xlab = "Time elapsed (days)",
             ylab = "Survival Probability",
             #legend = "right", # Pick this or below
             #legend = c(0.2,0.2),
             legend.title = "",
             legend.labs = c("20 °C","22 °C","24 °C","26 °C")
             #font.x = c(12, "bold"),
             #font.y = c(12, "bold"),
             #font.tickslab = 11
             )
plot.surv



# Let's print something
pdf("ch1_survival.pdf",
    pointsize = 13)
plot.surv
dev.off()


# Analyse data
# Log-rank test for equal survival functions. 
#Tests if there is a difference between survival curves using the G-rho family of tests. 
# I wonder if an unbalanced desing ANOVA is better for this. 
#Can't remember why I didn't use it but I think it might be due to the way things were replicated 
#i.e. 2 headers, 6 tanks, so everything would need to be averaged by 3 (or 6!!)
 
# Reference
# Harrington, D. P. and Fleming, T. R. (1982). A class of rank test procedures for censored survival data. Biometrika 69, 553-566.
# Perform test using the survival package and calling the `survdiff` command.
library(survival)
library(survminer)
survdiff(Surv(time, death) ~ treatment, data = survival)
whoa <- pairwise_survdiff(Surv(time, death) ~ treatment, data = survival)
whoa
surv_summary(fit, data = survival)
