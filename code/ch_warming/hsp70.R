# Notes ----

## Description

# Pre-processing ----

library(snipR)  # my personal package
snipR::refresh()

# Load packages ----

snipR::pkgs(
  tidyverse,
  readxl,
  lubridate,
  cowplot,
  viridis,
  car,
  dunn.test
)

# Load data ----

dt <- read_excel("input/ch_warming/hsp70.xlsx") %>%
  gather(`20`:`26`, key = treatment, value = intensity) %>%
  filter(!is.na(intensity)) %>%
  mutate(treatment = factor(treatment)) %>%
  print()


# Manipulate data ----

hsp <- dt
fac <- levels(factor(hsp$treatment))
pos <- boxplot(intensity ~ treatment, hsp)$stats[5, ]
grp <- c("a","ab","b","b")
phoc <- tibble(fac = as.factor(fac), pos, grp)

## Grab mean values
means <- dt %>%
  group_by(treatment) %>%
  summarise(mean = mean(intensity),
    var = var(intensity),
    count = length(intensity)) %>%
  mutate(fold_change2 = mean/dplyr::filter(., treatment == 20)$mean) %>%
  print()

# Plotting ----

hsp_plot <- ggplot(hsp, aes(treatment, intensity)) +
  geom_boxplot(    
    fill = "lightyellow",
    fatten = 1,
    alpha = .6,
    outlier.size = 2,
    outlier.alpha = .8) +
  geom_text(data = phoc, aes(fac, pos+.1),
    label = phoc$grp, fontface = "bold") +
  stat_summary(fun.y = median, geom = "errorbar",
    aes(ymax = ..y.., ymin = ..y.., linetype = "Median"),
    width = .75) +
  stat_summary(fun.y = mean, geom = "errorbar",
    aes(ymax = ..y.., ymin = ..y.., linetype = "Mean"),
    width = .75) +
  scale_linetype_manual(name = "", values = c(3,1)) +
  theme(legend.position = "bottom", legend.justification = "center") +
  xlab("Treatment temperature (°C)") +  # Change y-axis title
  ylab("Mean band intensity (au)")  # Change x-axis title
hsp_plot

# Analysis ----

# Check if data is normal:
qqnorm(hsp$intensity)
qqline(hsp$intensity)

## Levene's test for normality
leveneTest(intensity ~ treatment, hsp, center = "mean")
## data not normal

## Welch test for for homogeneity of variances:
oneway.test(intensity ~ treatment,
            data = hsp,
            var.equal = T)

# Bartlett test for homogeneity of variances:
bartlett.test(intensity ~ treatment, hsp)




## perform nonparametric test
kruskal <- kruskal.test(intensity~treatment,hsp) %>%
  print()


## posthoc Dunn's
posthoc <- dunn.test(hsp$intensity, hsp$treatment, method = "bh", list = T)
## 26 = 24 = 22; 26 = 24 > 20; 22 = 20
?dunn.test


## another posthoc, Wilcox
pairwise.wilcox.test(hsp$intensity, hsp$treatment, p.adjust.method = "BH")
## 26 = 24 = 22; 26 = 24 > 20; 22 = 20; results are same as Dunn's (p values
## differ but pairwise differences are equal)


# Export ----

cowplot::save_plot("figs/ch_warming/c2hsp.png", hsp_plot,
  base_height = 8, base_width = 8)
