# Load my usual packages and functions
source("~/Dropbox/R/functions/allFunctions.R")
loadPackage("tidyverse")  # better code
loadPackage("lubridate")  # handles datetime
loadPackage("ggplot2")    # grammar of graphics (plots)
loadPackage("cowplot")    # scientific theme for ggplot2
loadPackage("viridis")    # colour scheme for ggplot2

# Set working directory
setwd("~/Dropbox/R/2013-experiments/")
# Load additional libraries if needed
loadPackage("readxl")
loadPackage("car")

# Load data
(hsp <- read_excel("hsp70.xlsx", sheet = 1))  # Read data from excel

# tidy data for analysis
hsp <- hsp %>%
  gather(`20`, `22`, `24`, `26`, key = treatment, value = intensity) %>% # gather data
  #dplyr::filter(intensity != "") %>% # take out blanks
  mutate(treatment = factor(treatment, levels = unique(treatment))) %>% # use treatment as factor
  na.omit() %>%
  print()

# ------------------------------------------------------------------------------
# Boxplot
# ------------------------------------------------------------------------------

# Create a new tibble that will be used to insert post-hoc results into boxplot

fac <- levels(factor(hsp$treatment))
pos <- boxplot(intensity ~ treatment, hsp)$stats[5, ]
grp <- c("a","ab","b","b")
phoc <- tibble(fac = as.factor(fac), pos, grp)

ggplot(hsp, aes(treatment, intensity)) +
  geom_violin(fill = "black", colour = "white", alpha = .1, width = 1) +
  # colour
  # geom_boxplot(alpha=.6, aes(fill = treatment),outlier.size = 1.5,
  #   outlier.colour = "black", outlier.fill = "red", outlier.alpha = .5) +
  # no colour
  geom_boxplot(width = .2, alpha=1, outlier.size = 1.5,
    outlier.colour = "black", outlier.fill = "red", outlier.alpha = .5) +
  geom_text(data = phoc, aes(fac, pos+.1, label = grp, fontface = "bold", size = 12)) +
  scale_fill_viridis(discrete=T) +
  theme(legend.position="none") +
  xlab("Treatment temperature (°C)") +  # Change y-axis title
  ylab("Mean band intensity (au)")  # Change x-axis title

# Save in various locations
cowplot::ggsave("c2_hsp70box.png", path = "~/Documents/thesis_usyd_2017/chapter-2/figs/", width = 8, height = 6)
cowplot::ggsave("c2_hsp70box.png", path = "~/Downloads/pandocThesis/source/figures/", width = 8, height = 6)

# ------------------------------------------------------------------------------
# Bar plot
# ------------------------------------------------------------------------------

# Summarise the data to plot means
hsp.summ <- hsp %>% # define hsp.summ, then
  dplyr::group_by(treatment) %>% # group by treatment, then
  dplyr::summarise(N = length(intensity),
            mean = mean(intensity, na.rm = T),
            sd = sd(intensity, na.rm = T),
            se = sd/sqrt(N))
hsp.summ

# Ok plot the means

ggplot(hsp.summ, aes(treatment, mean)) +
  geom_bar(stat = "identity",
           colour = "black",
           aes(fill = treatment),
           alpha = .6) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                size = .3,
                width = 0) + # Error bars
  geom_text(
    aes(label = c("a", "ab", "b", "b"), y = mean + se),
    vjust = -1,
    size = 4,
    fontface = "bold"
  ) +
  ylim(0, 1.1) +
  xlab("Treatment temperature (°C)") +  # Change y-axis title
  ylab("Mean band intensity (au)") +  # Change x-axis title
  scale_fill_viridis(discrete = T) +
  guides(fill = F)

# Save the file into latex project folder
cowplot::ggsave("c2_hsp70bar.png", path = "~/Documents/thesis_usyd_2017/chapter-2/figs/", width = 8, height = 6)
cowplot::ggsave("c2_hsp70bar.png", path = "~/Downloads/pandocThesis/source/figures/", width = 8, height = 6)

# STATISTICS
# Check histogram
library(lattice)
histogram( ~ intensity | treatment, hsp, layout = c(1, 4))

# test for normality
# Welch (check if equal variances)
oneway.test(intensity ~ treatment,
            data = hsp,
            var.equal = T)

#Levene (check if normal)
leveneTest(intensity ~ treatment, hsp)

# Perform nonparametric test
kruskal.test(intensity~treatment,hsp)
?kruskal.test
#Let's do multiple comparisons
library(FSA)
(dunn <- dunnTest(intensity~treatment,hsp, method ="bh"))
(PT <-  dunn$res)
library(rcompanion)
cldList(
  comparison = PT$Comparison,
  p.value    = PT$P.adj,
  threshold  = 0.05
)

# Let's print the graph
# pdf("ch02_hsp.pdf", pointsize = 13)
# plot.hsp
# dev.off()




