# Install required packages, if necessary.

wants <- c("easypackages")
has <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])

library(easypackages)    # This allows easy loading of multiple packages
packages("ggplot2",      # Plot better graphs
         "cowplot",      # Instant scientific graphs
         "tidyverse",    # Packages tibble, tidyr, readr, purrr and dplyr
         "readxl")       # Read excel files better

# Read excel
g.index <- read_excel("gonadindex.xlsx", sheet = 2)
g.index
g.index <- select(g.index,treatment,gonad.index)
g.index
summary(g.index)

ggplot(g.index, aes(as.character(treatment), gonad.index)) +
  geom_violin(fill = "black", colour = "white", alpha = .1, width = 1.5) +
  geom_boxplot(width = .2) +
  xlab("Treatment temperature (°C)") +
  ylab("Gonad Index")
cowplot::ggsave("gindex_violin.png", width = 8, height = 6)

ggplot(g.index, aes(as.character(treatment), gonad.index)) +
  geom_boxplot() 

gi.test <- oneway.test(gonad.index ~ treatment, g.index)
gi.test


# Summarise for bar
g.index.summ <- g.index %>%
  group_by(treatment) %>%
  summarise(N = length(treatment),
            mean = mean(gonad.index, na.rm = T),
            sd = sd(gonad.index, na.rm = T),
            se = sd/sqrt(N))
g.index.summ

# Plot
ggplot(g.index.summ, aes(treatment, mean, fill = "treatment")) +
  geom_bar(stat = "identity", colour = "black") +                       # Bar plot
  geom_point(data = g.index, aes(treatment-.2, gonad.index)) +          # Add data points to plot as dots
  geom_hline(yintercept = 4, linetype = "dotted") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), size = .3, width = .1) + # Error bar options
  xlab("Treatment temperature (°C)") +  # Change y-axis title
  ylab("Gonad Index (au)") +  # Change x-axis title
  scale_x_continuous(breaks=seq(20, 26, 2)) +
  scale_y_continuous(breaks=seq(0, 8, 2)) +
  scale_fill_brewer() +
  theme(legend.position="none")  # removes legend
  
