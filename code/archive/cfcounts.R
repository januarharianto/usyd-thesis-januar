# Install required packages, if necessary.

wants <- c("easypackages")
has <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])

library(easypackages)    # This allows easy loading of multiple packages
packages("ggplot2",      # Plot better graphs
         "cowplot",      # Instant scientific graphs
         "tidyverse",    # Packages tibble, tidyr, readr, purrr and dplyr
         "readxl")        # Read excel files better

# Load excel data
cfcount <- read_excel("cfcounts.xlsx", sheet = 1)
cfcount

# combine clear and light red columns


# Summarise
cfcount.summ <- cfcount %>%
  group_by(id) %>%
  summarise(N = length(id),
            mean = mean(red.cells),
            sd = sd(red.cells),
            se = sd/sqrt(N))


cfcount.summ$id <- as.character(cfcount.summ$id)
cfcount.summ

# Quick Plot

pdf("cfcount-1.pdf",
    pointsize = 13)

ggplot(cfcount.summ, aes(id, mean, fill = id)) +
  geom_bar(stat = "identity", colour = "black") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), size = .3, width = .1) + # Error bar options
  xlab("Treatment") + # Change x-axis title
  ylab("Average no. of red coelomocytes") + # Change y-axis title
  scale_x_discrete(labels = c("1" = "clear", "2" = "pink", "3" = "light red", "4" = "red")) + # change tick labels
  theme(legend.position="none") + # removes legend
  scale_fill_brewer()

dev.off()
