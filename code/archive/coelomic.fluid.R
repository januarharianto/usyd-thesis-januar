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

# ===============
# Colour

# Load excel data
(cfcolour <- read_excel("cfcolour.xlsx", sheet = 1))

# Tidy the dataset and save over itself
cfcolour <- cfcolour %>%
  gather(`Clear or Pink`,`Light Red`, `Red`, key=Colour, value= Cases)

cfcolour # check data

# Plot the data
pdf("cfcolour-%d.pdf",
    pointsize = 13)

ggplot(cfcolour, aes(Treatment, Cases, fill = Colour)) +
  geom_bar(stat = "identity", position = "stack") +
  xlab("Treatment") +                                   # Change x-axis title
  ylab("Count") +                                       # Change y-axis title
  labs(fill = "") +                                     # Change legend title
  theme(legend.position="top") +
  scale_fill_brewer(palette = "Reds")

dev.off()



