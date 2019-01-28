library("tidyverse")
library("lubridate")
library("cowplot")
library("viridis")

# ==============================================================================
# Load data
# ==============================================================================
# Research dataset, 2013-2017
log4 <- read_csv("/Users/januar/Dropbox (Personal)/R/temp/data/sims13-17cal.csv") %>% print()

# summarise the data by day and plot
log4 %>%
  mutate(day = yday(time)) %>%  # use yday for unique days of the year (1-365)
  group_by(day) %>%
  summarise_each(funs(mean, min, max, sd), temp) %>%
  ggplot() +
  geom_line(aes(day, mean)) +
  geom_ribbon(aes(x = day, ymin = min, ymax = max), 
              fill = viridis(1), 
              alpha = .1)

# mean temperature for all

# summarise the data by month and boxplot
coloura <- "lightyellow"
colourb <-  "lightyellow"
log4 %>%
  # filter(time > as.Date("2014-01-01"), time < as.Date("2014-12-31")) %>%
  mutate(month = month.abb[month(time)],  # extract month char using month.abb
    month = factor(month, levels = month.abb)) %>% # order the months
  ggplot() +
  geom_boxplot(aes(month, temp, group = month, fill = month), 
               alpha = .6, show.legend = F) +
  scale_fill_manual(values=c(coloura, coloura, coloura, coloura,
                             coloura, coloura, colourb, colourb,
                             colourb, colourb, colourb, colourb)) +
  geom_hline(yintercept = 19.66975, linetype = 2, alpha = .5) +  # mean
  geom_hline(yintercept = 19.81, linetype = 3, alpha = .5) +  # median
  xlab("2013 - 2017") +
  ylab("Temperature"~degree*C) +
  scale_y_continuous(breaks = seq(0,40,2)) 

cowplot::ggsave("c2_monthbox.png", path ="~/Documents/thesis_usyd_2017/chapter-2/figs/", width = 8, height = 6)
cowplot::ggsave("c2_monthbox.png", path ="~/Downloads/pandocThesis/source/figures/", width = 8, height = 6)



# Let's create the code for direct injection into bookdown


ptemp <- read_csv("/Users/januar/Dropbox (Personal)/R/temp/data/sims13-17cal.csv") %>%
  mutate(month = month.abb[month(time)],  # extract month char using month.abb
    month = factor(month, levels = month.abb)) %>% # order the months
  ggplot() +
  geom_boxplot(aes(month, temp, group = month), 
    alpha = .6, fill = "lightyellow",width = .8, show.legend = F) +
  # scale_fill_manual(values=c(coloura, coloura, coloura, coloura,
  #   coloura, coloura, colourb, colourb,
  #   colourb, colourb, colourb, colourb)) +
  geom_hline(yintercept = 19.66975, linetype = 2, alpha = .5) +  # mean
  geom_hline(yintercept = 19.81, linetype = 3, alpha = .5) +  # median
  xlab("2013 - 2017") +
  ylab("Temperature °C") +
  scale_y_continuous(breaks = seq(0,40,2))

ptemp  

plotly::ggplotly(ptemp)

# -----
log4 %>%
  summarise_each(funs(mean, median, max, min), temp)

# monthly stats
log4 %>%
  mutate(month = month(time)) %>%
  group_by(month) %>%
  summarise_each(funs(median, mean, min, max, max - min), temp)

# yday stats
log4 %>%
  mutate(day = yday(time)) %>%
  group_by(day) %>%
  summarise_each(funs(median, mean, min, max, max - min), temp) %>%
  summarise_each(funs(max, min),`-`)


# year stats
log4 %>%
  mutate(year = year(time)) %>%
  group_by(year) %>%
  summarise_each(funs(median, mean, min, max, max - min), temp)
