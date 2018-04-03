# Load packages
library(tidyverse)
library(huxtable)

## Scope for growth output
sfg_table <- 
  as.data.frame(readxl::read_excel("figs/ch_multistressor/sfg_table.xlsx")) %>%
  hux(add_colnames = TRUE) %>%
  # set_tabular_environment('longtable') %>%
  set_bold(1, everywhere, TRUE) %>%
  set_top_border(1, 1:6, 2) %>%
  set_bottom_border(1, 1:6, 1) %>%
  set_bottom_border(7, 1:6, 1) %>%
  set_position("left") %>%
  set_label('tab:sfg_table') %>%
  set_caption_pos("top") %>%
  set_caption("Testing $X_2$") %>%
  set_latex_float('p')

quick_pdf(sfg_table)



sfg_table <- 
  as.data.frame(readxl::read_excel("figs/ch_multistressor/sfg_table.xlsx")) %>%
  rename("Assimilation Efficiency" = A,
    Feeding = Ce,
    Respiration = Re,
    Excretion = Ue,
    "Scope for Growth" = SfG) %>%
  print()


?rename
