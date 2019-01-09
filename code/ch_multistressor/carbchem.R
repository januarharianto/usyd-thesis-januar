library(seacarb)
library(tidyverse)

dt <- readxl::read_excel("input/ch_multistressor/carbonate_chem.xlsx", 2, skip = 1) %>%
  print()

multi <- carb(8, dt$pH_T, dt$AT/1000000, T = dt$tmeasure) %>%
  as.tibble() %>%
  print()

fulldata <- as.tibble(cbind(dt,multi)) %>%
  select(settemp, setpH, Sal, pH, ALK, pCO2, DIC, OmegaCalcite, OmegaAragonite) %>%print()



 carbchem <- fulldata %>%
  group_by(settemp, setpH) %>%
  summarise(ph = round(mean(pH),2), sdph = round(sd(pH),2),
    pco2 = round(mean(pCO2),2), sdpco2 = round(sd(pCO2),2),
    dic = round((mean(DIC)*1000000),2), sddic = round((sd(DIC)*1000000),2),
    ca = round(mean(OmegaCalcite),2), sdca = round(sd(OmegaCalcite),2),
    ar = round(mean(OmegaAragonite),2), sdar = round(sd(OmegaAragonite),2),
    at = round(mean(ALK)*1000000, 2), sdat = round((sd(ALK)*1000000),2)) %>% 
   data.table::as.data.table()
 
 carbchem
 
 write_csv(carbchem, "input/ch_multistressor/carbchemoutput.csv")
 
 
