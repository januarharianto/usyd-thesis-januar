library(seacarb)
library(tidyverse)

dt <- readxl::read_excel("assets/data_in/c4_carbchem.xlsx", skip = 4) %>%
  print()

multi <- carb(8, dt$pH_T, dt$AT/1000000, T = dt$tmeasure) %>%
  as_tibble() %>%
  print()

fulldata <- as.tibble(cbind(dt,multi)) %>%
  select(settemp, setpH, Sal, pH, ALK, pCO2, DIC, OmegaCalcite, OmegaAragonite) %>%print()



carbchem <- fulldata %>%
  group_by(settemp, setpH) %>%
  summarise(ph = round(mean(pH),2), sdph = round(sd(pH),2),
    pco2 = round(mean(pCO2),2), sdpco2 = round(sd(pCO2),1),
    dic = round((mean(DIC)*1000000),2), sddic = round((sd(DIC)*1000000),1),
    ca = round(mean(OmegaCalcite),2), sdca = round(sd(OmegaCalcite),1),
    ar = round(mean(OmegaAragonite),2), sdar = round(sd(OmegaAragonite),2),
    at = round(mean(ALK)*1000000, 2), sdat = round((sd(ALK)*1000000),2)) %>% 
  data.table::as.data.table()

carbchem

# need to convert to readable table format

xtable::xtable(carbchem)


carbchem %>% 
  unite("°C|pH", settemp, setpH, sep = " | ") %>%
  unite("pHT", ph, sdph, sep = " ± ") %>%
  unite("pCO2", pco2, sdpco2, sep = " ± ") %>%
  unite("DIC", dic, sddic, sep = " ± ") %>%
  unite("Ca", ca, sdca, sep = " ± ") %>%
  unite("Ar", ar, sdar, sep = " ± ") %>%
  unite("At", at, sdat, sep = " ± ") %>% 
  select(1,2,7,3:5) %>% 
  xtable::xtable() %>%
  print(include.rownames=FALSE)