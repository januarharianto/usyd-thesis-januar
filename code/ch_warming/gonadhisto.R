if(!require(psych)){install.packages("psych")}
if(!require(betareg)){install.packages("betareg")}
if(!require(car)){install.packages("car")}
if(!require(lmtest)){install.packages("lmtest")}
if(!require(rcompanion)){install.packages("rcompanion")}
if(!require(multcompView)){install.packages("multcompView")}
if(!require(lsmeans)){install.packages("lsmeans")}
if(!require(ggplot2)){install.packages("ggplot2")}


# Create dataset locally (since it is small)
# total - total samples
# spent - gonads are spent;no gametes
# partial - gonads are partially spent, with some gametes
# ALL samples are post-spawn
gonad.hist <- data.frame(treatment = c("20", "22", "24", "26"),
  total = c(15,14,15,11),
  spent = c(5,4,4,2),
  partial = c(10,10,11,9)) %>%
  tibble::as.tibble() %>%
  print()



