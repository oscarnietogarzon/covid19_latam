###########################Time series analysis########################
## We have panel data since there is a multidimensional data with measurements over time
pks <-  c("lubridate","zoo","scales","reshape2","tidyverse", "ggthemes", "openair", "tseries")
lapply(pks, require, character.only = TRUE)

waze_bg %>% group_by(period_ld) %>%
  summarise(mean(per_change, na.rm = T))

## stationary tests - Dickey-Fuller test
waze_bg[, 2]
adf.test(waze_bg$per_change)


