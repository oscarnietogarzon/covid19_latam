## packages
pks <-  c("lubridate","zoo","scales","reshape2","tidyverse", "ggthemes")
lapply(pks, require, character.only = TRUE)

## load data from Rdata
## bogota
load(file = c("./data/cont_bog.RData"))
cont.final -> cont_bog
str(cont_bog)
sapply(cont_bog, class) #check columns type

cont_bog %>%
  mutate(id_parameter = plyr::revalue(id_parameter, c("OZONO" = "O3"))) -> cont_bog

cont_bog %>% 
  group_by(year = year(date), id_station) %>%
  summarize(miss_values = mean(is.na(value))) %>%
  arrange(desc(miss_values))

## sao paulo
load(file = c("./data/cont_sp.RData"))
cont_sp %>% arrange(date, id_station, id_parameter) -> cont_sp #order by date
str(cont_sp)

cont_sp %>% 
  group_by(year = year(date), id_station) %>%
  summarize(miss_values = mean(is.na(value))) %>%
  arrange(desc(miss_values))

## valley of mexico
load(file = c("./data/zmvm.RData"))
cont.final -> cont_mx
rm(cont.final)

cont_mx %>% 
  group_by(year = year(date), id_station) %>%
  summarize(miss_values = mean(is.na(value))) %>%
  arrange(desc(miss_values))
