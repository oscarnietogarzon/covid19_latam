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

# Percenteage NAs by station
cont_bog %>% 
  group_by(year = year(date), id_station) %>%
  summarize(miss_values = mean(is.na(value))) %>%
  arrange(desc(miss_values))

# Mean values by date
cont_bog %>% filter(lubridate::date(date) > lubridate::ymd("2020-03-01")) %>%
  group_by(date = lubridate::date(date), id_parameter) %>%
  summarise(mean_value = mean(value, na.rm = T)) %>%
  reshape2::dcast(date~id_parameter, value.var = "mean_value") -> cont_bog_diff
#%>% ungroup() %>%
#  mutate( across(CO:O3, ~ .x-lag(.x) ) ) 


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

######################Mobility data
## Google Mobility Index
read_csv("data/gmi.csv", col_names = TRUE) -> gmi
gmi$...1 <- NULL
gmi %>% 
  mutate(sub_region_1 = plyr::revalue(sub_region_1, c("State of São Paulo" = "Sao Paulo"))) -> gmi
colnames(gmi)

unique(gmi$date)

## Waze
read_csv("data/wd.csv", col_names = TRUE) -> waze
waze$...1 <- NULL
waze %>% rename( per_change = "% Change In Waze Driven Miles/KMs") -> waze

waze_mx <- waze %>% filter(City == "Mexico City",
                           date_n <= lubridate::ymd("2020-12-31")) %>%
                    mutate(period_ld = ifelse(date_n > lubridate::dmy("13-05-2020"),
                                              "After LD",
                                       ifelse( date_n < lubridate::dmy("23-03-2020"), "No LD", "LD")))

waze_bg <- waze %>% filter(City == "Bogota",
                           date_n <= lubridate::ymd("2020-12-31")) %>%
                    mutate(period_ld = ifelse(date_n > lubridate::dmy("27-04-2020"),
                                              "After LD",
                                      ifelse( date_n < lubridate::dmy("20-03-2020"), "No LD", "LD")))
                    
waze_sp <- waze %>% filter(City == "Sao Paulo",
                           date_n <= lubridate::ymd("2020-12-31")) %>%
                   mutate(period_ld = ifelse(date_n > lubridate::dmy("20-04-2020"),
                                             "After LD",
                                     ifelse( date_n < lubridate::dmy("22-03-2020"), "No LD", "LD")))

# First differences
#waze_mx %>% mutate(difference = per_change - lag(per_change)) -> waze_mx
waze_bg %>% mutate(difference = per_change - lag(per_change)) -> waze_bg

## Apple
remotes::install_github("joachim-gassen/tidycovid19")
library(tidycovid19)

download_apple_mtr_data(type = "country_city", silent = TRUE, cached = TRUE) %>%
  dplyr::filter(city %in% c("Mexico City", "Bogota", "Sao Paulo") ) -> apple_data

download_apple_mtr_data(type = "country_city", silent = TRUE, cached = TRUE) -> all_apple
all_apple %>% distinct(city) -> cities

############Time series correlation######################
waze_bg %>% filter(date_n <= lubridate::ymd("2020-12-31")) %>%
  left_join(cont_bog_diff, by = c("date_n" = "date") ) -> cor_bog

ggplot(data = cor_bog, aes(x=per_change, y=O3) ) + geom_point()
cor(select(cor_bog, -c("date_n")), method = "pearson", use = "complete.obs")

