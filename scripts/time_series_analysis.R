###########################Time series analysis########################
## We have panel data since there is a multidimensional data with measurements over time
pks <-  c("lubridate","zoo","scales","reshape2","tidyverse", "ggthemes", "openair", "tseries")
lapply(pks, require, character.only = TRUE)

waze_bg %>% group_by(period_ld) %>%
  summarise(mean(per_change, na.rm = T))

## stationary tests - Dickey-Fuller test
waze_bg[, 2]
adf.test(waze_bg$per_change)

################NOX - O3 - VOC sensitivity
cont_sp %>% select(-ref) %>% filter(lubridate::year(date) %in% c(2017, 2018, 2019, 2020),
                                    #id_station %in% c("MER", "TLA", "CCA", "UAX"),
                                    id_parameter %in% c("O3", "NOX"),
                                    between(lubridate::hour(date), 13, 17  )) %>%
  
    #reshape2::dcast(date+id_station~id_parameter, value.var = "value") %>%
    group_by(date = lubridate::date(date), id_station, id_parameter) %>%
    #filter(O3 == median(O3)) %>% ungroup() %>%
    
    summarise(value = median(value, na.rm = T)) %>% ungroup() %>%
    reshape2::dcast(date+id_station~id_parameter, value.var = "value") %>%

    mutate(doy = lubridate::yday(date),
    period_ld =  ifelse(doy %in% c(1:81), "Pre Lockdown",  #82 - 22/03/2022
    ifelse(doy %in% c(82:111),  #82:111 - 20/04/2022
                "First-Phase\n Lockdown", "Second-Phase\n Lockdown")), 
    ratio = O3/NOX) %>%
  
  filter(period_ld == "First-Phase\n Lockdown", ratio > 8) %>%
  
  ggplot(aes(y = O3, x = NOX, colour =  period_ld) )  + geom_point(size = 1, alpha = 0.8, shape = 21) +
  geom_abline(intercept = 0, slope = 8, size = 0.55) +
  scale_y_continuous(limits = c(0, 140), expand = c(0,0), breaks = seq(0, 140, 20)) +
  scale_x_continuous(limits = c(0, 60), expand = c(0,0), breaks = seq(0, 60, 10)) +
  facet_grid(rows = vars(lubridate::year(date)), scales = "free", space = "free_y") +
  
  theme_few() +
  theme(strip.placement = "outside",
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.direction="horizontal",
        legend.position="bottom",
        text = element_text(family = "Georgia"),
        plot.title = element_text(size = 14, margin = margin(b = 10), hjust = 0.5))
  
################ Weekends ############################
## Holidays
read_csv("data/hol_co.csv") -> hol_co
read_csv("data/hol_sp.csv") -> hol_sp
read_csv("data/hol_mx.csv") -> hol_mx

## data
## bogota
cont_bog %>%  select(-ref) %>%
  mutate(year_group = ifelse(year(date)==2020, "gr_2020", "gr_2016_19"),
         doy = lubridate::yday(date),
         period_ld =  ifelse(doy %in% c(1:79), "Pre Lockdown",  #79 - 20/03/2022
                             ifelse(doy %in% c(80:118),  #80:118 - 27/04/2022
                                    "First-Phase\n Lockdown", "Second-Phase\n Lockdown")),
         ref = format(date,"%m-%d"),
         holidays = ifelse(date(date) %in% lubridate::ymd(hol_co$date), TRUE, FALSE),
         weekend = ifelse( format(as.Date(date), "%a") %in% c("Sat", "Sun"), TRUE, FALSE ),
         wday = ifelse(holidays == TRUE | weekend == TRUE , TRUE, FALSE) ) %>%
  
  group_by(date = date(date), year_group,  id_parameter, id_station, period_ld, wday)  %>%
  summarize(value = mean(value, na.rm=T)) %>%
  reshape2::dcast(date+id_parameter+id_station+year_group+wday ~ period_ld , value.var = "value")  -> cont_w_bog

cont_w_bog %>% 
  reshape2::melt(id.vars = c("date", "id_parameter", "id_station", "year_group", "wday")) %>%
  
  group_by(year_group, variable) %>%
  
  wilcox_test(data=.,  value~wday) %>%
  adjust_pvalue(method = "holm") %>%
  add_significance("p.adj") 

## valley of mexico
cont_mx %>%  select(-ref, -unit) %>% filter(id_parameter=="O3") %>%
mutate(year_group = ifelse(year(date)==2020, 
                             "gr_2020", 
                             "gr_2016_19"),
         doy = lubridate::yday(date),
         period_ld =  ifelse(doy %in% c(1:83), "Pre Lockdown",  #83 - 23/03/2020
                             ifelse(doy %in% c(84:134),  #82:134 -  13/05/2022
                                    "First-Phase\n Lockdown", "Second-Phase\n Lockdown")),
         ref = format(date,"%m-%d"),
         holidays = ifelse(date(date) %in% lubridate::ymd(hol_mx$date), TRUE, FALSE),
         weekend = ifelse( format(as.Date(date), "%a") %in% c("Sat", "Sun"), TRUE, FALSE ),
         wday = ifelse(holidays == TRUE | weekend == TRUE , TRUE, FALSE) ) %>%
  
  group_by(date = date(date), year_group,  id_parameter, id_station, period_ld, wday)  %>%
  summarize(value = mean(value, na.rm=T)) %>%
  reshape2::dcast(date+id_parameter+id_station+year_group+wday ~ period_ld , value.var = "value") -> cont_w_mx

cont_w_mx %>% 
  reshape2::melt(id.vars = c("date", "id_parameter", "id_station", "year_group", "wday")) %>%

  group_by(year_group, variable) %>%
  
  wilcox_test(data=.,  value~wday) %>%
  adjust_pvalue(method = "holm") %>%
  add_significance("p.adj") 

## sao paulo
cont_sp %>%  select(-ref) %>% filter(id_parameter=="O3") %>%
  mutate(year_group = ifelse(year(date)==2020, 
                             "gr_2020", 
                             "gr_2016_19"),
         doy = lubridate::yday(date),
         period_ld =  ifelse(doy %in% c(1:81), "Pre Lockdown",  #82 - 22/03/2022
                             ifelse(doy %in% c(82:111),  #82:111 - 20/04/2022
                                    "First-Phase\n Lockdown", "Second-Phase\n Lockdown")),
         ref = format(date,"%m-%d"),
         holidays = ifelse(date(date) %in% lubridate::ymd(hol_sp$date), TRUE, FALSE),
         weekend = ifelse( format(as.Date(date), "%a") %in% c("Sat", "Sun"), TRUE, FALSE ),
         wday = ifelse(holidays == TRUE | weekend == TRUE , TRUE, FALSE) ) %>%
  
  group_by(date = date(date), year_group,  id_parameter, id_station, period_ld, wday)  %>%
  summarize(value = mean(value, na.rm=T)) %>%
  reshape2::dcast(date+id_parameter+id_station+year_group+wday ~ period_ld , value.var = "value") -> cont_w_sp

cont_w_sp %>% 
  reshape2::melt(id.vars = c("date", "id_parameter", "id_station", "year_group", "wday")) %>%
  
  group_by(year_group, variable) %>%
  
  wilcox_test(data=.,  value~wday) %>%
  adjust_pvalue(method = "holm") %>%
  add_significance("p.adj") 

## comparison 2016-2019 to COVID
cont_w_mx %>% 
  reshape2::melt(id.vars = c("date", "id_parameter", "id_station", "year_group", "wday")) %>%
  
  group_by(year_group, variable, wday) %>%
  summarize(value = mean(value, na.rm=T)) %>% 
  reshape2::dcast(year_group+variable~wday, value.var = "value") %>%
  group_by(variable) %>%
  mutate(pct_change = (lag(`TRUE`) - `TRUE`)/`TRUE`* 100) 

## 8h mean
cont_mx %>% filter(id_parameter == "O3") %>%
  group_by(id_station) %>%
  mutate(O3_8h = zoo::rollmean(value, k=8, align = "right", fill = NA)) %>%
  ungroup() %>%
  mutate(year_group = ifelse(year(date)==2020, 
                             "gr_2020", 
                             "gr_2016_19"),
         doy = lubridate::yday(date),
         period_ld =  ifelse(doy %in% c(1:83), "Pre Lockdown",  #83 - 23/03/2020
                             ifelse(doy %in% c(84:134),  #82:134 -  13/05/2022
                                    "First-Phase\n Lockdown", "Second-Phase\n Lockdown")),

         ref = format(date,"%m-%d") ) %>%
  
  group_by(year_group, period_ld) %>%
  summarize(max_O3_8h = median(O3_8h, na.rm = T)) %>% ungroup()
  #reshape2::dcast(year_group+period_ld~id_station, value.var = "max_O3_8h")

#15%

## 8h mean sp
cont_sp %>% filter(id_parameter == "O3") %>%
  group_by(id_station) %>%
  mutate(O3_8h = zoo::rollmean(value, k=8, align = "right", fill = NA)) %>%
  ungroup() %>%
  mutate(year_group = ifelse(year(date)==2020, 
                             "gr_2020", 
                             "gr_2016_19"),
         doy = lubridate::yday(date),
         period_ld =  ifelse(doy %in% c(1:79), "Pre Lockdown",  #79 - 20/03/2022
                             ifelse(doy %in% c(80:118),  #80:118 - 27/04/2022
                                    "First-Phase\n Lockdown", "Second-Phase\n Lockdown")),
         
         ref = format(date,"%m-%d") ) %>%
  
  group_by(year_group, period_ld) %>%
  summarize(m_O3_8h = median(O3_8h, na.rm = T)) %>% ungroup()
#reshape2::dcast(year_group+period_ld~id_station, value.var = "max_O3_8h")


#28%

cont_bog %>% filter(id_parameter == "O3") %>%
  group_by(id_station) %>%
  mutate(O3_8h = zoo::rollmean(value, k=8, align = "right", fill = NA)) %>%
  ungroup() %>%
  mutate(year_group = ifelse(year(date)==2020, 
                             "gr_2020", 
                             "gr_2016_19"),
         doy = lubridate::yday(date),
         period_ld =  ifelse(doy %in% c(1:79), "Pre Lockdown",  #79 - 20/03/2022
                              ifelse(doy %in% c(80:118),  #80:118 - 27/04/2022
                                     "First-Phase\n Lockdown", "Second-Phase\n Lockdown")),
         
         ref = format(date,"%m-%d") ) %>%
  
  group_by(year_group, period_ld) %>%
  summarize(max_O3_8h = median(O3_8h, na.rm = T)) %>% ungroup()
#reshape2::dcast(year_group+period_ld~id_station, value.var = "max_O3_8h")


