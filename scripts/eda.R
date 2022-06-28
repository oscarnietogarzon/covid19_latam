## packages
pks <-  c("lubridate","zoo","scales","reshape2","tidyverse", "ggthemes", "openair")
lapply(pks, require, character.only = TRUE)

cont_bog %>% select(-ref) %>%
  mutate(week = cut(date, "week", start.on.monday = TRUE)) %>%
  group_by(id_station, id_parameter, week) %>%
  
  summarize(weekly_m = mean(value, na.rm = T)) %>% ungroup() %>%
  mutate(week = lubridate::ymd(week)) %>%
  
  
  ggplot(aes(x=week, y=weekly_m, colour=id_station)) +
  geom_line(size=0.05, linetype ='solid', alpha = 0.85) +
  facet_wrap( ~id_parameter, nrow = 3, scales = "free") 

###############Open Air###############################
cont_mx %>% filter(id_parameter == "O3") %>%
  group_by(date) %>%  summarize(daily_m = mean(value, na.rm = T)) %>%

  splitByDate(dates = c("20/3/2020" , "1/5/2020"),
            labels = c("before", "during", "after") ) %>%
  
  timeVariation(pollutant = "daily_m", 
                group = "split.by",
                ylab = "O3 (ppb)",
                col = "firebrick")


