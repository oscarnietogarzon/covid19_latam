## packages
pks <-  c("lubridate","zoo","scales","reshape2","tidyverse", "ggthemes", "openair", "gridExtra")
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

c("CO", "O3", "NO2") -> pollutants
for (value in pollutants) {
  print(value)
}

cont_mx %>% filter( id_parameter %in% c("O3", "NO2") ) %>%
  group_by(id_parameter, date) %>% summarize(daily_m = mean(value, na.rm = T)) %>%
  reshape2::dcast(date~id_parameter, value.var = "daily_m") %>%

  splitByDate(dates = c("20/3/2020" , "1/5/2020"),
            labels = c("before", "during", "after") ) %>%
            
  timeVariation(pollutant = "NO2", ylab = "NO2 (ppb)", group = "split.by",
                conf.int = c(0.95), cols = "increment")-> plot_comp_mx


grid.arrange(plot_comp_mx$plot$hour, plot_comp_mx$plot$day, ncol = 2) -> grid1
grid.arrange(plot_comp_mx$plot$hour, plot_comp_mx$plot$day, ncol = 2) -> grid2
grid.arrange(grid1, grid2)
?timeVariation

###### Highest value per pollutant##############
