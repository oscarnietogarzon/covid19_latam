## packages
pks <-  c("lubridate","zoo","scales","reshape2","tidyverse", "ggthemes", "openair", "rstatix")
lapply(pks, require, character.only = TRUE)
options(scipen=999)

cont_mx %>% select(-ref, -unit) %>%
  mutate(year_group = ifelse(year(date) == 2020, "gr_2020", "gr_2016_19"),
         month_day = format(date,"%m-%d")) %>%
  
  group_by(year_group, month_day, id_station, id_parameter) %>%
  summarize(value_m = mean(value, na.rm = T)) 

cont_bog %>%
  group_by(date, id_parameter) %>%
  summarize(value = mean(value, na.rm = TRUE)) %>%
  reshape2::dcast(date~id_parameter, value.var = "value") %>%
  openair::timePlot(pollutant = c("CO", "NO", "NO2", "NOX", "PM10", "PM2.5", "O3"),
                    avg.time = "1 week",
                    y.relation = "free")

##############Percenteage change by Lockdown Period#######################
## bogota
cont_bog %>%  select(-ref) %>%
  mutate(year_group = ifelse(year(date)==2020, 
                             "gr_2020", 
                             "gr_2016_19"),
         doy = lubridate::yday(date),
         period_ld =  ifelse(doy %in% c(1:79), "P1",  #79 - 20/03/2022
                             ifelse(doy %in% c(80:118),  #80:118 - 27/04/2022
                                    "P2", "P3")),
         ref = format(date,"%m-%d")) %>% 
  group_by(year_group,  id_parameter, id_station, period_ld) %>%
  summarize(value = mean(value, na.rm=T)) %>% ungroup() %>%
  
  reshape2::dcast(id_parameter+id_station+period_ld~year_group, value.var = "value") %>%
  
  #group_by(id_parameter, id_station, period_ld) %>%
  mutate(pct_change = (gr_2020-gr_2016_19)*100/gr_2016_19) %>%
  
  ungroup() %>%
  filter(id_parameter %in% c("CO" , "NOX", "NO2", "O3", "PM10")) %>%
  group_by(id_parameter, period_ld) %>%
  summarize(pct_change = mean(pct_change, na.rm=T)) 

##2
cont_bog %>% 
  mutate(year_group = ifelse(year(date)==2020, 
                             "gr_2020", 
                             "gr_2016_19"),
         doy = lubridate::yday(date),
         ref = format(date,"%m-%d")) %>%
  
  group_by(year_group, ref, id_parameter, id_station) %>%
  summarize(value = mean(value, na.rm=T)) %>% ungroup() %>%
  reshape2::dcast(id_parameter+id_station+ref~year_group, value.var = "value") %>%
  
  mutate(pct_change = (gr_2020-gr_2016_19)/gr_2016_19,
         date = lubridate::mdy(paste0(ref, "-2020")),
         doy = lubridate::yday(date),
         period_ld =  ifelse(doy %in% c(1:79), "P1",  #79 - 20/03/2022
                             ifelse(doy %in% c(80:118),  #80:118 - 27/04/2022
                                    "P2", "P3"))) %>%
  group_by(id_parameter, period_ld) %>%
  summarise(mean_pct = mean(pct_change, na.rm = T)) %>% ungroup() %>%
  filter(id_parameter %in% c("CO" , "NOX", "NO2", "O3", "PM10")) -> dif_bg

## valley of mexico
cont_mx %>%  select(-ref, -unit) %>%
  mutate(year_group = ifelse(year(date)==2020, 
                             "gr_2020", 
                             "gr_2016_19"),
         doy = lubridate::yday(date),
         period_ld =  ifelse(doy %in% c(1:83), "P1",  #83 - 23/03/2020
                             ifelse(doy %in% c(84:134),  #82:134 -  13/05/2022
                                    "P2", "P3")),
         ref = format(date,"%m-%d")) %>% 
  group_by(year_group,  id_parameter, id_station, period_ld) %>%
  summarize(value = mean(value, na.rm=T)) %>% ungroup() %>%
  
  reshape2::dcast(id_parameter+id_station+period_ld~year_group, value.var = "value") %>%
  
  #group_by(id_parameter, id_station, period_ld) %>%
  mutate(pct_change = (gr_2020-gr_2016_19)/gr_2016_19) %>%
  
  ungroup() %>%
  filter(id_parameter %in% c("CO" , "NOX", "NO2", "O3", "PM10")) %>%
  group_by(id_parameter, period_ld) %>%
  summarize(pct_change = mean(pct_change, na.rm=T)) %>% ungroup() %>%
  filter(id_parameter %in% c("CO" , "NOX", "NO2", "O3", "PM10")) 

##2
cont_mx %>% 
  mutate(year_group = ifelse(year(date)==2020, 
                             "gr_2020", 
                             "gr_2016_19"),
         doy = lubridate::yday(date),
         ref = format(date,"%m-%d")) %>%
  group_by(year_group, ref, id_parameter, id_station) %>%
  summarize(value = mean(value, na.rm=T)) %>% ungroup() %>%
  reshape2::dcast(id_parameter+id_station+ref~year_group, value.var = "value") %>%
  
  mutate(pct_change = (gr_2020-gr_2016_19)/gr_2016_19,
         date = lubridate::mdy(paste0(ref, "-2020")),
         doy = lubridate::yday(date),
         period_ld =  ifelse(doy %in% c(1:83), "P1",  #83 - 23/03/2020
                             ifelse(doy %in% c(84:134),  #82:134 -  13/05/2022
                                    "P2", "P3"))) %>%

  group_by(id_parameter, period_ld) %>%
  summarise(mean_pct = mean(pct_change, na.rm = T)) %>% ungroup() %>%
  filter(id_parameter %in% c("CO" , "NOX", "NO2", "O3", "PM10")) -> dif_mx

## sao paulo
cont_sp %>% 
  mutate(year_group = ifelse(year(date)==2020, 
                                       "gr_2020", 
                                       "gr_2016_19"),
                   ref = format(date,"%m-%d")) %>%
  group_by(year_group, ref, id_parameter, id_station) %>%
  summarize(value = mean(value, na.rm=T)) %>% ungroup() %>%
  reshape2::dcast(id_parameter+id_station+ref~year_group, value.var = "value") %>%
  
  mutate(pct_change = (gr_2020-gr_2016_19)/gr_2016_19,
         date = lubridate::mdy(paste0(ref, "-2020")),
         doy = lubridate::yday(date),
         period_ld =  ifelse(doy %in% c(1:82), "P1",  #82 - 22/03/2022
                             ifelse(doy %in% c(83:111),  #83:111 - 20/04/2022
                                    "P2", "P3"))) %>%

  group_by(id_parameter, period_ld) %>%
  summarise(mean_pct = median(pct_change, na.rm = T)) %>% ungroup() %>%
  filter(id_parameter %in% c("CO" , "NOX", "NO2", "O3", "PM10")) -> dif_sp
 
  
cont_sp %>%  select(-ref) %>%
  mutate(year_group = ifelse(year(date)==2020, 
                             "gr_2020", 
                             "gr_2016_19"),
         doy = lubridate::yday(date),
         period_ld =  ifelse(doy %in% c(1:81), "P1",  #82 - 22/03/2022
                             ifelse(doy %in% c(82:111),  #82:111 - 20/04/2022
                                    "P2", "P3")),
         ref = format(date,"%m-%d")) %>% 
  group_by(year_group,  id_parameter, id_station, period_ld) %>%
  summarize(value = mean(value, na.rm=T)) %>% ungroup() %>%
  
  reshape2::dcast(id_parameter+id_station+period_ld~year_group, value.var = "value") %>%
  
  #group_by(id_parameter, id_station, period_ld) %>%
  mutate(pct_change = (gr_2020-gr_2016_19)/gr_2016_19) %>%
  
  ungroup() %>%
  filter(id_parameter %in% c("CO" , "NOX", "NO2", "O3", "PM10")) %>%
  group_by(id_parameter, period_ld) %>%
  summarize(pct_change = mean(pct_change, na.rm=T)) 

#################Concatenate the datasets####
dif_bg %>% mutate(city = "Bogota") -> dif_bg
dif_mx %>% mutate(city = "MZMC") -> dif_mx 
dif_sp %>% mutate(city = "MASP") -> dif_sp 

bind_rows(dif_bg, dif_mx, dif_sp) -> dot_plot_df
dot_plot_df %>% 
  mutate(city = factor(city, levels = c("MZMC", "MASP", "Bogota")),
          id_parameter = factor(id_parameter, levels = c("PM10", "O3", "NOX", "NO2", "CO")))

unique(dot_plot_df$id_parameter)

#################Dot plots###################
labels_dot = c( "PM10" = bquote(''*PM[10]*''), "O3" = bquote(''*O[3]*''),  "NOX" = bquote(''*NO[x]*''), 
                "NO2" = bquote(''*NO[2]*''), "CO" = "CO")

as_labeller(labels_dot) -> lab_id_paramters
dot_plot_df %>% 
  mutate(city = factor(city, levels = c("MZMC", "MASP", "Bogota")),
         id_parameter = factor(id_parameter, levels = c("PM10", "CO", "O3", "NOX", "NO2")),
         period_ld = factor(period_ld, levels = c("P1", "P2", "P3"))) %>%
  
ggplot(aes(mean_pct , id_parameter)) +
  geom_point(aes(colour = period_ld), size = 2.5, shape = 16, stroke = 1) +
  paletteer::scale_colour_paletteer_d("ggthemes::wsj_rgby") +
  geom_vline(data = data.frame(city = unique(dot_plot_df$city),
                               hline = c(0, 0, 0)), aes(xintercept = hline), size = 0.45) +
  scale_x_continuous(labels = scales::percent, expand = c(0.02, 0), 
                     limits = c(-1, 1.5),
                     breaks = seq(-1, 1.2, by = 0.2)) +
  scale_y_discrete(expand = c(.02, 0), labels = labels_dot) +
  facet_grid(rows = vars(city), scales = "free", switch = "y", space = "free_y") +

  theme_minimal() +
  theme(strip.placement = "outside",
        panel.grid.major = element_line(linetype = "dashed", colour = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.direction="horizontal",
        legend.position="bottom",
        text = element_text( family = "Times New Roman",
                            size = 12)) +
  
  labs(x = "Percentage changes (%)",
       y = "")

ggsave('dot_plot.png', dpi=300, width = 5.5, height = 6.5)

#################Bar plot##################
library(paletteer)
labels_dot = c( "PM10" = bquote(''*PM[10]*''), "O3" = bquote(''*O[3]*''),  "NOX" = bquote(''*NO[x]*''), 
                "NO2" = bquote(''*NO[2]*''), "CO" = "CO")


dot_plot_df %>% 
  mutate(city = factor(city, levels = c("MZMC", "MASP", "Bogota")),
         id_parameter = factor(id_parameter, levels = c("PM10", "CO", "O3", "NOX", "NO2")),
         period_ld = factor(period_ld, levels = c("P1", "P2", "P3"))) %>%
  
  ggplot(aes(y=mean_pct, x= city, fill=period_ld )) +
  geom_bar(position="dodge", stat="identity") +
  paletteer::scale_fill_paletteer_d("ggthemes::wsj_rgby") +
  
  facet_wrap(vars(id_parameter), ncol = 2, scales = "free") +
  coord_flip() +

  scale_y_continuous(labels = scales::percent, expand = c(0.02, 0), 
                   limits = c(-1, 1.3),
                   breaks = seq(-1, 1.3, by = 0.25)) +
  scale_x_discrete(expand = c(.02, 0), labels = as_labeller(labels_dot) ) +
  #guides(fill=FALSE) +
  
  theme_minimal() +
  theme(strip.placement = "outside",
        panel.grid.major = element_line(linetype = "dashed", colour = "grey80"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.direction="horizontal",
        legend.position="bottom",
        text = element_text( family = "Times New Roman",
                             size = 12)) +
  
  labs(x = "Percentage changes (%)",
       y = "")

ggsave('bar_plot.png', dpi=300, width = 5.5, height = 6.5)

#############Correlation##############################
## data preparation
cont_bog %>% filter(lubridate::date(date) > lubridate::ymd("2020-02-01")) %>%
  group_by(date = lubridate::date(date), id_parameter) %>%
  summarise(mean_value = mean(value, na.rm = T)) %>%
  reshape2::dcast(date~id_parameter, value.var = "mean_value") %>%
  mutate(City = "Bogota") -> temp_bog

cont_mx %>% filter(lubridate::date(date) > lubridate::ymd("2020-02-01") ) %>%
  group_by(date = lubridate::date(date), id_parameter) %>%
  summarise(mean_value = mean(value, na.rm = T)) %>%
  reshape2::dcast(date~id_parameter, value.var = "mean_value") %>%
  mutate(City = "Mexico City") -> temp_mx

cont_sp %>% filter(lubridate::date(date) > lubridate::ymd("2020-02-01")) %>%
  group_by(date = lubridate::date(date), id_parameter) %>%
  summarise(mean_value = mean(value, na.rm = T)) %>%
  reshape2::dcast(date~id_parameter, value.var = "mean_value") %>%
  mutate(City = "Sao Paulo") -> temp_sp

bind_rows(temp_mx, temp_sp, temp_bog) %>%
  arrange(date, City) -> temp_cont

mob_df %>% filter(City == "Mexico City") %>%
  reshape2::dcast( date + City ~ variable, value.var = "per_change") %>%
  left_join(temp_cont, by = c("date" = "date", "City" = "City") ) %>%
  rename(grocery = grocery_and_pharmacy_percent_change_from_baseline,
         retail = retail_and_recreation_percent_change_from_baseline,
         parks = parks_percent_change_from_baseline,
         transit_stations = transit_stations_percent_change_from_baseline,
         workplaces = workplaces_percent_change_from_baseline, 
         residential = residential_percent_change_from_baseline) %>% 
  
  select(City, CO, NO2, O3, PM10, driven_miles_km, transit_stations, workplaces, residential) %>% 
  split(list(.$City)) %>%
  
  map( ~rstatix::cor_mat(data=., vars = c(2:9), method = "spearman" ) )

## Matrix correlations
mob_df %>% filter(City == "Mexico City") %>%
  reshape2::dcast( date + City ~ variable, value.var = "per_change") %>%
  left_join(temp_mx, by = c("date" = "date", "City" = "City") ) %>%
  rename(grocery = grocery_and_pharmacy_percent_change_from_baseline,
         retail = retail_and_recreation_percent_change_from_baseline,
         parks = parks_percent_change_from_baseline,
         transit_stations = transit_stations_percent_change_from_baseline,
         workplaces = workplaces_percent_change_from_baseline, 
         residential = residential_percent_change_from_baseline) %>%
  select(CO, NOX, NO2, O3, PM10, driven_miles_km, transit_stations, workplaces, residential) %>%
  rstatix::cor_mat(method = "spearman" ) -> cor_mx

mob_df %>% filter(City == "Sao Paulo") %>%
  reshape2::dcast( date + City ~ variable, value.var = "per_change") %>%
  left_join(temp_sp, by = c("date" = "date", "City" = "City") ) %>%
  rename(grocery = grocery_and_pharmacy_percent_change_from_baseline,
         retail = retail_and_recreation_percent_change_from_baseline,
         parks = parks_percent_change_from_baseline,
         transit_stations = transit_stations_percent_change_from_baseline,
         workplaces = workplaces_percent_change_from_baseline, 
         residential = residential_percent_change_from_baseline) %>%
  select(CO, NOX, NO2, O3, PM10, driven_miles_km, transit_stations, workplaces, residential) %>%
  rstatix::cor_mat(method = "spearman" ) -> cor_sp

mob_df %>% filter(City == "Bogota") %>%
  reshape2::dcast( date + City ~ variable, value.var = "per_change") %>%
  left_join(temp_bog, by = c("date" = "date", "City" = "City") ) %>%
  rename(grocery = grocery_and_pharmacy_percent_change_from_baseline,
         retail = retail_and_recreation_percent_change_from_baseline,
         parks = parks_percent_change_from_baseline,
         transit_stations = transit_stations_percent_change_from_baseline,
         workplaces = workplaces_percent_change_from_baseline, 
         residential = residential_percent_change_from_baseline) %>%
  select(CO, NOX, NO2, O3, PM10, driven_miles_km, transit_stations, workplaces, residential) %>%
  rstatix::cor_mat(method = "spearman" ) -> cor_bog

cor_sp %>% rstatix::cor_get_pval() < 0.05

## Final output
cor_mx %>% filter(rowname %in% c("driven_miles_km", "transit_stations", "residential", "workplaces")) %>%
  select(rowname, CO, NOX, NO2, O3, PM10) %>% mutate(City = "Mexico") %>%
  
  bind_rows( cor_sp %>% filter(rowname %in% c("driven_miles_km", "transit_stations", "residential", "workplaces")) %>%
  select(rowname, CO, NOX, NO2, O3, PM10) %>% mutate(City = "Sao Paulo") ) %>%
  
  bind_rows( cor_bog %>% filter(rowname %in% c("driven_miles_km", "transit_stations", "residential", "workplaces")) %>%
  select(rowname, CO, NOX, NO2, O3, PM10) %>% mutate(City = "Bogota") ) %>%
  
  write_csv("out/cor_data.csv") 

#############ANOVA periods ##############################
params_int <- c("PM10", "CO", "NO2", "O3")
# Bogota
cont_bog %>%  select(-ref) %>%
  mutate(year_group = ifelse(year(date)==2020, 
                             "gr_2020", 
                             "gr_2016_19"),
         doy = lubridate::yday(date),
         period_ld =  ifelse(doy %in% c(1:79), "No LD",  #79 - 20/03/2022
                             ifelse(doy %in% c(80:118),  #80:118 - 27/04/2022
                                    "LD March and April", "After LD")) ) %>%
  
  filter( year_group ==  "gr_2020" ) %>%
  select(-c("year_group")) %>%
  
  split(list(.$id_parameter)) %>%
  map( ~kruskal_test(data=.,  period_ld~value ) ) %>%
  map_dfr(~ as.data.frame(as.matrix(.)), .id="ref")

# Valley of Mexico
cont_mx %>%  select(-ref) %>%
  mutate(year_group = ifelse(year(date)==2020, 
                             "gr_2020", 
                             "gr_2016_19"),
         doy = lubridate::yday(date),
         period_ld =  ifelse(doy %in% c(1:79), "No LD",  #79 - 20/03/2022
                             ifelse(doy %in% c(80:118),  #80:118 - 27/04/2022
                                    "LD March and April", "After LD")) ) %>%
  
  filter( year_group ==  "gr_2020" ) %>%
  select(-c("year_group")) %>%
  
  split(list(.$id_parameter)) %>%
  map( ~kruskal_test(data=.,  period_ld~value ) ) %>%
  map_dfr(~ as.data.frame(as.matrix(.)), .id="ref")

# Sao paulo
cont_sp %>%  select(-ref) %>%
  mutate(year_group = ifelse(year(date)==2020, 
                             "gr_2020", 
                             "gr_2016_19"),
         doy = lubridate::yday(date),
         period_ld =  ifelse(doy %in% c(1:81), "P1",  #82 - 22/03/2022
                             ifelse(doy %in% c(82:111),  #82:111 - 20/04/2022
                                    "P2", "P3"))) %>%
  
  filter( year_group ==  "gr_2020" ) %>%
  select(-c("year_group")) %>%
  
  split(list(.$id_parameter)) %>%
  map( ~kruskal_test(data=.,  period_ld~value ) ) %>%
  map_dfr(~ as.data.frame(as.matrix(.)), .id="ref")

################### Dunn test ###########################
##Between LD periods
# Valley of Mexico
cont_mx %>%  select(-ref) %>%
  mutate(year_group = ifelse(year(date)==2020, 
                             "gr_2020", 
                             "gr_2016_19"),
         doy = lubridate::yday(date),
         period_ld =  ifelse(doy %in% c(1:83), "P1",  #83 - 23/03/2020
                             ifelse(doy %in% c(84:134),  #82:134 -  13/05/2022
                                    "P2", "P3")) ) %>%
  filter( year_group ==  "gr_2020" ) %>%
  group_by(id_parameter) %>%
  dunn_test(data =., value~period_ld, p.adjust.method = "holm" ) -> dunn_mx

## sao paulo
cont_sp %>%  select(-ref) %>%
  mutate(year_group = ifelse(year(date)==2020, 
                             "gr_2020", 
                             "gr_2016_19"),
         doy = lubridate::yday(date),
         period_ld =  ifelse(doy %in% c(1:81), "P1",  #82 - 22/03/2022
                             ifelse(doy %in% c(82:111),  #82:111 - 20/04/2022
                                    "P2", "P3"))) %>%
  filter( year_group ==  "gr_2020" ) %>%
  group_by(id_parameter) %>%
  dunn_test(data =., value~period_ld, p.adjust.method = "holm" ) -> dunn_sp

## Bogota
cont_bog %>%  select(-ref) %>%
  mutate(year_group = ifelse(year(date)==2020, 
                             "gr_2020", 
                             "gr_2016_19"),
         doy = lubridate::yday(date),
         period_ld =  ifelse(doy %in% c(1:79), "P1",  #79 - 20/03/2022
                             ifelse(doy %in% c(80:118),  #80:118 - 27/04/2022
                                    "P2", "P3")) ) %>%  
        filter( year_group ==  "gr_2020" ) %>% 
        group_by( id_parameter ) %>%
  
  dunn_test(data =., value~period_ld, p.adjust.method = "holm" ) -> dunn_bog

dunn_mx %>% mutate(City = "MCMA") -> dunn_mx
dunn_sp %>% mutate(City = "MASP") -> dunn_sp
dunn_bog  %>% mutate(City = "Bogota") -> dunn_bog

bind_rows(dunn_mx, dunn_sp, dunn_bog) -> dunn_df
dunn_df %>%  write_csv("out/dunn_data.csv") 

#############Wilcox baseline ##############################
params_int <- c("PM10", "CO", "NO2", "O3")
# Bogota
cont_bog %>%  select(-ref) %>%
  mutate(year_group = ifelse(year(date)==2020, 
                             "gr_2020", 
                             "gr_2016_19"),
         doy = lubridate::yday(date),
         period_ld =  ifelse(doy %in% c(1:79), "No LD",  #79 - 20/03/2022
                             ifelse(doy %in% c(80:118),  #80:118 - 27/04/2022
                                    "LD March and April", "After LD")) ) %>%
  group_by(id_parameter, period_ld) %>%
  wilcox_test(data=.,  value~year_group) %>%
  adjust_pvalue(method = "holm") %>%
  add_significance("p.adj") -> wil_bog
  
# Valley of Mexico
cont_mx %>%  select(-ref) %>%
  mutate(year_group = ifelse(year(date)==2020, 
                             "gr_2020", 
                             "gr_2016_19"),
         doy = lubridate::yday(date),
         period_ld =  ifelse(doy %in% c(1:83), "P1",  #83 - 23/03/2020
                             ifelse(doy %in% c(84:134),  #82:134 -  13/05/2022
                                    "P2", "P3")) ) %>%
  group_by(date = date(date), year_group,  id_parameter, id_station, period_ld) %>%
  summarize(value = median(value, na.rm=T)) %>%
  
  group_by(id_parameter, period_ld) %>%
  wilcox_test(data=.,  value~year_group) %>%
  adjust_pvalue(method = "holm") %>%
  add_significance("p.adj") -> wil_mx

# Sao paulo
cont_sp %>%  select(-ref) %>%
  mutate(year_group = ifelse(year(date)==2020, 
                             "gr_2020", 
                             "gr_2016_19"),
         doy = lubridate::yday(date),
         period_ld =  ifelse(doy %in% c(1:81), "P1",  #82 - 22/03/2022
                             ifelse(doy %in% c(82:111),  #82:111 - 20/04/2022
                                    "P2", "P3"))) %>%
  
  group_by(id_parameter, period_ld) %>%
  wilcox_test(data=.,  value~year_group) %>%
  adjust_pvalue(method = "holm") %>%
  add_significance("p.adj") -> wil_sp

wil_mx %>% mutate(City = "MCMA") -> wil_mx
wil_sp %>% mutate(City = "MASP") -> wil_sp
wil_bog  %>% mutate(City = "Bogota") -> wil_bog

bind_rows(wil_mx, wil_sp, wil_bog) -> wil_df
wil_df %>%  write_csv("out/wil_data.csv") 
