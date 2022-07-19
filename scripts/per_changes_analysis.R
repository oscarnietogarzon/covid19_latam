## packages
pks <-  c("lubridate","zoo","scales","reshape2","tidyverse", "ggthemes", "openair", "rstatix")
lapply(pks, require, character.only = TRUE)

cont_mx %>% select(-ref, -unit) %>%
  mutate(year_group = ifelse(year(date) == 2020, "gr_2020", "gr_2016_19"),
         month_day = format(date,"%m-%d")) %>%
  
  group_by(year_group, month_day, id_station, id_parameter) %>%
  summarize(value_m = mean(value, na.rm = T)) 

##############Percenteage change by Lockdown Period#######################
## bogota
cont_bog %>%  select(-ref) %>%
  mutate(year_group = ifelse(year(date)==2020, 
                             "gr_2020", 
                             "gr_2016_19"),
         doy = lubridate::yday(date),
         period_ld =  ifelse(doy %in% c(1:79), "Pre Lockdown",  #79 - 20/03/2022
                             ifelse(doy %in% c(80:118),  #80:118 - 27/04/2022
                                    "First-Phase\n Lockdown", "Second-Phase\n Lockdown")),
         ref = format(date,"%m-%d")) %>% 
  group_by(year_group,  id_parameter, id_station, period_ld) %>%
  summarize(value = mean(value, na.rm=T)) %>% ungroup() %>%
  
  reshape2::dcast(id_parameter+id_station+period_ld~year_group, value.var = "value") %>%
  
  #group_by(id_parameter, id_station, period_ld) %>%
  mutate(pct_change = (gr_2020-gr_2016_19)/gr_2016_19) %>%
  
  ungroup() %>%
  filter(id_parameter %in% c("CO" , "NOX", "NO2", "O3", "PM10")) %>%
  group_by(id_parameter, period_ld) %>%
  summarize(pct_change = mean(pct_change, na.rm=T)) -> dif_bg

## valley of mexico
cont_mx %>%  select(-ref, -unit) %>%
  mutate(year_group = ifelse(year(date)==2020, 
                             "gr_2020", 
                             "gr_2016_19"),
         doy = lubridate::yday(date),
         period_ld =  ifelse(doy %in% c(1:83), "Pre Lockdown",  #83 - 23/03/2020
                             ifelse(doy %in% c(84:134),  #82:134 -  13/05/2022
                                    "First-Phase\n Lockdown", "Second-Phase\n Lockdown")),
         ref = format(date,"%m-%d")) %>% 
  group_by(year_group,  id_parameter, id_station, period_ld) %>%
  summarize(value = mean(value, na.rm=T)) %>% ungroup() %>%
  
  reshape2::dcast(id_parameter+id_station+period_ld~year_group, value.var = "value") %>%
  
  #group_by(id_parameter, id_station, period_ld) %>%
  mutate(pct_change = (gr_2020-gr_2016_19)/gr_2016_19) %>%
  
  ungroup() %>%
  filter(id_parameter %in% c("CO" , "NOX", "NO2", "O3", "PM10")) %>%
  group_by(id_parameter, period_ld) %>%
  summarize(pct_change = mean(pct_change, na.rm=T)) -> dif_mx

## sao paulo
cont_sp %>%  select(-ref) %>%
  mutate(year_group = ifelse(year(date)==2020, 
                             "gr_2020", 
                             "gr_2016_19"),
         doy = lubridate::yday(date),
         period_ld =  ifelse(doy %in% c(1:81), "Pre Lockdown",  #82 - 22/03/2022
                             ifelse(doy %in% c(82:111),  #82:111 - 20/04/2022
                                    "First-Phase\n Lockdown", "Second-Phase\n Lockdown")),
         ref = format(date,"%m-%d")) %>% 
  group_by(year_group,  id_parameter, id_station, period_ld) %>%
  summarize(value = mean(value, na.rm=T)) %>% ungroup() %>%
  
  reshape2::dcast(id_parameter+id_station+period_ld~year_group, value.var = "value") %>%
  
  #group_by(id_parameter, id_station, period_ld) %>%
  mutate(pct_change = (gr_2020-gr_2016_19)/gr_2016_19) %>%
  
  ungroup() %>%
  filter(id_parameter %in% c("CO" , "NOX", "NO2", "O3", "PM10")) %>%
  group_by(id_parameter, period_ld) %>%
  summarize(pct_change = mean(pct_change, na.rm=T)) -> dif_sp

#################Concatenate the datasets####
dif_bg %>% mutate(city = "Bogota") -> dif_bg
dif_mx %>% mutate(city = "MCMA") -> dif_mx 
dif_sp %>% mutate(city = "MASP") -> dif_sp 

bind_rows(dif_bg, dif_mx, dif_sp) -> dot_plot_df
dot_plot_df %>% 
  mutate(city = factor(city, levels = c("MCMA", "MASP", "Bogota")),
          id_parameter = factor(id_parameter, levels = c("PM10", "O3", "NOX", "NO2", "CO")))

unique(dot_plot_df$id_parameter)

#################Dot plots###################
labels_dot = c( "PM10" = bquote(''*PM[10]*''), "O3" = bquote(''*O[3]*''),  "NOX" = bquote(''*NO[x]*''), 
                "NO2" = bquote(''*NO[2]*''), "CO" = "CO")
dot_plot_df %>% 
  mutate(city = factor(city, levels = c("MCMA", "MASP", "Bogota")),
         id_parameter = factor(id_parameter, levels = c("PM10", "CO", "O3", "NOX", "NO2")),
         period_ld = factor(period_ld, levels = c("First-Phase\n Lockdown", "Second-Phase\n Lockdown", "Pre Lockdown"))) %>%
ggplot(aes(pct_change, id_parameter)) +
  geom_point(aes(colour = period_ld), size = 1.5, shape = 21, stroke = 1.25) +
  scale_colour_wsj("colors6") +
  geom_vline(data = data.frame(city = unique(dot_plot_df$city),
                               hline = c(0, 0, 0)), aes(xintercept = hline), size = 0.45) +
  scale_x_continuous(labels = scales::percent, expand = c(0.02, 0), 
                     limits = c(-0.8, 0.6),
                     breaks = seq(-0.8, 0.6, by = 0.2)) +
  scale_y_discrete(expand = c(.02, 0), labels = labels_dot) +
  facet_grid(rows = vars(city), scales = "free", switch = "y", space = "free_y") +

  theme_minimal() +
  theme(strip.placement = "outside",
        panel.grid.major = element_line(linetype = "dashed", colour = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.direction="horizontal",
        legend.position="bottom",
        text = element_text(family = "Georgia"),
        plot.title = element_text(size = 14, margin = margin(b = 10), hjust = 0.5)) +
  
  labs(title = "Pollutant percenteage change by period",
       x = "Percenteage changes (%)",
       y = "")

ggsave('dot_plot.jpg', dpi=300, width = 5.5, height = 7.5)

#############Correlation##############################
mob_df %>%
  reshape2::dcast( date + City ~ variable, value.var = "per_change") %>%
  select(-c("date")) %>%
  split(list(.$City)) %>%
  map( ~cor_mat(data=., vars = c(2:ncol(.))) )



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
         period_ld =  ifelse(doy %in% c(1:81), "Pre Lockdown",  #82 - 22/03/2022
                             ifelse(doy %in% c(82:111),  #82:111 - 20/04/2022
                                    "First-Phase\n Lockdown", "Second-Phase\n Lockdown"))) %>%
  
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
         period_ld =  ifelse(doy %in% c(1:83), "Pre Lockdown",  #83 - 23/03/2020
                             ifelse(doy %in% c(84:134),  #82:134 -  13/05/2022
                                    "First-Phase\n Lockdown", "Second-Phase\n Lockdown")) ) %>%
  filter( year_group ==  "gr_2020" ) %>%
  group_by(id_parameter) %>%
  dunn_test(data =., value~period_ld, p.adjust.method = "holm" ) -> dunn_mx

## sao paulo
cont_sp %>%  select(-ref) %>%
  mutate(year_group = ifelse(year(date)==2020, 
                             "gr_2020", 
                             "gr_2016_19"),
         doy = lubridate::yday(date),
         period_ld =  ifelse(doy %in% c(1:81), "Pre Lockdown",  #82 - 22/03/2022
                             ifelse(doy %in% c(82:111),  #82:111 - 20/04/2022
                                    "First-Phase\n Lockdown", "Second-Phase\n Lockdown"))) %>%
  filter( year_group ==  "gr_2020" ) %>%
  group_by(id_parameter) %>%
  dunn_test(data =., value~period_ld, p.adjust.method = "holm" ) -> dunn_sp

## Bogota
cont_bog %>%  select(-ref) %>%
  mutate(year_group = ifelse(year(date)==2020, 
                             "gr_2020", 
                             "gr_2016_19"),
         doy = lubridate::yday(date),
         period_ld =  ifelse(doy %in% c(1:79), "Pre Lockdown",  #79 - 20/03/2022
                             ifelse(doy %in% c(80:118),  #80:118 - 27/04/2022
                                    "First-Phase\n Lockdown", "Second-Phase\n Lockdown")) ) %>%  
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
         period_ld =  ifelse(doy %in% c(1:83), "Pre Lockdown",  #83 - 23/03/2020
                             ifelse(doy %in% c(84:134),  #82:134 -  13/05/2022
                                    "First-Phase\n Lockdown", "Second-Phase\n Lockdown")) ) %>%
  
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
         period_ld =  ifelse(doy %in% c(1:81), "Pre Lockdown",  #82 - 22/03/2022
                             ifelse(doy %in% c(82:111),  #82:111 - 20/04/2022
                                    "First-Phase\n Lockdown", "Second-Phase\n Lockdown"))) %>%
  
  group_by(id_parameter, period_ld) %>%
  wilcox_test(data=.,  value~year_group) %>%
  adjust_pvalue(method = "holm") %>%
  add_significance("p.adj") -> wil_sp

wil_mx %>% mutate(City = "MCMA") -> wil_mx
wil_sp %>% mutate(City = "MASP") -> wil_sp
wil_bog  %>% mutate(City = "Bogota") -> wil_bog

bind_rows(wil_mx, wil_sp, wil_bog) -> wil_df
wil_df %>%  write_csv("out/wil_data.csv") 
