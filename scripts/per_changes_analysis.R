

cont_mx %>% select(-ref, -unit) %>%
  mutate(year_group = ifelse(year(date) == 2020, "gr_2020", "gr_2016_19"),
         month_day = format(date,"%m-%d")) %>%
  
  group_by(year_group, month_day, id_station, id_parameter) %>%
  summarize(value_m = mean(value, na.rm = T)) 

##############Percenteage change by Lockdown Period#######################
## valley of mexico
cont_mx %>%  select(-ref, -unit) %>%
  mutate(year_group = ifelse(year(date)==2020, 
                             "gr_2020", 
                             "gr_2016_19"),
         doy = lubridate::yday(date),
         period_ld =  ifelse(doy %in% c(1:81), "No LD",  #82
                             ifelse(doy %in% c(82:162),  #82:162
                                    "LD March and April", "After LD")),
         ref = format(date,"%m-%d")) %>% 
  group_by(year_group,  id_parameter, id_station, period_ld) %>%
  summarize(value = mean(value, na.rm=T)) %>% ungroup() %>%
  
  reshape2::dcast(id_parameter+id_station+period_ld~year_group, value.var = "value") %>%
  
  group_by(id_parameter, id_station, period_ld) %>%
  summarize(pct_change = (gr_2020-gr_2016_19)/gr_2016_19) %>%
  
  ungroup() %>%
  filter(id_parameter %in% c("CO" , "NOX", "NO2", "O3", "PM10")) %>%
  group_by(id_parameter, period_ld) %>%
  summarize(pct_change = mean(pct_change, na.rm=T)) -> dif_mx

#################Dot plots###################
ggplot(dif_mx, aes(pct_change, id_parameter)) +
  geom_point(aes(color = period_ld), size = 1.75) +
  scale_colour_wsj("colors6") +
  scale_x_continuous(labels = scales::percent, expand = c(0.02, 0), 
                     limits = c(-0.6, 0.2),
                     breaks = seq(-0.6, 0.2, by = 0.1)) +
  scale_y_discrete(expand = c(.02, 0)) +
  theme_minimal() +
  theme(#axis.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.justification = c(0, 1), 
        legend.position = c(.1, 1.07),
        legend.background = element_blank(),
        legend.direction="horizontal",
        #text = element_text(family = "Georgia"),
        plot.title = element_text(size = 20, margin = margin(b = 10)),
        plot.subtitle = element_text(size = 10, color = "darkslategrey", margin = margin(b = 15)),
        plot.caption = element_text(size = 8, margin = margin(t = 10), color = "grey70", hjust = 0)) +
  
  labs(title = "Total Revenue by City and Gender",
       subtitle = "Out of 23 cities, eight locations experience a 20% or greater.",
       x = "Percenteage changes",
       y = "")


