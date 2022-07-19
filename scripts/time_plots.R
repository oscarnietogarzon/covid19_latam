###############################Time series plot###################
install.packages("ggthemes")
############ O3 ##########################
## Valley of Mexico
cont_mx %>% select(-ref) %>%
  mutate(year_group = ifelse(year(date) == 2020, "gr_2020", "gr_2016_19"),
         month_day = format(date,"%m-%d")) %>%
  
  group_by(year_group, month_day, id_parameter) %>%
  summarize(value_m = mean(value, na.rm = T)) %>% ungroup() %>%
  filter(id_parameter == "O3") %>%
  mutate(date = lubridate::mdy(paste0(month_day, "-2020") )) %>%
  
  ggplot(aes(x=date, y=value_m, color = year_group)) +
  geom_line(size=0.05, linetype ='solid', alpha = 0.85) +
  scale_color_manual(values=c("azure4", "coral3"), labels = c('Mean 2016 - 2019', 'Mean 2020')) +
  geom_smooth(aes(fill = year_group), method = "loess", size = 0.65, se = T, alpha = 0.25)  +
  scale_fill_manual(values=c("dimgrey", "coral2"), labels = c('Mean 2016 - 2019', 'Mean 2020')) +
  
  scale_x_date(breaks = "2 month", labels=date_format("%B"), expand = c(0, 0) ) +
  theme_few() +  labs( y = bquote(''*O[3]*' (ppb)'), x = c('Date')) + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(0.88, 0.92),
        legend.title = element_blank(),
        legend.background=element_blank()) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 60)) +
  
  geom_vline(xintercept = as.Date(c('23-03-2020'), format = "%d-%m-%Y"), color = "black", size=0.5) +
  geom_vline(xintercept = as.Date(c('13-05-2020'), format = "%d-%m-%Y"), color = "black", size=0.5) + 

    geom_text(label="Pre Lockdown", x= ymd("2020-02-1"), y=5, color="black", size = 2.5) +
    geom_text(label="First-Phase\n Lockdown", x= ymd("2020-04-20"), y=5, color="black", size = 2.5) +
   geom_text(label="Second-Phase\n Lockdown", x= ymd("2020-09-01"), y=5, color="black", size = 2.5)

ggsave('plot_mx_o3.png', dpi=300, width = 6.5, height = 4)

## São Paulo
cont_sp %>% select(-ref) %>%
  mutate(year_group = ifelse(year(date) == 2020, "gr_2020", "gr_2016_19"),
         month_day = format(date,"%m-%d")) %>%
  
  group_by(year_group, month_day, id_parameter) %>%
  summarize(value_m = mean(value, na.rm = T)) %>% ungroup() %>%
  filter(id_parameter == "O3") %>%
  mutate(date = lubridate::mdy(paste0(month_day, "-2020") )) %>%
  
  ggplot(aes(x=date, y=value_m, color = year_group)) +
  geom_line(size=0.05, linetype ='solid', alpha = 0.85) +
  scale_color_manual(values=c("grey30", "coral3"), labels = c('Mean 2016 - 2019', 'Mean 2020')) +
  geom_smooth(aes(fill = year_group), method = "loess", size = 0.65, se = T, alpha = 0.25)  +
  scale_fill_manual(values=c("dimgrey", "coral2"), labels = c('Mean 2016 - 2019', 'Mean 2020')) +
  
  scale_x_date(breaks = "2 month", labels=date_format("%B"), expand = c(0, 0) ) +
  theme_few() +  labs( y = bquote(''*O[3]*' ('*mu*'m/'*g^3*')'), x = c('Date')) + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(0.88, 0.92),
        legend.title = element_blank(),
        legend.background=element_blank()) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100), breaks = seq(0, 100, 20)) +
  
  geom_vline(xintercept = as.Date(c('22-03-2020'), format = "%d-%m-%Y"), color = "darkred", size=0.5) +
  geom_vline(xintercept = as.Date(c('01-05-2020'), format = "%d-%m-%Y"), color = "darkred", size=0.5) + 
  
  geom_text(label="Pre Lockdown", x= ymd("2020-02-1"), y=5, color="black", size = 2.5) +
  geom_text(label="First-Phase\n Lockdown", x= ymd("2020-04-8"), y=7.5, color="black", size = 2.5) +
  geom_text(label="Second-Phase\n Lockdown", x= ymd("2020-09-01"), y=7.5, color="black", size = 2.5)

ggsave('plot_sp_o3.png', dpi=300, width = 6.5, height = 4)

## Bogota
cont_bog %>% select(-ref) %>%
  mutate(year_group = ifelse(year(date) == 2020, "gr_2020", "gr_2016_19"),
         month_day = format(date,"%m-%d")) %>%
  
  group_by(year_group, month_day, id_parameter) %>%
  summarize(value_m = mean(value, na.rm = T)) %>% ungroup() %>%
  filter(id_parameter == "O3") %>%
  mutate(date = lubridate::mdy(paste0(month_day, "-2020") )) %>%
  
  ggplot(aes(x=date, y=value_m, color = year_group)) +
  geom_line(size=0.05, linetype ='solid', alpha = 0.85) +
  scale_color_manual(values=c("grey30", "coral3"), labels = c('Mean 2016 - 2019', 'Mean 2020')) +
  geom_smooth(aes(fill = year_group), method = "loess", size = 0.65, se = T, alpha = 0.25)  +
  scale_fill_manual(values=c("dimgrey", "coral2"), labels = c('Mean 2016 - 2019', 'Mean 2020')) +
  
  scale_x_date(breaks = "2 month", labels=date_format("%B"), expand = c(0, 0) ) +
  theme_few() +  labs( y = bquote(''*O[3]*' (ppb)'), x = c('Date')) + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(0.88, 0.92),
        legend.title = element_blank(),
        legend.background=element_blank()) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 40), breaks = seq(0, 40, 10)) +
  
  geom_vline(xintercept = as.Date(c('20-03-2020'), format = "%d-%m-%Y"), color = "darkred", size=0.5) +
  geom_vline(xintercept = as.Date(c('27-04-2020'), format = "%d-%m-%Y"), color = "darkred", size=0.5) + 
  
  geom_text(label="Pre Lockdown", x= ymd("2020-02-1"), y=2, color="black", size=2.5) +
  geom_text(label="First-Phase\n Lockdown", x= ymd("2020-04-08"), y=2.5, color="black", size=2.5) +
  geom_text(label="Second-Phase\n Lockdown", x= ymd("2020-09-01"), y=2.5, color="black", size=2.5)

ggsave('plot_bog_o3.png', dpi=300, width = 6.5, height = 4)

############ NO2 ###################
## Valley of Mexico
cont_mx %>% select(-ref) %>%
  mutate(year_group = ifelse(year(date) == 2020, "gr_2020", "gr_2016_19"),
         month_day = format(date,"%m-%d")) %>%
  
  group_by(year_group, month_day, id_parameter) %>%
  summarize(value_m = mean(value, na.rm = T)) %>% ungroup() %>%
  filter(id_parameter == "NO2") %>%
  mutate(date = lubridate::mdy(paste0(month_day, "-2020") )) %>%
  
  ggplot(aes(x=date, y=value_m, color = year_group)) +
  geom_line(size=0.05, linetype ='solid', alpha = 0.85) +
  scale_color_manual(values=c("grey30", "darkorange2"), labels = c('Mean 2016 - 2019', 'Mean 2020')) +
  geom_smooth(aes(fill = year_group), method = "loess", size = 0.65, se = T, alpha = 0.25)  +
  scale_fill_manual(values=c("dimgrey", "darkorange2"), labels = c('Mean 2016 - 2019', 'Mean 2020')) +
  
  scale_x_date(breaks = "2 month", labels=date_format("%B"), expand = c(0, 0) ) +
  theme_few() +  labs( y = bquote(''*NO[2]*' (ppb)'), x = c('Date')) + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(0.88, 0.92),
        legend.title = element_blank(),
        legend.background=element_blank()) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 50), breaks = seq(0, 40, 10))  +
  
  geom_vline(xintercept = as.Date(c('23-03-2020'), format = "%d-%m-%Y"), color = "darkred", size=0.5) +
  geom_vline(xintercept = as.Date(c('13-05-2020'), format = "%d-%m-%Y"), color = "darkred", size=0.5) + 
  
  geom_text(label="Pre Lockdown", x= ymd("2020-02-1"), y=2, color="black", size = 2.5) +
  geom_text(label="First-Phase\n Lockdown", x= ymd("2020-04-20"), y=2.5, color="black", size = 2.5) +
  geom_text(label="Second-Phase\n Lockdown", x= ymd("2020-09-01"), y=2.5, color="black", size = 2.5)

ggsave('plot_mx_no2.png', dpi=300, width = 6.5, height = 4)

## São Paulo
cont_sp %>% select(-ref) %>%
  mutate(year_group = ifelse(year(date) == 2020, "gr_2020", "gr_2016_19"),
         month_day = format(date,"%m-%d")) %>%
  
  group_by(year_group, month_day, id_parameter) %>%
  summarize(value_m = mean(value, na.rm = T)) %>% ungroup() %>%
  filter(id_parameter == "NO2") %>%
  mutate(date = lubridate::mdy(paste0(month_day, "-2020") )) %>%
  
  ggplot(aes(x=date, y=value_m, color = year_group)) +
  geom_line(size=0.05, linetype ='solid', alpha = 0.85) +
  scale_color_manual(values=c("grey30", "darkorange3"), labels = c('Mean 2016 - 2019', 'Mean 2020')) +
  geom_smooth(aes(fill = year_group), method = "loess", size = 0.65, se = T, alpha = 0.25)  +
  scale_fill_manual(values=c("dimgrey", "darkorange2"), labels = c('Mean 2016 - 2019', 'Mean 2020')) +
  
  scale_x_date(breaks = "2 month", labels=date_format("%B"), expand = c(0, 0) ) +
  theme_few() +  labs( y = bquote(''*NO[2]*' ('*mu*'m/'*g^3*')'), x = c('Date')) + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(0.88, 0.92),
        legend.title = element_blank(),
        legend.background=element_blank()) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 85), breaks = seq(0, 80, 20)) +
  
  geom_vline(xintercept = as.Date(c('22-03-2020'), format = "%d-%m-%Y"), color = "darkred", size=0.5) +
  geom_vline(xintercept = as.Date(c('01-05-2020'), format = "%d-%m-%Y"), color = "darkred", size=0.5) +
  
  geom_text(label="Pre Lockdown", x= ymd("2020-02-1"), y=2, color="black", size=2.5) +
  geom_text(label="First-Phase\n Lockdown", x= ymd("2020-04-10"), y=3, color="black", size=2.5) +
  geom_text(label="Second-Phase\n Lockdown", x= ymd("2020-09-01"), y=3, color="black", size=2.5)

ggsave('plot_sp_no2.png', dpi=300, width = 6.5, height = 4)

## Bogota
cont_bog %>% select(-ref) %>%
  mutate(year_group = ifelse(year(date) == 2020, "gr_2020", "gr_2016_19"),
         month_day = format(date,"%m-%d")) %>%
  
  group_by(year_group, month_day, id_parameter) %>%
  summarize(value_m = mean(value, na.rm = T)) %>% ungroup() %>%
  filter(id_parameter == "NO2") %>%
  mutate(date = lubridate::mdy(paste0(month_day, "-2020") )) %>%
  
  ggplot(aes(x=date, y=value_m, color = year_group)) +
  geom_line(size=0.05, linetype ='solid', alpha = 0.85) +
  scale_color_manual(values=c("grey30", "darkorange3"), labels = c('Mean 2016 - 2019', 'Mean 2020')) +
  geom_smooth(aes(fill = year_group), method = "loess", size = 0.65, se = T, alpha = 0.25)  +
  scale_fill_manual(values=c("dimgrey", "darkorange2"), labels = c('Mean 2016 - 2019', 'Mean 2020')) +
  
  scale_x_date(breaks = "2 month", labels=date_format("%B"), expand = c(0, 0) ) +
  theme_few() +  labs( y = bquote(''*NO[2]*' (ppb)'), x = c('Date')) + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(0.88, 0.92),
        legend.title = element_blank(),
        legend.background=element_blank()) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 35), breaks = seq(0, 40, 10)) +
  
  geom_vline(xintercept = as.Date(c('20-03-2020'), format = "%d-%m-%Y"), color = "darkred", size=0.5) +
  geom_vline(xintercept = as.Date(c('27-04-2020'), format = "%d-%m-%Y"), color = "darkred", size=0.5) + 
  
  geom_text(label="Pre Lockdown", x= ymd("2020-02-1"), y=1, color="black", size=2.5) +
  geom_text(label="First-Phase\n Lockdown", x= ymd("2020-04-08"), y=1.5, color="black", size=2.5) +
  geom_text(label="Second-Phase\n Lockdown", x= ymd("2020-09-01"), y=1.5, color="black", size=2.5)

ggsave('plot_bog_no2.png', dpi=300, width = 6.5, height = 4)

############ CO ################################
## Valley of Mexico
cont_mx %>% select(-ref) %>%
  mutate(year_group = ifelse(year(date) == 2020, "gr_2020", "gr_2016_19"),
         month_day = format(date,"%m-%d")) %>%
  
  group_by(year_group, month_day, id_parameter) %>%
  summarize(value_m = mean(value, na.rm = T)) %>% ungroup() %>%
  filter(id_parameter == "CO") %>%
  mutate(date = lubridate::mdy(paste0(month_day, "-2020") )) %>%
  
  ggplot(aes(x=date, y=value_m, color = year_group)) +
  geom_line(size=0.05, linetype ='solid', alpha = 0.85) +
  scale_color_manual(values=c("grey30", "blue2"), labels = c('Mean 2016 - 2019', 'Mean 2020')) +
  geom_smooth(aes(fill = year_group), method = "loess", size = 0.65, se = T, alpha = 0.25)  +
  scale_fill_manual(values=c("dimgrey", "blue"), labels = c('Mean 2016 - 2019', 'Mean 2020')) +
  scale_x_date(breaks = "2 month", labels=date_format("%B"), expand = c(0, 0) ) +
  theme_few() +  labs( y = bquote('CO (ppm)'), x = c('Date')) + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(0.88, 0.92),
        legend.title = element_blank(),
        legend.background=element_blank()) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks = seq(0, 1, 0.4))  +
  
  geom_vline(xintercept = as.Date(c('23-03-2020'), format = "%d-%m-%Y"), color = "black", size=0.5) +
  geom_vline(xintercept = as.Date(c('13-05-2020'), format = "%d-%m-%Y"), color = "black", size=0.5) + 
  
  geom_text(label="Pre Lockdown", x= ymd("2020-02-1"), y=0.05, color="black", size = 2.5) +
  geom_text(label="First-Phase\n Lockdown", x= ymd("2020-04-20"), y=0.055, color="black", size = 2.5) +
  geom_text(label="Second-Phase\n Lockdown", x= ymd("2020-09-01"), y=0.055, color="black", size = 2.5)

ggsave('plot_mx_co.png', dpi=300, width = 6.5, height = 4)

## São Paulo
cont_sp %>% select(-ref) %>%
  mutate(year_group = ifelse(year(date) == 2020, "gr_2020", "gr_2016_19"),
         month_day = format(date,"%m-%d")) %>%
  
  group_by(year_group, month_day, id_parameter) %>%
  summarize(value_m = mean(value, na.rm = T)) %>% ungroup() %>%
  filter(id_parameter == "CO") %>%
  mutate(date = lubridate::mdy(paste0(month_day, "-2020") )) %>%
  
  ggplot(aes(x=date, y=value_m, color = year_group)) +
  geom_line(size=0.05, linetype ='solid', alpha = 0.85) +
  scale_color_manual(values=c("grey30", "blue2"), labels = c('Mean 2016 - 2019', 'Mean 2020')) +
  geom_smooth(aes(fill = year_group), method = "loess", size = 0.65, se = T, alpha = 0.25)  +
  scale_fill_manual(values=c("dimgrey", "blue"), labels = c('Mean 2016 - 2019', 'Mean 2020')) +
  scale_x_date(breaks = "2 month", labels=date_format("%B"), expand = c(0, 0) ) +
  theme_few() +  labs( y = bquote('CO (ppm)'), x = c('Date')) + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(0.88, 0.92),
        legend.title = element_blank(),
        legend.background=element_blank()) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.8), breaks = seq(0, 2.2, 0.4)) +
  
  geom_vline(xintercept = as.Date(c('22-03-2020'), format = "%d-%m-%Y"), color = "black", size=0.5) +
  geom_vline(xintercept = as.Date(c('01-05-2020'), format = "%d-%m-%Y"), color = "black", size=0.5) +
  
  geom_text(label="Pre Lockdown", x= ymd("2020-02-1"), y=0.05, color="black", size=2.5) +
  geom_text(label="First-Phase\n Lockdown", x= ymd("2020-04-10"), y=0.075, color="black", size=2.5) +
  geom_text(label="Second-Phase\n Lockdown", x= ymd("2020-09-01"), y=0.075, color="black", size=2.5) 

ggsave('plot_sp_co.png', dpi=300, width = 6.5, height = 4)

## Bogota
cont_bog %>% select(-ref) %>%
  mutate(year_group = ifelse(year(date) == 2020, "gr_2020", "gr_2016_19"),
         month_day = format(date,"%m-%d")) %>%
  
  group_by(year_group, month_day, id_parameter) %>%
  summarize(value_m = mean(value, na.rm = T)) %>% ungroup() %>%
  filter(id_parameter == "CO") %>%
  mutate(date = lubridate::mdy(paste0(month_day, "-2020") )) %>%
  
  ggplot(aes(x=date, y=value_m, group = year_group, color = year_group)) +
  geom_line(size=0.05, linetype ='solid', alpha = 0.85) +
  scale_color_manual(values=c("grey30", "blue2"), labels = c('Mean 2016 - 2019', 'Mean 2020')) +
  geom_smooth(aes(fill = year_group), method = "loess", size = 0.65, se = T, alpha = 0.25)  +
  scale_fill_manual(values=c("dimgrey", "blue"), labels = c('Mean 2016 - 2019', 'Mean 2020')) +
  scale_x_date(breaks = "2 month", labels=date_format("%B"), expand = c(0, 0) ) +
  theme_few() +  labs( y = bquote('CO (ppm)'), x = c('Date')) + 
  theme(legend.position = c(0.88, 0.92),
        legend.title = element_blank(),
        legend.background=element_blank()) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.4), breaks = seq(0, 1.6, 0.4) ) +
  
  geom_vline(xintercept = as.Date(c('20-03-2020'), format = "%d-%m-%Y"), color = "darkred", size=0.5) +
  geom_vline(xintercept = as.Date(c('27-04-2020'), format = "%d-%m-%Y"), color = "darkred", size=0.5) + 
  
  geom_text(label="Pre Lockdown", x= ymd("2020-02-1"), y=0.05, color="black", size=2.5) +
  geom_text(label="First-Phase\n Lockdown", x= ymd("2020-04-08"), y=0.065, color="black", size=2.5) +
  geom_text(label="Second-Phase\n Lockdown", x= ymd("2020-09-01"), y=0.065, color="black", size=2.5)

ggsave('plot_bog_co.png', dpi=300, width = 6.5, height = 4)

############ PM10 ########################
## Valley of Mexico
cont_mx %>% select(-ref) %>%
  mutate(year_group = ifelse(year(date) == 2020, "gr_2020", "gr_2016_19"),
         month_day = format(date,"%m-%d")) %>%
  
  group_by(year_group, month_day, id_parameter) %>%
  summarize(value_m = mean(value, na.rm = T)) %>% ungroup() %>%
  filter(id_parameter == "PM10") %>%
  mutate(date = lubridate::mdy(paste0(month_day, "-2020") )) %>%
  
  ggplot(aes(x=date, y=value_m, color = year_group)) +
  geom_line(size=0.05, linetype ='solid', alpha = 0.85) +
  scale_color_manual(values=c("grey30", "darkgreen"), labels = c('Mean 2016 - 2019', 'Mean 2020')) +
  geom_smooth(aes(fill = year_group), method = "loess", size = 0.65, se = T, alpha = 0.25)  +
  scale_fill_manual(values=c("dimgrey", "darkgreen"), labels = c('Mean 2016 - 2019', 'Mean 2020')) +
  
  scale_x_date(breaks = "2 month", labels=date_format("%B"), expand = c(0, 0) ) +
  theme_few() +  labs( y = bquote(''*PM[10]*' ('*mu*'m/'*g^3*')'), x = c('Date')) + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(0.88, 0.92),
        legend.title = element_blank(),
        legend.background=element_blank()) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 80),  breaks = seq(0, 80, 20))  +
  
  geom_vline(xintercept = as.Date(c('23-03-2020'), format = "%d-%m-%Y"), color = "black", size=0.5) +
  geom_vline(xintercept = as.Date(c('13-05-2020'), format = "%d-%m-%Y"), color = "black", size=0.5) + 
  
  geom_text(label="Pre Lockdown", x= ymd("2020-02-1"), y=2.5, color="black", size = 2.5) +
  geom_text(label="First-Phase\n Lockdown", x= ymd("2020-04-20"), y=3, color="black", size = 2.5) +
  geom_text(label="Second-Phase\n Lockdown", x= ymd("2020-09-01"), y=3, color="black", size = 2.5)

ggsave('plot_mx_pm10.png', dpi=300, width = 6.5, height = 4)

## São Paulo
cont_sp %>% select(-ref) %>%
  mutate(year_group = ifelse(year(date) == 2020, "gr_2020", "gr_2016_19"),
         month_day = format(date,"%m-%d")) %>%
  
  group_by(year_group, month_day, id_parameter) %>%
  summarize(value_m = mean(value, na.rm = T)) %>% ungroup() %>%
  filter(id_parameter == "PM10") %>%
  mutate(date = lubridate::mdy(paste0(month_day, "-2020") )) %>%
  
  ggplot(aes(x=date, y=value_m, color = year_group)) +
  geom_line(size=0.05, linetype ='solid', alpha = 0.85) +
  scale_color_manual(values=c("grey30", "darkgreen"), labels = c('Mean 2016 - 2019', 'Mean 2020')) +
  geom_smooth(aes(fill = year_group), method = "loess", size = 0.65, se = T, alpha = 0.25)  +
  scale_fill_manual(values=c("dimgrey", "darkgreen"), labels = c('Mean 2016 - 2019', 'Mean 2020')) +
  
  scale_x_date(breaks = "2 month", labels=date_format("%B"), expand = c(0, 0) ) +
  theme_few() +  labs( y = bquote(''*PM[10]*' ('*mu*'m/'*g^3*')'), x = c('Date')) + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(0.88, 0.92),
        legend.title = element_blank(),
        legend.background=element_blank()) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 80), breaks = seq(0, 80, 20)) +
  
  geom_vline(xintercept = as.Date(c('22-03-2020'), format = "%d-%m-%Y"), color = "black", size=0.5) +
  geom_vline(xintercept = as.Date(c('01-05-2020'), format = "%d-%m-%Y"), color = "black", size=0.5) +
  
  geom_text(label="Pre Lockdown", x= ymd("2020-02-1"), y=2.5, color="black", size=2.5) +
  geom_text(label="First-Phase\n Lockdown", x= ymd("2020-04-10"), y=3, color="black", size=2.5) +
  geom_text(label="Second-Phase\n Lockdown", x= ymd("2020-09-01"), y=3, color="black", size=2.5)

ggsave('plot_sp_pm10.png', dpi=300, width = 6.5, height = 4)

## Bogota
cont_bog %>% select(-ref) %>%
  mutate(year_group = ifelse(year(date) == 2020, "gr_2020", "gr_2016_19"),
         month_day = format(date,"%m-%d")) %>%
  
  group_by(year_group, month_day, id_parameter) %>%
  summarize(value_m = mean(value, na.rm = T)) %>% ungroup() %>%
  filter(id_parameter == "PM10") %>%
  mutate(date = lubridate::mdy(paste0(month_day, "-2020") )) %>%
  
  ggplot(aes(x=date, y=value_m, color = year_group)) +
  geom_line(size=0.05, linetype ='solid', alpha = 0.85) +
  scale_color_manual(values=c("grey30", "darkgreen"), labels = c('Mean 2016 - 2019', 'Mean 2020')) +
  geom_smooth(aes(fill = year_group), method = "loess", size = 0.65, se = T, alpha = 0.25)  +
  scale_fill_manual(values=c("dimgrey", "darkgreen"), labels = c('Mean 2016 - 2019', 'Mean 2020')) +
  
  scale_x_date(breaks = "2 month", labels=date_format("%B"), expand = c(0, 0) ) +
  theme_few() +  labs( y = bquote(''*PM[10]*' ('*mu*'m/'*g^3*')'), x = c('Date')) + 
  theme(legend.position = c(0.88, 0.92),
        legend.title = element_blank(),
        legend.background=element_blank()) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100), breaks = seq(0, 100, 20)) +
  
  geom_vline(xintercept = as.Date(c('20-03-2020'), format = "%d-%m-%Y"), color = "black", size=0.5) +
  geom_vline(xintercept = as.Date(c('27-04-2020'), format = "%d-%m-%Y"), color = "black", size=0.5) + 
  
  geom_text(label="Pre Lockdown", x= ymd("2020-02-1"), y=4, color="black", size=2.5) +
  geom_text(label="First-Phase\n Lockdown", x= ymd("2020-04-08"), y=5.5, color="black", size=2.5) +
  geom_text(label="Second-Phase\n Lockdown", x= ymd("2020-09-01"), y=5.5, color="black", size=2.5)

ggsave('plot_bog_pm10.png', dpi=300, width = 6.5, height = 4)

##########################Mobility data plot#############################
##Waze
waze %>% 
  filter(between(date_n, dmy("01/01/2020"), dmy("01/01/2021")))  %>%
  mutate(variable = "driven_miles_km") %>%
  select(c("date_n", "City", "variable", "per_change")) %>%
  rename("date"="date_n") -> waze_melt

##Google Mobility Index
gmi %>% 
  filter(between(date, dmy("01/01/2020"), dmy("01/01/2021"))) %>%
  select("date", "sub_region_1", "retail_and_recreation_percent_change_from_baseline", "grocery_and_pharmacy_percent_change_from_baseline",
         "parks_percent_change_from_baseline", "transit_stations_percent_change_from_baseline", "workplaces_percent_change_from_baseline", "residential_percent_change_from_baseline") %>%
  reshape2::melt(id.vars = c("date", "sub_region_1")) %>%
  mutate(per_change = value/100) %>%
  rename("City"="sub_region_1") %>% select(-c("value")) -> gmi_melt

bind_rows(waze_melt, gmi_melt) -> mob_df

####Overall plot
unique(mob_df$variable)
mob_df %>% 
  filter(variable %in% c("driven_miles_km", "workplaces_percent_change_from_baseline", 
                         "transit_stations_percent_change_from_baseline")) %>%
  ggplot(aes(x=date, y=per_change, colour=variable)) + 
  geom_line(size=0.35) + scale_colour_wsj("colors6") +
  facet_grid(rows = vars(City), scales = "free", switch = "y", space = "free_y") +
  scale_y_continuous(labels = scales::percent, expand = c(0.02, 0), 
                     limits = c(-1, 0.5),
                     breaks = seq(-1, 0.5, by = 0.2)) +
  theme_light() +
  geom_hline(data = data.frame(City = unique(mob_df$City),
                               hline = c(0, 0, 0)), aes(yintercept = hline), size = 0.45, linetype = "dashed", color="grey") +
  
  geom_vline(data = data.frame(City = unique(mob_df$City),
                               vline = lubridate::dmy( c("18/03/2020", "15/03/2020", "20/03/2020") ) ),
             aes(xintercept = vline), size = 0.45, linetype = "dashed", color="red") +
  
  geom_vline(data = data.frame(City = unique(waze$City),
                               vline = lubridate::dmy( c("13/05/2020", "20/04/2020", "27/04/2020") ) ),
             aes(xintercept = vline), size = 0.45, linetype = "dashed", color="red") +
  

  #theme(strip.background = element_rect(colour="black", fill="white", 
  #                                      size=0.75, linetype="solid"))
  
  theme(strip.placement = "outside",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.title = element_blank(),
    legend.position="bottom",
    legend.background = element_blank(),
    legend.direction="horizontal",
    text = element_text(family = "Georgia"),
    plot.title = element_text(size = 16, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 10, color = "darkslategrey", margin = margin(b = 15)),
    plot.caption = element_text(size = 8, margin = margin(t = 10), color = "grey70", hjust = 0)) +
  
  labs(title = "Total Revenue by City and Gender",
       subtitle = "Out of 23 cities, eight locations experience a 20% or greater.",
       y = "Percenteage changes",
       x = "Date")

