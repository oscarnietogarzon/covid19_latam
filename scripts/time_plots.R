###############################Time series plot###################
install.packages("ggthemes")

cont_mx %>% select(-ref, -unit) %>%
  mutate(year_group = ifelse(year(date) == 2020, "gr_2020", "gr_2016_19"),
         month_day = format(date,"%m-%d")) %>%
  
  group_by(year_group, month_day, id_parameter) %>%
  summarize(value_m = mean(value, na.rm = T)) %>% ungroup() %>%
  filter(id_parameter == "O3") %>%
  mutate(date = lubridate::mdy(paste0(month_day, "-2020") )) %>%
  
  ggplot(aes(x=date, y=value_m, color = year_group)) +
  geom_line(size=0.05, linetype ='solid', alpha = 0.85) +
  scale_color_manual(values=c("#003f5c", "#ffa600"), labels = c('Mean 2016 - 2019', 'Mean 2020')) +
  geom_smooth(method = "loess", size = 0.5, span = 0.2, se = F)  +
  scale_x_date(breaks = "2 month", labels=date_format("%B") ) +
  theme_light() +  labs(title=bquote('Metropolitan Area of Mexico City'), y = bquote(''*NO[X]*' (ppb)'), 
                        x = c('Date')) + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(0.85, 0.85),
        legend.title = element_blank(),
        legend.background=element_blank()) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 120)) +
  
  geom_vline(xintercept = as.Date(c('16-03-2020'), format = "%d-%m-%Y"),
             linetype="dashed", color = "red", size=0.5) +
  geom_vline(xintercept = as.Date(c('01-06-2020'), format = "%d-%m-%Y"),
             linetype="dashed", color = "red", size=0.5) -> plot_o3_zmvm

ggsave('plot_o3_zmvm.tiff', dpi=300, width = 8, height = 6)


''' 
    geom_label(label="Previous Lockdown", x= ymd("2020-02-1"), y=60,
     label.size=0.2, color="black") +
    geom_label(label="First-Phase Lockdown", x= ymd("2020-04-15"), y=60,
      label.size=0.2, color="black") +
    geom_label(label="Second-Phase Lockdown", x= ymd("2020-10-01"), y=60,
             label.size=0.2, color="black")
''' 

##########################Mobility data plot#############################
remotes::install_github("joachim-gassen/tidycovid19")
library(tidycovid19)

download_apple_mtr_data(type = "country_city", silent = TRUE, cached = TRUE) %>%
  dplyr::filter(iso3c == "MEX") %>% distinct(city, sub_region)
ggplot2::ggplot(ggplot2::aes(x = date, y = driving, color = city)) +
  ggplot2::geom_line()

