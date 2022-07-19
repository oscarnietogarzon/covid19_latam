####################Data for the tables#############################
pks <-  c("lubridate","zoo","scales","reshape2","tidyverse", "ggthemes", "openair", "tseries",
          "rstatix", "boot")
lapply(pks, require, character.only = TRUE)

## Bogota
params_int <- c("PM10", "CO", "NO2", "O3")
cont_bog %>%  select(-ref) %>%
  filter(id_parameter %in% params_int) %>%
  mutate(year_group = ifelse(year(date)==2020, 
                             "gr_2020", 
                             "gr_2016_19"),
         doy = lubridate::yday(date),
         period_ld =  ifelse(doy %in% c(1:79), "No LD",  #79 - 20/03/2022
                             ifelse(doy %in% c(80:118),  #80:118 - 27/04/2022
                                    "LD March and April", "After LD")),
         ref = format(date,"%m-%d")) %>% 
  mutate(period_ld = factor(period_ld, levels = c("No LD", "LD March and April", "After LD")) ) %>%
  select(-date) %>%
  group_by(year_group,  id_parameter, period_ld) %>%
  
  summarise(mean.value = mean(value, na.rm = TRUE),
            median.value = median(value, na.rm = TRUE),
            sd.value = sd(value, na.rm = TRUE),
            n.value = n()) %>%
  
  mutate(se.value = sd.value / sqrt(n.value),
         lower.ci = mean.value - qt(1 - (0.05 / 2), n.value - 1) * se.value,
         upper.ci = mean.value + qt(1 - (0.05 / 2), n.value - 1) * se.value) %>%
  
  write_csv("out/bog_data.csv")
  
## Valley of mexico
params_int <- c("PM10", "CO", "NO2", "O3")
cont_mx %>%  select(-ref) %>%
  filter(id_parameter %in% params_int) %>%
  mutate(year_group = ifelse(year(date)==2020, 
                             "gr_2020", 
                             "gr_2016_19"),
         doy = lubridate::yday(date),
         period_ld =  ifelse(doy %in% c(1:83), "No LD",  #83 - 23/03/2020
                             ifelse(doy %in% c(84:134),  #82:134 -  13/05/2022
                                    "LD March and April", "After LD")),
         ref = format(date,"%m-%d")) %>% 
  mutate(period_ld = factor(period_ld, levels = c("No LD", "LD March and April", "After LD")) ) %>%
  select(-date) %>%
  group_by(year_group,  id_parameter, period_ld) %>%
  
  summarise(mean.value = mean(value, na.rm = TRUE),
            median.value = median(value, na.rm = TRUE),
            sd.value = sd(value, na.rm = TRUE),
            n.value = n()) %>%
  
  mutate(se.value = sd.value / sqrt(n.value),
         lower.ci = mean.value - qt(1 - (0.05 / 2), n.value - 1) * se.value,
         upper.ci = mean.value + qt(1 - (0.05 / 2), n.value - 1) * se.value) %>%
  
  write_csv("out/mcma_data.csv")

## Sao Paulo
params_int <- c("PM10", "CO", "NO2", "O3")
cont_sp %>%  select(-ref) %>%
  filter(id_parameter %in% params_int) %>%
  mutate(year_group = ifelse(year(date)==2020, 
                             "gr_2020", 
                             "gr_2016_19"),
         doy = lubridate::yday(date),
         period_ld =  ifelse(doy %in% c(1:81), "No LD",  #82 - 22/03/2022
                             ifelse(doy %in% c(82:111),  #82:111 - 20/04/2022
                                    "LD March and April", "After LD")),
         ref = format(date,"%m-%d")) %>% 
  mutate(period_ld = factor(period_ld, levels = c("No LD", "LD March and April", "After LD")) ) %>%
  select(-date) %>%
  group_by(year_group,  id_parameter, period_ld) %>%
  
  summarise(mean.value = mean(value, na.rm = TRUE),
            median.value = median(value, na.rm = TRUE),
            sd.value = sd(value, na.rm = TRUE),
            n.value = n()) %>%
  
  mutate(se.value = sd.value / sqrt(n.value),
         lower.ci = mean.value - qt(1 - (0.05 / 2), n.value - 1) * se.value,
         upper.ci = mean.value + qt(1 - (0.05 / 2), n.value - 1) * se.value) %>%
  
  ungroup() %>%
  arrange(year_group, id_parameter, period_ld) %>%
  
  write_csv("out/sp_data.csv")


