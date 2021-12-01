library(data.table)
library(tidyverse)
main_data <- readRDS("data/main_data")
source("help_functions/seven_day_inzidenz.R")
setDT(main_data)
main_data %>% 
  ggplot(aes(x = rep_date_divi)) +
  geom_line(aes(y = seven_day_inz(cases_covid_divi), color = "Covid Patienten Intensivbehandlung")) +
  geom_line(aes(y = seven_day_inz(cases_covid_invasive_divi), color = "Covid Patienten Invasivbeatmung")) +
  geom_line(aes(y = seven_day_inz(beds_occupied_divi), color = "Bestzte Betten")) +
  geom_line(aes(y = seven_day_inz(beds_occupied_just_adults_divi), color = "Von Erwachsenen besetzte Betten")) +
  geom_line(aes(y = seven_day_inz(total_cases), color = "Coronafälle")) +
  geom_line(aes(y = seven_day_inz(total_death_cases), color = "Corona-Todesfälle")) +
  labs(title = "7-tageinszidenzen im Coronazusammenhang",
       x = "Zeit", y = "genormte Zahl")#+
  # geom_line(aes(y = cases_covid_invasive)) +
  # geom_line(aes(y = cases_covid_invasive)) +
  # geom_line(aes(y = cases_covid_invasive)) +
  