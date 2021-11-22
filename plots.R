library(data.table)
library(tidyverse)
main_data <- readRDS("data/main_data")
setDT(main_data)
main_data %>% 
  ggplot(aes(x = rep_date)) +
  geom_line(aes(y = cases_covid, color = "cases_covid")) +
  geom_line(aes(y = cases_covid_invasive, color = "cases_covid_invasive")) +
  geom_line(aes(y = beds_occupied, color = "beds_occupied")) +
  geom_line(aes(y = beds_occupied_just_adults, color = "beds_occupied_just_adults")) +
  geom_line(aes(y = total_cases, color = "total_cases")) +
  geom_line(aes(y = total_death_cases, color = "total_death_cases")) #+
  # geom_line(aes(y = cases_covid_invasive)) +
  # geom_line(aes(y = cases_covid_invasive)) +
  # geom_line(aes(y = cases_covid_invasive)) +
  
