library(data.table)
library(tidyverse)
library(checkmate)
library(ggcorrplot)
main_data <- readRDS("data/main_data")
source("help_functions/seven_day_inzidenz.R")
setDT(main_data)
plot_overview <- main_data %>% 
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
  
plot_rates <- main_data %>% 
  ggplot(aes(x = rep_date_divi)) +
  geom_line(aes(y = seven_day_inz(total_death_cases) /seven_day_inz(total_cases), color = "deaths/cases")) +
  geom_line(aes(y = seven_day_inz(cases_covid_invasive_divi) /seven_day_inz(beds_occupied_divi), color = "cases_hosp_inva/beds_occ")) +
  geom_line(aes(y = seven_day_inz(cases_covid_divi) /seven_day_inz(total_cases), color = "hosp_cases/cases")) +
  geom_line(aes(y = seven_day_inz(total_death_cases) /seven_day_inz(beds_occupied_divi), color = "deaths/beds_occ")) +
  geom_line(aes(y = seven_day_inz(cases_covid_divi) /seven_day_inz(beds_occupied_divi), color = "hosp_cases/beds_occ"))

cor_plot <- ggcorrplot(cor(main_data["2020-04-25" < rep_date_divi][, c(2:8, 94:95)]))
p_values_cors <- cor_pmat(main_data["2020-04-25" < rep_date_divi][, c(2:8, 94:95)])
