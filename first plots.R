library(data.table)
library(tidyverse)
library(checkmate)
main_data <- readRDS("data/main_data")
source("help_functions/seven_day_inzidenz.R")
setDT(main_data)
cnames <- colnames(main_data)
inhabitants <- 83240000
#https://service.destatis.de/bevoelkerungspyramide/#!y=2020&a=60,80&g als Quelle für Altersgruppen

#Inzidenzen
#Altersgruppe 0-4
main_data[, cases_0bis4 := sum(.SD),
          .SDcols = cnames[grepl("A00-A04", cnames) & grepl("number_cases", cnames)], by = rep_date_divi]
#Altersgruppe 5-14
main_data[, cases_5bis14 := sum(.SD),
          .SDcols = cnames[grepl("A05-A14", cnames) & grepl("number_cases", cnames)], by = rep_date_divi]
#Altersgruppe 15-34
main_data[, cases_15bis34 := sum(.SD),
          .SDcols = cnames[grepl("A15-A34", cnames) & grepl("number_cases", cnames)], by = rep_date_divi]
#Altersgruppe 35-59
main_data[, cases_35bis59 := sum(.SD),
          .SDcols = cnames[grepl("A35-A59", cnames) & grepl("number_cases", cnames)], by = rep_date_divi]
#Altersgruppe 60-79
main_data[, cases_60bis79 := sum(.SD),
          .SDcols = cnames[grepl("A60-A79", cnames) & grepl("number_cases", cnames)], by = rep_date_divi]
#Altersgruppe 80+
main_data[, cases_über80 := sum(.SD),
          .SDcols = cnames[grepl("A80+", cnames) & grepl("number_cases", cnames)], by = rep_date_divi]

#Todesfälle
#Altersgruppe 0-4
main_data[, deaths_0bis4 := sum(.SD),
          .SDcols = cnames[grepl("A00-A04", cnames) & grepl("number_case_death", cnames)], by = rep_date_divi]
#Altersgruppe 5-14
main_data[, deaths_5bis14 := sum(.SD),
          .SDcols = cnames[grepl("A05-A14", cnames) & grepl("number_case_death", cnames)], by = rep_date_divi]
#Altersgruppe 15-34
main_data[, deaths_15bis34 := sum(.SD),
          .SDcols = cnames[grepl("A15-A34", cnames) & grepl("number_case_death", cnames)], by = rep_date_divi]
#Altersgruppe 35-59
main_data[, deaths_35bis59 := sum(.SD),
          .SDcols = cnames[grepl("A35-A59", cnames) & grepl("number_case_death", cnames)], by = rep_date_divi]
#Altersgruppe 60-79
main_data[, deaths_60bis79 := sum(.SD),
          .SDcols = cnames[grepl("A60-A79", cnames) & grepl("number_case_death", cnames)], by = rep_date_divi]
#Altersgruppe 80+
main_data[, deaths_über80 := sum(.SD),
          .SDcols = cnames[grepl("A80+", cnames) & grepl("number_case_death", cnames)], by = rep_date_divi]

# 7-Tage-Inzidenz (einfach)
main_data %>% 
  ggplot(aes(x = rep_date_divi)) +
  geom_line(aes(y = seven_day_inz(total_cases, inhabitants))) +
  labs(title = "7-Tage-Inzidenz", x = "Zeit", y = "7-Tage-Inzidenz")
# 7-Tage-Inzidenz (nach Altersgruppen) 
main_data %>% 
  ggplot(aes(x = rep_date_divi)) +
  geom_line(aes(y = seven_day_inz(cases_0bis4, inhabitants = 4000000)), color = "red") +
  geom_line(aes(y = seven_day_inz(cases_5bis14, inhabitants = 7500000)), color = "blue") +
  geom_line(aes(y = seven_day_inz(cases_15bis34, inhabitants = 19000000)), color = "green") +
  geom_line(aes(y = seven_day_inz(cases_35bis59, inhabitants = 28800000)), color = "violet") +
  geom_line(aes(y = seven_day_inz(cases_60bis79, inhabitants = 18200000)), color = "orange") +
  geom_line(aes(y = seven_day_inz(cases_über80, inhabitants = 5900000)), color = "yellow") +
  labs(title = "7-Tage-Inzidenz", x = "Zeit", y = "7-Tage-Inzidenz")

# Intensivbettenbelegung
main_data %>% 
  ggplot(aes(x = rep_date_divi)) +
  geom_line(aes(y = cases_covid_divi)) +
  labs(title = "Intensivbettenbelegung", x = "Zeit", y = "belegte Betten")

# Intensivbettenbelegung als 7-Tage-Inzidenz (macht als Wert wenig Sinn in meinen Augen)
main_data %>% 
  ggplot(aes(x = rep_date_divi)) +
  geom_line(aes(y = seven_day_inz(cases_covid_divi, inhabitants))) +
  labs(title = "Intensivbettenbelegung", x = "Zeit", y = "belegte Betten als 7-Tage-Inzidenz")

# Todesfälle (einfach)
main_data %>% 
  ggplot(aes(x = rep_date_divi)) +
  geom_line(aes(y = total_death_cases)) +
  labs(title = "Todesfälle", x = "Zeit", y = "Todesfälle")

# Todesfälle (nach Altersgruppen)
main_data %>% 
  ggplot(aes(x = rep_date_divi)) +
  geom_line(aes(y = deaths_0bis4), color = "red") +
  geom_line(aes(y = deaths_5bis14), color = "blue") +
  geom_line(aes(y = deaths_15bis34), color = "green") +
  geom_line(aes(y = deaths_35bis59), color = "violet") +
  geom_line(aes(y = deaths_60bis79), color = "orange") +
  geom_line(aes(y = deaths_über80), color = "yellow") +
  labs(title = "Todesfälle", x = "Zeit", y = "Todesfälle")

# Todesfälle (einfach) als 7-Tage-Inzidenz (vllt. ohne Multiplizieren mit 100000/inhabitants)
main_data %>% 
  ggplot(aes(x = rep_date_divi)) +
  geom_line(aes(y = seven_day_inz(total_death_cases, inhabitants))) +
  labs(title = "7-Tages-Inzidenz der Todesfälle", x = "Zeit", y = "Todesfälle")
main_data %>% 
  ggplot(aes(x = rep_date_divi)) +
  geom_line(aes(y = seven_day_inz(total_death_cases, inhabitants)*83240000/100000)) +
  labs(title = "7-Tages-Inzidenz der Todesfälle", x = "Zeit", y = "Todesfälle")

# Todesfälle (nach Altersgruppen) als 7-Tage-Inzidenz (vllt. ohne Multiplizieren mit 100000/inhabitants)
main_data %>% 
  ggplot(aes(x = rep_date_divi)) +
  geom_line(aes(y = seven_day_inz(deaths_0bis4, inhabitants = 4000000)), color = "red") +
  geom_line(aes(y = seven_day_inz(deaths_5bis14, inhabitants = 7500000)), color = "blue") +
  geom_line(aes(y = seven_day_inz(deaths_15bis34, inhabitants = 19000000)), color = "green") +
  geom_line(aes(y = seven_day_inz(deaths_35bis59, inhabitants = 28800000)), color = "violet") +
  geom_line(aes(y = seven_day_inz(deaths_60bis79, inhabitants = 18200000)), color = "orange") +
  geom_line(aes(y = seven_day_inz(deaths_über80, inhabitants = 5900000)), color = "yellow") +
  labs(title = "7-Tages-Inzidenz der Todesfälle", x = "Zeit", y = "Todesfälle")
