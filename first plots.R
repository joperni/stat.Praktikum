library(data.table)
library(tidyverse)
library(checkmate)
main_data <- readRDS("data/main_data")
source("help_functions/seven_day_inzidenz.R")
source("examples_of_code/example_aggregating.R")
setDT(main_data)
cnames <- colnames(main_data)
inhabitants <- 83240000


# 7-Tage-Inzidenz (einfach)
main_data %>% 
  ggplot(aes(x = rep_date_divi)) +
  geom_line(aes(y = seven_day_inz)) +
  labs(title = "7-Tage-Inzidenz", x = "Zeit", y = "7-Tage-Inzidenz")
# 7-Tage-Inzidenz (nach Altersgruppen) 
main_data %>% 
  ggplot(aes(x = rep_date_divi)) +
  geom_line(aes(y = seven_day_inz_A00_A14), color = "blue") +
  geom_line(aes(y = seven_day_inz_A15_A34), color = "green") +
  geom_line(aes(y = seven_day_inz_A35_A59), color = "violet") +
  geom_line(aes(y = seven_day_inz_A60_A79), color = "orange") +
  geom_line(aes(y = seven_day_inz_A80), color = "yellow") +
  labs(title = "7-Tage-Inzidenz", x = "Zeit", y = "7-Tage-Inzidenz")

# Intensivbettenbelegung
main_data %>% 
  ggplot(aes(x = rep_date_divi)) +
  geom_line(aes(y = cases_covid_divi)) +
  labs(title = "Intensivbettenbelegung", x = "Zeit", y = "belegte Betten")

# Intensivbettenbelegung als 7-Tage-Inzidenz (macht als Wert wenig Sinn in meinen Augen)
main_data %>% 
  ggplot(aes(x = rep_date_divi)) +
  geom_line(aes(y = seven_day_inz(cases_covid_divi))) +
  labs(title = "Intensivbettenbelegung", x = "Zeit", y = "belegte Betten als 7-Tage-Inzidenz")

# Todesfälle (einfach)
main_data %>% 
  ggplot(aes(x = rep_date_divi)) +
  geom_line(aes(y = total_death_cases)) +
  labs(title = "Todesfälle", x = "Zeit", y = "Todesfälle")

# Todesfälle (nach Altersgruppen)
main_data %>% 
  ggplot(aes(x = rep_date_divi)) +
  geom_line(aes(y = A00_A14_death_cases), color = "blue") +
  geom_line(aes(y = A15_A34_death_cases), color = "green") +
  geom_line(aes(y = A35_A59_death_cases), color = "violet") +
  geom_line(aes(y = A60_A79_death_cases), color = "orange") +
  geom_line(aes(y = A80_death_cases), color = "yellow") +
  labs(title = "Todesfälle", x = "Zeit", y = "Todesfälle")

# Todesfälle (einfach) als 7-Tage-Inzidenz (vllt. ohne Multiplizieren mit 100000/inhabitants)
main_data %>% 
  ggplot(aes(x = rep_date_divi)) +
  geom_line(aes(y = seven_day_death_inz)) +
  labs(title = "7-Tages-Inzidenz der Todesfälle", x = "Zeit", y = "Todesfälle")
main_data %>% 
  ggplot(aes(x = rep_date_divi)) +
  geom_line(aes(y = seven_day_death_inz*83240000/100000)) +
  labs(title = "7-Tages-Inzidenz der Todesfälle", x = "Zeit", y = "Todesfälle")

# Todesfälle (nach Altersgruppen) als 7-Tage-Inzidenz (vllt. ohne Multiplizieren mit 100000/inhabitants)
main_data %>% 
  ggplot(aes(x = rep_date_divi)) +
  geom_line(aes(y = seven_day_death_inz_A00_A14), color = "blue") +
  geom_line(aes(y = seven_day_death_inz_A15_A34), color = "green") +
  geom_line(aes(y = seven_day_death_inz_A35_A59), color = "violet") +
  geom_line(aes(y = seven_day_death_inz_A60_A79), color = "orange") +
  geom_line(aes(y = seven_day_death_inz_A80), color = "yellow") +
  labs(title = "7-Tages-Inzidenz der Todesfälle", x = "Zeit", y = "Todesfälle")

# Vergleich der korrigierten und echten Inzidenz
source("Praktikum.R")
korr_inz_tot <- (main_data["2020-12-31" > rep_date_divi & rep_date_divi >= "2020-10-01"]$seven_day_death_inz)/CFR_jul_sep_total
korr_inz_A00_A14 <- (main_data["2020-12-31" > rep_date_divi & rep_date_divi >= "2020-10-01"]$seven_day_death_inz_A00_A14)/CFR_jul_sep_A00_A14 #keine Toten -> keine Inzidenz
korr_inz_A15_A34 <- (main_data["2020-12-31" > rep_date_divi & rep_date_divi >= "2020-10-01"]$seven_day_death_inz_A15_A34)/CFR_jul_sep_A15_A34
korr_inz_A35_A59 <- (main_data["2020-12-31" > rep_date_divi & rep_date_divi >= "2020-10-01"]$seven_day_death_inz_A35_A59)/CFR_jul_sep_A35_A59
korr_inz_A60_A79 <- (main_data["2020-12-31" > rep_date_divi & rep_date_divi >= "2020-10-01"]$seven_day_death_inz_A60_A79)/CFR_jul_sep_A60_A79
korr_inz_A80 <- (main_data["2020-12-31" > rep_date_divi & rep_date_divi >= "2020-10-01"]$seven_day_death_inz_A80)/CFR_jul_sep_A80

#Datensatz: Zeitraum Oktober bis Dezember
data_okt_dez <- main_data["2020-12-31" > rep_date_divi & rep_date_divi >= "2020-10-01"]
test1 <- as.data.table(c(korr_inz_tot), na.rm = T)
setnames(test1, c("V1"), c("korr_inz_tot"))
test2 <- as.data.table(c(korr_inz_A00_A14), na.rm = T)
setnames(test2, c("V1"), c("korr_inz_A00_A14"))
test3 <- as.data.table(c(korr_inz_A15_A34), na.rm = T)
setnames(test3, c("V1"), c("korr_inz_A15_A34"))
test4 <- as.data.table(c(korr_inz_A35_A59), na.rm = T)
setnames(test4, c("V1"), c("korr_inz_A35_A59"))
test5 <- as.data.table(c(korr_inz_A60_A79), na.rm = T)
setnames(test5, c("V1"), c("korr_inz_A60_A79"))
test6 <- as.data.table(c(korr_inz_A80), na.rm = T)
setnames(test6, c("V1"), c("korr_inz_A80"))
test <- cbind(test1, test2, test3, test4, test5, test6)
data_okt_dez <- cbind(data_okt_dez, test)

#Plots
data_okt_dez %>% 
  ggplot(aes(x = rep_date_divi)) +
  geom_line(aes(y = seven_day_inz_A15_A34), color = "green1") +
  geom_line(aes(y = seven_day_inz_A35_A59), color = "violet") +
  geom_line(aes(y = seven_day_inz_A60_A79), color = "orange1") +
  geom_line(aes(y = seven_day_inz_A80), color = "yellow1") +
  geom_line(aes(y = korr_inz_A15_A34), color = "green2") +
  geom_line(aes(y = korr_inz_A35_A59), color = "red") +
  geom_line(aes(y = korr_inz_A60_A79), color = "orange2") +
  geom_line(aes(y = korr_inz_A80), color = "yellow2") +
  labs(title = "korrigierte vs. offizielle Inzidenz", x = "Zeit", y = "7-Tage-Inzidenz")
#Altersgruppe 15-34
data_okt_dez %>% 
  ggplot(aes(x = rep_date_divi)) +
  geom_line(aes(y = seven_day_inz_A15_A34), color = "green") +
  geom_line(aes(y = korr_inz_A15_A34), color = "red") +
  labs(title = "korrigierte vs. offizielle Inzidenz", x = "Zeit", y = "7-Tage-Inzidenz")
#Altersgruppe 35-59
data_okt_dez %>% 
  ggplot(aes(x = rep_date_divi)) +
  geom_line(aes(y = seven_day_inz_A35_A59), color = "green") +
  geom_line(aes(y = korr_inz_A35_A59), color = "red") +
  labs(title = "korrigierte vs. offizielle Inzidenz", x = "Zeit", y = "7-Tage-Inzidenz")
#Altersgruppe 60-79
data_okt_dez %>% 
  ggplot(aes(x = rep_date_divi)) +
  geom_line(aes(y = seven_day_inz_A60_A79), color = "green") +
  geom_line(aes(y = korr_inz_A60_A79), color = "red") +
  labs(title = "korrigierte vs. offizielle Inzidenz", x = "Zeit", y = "7-Tage-Inzidenz")
#Altersgruppe 80+
data_okt_dez %>% 
  ggplot(aes(x = rep_date_divi)) +
  geom_line(aes(y = seven_day_inz_A80), color = "green") +
  geom_line(aes(y = korr_inz_A80), color = "red") +
  labs(title = "korrigierte vs. offizielle Inzidenz", x = "Zeit", y = "7-Tage-Inzidenz")
#Overall
data_okt_dez %>% 
  ggplot(aes(x = rep_date_divi)) +
  geom_line(aes(y = seven_day_inz), color = "green") +
  geom_line(aes(y = korr_inz_tot), color = "red") +
  labs(title = "korrigierte vs. offizielle Inzidenz", x = "Zeit", y = "7-Tage-Inzidenz")
