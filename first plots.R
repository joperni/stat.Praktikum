library(data.table)
library(tidyverse)
library(checkmate)
main_data <- readRDS("data/main_data")
source("help_functions/seven_day_inzidenz.R")
source("examples_of_code/example_aggregating.R")
setDT(main_data)
cnames <- colnames(main_data)
inhabitants <- 83240000
col_alter <- adjustcolor(col = c("black", "orange", "blue", "violet", "red"),
                         alpha.f = 0.3)
main_data_divi <- main_data[rep_date_divi >= "2020-04-24"]

# 7-Tage-Inzidenz (einfach)
main_data %>% 
  ggplot(aes(x = rep_date_divi)) +
  geom_line(aes(y = seven_day_inz)) +
  labs(title = "7-Tage-Inzidenz", x = "Zeit", y = "7-Tage-Inzidenz")
# 7-Tage-Inzidenz (nach Altersgruppen) 
seven_day_alter <- main_data %>% 
  ggplot(aes(x = rep_date_divi)) +
  geom_line(aes(y = seven_day_inz_A00_A14, color = "black")) +
  geom_line(aes(y = seven_day_inz_A15_A34), color = "orange") +
  geom_line(aes(y = seven_day_inz_A35_A59), color = "blue") +
  geom_line(aes(y = seven_day_inz_A60_A79), color = "violet") +
  geom_line(aes(y = seven_day_inz_A80), color = "red") +
  labs(x = "Zeit", y = "7-Tage-Inzidenz") +
  scale_y_continuous(labels = scales::comma,limits = c(0, 400)) +
  scale_color_manual(values = c("0-14 Jahre" = "black", "15-34 Jahre" = "orange", "35-59 Jahre" = "blue",
                                "60-79 Jahre" = "violet", "ü80 Jahre" = "red")) +
  theme(axis.text.x = element_text(size = 12), axis.title.x = element_text(size = 15),
        axis.text.y = element_text(size = 12), axis.title.y = element_text(size = 15))
seven_day_alter
ggsave("Plots/7-Tage-Inzidenz nach Alter.png", plot = seven_day_alter, width = 20, height = 10, units = c("cm"))

# Intensivbettenbelegung
betten <- main_data_divi %>% 
  ggplot(aes(x = rep_date_divi)) +
  geom_line(aes(y = cases_covid_divi)) +
  labs(x = "Zeit", y = "belegte Betten") +
  scale_y_continuous(labels = scales::comma,limits = c(0, 6000)) +
  theme(axis.text.x = element_text(size = 12), axis.title.x = element_text(size = 15),
        axis.text.y = element_text(size = 12), axis.title.y = element_text(size = 15))
betten
ggsave("Plots/intensivbettenbelegung.png", plot = betten, width = 20, height = 10, units = c("cm"))

# Intensivbettenbelegung als 7-Tage-Inzidenz (macht als Wert wenig Sinn in meinen Augen)
main_data_divi %>% 
  ggplot(aes(x = rep_date_divi)) +
  geom_line(aes(y = seven_day_inz(cases_covid_divi))) +
  labs(x = "Zeit", y = "belegte Betten als 7-Tage-Inzidenz")

# Todesfälle (einfach)
main_data %>% 
  ggplot(aes(x = rep_date_divi)) +
  geom_line(aes(y = total_death_cases)) +
  labs(title = "Todesfälle", x = "Zeit", y = "Todesfälle")

# Todesfälle (nach Altersgruppen)
main_data %>% 
  ggplot(aes(x = rep_date_divi)) +
  geom_line(aes(y = A00_A14_death_cases), color = "black") +
  geom_line(aes(y = A15_A34_death_cases), color = "orange") +
  geom_line(aes(y = A35_A59_death_cases), color = "blue") +
  geom_line(aes(y = A60_A79_death_cases), color = "violet") +
  geom_line(aes(y = A80_death_cases), color = "red") +
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
seven_death_alter <- main_data %>% 
  ggplot(aes(x = rep_date_divi)) +
  geom_line(aes(y = seven_day_death_inz_A00_A14, color = "black")) +
  geom_line(aes(y = seven_day_death_inz_A15_A34), color = "orange") +
  geom_line(aes(y = seven_day_death_inz_A35_A59), color = "blue") +
  geom_line(aes(y = seven_day_death_inz_A60_A79), color = "violet") +
  geom_line(aes(y = seven_day_death_inz_A80), color = "red") +
  labs(x = "Zeit", y = "Todesfälle") +
  scale_y_continuous(labels = scales::comma,limits = c(0, 85)) +
  scale_color_manual(values = c("0-14 Jahre" = "black", "15-34 Jahre" = "orange", "35-59 Jahre" = "blue",
                                "60-79 Jahre" = "violet", "ü80 Jahre" = "red")) +
  theme(axis.text.x = element_text(size = 12), axis.title.x = element_text(size = 15),
        axis.text.y = element_text(size = 12), axis.title.y = element_text(size = 15))
seven_death_alter
ggsave("Plots/Todesfälle nach Alter.png", plot = seven_death_alter, width = 20, height = 10, units = c("cm"))

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
data_okt_dez <- data_okt_dez["2020-12-24" > rep_date_divi & rep_date_divi >= "2020-10-01"]

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
#Altersgruppe 15-34 -> nehmen!
plot_korr_inz_15_34 <- data_okt_dez %>% 
  ggplot(aes(x = rep_date_divi)) +
  geom_line(aes(y = seven_day_inz_A15_A34, color = "black")) +
  geom_line(aes(y = korr_inz_A15_A34), color = "red") +
  labs(x = "Zeit", y = "7-Tage-Inzidenz") +
  scale_y_continuous(labels = scales::comma,limits = c(0, 600)) +
  scale_color_manual(values = c("offiziell" = "black", "korrigiert" = "red")) +
  theme(axis.text.x = element_text(size = 12), axis.title.x = element_text(size = 15),
        axis.text.y = element_text(size = 12), axis.title.y = element_text(size = 15))
plot_korr_inz_15_34
ggsave("Plots/korrigierte Inzidenz 15-34.png", plot = plot_korr_inz_15_34, width = 20, height = 10, units = c("cm"))

#Altersgruppe 35-59 -> nehmen!
plot_korr_inz_35_59 <- data_okt_dez %>% 
  ggplot(aes(x = rep_date_divi)) +
  geom_line(aes(y = seven_day_inz_A35_A59, color = "black")) +
  geom_line(aes(y = korr_inz_A35_A59), color = "red") +
  labs(x = "Zeit", y = "7-Tage-Inzidenz") +
  scale_y_continuous(labels = scales::comma,limits = c(0, 600)) +
  scale_color_manual(values = c("offiziell" = "black", "korrigiert" = "red")) +
  theme(axis.text.x = element_text(size = 12), axis.title.x = element_text(size = 15),
        axis.text.y = element_text(size = 12), axis.title.y = element_text(size = 15))
plot_korr_inz_35_59
ggsave("Plots/korrigierte Inzidenz 35-59.png", plot = plot_korr_inz_35_59, width = 20, height = 10, units = c("cm"))

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
#Overall -> nehmen!
plot_korr_inz <- data_okt_dez %>% 
  ggplot(aes(x = rep_date_divi)) +
  geom_line(aes(y = seven_day_inz, color = "black")) +
  geom_line(aes(y = korr_inz_tot), color = "red") +
  labs(x = "Zeit", y = "7-Tage-Inzidenz") +
  scale_y_continuous(labels = scales::comma,limits = c(0, 1500)) +
  scale_color_manual(values = c("offiziell" = "black", "korrigiert" = "red")) +
  theme(axis.text.x = element_text(size = 12), axis.title.x = element_text(size = 15),
        axis.text.y = element_text(size = 12), axis.title.y = element_text(size = 15))
plot_korr_inz
ggsave("Plots/korrigierte Inzidenz.png", plot = plot_korr_inz, width = 20, height = 10, units = c("cm"))
