source("data_transformation_for_plots.R")
farben <- c("Gesamt" = "#000000", "0-14 Jahre" = "#B2DF8A","15-34 Jahre" = "#1F78B4",
             "35-59 Jahre" = "#33A02C", "60-79 Jahre" = "#FB9A99", "ueber 80 Jahre" = "#E31A1C")

# 7-Tage-Inzidenz (einfach)
main_data %>% 
  ggplot(aes(x = rep_date_divi)) +
  geom_line(aes(y = seven_day_inz)) +
  labs(title = "7-Tage-Inzidenz", x = "Zeit", y = "7-Tage-Inzidenz")
# 7-Tage-Inzidenz (nach Altersgruppen) 
seven_day_alter <- dt_seven_day_inz_melt %>%
  ggplot(aes(x = time, y = inz, color = variable)) +
  geom_line() +
  labs(x = "", y = "7-Tages-Inzidenz") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 400), breaks = c(0, 50, 100, 150, 200, 250, 300, 350, 400)) +
  scale_color_manual(values = farben, name = "Altersgruppe", labels = c(names(farben)[1:5], "Über 79 Jahre")) +
  theme(axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 13),
        axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 13)) +
  scale_x_date(breaks = as.Date(c("2020-01-01", "2020-04-01", "2020-07-01", "2020-10-01", "2021-01-01")),
               date_labels = "%d. %b %Y")
seven_day_alter
ggsave("Plots/7-Tage-Inzidenz nach Alter.png", plot = seven_day_alter, width = 20, height = 10, units = c("cm"))
# Intensivbettenbelegung
betten <- main_data_divi %>% 
  ggplot(aes(x = rep_date_divi)) +
  geom_line(aes(y = cases_covid_divi), color = "#000000") +
  labs(x = "", y = "belegte Intensivbetten") +
  scale_y_continuous(limits = c(0, 6000), breaks = c(0, 1000, 2000, 3000, 4000, 5000, 6000)) +
  theme(axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 13),
        axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 13)) +
  scale_x_date(breaks = as.Date(c("2020-05-01", "2020-07-01", "2020-10-01", "2021-01-01")),
               date_labels = "%d. %b %Y")
betten
ggsave("Plots/intensivbettenbelegung.png", plot = betten, width = 20, height = 10, units = c("cm"))

# Intensivbettenbelegung als 7-Tage-Inzidenz (macht als Wert wenig Sinn in meinen Augen)
main_data_divi %>% 
  ggplot(aes(x = rep_date_divi)) +
  geom_line(aes(y = seven_day_hosp_inz)) +
  labs(x = "Zeit", y = "belegte Betten als 7-Tage-Inzidenz") +
  scale_y_continuous(labels = scales::comma,limits = c(0, 50)) +
  theme(axis.text.x = element_text(size = 12), axis.title.x = element_text(size = 15),
        axis.text.y = element_text(size = 12), axis.title.y = element_text(size = 15)) +
  scale_x_date(breaks = as.Date(c("2020-05-01", "2020-07-01", "2020-10-01", "2021-01-01")),
               date_labels = "%d. %B %Y")


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

seven_death_alter <- dt_seven_day_deaths_melt %>%
  ggplot(aes(x = time, y = death, color = variable)) +
  geom_line() +
  labs(x = "", y = "7-Tages-Todesfälle") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 85), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80)) +
  scale_color_manual(values = farben, name = "Altersgruppe", labels = c(names(farben)[1:5], "Über 79 Jahre")) +
  theme(axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 13),
        axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 13)) +
  scale_x_date(breaks = as.Date(c("2020-01-01", "2020-04-01", "2020-07-01", "2020-10-01", "2021-01-01")),
               date_labels = "%d. %b %Y")
seven_death_alter
ggsave("Plots/Todesfälle nach Alter.png", plot = seven_death_alter, width = 20, height = 10, units = c("cm"))
