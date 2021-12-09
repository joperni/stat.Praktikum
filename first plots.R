library(data.table)
library(tidyverse)
library(checkmate)
library(lubridate)
main_data <- readRDS("data/main_data")
source("help_functions/seven_day_inzidenz.R")
source("examples_of_code/example_aggregating.R")
setDT(main_data)
cnames <- colnames(main_data)
inhabitants <- 83240000
main_data_divi <- main_data[rep_date_divi >= "2020-05-01"]
farben <- c("Gesamt" = "#000000", "0-14 Jahre" = "#A6CEE3", "15-34 Jahre" = "#1F78B4",
            "35-59 Jahre" = "#33A02C", "60-79 Jahre" = "#FB9A99", "über 80 Jahre" = "#E31A1C")

dt_seven_day_inz <- main_data[, c(1, 112, 115:119)]
setnames(dt_seven_day_inz, c("seven_day_inz", "seven_day_inz_A00_A14", "seven_day_inz_A15_A34", "seven_day_inz_A35_A59", "seven_day_inz_A60_A79",
                             "seven_day_inz_A80"), c("Gesamt", "0-14 Jahre", "15-34 Jahre", "35-59 Jahre", "60-79 Jahre", "über 80 Jahre"))
dt_seven_day_inz[, time := dt_seven_day_inz$rep_date_divi]
dt_seven_day_inz <- dt_seven_day_inz[, -1]
dt_seven_day_inz_melt <- melt(dt_seven_day_inz, id.vars = "time", value.name = "inz")



# 7-Tage-Inzidenz (einfach)
main_data %>% 
  ggplot(aes(x = rep_date_divi)) +
  geom_line(aes(y = seven_day_inz)) +
  labs(title = "7-Tage-Inzidenz", x = "Zeit", y = "7-Tage-Inzidenz")
# 7-Tage-Inzidenz (nach Altersgruppen) 
seven_day_alter <- dt_seven_day_inz_melt %>%
  ggplot(aes(x = time, y = inz, color = variable)) +
  geom_line() +
  labs(x = "Zeit", y = "7-Tages Neuinfektionen pro 100.000 Einw.") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 400), breaks = c(0, 50, 100, 150, 200, 250, 300, 350, 400)) +
  scale_color_manual(values = farben, name = "Altersgruppe") +
  theme(axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 13),
        axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 13)) +
  scale_x_date(breaks = as.Date(c("2020-01-01", "2020-04-01", "2020-07-01", "2020-10-01", "2021-01-01")),
               date_labels = "%d. %B %Y")
seven_day_alter
ggsave("Plots/7-Tage-Inzidenz nach Alter.png", plot = seven_day_alter, width = 20, height = 10, units = c("cm"))

# Intensivbettenbelegung
betten <- main_data_divi %>% 
  ggplot(aes(x = rep_date_divi)) +
  geom_line(aes(y = seven_day_hosp_inz), color = "#000000") +
  labs(x = "Zeit", y = "belegte Intensivbetten") +
  scale_y_continuous(labels = scales::comma,limits = c(0, 6000), breaks = c(0, 1000, 2000, 3000, 4000, 5000, 6000)) +
  theme(axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 13),
        axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 13)) +
  scale_x_date(breaks = as.Date(c("2020-05-01", "2020-07-01", "2020-10-01", "2021-01-01")),
               date_labels = "%d. %B %Y")
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

# Todesfälle (nach Altersgruppen) als 7-Tage-Inzidenz
dt_seven_day_deaths <- main_data[, c(1, 113, 120:124)]
setnames(dt_seven_day_deaths, c("seven_day_death_inz", "seven_day_death_inz_A00_A14", "seven_day_death_inz_A15_A34", "seven_day_death_inz_A35_A59",
                                "seven_day_death_inz_A60_A79", "seven_day_death_inz_A80"),
         c("overall", "0-14 Jahre", "15-34 Jahre", "35-59 Jahre", "60-79 Jahre", "über 80 Jahre"))
dt_seven_day_deaths[, time := dt_seven_day_deaths$rep_date_divi]
dt_seven_day_deaths <- dt_seven_day_deaths[, -1]
dt_seven_day_deaths_melt <- melt(dt_seven_day_deaths, id.vars = "time", value.name = "death")
seven_death_alter <- dt_seven_day_deaths_melt %>%
  ggplot(aes(x = time, y = death, color = variable)) +
  geom_line() +
  labs(x = "Zeit", y = "7-Tages-Todesfälle pro 100.000 Einw.") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 85), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80)) +
  scale_color_manual(values = farben, name = "Altersgruppe") +
  theme(axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 13),
        axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 13)) +
  scale_x_date(breaks = as.Date(c("2020-01-01", "2020-04-01", "2020-07-01", "2020-10-01", "2021-01-01")),
               date_labels = "%d. %B %Y")
seven_death_alter
ggsave("Plots/Todesfälle nach Alter.png", plot = seven_death_alter, width = 20, height = 10, units = c("cm"))

# Vergleich der korrigierten und echten Inzidenz
source("Praktikum.R")
korr_inz_tot <- (main_data["2020-12-23" > rep_date_divi & rep_date_divi >= "2020-09-25"]$seven_day_death_inz)/CFR_jul_sep_total
korr_inz_A00_A14 <- (main_data["2020-12-23" > rep_date_divi & rep_date_divi >= "2020-09-25"]$seven_day_death_inz_A00_A14)/CFR_jul_sep_A00_A14 #keine Toten -> keine Inzidenz
korr_inz_A15_A34 <- (main_data["2020-12-23" > rep_date_divi & rep_date_divi >= "2020-09-25"]$seven_day_death_inz_A15_A34)/CFR_jul_sep_A15_A34
korr_inz_A35_A59 <- (main_data["2020-12-23" > rep_date_divi & rep_date_divi >= "2020-09-25"]$seven_day_death_inz_A35_A59)/CFR_jul_sep_A35_A59
korr_inz_A60_A79 <- (main_data["2020-12-23" > rep_date_divi & rep_date_divi >= "2020-09-25"]$seven_day_death_inz_A60_A79)/CFR_jul_sep_A60_A79
korr_inz_A80 <- (main_data["2020-12-23" > rep_date_divi & rep_date_divi >= "2020-09-25"]$seven_day_death_inz_A80)/CFR_jul_sep_A80

#Datensatz: Zeitraum 25. September bis 23. Dezember
data_okt_dez <- main_data["2020-12-23" > rep_date_divi & rep_date_divi >= "2020-09-25"]
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

farben2 <-  c("gemeldete" = "#000000", "korrigierte" = "#E31A1C")

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
dt_korr_inz_15_34 <- data_okt_dez[, c(1, 116, 127)]
setnames(dt_korr_inz_15_34, c("seven_day_inz_A15_A34", "korr_inz_A15_A34"), c("gemeldete", "korrigierte"))
dt_korr_inz_15_34[, time := dt_korr_inz_15_34$rep_date_divi]
dt_korr_inz_15_34 <- dt_korr_inz_15_34[, -1]
dt_korr_inz_15_34_melt <- melt(dt_korr_inz_15_34, id.vars = "time", value.name = "inz")
plot_korr_inz_15_34 <- dt_korr_inz_15_34_melt %>% 
  ggplot(aes(x = time, y = inz, color = variable)) +
  geom_line() +
  labs(x = "Zeit", y = "7-Tages Neuinfektionen pro 100.000 Einw.") +
  scale_y_continuous(labels = scales::comma,limits = c(0, 500)) +
  scale_color_manual(values = farben2, name = "Art der Inzidenz") +
  theme(axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 13),
        axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 13)) +
  scale_x_date(breaks = as.Date(c("2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%d. %B %Y")
plot_korr_inz_15_34
ggsave("Plots/korrigierte Inzidenz 15-34.png", plot = plot_korr_inz_15_34, width = 20, height = 10, units = c("cm"))

#Altersgruppe 35-59 -> nehmen!
dt_korr_inz_35_59 <- data_okt_dez[, c(1, 117, 128)]
setnames(dt_korr_inz_35_59, c("seven_day_inz_A35_A59", "korr_inz_A35_A59"), c("gemeldete", "korrigierte"))
dt_korr_inz_35_59[, time := dt_korr_inz_35_59$rep_date_divi]
dt_korr_inz_35_59 <- dt_korr_inz_35_59[, -1]
dt_korr_inz_35_59_melt <- melt(dt_korr_inz_35_59, id.vars = "time", value.name = "inz")
plot_korr_inz_35_59 <- dt_korr_inz_35_59_melt %>% 
  ggplot(aes(x = time, y = inz, color = variable)) +
  geom_line() +
  labs(x = "Zeit", y = "7-Tages Neuinfektionen pro 100.000 Einw.") +
  scale_y_continuous(labels = scales::comma,limits = c(0, 600), breaks = c(0, 100, 200, 300, 400, 500, 600)) +
  scale_color_manual(values = farben2, name = "Art der Inzidenz") +
  theme(axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 13),
        axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 13)) +
  scale_x_date(breaks = as.Date(c("2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%d. %B %Y")
plot_korr_inz_35_59
ggsave("Plots/korrigierte Inzidenz 35-59.png", plot = plot_korr_inz_35_59, width = 20, height = 10, units = c("cm"))

#Altersgruppe 60-79
dt_korr_inz_60_79 <- data_okt_dez[, c(1, 118, 129)]
setnames(dt_korr_inz_60_79, c("seven_day_inz_A60_A79", "korr_inz_A60_A79"), c("gemeldete", "korrigierte"))
dt_korr_inz_60_79[, time := dt_korr_inz_60_79$rep_date_divi]
dt_korr_inz_60_79 <- dt_korr_inz_60_79[, -1]
dt_korr_inz_60_79_melt <- melt(dt_korr_inz_60_79, id.vars = "time", value.name = "inz")
plot_korr_inz_60_79 <- dt_korr_inz_60_79_melt %>%
ggplot(aes(x = time, y = inz, color = variable)) +
  geom_line() +
  labs(x = "Zeit", y = "7-Tages Neuinfektionen pro 100.000 Einw.") +
  scale_y_continuous(labels = scales::comma,limits = c(0, 500)) +
  scale_color_manual(values = farben2, name = "Art der Inzidenz") +
  theme(axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 13),
        axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 13)) +
  scale_x_date(breaks = as.Date(c("2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%d. %B %Y")
plot_korr_inz_60_79
ggsave("Plots/korrigierte Inzidenz 60-79.png", plot = plot_korr_inz_60_79, width = 20, height = 10, units = c("cm"))

#Altersgruppe 80+
dt_korr_inz_80 <- data_okt_dez[, c(1, 119, 130)]
setnames(dt_korr_inz_80, c("seven_day_inz_A80", "korr_inz_A80"), c("gemeldete", "korrigierte"))
dt_korr_inz_80[, time := dt_korr_inz_80$rep_date_divi]
dt_korr_inz_80 <- dt_korr_inz_80[, -1]
dt_korr_inz_80_melt <- melt(dt_korr_inz_80, id.vars = "time", value.name = "inz")
plot_korr_inz_80 <- dt_korr_inz_80_melt %>% 
  ggplot(aes(x = time, y = inz, color = variable)) +
  geom_line() +
  labs(x = "Zeit", y = "7-Tages Neuinfektionen pro 100.000 Einw.") +
  scale_y_continuous(labels = scales::comma,limits = c(0, 600), breaks = c(0, 100, 200, 300, 400, 500, 600)) +
  scale_color_manual(values = farben2, name = "Art der Inzidenz") +
  theme(axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 13),
        axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 13)) +
  scale_x_date(breaks = as.Date(c("2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%d. %B %Y")
plot_korr_inz_80
ggsave("Plots/korrigierte Inzidenz über 80.png", plot = plot_korr_inz_80, width = 20, height = 10, units = c("cm"))

#Overall -> nehmen!
dt_korr_inz <- data_okt_dez[, c(1, 112, 125)]
setnames(dt_korr_inz, c("seven_day_inz", "korr_inz_tot"), c("gemeldete", "korrigierte"))
dt_korr_inz[, time := dt_korr_inz$rep_date_divi]
dt_korr_inz <- dt_korr_inz[, -1]
dt_korr_inz_melt <- melt(dt_korr_inz, id.vars = "time", value.name = "inz")
plot_korr_inz <- dt_korr_inz_melt %>% 
  ggplot(aes(x = time, y = inz, color = variable)) +
  geom_line() +
  labs(x = "Zeit", y = "7-Tages Neuinfektionen pro 100.000 Einw.") +
  scale_y_continuous(labels = scales::comma,limits = c(0, 1400), breaks = c(0, 200, 400, 600, 800, 1000, 1200, 1400)) +
  scale_color_manual(values = farben2, name = "Art der Inzidenz") +
  theme(axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 13),
        axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 13)) +
  scale_x_date(breaks = as.Date(c("2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%d. %B %Y")
plot_korr_inz
ggsave("Plots/korrigierte Inzidenz.png", plot = plot_korr_inz, width = 20, height = 10, units = c("cm"))

#Unterschätzung durch die gemeldete Inzidenz
data_okt_dez$unterschätzung_total <- data_okt_dez$korr_inz_tot/data_okt_dez$seven_day_inz
data_okt_dez$unterschätzung_A15_A34 <- data_okt_dez$korr_inz_A15_A34/data_okt_dez$seven_day_inz_A15_A34
data_okt_dez$unterschätzung_A35_A59 <- data_okt_dez$korr_inz_A35_A59/data_okt_dez$seven_day_inz_A35_A59
data_okt_dez$unterschätzung_A60_A79 <- data_okt_dez$korr_inz_A60_A79/data_okt_dez$seven_day_inz_A60_A79
data_okt_dez$unterschätzung_A80 <- data_okt_dez$korr_inz_A80/data_okt_dez$seven_day_inz_A80
dt_unterschätzung <- data_okt_dez[, c(1, 131:135)]
setnames(dt_unterschätzung, c("unterschätzung_total", "unterschätzung_A15_A34", "unterschätzung_A35_A59", "unterschätzung_A60_A79",
                        "unterschätzung_A80"), c("Gesamt", "15-34 Jahre", "35-59 Jahre", "60-79 Jahre", "über 80 Jahre"))
dt_unterschätzung[, time := dt_unterschätzung$rep_date_divi]
dt_unterschätzung <- dt_unterschätzung[, -1]
dt_unterschätzung_melt <- melt(dt_unterschätzung, id.vars = "time", value.name = "inz")

farben3 <- c("Gesamt" = "#000000", "15-34 Jahre" = "#1F78B4",
             "35-59 Jahre" = "#33A02C", "60-79 Jahre" = "#FB9A99", "über 80 Jahre" = "#E31A1C")

plot_unterschätzung <- dt_unterschätzung_melt %>% 
  ggplot(aes(x = time, y = inz, color = variable)) +
  geom_line() +
  labs(x = "Zeit", y = "Faktor der Unterschätzung") +
  scale_y_continuous(labels = scales::comma,limits = c(0, 7), breaks = c(0, 1, 2, 3, 4, 5, 6, 7)) +
  scale_color_manual(values = farben3, name = "Altersgruppe") +
  theme(axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 13),
        axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 13)) +
  scale_x_date(breaks = as.Date(c("2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%d. %B %Y")
plot_unterschätzung
ggsave("Plots/Unterschätzung Inzidenz.png", plot = plot_unterschätzung, width = 20, height = 10, units = c("cm"))
