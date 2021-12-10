library(data.table)
library(tidyverse)
library(checkmate)
library(lubridate)
main_data <- readRDS("data/main_data")
source("help_functions/seven_day_inzidenz.R")
source("examples_of_code/example_aggregating.R")


# general transformation --------------------------------------------------

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

# Todesfälle (nach Altersgruppen) als 7-Tage-Inzidenz
dt_seven_day_deaths <- main_data[, c(1, 113, 120:124)]
setnames(dt_seven_day_deaths, c("seven_day_death_inz", "seven_day_death_inz_A00_A14", "seven_day_death_inz_A15_A34", "seven_day_death_inz_A35_A59",
                                "seven_day_death_inz_A60_A79", "seven_day_death_inz_A80"),
         c("overall", "0-14 Jahre", "15-34 Jahre", "35-59 Jahre", "60-79 Jahre", "über 80 Jahre"))
dt_seven_day_deaths[, time := dt_seven_day_deaths$rep_date_divi]
dt_seven_day_deaths <- dt_seven_day_deaths[, -1]
dt_seven_day_deaths_melt <- melt(dt_seven_day_deaths, id.vars = "time", value.name = "death")


# Verhaeltnis_transformation ----------------------------------------------

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

#Unterschaetzung durch die gemeldete Inzidenz
data_okt_dez$unterschaetzung_total <- data_okt_dez$korr_inz_tot/data_okt_dez$seven_day_inz
data_okt_dez$unterschaetzung_A15_A34 <- data_okt_dez$korr_inz_A15_A34/data_okt_dez$seven_day_inz_A15_A34
data_okt_dez$unterschaetzung_A35_A59 <- data_okt_dez$korr_inz_A35_A59/data_okt_dez$seven_day_inz_A35_A59
data_okt_dez$unterschaetzung_A60_A79 <- data_okt_dez$korr_inz_A60_A79/data_okt_dez$seven_day_inz_A60_A79
data_okt_dez$unterschaetzung_A80 <- data_okt_dez$korr_inz_A80/data_okt_dez$seven_day_inz_A80
dt_unterschaetzung <- data_okt_dez[, c(1, 131:135)]
setnames(dt_unterschaetzung, c("unterschaetzung_total", "unterschaetzung_A15_A34", "unterschaetzung_A35_A59", "unterschaetzung_A60_A79",
                              "unterschaetzung_A80"), c("Gesamt", "15-34 Jahre", "35-59 Jahre", "60-79 Jahre", "über 80 Jahre"))
dt_unterschaetzung[, time := dt_unterschaetzung$rep_date_divi]
dt_unterschaetzung <- dt_unterschaetzung[, -1]
dt_unterschaetzung_melt <- melt(dt_unterschaetzung, id.vars = "time", value.name = "inz")
