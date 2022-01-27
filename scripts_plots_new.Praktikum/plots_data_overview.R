library(ggplot2)
library(dplyr)
library(cowplot)
library(gridExtra)
source("data_transformation_for_plots.R")

farben <- c("Gesamt" = "#010101", "0-14 Jahre" = "#B2DF8A","15-34 Jahre" = "#1F78B4",
           "35-59 Jahre" = "#33A02C", "60-79 Jahre" = "#FB9A99", "Über 79 Jahre" = "#E31A1C")

# 7-Tage-Inzidenz (nach Altersgruppen) 
ppseven_day_alter = dt_seven_day_inz_melt %>%
  ggplot(aes(x = time, y = inz, color = variable)) +
  geom_line() +
  labs(x = "", y = "7-Tages-Inzidenz") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 405), breaks = c(0, 50, 100, 150, 200, 250, 300, 350, 400)) +
  scale_color_manual(values = farben, name = "Altersgruppe", labels = c(names(farben)[1:5], "Über 79 Jahre")) +
  theme(axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 13),
        axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 13)) +
  scale_x_date(breaks = as.Date(c("2020-01-01", "2020-04-01", "2020-07-01", "2020-10-01", "2021-01-01")),
               date_labels = "%d. %b %Y") +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black", size=1),
        panel.grid = element_line(colour = "gray57", size = 0.2),
        axis.title.y = element_text(margin = margin(t = 0, r = 9, b = 0, l = 0)),
        axis.text   = element_text(colour = "black"))

# 7TI für Startseite
banner = ppseven_day_alter + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="none", axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())
ggsave("Plots/Hintergrund.png", plot = banner, width = 21, height = 10, units = c("cm"))

# Intensivbettenbelegung
ppbetten = main_data_divi %>% 
  ggplot(aes(x = rep_date_divi)) +
  geom_line(aes(y = cases_covid_divi), color = "#000000") +
  labs(x = "", y = "Belegte Intensivbetten") +
  scale_y_continuous(limits = c(0, 6000), breaks = c(0, 1000, 2000, 3000, 4000, 5000, 6000)) +
  theme(axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 13),
        axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 13)) +
  scale_x_date(breaks = as.Date(c("2020-01-01", "2020-04-01", "2020-07-01", "2020-10-01", "2021-01-01")),
               date_labels = "%d. %b %Y") +
  theme_bw() +
  expand_limits(x = as.Date("2020-01-01"), date_labels = "%d. %b %Y") +
  theme(panel.border = element_rect(colour = "black", size=1),
        panel.grid = element_line(colour = "gray57", size = 0.2),
        axis.title.y = element_text(margin = margin(t = 0, r = 4, b = 0, l = 0)),
        axis.text   = element_text(colour = "black"))

# 7-Tages-Todesfaelle nach Alter
dt_seven_day_deaths_melt = as.data.frame(dt_seven_day_deaths_melt)
levels(dt_seven_day_deaths_melt$variable)[1] = c("Gesamt")

ppdeaths = ggplot(aes(x = time, y = death, color = variable), data = dt_seven_day_deaths_melt) +
  geom_line() +
  labs(x = "", y = "7-Tages-Todesfälle") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 81), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80)) +
  scale_color_manual(values = farben, name = "Altersgruppe", labels = c(names(farben)[1:6], "Über 79 Jahre")) +
  theme(axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 13),
        axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 13)) +
  scale_x_date(breaks = as.Date(c("2020-01-01", "2020-04-01", "2020-07-01", "2020-10-01", "2021-01-01")),
               date_labels = "%d. %b %Y") +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black", size=1),
        panel.grid = element_line(colour = "gray57", size = 0.2),
        axis.title.y = element_text(margin = margin(t = 0, r = 13, b = 0, l = 0)),
        axis.text   = element_text(colour = "black"))

# Alignment
aligned <- align_plots(ppseven_day_alter,
                       ppbetten,
                       ppdeaths,
                       align = "hv",
                       axis = "tblr")
ggsave("Plots/7-Tage-Inzidenz nach Alter.png", plot = ggdraw(aligned[[1]]), width = 21, height = 10, units = c("cm"))
ggsave("Plots/intensivbettenbelegung.png", plot = ggdraw(aligned[[2]]), width = 21, height = 10, units = c("cm"))
ggsave("Plots/Todesfälle nach Alter.png", plot = ggdraw(aligned[[3]]), width = 21, height = 10, units = c("cm"))
