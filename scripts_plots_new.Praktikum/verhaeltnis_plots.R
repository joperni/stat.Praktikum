# Vergleich der korrigierten und echten Inzidenz
source("data_transformation_for_plots.R")
source("Praktikum.R")

farben2 <-  c("gemeldete" = "#000000", "korrigierte" = "#E31A1C")
data_okt_dez$kor_inz_A00_A14 <- kor_inz_A00_A14
data_okt_dez$kor_inz_A15_A34 <- kor_inz_A15_A34
data_okt_dez$kor_inz_A35_A59 <- kor_inz_A35_A59
data_okt_dez$kor_inz_A60_A79 <- kor_inz_A60_A79
data_okt_dez$kor_inz_A80 <- kor_inz_A80
data_okt_dez$kor_inz_total <- kor_inz_total

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
dt_korr_inz_15_34 <- data_okt_dez[, c("rep_date_divi", "seven_day_inz_A15_A34", "kor_inz_A15_A34")]
setnames(dt_korr_inz_15_34, c("seven_day_inz_A15_A34", "kor_inz_A15_A34"), c("gemeldete", "korrigierte"))
dt_korr_inz_15_34[, time := dt_korr_inz_15_34$rep_date_divi]
dt_korr_inz_15_34 <- dt_korr_inz_15_34[, -1]
dt_korr_inz_15_34_melt <- melt(dt_korr_inz_15_34, id.vars = "time", value.name = "inz")
plot_korr_inz_15_34 <- dt_korr_inz_15_34_melt %>% 
  ggplot(aes(x = time, y = inz, color = variable)) +
  geom_line() +
  labs(x = "", y = "7-Tages-Inzidenz") +
  scale_y_continuous(labels = scales::comma,limits = c(0, 600), breaks = c(0, 100, 200, 300, 400, 500, 600)) +
  scale_color_manual(values = farben2, name = "Art der Inzidenz") +
  theme(axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 13),
        axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 13)) +
  scale_x_date(breaks = as.Date(c("2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%d. %b %Y") + theme_bw()
plot_korr_inz_15_34
ggsave("Plots/korrigierte Inzidenz 15-34.png", plot = plot_korr_inz_15_34, width = 20, height = 10, units = c("cm"))

#Altersgruppe 35-59 -> nehmen!
dt_korr_inz_35_59 <- data_okt_dez[, c("rep_date_divi", "seven_day_inz_A35_A59", "kor_inz_A35_A59")]
setnames(dt_korr_inz_35_59, c("seven_day_inz_A35_A59", "kor_inz_A35_A59"), c("gemeldete", "korrigierte"))
dt_korr_inz_35_59[, time := dt_korr_inz_35_59$rep_date_divi]
dt_korr_inz_35_59 <- dt_korr_inz_35_59[, -1]
dt_korr_inz_35_59_melt <- melt(dt_korr_inz_35_59, id.vars = "time", value.name = "inz")
plot_korr_inz_35_59 <- dt_korr_inz_35_59_melt %>% 
  ggplot(aes(x = time, y = inz, color = variable)) +
  geom_line() +
  labs(x = "", y = "7-Tages-Inzidenz") +
  scale_y_continuous(labels = scales::comma,limits = c(0, 600), breaks = c(0, 100, 200, 300, 400, 500, 600)) +
  scale_color_manual(values = farben2, name = "Art der Inzidenz") +
  theme(axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 13),
        axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 13)) +
  scale_x_date(breaks = as.Date(c("2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%d. %b %Y") + theme_bw()
plot_korr_inz_35_59
ggsave("Plots/korrigierte Inzidenz 35-59.png", plot = plot_korr_inz_35_59, width = 20, height = 10, units = c("cm"))

#Altersgruppe 60-79
dt_korr_inz_60_79 <- data_okt_dez[, c("rep_date_divi", "seven_day_inz_A60_A79", "kor_inz_A60_A79")]
setnames(dt_korr_inz_60_79, c("seven_day_inz_A60_A79", "kor_inz_A60_A79"), c("gemeldete", "korrigierte"))
dt_korr_inz_60_79[, time := dt_korr_inz_60_79$rep_date_divi]
dt_korr_inz_60_79 <- dt_korr_inz_60_79[, -1]
dt_korr_inz_60_79_melt <- melt(dt_korr_inz_60_79, id.vars = "time", value.name = "inz")
plot_korr_inz_60_79 <- dt_korr_inz_60_79_melt %>%
ggplot(aes(x = time, y = inz, color = variable)) +
  geom_line() +
  labs(x = "", y = "7-Tages-Inzidenz") +
  scale_y_continuous(labels = scales::comma,limits = c(0, 600), breaks = c(0, 100, 200, 300, 400, 500, 600)) +
  scale_color_manual(values = farben2, name = "Art der Inzidenz") +
  theme(axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 13),
        axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 13)) +
  scale_x_date(breaks = as.Date(c("2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%d. %b %Y") + theme_bw()
plot_korr_inz_60_79
ggsave("Plots/korrigierte Inzidenz 60-79.png", plot = plot_korr_inz_60_79, width = 20, height = 10, units = c("cm"))

#Altersgruppe 80+
dt_korr_inz_80 <- data_okt_dez[, c("rep_date_divi", "seven_day_inz_A80", "kor_inz_A80")]
setnames(dt_korr_inz_80, c("seven_day_inz_A80", "kor_inz_A80"), c("gemeldete", "korrigierte"))
dt_korr_inz_80[, time := dt_korr_inz_80$rep_date_divi]
dt_korr_inz_80 <- dt_korr_inz_80[, -1]
dt_korr_inz_80_melt <- melt(dt_korr_inz_80, id.vars = "time", value.name = "inz")
plot_korr_inz_80 <- dt_korr_inz_80_melt %>% 
  ggplot(aes(x = time, y = inz, color = variable)) +
  geom_line() +
  labs(x = "", y = "7-Tages-Inzidenz") +
  scale_y_continuous(labels = scales::comma,limits = c(0, 600), breaks = c(0, 100, 200, 300, 400, 500, 600)) +
  scale_color_manual(values = farben2, name = "Art der Inzidenz") +
  theme(axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 13),
        axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 13)) +
  scale_x_date(breaks = as.Date(c("2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%d. %b %Y") + theme_bw()
plot_korr_inz_80
ggsave("Plots/korrigierte Inzidenz über 80.png", plot = plot_korr_inz_80, width = 20, height = 10, units = c("cm"))

#Overall -> nehmen!
dt_korr_inz <- data_okt_dez[, c("rep_date_divi", "seven_day_inz", "kor_inz_total")]
setnames(dt_korr_inz, c("seven_day_inz", "kor_inz_total"), c("gemeldete", "korrigierte"))
dt_korr_inz[, time := dt_korr_inz$rep_date_divi]
dt_korr_inz <- dt_korr_inz[, -1]
dt_korr_inz_melt <- melt(dt_korr_inz, id.vars = "time", value.name = "inz")
plot_korr_inz <- dt_korr_inz_melt %>% 
  ggplot(aes(x = time, y = inz, color = variable)) +
  geom_line() +
  labs(x = "", y = "7-Tages-Inzidenz") +
  scale_y_continuous(labels = scales::comma,limits = c(0, 600), breaks = c(0, 100, 200, 300, 400, 500, 600)) +
  scale_color_manual(values = farben2, name = "Art der Inzidenz") +
  theme(axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 13),
        axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 13)) +
  scale_x_date(breaks = as.Date(c("2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%d. %b %Y") + theme_bw()
plot_korr_inz
ggsave("Plots/korrigierte Inzidenz.png", plot = plot_korr_inz, width = 20, height = 10, units = c("cm"))


# Plot mit Unterschätzung
#Unterschätzung durch die gemeldete Inzidenz
data_okt_dez$unterschätzung_total <- data_okt_dez$kor_inz_tot/data_okt_dez$seven_day_inz
data_okt_dez$unterschätzung_A15_A34 <- data_okt_dez$kor_inz_A15_A34/data_okt_dez$seven_day_inz_A15_A34
data_okt_dez$unterschätzung_A35_A59 <- data_okt_dez$kor_inz_A35_A59/data_okt_dez$seven_day_inz_A35_A59
data_okt_dez$unterschätzung_A60_A79 <- data_okt_dez$kor_inz_A60_A79/data_okt_dez$seven_day_inz_A60_A79
data_okt_dez$unterschätzung_A80 <- data_okt_dez$kor_inz_A80/data_okt_dez$seven_day_inz_A80
dt_unterschätzung <- data_okt_dez[, c("rep_date_divi", "unterschätzung_total", "unterschätzung_A15_A34", "unterschätzung_A35_A59", "unterschätzung_A60_A79",
                                      "unterschätzung_A80")]
setnames(dt_unterschätzung, c("unterschätzung_total", "unterschätzung_A15_A34", "unterschätzung_A35_A59", "unterschätzung_A60_A79",
                              "unterschätzung_A80"), c("Gesamt", "15-34 Jahre", "35-59 Jahre", "60-79 Jahre", "Über 79 Jahre"))
dt_unterschätzung[, time := dt_unterschätzung$rep_date_divi]
dt_unterschätzung <- dt_unterschätzung[, -1]
dt_unterschätzung_melt <- melt(dt_unterschätzung, id.vars = "time", value.name = "inz")

farben3 <- c("Gesamt" = "#000000", "15-34 Jahre" = "#1F78B4",
             "35-59 Jahre" = "#33A02C", "60-79 Jahre" = "#FB9A99", "Über 79 Jahre" = "#E31A1C")

plot_unterschätzung <- dt_unterschätzung_melt %>% 
  ggplot(aes(x = time, y = inz, color = variable)) +
  geom_line() +
  labs(x = "", y = "Faktor der Unterschätzung") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 3), breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3)) +
  scale_color_manual(values = farben3, name = "Altersgruppe") +
  geom_hline(yintercept = 1, color = "#808080") + 
  theme(axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 13),
        axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 13)) +
  scale_x_date(breaks = as.Date(c("2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%d. %b %Y") + theme_bw()
plot_unterschätzung
ggsave("Plots/Unterschätzung Inzidenz.png", plot = plot_unterschätzung, width = 20, height = 10, units = c("cm"))
