source("nicer_implementation_models.R")

farben3 <- c("Gesamt" = "#000000", "15-34 Jahre" = "#1F78B4",
             "35-59 Jahre" = "#33A02C", "60-79 Jahre" = "#FB9A99", "ueber 80 Jahre" = "#E31A1C")

# Age_group_plot ----------------------------------------------------------

cases_breakpoints <- ggplot(fitted_vals_melt_cases,
                            aes(x = time, y = sdi, color = variable)) +
  geom_line() +
  labs(x = "", y = "7-Tages-Inzidenz") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 400), breaks = c(0, 50, 100, 150, 200, 250, 300, 350, 400)) +
  scale_color_manual(values = farben3, name = "Altersgruppe", labels = c(names(farben3)[1:4], "Über 79 Jahre")) +
  theme(axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 13),
        axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 13)) +
  scale_x_date(breaks = as.Date(c("2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%d. %b %Y") +
  geom_point(data = dt_breakpoints["inz" == origin, .(sdi, time, variable)], shape = 18, size = 2) +
  geom_segment(data = dt_breakpoints["inz" == origin], aes(x = lowerCI, y = sdi, xend = upperCI, yend = sdi))
cases_breakpoints
ggsave("Plots/breakpoints_cases.png", plot = cases_breakpoints, width = 20, height = 10, units = c("cm"))



deaths_breakpoints <- fitted_vals_melt_deaths %>%
  ggplot(aes(x = time, y = sdi, color = variable)) +
  geom_line() +
  labs(x = "", y = "7-Tages-Todesfälle") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 80), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80)) +
  scale_color_manual(values = farben3, name = "Altersgruppe", labels = c(names(farben3)[1:4], "Über 79 Jahre")) +
  theme(axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 13),
        axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 13)) +
  scale_x_date(breaks = as.Date(c("2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%d. %b %Y") +
  geom_point(data = dt_breakpoints["deaths" == origin, .(sdi, time, variable)], shape = 18, size = 2) +
  geom_segment(data = dt_breakpoints["deaths" == origin], aes(x = lowerCI, y = sdi, xend = upperCI, yend = sdi))
deaths_breakpoints
ggsave("Plots/breakpoints_deaths.png", plot = deaths_breakpoints, width = 20, height = 10, units = c("cm"))

# 
# hosp_breakpoints <- dt_hosp_y_fitted %>%
#   ggplot(aes(x = as.Date(time, format = "%d. %b %Y", origin = lubridate::origin),
#              y = geschaetzt), color = "#000000") +
#   geom_line() +
#   labs(x = "Zeit", y = "belegte Intensivbetten") +
#   scale_y_continuous(limits = c(0, 6000)) +
#   theme(axis.text.x = element_text(size = 12), axis.title.x = element_text(size = 15),
#         axis.text.y = element_text(size = 12), axis.title.y = element_text(size = 15)) +
#   scale_x_date(breaks = as.Date(c("2020-10-01", "2020-11-01", "2020-12-01")),
#                date_labels = "%d. %B %Y")
# hosp_breakpoints
# ggsave("Plots/breakpoints_hosp.png", plot = hosp_breakpoints, width = 20, height = 10, units = c("cm"))

# Grid plot ---------------------------------------------------------------

# At first each plot must be made singly
cases_for_grid <- fitted_vals_melt_cases %>%
  ggplot(aes(x = time, y = sdi, color = variable)) +
  geom_line() +
  scale_y_continuous(labels = scales::comma, limits = c(0, 400)) +
  scale_color_manual(values = farben3, name = "Altersgruppe", labels = c(names(farben3)[1:4], "Über 79 Jahre")) +
  # deleting 
  theme(axis.title = element_blank(), axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  labs(y = "7-Tages-Inzidenz je 100.000 Einw.") +
  geom_point(data = dt_breakpoints["inz" == origin, .(sdi, time, variable)], shape = 18, size = 2)

deaths_for_grid <- fitted_vals_melt_deaths %>%
  ggplot(aes(x = time, y = sdi, color = variable)) +
  geom_line() +
  scale_y_continuous(labels = scales::comma, limits = c(0, 80)) +
  scale_color_manual(values = farben3, name = "Altersgruppe", labels = c(names(farben3)[1:4], "Über 79 Jahre")) +
  theme(axis.title = element_blank(), legend.position = "none", 
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  labs(y = "7-Tages-Todesfälle je 100.000 Einw.") +
  geom_point(data = dt_breakpoints["deaths" == origin, .(sdi, time, variable)], shape = 18, size = 2)

# for hosp
hosp_for_grids <- dt_hosp_y_fitted %>% 
  ggplot(aes(time, y = geschaetzt), color = "#000000") +
  geom_line() +
  scale_y_continuous(limits = c(0, 8000)) +
  scale_x_date(breaks = as.Date(c("2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%d. %b %Y") +
  theme(axis.title.y = element_blank()) +
  labs(y = "7-Tages-Hospitalisierungsfälle", x = "") +
  geom_point(data = dt_breakpoints["beds" == origin, .(geschaetzt = sdi, time, variable)], shape = 18, size = 2)
  

# legend for the plot
legend <- get_legend(
  # create some space to the left of the legend
  cases_for_grid + theme(legend.box.margin = margin(0, 0, 0, 12))
)

# # creates y Axis for the hole plot
# y_axis <- textGrob("Inzidenz", 
#                      gp = gpar(fontface="bold", fontsize = 15), rot = 90)

p_grid <- plot_grid(cases_for_grid +
                      # deleting legend manuall to add it later, for the grid plot
                      theme(legend.position = "none"),
                    deaths_for_grid,
                    hosp_for_grids,
                    # labels need to be adjusted
                    labels = c('Inzidenz', "Todesfälle", "Intensivbettenbelegung"),
                    # one col for the three plots, to adjust them among each other
                    ncol = 1,
                    # adjusting the positions of the labels
                    label_x = .08, label_y = 1, hjust = 0, scale = 1, 
                    align = "hv")

# Arranges all together
grid_plot <- grid.arrange(arrangeGrob(p_grid, right = legend))

ggsave("Plots/grid_plot_models.png", plot = grid_plot, width = 20, height = 10, units = c("cm"))


# model_plus_timeseries ---------------------------------------------------
farben4 <- c("geschätzt" = "#000000", "gemeldet" = "purple")

cases_timeseries <- dt_cases_y_fitted_melt %>% 
  ggplot(aes(x = time, y = values, color = variable)) +
  geom_line() +
  labs(x = "", y = "7-Tages-Inzidenz") +
  scale_color_manual(values = farben4, name = "") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 250)) +
  theme(axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 13),
        axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 13)) +
  scale_x_date(breaks = as.Date(c("2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%d. %b %Y") +
  geom_point(data = dt_breakpoints["inz" == origin & variable == "Gesamt", .(values = sdi, time)],
             color = "black", shape = 18, size = 2) +
  geom_segment(data = dt_breakpoints["inz" == origin & variable == "Gesamt"],
               aes(x = lowerCI, y = sdi, xend = upperCI, yend = sdi))
cases_timeseries
ggsave("Plots/timeseries_model_cases.png", plot = cases_timeseries, width = 20, height = 10, units = c("cm"))

deaths_timeseries <- dt_deaths_y_fitted_melt %>% 
  ggplot(aes(x = time, y = values, color = variable)) +
  geom_line() +
  labs(x = "", y = "7-Tages-Todesfälle") +
  scale_color_manual(values = farben4, name = "") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 10), breaks = c(0, 2, 4, 6, 8, 10)) +
  theme(axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 13),
        axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 13)) +
  scale_x_date(breaks = as.Date(c("2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%d. %b %Y") +
  geom_point(data = dt_breakpoints["deaths" == origin & variable == "Gesamt", .(values = sdi, time)],
             color = "black", shape = 18, size = 2) +
  geom_segment(data = dt_breakpoints["deaths" == origin & variable == "Gesamt"],
               aes(x = lowerCI, y = sdi, xend = upperCI, yend = sdi))
deaths_timeseries
ggsave("Plots/timeseries_model_deaths.png", plot = deaths_timeseries, width = 20, height = 10, units = c("cm"))

hosp_timeseries <- dt_hosp_y_fitted_melt %>% 
  ggplot(aes(x = time, y = values, color = variable)) +
  geom_line() +
  labs(x = "", y = "belegte Intensivbetten") +
  scale_color_manual(values = farben4, name = "") +
  scale_y_continuous(limits = c(0, 6000), breaks = c(0, 1000, 2000, 3000, 4000, 5000, 6000, 7000)) +
  theme(axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 13),
        axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 13)) +
  scale_x_date(breaks = as.Date(c("2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%d. %b %Y") +
  geom_point(data = dt_breakpoints["beds" == origin & variable == "Gesamt", .(values = sdi, time)],
             color = "black", shape = 18, size = 2) +
  geom_segment(data = dt_breakpoints["beds" == origin & variable == "Gesamt"],
               aes(x = lowerCI, y = sdi, xend = upperCI, yend = sdi))
hosp_timeseries
ggsave("Plots/timeseries_model_hosp.png", plot = hosp_timeseries, width = 20, height = 10, units = c("cm"))