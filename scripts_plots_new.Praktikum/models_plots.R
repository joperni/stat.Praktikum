source("nicer_implementation_models.R")

farben3 <- c("Gesamt" = "#000000", "15-34 Jahre" = "#1F78B4",
             "35-59 Jahre" = "#33A02C", "60-79 Jahre" = "#FB9A99", "Über 79 Jahre" = "#E31A1C")

# Age_group_plot ----------------------------------------------------------
dt_breakpoints$variable[dt_breakpoints$variable == "ueber 80 Jahre"] = c("Über 79 Jahre")
levels(fitted_vals_melt_cases$variable)[1] = c("Gesamt")
levels(fitted_vals_melt_cases$variable)[5] = c("Über 79 Jahre")
cases_breakpoints <- ggplot(fitted_vals_melt_cases,
                            aes(x = time, y = sdi, color = variable)) +
  geom_line() +
  labs(x = "", y = "7-Tages-Inzidenz") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 400), breaks = c(0, 50, 100, 150, 200, 250, 300, 350, 400)) +
  scale_color_manual(values = farben3, name = "Altersgruppe", labels = names(farben3)) +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black", size=1),
        panel.grid = element_line(colour = "gray57", size = 0.2),
        axis.title.y = element_text(margin = margin(t = 0, r = 18, b = 0, l = 0)),
        axis.text   = element_text(colour = "black")) +
  theme(axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 13),
        axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 13)) +
  scale_x_date(breaks = as.Date(c("2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%d. %b %Y") +
  geom_point(data = dt_breakpoints["inz" == origin, .(sdi, time, variable)], shape = 18, size = 3) +
  geom_segment(data = dt_breakpoints["inz" == origin], aes(x = lowerCI, y = sdi, xend = upperCI, yend = sdi))
#cases_breakpoints
#ggsave("Plots/breakpoints_cases.png", plot = cases_breakpoints, width = 20, height = 10, units = c("cm"))


levels(fitted_vals_melt_deaths$variable)[1] = c("Gesamt")
levels(fitted_vals_melt_deaths$variable)[5] = c("Über 79 Jahre")
deaths_breakpoints <- fitted_vals_melt_deaths %>%
  ggplot(aes(x = time, y = sdi, color = variable)) +
  geom_line() +
  labs(x = "", y = "7-Tages-Todesfälle") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 80), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80)) +
  scale_color_manual(values = farben3, name = "Altersgruppe", labels = names(farben3)) +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black", size=1),
        panel.grid = element_line(colour = "gray57", size = 0.2),
        axis.title.y = element_text(margin = margin(t = 0, r = 23, b = 0, l = 0)),
        axis.text   = element_text(colour = "black")) +
  theme(axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 13),
        axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 13)) +
  scale_x_date(breaks = as.Date(c("2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%d. %b %Y") +
  geom_point(data = dt_breakpoints["deaths" == origin, .(sdi, time, variable)], shape = 18, size = 3) +
  geom_segment(data = dt_breakpoints["deaths" == origin], aes(x = lowerCI, y = sdi, xend = upperCI, yend = sdi))
#deaths_breakpoints
#ggsave("Plots/breakpoints_deaths.png", plot = deaths_breakpoints, width = 20, height = 10, units = c("cm"))

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
  theme_bw() +
  # deleting 
  theme(axis.title = element_blank(), axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  #theme(plot.margin = unit(c(-0.5, 1.5, 3, 0), "cm")) +
  annotate("text", label = "Inzidenz", x = as.Date(c("2020-10-01")), y = 355, size = 4.5, colour = "black", hjust = 0) +
  labs(y = "7-Tages-Inzidenz je 100.000 Einw.") +
  geom_point(data = dt_breakpoints["inz" == origin, .(sdi, time, variable)], shape = 18, size = 2.2) +
  theme(panel.border = element_rect(colour = "black", size=1),
        panel.grid = element_line(colour = "gray57", size = 0.2),
        axis.text   = element_text(colour = "black"))#+
  #geom_segment(data = dt_breakpoints["inz" == origin], aes(x = lowerCI, y = sdi, xend = upperCI, yend = sdi))

deaths_for_grid <- fitted_vals_melt_deaths %>%
  ggplot(aes(x = time, y = sdi, color = variable)) +
  geom_line() +
  scale_y_continuous(labels = scales::comma, limits = c(0, 80)) +
  scale_color_manual(values = farben3, name = "Altersgruppe", labels = c(names(farben3)[1:4], "Über 79 Jahre")) +
  theme_bw() +
  theme(axis.title = element_blank(), legend.position = "none", 
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  annotate("text", label = "Todesfälle", x = as.Date(c("2020-10-01")), y = 71, size = 4.5, colour = "black", hjust = 0) +
  #theme(plot.margin = unit(c(-0.5, 1.5, 1, 0), "cm")) +
  labs(y = "7-Tages-Todesfälle je 100.000 Einw.") +
  geom_point(data = dt_breakpoints["deaths" == origin, .(sdi, time, variable)], shape = 18, size = 2.2) +
  theme(panel.border = element_rect(colour = "black", size=1),
        panel.grid = element_line(colour = "gray57", size = 0.2),
        axis.text   = element_text(colour = "black"))#+
  #geom_segment(data = dt_breakpoints["deaths" == origin], aes(x = lowerCI, y = sdi, xend = upperCI, yend = sdi))

# for hosp
hosp_for_grids <- dt_hosp_y_fitted %>% 
  ggplot(aes(time, y = geschaetzt), color = "#000000") +
  geom_line() +
  scale_y_continuous(limits = c(0, 8000)) +
  scale_x_date(breaks = as.Date(c("2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%d. %b %Y") +
  theme_bw() +
  theme(axis.title.y = element_blank()) +
  annotate("text", label = "Intensivbettenbelegung", x = as.Date(c("2020-10-01")), y = 6970, size = 4.5, colour = "black", hjust = 0) +
  #theme(plot.margin = unit(c(-0.5, 1.5, 1, 0), "cm")) +
  labs(y = "7-Tages-Hospitalisierungsfälle", x = "") +
  geom_point(data = dt_breakpoints["beds" == origin, .(geschaetzt = sdi, time, variable)], shape = 18, size = 2.2) +
  theme(panel.border = element_rect(colour = "black", size=1),
        panel.grid = element_line(colour = "gray57", size = 0.2),
        axis.text   = element_text(colour = "black"))#+
  #geom_segment(data = dt_breakpoints["beds" == origin], aes(x = lowerCI, y = sdi, xend = upperCI, yend = sdi))
  

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
                    NULL,
                    deaths_for_grid,
                    NULL,
                    hosp_for_grids,
                    # labels need to be adjusted
                    #labels = c('Inzidenz', "Todesfälle", "Intensivbettenbelegung"),
                    # one col for the three plots, to adjust them among each other
                    ncol = 1,
                    # adjusting the positions of the labels
                    label_x = .08, label_y = .97, hjust = 0, scale = 1, 
                    align = "hv",
                    rel_heights = c(1, -0.22, 1, -0.22, 1))

# Arranges all together

grid_plot <- grid.arrange(arrangeGrob(p_grid, right = legend))
ggsave("Plots/grid_plot_models.png", plot = grid_plot, width = 21, height = 10, units = c("cm"))


# model_plus_timeseries ---------------------------------------------------
farben4 <- c("geschätzt" = "darkorange", "gemeldet" = "#000000")

levels(dt_cases_y_fitted_melt$variable)[1] = c("geschätzt")
cases_timeseries <- dt_cases_y_fitted_melt %>% 
  ggplot(aes(x = time, y = values, color = variable)) +
  geom_line() +
  labs(x = "", y = "7-Tages-Inzidenz") +
  scale_color_manual(values = farben4, name = "", labels = names(farben4)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 250)) +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black", size=1),
        panel.grid = element_line(colour = "gray57", size = 0.2),
        axis.title.y = element_text(margin = margin(t = 0, r = 19, b = 0, l = 0)),
        axis.text   = element_text(colour = "black")) +
  theme(axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 13),
        axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 13)) +
  scale_x_date(breaks = as.Date(c("2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%d. %b %Y") +
  geom_point(data = dt_breakpoints["inz" == origin & variable == "Gesamt"],
             aes(x = time, y = sdi), shape = 18, size = 3, colour = farben4[[1]]) +
  geom_segment(data = dt_breakpoints["inz" == origin & variable == "Gesamt"],
               aes(x = lowerCI, y = sdi, xend = upperCI, yend = sdi), colour = farben4[[1]])
#cases_timeseries
#ggsave("Plots/timeseries_model_cases.png", plot = cases_timeseries, width = 20, height = 10, units = c("cm"))

levels(dt_deaths_y_fitted_melt$variable)[1] = c("geschätzt")
deaths_timeseries <- dt_deaths_y_fitted_melt %>% 
  ggplot(aes(x = time, y = values, color = variable)) +
  geom_line() +
  labs(x = "", y = "7-Tages-Todesfälle") +
  scale_color_manual(values = farben4, name = "", labels = names(farben4)) +
  scale_y_continuous(labels = scales::label_number(accuracy = 1), limits = c(0, 10), breaks = c(0, 2, 4, 6, 8, 10)) +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black", size=1),
        panel.grid = element_line(colour = "gray57", size = 0.2),
        axis.title.y = element_text(margin = margin(t = 0, r = 25, b = 0, l = 0)),
        axis.text   = element_text(colour = "black")) +
  theme(axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 13),
        axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 13)) +
  scale_x_date(breaks = as.Date(c("2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%d. %b %Y") +
  geom_point(data = dt_breakpoints["deaths" == origin & variable == "Gesamt"],
             aes(x = time, y = sdi), shape = 18, size = 3, colour = farben4[[1]]) +
  geom_segment(data = dt_breakpoints["deaths" == origin & variable == "Gesamt"],
               aes(x = lowerCI, y = sdi, xend = upperCI, yend = sdi), colour = farben4[[1]])
#deaths_timeseries
#ggsave("Plots/timeseries_model_deaths.png", plot = deaths_timeseries, width = 20, height = 10, units = c("cm"))

levels(dt_hosp_y_fitted_melt$variable)[1] = c("geschätzt")
hosp_timeseries <- dt_hosp_y_fitted_melt %>% 
  ggplot(aes(x = time, y = values, color = variable)) +
  geom_line() +
  labs(x = "", y = "Belegte Intensivbetten") +
  scale_color_manual(values = farben4, name = "", labels = names(farben4)) +
  scale_y_continuous(limits = c(0, 6000), breaks = c(0, 1000, 2000, 3000, 4000, 5000, 6000, 7000)) +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black", size=1),
        panel.grid = element_line(colour = "gray57", size = 0.2),
        axis.title.y = element_text(margin = margin(t = 0, r = 13, b = 0, l = 0)),
        axis.text   = element_text(colour = "black")) +
  theme(axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 13),
        axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 13)) +
  scale_x_date(breaks = as.Date(c("2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%d. %b %Y") +
  geom_point(data = dt_breakpoints["beds" == origin & variable == "Gesamt"],
             aes(x = time, y = sdi), shape = 18, size = 3, colour = farben4[[1]]) +
  geom_segment(data = dt_breakpoints["beds" == origin & variable == "Gesamt"],
               aes(x = lowerCI, y = sdi, xend = upperCI, yend = sdi), colour = farben4[[1]])
#hosp_timeseries
#ggsave("Plots/timeseries_model_hosp.png", plot = hosp_timeseries, width = 20, height = 10, units = c("cm"))

### Gesamtgrid

aligned <- align_plots(cases_timeseries,
                       hosp_timeseries,
                       deaths_timeseries,
                       cases_breakpoints,
                       deaths_breakpoints,
                       align = "hv",
                       axis = "tblr")
ggsave("Plots/timeseries_model_cases.png", plot = ggdraw(aligned[[1]]), width = 21, height = 10, units = c("cm"))
ggsave("Plots/timeseries_model_hosp.png", plot = ggdraw(aligned[[2]]), width = 21, height = 10, units = c("cm"))
ggsave("Plots/timeseries_model_deaths.png", plot = ggdraw(aligned[[3]]), width = 21, height = 10, units = c("cm"))
ggsave("Plots/breakpoints_cases.png", plot = ggdraw(aligned[[4]]), width = 21, height = 10, units = c("cm"))
ggsave("Plots/breakpoints_deaths.png", plot = ggdraw(aligned[[5]]), width = 21, height = 10, units = c("cm"))
