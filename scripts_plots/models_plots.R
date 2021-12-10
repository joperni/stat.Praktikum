source("nicer_implementation_models.R")
library(egg)
library(ggpubr)

farben3 <- c("Gesamt" = "#000000", "15-34 Jahre" = "#1F78B4",
             "35-59 Jahre" = "#33A02C", "60-79 Jahre" = "#FB9A99", "ueber 80 Jahre" = "#E31A1C")

# Age_group_plot ----------------------------------------------------------

cases_breakpoints <- ggplot(NULL, aes(x = time, y = sdi, color = variable)) +
  geom_line(data = fitted_vals_melt_cases) +
  # geom_point(aes(x = as.Date(dt_breakpoints$x_bp[c(1:5, 11:22)], origin = "1970-01-01"),
  #                            y = dt_breakpoints$y_bp[c(1:5, 11:22)])) +
  labs(x = "Zeit", y = "7-Tages-Inzidenz") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 400), breaks = c(0, 50, 100, 150, 200, 250, 300, 350, 400)) +
  scale_color_manual(values = farben3, name = "Altersgruppe", labels = c(names(farben3)[1:4], "Über 79 Jahre")) +
  theme(axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 13),
        axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 13)) +
  scale_x_date(breaks = as.Date(c("2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%d. %b %Y") +
  geom_point(data = dt_breakpoints[c(1:5, 7:22)])
cases_breakpoints
ggsave("Plots/breakpoints_cases.png", plot = cases_breakpoints, width = 20, height = 10, units = c("cm"))



deaths_breakpoints <- fitted_vals_melt_deaths %>%
  ggplot(aes(x = time, y = sdi, color = variable)) +
  geom_line() +
  labs(x = "Zeit", y = "7-Tages-Todesfälle pro 100.000 Einw.") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 80), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80)) +
  scale_color_manual(values = farben3, name = "Altersgruppe", labels = c(names(farben3)[1:4], "Über 79 Jahre")) +
  theme(axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 13),
        axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 13)) +
  scale_x_date(breaks = as.Date(c("2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%d. %b %Y") +
  geom_point(data = dt_breakpoints[c(22:23, 30:38)])
deaths_breakpoints
ggsave("Plots/breakpoints_deaths.png", plot = deaths_breakpoints, width = 20, height = 10, units = c("cm"))


hosp_breakpoints <- dt_hosp_y_fitted %>%
  ggplot(aes(x = time, y = geschaetztes), color = "#000000") +
  geom_line() +
  labs(x = "Zeit", y = "belegte Intensivbetten") +
  scale_y_continuous(limits = c(0, 6000)) +
  theme(axis.text.x = element_text(size = 12), axis.title.x = element_text(size = 15),
        axis.text.y = element_text(size = 12), axis.title.y = element_text(size = 15)) +
  scale_x_date(breaks = as.Date(c("2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%d. %B %Y")
hosp_breakpoints
ggsave("Plots/breakpoints_hosp.png", plot = hosp_breakpoints, width = 20, height = 10, units = c("cm"))

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
  labs(y = "7-Tages-Inzidenz je 100.000 Einw.")

deaths_for_grid <- fitted_vals_melt_deaths %>%
  ggplot(aes(x = time, y = sdi, color = variable)) +
  geom_line() +
  scale_y_continuous(labels = scales::comma, limits = c(0, 80)) +
  scale_color_manual(values = farben3, name = "Altersgruppe", labels = c(names(farben3)[1:4], "Über 79 Jahre")) +
  theme(axis.title = element_blank(), legend.position = "none", 
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  labs(y = "7-Tages-Todesfälle je 100.000 Einw.")

# for hosp
hosp_for_grids <- dt_hosp_y_fitted %>% 
  ggplot(aes(x = time, y = geschaetztes), color = "#000000") +
  geom_line() +
  labs(x = "Zeit") +
  scale_y_continuous(limits = c(0, 8000)) +
  scale_x_date(breaks = as.Date(c("2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%d. %b %Y") +
  theme(axis.title.y = element_blank()) +
  labs(y = "7-Tages-Hospitalisierungsfälle") 

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
                    labels = c('Inzidenz', "Todesfälle", "Intesivbettenbelegung"),
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
  labs(x = "Zeit", y = "7-Tages-Inzidenz") +
  scale_color_manual(values = farben4, name = "") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 250)) +
  theme(axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 13),
        axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 13)) +
  scale_x_date(breaks = as.Date(c("2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%d. %b %Y") +
  geom_vline(xintercept = dt_breakpoints[1:5, time]
             , color = "black", linetype = "dotted", size = 1.0)
cases_timeseries
ggsave("Plots/timeseries_model_cases.png", plot = cases_timeseries, width = 20, height = 10, units = c("cm"))

deaths_timeseries <- dt_deaths_y_fitted_melt %>% 
  ggplot(aes(x = time, y = values, color = variable)) +
  geom_line() +
  labs(x = "Zeit", y = "7-Tages-Todesfälle pro 100.000 Einw.") +
  scale_color_manual(values = farben4, name = "Modell") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 10)) +
  theme(axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 13),
        axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 13)) +
  scale_x_date(breaks = as.Date(c("2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%d. %b %Y") +
  geom_vline(xintercept = dt_breakpoints[23:24, time],
             color = "black", linetype = "dotted", size = 1.0)
deaths_timeseries
ggsave("Plots/timeseries_model_deaths.png", plot = deaths_timeseries, width = 20, height = 10, units = c("cm"))

hosp_timeseries <- dt_hosp_y_fitted_melt %>% 
  ggplot(aes(x = time, y = values, color = variable)) +
  geom_line() +
  labs(x = "Zeit", y = "belegte Intensivbetten pro 100.000 Einw.") +
  scale_color_manual(values = farben4, name = "Modell") +
  scale_y_continuous(labels = scales::comma,limits = c(0, 6000), breaks = c(0, 1000, 2000, 3000, 4000, 5000, 6000, 7000)) +
  theme(axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 13),
        axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 13)) +
  scale_x_date(breaks = as.Date(c("2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%d. %b %Y") +
  geom_vline(xintercept = dt_breakpoints[39:42, time],
             color = "black", linetype = "dotted", size = 1.0)
hosp_timeseries
ggsave("Plots/timeseries_model_hosp.png", plot = hosp_timeseries, width = 20, height = 10, units = c("cm"))
