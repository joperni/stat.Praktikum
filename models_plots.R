source("nicer_implementation_models.R")
library(segmented)
library(data.table)
library(ggplot2)
library(checkmate)
library(gridExtra)
library(RColorBrewer)
library(grid)
library(dplyr)
library(cowplot)

farben3 <- c("Gesamt" = "#000000", "15-34 Jahre" = "#1F78B4",
             "35-59 Jahre" = "#33A02C", "60-79 Jahre" = "#FB9A99", "über 80 Jahre" = "#E31A1C")

# Age_group_plot ----------------------------------------------------------

# At first: Making data table with breakpoints, for all used gamma models
dt_breakpoints <- 
  dt_models[!grepl("log", formula), .(x_breakpoints = model_bic_seq[[1]]$psi[, 2], model = model_bic_seq),
            by = formula]



# plot for cases

# Transforming data table for easier plotting 
#
# add fitted values to a data_frame
dt_cases_fitted_vals <- 
  dt_models[c(1, 3:6), lapply(model_bic_seq, function(model) model$fitted.values)]
# colnames
colnames(dt_cases_fitted_vals) <- c(sdi_cases_colnames)[c(1, 3:6)]
# add time column
dt_cases_fitted_vals[, time := data$rep_date_divi]
setnames(dt_cases_fitted_vals, c("seven_day_inz", "seven_day_inz_A15_A34", "seven_day_inz_A35_A59", "seven_day_inz_A60_A79",
                                 "seven_day_inz_A80"), c("overall", "15-34 Jahre", "35-59 Jahre", "60-79 Jahre", "über 80 Jahre"))
# melting for easier plotting
fitted_vals_melt_cases <- melt(dt_cases_fitted_vals, id.vars = "time", value.name = "sdi")

cases_breakpoints <- fitted_vals_melt_cases %>%
  ggplot(aes(x = time, y = sdi, color = variable)) +
  geom_line() +
  labs(x = "Zeit", y = "7-Tages Neuinfektionen pro 100.000 Einw.") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 400)) +
  scale_color_manual(values = farben3, name = "Altersgruppe") +
  theme(axis.text.x = element_text(size = 12), axis.title.x = element_text(size = 15),
        axis.text.y = element_text(size = 12), axis.title.y = element_text(size = 15)) +
  scale_x_date(breaks = as.Date(c("2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%d. %B %Y")
cases_breakpoints
ggsave("Plots/breakpoints_cases.png", plot = cases_breakpoints, width = 20, height = 10, units = c("cm"))

# for deaths now
dt_deaths_fitted_vals <- dt_models[c(13, 15:18), lapply(model_bic_seq, function(model) model$fitted.values)]
colnames(dt_deaths_fitted_vals) <- c(sdi_deaths_colnames)[c(1, 3:6)]
dt_deaths_fitted_vals[, time := data$rep_date_divi]
setnames(dt_deaths_fitted_vals, c("seven_day_death_inz", "seven_day_death_inz_A15_A34", "seven_day_death_inz_A35_A59", "seven_day_death_inz_A60_A79",
                                  "seven_day_death_inz_A80"), c("overall", "15-34 Jahre", "35-59 Jahre", "60-79 Jahre", "über 80 Jahre"))
fitted_vals_melt_deaths <- melt(dt_deaths_fitted_vals, id.vars = "time", value.name = "sdi")

deaths_breakpoints <- fitted_vals_melt_deaths %>%
  ggplot(aes(x = time, y = sdi, color = variable)) +
  geom_line() +
  labs(x = "Zeit", y = "7-Tages-Todesfälle je 100.000 Einw.") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 80)) +
  scale_color_manual(values = farben3, name = "Altersgruppe") +
  theme(axis.text.x = element_text(size = 12), axis.title.x = element_text(size = 15),
        axis.text.y = element_text(size = 12), axis.title.y = element_text(size = 15)) +
  scale_x_date(breaks = as.Date(c("2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%d. %B %Y")
deaths_breakpoints
ggsave("Plots/breakpoints_deaths.png", plot = deaths_breakpoints, width = 20, height = 10, units = c("cm"))

# for hospitalisierung
dt_hosp_y_fitted <- data.table(time = data$rep_date_divi, fitted = dt_models[25, model_bic_seq[[1]]$fitted.values],
                               timeseries = dt_models[25, model_bic_seq[[1]]$y])
hosp_breakpoints <- dt_hosp_y_fitted %>% 
  ggplot(aes(x = time, y = fitted), color = "#000000") +
  geom_line() +
  labs(x = "Zeit", y = "belegte Intensivbetten") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 6000)) +
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
  scale_color_manual(values = farben3, name = "Altersgruppe") +
  # deleting 
  theme(axis.title = element_blank(), axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  labs(y = "7-Tages-Inzidenz je 100.000 Einw.")

deaths_for_grid <- fitted_vals_melt_deaths %>%
  ggplot(aes(x = time, y = sdi, color = variable)) +
  geom_line() +
  scale_y_continuous(labels = scales::comma, limits = c(0, 80)) +
  scale_color_manual(values = farben3, name = "Altersgruppe") +
  theme(axis.title = element_blank(), legend.position = "none", 
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  labs(y = "7-Tages-Todesfälle je 100.000 Einw.")

# for hosp
hosp_for_grids <- dt_hosp_y_fitted %>% 
  ggplot(aes(x = time, y = fitted), color = "#000000") +
  geom_line() +
  labs(x = "Zeit") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 6000)) +
  scale_x_date(breaks = as.Date(c("2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%d. %B %Y") +
  theme(axis.title.y = element_blank()) +
  labs(y = "7-Tages-Hospitalisierungsfälle")

# legend for the plot
legend <- get_legend(
  # create some space to the left of the legend
  cases_for_grid + theme(legend.box.margin = margin(0, 0, 0, 12))
)

# creates y Axis for the hole plot
y_axis <- textGrob("Inzidenz", 
                     gp = gpar(fontface="bold", fontsize = 15), rot = 90)

p_grid <- plot_grid(cases_for_grid +
                      # deleting legend manuall to add it later, for the grid plot
                      theme(legend.position = "none"),
                    deaths_for_grid,
                    hosp_for_grids,
                    # labels need to be adjusted
                    labels = c('Inzidenz', "Todeszahlen", 'Hospitalisierung'),
                    # one col for the three plots, to adjust them among each other
                    ncol = 1,
                    # adjusting the positions of the labels
                    label_x = .07, label_y = 1, hjust = 0,
                    align = "v")

# Arranges all together
grid_plot <- grid.arrange(arrangeGrob(p_grid, left = y_axis, right = legend))

ggsave("Plots/grid_plot_models.png", plot = grid_plot, width = 20, height = 10, units = c("cm"))


# model_plus_timeseries ---------------------------------------------------
farben4 <- c("fitted" = "#000000", "timeseries" = "purple")
# for cases
dt_cases_y_fitted <- data.table(time = data$rep_date_divi, fitted = dt_models[1, model_bic_seq[[1]]$fitted.values],
                                  timeseries = dt_models[1, model_bic_seq[[1]]$y])
dt_cases_y_fitted_melt <- melt(dt_cases_y_fitted, id.vars = "time", value.name = "values")

cases_timeseries <- dt_cases_y_fitted_melt %>% 
  ggplot(aes(x = time, y = values, color = variable)) +
  geom_line() +
  labs(x = "Zeit", y = "7-Tages Neuinfektionen pro 100.000 Einw.") +
  scale_color_manual(values = farben4, name = "Modell") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 250)) +
  theme(axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 13),
        axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 13)) +
  scale_x_date(breaks = as.Date(c("2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%d. %B %Y") +
  geom_vline(xintercept = dt_breakpoints[formula == "seven_day_inz  ~ rep_date_divi", V1]
             , color = "black", linetype = "dotted", size = 1.0)
cases_timeseries
ggsave("Plots/timeseries_model_cases.png", plot = cases_timeseries, width = 20, height = 10, units = c("cm"))

# for deaths
dt_deaths_y_fitted <- data.table(time = data$rep_date_divi, fitted = dt_models[13, model_bic_seq[[1]]$fitted.values],
                                  timeseries = dt_models[13, model_bic_seq[[1]]$y])
dt_deaths_y_fitted_melt <- melt(dt_deaths_y_fitted, id.vars = "time", value.name = "values")

deaths_timeseries <- dt_deaths_y_fitted_melt %>% 
  ggplot(aes(x = time, y = values, color = variable)) +
  geom_line() +
  labs(x = "Zeit", y = "7-Tages-Todesfälle pro 100.000 Einw.") +
  scale_color_manual(values = farben4, name = "Modell") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 10)) +
  theme(axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 13),
        axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 13)) +
  scale_x_date(breaks = as.Date(c("2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%d. %B %Y") +
  geom_vline(xintercept = dt_breakpoints[formula == "seven_day_death_inz  + 0.0001 ~ rep_date_divi", V1],
             color = "black", linetype = "dotted", size = 1.0)
deaths_timeseries
ggsave("Plots/timeseries_model_deaths.png", plot = deaths_timeseries, width = 20, height = 10, units = c("cm"))

dt_hosp_y_fitted_melt <- melt(dt_hosp_y_fitted, id.vars = "time", value.name = "values")
hosp_timeseries <- dt_hosp_y_fitted_melt %>% 
  ggplot(aes(x = time, y = values, color = variable)) +
  geom_line() +
  labs(x = "Zeit", y = "belegte Intensivbetten") +
  scale_color_manual(values = farben4, name = "Modell") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 50)) +
  theme(axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 13),
        axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 13)) +
  scale_x_date(breaks = as.Date(c("2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%d. %B %Y") +
  geom_vline(xintercept = dt_breakpoints[formula == "seven_day_hosp_inz ~ rep_date_divi", V1]
             , color = "black", linetype = "dotted", size = 1.0)
hosp_timeseries
ggsave("Plots/timeseries_model_hosp.png", plot = hosp_timeseries, width = 20, height = 10, units = c("cm"))
