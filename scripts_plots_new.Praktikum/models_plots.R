source("implementation_models.R")
source("implementation_ratio_models.R")
# error messages are no problem. They get produced and catched by the segmented::selgmented function
source("help_functions/model_diagnose.R")

farben3 <- c("Gesamt" = "#000000", "15-34 Jahre" = "#1F78B4",
             "35-59 Jahre" = "#33A02C", "60-79 Jahre" = "#FB9A99", "Über 79 Jahre" = "#E31A1C")
load("dt_breakpoints260222.Rda")
load("dt_exp_betas260222.Rda")

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
  theme(panel.border = element_rect(colour = "black", size = 1),
        panel.grid = element_line(colour = "gray57", size = 0.2),
        axis.title.y = element_text(margin = margin(t = 0, r = 18, b = 0, l = 0)),
        axis.text   = element_text(colour = "black")) +
  theme(axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 13),
        axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 13)) +
  scale_x_date(breaks = as.Date(c("2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%d.%m.%y") +
  geom_point(data = dt_breakpoints["inz" == origin, .(sdi, time, variable)], shape = 18, size = 3) +
  geom_segment(data = dt_breakpoints["inz" == origin], aes(x = lowerCI, y = sdi, xend = upperCI, yend = sdi))
# cases_breakpoints
# ggsave("Plots/breakpoints_cases.png", plot = cases_breakpoints, width = 20, height = 10, units = c("cm"))


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
               date_labels = "%d.%m.%y") +
  geom_point(data = dt_breakpoints["deaths" == origin, .(sdi, time, variable)], shape = 18, size = 3) +
  geom_segment(data = dt_breakpoints["deaths" == origin], aes(x = lowerCI, y = sdi, xend = upperCI, yend = sdi))
#deaths_breakpoints
#ggsave("Plots/breakpoints_deaths.png", plot = deaths_breakpoints, width = 20, height = 10, units = c("cm"))

# 
log_deaths_breakpoints <- fitted_vals_melt_deaths %>%
  ggplot(aes(x = time, y = log(sdi), color = variable)) +
  geom_line() +
  labs(x = "", y = "log(7-Tages-Todesfälle)") +
  scale_y_continuous(limits = c(-6, 6), breaks = c(-6, -3, 0, 3, 6)) +
  scale_color_manual(values = farben3, name = "Altersgruppe", labels = names(farben3)) +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black", size=1),
        panel.grid = element_line(colour = "gray57", size = 0.2),
        axis.title.y = element_text(margin = margin(t = 0, r = 23, b = 0, l = 0)),
        axis.text   = element_text(colour = "black")) +
  theme(axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 13),
        axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 13)) +
  scale_x_date(breaks = as.Date(c("2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%d.%m.%y") +
  geom_point(data = dt_breakpoints["deaths" == origin, .(sdi, time, variable)], shape = 18, size = 3) +
  geom_segment(data = dt_breakpoints["deaths" == origin], aes(x = lowerCI, y = log(sdi), xend = upperCI, yend = log(sdi)))

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

dt_bp_ratio$variable[dt_bp_ratio$variable == "ueber 80 Jahre"] = c("Über 79 Jahre")
levels(fitted_vals_melt_ratio$variable)[1] = c("Gesamt")
levels(fitted_vals_melt_ratio$variable)[5] = c("Über 79 Jahre")
ratio_for_grid <- ggplot(fitted_vals_melt_ratio,
                         aes(x = time, y = sdi, color = variable)) +
  geom_line() +
  scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ","),
                     limits = c(0, 0.24), breaks = seq(0, 0.24, 0.06)) +
  scale_color_manual(values = farben3, name = "Altersgruppe", labels = names(farben3)) +
  theme_bw() +
  annotate("text", label = "Todesfälle/Inzidenz", x = as.Date(c("2020-10-01")),
           y = 0.21, size = 4.5, colour = "black", hjust = 0) +
  theme(axis.title = element_blank(), legend.position = "none", 
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  theme(panel.border = element_rect(colour = "black", size=1),
        panel.grid = element_line(colour = "gray57", size = 0.2),
        axis.text   = element_text(colour = "black")) +
  geom_point(data = dt_bp_ratio[, .(sdi, time, variable)], shape = 18, size = 2.2) +
  labs(y = "Geschätzer Anteil der Verstorbenen an den Infizierten")

# for hosp
hosp_for_grids <- dt_hosp_y_fitted %>% 
  ggplot(aes(time, y = geschaetzt), color = "#000000") +
  geom_line() +
  scale_y_continuous(limits = c(0, 8000)) +
  scale_x_date(breaks = as.Date(c("2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%d.%m.%y") +
  theme_bw() +
  theme(axis.title.y = element_blank()) +
  annotate("text", label = "Intensivbettenbelegung", x = as.Date(c("2020-10-01")),
           y = 6970, size = 4.5, colour = "black", hjust = 0) +
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
                    ratio_for_grid,
                    NULL,
                    hosp_for_grids,
                    # labels need to be adjusted
                    #labels = c('Inzidenz', "Todesfälle", "Intensivbettenbelegung"),
                    # one col for the three plots, to adjust them among each other
                    ncol = 1,
                    # adjusting the positions of the labels
                    label_x = .08, label_y = .97, hjust = 0, scale = 1, 
                    align = "hv",
                    rel_heights = c(1, -0.22, 1, -0.22, 1, -0.22, 1))

# Arranges all together

grid_plot <- arrangeGrob(p_grid, right = legend)
ggsave("Plots/grid_plot_models.png", plot = grid_plot, width = 22, height = 10, units = c("cm"))




# model_plus_timeseries ---------------------------------------------------
farben4cas <- c("geschätzt" = "#009E73", "gemeldet" = "#000000")
farben4de <- c("geschätzt" = "darkorange", "gemeldet" = "#000000")
farben4int <- c("geschätzt" = "#CC79A7", "gemeldet" = "#000000")


levels(dt_cases_y_fitted_melt$variable)[1] = c("geschätzt")
cases_timeseries <- dt_cases_y_fitted_melt %>% 
  ggplot(aes(x = time, y = values, color = variable)) +
  geom_line() +
  labs(x = "", y = "7-Tages-Inzidenz") +
  scale_color_manual(values = farben4cas, name = "7-Tages-Inzidenz", labels = names(farben4cas)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 250)) +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black", size=1),
        panel.grid = element_line(colour = "gray57", size = 0.2),
        axis.title.y = element_text(margin = margin(t = 0, r = 19, b = 0, l = 0)),
        axis.text   = element_text(colour = "black")) +
  theme(axis.text.x = element_text(size = 10), axis.title.x = element_text(size = 10),
        axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 10)) +
  scale_x_date(breaks = as.Date(c("2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%d.%m.%y") +
  geom_point(data = dt_breakpoints["inz" == origin & variable == "Gesamt"],
             aes(x = time, y = sdi), shape = 18, size = 3, colour = farben4cas[[1]]) +
  geom_segment(data = dt_breakpoints["inz" == origin & variable == "Gesamt"],
               aes(x = lowerCI, y = sdi, xend = upperCI, yend = sdi), colour = farben4cas[[1]]) +
  theme(legend.box.background = element_rect(colour = "black", size = 1),
        legend.title=element_text(size=8, face = "bold"),
        legend.text = element_text(size=8),
        legend.key.height= unit(0.3, 'cm'),
        legend.key.width= unit(0.2, 'cm'))
#cases_timeseries
#ggsave("Plots/timeseries_model_cases.png", plot = cases_timeseries, width = 20, height = 10, units = c("cm"))

levels(dt_deaths_y_fitted_melt$variable)[1] = c("geschätzt")
deaths_timeseries <- dt_deaths_y_fitted_melt %>% 
  ggplot(aes(x = time, y = values, color = variable)) +
  geom_line() +
  labs(x = "", y = "7-Tages-Todesfälle") +
  scale_color_manual(values = farben4de, name = "7-Tages-Todesfälle", labels = names(farben4de)) +
  scale_y_continuous(labels = scales::label_number(accuracy = 1), limits = c(0, 10), breaks = c(0, 2, 4, 6, 8, 10)) +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black", size=1),
        panel.grid = element_line(colour = "gray57", size = 0.2),
        axis.title.y = element_text(margin = margin(t = 0, r = 25, b = 0, l = 0)),
        axis.text   = element_text(colour = "black")) +
  theme(axis.text.x = element_text(size = 10), axis.title.x = element_text(size = 10),
        axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 10)) +
  scale_x_date(breaks = as.Date(c("2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%d.%m.%y") +
  geom_point(data = dt_breakpoints["deaths" == origin & variable == "Gesamt"],
             aes(x = time, y = sdi), shape = 18, size = 3, colour = farben4de[[1]]) +
  geom_segment(data = dt_breakpoints["deaths" == origin & variable == "Gesamt"],
               aes(x = lowerCI, y = sdi, xend = upperCI, yend = sdi), colour = farben4de[[1]]) +
  theme(legend.box.background = element_rect(colour = "black", size = 1),
        legend.title=element_text(size=8, face = "bold"),
        legend.text = element_text(size=8),
        legend.key.height= unit(0.3, 'cm'),
        legend.key.width= unit(0.2, 'cm'))
#deaths_timeseries
#ggsave("Plots/timeseries_model_deaths.png", plot = deaths_timeseries, width = 20, height = 10, units = c("cm"))

levels(dt_hosp_y_fitted_melt$variable)[1] = c("geschätzt")
hosp_timeseries <- dt_hosp_y_fitted_melt %>% 
  ggplot(aes(x = time, y = values, color = variable)) +
  geom_line() +
  labs(x = "", y = "Belegte Intensivbetten") +
  scale_color_manual(values = farben4int, name = "Intensivbettenbelegung", labels = names(farben4int)) +
  scale_y_continuous(limits = c(0, 6000), breaks = c(0, 1000, 2000, 3000, 4000, 5000, 6000, 7000)) +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black", size=1),
        panel.grid = element_line(colour = "gray57", size = 0.2),
        axis.title.y = element_text(margin = margin(t = 0, r = 13, b = 0, l = 0)),
        axis.text   = element_text(colour = "black")) +
  theme(axis.text.x = element_text(size = 10), axis.title.x = element_text(size = 10),
        axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 10)) +
  scale_x_date(breaks = as.Date(c("2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%d.%m.%y") +
  geom_point(data = dt_breakpoints["beds" == origin & variable == "Gesamt"],
             aes(x = time, y = sdi), shape = 18, size = 3, colour = farben4int[[1]]) +
  geom_segment(data = dt_breakpoints["beds" == origin & variable == "Gesamt"],
               aes(x = lowerCI, y = sdi, xend = upperCI, yend = sdi), colour = farben4int[[1]]) +
  theme(legend.box.background = element_rect(colour = "black", size = 1),
        legend.title=element_text(size=8, face = "bold"),
        legend.text = element_text(size=8),
        legend.key.height= unit(0.3, 'cm'),
        legend.key.width= unit(0.2, 'cm'))
#hosp_timeseries
#ggsave("Plots/timeseries_model_hosp.png", plot = hosp_timeseries, width = 20, height = 10, units = c("cm"))

colors_betas <- c("#009E73", "darkorange", "#CC79A7")
names(colors_betas) <- c("7-Tages-Inzidenz", "7-Tages-Todesfälle", "Intensivbettenbelegung")

# change "origin" for plots
new_origin <- c("7-Tages-Inzidenz", "7-Tages-Todesfälle", "Intensivbettenbelegung")
names(new_origin) <- c("inz", "deaths", "beds")
for (i in names(new_origin)) {
  dt_exp_betas[origin == i, origin := new_origin[i]]
}
dt_exp_betas[, time_end := c(time[-1], NA)]
beta_comparison <- dt_exp_betas %>% 
  ggplot(aes(x = time, y = exp_beta, yend = exp_beta, xend = time_end, color = origin)) +
  geom_step(direction = "hv") +
  # geom_segment(aes(xend = )) +
  # geom_vline(aes(xintercept = time), linetype = 2, color = "grey") +
  geom_hline(yintercept = 1, color = "#808080") +
  # data filters out the starting and ending point
  geom_point(data = dt_exp_betas["2020-12-22" > time], shape = 18, size = 3) +
  labs(x = "", y = "exp(beta)") +
  scale_color_manual(values = colors_betas, name = "Wachstumsfaktoren", labels = names(colors_betas)) +
  scale_y_continuous(labels = scales::comma_format(accuracy = 0.01, big.mark = ".", decimal.mark = ","),
                     limits = c(0.98, 1.11), breaks = seq(0.98, 1.10, 0.02)) +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black", size = 1),
        panel.grid = element_line(colour = "gray57", size = 0.2),
        axis.title.y = element_text(margin = margin(t = 0, r = 13, b = 0, l = 0)),
        axis.text   = element_text(colour = "black")) +
  theme(axis.text.x = element_text(size = 10), axis.title.x = element_text(size = 10),
        axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 10)) +
  scale_x_date(breaks = as.Date(c("2020-10-01", "2020-11-01", "2020-12-01")),
               date_labels = "%d.%m.%y") +
  theme(legend.box.background = element_rect(colour = "black", size = 1),
        legend.title=element_text(size=8, face = "bold"),
        legend.text = element_text(size=8),
        legend.key.height= unit(0.3, 'cm'),
        legend.key.width= unit(0.2, 'cm'))


# model diagnose plot. seg_diagnose is from the help_functions/model_diagnose.R file
#
list_model_diagnose <- lapply(dt_models[, model_bic_seq], seg_diagnose)

model_name <- c("inz_gesamt","inz_15-34", "inz_35-59e", "inz_60-79", "inz_80", "death_gesamt",
                "death_15-34", "death_35-59e", "death_60-79", "death_80", "hosp_gesamt")

align_diagnose <- align_plots(plotlist = list_model_diagnose,
                              align = "vh",
                              axis = "tblr")

# save diagnose plots as pngs
Map(function(filename, plot) {
  ggsave(filename, plot = ggdraw(plot), width = 22, height = 10, units = c("cm"))
}, paste("Plots/diagnose_", model_name, ".png", sep = ""), align_diagnose)



### Gesamtgrid

aligned <- align_plots(cases_timeseries,
                       hosp_timeseries,
                       deaths_timeseries,
                       cases_breakpoints,
                       deaths_breakpoints,
                       log_deaths_breakpoints,
                       beta_comparison,
                       align = "hv",
                       axis = "tblr")
ggsave("Plots/timeseries_model_cases.png", plot = ggdraw(aligned[[1]]), width = 22, height = 10, units = c("cm"))
ggsave("Plots/timeseries_model_hosp.png", plot = ggdraw(aligned[[2]]), width = 22, height = 10, units = c("cm"))
ggsave("Plots/timeseries_model_deaths.png", plot = ggdraw(aligned[[3]]), width = 22, height = 10, units = c("cm"))
ggsave("Plots/breakpoints_cases.png", plot = ggdraw(aligned[[4]]), width = 22, height = 10, units = c("cm"))
ggsave("Plots/breakpoints_deaths.png", plot = ggdraw(aligned[[5]]), width = 22, height = 10, units = c("cm"))
ggsave("Plots/log_breakpoints_deaths.png", plot = ggdraw(aligned[[6]]), width = 22, height = 10, units = c("cm"))
ggsave("Plots/beta_comparison.png", plot = ggdraw(aligned[[7]]), width = 22, height = 10, units = c("cm"))